;;;; inline.lisp — inlining of small known functions
;;;;
;;;; Before a lambda is analyzed and compiled, calls in its body to small
;;;; known global functions (defined once, required parameters only, no
;;;; closures inside) are replaced by a copy of the function's body that binds
;;;; the parameters to the arguments, so that type inference sees through
;;;; them and no call is made.  As with direct calls (CLHS 3.2.2.3), the
;;;; definition is assumed not to change; NOTINLINE prevents it.

(in-package :clysm)

(defparameter *inline-max-nodes* 30)
(defparameter *inline-max-depth* 3)
(defparameter *inline-max-local-nodes* 30 "for local functions and lambdas of the function")
(defparameter *inline-max-single-use-nodes* 300
  "for a local function or lambda called once, nowhere else referenced: the
copy replaces its code")
(defvar *inline-candidates* (make-hash-table :test 'eq) "lam -> inlinable-p")
(defvar *inlined-into* (make-hash-table :test 'eq) "lams whose bodies were processed")
(defvar *inline-caller* nil "the lam receiving the copies")
(defvar *declared-inline* (make-hash-table :test 'eq) "functions declaimed INLINE (no size limit)")
(defparameter *inline-max-declared-nodes* 400)
(defvar *inline-big-caller-nodes* nil
  "When set, a function larger than this (in nodes) gets no copies of
functions not declared INLINE, and *INLINE-CALLER-BUDGET* limits those of
the others (tools/build-repl.lisp sets them for the compiler's own code).")
(defvar *inline-caller-budget* nil
  "When set, the nodes of copies of functions not declared INLINE that a
function gets at most.")
(defvar *inline-budget-source-p* (constantly t)
  "Whether the limits above apply to a function of the source file (a name).")

(defun count-nodes (node limit)
  "The number of nodes of NODE, counting at most to LIMIT + 1."
  (let ((n 0))
    (labels ((walk (x)
               (when (> (incf n) limit) (return-from count-nodes n))
               (map-subnodes #'walk x)))
      (walk node)
      n)))

(defun inlinable-lam-p (lam sym)
  (multiple-value-bind (v found) (gethash lam *inline-candidates*)
    (if found v
        (setf (gethash lam *inline-candidates*)
              (and (lam-simple-p lam) (null (lam-free-vars lam)) (lam-static-p lam)
                   (notany #'lvar-special (lam-req lam))
                   (let ((n 0) (limit (if (gethash sym *declared-inline*)
                                          *inline-max-declared-nodes* *inline-max-nodes*)))
                     (labels ((walk (node)
                                (when (or (> (incf n) limit)
                                          (member (car node) '(:lambda :flet :labels :defun :lcall
                                                               :symtab :catch :unwind-protect :progv))
                                          ;; (load-time-value (find-class 'x)) may be copied
                                          (and (eq (car node) :ltv) (not (struct-class-node-name node)))
                                          (and (eq (car node) :call) (eq (second node) sym)))
                                  (return-from inlinable-lam-p
                                    (setf (gethash lam *inline-candidates*) nil)))
                                (map-subnodes #'walk node)))
                       (walk (lam-body lam))
                       t)))))))

(defun copy-lvar-for-inline (lv map)
  (let ((new (make-lvar :name (lvar-name lv) :owner *inline-caller* :captured nil
                        :mutated (lvar-mutated lv) :special (lvar-special lv)
                        :refs (lvar-refs lv) :ignorable (lvar-ignorable lv)
                        ;; (the copy is typed as the function was)
                        :decl (lvar-decl lv))))
    (setf (gethash lv map) new)
    new))

(defun inline-checked-arg (p arg callee)
  "ARG, given to the parameter P of a copy of CALLEE: checked to be of the
type P is declared of, as the prologue of CALLEE checks it (PARAM-CHECK-CODE;
at SAFETY 1 and more, the test left out where the type of ARG decides it)."
  (let ((spec (lvar-decl p)))
    (if (and spec (not (lvar-special p)) (lam-safety callee) (not (eql (lam-safety callee) 0))
             (decl-check-ty spec)
             ;; (a function given to a parameter declared FUNCTION: its
             ;; calls are copied too, see REGISTER below)
             (not (and (member (car arg) '(:lambda :fref))
                       (logtest (ty-bits (decl-check-ty spec)) +k-func+))))
        (let* ((sym (gensym "V"))
               (lv (make-lvar :name sym :owner *inline-caller*))
               (env (env-extend (make-cenv) (list :var sym lv)))
               (test (convert (type-test-form sym spec) env))
               (err (convert `(clysm-sys::%signal-type-error ,sym ',spec) env)))
          `(:let ((,lv . ,arg)) (:if ,test ,(convert sym env) ,err)))
        arg)))

(defun copy-node-for-inline (node map)
  (labels ((m (x) (or (gethash x map) x))
           (c (n) (copy-node-for-inline n map)))
    (case (car node)
      ((:const :sref :fref) node)
      (:ltv `(:ltv ,(c (second node))))
      (:lref `(:lref ,(m (second node))))
      (:lset `(:lset ,(m (second node)) ,(c (third node))))
      (:sset `(:sset ,(second node) ,(c (third node))))
      ((:if :progn :values) `(,(car node) ,@(mapcar #'c (cdr node))))
      (:let (let ((inits (mapcar (lambda (b) (c (cdr b))) (second node))))
              `(:let ,(loop for b in (second node) for init in inits
                            collect (cons (copy-lvar-for-inline (car b) map) init))
                     ,(c (third node)))))
      (:mvbind (let ((v (c (third node))))
                 `(:mvbind ,(mapcar (lambda (lv) (copy-lvar-for-inline lv map)) (second node))
                           ,v ,(c (fourth node)))))
      ((:call :prim :lcall) `(,(car node) ,(second node) ,@(mapcar #'c (cddr node))))
      ((:funcall :mvcall :apply) `(,(car node) ,@(mapcar #'c (cdr node))))
      (:block (let* ((b (second node))
                     (nb (make-blk :name (blk-name b) :lam *inline-caller*)))
                (setf (blk-tag-var nb) (copy-lvar-for-inline (blk-tag-var b) map))
                (setf (gethash b map) nb)
                `(:block ,nb ,(c (third node)))))
      (:return-from `(:return-from ,(m (second node)) ,(c (third node))))
      (:tagbody (let* ((tb (second node))
                       (nt (make-tb :lam *inline-caller* :tags (tb-tags tb))))
                  (setf (tb-tag-var nt) (copy-lvar-for-inline (tb-tag-var tb) map))
                  (setf (gethash tb map) nt)
                  `(:tagbody ,nt ,(mapcar (lambda (s) (cons (car s) (mapcar #'c (cdr s)))) (third node)))))
      (:go `(:go ,(m (second node)) ,(third node)))
      ((:throw :mvprog1) `(,(car node) ,(c (second node)) ,(c (third node))))
      (:the `(:the ,(second node) ,(c (third node)) ,@(cdddr node)))
      (:wasm `(:wasm ,(second node) ,(mapcar #'c (third node)) ,(fourth node)))
      (:wasm-test `(:wasm-test ,(mapcar #'c (second node)) ,(third node)))
      (t (error "inline: cannot copy ~S" (car node))))))

(defun hash-table-alist* (table)
  (let ((r nil)) (maphash (lambda (k v) (push (cons k v) r)) table) r))

(defun inline-candidate (node)
  (let* ((sym (second node)) (lam (known-global-lam sym)))
    (and lam (= (length (cddr node)) (length (lam-req lam)))
         (inlinable-lam-p lam sym)
         lam)))

(defun local-inlinable-p (callee caller &optional (limit *inline-max-local-nodes*))
  "CALLEE (a local function or lambda defined in CALLER) can be copied into CALLER."
  (and (eq (lam-parent callee) caller) (lam-simple-p callee)
       (notany #'lvar-special (lam-req callee))
       (let ((n 0))
         (labels ((walk (node)
                    (when (or (> (incf n) limit)
                              (member (car node) '(:lambda :flet :labels :ltv :defun :symtab
                                                   :catch :unwind-protect :progv))
                              (and (eq (car node) :lcall) (eq (fun-lam (second node)) callee)))
                      (return-from local-inlinable-p nil))
                    (map-subnodes #'walk node)))
           (walk (lam-body callee))
           t))))

(defun inline-calls (lam)
  "Replace (in place) the calls in LAM's body to inlinable functions."
  (unless (gethash lam *inlined-into*)
    (setf (gethash lam *inlined-into*) t)
    (let* ((*inline-caller* lam) (lvar-lams (make-hash-table :test 'eq))
           ;; a variable bound to a variable bound to a lambda -> the latter
           (lvar-roots (make-hash-table :test 'eq))
           (fresh (make-hash-table :test 'eq))   ; the variables made by copying
           ;; (copies in a large function make its code larger, which V8
           ;; compiles when it first runs, more than faster)
           (budget (cond ((or (null *inline-big-caller-nodes*)
                              (not (funcall *inline-budget-source-p* (car (lam-source lam)))))
                          nil)
                         ((> (count-nodes (lam-body lam) *inline-big-caller-nodes*) *inline-big-caller-nodes*)
                          0)
                         (t *inline-caller-budget*))))
      (labels ((afford (callee declared)
                 (or declared (null budget)
                     (let ((n (count-nodes (lam-body callee) budget)))
                       (when (<= n budget) (decf budget n) t))))
               (register (bindings)
                 (dolist (b bindings)
                   (cond ((and (member (car (cdr b)) '(:lambda :fref)) (not (lvar-mutated (car b)))
                               (not (lvar-special (car b))))
                          (setf (gethash (car b) lvar-lams)
                                (if (eq (car (cdr b)) :lambda) (second (cdr b)) (cdr b))))
                         ;; a variable bound to such a variable (the parameter
                         ;; of a copy of an inline function given a lambda)
                         ((and (eq (car (cdr b)) :lref) (not (lvar-mutated (car b)))
                               (not (lvar-special (car b))) (not (lvar-mutated (second (cdr b))))
                               (gethash (second (cdr b)) lvar-lams))
                          (setf (gethash (car b) lvar-lams) (gethash (second (cdr b)) lvar-lams))
                          (setf (gethash (car b) lvar-roots)
                                (or (gethash (second (cdr b)) lvar-roots) (second (cdr b))))))))
               (expand (node callee args depth)
                 (let* ((map (make-hash-table :test 'eq))
                        (bindings (loop for p in (lam-req callee) for a in args
                                        collect (cons (copy-lvar-for-inline p map) (inline-checked-arg p a callee))))
                        (body (copy-node-for-inline (lam-body callee) map)))
                   (loop for (nil . new) in (hash-table-alist* map)
                         do (when (lvar-p new) (setf (gethash new fresh) t)))
                   (register bindings)
                   (walk body (1+ depth))
                   (drop-aliases bindings depth)
                   (setf (car node) :let
                         (cdr node) (list bindings body))))
               (single-use-p (v depth)
                 ;; the only reference of a variable whose lambda is in no
                 ;; other variable (copying it moves its code)
                 (and (eql (lvar-refs v) 1) (or (= depth 0) (gethash v fresh))
                      (let ((root (gethash v lvar-roots)))
                        (or (null root) (and (eql (lvar-refs root) 1)
                                             (or (= depth 0) (gethash root fresh)))))))
               (drop-aliases (bindings depth)
                 ;; a variable bound to a variable always bound to a lambda,
                 ;; no longer referenced (its calls were copied): not bound,
                 ;; so that the lambda, if it has no other references, makes
                 ;; no closure
                 (dolist (b bindings)
                   (let ((init (cdr b)))
                     (when (and (zerop (lvar-refs (car b))) (not (lvar-special (car b)))
                                (eq (car init) :lref) (lam-p (gethash (second init) lvar-lams))
                                (not (lvar-mutated (second init))))
                       (when (or (= depth 0) (gethash (second init) fresh))
                         (decf (lvar-refs (second init))))
                       (setf (cdr b) '(:const nil))))))
               (walk (node depth)
                 (when (consp node)
                   ;; a variable always bound to a lambda of this function
                   (when (eq (car node) :let) (register (second node)))
                   ;; a test of a variable always bound to a lambda is true:
                   ;; (WHEN FN (FUNCALL FN)) of a copy given a lambda, whose
                   ;; call is copied, then makes no closure (before the
                   ;; call is reached: the call may then be the variable's
                   ;; only reference)
                   (when (and (eq (car node) :if) (consp (second node)) (eq (car (second node)) :lref)
                              (lam-p (gethash (second (second node)) lvar-lams)))
                     (when (or (= depth 0) (gethash (second (second node)) fresh))
                       (decf (lvar-refs (second (second node)))))
                     (setf (second node) '(:const t)))
                   (map-subnodes (lambda (n) (walk n depth)) node)
                   (when (eq (car node) :let) (drop-aliases (second node) depth))
                   ;; FUNCALL of a variable bound to #'name: a call of NAME
                   (when (and (eq (car node) :funcall) (eq (car (second node)) :lref)
                              (consp (gethash (second (second node)) lvar-lams)))
                     (let ((sym (second (gethash (second (second node)) lvar-lams))))
                       (setf (cdr node) (cons sym (cddr node)) (car node) :call)))
                   ;; calls of local functions and FUNCALLs of such variables
                   (when (< depth *inline-max-depth*)
                     (let ((callee (case (car node)
                                     (:lcall (fun-lam (second node)))
                                     (:funcall (and (eq (car (second node)) :lref)
                                                    (lam-p (gethash (second (second node)) lvar-lams))
                                                    (gethash (second (second node)) lvar-lams))))))
                       (when (and callee (= (length (lam-req callee)) (length (cddr node)))
                                  (local-inlinable-p callee lam
                                                     (if (single-use-p (if (eq (car node) :lcall)
                                                                           (fun-lvar (second node))
                                                                           (second (second node)))
                                                                       depth)
                                                         *inline-max-single-use-nodes*
                                                         *inline-max-local-nodes*))
                                  (afford callee nil))
                         ;; the reference replaced (references in copies still exist
                         ;; in their originals)
                         (when (or (= depth 0)
                                   (gethash (if (eq (car node) :lcall) (fun-lvar (second node)) (second (second node)))
                                            fresh))
                           (if (eq (car node) :lcall)
                               (decf (lvar-refs (fun-lvar (second node))))
                               (decf (lvar-refs (second (second node))))))
                         (expand node callee (cddr node) depth)
                         (return-from walk))))
                   (when (and (eq (car node) :call) (< depth *inline-max-depth*))
                     (let ((callee (inline-candidate node)))
                       (when (and callee (afford callee (gethash (second node) *declared-inline*)))
                         (let* ((map (make-hash-table :test 'eq))
                                (bindings (loop for p in (lam-req callee) for a in (cddr node)
                                                collect (cons (copy-lvar-for-inline p map)
                                                              (inline-checked-arg p a callee))))
                                (body (copy-node-for-inline (lam-body callee) map)))
                           (walk body (1+ depth))
                           (setf (car node) :let
                                 (cdr node) (list bindings body)))))))))
        (walk (lam-body lam) 0)))))
