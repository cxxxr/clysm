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

(defvar *arity-entries*)                ; (src/codegen.lisp)
(defparameter *inline-max-rest-nodes* 60
  "The size limit for a rest-list consumer whose copied list can be removed.")
(defparameter *inline-max-rest-args* 8
  "The arguments a copy of a function with &REST takes at most for its rest list.")

(defun lam-rest-only-p (l)
  "L has required parameters and &REST, nothing else."
  (and (lam-rest l) (null (lam-opt l)) (not (lam-keyp l))))

(defun fresh-list-node (nodes)
  "The node of (LIST . NODES), as the compiler converts it (FRESH-LIST-ARGS)."
  (if nodes
      `(:wasm :eqref ,nodes ((:nil) ,@(loop repeat (length nodes) collect '(:struct.new :cons))))
      (list :const nil)))

(defun fresh-list-args (node)
  "The nodes of the elements when NODE makes a list of them (LIST of one
or more arguments), else :NO."
  (if (and (eq (car node) :wasm) (third node) (equal (car (fourth node)) '(:nil))
           (= (length (cdr (fourth node))) (length (third node)))
           (every (lambda (i) (equal i '(:struct.new :cons))) (cdr (fourth node))))
      (third node)
      :no))

(defun inlinable-lam-p (lam sym)
  (multiple-value-bind (v found) (gethash lam *inline-candidates*)
    (if found v
        (setf (gethash lam *inline-candidates*)
              (and (or (lam-simple-p lam)
                       ;; (the rest list of a copy is made where it is
                       ;; called, and not made at all when the body only
                       ;; measures it or takes it apart: SCALARIZE-LISTS)
                       ;; (only then: a copy that makes the list is the
                       ;; call with more code)
                       (and (lam-rest-only-p lam) (not (lvar-special (lam-rest lam)))
                            ;; (not APPEND, MAX...: the call of two
                            ;; arguments is the call of a function of two)
                            (not (assoc sym *arity-entries*))
                            (not (lvar-captured (lam-rest lam)))
                            (scalarize-let (list :let nil (lam-body lam)) (list (lam-rest lam)) nil lam t)))
                   (null (lam-free-vars lam)) (lam-static-p lam)
                   (notany #'lvar-special (lam-req lam))
                   (let ((n 0) (limit (if (gethash sym *declared-inline*)
                                          *inline-max-declared-nodes*
                                          (if (lam-rest-only-p lam) *inline-max-rest-nodes* *inline-max-nodes*))))
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
        (let* ((sym (cgensym "V"))
               (lv (make-lvar :name sym :owner *inline-caller*))
               (env (env-extend (make-cenv) (list :var sym lv)))
               ;; (converted as code of the caller: a block of the test's
               ;; expansion, TYPEP by the kind, is the caller's, and an
               ;; exit to it a branch)
               (test (let ((*current-lam* *inline-caller*)) (convert (type-test-form sym spec) env)))
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
    (and lam
         (not (not-copied-p sym))
         (if (lam-rest lam)
             (<= (length (lam-req lam)) (length (cddr node)) (+ (length (lam-req lam)) *inline-max-rest-args*))
             (= (length (cddr node)) (length (lam-req lam))))
         ;; not a function that signals (%TYPE-ERROR, an error helper): its
         ;; call is on a path not expected to be taken, where a copy (the
         ;; list of ERROR's arguments made inline) only makes the code larger
         (not (cold-function-p sym))
         (inlinable-lam-p lam sym)
         ;; a function whose own calls were copied into it since it was found
         ;; small: still not much larger (mutually recursive functions would
         ;; otherwise copy each other's growing bodies without end)
         (or (not (gethash lam *inlined-into*))
             (let ((cap (* 3 (if (gethash sym *declared-inline*) *inline-max-declared-nodes*
                                (if (lam-rest-only-p lam) *inline-max-rest-nodes* *inline-max-nodes*)))))
               (<= (count-nodes (lam-body lam) cap) cap)))
         lam)))

(defparameter *partial-inline-max-nodes* 16
  "The size of the test and of the other branch of a function that
PARTIAL-INLINE-CANDIDATE copies.")

(defun partial-inline-candidate (node)
  "The lam of the function NODE calls when its body is an IF of which the
test and one branch make no call and are small, and the other branch calls
the function itself (a recursive function's base case, as in (if (< n 2) n
(+ (fib (- n 1)) (fib (- n 2))))): the call can be replaced by a copy of the
test and the small branch, the other branch being the call.  The calls that
end in the base case, most of a recursion's, then make no call: V8 makes a
frame (its stack check, the parameters spilled, the instance pushed) for
every call, which in such a function costs more than the body.  The first
value is the lam, the second :THEN or :ELSE, the small branch, which is a
constant, a variable or an IF of them."
  (let* ((sym (second node)) (lam (known-global-lam sym)) (body (and lam (lam-body lam))))
    (labels ((cheap (n)
               (and (member (car n) '(:const :lref :prim :the :if))
                    (or (not (eq (car n) :prim)) (eq (prim-kind (second n)) :value)
                        (eq (prim-kind (second n)) :test))
                    (block sub (map-subnodes (lambda (x) (unless (cheap x) (return-from sub nil))) n) t)))
             (leaf (n)
               ;; a value the branch gives: a constant, a variable, or an IF of
               ;; them (no primitive, whose code may be large: %SIMPLE-REF)
               (case (car n)
                 ((:const :lref) t)
                 (:the (leaf (third n)))
                 (:if (and (cheap (second n)) (leaf (third n)) (leaf (fourth n))))))
             (self-call-p (n)
               (or (and (eq (car n) :call) (eq (second n) sym))
                   (block sub (map-subnodes (lambda (x) (when (self-call-p x) (return-from sub t))) n) nil))))
      (when (and lam (eq (car body) :if) (lam-simple-p lam) (null (lam-free-vars lam))
                 (= (length (cddr node)) (length (lam-req lam)))
                 (notany #'lvar-special (lam-req lam))
                 (not (gethash sym *notinline*))
                 (not (not-copied-p sym))
                 (cheap (second body))
                 (<= (count-nodes (second body) *partial-inline-max-nodes*) *partial-inline-max-nodes*))
        (flet ((small (n) (and (leaf n) (<= (count-nodes n *partial-inline-max-nodes*) *partial-inline-max-nodes*))))
          (cond ((and (small (third body)) (self-call-p (fourth body))) (values lam :then))
                ((and (small (fourth body)) (self-call-p (third body))) (values lam :else))))))))

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
           (partial-calls (make-hash-table :test 'eq)) ; calls copied by PARTIAL-INLINE-CANDIDATE -> arm
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
                   (when (and (eq (car node) :call) (< depth *inline-max-depth*)
                              (not (inline-candidate node)))
                     (multiple-value-bind (callee arm) (partial-inline-candidate node)
                       (when (and callee (eq callee lam) (afford callee nil))
                         ;; (f a b) -> (let ((x a) (y b)) (if test small (f x y))):
                         ;; the residual call is not walked again
                         (let* ((map (make-hash-table :test 'eq))
                                (bindings (loop for p in (lam-req callee) for a in (cddr node)
                                                collect (cons (copy-lvar-for-inline p map)
                                                              (inline-checked-arg p a callee))))
                                (body (lam-body callee))
                                (test (copy-node-for-inline (second body) map))
                                (small (copy-node-for-inline (if (eq arm :then) (third body) (fourth body)) map))
                                (call `(:call ,(second node)
                                              ,@(loop for b in bindings
                                                      do (incf (lvar-refs (car b)))
                                                         collect `(:lref ,(car b))))))
                           (setf (car node) :let
                                 (cdr node) (list bindings (if (eq arm :then)
                                                               `(:if ,test ,small ,call)
                                                               `(:if ,test ,call ,small))))
                           (setf (gethash node partial-calls) arm)
                           (return-from walk)))))
                   ;; (the fixnum (f a)) at (SAFETY 0), F's call copied as above:
                   ;; the residual call is (the fixnum (f x)) too, so that its
                   ;; raw result is not boxed for the IF
                   (when (and (eq (car node) :the) (eq (fourth node) :trust)
                              (gethash (third node) partial-calls))
                     (let* ((if-node (third (third node)))
                            (pos (if (eq (gethash (third node) partial-calls) :then) (cdddr if-node) (cddr if-node))))
                       (setf (car pos) `(:the ,(second node) ,(car pos) :trust))))
                   (when (and (eq (car node) :call) (< depth *inline-max-depth*))
                     (let ((callee (inline-candidate node)))
                       (when (and callee (afford callee (gethash (second node) *declared-inline*)))
                         (let* ((map (make-hash-table :test 'eq))
                                (bindings (loop for p in (lam-req callee) for a in (cddr node)
                                                collect (cons (copy-lvar-for-inline p map)
                                                              (inline-checked-arg p a callee))))
                                ;; (&REST: the list of the other arguments, last)
                                (bindings (if (lam-rest callee)
                                              (append bindings
                                                      (list (cons (copy-lvar-for-inline (lam-rest callee) map)
                                                                  (fresh-list-node
                                                                   (nthcdr (length (lam-req callee)) (cddr node))))))
                                              bindings))
                                (body (copy-node-for-inline (lam-body callee) map)))
                           (walk body (1+ depth))
                           (setf (car node) :let
                                 (cdr node) (list bindings body)))))))))
        (walk (lam-body lam) 0)
        (scalarize-lists lam)))))

;;; A variable bound to (LIST e1 ... ek) -- the rest list of a copy of a
;;; function with &REST, a list made to be walked once -- whose list is only
;;; measured and taken apart is not made: the elements are variables, and a
;;; variable that walks the list (DOLIST) holds the index of its place.
;;; The variable (and the variables bound to it or to a CDR of it, which
;;; may be set to their own CDRs: all of them *places*, a variable and a
;;; number of CDRs) may be the argument of CAR, NULL, NOT, ATOM, CONSP,
;;; ENDP, LENGTH, NTH, the test of an IF, the datum of
;;; %TYPE-ERROR (the branch of a DOLIST that a proper list does not take),
;;; and the last argument of APPLY at a place known where it is compiled;
;;; anything else (a call given the list, RPLACA, a closure, a declared
;;; type) leaves the list as it is.  (SBCL does as much for &REST, with the
;;; arguments left on the stack.)
(defparameter *scalarize-max-elements* 8)

(defun scalarize-lists (lam)
  "Replace (in place) in LAM's body the lists that SCALARIZE-LET can."
  (let ((*current-lam* lam))
    (labels ((walk (node)
               (when (consp node)
                 (when (eq (car node) :let)
                   (dolist (b (second node))
                     (let ((args (fresh-list-args (cdr b))))
                       (when (and (listp args) (<= (length args) *scalarize-max-elements*)
                                  (not (lvar-special (car b))) (not (lvar-captured (car b)))
                                  (scalarize-let node b args lam))
                         ;; (the node has other bindings now: again)
                         (walk node)
                         (return-from walk)))))
                 (map-subnodes #'walk node))))
      (walk (lam-body lam)))))

(defun scalarize-let (let-node binding args lam &optional dry)
  "BINDING of LET-NODE binds a variable to the list of the nodes ARGS: when
the list is only used as SCALARIZE-LISTS says, bind the elements instead
and rewrite the uses; true when it was done.  DRY: only tell whether it
would be (BINDING is then (variable), and LET-NODE has the body)."
  (let* ((root (car binding)) (k (length args))
         (vars (list root))              ; the variables that are places of the list
         (temps (loop for i from 0 below k
                      collect (make-lvar :name (make-symbol (format nil "ELEMENT~D" i)) :owner lam)))
         (syms (make-hash-table :test 'eq))
         (static (make-hash-table :test 'eq)) ; variable -> its index, when it is known
         (aliases nil) (applies nil)
         (env nil))
    (labels ((sym (lv) (or (gethash lv syms) (setf (gethash lv syms) (make-symbol (princ-to-string (lvar-name lv))))))
             (place (n)
               ;; (variable . cdrs) when N is a place of the list
               (cond ((and (eq (car n) :lref) (member (second n) vars)) (cons (second n) 0))
                     ((and (eq (car n) :prim) (member (prim-name (second n)) '(cdr rest))
                           (cddr n) (null (cdddr n)))
                      (let ((p (place (third n)))) (and p (cons (car p) (+ (cdr p) 1)))))
                     (t nil)))
             (static-index (p)
               (let ((base (gethash (car p) static)))
                 (and base (+ base (cdr p)))))
             (index-form (p)
               (or (static-index p)
                   (if (eql (cdr p) 0) (sym (car p)) `(clysm-sys::%fx+ ,(sym (car p)) ,(cdr p)))))
             (element-form (index)
               (if (integerp index)
                   (if (< index k) (sym (nth index temps)) nil)
                   (let ((i (cgensym "I")) (form nil))
                     (loop for j from (- k 1) downto 0
                           do (setq form `(if (eql ,i ,j) ,(sym (nth j temps)) ,form)))
                     `(let ((,i ,index)) ,form))))
             (ir (form) (convert form env))
             (replace-node (node new)
               (setf (car node) (car new) (cdr node) (cdr new)))
             ;; MODE :CHECK returns from the function when a use is not one
             ;; of those above; :REWRITE replaces the uses.  USED: the value
             ;; of NODE is.
             (process (node used mode)
               (when (consp node)
                 (let ((op (car node)))
                   (cond
                     ((eq op :lref)
                      (when (member (second node) vars) (return-from scalarize-let nil)))
                     ((and (eq op :prim) (cddr node) (null (cdddr node)) (place (third node))
                           (member (prim-name (second node)) '(car first null not atom consp)))
                      (when (eq mode :rewrite)
                        (let ((index (index-form (place (third node)))) (name (prim-name (second node))))
                          (replace-node
                           node
                           (ir (cond ((member name '(car first)) (element-form index))
                                     ((integerp index) (if (eq name 'consp) (< index k) (>= index k)))
                                     ((eq name 'consp) `(clysm-sys::%fx< ,index ,k))
                                     (t `(clysm-sys::%fx>= ,index ,k))))))))
                     ((and (eq op :call) (member (second node) '(length endp)) (cddr node) (null (cdddr node))
                           (place (third node)))
                      (when (eq mode :rewrite)
                        (let ((index (index-form (place (third node)))))
                          (replace-node
                           node
                           (ir (cond ((eq (second node) 'endp)
                                      (if (integerp index) (>= index k) `(clysm-sys::%fx>= ,index ,k)))
                                     ((integerp index) (max 0 (- k index)))
                                     (t (let ((i (cgensym "I")))
                                          `(let ((,i ,index))
                                             (if (clysm-sys::%fx< ,i ,k) (clysm-sys::%fx- ,k ,i) 0))))))))))
                     ((and (eq op :call) (eq (second node) 'nth) (= (length (cddr node)) 2)
                           (eq (car (third node)) :const) (typep (second (third node)) '(integer 0 1000))
                           (place (fourth node)))
                      (when (eq mode :rewrite)
                        (let ((index (index-form (place (fourth node)))) (n (second (third node))))
                          (replace-node node (ir (element-form (if (integerp index)
                                                                   (+ index n)
                                                                   `(clysm-sys::%fx+ ,index ,n))))))))
                     ((and (eq op :call) (eq (second node) 'nth) (= (length (cddr node)) 2)
                           (place (fourth node)))
                      (process (third node) t mode)
                      (when (eq mode :rewrite)
                        (let ((n (cgensym "N")) (i (cgensym "I"))
                              (index (index-form (place (fourth node))))
                              (arg (third node)))
                          ;; N is evaluated before the list cursor. Invalid
                          ;; indices use NTH's ordinary error path; huge valid
                          ;; indices return NIL without overflowing %FX+.
                          ;; Bind the original IR argument without converting
                          ;; it back to source or changing evaluation order.
                          (let* ((v (make-lvar :name n :owner lam))
                                 (env (env-extend env (list :var n v))))
                            (replace-node node
                              (list :let (list (cons v arg))
                                    (convert
                                     `(if (typep ,n '(integer 0 *))
                                          (let ((,i ,index))
                                            (if (and (< ,n ,k) (clysm-sys::%fx< ,i ,k))
                                                ,(element-form `(clysm-sys::%fx+ ,i ,n)) nil))
                                          (nth ,n (nthcdr ,index (list ,@(mapcar #'sym temps)))))
                                     env)))))))
                     ((and (eq op :call) (eq (second node) 'clysm-sys::%type-error) (cddr node) (place (third node)))
                      (dolist (a (cdddr node)) (process a t mode))
                      (when (eq mode :rewrite)
                        ;; Preserve the error's datum, including for explicit
                        ;; type errors. DOLIST's proper-list check cannot take
                        ;; this path, so it still allocates nothing.
                        (setf (third node)
                              (ir `(nthcdr ,(index-form (place (third node)))
                                           (list ,@(mapcar #'sym temps)))))))
                     ((and (eq op :if) (place (second node)))
                      (when (eq mode :rewrite)
                        (let ((index (index-form (place (second node)))))
                          (setf (second node)
                                (ir (if (integerp index) (< index k) `(clysm-sys::%fx< ,index ,k))))))
                      (process (third node) used mode)
                      (process (fourth node) used mode))
                     ((and (eq op :apply) (cddr node) (place (car (last node))))
                      (let ((p (place (car (last node)))))
                        (dolist (a (butlast (cdr node))) (process a t mode))
                        (if (eq mode :check)
                            ;; (the place must be known where the call is
                            ;; compiled: decided when all the variables are)
                            (push p applies)
                            (let ((index (static-index p)))
                              (setf (car node) :funcall
                                    (cdr node) (append (butlast (cdr node))
                                                       (loop for j from index below k
                                                             collect (ir (sym (nth j temps))))))))))
                     ((eq op :lset)
                      (let ((v (second node)))
                        (if (member v vars)
                            (let ((p (place (third node))))
                              (unless (and p (not used)) (return-from scalarize-let nil))
                              (when (eq mode :rewrite)
                                (replace-node node (ir `(setq ,(sym v) ,(index-form p))))))
                            (process (third node) t mode))))
                     ((eq op :let)
                      (let ((new nil))
                        (dolist (b (second node))
                          (let ((p (place (cdr b))))
                            (cond ((null p) (process (cdr b) t mode))
                                  ((eq mode :check)
                                   (when (or (lvar-special (car b)) (lvar-captured (car b)) (lvar-decl (car b)))
                                     (return-from scalarize-let nil))
                                   (push (cons (car b) p) aliases)
                                   (push (car b) new))
                                  (t (setf (cdr b) (ir (if (gethash (car b) static) nil (index-form p))))))))
                        ;; (the variables are places in the body only)
                        (setq vars (append new vars)))
                      (process (third node) used mode))
                     ((eq op :progn)
                      (loop for l on (cdr node)
                            do (process (car l) (and used (null (cdr l))) mode)))
                     ((eq op :tagbody)
                      (dolist (s (third node)) (dolist (n (cdr s)) (process n nil mode))))
                     (t (map-subnodes (lambda (n) (process n t mode)) node)))))))
      (when (lvar-decl root) (return-from scalarize-let nil))
      (process (third let-node) t :check)
      ;; the variables that are not set have a known index
      (unless (lvar-mutated root) (setf (gethash root static) 0))
      (dolist (a (reverse aliases))
        (let ((v (car a)) (p (cdr a)))
          (unless (lvar-mutated v)
            (let ((index (static-index p)))
              (when index (setf (gethash v static) index))))))
      (dolist (p applies)
        (unless (static-index p) (return-from scalarize-let nil)))
      (when dry (return-from scalarize-let t))
      ;; rewrite: the other variables hold their index
      (dolist (v vars)
        (setf (lvar-refs v) 0 (lvar-type v) nil (lvar-ntys v) nil))
      (setq env (apply #'env-extend (make-cenv)
                       (append (mapcar (lambda (v) (list :var (sym v) v)) vars)
                               (mapcar (lambda (v) (list :var (sym v) v)) temps))))
      (process (third let-node) t :rewrite)
      (setf (second let-node)
            (loop for b in (second let-node)
                  append (if (eq b binding)
                             (append (mapcar #'cons temps args)
                                     (list (cons root (list :const (if (gethash root static) nil 0)))))
                             (list b))))
      t)))
