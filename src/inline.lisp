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
(defvar *inline-candidates* (make-hash-table :test 'eq) "lam -> inlinable-p")
(defvar *inlined-into* (make-hash-table :test 'eq) "lams whose bodies were processed")
(defvar *inline-caller* nil "the lam receiving the copies")
(defvar *declared-inline* (make-hash-table :test 'eq) "functions declaimed INLINE (no size limit)")
(defparameter *inline-max-declared-nodes* 400)

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
                                          (member (car node) '(:lambda :flet :labels :ltv :defun :lcall
                                                               :symtab :catch :unwind-protect :progv))
                                          (and (eq (car node) :call) (eq (second node) sym)))
                                  (return-from inlinable-lam-p
                                    (setf (gethash lam *inline-candidates*) nil)))
                                (map-subnodes #'walk node)))
                       (walk (lam-body lam))
                       t)))))))

(defun copy-lvar-for-inline (lv map)
  (let ((new (make-lvar :name (lvar-name lv) :owner *inline-caller* :captured nil
                        :mutated (lvar-mutated lv) :special (lvar-special lv)
                        :refs (lvar-refs lv) :ignorable (lvar-ignorable lv))))
    (setf (gethash lv map) new)
    new))

(defun copy-node-for-inline (node map)
  (labels ((m (x) (or (gethash x map) x))
           (c (n) (copy-node-for-inline n map)))
    (case (car node)
      ((:const :sref :fref) node)
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
      (:the `(:the ,(second node) ,(c (third node))))
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

(defun local-inlinable-p (callee caller)
  "CALLEE (a local function or lambda defined in CALLER) can be copied into CALLER."
  (and (eq (lam-parent callee) caller) (lam-simple-p callee)
       (notany #'lvar-special (lam-req callee))
       (let ((n 0))
         (labels ((walk (node)
                    (when (or (> (incf n) *inline-max-local-nodes*)
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
    (let ((*inline-caller* lam) (lvar-lams (make-hash-table :test 'eq))
          (fresh (make-hash-table :test 'eq)))   ; the variables made by copying
      (labels ((register (bindings)
                 (dolist (b bindings)
                   (when (and (eq (car (cdr b)) :lambda) (not (lvar-mutated (car b)))
                              (not (lvar-special (car b))))
                     (setf (gethash (car b) lvar-lams) (second (cdr b))))))
               (expand (node callee args depth)
                 (let* ((map (make-hash-table :test 'eq))
                        (bindings (loop for p in (lam-req callee) for a in args
                                        collect (cons (copy-lvar-for-inline p map) a)))
                        (body (copy-node-for-inline (lam-body callee) map)))
                   (loop for (nil . new) in (hash-table-alist* map)
                         do (when (lvar-p new) (setf (gethash new fresh) t)))
                   (register bindings)
                   (walk body (1+ depth))
                   (setf (car node) :let
                         (cdr node) (list bindings body))))
               (walk (node depth)
                 (when (consp node)
                   ;; a variable always bound to a lambda of this function
                   (when (eq (car node) :let) (register (second node)))
                   (map-subnodes (lambda (n) (walk n depth)) node)
                   ;; calls of local functions and FUNCALLs of such variables
                   (when (< depth *inline-max-depth*)
                     (let ((callee (case (car node)
                                     (:lcall (fun-lam (second node)))
                                     (:funcall (and (eq (car (second node)) :lref)
                                                    (gethash (second (second node)) lvar-lams))))))
                       (when (and callee (= (length (lam-req callee)) (length (cddr node)))
                                  (local-inlinable-p callee lam))
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
                       (when callee
                         (let* ((map (make-hash-table :test 'eq))
                                (bindings (loop for p in (lam-req callee) for a in (cddr node)
                                                collect (cons (copy-lvar-for-inline p map) a)))
                                (body (copy-node-for-inline (lam-body callee) map)))
                           (walk body (1+ depth))
                           (setf (car node) :let
                                 (cdr node) (list bindings body)))))))))
        (walk (lam-body lam) 0)))))
