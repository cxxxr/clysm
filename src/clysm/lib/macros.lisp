;;;; macros.lisp - Standard macros for Clysm
;;;; Phase 8 - US6: Standard macro implementations

(in-package #:clysm/lib/macros)

;;; ============================================================
;;; Host-side macro definitions (for use in host SBCL)
;;; ============================================================

(defmacro when* (test &body body)
  "Execute body if test is true."
  `(if ,test (progn ,@body) nil))

(defmacro unless* (test &body body)
  "Execute body if test is false."
  `(if ,test nil (progn ,@body)))

(defmacro cond* (&rest clauses)
  "Conditional with multiple branches."
  (if clauses
      (let ((clause (first clauses)))
        (if (eq (first clause) t)
            `(progn ,@(rest clause))
            `(if ,(first clause)
                 (progn ,@(rest clause))
                 (cond* ,@(rest clauses)))))
      nil))

(defmacro dolist* ((var list &optional result) &body body)
  "Iterate over a list."
  (let ((lst (gensym)))
    `(let ((,lst ,list))
       (loop while ,lst
             do (let ((,var (car ,lst)))
                  ,@body)
                (setf ,lst (cdr ,lst)))
       ,result)))

(defmacro dotimes* ((var count &optional result) &body body)
  "Iterate a fixed number of times."
  (let ((cnt (gensym)))
    `(let ((,cnt ,count))
       (loop for ,var from 0 below ,cnt
             do (progn ,@body))
       ,result)))

(defmacro and* (&rest forms)
  "Evaluate forms left to right, return nil if any is nil."
  (cond
    ((null forms) t)
    ((null (rest forms)) (first forms))
    (t `(if ,(first forms)
            (and* ,@(rest forms))
            nil))))

(defmacro or* (&rest forms)
  "Evaluate forms left to right, return first non-nil value."
  (cond
    ((null forms) nil)
    ((null (rest forms)) (first forms))
    (t (let ((g (gensym)))
         `(let ((,g ,(first forms)))
            (if ,g ,g (or* ,@(rest forms))))))))

;;; ============================================================
;;; Macro expander functions for Clysm compiler
;;; These are the macro functions that get registered in the macro registry
;;; ============================================================

(defun make-when-expander ()
  "Create a macro expander for WHEN."
  (lambda (form)
    (let ((test (second form))
          (body (cddr form)))
      (list 'if test (cons 'progn body) nil))))

(defun make-unless-expander ()
  "Create a macro expander for UNLESS."
  (lambda (form)
    (let ((test (second form))
          (body (cddr form)))
      (list 'if test nil (cons 'progn body)))))

(defun make-cond-expander ()
  "Create a macro expander for COND."
  (lambda (form)
    (labels ((expand-clauses (clauses)
               (if (null clauses)
                   nil
                   (let ((clause (first clauses)))
                     (if (eq (first clause) t)
                         (cons 'progn (rest clause))
                         (list 'if (first clause)
                               (cons 'progn (rest clause))
                               (expand-clauses (rest clauses))))))))
      (expand-clauses (rest form)))))

(defun make-and-expander ()
  "Create a macro expander for AND."
  (lambda (form)
    (let ((args (rest form)))
      (cond
        ((null args) t)
        ((null (rest args)) (first args))
        (t (list 'if (first args)
                 (cons 'and (rest args))
                 nil))))))

(defun make-or-expander ()
  "Create a macro expander for OR."
  (lambda (form)
    (let ((args (rest form)))
      (cond
        ((null args) nil)
        ((null (rest args)) (first args))
        (t (let ((g (gensym "OR-")))
             (list 'let (list (list g (first args)))
                   (list 'if g g (cons 'or (rest args))))))))))

(defun make-dolist-expander ()
  "Create a macro expander for DOLIST."
  (lambda (form)
    (let* ((spec (second form))
           (var (first spec))
           (list-form (second spec))
           (result (third spec))
           (body (cddr form))
           (lst-var (gensym "LST-")))
      ;; Expand to a loop construct
      (list 'let (list (list lst-var list-form))
            (list 'block nil
                  (list 'tagbody
                        'loop-start
                        (list 'if (list 'null lst-var)
                              (list 'go 'loop-end))
                        (list 'let (list (list var (list 'car lst-var)))
                              (cons 'progn body))
                        (list 'setq lst-var (list 'cdr lst-var))
                        (list 'go 'loop-start)
                        'loop-end)
                  result)))))

(defun make-dotimes-expander ()
  "Create a macro expander for DOTIMES."
  (lambda (form)
    (let* ((spec (second form))
           (var (first spec))
           (count-form (second spec))
           (result (third spec))
           (body (cddr form))
           (cnt-var (gensym "CNT-")))
      ;; Expand to a loop construct
      (list 'let (list (list cnt-var count-form)
                       (list var 0))
            (list 'block nil
                  (list 'tagbody
                        'loop-start
                        (list 'if (list '>= var cnt-var)
                              (list 'go 'loop-end))
                        (cons 'progn body)
                        (list 'setq var (list '+ var 1))
                        (list 'go 'loop-start)
                        'loop-end)
                  result)))))

(defun make-case-expander ()
  "Create a macro expander for CASE.
   (case keyform (key form...) ... [(otherwise|t form...)])
   Expands to a let binding the keyform, then nested if/eql tests."
  (lambda (form)
    (let ((keyform (second form))
          (clauses (cddr form))
          (key-var (gensym "KEY-")))
      (labels ((otherwise-clause-p (clause)
                 (let ((keys (first clause)))
                   (or (eq keys 'otherwise)
                       (eq keys 't))))
               (make-key-test (key-var key)
                 (list 'eql key-var (list 'quote key)))
               (make-clause-test (key-var keys)
                 (cond
                   ;; Single key (not a list)
                   ((not (listp keys))
                    (make-key-test key-var keys))
                   ;; Multiple keys in a list
                   ((null (rest keys))
                    (make-key-test key-var (first keys)))
                   (t
                    (cons 'or (mapcar (lambda (k)
                                        (make-key-test key-var k))
                                      keys)))))
               (expand-clauses (clauses)
                 (if (null clauses)
                     nil
                     (let* ((clause (first clauses))
                            (keys (first clause))
                            (body (rest clause)))
                       (if (otherwise-clause-p clause)
                           ;; otherwise/t clause - just the body
                           (cons 'progn body)
                           ;; Normal clause
                           (list 'if
                                 (make-clause-test key-var keys)
                                 (cons 'progn body)
                                 (expand-clauses (rest clauses))))))))
        (list 'let (list (list key-var keyform))
              (expand-clauses clauses))))))

(defun make-prog1-expander ()
  "Create a macro expander for PROG1.
   (prog1 first-form form*) - Evaluates all forms, returns first's value."
  (lambda (form)
    (let ((first-form (second form))
          (rest-forms (cddr form))
          (result-var (gensym "PROG1-")))
      (list 'let (list (list result-var first-form))
            (cons 'progn (append rest-forms (list result-var)))))))

(defun make-prog2-expander ()
  "Create a macro expander for PROG2.
   (prog2 first-form second-form form*) - Evaluates all forms, returns second's value."
  (lambda (form)
    (let ((first-form (second form))
          (second-form (third form))
          (rest-forms (cdddr form))
          (result-var (gensym "PROG2-")))
      (list 'progn
            first-form
            (list 'let (list (list result-var second-form))
                  (cons 'progn (append rest-forms (list result-var))))))))

(defun make-do-expander ()
  "Create a macro expander for DO.
   (do ((var init [step])...) (end-test result...) body...)
   Expands to a block with tagbody for iteration."
  (lambda (form)
    (let ((var-clauses (second form))
          (end-clause (third form))
          (body (cdddr form)))
      (let ((end-test (first end-clause))
            (result-forms (rest end-clause))
            (loop-tag (gensym "DO-LOOP-"))
            (end-tag (gensym "DO-END-")))
        ;; Build initial bindings and step forms
        (let ((bindings (mapcar (lambda (clause)
                                  (list (first clause) (second clause)))
                                var-clauses))
              (step-setqs (loop for clause in var-clauses
                                when (cddr clause)
                                  collect (list (first clause) (third clause)))))
          (list 'block nil
                (list* 'let bindings
                       (list 'tagbody
                             loop-tag
                             (list 'if end-test
                                   (list 'go end-tag))
                             (cons 'progn body)
                             ;; Parallel assignment of step forms
                             (if step-setqs
                                 (list* 'psetq (apply #'append step-setqs))
                                 nil)
                             (list 'go loop-tag)
                             end-tag)
                       ;; Return result forms
                       result-forms)))))))

(defun make-do*-expander ()
  "Create a macro expander for DO*.
   Like DO but with sequential variable binding."
  (lambda (form)
    (let ((var-clauses (second form))
          (end-clause (third form))
          (body (cdddr form)))
      (let ((end-test (first end-clause))
            (result-forms (rest end-clause))
            (loop-tag (gensym "DO*-LOOP-"))
            (end-tag (gensym "DO*-END-")))
        ;; Build initial bindings and step forms
        (let ((bindings (mapcar (lambda (clause)
                                  (list (first clause) (second clause)))
                                var-clauses))
              (step-setqs (loop for clause in var-clauses
                                when (cddr clause)
                                  collect (list 'setq (first clause) (third clause)))))
          (list 'block nil
                (list* 'let* bindings
                       (list 'tagbody
                             loop-tag
                             (list 'if end-test
                                   (list 'go end-tag))
                             (cons 'progn body)
                             ;; Sequential assignment of step forms
                             (if step-setqs
                                 (cons 'progn step-setqs)
                                 nil)
                             (list 'go loop-tag)
                             end-tag)
                       ;; Return result forms
                       result-forms)))))))

;;; ============================================================
;;; Standard macro installation
;;; ============================================================

(defun install-standard-macros (registry)
  "Install all standard macros into REGISTRY."
  (clysm/compiler/transform/macro:register-macro
   registry 'when (make-when-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'unless (make-unless-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'cond (make-cond-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'case (make-case-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'and (make-and-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'or (make-or-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'prog1 (make-prog1-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'prog2 (make-prog2-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'do (make-do-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'do* (make-do*-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'dolist (make-dolist-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'dotimes (make-dotimes-expander))
  registry)
