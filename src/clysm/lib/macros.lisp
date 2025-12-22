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
   registry 'and (make-and-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'or (make-or-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'dolist (make-dolist-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'dotimes (make-dotimes-expander))
  registry)
