;;;; The warnings of variables never used (CLYSM_WARN_UNUSED; the check is
;;;; CHECK-UNUSED in src/frontend.lisp): tests/lib/warn-unused.expected
;;;; holds the warnings of this file, tests/lib/run.sh compares them (they
;;;; are told as the forms are converted, before --shake leaves any out).

(defpackage :warn-unused (:use :cl))
(in-package :warn-unused)

;;; Told: a parameter, a supplied-p and a key parameter, a variable of LET,
;;; LET*, MULTIPLE-VALUE-BIND and DESTRUCTURING-BIND, one only assigned.

(defun unused-parameter (a b) a)
(defun unused-optional (a &optional (b 1 b-supplied-p) &key c) (list a b))
(defun unused-let () (let ((x 1) (y 2)) x))
(defun unused-let* () (let* ((x 1) (y (1+ x))) x))
(defun unused-value () (multiple-value-bind (q r) (floor 7 2) q))
(defun unused-destructured (list) (destructuring-bind (a b &rest more) list (list a more)))
(defun only-assigned () (let ((n 0)) (setq n 1) nil))

;;; Not told: what is declared IGNORE or IGNORABLE, also in a form inside
;;; the binding (as a macro puts a body's declarations), ...

(defun ignored (a b) (declare (ignore b)) a)
(defun ignorable (a b) (declare (ignorable b)) a)
(defmacro with-two ((first second) &body body)
  `(let ((,first 1)) (let ((,second 2)) ,@body)))
(defun ignored-inside () (with-two (x y) (declare (ignore x)) y))

;;; ... a variable that only the expansion of a MACROLET reads, a variable
;;; named LABELS, a clause of CASE whose key is :IF, ...

(defun read-by-macrolet (this other)
  (macrolet ((same (key) `(equal (getf this ,key) (getf other ,key))))
    (and (same :a) (same :b))))
(defun named-labels (edges)
  (let* ((n (length edges))
         (labels (loop for edge in edges collect (list (third edge) n))))
    labels))
(defun case-of-if (node)
  (case (car node)
    (:if (list 1 2))
    (t nil)))

;;; ... a binding of a special variable, a variable read only by a later
;;; binding, one incremented, a variable a macro makes with GENSYM, and those
;;; of LOOP (it declares them IGNORABLE).

(defvar *options* nil)
(defun option (name) (cdr (assoc name *options* :test #'string=)))
(defun bound-special (options) (let* ((*options* options) (ai (option "ai"))) ai))
(defun read-by-later (x) (let* ((a x) (b (1+ a))) b))
(defun incremented () (let ((n 0)) (incf n) n))
(defmacro twice (form) (let ((g (gensym))) `(let ((,g ,form)) (list 1 2))))
(defun gensym-of-macro () (twice 1))
(defun loop-variables (specs) (loop for (x y . more) in specs collect (list x more)))

;;; Told, and said to be bound by a macro: a variable this form does not name.

(defmacro with-fixture (&body body) `(let ((fixture-a 1) (fixture-b 2)) ,@body))
(defun uses-one-fixture () (with-fixture fixture-a))

