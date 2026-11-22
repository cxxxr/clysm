;;; Every symbol originally interned in this package is removed by
;;; --shake-symbols.  Its empty table must be written with length zero,
;;; rather than the original table's length and references.
(defpackage :empty-symbols (:use :cl))
(in-package :empty-symbols)

(defun unused () 2)

(js:export "one" (lambda () 1))
(js:export "lookup" (lambda (name) (symbol-name (intern name :empty-symbols))))
