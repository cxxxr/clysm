;;; A library compiled with --shake-symbols and without the printer
;;; (tests/lib/run.sh, called by tests/lib/symbols-message.mjs): the message
;;; of an unhandled error names a symbol whose name is also that of a symbol
;;; left out of the current package (PB::THING, a function never called) or
;;; of a package it uses (COMMON-LISP:PAIRLIS).  The small printer of
;;; runtime/cond.lisp looks the name up to see how to qualify it, and that is
;;; not the error of FIND-SYMBOL for a name left out.

(defpackage :pa (:use :cl) (:export #:make-thing #:thing-x))
(in-package :pa)
(defstruct (thing (:constructor make-thing (x))) x)

(defpackage :pc (:use))

(defpackage :pb (:use :cl) (:import-from :pa #:make-thing #:thing-x))
(in-package :pb)

(defun thing () 1)

(js:export "currentPackage" (lambda () (package-name *package*)))
(js:export "good" (lambda (v) (thing-x (make-thing v))))
;; a TYPE-ERROR whose expected type is PA::THING
(js:export "notAThing" (lambda (v) (thing-x v)))
(js:export "odd" (lambda (v) (error "~S is odd (~S ~S)" 'pa::thing 'pc::pairlis v)))
;; the names are left out: looking them up is still an error
(js:export "lookUp" (lambda (name) (symbol-name (find-symbol name))))
