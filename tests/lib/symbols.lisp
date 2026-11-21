;;; A library compiled with --shake-symbols (tests/lib/run.sh, called by
;;; tests/lib/symbols.mjs): the symbols that its code does not refer to are
;;; left out, and looking one of them up by name is an error.

(defpackage :shapes (:use :cl) (:export #:circle #:square #:area #:unused-export))
(in-package :shapes)

(defun area (kind size)
  (ecase kind
    (circle (* 3 size size))
    (square (* size size))))

(defun make-shape (&key kind (size 1)) (list kind size))

(defun never-called (x) (list x x))

(in-package :cl-user)

;; a name made at run time finds the symbol the code refers to
(js:export "areaOf" (lambda (kind size)
                      (shapes:area (find-symbol (string-upcase kind) :shapes) size)))
;; a keyword made at run time is the one of the &key parameter
(js:export "keyArg" (lambda (name)
                      (second (apply #'shapes::make-shape (intern (string-upcase name) :keyword) '(5)))))
;; the printer and the reader
(js:export "printed" (lambda () (format nil "~S ~S ~S" 'shapes:circle :size 'car)))
(js:export "readLength" (lambda (text) (length (read-from-string text))))
(js:export "readSquare" (lambda () (shapes:area (let ((*package* (find-package :shapes)))
                                                    (read-from-string "square"))
                                                  4)))
;; names nothing refers to: an error, not a new symbol
(js:export "lookUp" (lambda (name package)
                      (multiple-value-bind (s status) (find-symbol (string-upcase name) (string-upcase package))
                        (format nil "~S ~S" s status))))
(js:export "internNew" (lambda (name) (symbol-name (intern (string-upcase name) :cl-user))))
(js:export "symbolCount" (lambda () (let ((n 0)) (do-all-symbols (s) (declare (ignore s)) (incf n)) n)))
