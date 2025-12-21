;;;; parser.lisp - S-expression parsing

(in-package #:clysm/reader/parser)

(define-condition parse-error (error)
  ((message :initarg :message :reader parse-error-message)
   (line :initarg :line :reader parse-error-line)
   (column :initarg :column :reader parse-error-column))
  (:report (lambda (c s)
             (format s "Parse error at line ~D, column ~D: ~A"
                     (parse-error-line c)
                     (parse-error-column c)
                     (parse-error-message c)))))

(defun parse (tokens)
  "Parse tokens into S-expressions."
  ;; TODO: Implement
  (declare (ignore tokens))
  nil)
