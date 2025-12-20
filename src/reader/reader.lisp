;;;; reader.lisp - S-expression reader for clysm

(in-package #:clysm/reader)

;;; Reader Interface
;;; We use the host CL's reader for now, with potential customization later.

(defun read-source (source)
  "Read Lisp source from a string. Returns a list of top-level forms."
  (with-input-from-string (stream source)
    (let ((forms nil))
      (do ((form (read stream nil :eof) (read stream nil :eof)))
          ((eq form :eof) (nreverse forms))
        (push form forms)))))

(defun read-file (pathname)
  "Read Lisp source from a file. Returns a list of top-level forms."
  (with-open-file (stream pathname :direction :input)
    (let ((forms nil))
      (do ((form (read stream nil :eof) (read stream nil :eof)))
          ((eq form :eof) (nreverse forms))
        (push form forms)))))
