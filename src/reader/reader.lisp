;;;; reader.lisp - S-expression reader for cl-wasm

(in-package #:cl-wasm/reader)

;;; Reader Interface
;;; We use the host CL's reader for now, with potential customization later.

(defun read-source (source)
  "Read Lisp source from a string. Returns a list of top-level forms."
  (with-input-from-string (stream source)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun read-file (pathname)
  "Read Lisp source from a file. Returns a list of top-level forms."
  (with-open-file (stream pathname :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))
