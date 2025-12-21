;;;; printer.lisp - Object printer

(in-package #:clysm/runtime/printer)

(defun print* (object &optional (stream *standard-output*))
  "Print an object with newline."
  (prin1* object stream)
  (terpri stream)
  object)

(defun prin1* (object &optional (stream *standard-output*))
  "Print an object readably."
  (prin1 object stream)
  object)

(defun princ* (object &optional (stream *standard-output*))
  "Print an object for human consumption."
  (princ object stream)
  object)
