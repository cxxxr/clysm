;;;; image-training.lisp — what bin/clysm runs once in the REPL image started
;;;; from its new heap image (CLYSM_TRAIN_IMAGE): the image's functions that
;;;; run here are named in the heap image's compilationHints section, and V8
;;;; compiles them in background threads with the image instead of one by
;;;; one on the main thread when first called (see "Start-up and loading" in
;;;; docs/INTERNALS.md).  Ordinary use of the REPL: a function compiled and
;;;; called, a macro, a loop, FORMAT.
(defun image-training-fact (n) (if (< n 2) 1 (* n (image-training-fact (1- n)))))
(defmacro image-training-twice (form) `(progn ,form ,form))
(image-training-twice
 (format nil "~D ~A~%" (length (princ-to-string (image-training-fact 30)))
         (loop for i below 10 collect (* i i))))
