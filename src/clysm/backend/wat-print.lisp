;;;; wat-print.lisp - WAT (WebAssembly Text) output for debugging
;;;; Reference: WebAssembly Text Format specification

(in-package #:clysm/backend/wat-print)

(defun print-wat (module &optional (stream *standard-output*))
  "Print a Wasm module in WAT format for debugging."
  (format stream "(module~%")
  ;; TODO: Implement full WAT printing
  (format stream "  ;; Empty module~%")
  (format stream ")~%"))

(defun wat-to-string (module)
  "Convert a Wasm module to WAT string."
  (with-output-to-string (s)
    (print-wat module s)))
