;;;; helpers.lisp - Test helper functions

(in-package #:clysm/tests/helpers)

(defun compile-and-run (expr)
  "Compile an expression to Wasm and run it with wasmtime.
   Returns the result of execution."
  ;; TODO: Implement full compilation and execution
  (declare (ignore expr))
  nil)

(defun validate-wasm (bytes)
  "Validate Wasm bytes using wasm-tools.
   Returns T if valid, signals error otherwise."
  (let* ((temp-file (format nil "/tmp/clysm-test-~A.wasm" (random 1000000))))
    (unwind-protect
         (progn
           (with-open-file (stream temp-file
                                   :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
             (write-sequence bytes stream))
           (let ((result (uiop:run-program
                          (list "wasm-tools" "validate" temp-file)
                          :ignore-error-status t)))
             (zerop result)))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(defmacro with-temp-wasm-file ((var bytes) &body body)
  "Execute body with a temporary Wasm file."
  (let ((temp-file (gensym)))
    `(let* ((,temp-file (format nil "/tmp/clysm-test-~A.wasm" (random 1000000)))
            (,var ,temp-file))
       (unwind-protect
            (progn
              (with-open-file (stream ,temp-file
                                      :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
                (write-sequence ,bytes stream))
              ,@body)
         (when (probe-file ,temp-file)
           (delete-file ,temp-file))))))
