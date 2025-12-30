;;;; debug-validation.lisp - Check why single defun bundle fails validation

(require :asdf)
(asdf:load-system :clysm :force nil)

;; Test a simple defun wrapped in progn
(format t "~%=== Testing simple defun ===~%")
(let ((bytes (handler-case
                 (clysm:compile-to-wasm '(defun test-add (x y) (+ x y)))
               (error (e)
                 (format t "Compile error: ~A~%" e)
                 nil))))
  (when bytes
    (format t "Compiled defun: ~D bytes~%" (length bytes))
    ;; Write to temp file and validate
    (let ((path "/tmp/test-defun.wasm"))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence bytes out))
      (multiple-value-bind (output error-output status)
          (uiop:run-program (list "wasm-tools" "validate" path)
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (declare (ignore output))
        (format t "Validation status: ~D~%" status)
        (when (not (zerop status))
          (format t "Error: ~A~%" error-output))))))

;; Test a bundle with progn
(format t "~%=== Testing progn bundle ===~%")
(let ((bytes (handler-case
                 (clysm:compile-to-wasm '(progn
                                           (defun test-add (x y) (+ x y))))
               (error (e)
                 (format t "Compile error: ~A~%" e)
                 nil))))
  (when bytes
    (format t "Compiled bundle: ~D bytes~%" (length bytes))
    ;; Write to temp file and validate
    (let ((path "/tmp/test-bundle.wasm"))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence bytes out))
      (multiple-value-bind (output error-output status)
          (uiop:run-program (list "wasm-tools" "validate" path)
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (declare (ignore output))
        (format t "Validation status: ~D~%" status)
        (when (not (zerop status))
          (format t "Error: ~A~%" error-output))))))

;; Test with actual compiler defun (ENCODE-UNSIGNED-LEB128)
(format t "~%=== Testing compiler defun ===~%")
(let* ((modules (clysm/stage1:read-all-modules))
       (all-forms (clysm/stage1::all-compilable-forms modules))
       (defun-form nil))
  ;; Find first successful defun
  (loop for form in all-forms
        when (and (clysm/stage1::source-form-p form)
                  (eq (car (clysm/stage1:source-form-sexp form)) 'defun))
        do (handler-case
               (progn
                 (clysm:compile-to-wasm (clysm/stage1:source-form-sexp form))
                 (setf defun-form form)
                 (return))
             (error () nil)))

  (when defun-form
    (let* ((sexp (clysm/stage1:source-form-sexp defun-form))
           (name (second sexp)))
      (format t "Found compilable defun: ~A~%" name)

      ;; Test single defun
      (format t "~%Direct defun compilation:~%")
      (let ((bytes (clysm:compile-to-wasm sexp)))
        (format t "  Size: ~D bytes~%" (length bytes))
        (let ((path "/tmp/direct-defun.wasm"))
          (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (write-sequence bytes out))
          (multiple-value-bind (output error-output status)
              (uiop:run-program (list "wasm-tools" "validate" path)
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output))
            (format t "  Validation: ~A~%" (if (zerop status) "VALID" "INVALID"))
            (when (not (zerop status))
              (format t "  Error: ~A~%" error-output)))))

      ;; Test wrapped in progn
      (format t "~%Progn-wrapped compilation:~%")
      (let ((bytes (clysm:compile-to-wasm `(progn ,sexp))))
        (format t "  Size: ~D bytes~%" (length bytes))
        (let ((path "/tmp/progn-defun.wasm"))
          (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (write-sequence bytes out))
          (multiple-value-bind (output error-output status)
              (uiop:run-program (list "wasm-tools" "validate" path)
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output))
            (format t "  Validation: ~A~%" (if (zerop status) "VALID" "INVALID"))
            (when (not (zerop status))
              (format t "  Error: ~A~%" error-output))))))))

(quit)
