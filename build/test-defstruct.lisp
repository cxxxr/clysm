;;;; test-defstruct.lisp - Test defstruct compilation

(require :asdf)
(asdf:load-system :clysm :force nil)

;; Test a simple defstruct
(format t "~%=== Testing simple defstruct ===~%")
(handler-case
    (let ((bytes (clysm:compile-to-wasm '(defstruct point x y))))
      (format t "Compiled: ~D bytes~%" (length bytes))
      (let ((path "/tmp/defstruct-test.wasm"))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence bytes out))
        (multiple-value-bind (output error-output status)
            (uiop:run-program (list "wasm-tools" "validate" path)
                              :output :string :error-output :string :ignore-error-status t)
          (declare (ignore output))
          (if (zerop status)
              (format t "Validation: PASSED~%")
              (format t "Validation: FAILED~%Error: ~A~%" error-output)))))
  (error (e)
    (format t "Compile error: ~A~%" e)))

;; Find and test first actual defstruct from codebase
(format t "~%=== Testing real defstruct from codebase ===~%")
(let* ((modules (clysm/stage1:read-all-modules))
       (all-forms (clysm/stage1::all-compilable-forms modules)))
  (loop for form in all-forms
        when (and (clysm/stage1::source-form-p form)
                  (eq (clysm/stage1:source-form-operator form) 'defstruct))
        do (let ((sexp (clysm/stage1:source-form-sexp form)))
             (format t "~%Defstruct: ~S~%" (second sexp))
             (handler-case
                 (let ((bytes (clysm:compile-to-wasm sexp)))
                   (format t "  Compiled: ~D bytes~%" (length bytes)))
               (error (e)
                 (format t "  Error: ~A~%" e)))
             (return))))

(quit)
