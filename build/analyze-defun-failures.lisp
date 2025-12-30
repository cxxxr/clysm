;;;; analyze-defun-failures.lisp - Analyze specific defun validation failures

(require :asdf)
(asdf:load-system :clysm :force nil)

(let* ((modules (clysm/stage1:read-all-modules))
       (all-forms (clysm/stage1::all-compilable-forms modules))
       (defuns (remove-if-not (lambda (f)
                                (eq (clysm/stage1:source-form-operator f) 'defun))
                              all-forms))
       (failures nil))

  ;; Find first 10 failing defuns with their error messages
  (format t "~%=== Analyzing DEFUN Validation Failures ===~%")
  (loop for form in defuns
        for i from 1
        while (< (length failures) 10)
        do (let ((sexp (clysm/stage1:source-form-sexp form)))
             (handler-case
                 (let ((bytes (clysm:compile-to-wasm sexp)))
                   ;; Write to temp file and get validation error
                   (let ((path (format nil "/tmp/defun-test-~D.wasm" i)))
                     (with-open-file (out path :direction :output
                                          :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)
                       (write-sequence bytes out))
                     (multiple-value-bind (output error-output status)
                         (uiop:run-program (list "wasm-tools" "validate" path)
                                           :output :string
                                           :error-output :string
                                           :ignore-error-status t)
                       (declare (ignore output))
                       (unless (zerop status)
                         (push (list (second sexp) error-output sexp) failures)))))
               (error (e)
                 (push (list (if (consp sexp) (second sexp) "?")
                             (format nil "Compile error: ~A" e)
                             sexp)
                       failures)))))

  ;; Print failures
  (format t "~%First 10 failing defuns:~%")
  (loop for (name error sexp) in (reverse failures)
        for i from 1
        do (format t "~%~D. ~A~%" i name)
           (format t "   Error: ~A~%" (subseq error 0 (min 200 (length error))))
           (format t "   Arglist: ~S~%" (third sexp))
           (format t "   Body preview: ~S~%"
                   (if (> (length (cdddr sexp)) 0)
                       (subseq (format nil "~S" (fourth sexp)) 0
                               (min 80 (length (format nil "~S" (fourth sexp)))))
                       "(empty)"))))

(quit)
