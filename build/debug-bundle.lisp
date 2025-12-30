;;;; debug-bundle.lisp - Debug bundled compilation issue

(require :asdf)
(asdf:load-system :clysm :force nil)

;; Get all modules and forms
(let* ((modules (clysm/stage1:read-all-modules))
       (all-forms (clysm/stage1::all-compilable-forms modules)))
  (format t "~&Total compilable forms: ~D~%" (length all-forms))

  ;; Classify forms to get successful ones
  (multiple-value-bind (successful-sexps results stats)
      (clysm/stage1:classify-forms all-forms)
    (format t "~&Successful: ~D~%" (length successful-sexps))

    ;; Get defuns only
    (let ((defuns (remove-if-not (lambda (sexp)
                                    (and (consp sexp) (eq (car sexp) 'defun)))
                                  successful-sexps)))
      (format t "~&Defuns: ~D~%" (length defuns))

      ;; Show first 5 defuns
      (format t "~%First 5 defuns:~%")
      (loop for defun in (subseq defuns 0 (min 5 (length defuns)))
            for i from 1
            do (format t "~%~D. ~S~%" i (second defun)))

      ;; Test bundling first defun only
      (when defuns
        (format t "~%Testing bundle of first defun...~%")
        (let ((bytes (clysm/stage1:compile-defuns-bundle (list (first defuns)))))
          (if bytes
              (progn
                (format t "  Compiled: ~D bytes~%" (length bytes))
                (if (clysm/stage1:validate-wasm-bytes bytes)
                    (format t "  Validation: PASSED~%")
                    (format t "  Validation: FAILED~%")))
              (format t "  Compilation failed~%")))

        ;; Test first 3 defuns
        (when (>= (length defuns) 3)
          (format t "~%Testing bundle of first 3 defuns...~%")
          (let ((bytes (clysm/stage1:compile-defuns-bundle (subseq defuns 0 3))))
            (if bytes
                (progn
                  (format t "  Compiled: ~D bytes~%" (length bytes))
                  (if (clysm/stage1:validate-wasm-bytes bytes)
                      (format t "  Validation: PASSED~%")
                      (format t "  Validation: FAILED~%")))
                (format t "  Compilation failed~%"))))

        ;; Test each defun individually in a bundle, find first failure
        (format t "~%Testing each defun individually in bundle format:~%")
        (loop for defun in (subseq defuns 0 (min 10 (length defuns)))
              for i from 1
              do (let ((bytes (clysm/stage1:compile-defuns-bundle (list defun))))
                   (format t "~D. ~S: " i (second defun))
                   (if bytes
                       (if (clysm/stage1:validate-wasm-bytes bytes)
                           (format t "~D bytes, VALID~%" (length bytes))
                           (format t "~D bytes, INVALID~%" (length bytes)))
                       (format t "compile-failed~%"))))))))

(quit)
