;;;; analyze-validation.lisp - Analyze which forms pass/fail validation

(require :asdf)
(asdf:load-system :clysm :force nil)

(let* ((modules (clysm/stage1:read-all-modules))
       (all-forms (clysm/stage1::all-compilable-forms modules))
       (operator-stats (make-hash-table :test 'eq)))

  ;; Test each form
  (loop for form in all-forms
        for sexp = (clysm/stage1:source-form-sexp form)
        for operator = (clysm/stage1:source-form-operator form)
        do (progn
             (unless (gethash operator operator-stats)
               (setf (gethash operator operator-stats) (cons 0 0)))
             (handler-case
                 (let ((bytes (clysm:compile-to-wasm sexp)))
                   (if (clysm/stage1:validate-wasm-bytes bytes)
                       (incf (car (gethash operator operator-stats)))
                       (incf (cdr (gethash operator operator-stats)))))
               (error ()
                 (incf (cdr (gethash operator operator-stats)))))))

  ;; Print results
  (format t "~%=== Validation Results by Operator ===~%")
  (format t "~30A ~8A ~8A ~8A~%" "Operator" "Valid" "Invalid" "Rate")
  (format t "~30A ~8A ~8A ~8A~%" "--------" "-----" "-------" "----")

  (let ((entries nil))
    (maphash (lambda (k v)
               (push (list k (car v) (cdr v)) entries))
             operator-stats)
    ;; Sort by valid count descending
    (setf entries (sort entries #'> :key #'second))

    (loop for (op valid invalid) in entries
          for total = (+ valid invalid)
          for rate = (if (zerop total) 0.0 (* 100.0 (/ valid total)))
          do (format t "~30S ~8D ~8D ~6,1F%~%" op valid invalid rate))

    (format t "~%Total valid: ~D~%"
            (reduce #'+ entries :key #'second))
    (format t "Total invalid: ~D~%"
            (reduce #'+ entries :key #'third))))

(quit)
