;;;; compile-only-stats.lisp - Check compilation rate without validation

(require :asdf)
(asdf:load-system :clysm :force nil)

(let* ((modules (clysm/stage1:read-all-modules))
       (all-forms (clysm/stage1::all-compilable-forms modules))
       (compiled 0)
       (failed 0)
       (operator-stats (make-hash-table :test 'eq)))

  (format t "~%=== Compile-Only Statistics (no validation) ===~%")

  (loop for form in all-forms
        for sexp = (clysm/stage1:source-form-sexp form)
        for operator = (clysm/stage1:source-form-operator form)
        do (unless (gethash operator operator-stats)
             (setf (gethash operator operator-stats) (cons 0 0)))
           (handler-case
               (progn
                 (clysm:compile-to-wasm sexp)
                 (incf compiled)
                 (incf (car (gethash operator operator-stats))))
             (error ()
               (incf failed)
               (incf (cdr (gethash operator operator-stats))))))

  (format t "~%Total: ~D forms~%" (+ compiled failed))
  (format t "Compiled (no validation): ~D (~,1F%)~%"
          compiled (* 100.0 (/ compiled (+ compiled failed))))
  (format t "Failed: ~D~%" failed)

  (format t "~%By operator:~%")
  (format t "~30A ~10A ~10A ~10A~%" "Operator" "Compiled" "Failed" "Rate")
  (let ((entries nil))
    (maphash (lambda (k v) (push (list k (car v) (cdr v)) entries)) operator-stats)
    (setf entries (sort entries #'> :key #'second))
    (loop for (op ok fail) in entries
          for total = (+ ok fail)
          do (format t "~30S ~10D ~10D ~9,1F%~%" op ok fail
                     (if (zerop total) 0.0 (* 100.0 (/ ok total)))))))

(quit)
