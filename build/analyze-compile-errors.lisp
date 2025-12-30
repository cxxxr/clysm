;;;; analyze-compile-errors.lisp - Categorize compile-time errors

(require :asdf)
(asdf:load-system :clysm :force nil)

(let* ((modules (clysm/stage1:read-all-modules))
       (all-forms (clysm/stage1::all-compilable-forms modules))
       (error-types (make-hash-table :test 'equal)))

  (format t "~%=== Compile Error Analysis ===~%")

  ;; Categorize errors
  (loop for form in all-forms
        for sexp = (clysm/stage1:source-form-sexp form)
        do (handler-case
               (clysm:compile-to-wasm sexp)
             (error (e)
               (let* ((msg (format nil "~A" e))
                      ;; Extract key phrase for categorization
                      (key (cond
                             ((search "Undefined function:" msg)
                              (let* ((start (+ (search "Undefined function:" msg) 20))
                                     (end (or (position #\Newline msg :start start)
                                              (min (+ start 40) (length msg)))))
                                (format nil "Undefined: ~A" (subseq msg start end))))
                             ((search "not of type" msg) "Type error")
                             ((search "no block named" msg) "Block not found")
                             ((search "DEFSTRUCT" msg) "DEFSTRUCT unsupported")
                             ((search "DEFMACRO" msg) "DEFMACRO unsupported")
                             ((search "DEFINE-CONDITION" msg) "DEFINE-CONDITION unsupported")
                             ((search "Unknown special" msg) "Unknown special operator")
                             ((search "is not" msg) "Type/value error")
                             (t (subseq msg 0 (min 60 (length msg)))))))
                 (incf (gethash key error-types 0))))))

  ;; Print summary
  (format t "~%Error categories (top 20):~%")
  (format t "~50A ~8A~%" "Error Type" "Count")
  (format t "~50A ~8A~%" "----------" "-----")

  (let ((entries nil))
    (maphash (lambda (k v) (push (cons k v) entries)) error-types)
    (setf entries (sort entries #'> :key #'cdr))
    (loop for (type . count) in (subseq entries 0 (min 20 (length entries)))
          do (format t "~50A ~8D~%" type count))
    (format t "~%Total error categories: ~D~%" (length entries))))

(quit)
