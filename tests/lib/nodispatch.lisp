;;; A shaken library that defines no class and no method (tests/lib/run.sh):
;;; the generic functions it calls have no dispatch in the module (those of
;;; the runtime with one method that specializes nothing call it, those
;;; without methods signal that none applies); the results must be those
;;; of the module with the dispatch.
(defun missing ()
  (handler-case (slot-value (make-condition 'simple-error) 'no-such-slot)
    (error (e) (symbol-name (type-of e)))))
(defun missing-message ()
  (handler-case (slot-value (make-condition 'simple-error) 'no-such-slot)
    (simple-error (e) (simple-condition-format-control e))))
(js:export "missing" #'missing)
(js:export "missingMessage" #'missing-message)
