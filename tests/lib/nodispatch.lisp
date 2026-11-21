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
;; element types and sequence types upgraded without SUBTYPEP, which the
;; module does not have: a bit for each that is as with SUBTYPEP
(deftype small-char () '(member #\a #\b))
(defun element-types ()
  (let ((r 0) (bit 1))
    (dolist (ok (list (stringp (make-array 2 :element-type '(member #\a #\b) :initial-element #\a))
                      (stringp (make-array 2 :element-type 'small-char :initial-element #\a))
                      (bit-vector-p (make-array 2 :element-type '(or (eql 0) (eql 1)) :initial-element 0))
                      (stringp (make-array 2 :element-type '(or base-char standard-char) :initial-element #\a))
                      (stringp (make-array 2 :element-type '(and character (satisfies graphic-char-p))
                                             :initial-element #\a))
                      (equal (make-string 2 :initial-element #\x :element-type 'small-char) "xx")
                      (equal (concatenate '(vector character) "ab" "c") "abc")
                      (equal (map '(vector small-char) #'identity "ab") "ab")
                      (equal (concatenate '(and list (satisfies consp)) '(1) '(2)) '(1 2))))
      (when ok (setq r (logior r bit)))
      (setq bit (* bit 2)))
    r))
(js:export "elementTypes" #'element-types)
