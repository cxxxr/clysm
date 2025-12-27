;;;; typecase-ansi-test.lisp - ANSI CL compliance tests for typecase
;;;; Feature: 030-typecase-macros

(in-package #:clysm/tests/integration/typecase-ansi)

;;; ============================================================
;;; T075: ANSI Typecase Compliance Tests
;;; ============================================================

(deftest ansi-typecase-basic-dispatch
  "ANSI CL: typecase basic type dispatch"
  ;; Test that typecase expander produces valid expansion
  (let* ((expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander '(typecase x (integer 1) (symbol 2)))))
    (ok (eq (car expanded) 'let)
        "typecase expands to LET form")))

(deftest ansi-typecase-otherwise
  "ANSI CL: typecase otherwise clause"
  (let* ((expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander '(typecase x (integer 1) (otherwise 0)))))
    (ok (eq (car expanded) 'let)
        "typecase with otherwise expands correctly")))

(deftest ansi-typecase-nil-return
  "ANSI CL: typecase returns NIL when no match"
  (let* ((expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander '(typecase x (integer 1)))))
    ;; Without otherwise, the final else should be NIL
    (let* ((body (third expanded))
           (else-branch (fourth body)))
      (ok (null else-branch)
          "typecase without otherwise returns NIL on no match"))))

;;; ============================================================
;;; T076: ANSI Etypecase Compliance Tests
;;; ============================================================

(deftest ansi-etypecase-basic-dispatch
  "ANSI CL: etypecase basic type dispatch"
  (let* ((expander (clysm/lib/macros:make-etypecase-expander))
         (expanded (funcall expander '(etypecase x (integer 1) (symbol 2)))))
    (ok (eq (car expanded) 'let)
        "etypecase expands to LET form")))

(deftest ansi-etypecase-type-error
  "ANSI CL: etypecase signals type-error when no clause matches"
  (let* ((expander (clysm/lib/macros:make-etypecase-expander))
         (expanded (funcall expander '(etypecase x (integer 1) (symbol 2)))))
    ;; Should contain ERROR and TYPE-ERROR somewhere in expansion
    (let ((flat (flatten expanded)))
      (ok (member 'error flat)
          "etypecase expansion contains ERROR")
      (ok (member 'type-error flat)
          "etypecase expansion signals TYPE-ERROR"))))

(deftest ansi-etypecase-expected-type
  "ANSI CL: etypecase provides expected-type in type-error"
  (let ((expected (clysm/lib/macros:construct-expected-type
                   '((integer 1) (symbol 2) (cons 3)))))
    (ok (eq (car expected) 'or)
        "Expected type is (or ...) form")
    (ok (= 4 (length expected))
        "Expected type includes all clause types")))

;;; ============================================================
;;; T077: ANSI Ctypecase Compliance Tests
;;; ============================================================

(deftest ansi-ctypecase-basic-dispatch
  "ANSI CL: ctypecase basic type dispatch"
  (let* ((expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander '(ctypecase x (integer 1) (symbol 2)))))
    (ok (eq (car expanded) 'loop)
        "ctypecase expands to LOOP form (for restart re-dispatch)")))

(deftest ansi-ctypecase-store-value-restart
  "ANSI CL: ctypecase provides store-value restart"
  (let* ((expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander '(ctypecase x (integer 1)))))
    (let ((flat (flatten expanded)))
      (ok (member 'restart-case flat)
          "ctypecase expansion contains RESTART-CASE")
      (ok (member 'store-value flat)
          "ctypecase expansion contains STORE-VALUE restart"))))

(deftest ansi-ctypecase-loop-re-dispatch
  "ANSI CL: ctypecase re-dispatches after store-value"
  (let* ((expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander '(ctypecase x (integer 1)))))
    (let ((flat (flatten expanded)))
      (ok (member 'loop flat)
          "ctypecase expansion contains LOOP for re-dispatch"))))

;;; ============================================================
;;; T078: ANSI Check-type Compliance Tests
;;; ============================================================

(deftest ansi-check-type-success
  "ANSI CL: check-type returns NIL when type matches"
  (let* ((expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander '(check-type x integer))))
    (ok (eq (car expanded) 'loop)
        "check-type expands to LOOP form (for restart re-check)")))

(deftest ansi-check-type-type-error
  "ANSI CL: check-type signals type-error when type doesn't match"
  (let* ((expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander '(check-type x integer))))
    (let ((flat (flatten expanded)))
      (ok (member 'error flat)
          "check-type expansion contains ERROR")
      (ok (member 'type-error flat)
          "check-type expansion signals TYPE-ERROR"))))

(deftest ansi-check-type-store-value-restart
  "ANSI CL: check-type provides store-value restart"
  (let* ((expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander '(check-type x integer))))
    (let ((flat (flatten expanded)))
      (ok (member 'restart-case flat)
          "check-type expansion contains RESTART-CASE")
      (ok (member 'store-value flat)
          "check-type expansion contains STORE-VALUE restart"))))

(deftest ansi-check-type-loop-re-check
  "ANSI CL: check-type re-checks after store-value"
  (let* ((expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander '(check-type x integer))))
    (let ((flat (flatten expanded)))
      (ok (member 'loop flat)
          "check-type expansion contains LOOP for re-checking"))))

(deftest ansi-check-type-optional-string
  "ANSI CL: check-type accepts optional type-string"
  (let* ((expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander '(check-type x integer "a positive integer"))))
    (ok (eq (car expanded) 'loop)
        "check-type with type-string expands to LOOP")))
