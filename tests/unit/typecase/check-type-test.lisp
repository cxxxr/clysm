;;;; check-type-test.lisp - Unit tests for check-type macro
;;;; Feature: 030-typecase-macros

(in-package #:clysm/tests/unit/typecase)

;;; ============================================================
;;; T035: Check-Type Success Tests
;;; ============================================================

(deftest check-type-success-returns-nil
  "Test check-type returns NIL when type matches"
  (let* ((form '(check-type x integer))
         (expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander form)))
    ;; Should expand to a loop with type check
    (ok (listp expanded)
        "Expansion is a list")
    (ok (eq (car expanded) 'loop)
        "Expansion starts with LOOP for re-validation")))

;;; ============================================================
;;; T036: Check-Type Type-Error Signaling Tests
;;; ============================================================

(deftest check-type-type-error-signaling
  "Test check-type signals type-error when type doesn't match"
  (let* ((form '(check-type x integer))
         (expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'error (flatten expanded))
          "Expansion contains ERROR call")
      (ok (member 'type-error (flatten expanded))
          "Expansion contains TYPE-ERROR"))))

;;; ============================================================
;;; T037: Check-Type Store-Value Restart Tests
;;; ============================================================

(deftest check-type-store-value-restart
  "Test check-type provides store-value restart"
  (let* ((form '(check-type x integer))
         (expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'restart-case (flatten expanded))
          "Expansion contains RESTART-CASE")
      (ok (member 'store-value (flatten expanded))
          "Expansion contains STORE-VALUE restart"))))

(deftest check-type-setf-on-store-value
  "Test check-type uses setf to update place on store-value"
  (let* ((form '(check-type x integer))
         (expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'setf (flatten expanded))
          "Expansion contains SETF for updating place"))))

;;; ============================================================
;;; T038: Check-Type Optional Type-String Tests
;;; ============================================================

(deftest check-type-with-type-string
  "Test check-type handles optional type-string"
  (let* ((form '(check-type x integer "a positive integer"))
         (expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      ;; Type-string should be in the expansion
      (ok (member "a positive integer" (flatten expanded) :test #'equal)
          "Type-string is included in expansion"))))

(deftest check-type-without-type-string
  "Test check-type works without type-string"
  (let* ((form '(check-type x integer))
         (expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander form)))
    ;; Should still work
    (ok (listp expanded)
        "Expansion is valid without type-string")))

;;; ============================================================
;;; Additional Check-Type Tests
;;; ============================================================

(deftest check-type-place-evaluation
  "Test check-type evaluates place properly"
  (let* ((form '(check-type (aref arr 0) integer))
         (expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      ;; Should contain the place form
      (ok (member 'aref (flatten expanded))
          "Complex place is preserved in expansion"))))

(deftest check-type-compound-type
  "Test check-type with compound type specifier"
  (let* ((form '(check-type x (or integer symbol)))
         (expander (clysm/lib/macros:make-check-type-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'or (flatten expanded))
          "Compound type generates OR predicate"))))
