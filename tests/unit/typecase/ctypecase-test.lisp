;;;; ctypecase-test.lisp - Unit tests for ctypecase macro
;;;; Feature: 030-typecase-macros

(in-package #:clysm/tests/unit/typecase)

;;; ============================================================
;;; T048: Basic ctypecase Dispatch Tests
;;; ============================================================

(deftest ctypecase-matching-clause
  "Test ctypecase dispatches to matching clause"
  (let* ((form '(ctypecase x
                  (integer "int")
                  (symbol "sym")))
         (expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander form)))
    (ok (eq (car expanded) 'loop)
        "Expansion starts with LOOP")
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'integerp (flatten expanded))
          "Contains INTEGERP predicate")
      (ok (member 'symbolp (flatten expanded))
          "Contains SYMBOLP predicate"))))

;;; ============================================================
;;; T049: Type-Error with Store-Value Restart Tests
;;; ============================================================

(deftest ctypecase-type-error-with-store-value
  "Test ctypecase signals type-error with store-value restart when no match"
  (let* ((form '(ctypecase x
                  (integer "int")
                  (symbol "sym")))
         (expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'error (flatten expanded))
          "Expansion contains ERROR call")
      (ok (member 'type-error (flatten expanded))
          "Expansion contains TYPE-ERROR")
      (ok (member 'restart-case (flatten expanded))
          "Expansion contains RESTART-CASE")
      (ok (member 'store-value (flatten expanded))
          "Expansion contains STORE-VALUE restart"))))

;;; ============================================================
;;; T050: Place Update and Re-Dispatch Tests
;;; ============================================================

(deftest ctypecase-place-update
  "Test ctypecase updates place on store-value"
  (let* ((form '(ctypecase x
                  (integer "int")))
         (expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'setf (flatten expanded))
          "Expansion contains SETF for place update"))))

(deftest ctypecase-loop-for-re-dispatch
  "Test ctypecase uses loop for re-dispatch after store-value"
  (let* ((form '(ctypecase x
                  (integer "int")))
         (expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander form)))
    (ok (eq (car expanded) 'loop)
        "ctypecase uses LOOP for re-dispatch")))

;;; ============================================================
;;; T051: Otherwise/T Rejection Tests
;;; ============================================================

(deftest ctypecase-rejects-otherwise
  "Test ctypecase rejects otherwise clause"
  (ok (handler-case
          (progn
            (let* ((form '(ctypecase x
                            (integer "int")
                            (otherwise "default")))
                   (expander (clysm/lib/macros:make-ctypecase-expander)))
              (funcall expander form))
            nil)  ; Should not reach here
        (error () t))
      "ctypecase signals error for otherwise clause"))

(deftest ctypecase-rejects-t-clause
  "Test ctypecase rejects t clause"
  (ok (handler-case
          (progn
            (let* ((form '(ctypecase x
                            (integer "int")
                            (t "catch-all")))
                   (expander (clysm/lib/macros:make-ctypecase-expander)))
              (funcall expander form))
            nil)  ; Should not reach here
        (error () t))
      "ctypecase signals error for t clause"))

;;; ============================================================
;;; Additional ctypecase Tests
;;; ============================================================

(deftest ctypecase-multiple-types-per-clause
  "Test ctypecase with multiple type specifiers per clause"
  (let* ((form '(ctypecase x
                  ((integer symbol) "int-or-sym")
                  (cons "cons")))
         (expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'or (flatten expanded))
          "Multiple types generate OR predicate"))))

(deftest ctypecase-expected-type-in-error
  "Test ctypecase includes expected-type in error"
  (let* ((form '(ctypecase x
                  (integer "int")
                  (symbol "sym")))
         (expander (clysm/lib/macros:make-ctypecase-expander))
         (expanded (funcall expander form)))
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member :expected-type (flatten expanded))
          "Expansion contains :expected-type keyword"))))
