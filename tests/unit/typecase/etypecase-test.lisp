;;;; etypecase-test.lisp - Unit tests for etypecase macro
;;;; Feature: 030-typecase-macros

(in-package #:clysm/tests/unit/typecase)

;;; ============================================================
;;; T022: Basic etypecase Dispatch Tests
;;; ============================================================

(deftest etypecase-matching-clause
  "Test etypecase dispatches to matching clause"
  (let* ((form '(etypecase x
                  (integer "int")
                  (symbol "sym")))
         (expander (clysm/lib/macros:make-etypecase-expander))
         (expanded (funcall expander form)))
    (ok (eq (car expanded) 'let)
        "Expansion starts with LET")
    (ok (member 'integerp (flatten expanded))
        "Contains INTEGERP predicate")
    (ok (member 'symbolp (flatten expanded))
        "Contains SYMBOLP predicate")))

;;; ============================================================
;;; T023: Type-Error Signaling Tests
;;; ============================================================

(deftest etypecase-type-error-when-no-match
  "Test etypecase signals type-error when no clause matches"
  (let* ((form '(etypecase x
                  (integer "int")
                  (symbol "sym")))
         (expander (clysm/lib/macros:make-etypecase-expander))
         (expanded (funcall expander form)))
    ;; Should contain error signaling
    (ok (member 'error (flatten expanded))
        "Expansion contains ERROR call")
    (ok (member 'type-error (flatten expanded))
        "Expansion contains TYPE-ERROR")))

;;; ============================================================
;;; T024: Expected-Type Construction Tests
;;; ============================================================

(deftest etypecase-expected-type-construction
  "Test construct-expected-type produces correct (or ...) form"
  (let ((expected (clysm/lib/macros:construct-expected-type
                   '((integer 1)
                     (symbol 2)
                     (cons 3)))))
    (ok (eq (car expected) 'or)
        "Expected type is an OR form")
    (ok (member 'integer (cdr expected))
        "Contains INTEGER type")
    (ok (member 'symbol (cdr expected))
        "Contains SYMBOL type")
    (ok (member 'cons (cdr expected))
        "Contains CONS type")))

(deftest etypecase-expected-type-single
  "Test construct-expected-type with single type returns just that type"
  (let ((expected (clysm/lib/macros:construct-expected-type
                   '((integer 1)))))
    (ok (eq expected 'integer)
        "Single type returns atomic type, not (or type)")))

;;; ============================================================
;;; T025: Otherwise/T Rejection Tests
;;; ============================================================

(deftest etypecase-rejects-otherwise
  "Test etypecase rejects otherwise clause"
  (ok (handler-case
          (progn
            (let* ((form '(etypecase x
                            (integer "int")
                            (otherwise "default")))
                   (expander (clysm/lib/macros:make-etypecase-expander)))
              (funcall expander form))
            nil)  ; Should not reach here
        (error () t))
      "etypecase signals error for otherwise clause"))

(deftest etypecase-rejects-t-clause
  "Test etypecase rejects t clause"
  (ok (handler-case
          (progn
            (let* ((form '(etypecase x
                            (integer "int")
                            (t "catch-all")))
                   (expander (clysm/lib/macros:make-etypecase-expander)))
              (funcall expander form))
            nil)  ; Should not reach here
        (error () t))
      "etypecase signals error for t clause"))

(deftest validate-exhaustive-clauses-rejects-otherwise
  "Test validate-exhaustive-clauses helper function"
  (ok (handler-case
          (progn
            (clysm/lib/macros:validate-exhaustive-clauses
             '((integer 1) (otherwise 2))
             'etypecase)
            nil)
        (error () t))
      "validate-exhaustive-clauses rejects otherwise"))

(deftest validate-exhaustive-clauses-rejects-t
  "Test validate-exhaustive-clauses helper function with t"
  (ok (handler-case
          (progn
            (clysm/lib/macros:validate-exhaustive-clauses
             '((integer 1) (t 2))
             'etypecase)
            nil)
        (error () t))
      "validate-exhaustive-clauses rejects t"))

;;; ============================================================
;;; Additional etypecase Tests
;;; ============================================================

(deftest etypecase-multiple-types-per-clause
  "Test etypecase with multiple type specifiers per clause"
  (let* ((form '(etypecase x
                  ((integer symbol) "int-or-sym")
                  (cons "cons")))
         (expander (clysm/lib/macros:make-etypecase-expander))
         (expanded (funcall expander form)))
    (ok (member 'or (flatten expanded))
        "Multiple types generate OR predicate")))

(deftest etypecase-gensym-keyvar
  "Test etypecase uses gensym for key variable"
  (let* ((form '(etypecase (foo)
                  (integer "int")))
         (expander (clysm/lib/macros:make-etypecase-expander))
         (expanded (funcall expander form)))
    (let* ((bindings (second expanded))
           (key-var (first (first bindings))))
      (ok (symbolp key-var)
          "Key variable is a symbol")
      (ok (null (symbol-package key-var))
          "Key variable is a gensym (uninterned)"))))
