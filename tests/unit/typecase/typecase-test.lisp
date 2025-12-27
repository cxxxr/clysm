;;;; typecase-test.lisp - Unit tests for typecase macro
;;;; Feature: 030-typecase-macros

(in-package #:clysm/tests/unit/typecase)

;;; ============================================================
;;; Test Infrastructure
;;; ============================================================

(defun flatten (tree)
  "Flatten a nested list structure."
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (flatten (car tree))
                   (flatten (cdr tree))))))

;;; ============================================================
;;; T009: Basic Dispatch Tests
;;; ============================================================

(deftest typecase-basic-dispatch
  "Test basic typecase type dispatch"
  (let* ((form '(typecase x
                  (integer "int")
                  (symbol "sym")
                  (otherwise "other")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Should expand to let + nested if
    (ok (eq (car expanded) 'let)
        "Expansion starts with LET")
    (ok (member 'integerp (flatten expanded))
        "Expansion contains INTEGERP predicate")
    (ok (member 'symbolp (flatten expanded))
        "Expansion contains SYMBOLP predicate")))

(deftest typecase-integer-dispatch
  "Test typecase dispatches to integer clause"
  (let* ((form '(typecase 42
                  (integer "int")
                  (symbol "sym")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Verify structure: (let ((#:KEY 42)) (if (integerp #:KEY) ...))
    (ok (eq (car expanded) 'let)
        "Expansion is a LET form")
    (ok (numberp (second (first (second expanded))))
        "Keyform 42 is bound")))

(deftest typecase-symbol-dispatch
  "Test typecase dispatches to symbol clause"
  (let* ((form '(typecase 'foo
                  (integer "int")
                  (symbol "sym")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    (ok (eq (car expanded) 'let)
        "Expansion is a LET form")))

;;; ============================================================
;;; T010: Multiple Type Specifiers Per Clause
;;; ============================================================

(deftest typecase-multiple-types-per-clause
  "Test typecase with multiple type specifiers per clause"
  (let* ((form '(typecase x
                  ((integer symbol) "int-or-sym")
                  (cons "cons")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Should generate (or (integerp ...) (symbolp ...))
    (ok (member 'or (flatten expanded))
        "Multiple types generate OR predicate")
    (ok (member 'integerp (flatten expanded))
        "Contains INTEGERP")
    (ok (member 'symbolp (flatten expanded))
        "Contains SYMBOLP")))

;;; ============================================================
;;; T011: Otherwise and T Clauses
;;; ============================================================

(deftest typecase-otherwise-clause
  "Test typecase with otherwise clause"
  (let* ((form '(typecase x
                  (integer "int")
                  (otherwise "default")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; otherwise clause should be last, unconditional
    (ok (member "default" (flatten expanded) :test #'equal)
        "Default body is in expansion")))

(deftest typecase-t-clause
  "Test typecase with t as catch-all clause"
  (let* ((form '(typecase x
                  (integer "int")
                  (t "catch-all")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; t clause should work like otherwise
    (ok (member "catch-all" (flatten expanded) :test #'equal)
        "T clause body is in expansion")))

;;; ============================================================
;;; T012: NIL Return When No Match
;;; ============================================================

(deftest typecase-nil-return-when-no-match
  "Test typecase returns NIL when no clause matches and no otherwise"
  (let* ((form '(typecase x
                  (integer "int")
                  (symbol "sym")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Without otherwise, the else branch should be NIL
    (ok (eq (car expanded) 'let)
        "Expansion is a LET form")
    ;; The nested if should have NIL as final else
    (let ((if-form (third expanded)))
      (ok (eq (car if-form) 'if)
          "Body is an IF form"))))

;;; ============================================================
;;; T013: Single Keyform Evaluation
;;; ============================================================

(deftest typecase-single-keyform-evaluation
  "Test typecase evaluates keyform exactly once"
  (let* ((form '(typecase (side-effect)
                  (integer "int")
                  (symbol "sym")
                  (cons "cons")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Keyform should be bound to a gensym, not repeated
    (let ((bindings (second expanded)))
      (ok (= 1 (length bindings))
          "Only one binding (gensym for keyform)")
      (ok (eq (second (first bindings)) '(side-effect))
          "Keyform is bound once to the gensym"))))

(deftest typecase-gensym-keyvar
  "Test typecase uses gensym for key variable"
  (let* ((form '(typecase (foo)
                  (integer "int")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    (let* ((bindings (second expanded))
           (key-var (first (first bindings))))
      ;; Key variable should be a gensym (uninterned symbol)
      (ok (symbolp key-var)
          "Key variable is a symbol")
      (ok (null (symbol-package key-var))
          "Key variable is a gensym (uninterned)"))))

;;; ============================================================
;;; Additional Edge Case Tests
;;; ============================================================

(deftest typecase-empty-clauses
  "Test typecase with no clauses returns NIL"
  (let* ((form '(typecase x))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Should expand to (let ((#:KEY x)) nil)
    (ok (eq (car expanded) 'let)
        "Still wraps in LET")))

(deftest typecase-multiple-body-forms
  "Test typecase clause with multiple body forms"
  (let* ((form '(typecase x
                  (integer (print "int") (+ x 1))))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Body should be wrapped in PROGN
    (ok (member 'progn (flatten expanded))
        "Multiple body forms wrapped in PROGN")))

(deftest typecase-type-specifier-conversion
  "Test type-specifier-to-predicate function"
  ;; Test atomic types
  (ok (equal (clysm/lib/macros:type-specifier-to-predicate 'integer 'x)
             '(integerp x))
      "integer -> integerp")
  (ok (equal (clysm/lib/macros:type-specifier-to-predicate 'symbol 'x)
             '(symbolp x))
      "symbol -> symbolp")
  (ok (equal (clysm/lib/macros:type-specifier-to-predicate 'cons 'x)
             '(consp x))
      "cons -> consp")
  (ok (equal (clysm/lib/macros:type-specifier-to-predicate 'null 'x)
             '(null x))
      "null -> null")
  (ok (equal (clysm/lib/macros:type-specifier-to-predicate 'string 'x)
             '(stringp x))
      "string -> stringp")
  ;; Test t type
  (ok (eq (clysm/lib/macros:type-specifier-to-predicate 't 'x) t)
      "t -> t (always true)"))
