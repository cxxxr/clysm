;;;; compound-types-test.lisp - Unit tests for compound type specifiers
;;;; Feature: 030-typecase-macros

(in-package #:clysm/tests/unit/typecase)

;;; ============================================================
;;; T061: OR Compound Type Specifier Tests
;;; ============================================================

(deftest compound-or-type-specifier
  "Test (or type1 type2) compound specifier"
  (let ((or-form (clysm/lib/macros:type-specifier-to-predicate
                  '(or integer symbol) 'x)))
    (ok (eq (car or-form) 'or)
        "OR compound produces OR form")
    (ok (= 3 (length or-form))
        "OR form has 3 elements (or pred1 pred2)")
    (ok (equal (second or-form) '(integerp x))
        "First predicate is (integerp x)")
    (ok (equal (third or-form) '(symbolp x))
        "Second predicate is (symbolp x)")))

(deftest compound-or-in-typecase
  "Test (or ...) specifier works in typecase"
  (let* ((form '(typecase x
                  ((or integer symbol) "int-or-sym")
                  (cons "cons")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    (ok (listp expanded)
        "Expansion is valid")))

;;; ============================================================
;;; T062: AND Compound Type Specifier Tests
;;; ============================================================

(deftest compound-and-type-specifier
  "Test (and type1 type2) compound specifier"
  (let ((and-form (clysm/lib/macros:type-specifier-to-predicate
                   '(and number (satisfies evenp)) 'x)))
    (ok (eq (car and-form) 'and)
        "AND compound produces AND form")
    (ok (= 3 (length and-form))
        "AND form has 3 elements")))

(deftest compound-and-multiple-types
  "Test (and ...) with multiple types"
  (let ((and-form (clysm/lib/macros:type-specifier-to-predicate
                   '(and integer (not null)) 'x)))
    (ok (eq (car and-form) 'and)
        "AND produces AND form")
    (ok (member 'integerp (flatten and-form))
        "Contains integerp")))

;;; ============================================================
;;; T063: NOT Compound Type Specifier Tests
;;; ============================================================

(deftest compound-not-type-specifier
  "Test (not type) compound specifier"
  (let ((not-form (clysm/lib/macros:type-specifier-to-predicate
                   '(not null) 'x)))
    (ok (eq (car not-form) 'not)
        "NOT compound produces NOT form")
    (ok (= 2 (length not-form))
        "NOT form has 2 elements (not pred)")
    (ok (equal (second not-form) '(null x))
        "Inner predicate is (null x)")))

(deftest compound-not-null-in-typecase
  "Test (not null) in typecase"
  (let* ((form '(typecase x
                  ((not null) "not-null")
                  (null "is-null")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    (ok (member 'not (flatten expanded))
        "Expansion contains NOT")))

;;; ============================================================
;;; T064: MEMBER Compound Type Specifier Tests
;;; ============================================================

(deftest compound-member-type-specifier
  "Test (member item1 item2) compound specifier"
  (let ((member-form (clysm/lib/macros:type-specifier-to-predicate
                      '(member :a :b :c) 'x)))
    (ok (eq (car member-form) 'or)
        "MEMBER compound produces OR of EQL forms")
    (ok (= 4 (length member-form))
        "OR form has 4 elements (or eql1 eql2 eql3)")
    (ok (member 'eql (flatten member-form))
        "Contains EQL tests")))

(deftest compound-member-in-typecase
  "Test (member ...) in typecase for keyword dispatch"
  (let* ((form '(typecase x
                  ((member :foo :bar) "foo-or-bar")
                  (symbol "other-sym")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    (ok (member 'eql (flatten expanded))
        "Expansion contains EQL for member check")))

;;; ============================================================
;;; T065: SATISFIES Compound Type Specifier Tests
;;; ============================================================

(deftest compound-satisfies-type-specifier
  "Test (satisfies predicate) compound specifier"
  (let ((satisfies-form (clysm/lib/macros:type-specifier-to-predicate
                         '(satisfies evenp) 'x)))
    (ok (eq (car satisfies-form) 'funcall)
        "SATISFIES compound produces FUNCALL form")
    (ok (member 'evenp (flatten satisfies-form))
        "Contains the predicate function")))

(deftest compound-satisfies-in-typecase
  "Test (satisfies ...) in typecase"
  (let* ((form '(typecase x
                  ((satisfies evenp) "even")
                  (integer "odd-or-non-integer")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    (ok (member 'funcall (flatten expanded))
        "Expansion contains FUNCALL for satisfies")))

;;; ============================================================
;;; T073: EQL Compound Type Specifier Tests (bonus)
;;; ============================================================

(deftest compound-eql-type-specifier
  "Test (eql object) compound specifier"
  (let ((eql-form (clysm/lib/macros:type-specifier-to-predicate
                   '(eql 42) 'x)))
    (ok (eq (car eql-form) 'eql)
        "EQL compound produces EQL form")
    (ok (member 42 (flatten eql-form))
        "Contains the specific object")))

;;; ============================================================
;;; Nested Compound Type Tests
;;; ============================================================

(deftest nested-compound-types
  "Test nested compound type specifiers"
  (let ((nested-form (clysm/lib/macros:type-specifier-to-predicate
                      '(or (and integer (satisfies evenp))
                           (and symbol (not null)))
                      'x)))
    (ok (eq (car nested-form) 'or)
        "Outer is OR")
    (ok (member 'and (flatten nested-form))
        "Contains AND")
    (ok (member 'not (flatten nested-form))
        "Contains NOT")))
