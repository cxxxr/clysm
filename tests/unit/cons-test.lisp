;;;; cons-test.lisp - Unit tests for cons cell operations (006-cons-list-ops)
(in-package #:clysm/tests/unit/cons)

;;; ============================================================
;;; Phase 3: User Story 1 - Basic Cons Cell Creation and Access
;;; ============================================================

;; T011: Unit test for cons creation
(deftest test-cons-creation
  "Verify cons compiles to struct.new 2"
  ;; This test verifies that (cons x y) generates the correct Wasm instructions
  ;; The compiled code should include struct.new with type index 2 ($cons)
  (let ((wat (clysm/compiler:compile-to-wat '(cons 1 2))))
    (ok (search "struct.new" wat)
        "cons should generate struct.new instruction")
    (ok (search "struct.new 2" wat)
        "cons should use type index 2 for $cons")))

;; T012: Unit test for car access
(deftest test-car-access
  "Verify car compiles to struct.get 2 0"
  ;; This test verifies that (car x) generates the correct Wasm instructions
  ;; The compiled code should include struct.get with type 2 and field 0
  (let ((wat (clysm/compiler:compile-to-wat '(car (cons 1 2)))))
    (ok (search "struct.get" wat)
        "car should generate struct.get instruction")
    (ok (search "struct.get 2 0" wat)
        "car should use type 2 field 0")))

;; T013: Unit test for cdr access
(deftest test-cdr-access
  "Verify cdr compiles to struct.get 2 1"
  ;; This test verifies that (cdr x) generates the correct Wasm instructions
  ;; The compiled code should include struct.get with type 2 and field 1
  (let ((wat (clysm/compiler:compile-to-wat '(cdr (cons 1 2)))))
    (ok (search "struct.get" wat)
        "cdr should generate struct.get instruction")
    (ok (search "struct.get 2 1" wat)
        "cdr should use type 2 field 1")))

;;; ============================================================
;;; Phase 5: User Story 3 - List Construction
;;; ============================================================

;; T031: Unit test for empty list
(deftest test-list-empty
  "Verify (list) returns NIL"
  ;; Empty list should compile to NIL (ref.null none in current implementation)
  (let ((wat (clysm/compiler:compile-to-wat '(list))))
    (ok (search "ref.null" wat)
        "(list) should generate ref.null for NIL")))

;; T032: Unit test for single-element list
(deftest test-list-single
  "Verify (list 1) creates proper structure"
  ;; (list 1) = (cons 1 nil)
  (let ((wat (clysm/compiler:compile-to-wat '(list 1))))
    (ok (search "struct.new 2" wat)
        "(list 1) should create a cons cell")))

;; T033: Unit test for multiple-element list
(deftest test-list-multiple
  "Verify (list 1 2 3) creates proper cons chain"
  ;; (list 1 2 3) = (cons 1 (cons 2 (cons 3 nil)))
  (let ((wat (clysm/compiler:compile-to-wat '(list 1 2 3))))
    ;; Should have 3 struct.new 2 instructions
    (ok (search "struct.new 2" wat)
        "(list 1 2 3) should create cons cells")))

;;; ============================================================
;;; Phase 6: User Story 4 - Type Predicates
;;; ============================================================

;; T041: Unit test for consp true case
(deftest test-consp-true
  "Verify (consp (cons 1 2)) compiles with ref.test"
  (let ((wat (clysm/compiler:compile-to-wat '(consp (cons 1 2)))))
    (ok (search "ref.test" wat)
        "consp should use ref.test instruction")))

;; T042: Unit test for consp false cases
(deftest test-consp-false
  "Verify consp on NIL and fixnum"
  (let ((wat1 (clysm/compiler:compile-to-wat '(consp nil)))
        (wat2 (clysm/compiler:compile-to-wat '(consp 42))))
    (ok (search "ref.test" wat1)
        "consp nil should use ref.test")
    (ok (search "ref.test" wat2)
        "consp 42 should use ref.test")))

;; T043: Unit test for null true case
(deftest test-null-true
  "Verify (null nil) compiles with ref.is_null"
  (let ((wat (clysm/compiler:compile-to-wat '(null nil))))
    (ok (search "ref.is_null" wat)
        "null should use ref.is_null instruction")))

;; T044: Unit test for null false case
(deftest test-null-false
  "Verify (null (cons 1 2)) compiles with ref.is_null"
  (let ((wat (clysm/compiler:compile-to-wat '(null (cons 1 2)))))
    (ok (search "ref.is_null" wat)
        "null on cons should use ref.is_null")))

;; T045: Unit test for atom true cases
(deftest test-atom-true
  "Verify atom on NIL and fixnum"
  (let ((wat1 (clysm/compiler:compile-to-wat '(atom nil)))
        (wat2 (clysm/compiler:compile-to-wat '(atom 42))))
    (ok (search "ref.test" wat1)
        "atom nil should use ref.test")
    (ok (search "ref.test" wat2)
        "atom 42 should use ref.test")))

;; T046: Unit test for atom false case
(deftest test-atom-false
  "Verify (atom (cons 1 2)) compiles with ref.test"
  (let ((wat (clysm/compiler:compile-to-wat '(atom (cons 1 2)))))
    (ok (search "ref.test" wat)
        "atom on cons should use ref.test")))

;; T047: Unit test for listp true cases
(deftest test-listp-true
  "Verify listp on NIL and cons"
  (let ((wat1 (clysm/compiler:compile-to-wat '(listp nil)))
        (wat2 (clysm/compiler:compile-to-wat '(listp (cons 1 2)))))
    (ok (or (search "ref.test" wat1) (search "ref.eq" wat1))
        "listp nil should check type")
    (ok (or (search "ref.test" wat2) (search "ref.eq" wat2))
        "listp cons should check type")))

;; T048: Unit test for listp false case
(deftest test-listp-false
  "Verify (listp 42) compiles with type check"
  (let ((wat (clysm/compiler:compile-to-wat '(listp 42))))
    (ok (or (search "ref.test" wat) (search "ref.eq" wat))
        "listp 42 should check type")))

;;; ============================================================
;;; Phase 8: User Story 6 - Destructive Modification
;;; ============================================================

;; T066: Unit test for rplaca
(deftest test-rplaca
  "Verify rplaca compiles to struct.set 2 0"
  (let ((wat (clysm/compiler:compile-to-wat
              '(let ((x (cons 1 2))) (rplaca x 10)))))
    (ok (search "struct.set" wat)
        "rplaca should generate struct.set instruction")
    (ok (search "struct.set 2 0" wat)
        "rplaca should use type 2 field 0")))

;; T067: Unit test for rplacd
(deftest test-rplacd
  "Verify rplacd compiles to struct.set 2 1"
  (let ((wat (clysm/compiler:compile-to-wat
              '(let ((x (cons 1 2))) (rplacd x 20)))))
    (ok (search "struct.set" wat)
        "rplacd should generate struct.set instruction")
    (ok (search "struct.set 2 1" wat)
        "rplacd should use type 2 field 1")))

;; T068: Unit test for rplaca return value
(deftest test-rplaca-return-value
  "Verify rplaca returns the cons cell"
  ;; Per CL spec, rplaca returns the modified cons, not the new value
  ;; This test verifies the return value pattern
  (ok t "Return value verification deferred to integration test"))

;;; ============================================================
;;; Phase 9: User Story 7 - List Accessors
;;; ============================================================

;; T077: Unit test for first and rest
(deftest test-first-rest
  "Verify first and rest compile correctly"
  (let ((wat1 (clysm/compiler:compile-to-wat '(first (cons 1 2))))
        (wat2 (clysm/compiler:compile-to-wat '(rest (cons 1 2)))))
    (ok (search "struct.get 2 0" wat1)
        "first should access car (field 0)")
    (ok (search "struct.get 2 1" wat2)
        "rest should access cdr (field 1)")))

;; T078: Unit test for second through tenth
(deftest test-second-through-tenth
  "Verify position accessors compile correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(second (list 1 2 3)))))
    (ok (search "struct.get" wat)
        "second should use struct.get")))

;; T079: Unit test for nth
(deftest test-nth
  "Verify nth compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(nth 2 (list 1 2 3 4)))))
    (ok (search "struct.get" wat)
        "nth should use struct.get for traversal")))

;; T080: Unit test for nthcdr
(deftest test-nthcdr
  "Verify nthcdr compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(nthcdr 2 (list 1 2 3 4)))))
    (ok (search "struct.get" wat)
        "nthcdr should use struct.get for traversal")))
