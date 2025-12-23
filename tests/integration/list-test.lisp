;;;; list-test.lisp - Integration tests for list operations (006-cons-list-ops)
(in-package #:clysm/tests/integration/list)

;;; ============================================================
;;; Phase 3: User Story 1 - Basic Cons Cell Creation and Access
;;; ============================================================

;; T021: Integration test for cons/car/cdr roundtrip
(deftest test-cons-car-cdr-roundtrip
  "Verify (car (cons 1 2)) returns 1"
  (ok (= 1 (clysm/tests:compile-and-run '(car (cons 1 2))))
      "(car (cons 1 2)) should return 1")
  (ok (= 2 (clysm/tests:compile-and-run '(cdr (cons 1 2))))
      "(cdr (cons 1 2)) should return 2"))

;; T022: Integration test for nested cons
(deftest test-nested-cons
  "Verify nested cons access works"
  (ok (= 1 (clysm/tests:compile-and-run '(car (car (cons (cons 1 2) 3)))))
      "(car (car (cons (cons 1 2) 3))) should return 1")
  (ok (= 2 (clysm/tests:compile-and-run '(cdr (car (cons (cons 1 2) 3)))))
      "(cdr (car (cons (cons 1 2) 3))) should return 2")
  (ok (= 3 (clysm/tests:compile-and-run '(cdr (cons (cons 1 2) 3))))
      "(cdr (cons (cons 1 2) 3)) should return 3"))

;;; ============================================================
;;; Phase 4: User Story 2 - NIL Handling
;;; ============================================================

;; T024: Integration test for car of nil
(deftest test-car-nil-returns-nil
  "Verify (car nil) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(car nil)))
      "(car nil) should return NIL"))

;; T025: Integration test for cdr of nil
(deftest test-cdr-nil-returns-nil
  "Verify (cdr nil) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(cdr nil)))
      "(cdr nil) should return NIL"))

;; T026: Integration test for nested nil access
(deftest test-nested-nil-access
  "Verify (cdr (cdr (cons 1 nil))) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(cdr (cdr (cons 1 nil)))))
      "(cdr (cdr (cons 1 nil))) should return NIL"))

;;; ============================================================
;;; Phase 5: User Story 3 - List Construction
;;; ============================================================

;; T038: Integration test for list traversal
(deftest test-list-traversal
  "Verify (car (cdr (list 1 2 3))) returns 2"
  (ok (= 2 (clysm/tests:compile-and-run '(car (cdr (list 1 2 3)))))
      "(car (cdr (list 1 2 3))) should return 2"))

;; T039: Integration test for list termination
(deftest test-list-termination
  "Verify list terminates with NIL"
  (ok (null (clysm/tests:compile-and-run '(cdr (cdr (cdr (list 1 2 3))))))
      "(cdr (cdr (cdr (list 1 2 3)))) should return NIL"))

;; Additional list tests
(deftest test-list-empty
  "Verify (list) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(list)))
      "(list) should return NIL"))

(deftest test-list-single
  "Verify (list 1) creates proper list"
  (ok (= 1 (clysm/tests:compile-and-run '(car (list 1))))
      "(car (list 1)) should return 1")
  (ok (null (clysm/tests:compile-and-run '(cdr (list 1))))
      "(cdr (list 1)) should return NIL"))

;;; ============================================================
;;; Phase 6: User Story 4 - Type Predicates
;;; ============================================================

;; T056: Integration test for all predicates
(deftest test-consp-predicate
  "Verify consp returns T for cons, NIL otherwise"
  (ok (clysm/tests:compile-and-run '(consp (cons 1 2)))
      "(consp (cons 1 2)) should return T")
  (ok (null (clysm/tests:compile-and-run '(consp nil)))
      "(consp nil) should return NIL")
  (ok (null (clysm/tests:compile-and-run '(consp 42)))
      "(consp 42) should return NIL"))

(deftest test-null-predicate
  "Verify null returns T for NIL, NIL otherwise"
  (ok (clysm/tests:compile-and-run '(null nil))
      "(null nil) should return T")
  (ok (null (clysm/tests:compile-and-run '(null (cons 1 2))))
      "(null (cons 1 2)) should return NIL")
  (ok (null (clysm/tests:compile-and-run '(null 42)))
      "(null 42) should return NIL"))

(deftest test-atom-predicate
  "Verify atom returns T for non-cons, NIL for cons"
  (ok (clysm/tests:compile-and-run '(atom nil))
      "(atom nil) should return T")
  (ok (clysm/tests:compile-and-run '(atom 42))
      "(atom 42) should return T")
  (ok (null (clysm/tests:compile-and-run '(atom (cons 1 2))))
      "(atom (cons 1 2)) should return NIL"))

(deftest test-listp-predicate
  "Verify listp returns T for cons or NIL"
  (ok (clysm/tests:compile-and-run '(listp nil))
      "(listp nil) should return T")
  (ok (clysm/tests:compile-and-run '(listp (cons 1 2)))
      "(listp (cons 1 2)) should return T")
  (ok (null (clysm/tests:compile-and-run '(listp 42)))
      "(listp 42) should return NIL"))

;;; ============================================================
;;; Phase 7: User Story 5 - Quoted List Literals
;;; ============================================================

;; T058: Integration test for empty quoted list
(deftest test-quote-empty-list
  "Verify '() returns NIL"
  (ok (null (clysm/tests:compile-and-run ''()))
      "'() should return NIL"))

;; T059: Integration test for quoted list
(deftest test-quote-list
  "Verify '(1 2 3) creates proper list"
  (ok (= 1 (clysm/tests:compile-and-run '(car '(1 2 3))))
      "(car '(1 2 3)) should return 1")
  (ok (= 2 (clysm/tests:compile-and-run '(car (cdr '(1 2 3)))))
      "(car (cdr '(1 2 3))) should return 2"))

;; T060: Integration test for nested quoted list
(deftest test-quote-nested-list
  "Verify '((1 2) 3) creates nested structure"
  (ok (= 1 (clysm/tests:compile-and-run '(car (car '((1 2) 3)))))
      "(car (car '((1 2) 3))) should return 1")
  (ok (= 3 (clysm/tests:compile-and-run '(car (cdr '((1 2) 3)))))
      "(car (cdr '((1 2) 3))) should return 3"))

;; T061: Integration test for deeply nested quoted list
(deftest test-quote-deeply-nested
  "Verify quoted lists work at least 10 levels deep"
  ;; Build a 10-level nested structure
  (ok (= 1 (clysm/tests:compile-and-run
            '(car (car (car (car (car (car (car (car (car (car
               '((((((((((1))))))))))))))))))))))
      "10-level nested quoted list should work"))

;;; ============================================================
;;; Phase 8: User Story 6 - Destructive Modification
;;; ============================================================

;; T074: Integration test for rplaca persistence
(deftest test-rplaca-modification
  "Verify rplaca modifies car in place"
  (ok (= 10 (clysm/tests:compile-and-run
             '(let ((x (cons 1 2)))
                (rplaca x 10)
                (car x))))
      "(rplaca x 10) should modify car to 10"))

;; T075: Integration test for rplacd persistence
(deftest test-rplacd-modification
  "Verify rplacd modifies cdr in place"
  (ok (= 20 (clysm/tests:compile-and-run
             '(let ((x (cons 1 2)))
                (rplacd x 20)
                (cdr x))))
      "(rplacd x 20) should modify cdr to 20"))

;; Return value tests
(deftest test-rplaca-return-value
  "Verify rplaca returns the modified cons"
  ;; rplaca should return the cons, and car of that cons should be the new value
  (ok (= 10 (clysm/tests:compile-and-run
             '(car (rplaca (cons 1 2) 10))))
      "(car (rplaca (cons 1 2) 10)) should return 10"))

(deftest test-rplacd-return-value
  "Verify rplacd returns the modified cons"
  (ok (= 20 (clysm/tests:compile-and-run
             '(cdr (rplacd (cons 1 2) 20))))
      "(cdr (rplacd (cons 1 2) 20)) should return 20"))

;;; ============================================================
;;; Phase 9: User Story 7 - List Accessors
;;; ============================================================

;; T089: Integration test for third
(deftest test-third-accessor
  "Verify (third '(a b c d)) returns C"
  ;; Using numbers since we don't have full symbol support yet
  (ok (= 3 (clysm/tests:compile-and-run '(third (list 1 2 3 4))))
      "(third (list 1 2 3 4)) should return 3"))

;; T090: Integration test for nth
(deftest test-nth-accessor
  "Verify (nth 2 list) returns third element"
  (ok (= 3 (clysm/tests:compile-and-run '(nth 2 (list 1 2 3 4))))
      "(nth 2 (list 1 2 3 4)) should return 3"))

;; Additional accessor tests
(deftest test-first-rest-accessors
  "Verify first and rest work"
  (ok (= 1 (clysm/tests:compile-and-run '(first (list 1 2 3))))
      "(first (list 1 2 3)) should return 1")
  (ok (= 2 (clysm/tests:compile-and-run '(car (rest (list 1 2 3)))))
      "(car (rest (list 1 2 3))) should return 2"))

(deftest test-second-accessor
  "Verify second works"
  (ok (= 2 (clysm/tests:compile-and-run '(second (list 1 2 3))))
      "(second (list 1 2 3)) should return 2"))

(deftest test-nthcdr-accessor
  "Verify nthcdr works"
  (ok (= 3 (clysm/tests:compile-and-run '(car (nthcdr 2 (list 1 2 3 4)))))
      "(car (nthcdr 2 (list 1 2 3 4))) should return 3"))

;;; ============================================================
;;; Phase 10: Edge Cases & Error Handling
;;; ============================================================

;; T092: car/cdr on non-cons, non-nil should error
(deftest test-car-on-fixnum-errors
  "Verify (car 42) signals error"
  (ok (handler-case
          (progn
            (clysm/tests:compile-and-run '(car 42))
            nil)  ; Should not reach here
        (clysm/tests/helpers:wasm-runtime-error () t))
      "(car 42) should signal error"))

;; T093: nth with negative index
(deftest test-nth-negative-index
  "Verify (nth -1 list) behavior"
  ;; Per CLHS, behavior is undefined. We return NIL.
  (ok (null (clysm/tests:compile-and-run '(nth -1 (list 1 2 3))))
      "(nth -1 list) should return NIL"))

;; T094: rplaca/rplacd on non-cons should error
(deftest test-rplaca-on-nil-errors
  "Verify (rplaca nil x) signals error"
  (ok (handler-case
          (progn
            (clysm/tests:compile-and-run '(rplaca nil 10))
            nil)
        (clysm/tests/helpers:wasm-runtime-error () t))
      "(rplaca nil 10) should signal error"))

;;; ============================================================
;;; Phase 11: Stress Tests
;;; ============================================================

;; T098: Stress test - create many cons cells
(deftest test-many-cons-cells
  "Verify creating 1,000 cons cells works"
  ;; Use labels for recursive loop since dotimes isn't implemented
  ;; Limited to 1000 due to wasmtime's default stack size (recursion depth)
  (ok (= 1000 (clysm/tests:compile-and-run
               '(labels ((loop (n acc)
                           (if (= n 0)
                               acc
                               (progn
                                 (cons n nil)
                                 (loop (- n 1) (+ acc 1))))))
                  (loop 1000 0))))
      "Creating 1,000 cons cells should work"))

;; T099: Stress test - deep list traversal
(deftest test-deep-list-traversal
  "Verify traversing a long list works"
  ;; Create a list of 100 elements and traverse to the end
  (ok (null (clysm/tests:compile-and-run
             '(nthcdr 100 (list 1 2 3 4 5 6 7 8 9 10
                               11 12 13 14 15 16 17 18 19 20
                               21 22 23 24 25 26 27 28 29 30
                               31 32 33 34 35 36 37 38 39 40
                               41 42 43 44 45 46 47 48 49 50
                               51 52 53 54 55 56 57 58 59 60
                               61 62 63 64 65 66 67 68 69 70
                               71 72 73 74 75 76 77 78 79 80
                               81 82 83 84 85 86 87 88 89 90
                               91 92 93 94 95 96 97 98 99 100))))
      "Traversing 100-element list to end should return NIL"))
