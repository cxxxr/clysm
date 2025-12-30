;;;; set-ops-test.lisp - Unit tests for union/intersection/set-difference/subsetp/adjoin
;;;;
;;;; Feature: 043-self-hosting-blockers, 001-ansi-list-ops (Phase 15A)
;;;; User Story: US5 - Set Operations
;;;;
;;;; HyperSpec references:
;;;;   [intersection](resources/HyperSpec/Body/f_intera.htm)
;;;;   [union](resources/HyperSpec/Body/f_unionc.htm)
;;;;   [set-difference](resources/HyperSpec/Body/f_set_di.htm)
;;;;   [subsetp](resources/HyperSpec/Body/f_subset.htm)
;;;;   [adjoin](resources/HyperSpec/Body/f_adjoin.htm)

(in-package #:clysm/tests)

;;; ============================================================
;;; UNION tests
;;; ============================================================

(deftest union-test
  "Test union compilation."
  ;; Note: Tests use numeric lists since quoted symbol lists are not fully supported
  (testing "basic union compiles"
    (let ((wasm (clysm:compile-to-wasm '(union '(1 2) '(2 3)))))
      (ok wasm "Should compile union")))

  (testing "union with disjoint lists"
    (let ((wasm (clysm:compile-to-wasm '(union '(1 2 3) '(4 5 6)))))
      (ok wasm "Should compile union of disjoint lists")))

  (testing "union with :test compiles"
    (let ((wasm (clysm:compile-to-wasm '(union '(1 2) '(3 4) :test #'eql))))
      (ok wasm "Should compile union with :test"))))

;;; ============================================================
;;; INTERSECTION tests
;;; ============================================================

(deftest intersection-test
  "Test intersection compilation."
  ;; Note: Tests use numeric lists since quoted symbol lists are not fully supported
  (testing "basic intersection compiles"
    (let ((wasm (clysm:compile-to-wasm '(intersection '(1 2 3) '(2 3 4)))))
      (ok wasm "Should compile intersection")))

  (testing "intersection with disjoint lists"
    (let ((wasm (clysm:compile-to-wasm '(intersection '(1 2) '(3 4)))))
      (ok wasm "Should compile intersection of disjoint lists")))

  (testing "intersection with :test compiles"
    (let ((wasm (clysm:compile-to-wasm '(intersection '(1 2) '(2 3) :test #'eql))))
      (ok wasm "Should compile intersection with :test"))))

;;; ============================================================
;;; SET-DIFFERENCE tests
;;; ============================================================

(deftest set-difference-test
  "Test set-difference compilation."
  ;; Note: Tests use numeric lists since quoted symbol lists are not fully supported
  (testing "basic set-difference compiles"
    (let ((wasm (clysm:compile-to-wasm '(set-difference '(1 2 3) '(2 3 4)))))
      (ok wasm "Should compile set-difference")))

  (testing "set-difference with empty result"
    (let ((wasm (clysm:compile-to-wasm '(set-difference '(1 2) '(1 2 3)))))
      (ok wasm "Should compile set-difference resulting in empty")))

  (testing "set-difference with :test compiles"
    (let ((wasm (clysm:compile-to-wasm '(set-difference '(1 2 3) '(2) :test #'eql))))
      (ok wasm "Should compile set-difference with :test"))))

;;; ============================================================
;;; SUBSETP tests
;;; ============================================================

(deftest subsetp-test
  "Test subsetp compilation."
  ;; Note: Tests use numeric lists since quoted symbol lists are not fully supported
  (testing "subsetp with subset compiles"
    (let ((wasm (clysm:compile-to-wasm '(subsetp '(1 2) '(1 2 3)))))
      (ok wasm "Should compile subsetp (true case)")))

  (testing "subsetp with non-subset compiles"
    (let ((wasm (clysm:compile-to-wasm '(subsetp '(1 2 3) '(1 2)))))
      (ok wasm "Should compile subsetp (false case)")))

  (testing "subsetp with :test compiles"
    (let ((wasm (clysm:compile-to-wasm '(subsetp '(1) '(1 2) :test #'eql))))
      (ok wasm "Should compile subsetp with :test"))))

;;; ============================================================
;;; ADJOIN tests
;;; ============================================================

(deftest adjoin-test
  "Test adjoin compilation."
  ;; Note: Tests use numeric lists since quoted symbol lists are not fully supported
  (testing "basic adjoin compiles"
    (let ((wasm (clysm:compile-to-wasm '(adjoin 1 '(2 3)))))
      (ok wasm "Should compile adjoin")))

  (testing "adjoin with existing element"
    (let ((wasm (clysm:compile-to-wasm '(adjoin 2 '(1 2 3)))))
      (ok wasm "Should compile adjoin (element present)")))

  (testing "adjoin with :test compiles"
    (let ((wasm (clysm:compile-to-wasm '(adjoin 1 '(2 3) :test #'eql))))
      (ok wasm "Should compile adjoin with :test")))

  (testing "adjoin with :key compiles"
    ;; Using pairlis and numeric values instead of dotted pair literals
    (let ((wasm (clysm:compile-to-wasm '(adjoin 1 (pairlis '(1 2) '(10 20)) :key #'car))))
      (ok wasm "Should compile adjoin with :key"))))
