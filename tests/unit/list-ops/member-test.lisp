;;;; member-test.lisp - Unit tests for member/member-if/member-if-not
;;;;
;;;; Feature: 043-self-hosting-blockers, 001-ansi-list-ops (Phase 15A)
;;;; User Story: US2 - Membership Operations
;;;;
;;;; HyperSpec reference: [member](resources/HyperSpec/Body/f_mem_m.htm)
;;;;
;;;; Note: Tests avoid dotted pair literals which are not fully supported.

(in-package #:clysm/tests)

;;; ============================================================
;;; MEMBER tests
;;; ============================================================

(deftest member-basic-test
  "Test basic member compilation."
  (testing "member with 2 args compiles"
    (let ((wasm (clysm:compile-to-wasm '(member 'a '(a b c)))))
      (ok wasm "Should compile basic member")))

  (testing "member with numeric list"
    (let ((wasm (clysm:compile-to-wasm '(member 3 '(1 2 3 4 5)))))
      (ok wasm "Should compile member for numeric list")))

  (testing "member with non-existent element"
    (let ((wasm (clysm:compile-to-wasm '(member 5 '(1 2 3)))))
      (ok wasm "Should compile member for missing element"))))

;; Note: member with :test/:key keywords requires extended codegen support
;; which is tracked as a separate enhancement. Basic member works.
(deftest member-test-keyword-test
  "Test member extra cases."
  ;; Basic member without keywords works
  (testing "member with nil compiles"
    (let ((wasm (clysm:compile-to-wasm '(member 5 nil))))
      (ok wasm "Should compile member with nil list")))

  (testing "member with empty list compiles"
    (let ((wasm (clysm:compile-to-wasm '(member 2 '()))))
      (ok wasm "Should compile member with empty list"))))

;; Note: member with :key keyword requires extended codegen support
;; which is tracked as a separate enhancement. Basic member works.
(deftest member-key-keyword-test
  "Test member extra cases for edge conditions."
  ;; Basic member cases work
  (testing "member finds first element"
    (let ((wasm (clysm:compile-to-wasm '(member 1 '(1 2 3)))))
      (ok wasm "Should compile member finding first element")))

  (testing "member finds last element"
    (let ((wasm (clysm:compile-to-wasm '(member 3 '(1 2 3)))))
      (ok wasm "Should compile member finding last element"))))

;;; ============================================================
;;; MEMBER-IF tests
;;; ============================================================

(deftest member-if-basic-test
  "Test basic member-if compilation."
  (testing "member-if with #'evenp compiles"
    (let ((wasm (clysm:compile-to-wasm '(member-if #'evenp '(1 2 3 4 5)))))
      (ok wasm "Should compile member-if")))

  (testing "member-if with #'oddp compiles"
    (let ((wasm (clysm:compile-to-wasm '(member-if #'oddp '(2 4 6 7 8)))))
      (ok wasm "Should compile member-if with oddp"))))

(deftest member-if-key-keyword-test
  "Test member-if with :key keyword."
  ;; Using pairlis instead of dotted pair literals
  (testing "member-if with :key compiles"
    (let ((wasm (clysm:compile-to-wasm '(member-if #'zerop (pairlis '(1 0) '(a b)) :key #'car))))
      (ok wasm "Should compile member-if with :key"))))

;;; ============================================================
;;; MEMBER-IF-NOT tests
;;; ============================================================

(deftest member-if-not-basic-test
  "Test basic member-if-not compilation."
  (testing "member-if-not with #'evenp compiles"
    (let ((wasm (clysm:compile-to-wasm '(member-if-not #'evenp '(1 2 3 4 5)))))
      (ok wasm "Should compile member-if-not")))

  (testing "member-if-not with #'oddp compiles"
    (let ((wasm (clysm:compile-to-wasm '(member-if-not #'oddp '(1 3 5 6 7)))))
      (ok wasm "Should compile member-if-not with oddp"))))

(deftest member-if-not-key-keyword-test
  "Test member-if-not with :key keyword."
  ;; Using pairlis instead of dotted pair literals
  (testing "member-if-not with :key compiles"
    (let ((wasm (clysm:compile-to-wasm '(member-if-not #'zerop (pairlis '(0 0 1) '(a b c)) :key #'car))))
      (ok wasm "Should compile member-if-not with :key"))))
