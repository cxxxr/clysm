;;;; list-tail-test.lisp - Unit tests for list tail operations
;;;;
;;;; Feature: 001-ansi-list-ops (Phase 15A)
;;;; User Story: US1 - List Tail Operations
;;;;
;;;; HyperSpec references:
;;;;   [last](resources/HyperSpec/Body/f_last.htm)
;;;;   [butlast](resources/HyperSpec/Body/f_butlas.htm)
;;;;   [nthcdr](resources/HyperSpec/Body/f_nthcdr.htm)
;;;;   [nth](resources/HyperSpec/Body/f_nth.htm)

(in-package #:clysm/tests)

;;; ============================================================
;;; LAST tests
;;; ============================================================

(deftest last-basic-test
  "Test basic last compilation."
  (testing "last with single arg compiles"
    (let ((wasm (clysm:compile-to-wasm '(last '(a b c)))))
      (ok wasm "Should compile (last list)")))

  (testing "last returns last cons cell"
    (let ((wasm (clysm:compile-to-wasm '(last '(a b c d e)))))
      (ok wasm "Should compile for 5-element list"))))

(deftest last-with-n-test
  "Test last with n argument."
  (testing "last with n=2 compiles"
    (let ((wasm (clysm:compile-to-wasm '(last '(a b c d e) 2))))
      (ok wasm "Should compile (last list 2)")))

  (testing "last with n=0 returns nil"
    (let ((wasm (clysm:compile-to-wasm '(last '(a b c) 0))))
      (ok wasm "Should compile (last list 0)"))))

(deftest last-edge-cases-test
  "Test last edge cases."
  (testing "last of empty list"
    (let ((wasm (clysm:compile-to-wasm '(last nil))))
      (ok wasm "Should compile (last nil)")))

  (testing "last of single element list"
    (let ((wasm (clysm:compile-to-wasm '(last '(x)))))
      (ok wasm "Should compile (last '(x))"))))

;;; ============================================================
;;; BUTLAST tests
;;; ============================================================

(deftest butlast-basic-test
  "Test basic butlast compilation."
  (testing "butlast with single arg compiles"
    (let ((wasm (clysm:compile-to-wasm '(butlast '(1 2 3 4 5)))))
      (ok wasm "Should compile (butlast list)")))

  (testing "butlast returns all but last element"
    (let ((wasm (clysm:compile-to-wasm '(butlast '(a b c d)))))
      (ok wasm "Should compile butlast for 4-element list"))))

(deftest butlast-with-n-test
  "Test butlast with n argument."
  (testing "butlast with n=2 compiles"
    (let ((wasm (clysm:compile-to-wasm '(butlast '(1 2 3 4 5) 2))))
      (ok wasm "Should compile (butlast list 2)")))

  (testing "butlast with n=0 returns copy"
    (let ((wasm (clysm:compile-to-wasm '(butlast '(a b c) 0))))
      (ok wasm "Should compile (butlast list 0)"))))

(deftest butlast-edge-cases-test
  "Test butlast edge cases."
  (testing "butlast of empty list"
    (let ((wasm (clysm:compile-to-wasm '(butlast nil))))
      (ok wasm "Should compile (butlast nil)")))

  (testing "butlast with n exceeding length"
    (let ((wasm (clysm:compile-to-wasm '(butlast '(a b) 5))))
      (ok wasm "Should compile (butlast list) with n > length"))))

;;; ============================================================
;;; NTH tests
;;; ============================================================

(deftest nth-basic-test
  "Test basic nth compilation."
  (testing "nth with index 0 compiles"
    (let ((wasm (clysm:compile-to-wasm '(nth 0 '(a b c d)))))
      (ok wasm "Should compile (nth 0 list)")))

  (testing "nth with index 2 compiles"
    (let ((wasm (clysm:compile-to-wasm '(nth 2 '(a b c d)))))
      (ok wasm "Should compile (nth 2 list)"))))

(deftest nth-edge-cases-test
  "Test nth edge cases."
  (testing "nth of empty list"
    (let ((wasm (clysm:compile-to-wasm '(nth 0 nil))))
      (ok wasm "Should compile (nth 0 nil)")))

  (testing "nth with index exceeding length"
    (let ((wasm (clysm:compile-to-wasm '(nth 10 '(a b)))))
      (ok wasm "Should compile (nth 10 list) with n > length"))))

;;; ============================================================
;;; NTHCDR tests
;;; ============================================================

(deftest nthcdr-basic-test
  "Test basic nthcdr compilation."
  (testing "nthcdr with n=0 compiles"
    (let ((wasm (clysm:compile-to-wasm '(nthcdr 0 '(a b c d)))))
      (ok wasm "Should compile (nthcdr 0 list)")))

  (testing "nthcdr with n=2 compiles"
    (let ((wasm (clysm:compile-to-wasm '(nthcdr 2 '(a b c d)))))
      (ok wasm "Should compile (nthcdr 2 list)"))))

(deftest nthcdr-edge-cases-test
  "Test nthcdr edge cases."
  (testing "nthcdr of empty list"
    (let ((wasm (clysm:compile-to-wasm '(nthcdr 0 nil))))
      (ok wasm "Should compile (nthcdr 0 nil)")))

  (testing "nthcdr with n exceeding length"
    (let ((wasm (clysm:compile-to-wasm '(nthcdr 10 '(a b)))))
      (ok wasm "Should compile (nthcdr n list) with n > length"))))
