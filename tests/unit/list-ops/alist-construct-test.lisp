;;;; alist-construct-test.lisp - Unit tests for alist construction operations
;;;;
;;;; Feature: 001-ansi-list-ops (Phase 15A)
;;;; User Story: US4 - Association List Construction
;;;;
;;;; HyperSpec references:
;;;;   [pairlis](resources/HyperSpec/Body/f_pairli.htm)
;;;;   [acons](resources/HyperSpec/Body/f_acons.htm)
;;;;   [copy-alist](resources/HyperSpec/Body/f_cp_ali.htm)

(in-package #:clysm/tests)

;;; ============================================================
;;; ACONS tests
;;; ============================================================

(deftest acons-basic-test
  "Test basic acons compilation."
  ;; Note: Tests avoid dotted pair literals which are not fully supported
  (testing "acons with key, value, alist compiles"
    (let ((wasm (clysm:compile-to-wasm '(acons 'a 1 nil))))
      (ok wasm "Should compile acons")))

  (testing "acons with empty alist"
    (let ((wasm (clysm:compile-to-wasm '(acons 'x 10 nil))))
      (ok wasm "Should compile acons with empty alist")))

  (testing "acons with string keys"
    (let ((wasm (clysm:compile-to-wasm '(acons "key" "value" nil))))
      (ok wasm "Should compile acons with string keys"))))

;;; ============================================================
;;; PAIRLIS tests
;;; ============================================================

(deftest pairlis-basic-test
  "Test basic pairlis compilation."
  ;; Note: Tests avoid dotted pair literals which are not fully supported
  (testing "pairlis with keys and values"
    (let ((wasm (clysm:compile-to-wasm '(pairlis '(a b c) '(1 2 3)))))
      (ok wasm "Should compile pairlis")))

  (testing "pairlis with existing alist (nil)"
    (let ((wasm (clysm:compile-to-wasm '(pairlis '(a b) '(1 2) nil))))
      (ok wasm "Should compile pairlis with nil alist")))

  (testing "pairlis with empty lists"
    (let ((wasm (clysm:compile-to-wasm '(pairlis nil nil))))
      (ok wasm "Should compile pairlis with empty lists"))))

(deftest pairlis-edge-cases-test
  "Test pairlis edge cases."
  (testing "pairlis with single element"
    (let ((wasm (clysm:compile-to-wasm '(pairlis '(x) '(1)))))
      (ok wasm "Should compile pairlis with single element"))))

;;; ============================================================
;;; COPY-ALIST tests
;;; ============================================================

(deftest copy-alist-basic-test
  "Test basic copy-alist compilation."
  ;; Note: Tests avoid dotted pair literals which are not fully supported
  (testing "copy-alist with empty alist"
    (let ((wasm (clysm:compile-to-wasm '(copy-alist nil))))
      (ok wasm "Should compile copy-alist with nil")))

  (testing "copy-alist with pairlis-built alist"
    ;; Use pairlis to construct an alist dynamically instead of literals
    (let ((wasm (clysm:compile-to-wasm '(copy-alist (pairlis '(a b) '(1 2))))))
      (ok wasm "Should compile copy-alist with dynamic alist"))))
