;;;; bootstrap-compile-test.lisp - Contract tests for bootstrap compilation (T014)
;;;; TDD: Tests written first per Constitution VII

(in-package :clysm/tests/contract/bootstrap-compile)

;;; ============================================================
;;; T014: compile-to-wasm accepts large progn
;;; ============================================================

(deftest compile-to-wasm-accepts-progn
  "compile-to-wasm should accept a progn wrapping multiple forms"
  (testing "single form progn compiles"
    (let ((bytes (clysm/compiler:compile-to-wasm '(progn (+ 1 2)))))
      (ok (arrayp bytes) "Result should be an array")
      (ok (> (length bytes) 0) "Should produce non-empty output")))

  (testing "multiple form progn compiles"
    (let ((bytes (clysm/compiler:compile-to-wasm
                  '(progn
                    (+ 1 2)
                    (* 3 4)
                    (- 10 5)))))
      (ok (arrayp bytes) "Result should be an array")
      (ok (> (length bytes) 0) "Should produce non-empty output"))))

(deftest compile-to-wasm-accepts-defun
  "compile-to-wasm should accept progn with defun forms"
  (testing "progn with defun and call"
    (let ((bytes (clysm/compiler:compile-to-wasm
                  '(progn
                    (defun add-two (x y) (+ x y))
                    (add-two 3 4)))))
      (ok (arrayp bytes) "Result should be an array")
      (ok (validate-wasm-silent bytes) "Should produce valid Wasm"))))

(deftest compile-to-wasm-accepts-multiple-defuns
  "compile-to-wasm should accept progn with multiple defun forms"
  (testing "multiple function definitions"
    (let ((bytes (clysm/compiler:compile-to-wasm
                  '(progn
                    (defun double-it (x) (* x 2))
                    (defun triple-it (x) (* x 3))
                    (defun add-them (a b) (+ a b))
                    (add-them (double-it 5) (triple-it 3))))))
      (ok (arrayp bytes) "Result should be an array")
      (ok (validate-wasm-silent bytes) "Should produce valid Wasm"))))

(deftest compile-to-wasm-handles-many-forms
  "compile-to-wasm should handle progn with many forms (simulating module compilation)"
  (testing "10 defun forms in one progn"
    (let* ((defuns (loop for i from 1 to 10
                         collect `(defun ,(intern (format nil "FN~D" i)) (x) (+ x ,i))))
           (final-call '(fn1 100))
           (expr `(progn ,@defuns ,final-call))
           (bytes (clysm/compiler:compile-to-wasm expr)))
      (ok (arrayp bytes) "Result should be an array")
      (ok (validate-wasm-silent bytes) "Should produce valid Wasm"))))

(deftest compile-to-wasm-handles-nested-structures
  "compile-to-wasm should handle complex nested forms"
  (testing "nested let and defun"
    (let ((bytes (clysm/compiler:compile-to-wasm
                  '(progn
                    (defun outer (x)
                      (let ((y (* x 2)))
                        (let ((z (+ y 1)))
                          z)))
                    (outer 10)))))
      (ok (arrayp bytes) "Result should be an array")
      (ok (validate-wasm-silent bytes) "Should produce valid Wasm"))))

(deftest compile-to-wasm-produces-bytes
  "compile-to-wasm should produce unsigned-byte 8 array"
  (testing "output is proper byte array"
    (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (ok (typep bytes '(array (unsigned-byte 8)))
          "Result should be (unsigned-byte 8) array"))))
