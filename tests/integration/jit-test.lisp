;;;; jit-test.lisp - JIT compilation tests (Phase 9 - T186-T188)
(in-package #:clysm/tests/integration/jit)

;;; Wasm Generation Tests (T186)

(deftest jit-wasm-generation
  (testing "generate wasm for simple expression"
    (let ((wasm (clysm/eval/jit:generate-wasm '(+ 1 2))))
      (ok wasm)
      (ok (typep wasm '(vector (unsigned-byte 8))))))

  (testing "generate wasm for lambda"
    (let ((wasm (clysm/eval/jit:generate-wasm '(lambda (x) (+ x 1)))))
      (ok wasm)))

  (testing "wasm starts with magic bytes"
    (let ((wasm (clysm/eval/jit:generate-wasm '(+ 1 2))))
      (ok (and (>= (length wasm) 4)
               (= (aref wasm 0) #x00)
               (= (aref wasm 1) #x61)
               (= (aref wasm 2) #x73)
               (= (aref wasm 3) #x6d))))))

;;; Binary Validation Tests (T186)

(deftest jit-validation
  (testing "validate generated wasm"
    (let ((wasm (clysm/eval/jit:generate-wasm '(+ 1 2))))
      (ok (clysm/eval/jit:validate-wasm wasm))))

  (testing "invalid wasm fails validation"
    (ok (not (clysm/eval/jit:validate-wasm #(1 2 3 4))))))

;;; Dynamic Linking Tests (T187)

(deftest jit-linking
  (testing "instantiate wasm module"
    (let* ((wasm (clysm/eval/jit:generate-wasm '(lambda () 42)))
           (instance (clysm/eval/jit:instantiate-wasm wasm)))
      (ok instance)))

  (testing "extract function from instance"
    (let* ((wasm (clysm/eval/jit:generate-wasm '(lambda () 42)))
           (instance (clysm/eval/jit:instantiate-wasm wasm))
           (fn (clysm/eval/jit:extract-function instance)))
      (ok (functionp fn)))))

;;; Compile Function Tests (T188)

(deftest compile-function
  (testing "compile nil with lambda returns function"
    (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (+ x 1)))))
      (ok (functionp fn))))

  (testing "compiled function is callable"
    (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (+ x 1)))))
      (ok (= 43 (funcall fn 42)))))

  (testing "compile identity function"
    (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) x))))
      (ok (= 100 (funcall fn 100))))))

;;; Tier Switching Tests (T203)

(deftest tier-switching
  (testing "initial eval uses Tier 1"
    ;; First evaluation should use interpreter
    (ok (= 3 (clysm/eval:eval* '(+ 1 2)))))

  (testing "compile promotes to Tier 2"
    ;; After compile, should use JIT
    (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) x))))
      (ok (functionp fn)))))

;;; Runtime Import Tests

(deftest jit-runtime-imports
  (testing "jit has access to arithmetic"
    (let ((fn (clysm/eval/compile:compile* nil '(lambda (x y) (+ x y)))))
      (ok (= 7 (funcall fn 3 4)))))

  (testing "jit has access to comparison"
    (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (< x 10)))))
      (ok (funcall fn 5))
      (ok (not (funcall fn 15))))))

;;; Acceptance Test (T205)

(deftest compile-acceptance
  (testing "(compile nil '(lambda (x) (+ x 1))) returns function"
    (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (+ x 1)))))
      (ok (functionp fn))
      (ok (= 2 (funcall fn 1))))))

;;; Edge Cases

(deftest jit-edge-cases
  (testing "compile empty lambda"
    (let ((fn (clysm/eval/compile:compile* nil '(lambda () nil))))
      (ok (functionp fn))
      (ok (null (funcall fn)))))

  (testing "compile lambda with multiple args"
    (let ((fn (clysm/eval/compile:compile* nil '(lambda (a b c) (+ a (+ b c))))))
      (ok (= 6 (funcall fn 1 2 3))))))
