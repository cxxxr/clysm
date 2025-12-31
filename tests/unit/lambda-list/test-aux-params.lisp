;;;; test-aux-params.lisp - Unit test for &aux parameter compilation
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T028 [US3] Unit test for &aux parameter compilation

(in-package #:clysm/tests)

(deftest aux-params-parsing
    "Test that &aux parameters are parsed correctly"
  (testing "ast-parsed-lambda-list includes aux"
    (let ((lambda-list '(x y &aux (z 10))))
      (let ((parsed (clysm/compiler/ast:parse-lambda-list lambda-list nil)))
        (ok parsed)
        (ok (clysm/compiler/ast:ast-parsed-lambda-list-aux parsed))))))

(deftest aux-params-simple-init
    "Test &aux with simple initialization"
  (testing "compiles DEFUN with simple &aux"
    ;; This should compile without error
    (let ((form '(defun test-aux-simple (x &aux (y 0))
                  (+ x y))))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with &aux compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with &aux failed: ~A" e)))))))

(deftest aux-params-multiple
    "Test &aux with multiple auxiliary variables"
  (testing "compiles DEFUN with multiple &aux"
    (let ((form '(defun test-aux-multiple (x &aux (y 0) (z 1))
                  (+ x y z))))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with multiple &aux compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with multiple &aux failed: ~A" e)))))))

(deftest aux-params-no-init
    "Test &aux without initialization form"
  (testing "compiles DEFUN with &aux without init"
    (let ((form '(defun test-aux-no-init (x &aux y)
                  (setq y x)
                  y)))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with &aux (no init) compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with &aux (no init) failed: ~A" e)))))))

(deftest aux-params-with-other-params
    "Test &aux combined with other parameter types"
  (testing "compiles DEFUN with &optional, &key, and &aux"
    (let ((form '(defun test-aux-combined (x &optional (y 0) &key (z 1) &aux (sum 0))
                  (setq sum (+ x y z))
                  sum)))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with &optional, &key, &aux compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with combined params failed: ~A" e)))))))
