;;;; test-allow-other-keys.lisp - Unit test for &allow-other-keys handling
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T030 [US3] Unit test for &allow-other-keys handling

(in-package #:clysm/tests)

(deftest allow-other-keys-parsing
    "Test that &allow-other-keys is parsed correctly"
  (testing "ast-parsed-lambda-list includes allow-other-keys flag"
    (let ((lambda-list '(x &key y &allow-other-keys)))
      (let ((parsed (clysm/compiler/ast:parse-lambda-list lambda-list nil)))
        (ok parsed)
        (ok (clysm/compiler/ast:ast-parsed-lambda-list-allow-other-keys parsed))))))

(deftest allow-other-keys-basic
    "Test basic &allow-other-keys compilation"
  (testing "compiles DEFUN with &allow-other-keys"
    (let ((form '(defun test-allow-keys (x &key y &allow-other-keys)
                  (+ x (or y 0)))))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with &allow-other-keys compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with &allow-other-keys failed: ~A" e)))))))

(deftest allow-other-keys-no-keys-defined
    "Test &allow-other-keys without any &key parameters"
  (testing "compiles DEFUN with only &allow-other-keys"
    (let ((form '(defun test-only-allow (x &rest args &key &allow-other-keys)
                  x)))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with only &allow-other-keys compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with only &allow-other-keys failed: ~A" e)))))))

(deftest allow-other-keys-with-rest
    "Test &allow-other-keys combined with &rest"
  (testing "compiles DEFUN with &rest and &allow-other-keys"
    (let ((form '(defun test-rest-allow (x &rest args &key y &allow-other-keys)
                  (cons x args))))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with &rest and &allow-other-keys compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with &rest and &allow-other-keys failed: ~A" e)))))))

(deftest allow-other-keys-with-defaults
    "Test &allow-other-keys with &key default values"
  (testing "compiles DEFUN with &key defaults and &allow-other-keys"
    (let ((form '(defun test-defaults-allow (x &key (y 0) (z 1) &allow-other-keys)
                  (+ x y z))))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with &key defaults and &allow-other-keys compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with &key defaults and &allow-other-keys failed: ~A" e)))))))

(deftest allow-other-keys-full-combination
    "Test &allow-other-keys with all parameter types"
  (testing "compiles DEFUN with full lambda-list including &allow-other-keys"
    (let ((form '(defun test-full-allow (a &optional (b 0) &rest r &key (c 1) &allow-other-keys &aux (d 2))
                  (+ a b c d))))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "DEFUN with full lambda-list compiled successfully"))
        (error (e)
          (fail (format nil "DEFUN with full lambda-list failed: ~A" e)))))))
