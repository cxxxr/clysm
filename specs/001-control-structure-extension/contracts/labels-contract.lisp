;;;; labels-contract.lisp - Test contracts for labels/flet mutual recursion
;;;; HyperSpec: resources/HyperSpec/Body/s_flet_.htm

(in-package #:clysm-test)

;;; ============================================================
;;; Contract Tests: labels Mutual Recursion
;;; ============================================================

(deftest labels-simple-recursion
  "Contract: labels with self-recursive function compiles"
  (let ((wasm (compile-form-to-wasm
               '(labels ((fact (n)
                          (if (zerop n)
                              1
                              (* n (fact (1- n))))))
                  (fact 5)))))
    (ok wasm "self-recursive labels should compile")
    (ok (wasm-validates wasm))))

(deftest labels-mutual-recursion
  "Contract: labels with two mutually recursive functions compiles"
  (let ((wasm (compile-form-to-wasm
               '(labels ((even-p (n)
                          (if (zerop n)
                              t
                              (odd-p (1- n))))
                        (odd-p (n)
                          (if (zerop n)
                              nil
                              (even-p (1- n)))))
                  (even-p 10)))))
    (ok wasm "mutually recursive labels should compile")
    (ok (wasm-validates wasm))))

(deftest labels-forward-reference-resolves
  "Contract: Forward references in labels body resolve correctly"
  (let ((wasm (compile-form-to-wasm
               '(labels ((a () (b))
                        (b () (a)))
                  (a)))))
    ;; Both function references should resolve
    (ok (wasm-validates wasm) "forward references must resolve")))

(deftest labels-closure-captures-env
  "Contract: labels functions capture enclosing environment"
  (let ((wasm (compile-form-to-wasm
               '(lambda (x)
                  (labels ((get-x () x))
                    (get-x))))))
    (ok wasm "labels capturing free variable should compile")
    (ok (wasm-validates wasm))))

(deftest labels-nested-in-labels
  "Contract: Nested labels forms compile correctly"
  (let ((wasm (compile-form-to-wasm
               '(labels ((outer ()
                          (labels ((inner () 42))
                            (inner))))
                  (outer)))))
    (ok wasm "nested labels should compile")
    (ok (wasm-validates wasm))))

(deftest flet-basic-compiles
  "Contract: Basic flet without recursion compiles"
  (let ((wasm (compile-form-to-wasm
               '(flet ((double (x) (* x 2)))
                  (double 5)))))
    (ok wasm "basic flet should compile")
    (ok (wasm-validates wasm))))

(deftest flet-shadowing-works
  "Contract: flet shadows outer function definitions"
  (let ((wasm (compile-form-to-wasm
               '(flet ((car (x) (cdr x)))
                  (car '(1 . 2))))))
    (ok wasm "flet shadowing should compile")
    (ok (wasm-validates wasm))))

(deftest labels-three-way-recursion
  "Contract: Three mutually recursive functions compile"
  (let ((wasm (compile-form-to-wasm
               '(labels ((a () (b))
                        (b () (c))
                        (c () (a)))
                  (a)))))
    (ok wasm "three-way mutual recursion should compile")
    (ok (wasm-validates wasm))))
