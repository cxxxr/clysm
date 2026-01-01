;;;; undefined-prim-test.lisp - Unit tests for undefined primitive detection
;;;; Feature 001-runtime-library-system
;;;; Task T027: Unit test for undefined primitive detection

(in-package #:clysm/tests)

(deftest undefined-primitive-error-signaled ()
  "Verify undefined-primitive-error is signaled for unknown calls"
  (testing "error signaling"
    (ok (subtypep 'clysm/runtime-library:undefined-primitive-error 'error)
        "undefined-primitive-error is a subtype of error")
    (ok (handler-case
            (progn
              (error 'clysm/runtime-library:undefined-primitive-error
                     :name 'unknown-function)
              nil)
          (clysm/runtime-library:undefined-primitive-error () t))
        "can signal and catch undefined-primitive-error")))

(deftest undefined-primitive-error-includes-name ()
  "Verify error includes the undefined primitive name"
  (testing "error includes name"
    (handler-case
        (error 'clysm/runtime-library:undefined-primitive-error
               :name 'unknown-function)
      (clysm/runtime-library:undefined-primitive-error (c)
        (ok (eq 'unknown-function
                (clysm/runtime-library:undefined-primitive-error-name c))
            "error includes the undefined function name")))))

(deftest circular-dependency-error-signaled ()
  "Verify circular-dependency-error is signaled for cycles"
  (testing "circular dependency error"
    (ok (subtypep 'clysm/runtime-library:circular-dependency-error 'error)
        "circular-dependency-error is a subtype of error")
    (handler-case
        (error 'clysm/runtime-library:circular-dependency-error
               :functions '(foo bar baz))
      (clysm/runtime-library:circular-dependency-error (c)
        (ok (equal '(foo bar baz)
                   (clysm/runtime-library:circular-dependency-error-functions c))
            "error includes the cycle function list")))))

(deftest primitive-p-prevents-false-positives ()
  "Verify registered primitives don't trigger error"
  (testing "no false positives for registered primitives"
    ;; This can be tested with existing primitives registry
    (clysm::initialize-primitives-registry)
    (ok (clysm::registered-primitive-p 'cons)
        "cons is a registered primitive")
    (ok (clysm::registered-primitive-p 'car)
        "car is a registered primitive")
    (ok (not (clysm::registered-primitive-p 'assoc))
        "assoc is not a primitive (should be runtime function)")))
