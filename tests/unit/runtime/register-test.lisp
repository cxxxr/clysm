;;;; register-test.lisp - Unit tests for register-primitive
;;;; Feature 001-runtime-library-system
;;;; Task T010: Unit test for register-primitive

(in-package #:clysm/tests)

(deftest register-primitive-creates-primitive ()
  "Verify register-primitive creates and stores a primitive"
  (testing "basic registration"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::clear-primitives-registry)

            (let ((result (clysm::register-primitive 'my-prim
                                                     :wasm-emitter #'identity
                                                     :signature '((i32) i32)
                                                     :inline-p t
                                                     :category :arithmetic)))
              ;; Returns the primitive
              (ok (clysm::primitive-p result)
                  "register-primitive returns primitive struct")

              ;; All fields set correctly
              (ok (eq (clysm::primitive-name result) 'my-prim)
                  "name field set")
              (ok (eq (clysm::primitive-wasm-emitter result) #'identity)
                  "wasm-emitter field set")
              (ok (equal (clysm::primitive-signature result) '((i32) i32))
                  "signature field set")
              (ok (eq (clysm::primitive-inline-p result) t)
                  "inline-p field set")
              (ok (eq (clysm::primitive-category result) :arithmetic)
                  "category field set")))
        (setf clysm::*primitives-registry* saved-registry)))))

(deftest register-primitive-defaults-category ()
  "Verify register-primitive defaults category to :other"
  (testing "category defaults to :other"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::clear-primitives-registry)
            (let ((result (clysm::register-primitive 'no-category-prim)))
              (ok (eq (clysm::primitive-category result) :other)
                  "category defaults to :other when not specified")))
        (setf clysm::*primitives-registry* saved-registry)))))

(deftest register-primitive-replaces-existing ()
  "Verify registering same name replaces previous primitive"
  (testing "replacement behavior"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::clear-primitives-registry)

            ;; Register first version
            (clysm::register-primitive 'dup-prim :category :memory)

            ;; Register second version
            (clysm::register-primitive 'dup-prim :category :predicate)

            ;; Should have only one, with new category
            (let ((retrieved (clysm::get-primitive 'dup-prim)))
              (ok (eq (clysm::primitive-category retrieved) :predicate)
                  "later registration replaces earlier")))
        (setf clysm::*primitives-registry* saved-registry)))))
