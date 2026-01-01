;;;; lookup-test.lisp - Unit tests for get-primitive and registered-primitive-p
;;;; Feature 001-runtime-library-system
;;;; Task T011: Unit test for get-primitive and registered-primitive-p

(in-package #:clysm/tests)

(deftest get-primitive-returns-primitive ()
  "Verify get-primitive returns the stored primitive struct"
  (testing "successful lookup"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::clear-primitives-registry)

            (clysm::register-primitive 'lookup-test-prim
                                       :wasm-emitter #'list
                                       :category :memory)

            (let ((result (clysm::get-primitive 'lookup-test-prim)))
              (ok (clysm::primitive-p result)
                  "returns primitive struct")
              (ok (eq (clysm::primitive-name result) 'lookup-test-prim)
                  "returns correct primitive")))
        (setf clysm::*primitives-registry* saved-registry)))))

(deftest get-primitive-returns-nil-for-missing ()
  "Verify get-primitive returns NIL for unregistered primitives"
  (testing "missing primitive lookup"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::clear-primitives-registry)
            (ok (null (clysm::get-primitive 'nonexistent-prim))
                "returns NIL for missing primitive"))
        (setf clysm::*primitives-registry* saved-registry)))))

(deftest registered-primitive-p-returns-boolean ()
  "Verify registered-primitive-p returns T or NIL correctly"
  (testing "boolean predicate behavior"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::clear-primitives-registry)

            ;; Before registration
            (ok (not (clysm::registered-primitive-p 'pred-test-prim))
                "returns NIL before registration")

            ;; After registration
            (clysm::register-primitive 'pred-test-prim :category :predicate)
            (ok (eq (clysm::registered-primitive-p 'pred-test-prim) t)
                "returns T after registration"))
        (setf clysm::*primitives-registry* saved-registry)))))

(deftest list-primitives-returns-sorted-names ()
  "Verify list-primitives returns sorted list of names"
  (testing "listing primitives"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::clear-primitives-registry)

            (clysm::register-primitive 'z-prim :category :memory)
            (clysm::register-primitive 'a-prim :category :memory)
            (clysm::register-primitive 'm-prim :category :memory)

            (let ((names (clysm::list-primitives)))
              (ok (= (length names) 3)
                  "returns all primitives")
              (ok (equal names '(a-prim m-prim z-prim))
                  "returns sorted by name")))
        (setf clysm::*primitives-registry* saved-registry)))))

(deftest clear-primitives-registry-removes-all ()
  "Verify clear-primitives-registry empties the registry"
  (testing "clearing registry"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::register-primitive 'clear-test-1 :category :memory)
            (clysm::register-primitive 'clear-test-2 :category :memory)

            (clysm::clear-primitives-registry)

            (ok (= (hash-table-count clysm::*primitives-registry*) 0)
                "registry is empty after clear"))
        (setf clysm::*primitives-registry* saved-registry)))))
