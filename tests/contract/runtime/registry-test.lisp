;;;; registry-test.lisp - Contract tests for primitives registry
;;;; Feature 001-runtime-library-system
;;;; Task T009: Contract test for registry hash-table

(in-package #:clysm/tests)

(deftest primitives-registry-is-hash-table ()
  "Verify *primitives-registry* is a hash-table with eq test"
  (testing "registry exists and is properly configured"
    (ok (hash-table-p clysm::*primitives-registry*)
        "*primitives-registry* is a hash-table")
    (ok (eq (hash-table-test clysm::*primitives-registry*) 'eq)
        "registry uses eq for symbol comparison")))

(deftest primitives-registry-stores-primitives ()
  "Verify registry can store and retrieve primitives"
  (testing "basic registry operations work"
    ;; Save current state
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            ;; Clear for test isolation
            (clysm::clear-primitives-registry)

            ;; Register a test primitive
            (clysm::register-primitive 'test-op
                                       :wasm-emitter (lambda (args env) nil)
                                       :signature '((any any) any)
                                       :category :memory)

            ;; Verify it's stored
            (ok (clysm::registered-primitive-p 'test-op)
                "registered primitive is found")
            (ok (not (clysm::registered-primitive-p 'nonexistent))
                "unregistered primitive not found")

            ;; Verify retrieval returns correct struct
            (let ((retrieved (clysm::get-primitive 'test-op)))
              (ok (clysm::primitive-p retrieved)
                  "get-primitive returns primitive struct")
              (ok (eq (clysm::primitive-name retrieved) 'test-op)
                  "retrieved primitive has correct name")))
        ;; Restore state
        (setf clysm::*primitives-registry* saved-registry)))))

(deftest primitives-registry-categories-work ()
  "Verify primitives can be filtered by category"
  (testing "category filtering works"
    (let ((saved-registry (alexandria:copy-hash-table clysm::*primitives-registry*)))
      (unwind-protect
          (progn
            (clysm::clear-primitives-registry)

            ;; Register primitives in different categories
            (clysm::register-primitive 'mem-op :category :memory)
            (clysm::register-primitive 'pred-op :category :predicate)
            (clysm::register-primitive 'arith-op :category :arithmetic)

            ;; Test category filtering
            (let ((memory-prims (clysm::list-primitives :memory))
                  (pred-prims (clysm::list-primitives :predicate))
                  (all-prims (clysm::list-primitives)))
              (ok (member 'mem-op memory-prims)
                  "memory category contains mem-op")
              (ok (not (member 'pred-op memory-prims))
                  "memory category excludes pred-op")
              (ok (= (length all-prims) 3)
                  "list-primitives without filter returns all")))
        (setf clysm::*primitives-registry* saved-registry)))))
