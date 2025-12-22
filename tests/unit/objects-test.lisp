;;;; objects-test.lisp - Runtime object tests
(in-package #:clysm/tests/unit/objects)

;;; T027: NIL/UNBOUND singleton tests

(deftest test-nil-singleton-creation
  "NIL singleton should be created as a global constant"
  (let ((nil-global (clysm/runtime/objects:make-nil-global)))
    (ok nil-global "NIL global should be created")
    (ok (eq :const (clysm/runtime/objects:global-mutability nil-global))
        "NIL should be immutable (const)")))

(deftest test-nil-singleton-uniqueness
  "NIL should be represented by a single global instance"
  (let ((nil1 (clysm/runtime/objects:nil-global-index))
        (nil2 (clysm/runtime/objects:nil-global-index)))
    (ok (= nil1 nil2) "NIL global index should be consistent")))

(deftest test-unbound-sentinel-creation
  "UNBOUND sentinel should be created as a global constant"
  (let ((unbound-global (clysm/runtime/objects:make-unbound-global)))
    (ok unbound-global "UNBOUND global should be created")
    (ok (eq :const (clysm/runtime/objects:global-mutability unbound-global))
        "UNBOUND should be immutable (const)")))

(deftest test-unbound-sentinel-uniqueness
  "UNBOUND should be represented by a single global instance"
  (let ((unbound1 (clysm/runtime/objects:unbound-global-index))
        (unbound2 (clysm/runtime/objects:unbound-global-index)))
    (ok (= unbound1 unbound2) "UNBOUND global index should be consistent")))

(deftest test-nil-is-not-unbound
  "NIL and UNBOUND should be distinct"
  (let ((nil-idx (clysm/runtime/objects:nil-global-index))
        (unbound-idx (clysm/runtime/objects:unbound-global-index)))
    (ok (not (= nil-idx unbound-idx))
        "NIL and UNBOUND should have different global indices")))

(deftest test-nil-nullability
  "NIL check should use ref.eq against NIL global (Constitution: NIL is NOT Wasm null)"
  (let ((nil-test-code (clysm/runtime/objects:emit-nil-check)))
    (ok nil-test-code "NIL check code should be generated")
    ;; Constitution II: NIL must NOT be Wasm null, so we use ref.eq
    (ok (member :ref.eq nil-test-code :test #'eq)
        "NIL check should use ref.eq against NIL global")))
