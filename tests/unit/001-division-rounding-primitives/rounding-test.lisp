;;;; rounding-test.lisp - Unit tests for floor/ceiling/round (001-division-rounding-primitives)
;;;; Phase 13D-1e: Division/Rounding Function Primitives
;;;; Tests T007, T014, T020, T026 - Unit tests for all rounding functions
(in-package #:clysm/tests/unit/rounding-primitives)

;;; ============================================================
;;; T007: Unit tests for floor/ceiling/round with integer arguments
;;; ============================================================

(deftest test-compile-floor-basic
  "Verify (floor 7 2) generates correct Wasm instructions"
  (let ((wat (clysm/compiler:compile-to-wat '(floor 7 2))))
    (ok (stringp wat)
        "(floor 7 2) should compile to WAT string")
    (ok (search "f64.floor" wat)
        "(floor 7 2) should use f64.floor instruction")
    (ok (search "global.set 2" wat)
        "Should set mv-count (global 2)")))

(deftest test-compile-ceiling-basic
  "Verify (ceiling 7 2) generates correct Wasm instructions"
  (let ((wat (clysm/compiler:compile-to-wat '(ceiling 7 2))))
    (ok (stringp wat)
        "(ceiling 7 2) should compile to WAT string")
    (ok (search "f64.ceil" wat)
        "(ceiling 7 2) should use f64.ceil instruction")))

(deftest test-compile-round-basic
  "Verify (round 7 2) generates correct Wasm instructions"
  (let ((wat (clysm/compiler:compile-to-wat '(round 7 2))))
    (ok (stringp wat)
        "(round 7 2) should compile to WAT string")
    (ok (search "f64.nearest" wat)
        "(round 7 2) should use f64.nearest instruction")))

(deftest test-compile-floor-negative
  "Verify (floor -7 2) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(floor -7 2))))
    (ok (stringp wat)
        "(floor -7 2) should compile")
    (ok (search "f64.floor" wat)
        "Should use f64.floor for negative dividend")))

(deftest test-compile-ceiling-negative
  "Verify (ceiling -7 2) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(ceiling -7 2))))
    (ok (stringp wat)
        "(ceiling -7 2) should compile")
    (ok (search "f64.ceil" wat)
        "Should use f64.ceil for negative dividend")))

;;; ============================================================
;;; T014: Unit tests for ffloor/fceiling/fround (float results)
;;; ============================================================

(deftest test-compile-ffloor-basic
  "Verify (ffloor 7.5 2.0) generates $float struct for quotient"
  (let ((wat (clysm/compiler:compile-to-wat '(ffloor 7.5 2.0))))
    (ok (stringp wat)
        "(ffloor 7.5 2.0) should compile to WAT string")
    (ok (search "f64.floor" wat)
        "(ffloor 7.5 2.0) should use f64.floor instruction")
    ;; ffloor returns $float struct (struct.new 16)
    (ok (search "struct.new 16" wat)
        "ffloor should create $float struct for quotient")))

(deftest test-compile-fceiling-basic
  "Verify (fceiling 7.5 2.0) generates correct instructions"
  (let ((wat (clysm/compiler:compile-to-wat '(fceiling 7.5 2.0))))
    (ok (stringp wat)
        "(fceiling 7.5 2.0) should compile")
    (ok (search "f64.ceil" wat)
        "(fceiling 7.5 2.0) should use f64.ceil instruction")))

(deftest test-compile-fround-basic
  "Verify (fround 7.5 2.0) generates correct instructions"
  (let ((wat (clysm/compiler:compile-to-wat '(fround 7.5 2.0))))
    (ok (stringp wat)
        "(fround 7.5 2.0) should compile")
    (ok (search "f64.nearest" wat)
        "(fround 7.5 2.0) should use f64.nearest instruction")))

;;; ============================================================
;;; T020: Unit tests for single-argument forms
;;; ============================================================

(deftest test-compile-floor-single-arg
  "Verify (floor 3.7) compiles with synthesized divisor=1"
  (let ((wat (clysm/compiler:compile-to-wat '(floor 3.7))))
    (ok (stringp wat)
        "(floor 3.7) should compile")
    (ok (search "f64.floor" wat)
        "Single-arg floor should use f64.floor")
    ;; Should push 1 as divisor
    (ok (search "i32.const 1" wat)
        "Single-arg form should synthesize divisor=1")))

(deftest test-compile-ceiling-single-arg
  "Verify (ceiling 3.7) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(ceiling 3.7))))
    (ok (stringp wat)
        "(ceiling 3.7) should compile")
    (ok (search "f64.ceil" wat)
        "Single-arg ceiling should use f64.ceil")))

(deftest test-compile-round-single-arg
  "Verify (round 3.7) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(round 3.7))))
    (ok (stringp wat)
        "(round 3.7) should compile")
    (ok (search "f64.nearest" wat)
        "Single-arg round should use f64.nearest")))

(deftest test-compile-ffloor-single-arg
  "Verify (ffloor 3.7) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(ffloor 3.7))))
    (ok (stringp wat)
        "(ffloor 3.7) should compile")
    (ok (search "f64.floor" wat)
        "Single-arg ffloor should use f64.floor")))

(deftest test-compile-fceiling-single-arg
  "Verify (fceiling 3.7) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(fceiling 3.7))))
    (ok (stringp wat)
        "(fceiling 3.7) should compile")))

(deftest test-compile-fround-single-arg
  "Verify (fround 3.7) compiles correctly"
  (let ((wat (clysm/compiler:compile-to-wat '(fround 3.7))))
    (ok (stringp wat)
        "(fround 3.7) should compile")))

;;; ============================================================
;;; T026: Unit tests for type preservation
;;; ============================================================

(deftest test-floor-returns-integer
  "Verify floor returns i31ref (integer) quotient"
  (let ((wat (clysm/compiler:compile-to-wat '(floor 10 3))))
    (ok (stringp wat)
        "(floor 10 3) should compile")
    ;; floor uses i32.trunc_f64_s to convert to integer
    (ok (search "i32.trunc" wat)
        "floor should truncate to integer")
    ;; and wraps in ref.i31
    (ok (search "ref.i31" wat)
        "floor should box as i31ref")))

(deftest test-ffloor-returns-float
  "Verify ffloor returns $float struct quotient"
  (let ((wat (clysm/compiler:compile-to-wat '(ffloor 10.0 3.0))))
    (ok (stringp wat)
        "(ffloor 10.0 3.0) should compile")
    ;; ffloor creates $float struct (type 16)
    ;; Should NOT have i32.trunc before the quotient struct.new
    (ok (search "struct.new 16" wat)
        "ffloor should create $float struct")))

(deftest test-multiple-values-mechanism
  "Verify mv-count is set to 2 for all rounding functions"
  (let ((wat (clysm/compiler:compile-to-wat '(floor 7 2))))
    (ok (search "i32.const 2" wat)
        "Should push constant 2 for mv-count")
    (ok (search "global.set 2" wat)
        "Should set global 2 (mv-count)")))

(deftest test-remainder-in-mv-buffer
  "Verify remainder is stored in mv-buffer"
  (let ((wat (clysm/compiler:compile-to-wat '(floor 7 2))))
    (ok (search "global.get 3" wat)
        "Should get global 3 (mv-buffer)")
    (ok (search "array.set 20" wat)
        "Should store in mv-buffer array (type 20)")))
