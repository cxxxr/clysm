;;;; tests/unit/runtime-types-test.lisp - Runtime Type System Tests

(in-package #:clysm/tests)

;;; ============================================================
;;; Type Registry Tests
;;; ============================================================

(defsuite runtime-types-suite
    "Tests for WasmGC runtime type system")

(deftest test-make-type-registry ()
  "Test creating an empty type registry"
  (let ((registry (clysm:make-type-registry)))
    (is-true (not (null registry)))
    (is-eq nil (clysm:type-registry-cons registry))
    (is-eq nil (clysm:type-registry-symbol registry))
    (is-eq nil (clysm:type-registry-closure registry))))

(deftest test-register-core-types ()
  "Test registering all core Lisp types with a module"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    ;; All type indices should be set
    (is-true (integerp (clysm:type-registry-env registry)))
    (is-true (integerp (clysm:type-registry-string registry)))
    (is-true (integerp (clysm:type-registry-vector registry)))
    (is-true (integerp (clysm:type-registry-func-0 registry)))
    (is-true (integerp (clysm:type-registry-func-1 registry)))
    (is-true (integerp (clysm:type-registry-func-2 registry)))
    (is-true (integerp (clysm:type-registry-func-n registry)))
    (is-true (integerp (clysm:type-registry-cons registry)))
    (is-true (integerp (clysm:type-registry-symbol registry)))
    (is-true (integerp (clysm:type-registry-closure registry)))
    (is-true (integerp (clysm:type-registry-nil-type registry)))
    (is-true (integerp (clysm:type-registry-unbound registry)))))

(deftest test-type-indices-unique ()
  "Test that all type indices are unique"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (indices (list (clysm:type-registry-env registry)
                        (clysm:type-registry-string registry)
                        (clysm:type-registry-vector registry)
                        (clysm:type-registry-func-0 registry)
                        (clysm:type-registry-func-1 registry)
                        (clysm:type-registry-func-2 registry)
                        (clysm:type-registry-func-n registry)
                        (clysm:type-registry-cons registry)
                        (clysm:type-registry-symbol registry)
                        (clysm:type-registry-closure registry)
                        (clysm:type-registry-nil-type registry)
                        (clysm:type-registry-unbound registry))))
    ;; All indices should be unique
    (is-eql (length indices)
            (length (remove-duplicates indices)))))

(deftest test-type-index-accessors ()
  "Test type index accessor functions"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    (is-eql (clysm:type-registry-cons registry)
            (clysm:cons-type-index registry))
    (is-eql (clysm:type-registry-symbol registry)
            (clysm:symbol-type-index registry))
    (is-eql (clysm:type-registry-closure registry)
            (clysm:closure-type-index registry))
    (is-eql (clysm:type-registry-env registry)
            (clysm:env-type-index registry))
    (is-eql (clysm:type-registry-string registry)
            (clysm:string-type-index registry))
    (is-eql (clysm:type-registry-vector registry)
            (clysm:vector-type-index registry))
    (is-eql (clysm:type-registry-nil-type registry)
            (clysm:nil-type-index registry))
    (is-eql (clysm:type-registry-unbound registry)
            (clysm:unbound-type-index registry))))

;;; ============================================================
;;; Field Index Constants Tests
;;; ============================================================

(deftest test-cons-field-indices ()
  "Test cons cell field indices"
  (is-eql 0 clysm:+cons-car+)
  (is-eql 1 clysm:+cons-cdr+))

(deftest test-symbol-field-indices ()
  "Test symbol field indices"
  (is-eql 0 clysm:+symbol-name+)
  (is-eql 1 clysm:+symbol-value+)
  (is-eql 2 clysm:+symbol-function+)
  (is-eql 3 clysm:+symbol-plist+))

(deftest test-closure-field-indices ()
  "Test closure field indices"
  (is-eql 0 clysm:+closure-code-0+)
  (is-eql 1 clysm:+closure-code-1+)
  (is-eql 2 clysm:+closure-code-2+)
  (is-eql 3 clysm:+closure-code-n+)
  (is-eql 4 clysm:+closure-env+))

(deftest test-nil-field-indices ()
  "Test NIL singleton field indices"
  (is-eql 0 clysm:+nil-car+)
  (is-eql 1 clysm:+nil-cdr+)
  (is-eql 2 clysm:+nil-name+)
  (is-eql 3 clysm:+nil-value+)
  (is-eql 4 clysm:+nil-function+)
  (is-eql 5 clysm:+nil-plist+))

;;; ============================================================
;;; GC Instruction Emitter Tests
;;; ============================================================

(deftest test-emit-struct.new ()
  "Test struct.new instruction emission"
  (let ((bytes (clysm:emit-struct.new 5)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))   ; GC prefix
    (is-eql #x00 (second bytes))  ; struct.new opcode
    (is-eql 5 (third bytes))))    ; type index

(deftest test-emit-struct.get ()
  "Test struct.get instruction emission"
  (let ((bytes (clysm:emit-struct.get 3 1)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x02 (second bytes))  ; struct.get opcode
    (is-eql 3 (third bytes))      ; type index
    (is-eql 1 (fourth bytes))))   ; field index

(deftest test-emit-struct.set ()
  "Test struct.set instruction emission"
  (let ((bytes (clysm:emit-struct.set 3 0)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x05 (second bytes))  ; struct.set opcode
    (is-eql 3 (third bytes))      ; type index
    (is-eql 0 (fourth bytes))))   ; field index

(deftest test-emit-array.new ()
  "Test array.new instruction emission"
  (let ((bytes (clysm:emit-array.new 2)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x06 (second bytes))  ; array.new opcode
    (is-eql 2 (third bytes))))    ; type index

(deftest test-emit-array.new-fixed ()
  "Test array.new_fixed instruction emission"
  (let ((bytes (clysm:emit-array.new-fixed 2 10)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x08 (second bytes))  ; array.new_fixed opcode
    (is-eql 2 (third bytes))      ; type index
    (is-eql 10 (fourth bytes))))  ; length

(deftest test-emit-array.get ()
  "Test array.get instruction emission"
  (let ((bytes (clysm:emit-array.get 2)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x0B (second bytes))  ; array.get opcode
    (is-eql 2 (third bytes))))    ; type index

(deftest test-emit-array.set ()
  "Test array.set instruction emission"
  (let ((bytes (clysm:emit-array.set 2)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x0E (second bytes))  ; array.set opcode
    (is-eql 2 (third bytes))))    ; type index

(deftest test-emit-array.len ()
  "Test array.len instruction emission"
  (let ((bytes (clysm:emit-array.len)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x0F (second bytes))))  ; array.len opcode

(deftest test-emit-ref.i31 ()
  "Test ref.i31 instruction emission"
  (let ((bytes (clysm:emit-ref.i31)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x1C (second bytes))))  ; ref.i31 opcode

(deftest test-emit-i31.get-s ()
  "Test i31.get_s instruction emission"
  (let ((bytes (clysm:emit-i31.get-s)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x1D (second bytes))))  ; i31.get_s opcode

(deftest test-emit-i31.get-u ()
  "Test i31.get_u instruction emission"
  (let ((bytes (clysm:emit-i31.get-u)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x1E (second bytes))))  ; i31.get_u opcode

(deftest test-emit-ref.test ()
  "Test ref.test instruction emission"
  (let ((bytes (clysm:emit-ref.test 5)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x14 (second bytes))  ; ref.test opcode
    (is-eql 5 (third bytes))))    ; type index

(deftest test-emit-ref.cast ()
  "Test ref.cast instruction emission"
  (let ((bytes (clysm:emit-ref.cast 5)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x15 (second bytes))  ; ref.cast opcode
    (is-eql 5 (third bytes))))    ; type index

;;; ============================================================
;;; High-Level Object Emitter Tests
;;; ============================================================

(deftest test-emit-make-cons ()
  "Test cons creation instruction emission"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-make-cons registry)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x00 (second bytes))))  ; struct.new

(deftest test-emit-car ()
  "Test car instruction emission"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-car registry)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x02 (second bytes))))  ; struct.get

(deftest test-emit-cdr ()
  "Test cdr instruction emission"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-cdr registry)))
    (is-true (listp bytes))
    (is-eql #xFB (first bytes))
    (is-eql #x02 (second bytes))))  ; struct.get

(deftest test-emit-fixnum-conversions ()
  "Test fixnum conversion instructions"
  (let ((to-i31 (clysm:emit-fixnum-from-i32))
        (from-i31 (clysm:emit-fixnum-to-i32)))
    (is-equal '(#xFB #x1C) to-i31)    ; ref.i31
    (is-equal '(#xFB #x1D) from-i31)))  ; i31.get_s

(deftest test-emit-type-predicates ()
  "Test type predicate instruction emission"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (consp-bytes (clysm:emit-consp registry))
         (symbolp-bytes (clysm:emit-symbolp registry))
         (functionp-bytes (clysm:emit-functionp registry)))
    ;; All should emit ref.test (0xFB 0x14)
    (is-eql #xFB (first consp-bytes))
    (is-eql #x14 (second consp-bytes))
    (is-eql #xFB (first symbolp-bytes))
    (is-eql #x14 (second symbolp-bytes))
    (is-eql #xFB (first functionp-bytes))
    (is-eql #x14 (second functionp-bytes))))

;;; ============================================================
;;; Module Integration Tests
;;; ============================================================

(deftest test-module-with-types ()
  "Test creating a module with all core types"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    ;; Module should have 12 types registered
    (clysm:module-finalize module)
    (is-eql 12 (length (clysm:wasm-module-types module)))
    ;; Registry should be populated
    (is-true (not (null registry)))))

(deftest test-type-definitions-valid ()
  "Test that registered type definitions are valid"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    (declare (ignore registry))
    (clysm:module-finalize module)
    ;; Each type should have a valid definition
    (dolist (typedef (clysm:wasm-module-types module))
      (is-true (not (null (clysm:wasm-type-definition typedef))))
      (is-true (integerp (clysm:wasm-type-index typedef))))))

(deftest test-emit-module-with-types ()
  "Test emitting a module with core types to binary"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    (declare (ignore registry))
    ;; Should emit without error
    (let ((bytes (clysm:emit-wasm-binary module)))
      (is-true (listp bytes))
      (is-true (> (length bytes) 8))  ; At least header
      ;; Check magic number
      (is-eql #x00 (first bytes))
      (is-eql #x61 (second bytes))
      (is-eql #x73 (third bytes))
      (is-eql #x6D (fourth bytes)))))

;;; ============================================================
;;; WAT Output Tests
;;; ============================================================

(deftest test-wat-output-with-types ()
  "Test WAT output includes type definitions"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    (declare (ignore registry))
    (let ((wat (clysm:wat-to-string module)))
      (is (stringp wat))
      (is (search "(module" wat))
      (is (search "(type" wat))
      ;; Should have named types
      (is (search "$cons" wat))
      (is (search "$symbol" wat))
      (is (search "$closure" wat)))))
