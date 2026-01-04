;;;; tests/unit/wasm-types-test.lisp - Tests for Wasm type definitions

(in-package #:clysm/tests)

(defsuite :wasm-types)

;;; ============================================================
;;; Value Type Encoding
;;; ============================================================

(deftest encode-valtype-i32
  (is-equal '(#x7F) (encode-valtype :i32)))

(deftest encode-valtype-i64
  (is-equal '(#x7E) (encode-valtype :i64)))

(deftest encode-valtype-f32
  (is-equal '(#x7D) (encode-valtype :f32)))

(deftest encode-valtype-f64
  (is-equal '(#x7C) (encode-valtype :f64)))

(deftest encode-valtype-funcref
  (is-equal '(#x70) (encode-valtype :funcref)))

(deftest encode-valtype-externref
  (is-equal '(#x6F) (encode-valtype :externref)))

(deftest encode-valtype-anyref
  (is-equal '(#x6E) (encode-valtype :anyref)))

(deftest encode-valtype-i31ref
  (is-equal '(#x6C) (encode-valtype :i31ref)))

;;; ============================================================
;;; Function Type
;;; ============================================================

(deftest functype-no-params-no-results
  (let ((ft (make-functype nil nil)))
    (is-equal '(#x60 0 0) (encode-functype ft))))

(deftest functype-one-param-one-result
  (let ((ft (make-functype '(:i32) '(:i32))))
    ;; 0x60, vec(1, i32), vec(1, i32)
    (is-equal '(#x60 1 #x7F 1 #x7F) (encode-functype ft))))

(deftest functype-two-params-one-result
  (let ((ft (make-functype '(:i32 :i32) '(:i32))))
    (is-equal '(#x60 2 #x7F #x7F 1 #x7F) (encode-functype ft))))

(deftest functype-anyref-param
  (let ((ft (make-functype '(:anyref) '(:anyref))))
    (is-equal '(#x60 1 #x6E 1 #x6E) (encode-functype ft))))

;;; ============================================================
;;; Struct Type (WasmGC)
;;; ============================================================

(deftest structtype-empty
  (let ((st (make-structtype nil)))
    ;; 0x5F, vec(0)
    (is-equal '(#x5F 0) (encode-structtype st))))

(deftest structtype-one-field
  (let ((st (make-structtype
             (list (make-field :i32 :mutable t)))))
    ;; 0x5F, vec(1, (i32, mut=1))
    (is-equal '(#x5F 1 #x7F 1) (encode-structtype st))))

(deftest structtype-two-fields
  (let ((st (make-structtype
             (list (make-field :anyref :mutable t)
                   (make-field :anyref :mutable t)))))
    ;; $cons equivalent
    (is-equal '(#x5F 2 #x6E 1 #x6E 1) (encode-structtype st))))

(deftest structtype-immutable-field
  (let ((st (make-structtype
             (list (make-field :i32 :mutable nil)))))
    (is-equal '(#x5F 1 #x7F 0) (encode-structtype st))))

;;; ============================================================
;;; Array Type (WasmGC)
;;; ============================================================

(deftest arraytype-mutable
  (let ((at (make-arraytype :anyref :mutable t)))
    ;; 0x5E, field(anyref, mut=1)
    (is-equal '(#x5E #x6E 1) (encode-arraytype at))))

(deftest arraytype-immutable
  (let ((at (make-arraytype :i32 :mutable nil)))
    (is-equal '(#x5E #x7F 0) (encode-arraytype at))))

;;; ============================================================
;;; Export
;;; ============================================================

(deftest encode-export-func
  (let ((exp (make-export "add" :func 0)))
    ;; name "add" (3, 97, 100, 100), kind 0x00, index 0
    (is-equal '(3 97 100 100 #x00 0) (encode-export exp))))

(deftest encode-export-memory
  (let ((exp (make-export "memory" :memory 0)))
    ;; name "memory" (6 bytes), kind 0x02, index 0
    (let ((result (encode-export exp)))
      (is-eql 6 (car result))  ; length
      (is-eql #x02 (nth 7 result))  ; memory kind
      (is-eql 0 (nth 8 result)))))  ; index

;;; ============================================================
;;; Limits
;;; ============================================================

(deftest encode-limits-min-only
  (let ((lim (make-limits 1)))
    ;; 0x00, min=1
    (is-equal '(#x00 1) (encode-limits lim))))

(deftest encode-limits-min-max
  (let ((lim (make-limits 1 10)))
    ;; 0x01, min=1, max=10
    (is-equal '(#x01 1 10) (encode-limits lim))))

;;; ============================================================
;;; Module Builder
;;; ============================================================

(deftest module-create-empty
  (let ((mod (make-wasm-module)))
    (is (null (wasm-module-types mod)))
    (is (null (wasm-module-funcs mod)))
    (is (null (wasm-module-exports mod)))))

(deftest module-add-type-increments-index
  (let ((mod (make-wasm-module)))
    (let ((idx1 (module-add-type mod (make-wasm-type (make-functype nil nil))))
          (idx2 (module-add-type mod (make-wasm-type (make-functype '(:i32) '(:i32))))))
      (is-eql 0 idx1)
      (is-eql 1 idx2))))

(deftest module-finalize-reverses-lists
  (let ((mod (make-wasm-module)))
    (module-add-type mod (make-wasm-type (make-functype nil nil) :name "first"))
    (module-add-type mod (make-wasm-type (make-functype '(:i32) nil) :name "second"))
    (module-finalize mod)
    (is-equal "first" (wasm-type-name (first (wasm-module-types mod))))
    (is-equal "second" (wasm-type-name (second (wasm-module-types mod))))))
