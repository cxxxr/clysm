# Data Model: FFI Foundation

**Date**: 2025-12-24
**Branch**: `012-ffi-foundation`

## Overview

FFI機能を支えるデータ構造の定義。コンパイラ内部表現とWasmバイナリ生成の両方をカバー。

---

## 1. FFI Declaration Types

### ForeignFunctionDecl

ホスト関数のインポート宣言を表す構造体。

```lisp
(defstruct (foreign-function-decl (:conc-name ffd-))
  "Declaration for an imported host function."
  (lisp-name nil :type symbol)           ; Lisp側の関数名
  (module-name nil :type string)         ; Wasm module name (e.g., "host")
  (field-name nil :type string)          ; Wasm field name (e.g., "console.log")
  (param-types nil :type list)           ; 引数の型リスト (:fixnum :string ...)
  (return-type nil :type keyword)        ; 戻り値の型 (:fixnum :void ...)
  (type-index nil :type (or null fixnum))) ; Generated type index
```

**Validation Rules**:
- `lisp-name`: 有効なシンボル（キーワード不可）
- `module-name`: 非空文字列
- `field-name`: 非空文字列
- `param-types`: MarshalTypeのリスト
- `return-type`: MarshalType または :void

### ExportDecl

Lisp関数のエクスポート宣言を表す構造体。

```lisp
(defstruct (export-decl (:conc-name ed-))
  "Declaration for an exported Lisp function."
  (lisp-name nil :type symbol)           ; Lisp関数シンボル
  (export-name nil :type string)         ; Wasm export name
  (param-types nil :type list)           ; 引数の型リスト
  (return-type nil :type keyword)        ; 戻り値の型
  (wrapper-func-index nil :type (or null fixnum))) ; Generated wrapper function index
```

**Validation Rules**:
- `lisp-name`: 関数として定義済みのシンボル
- `export-name`: 有効なWasm export name（ASCII、先頭非数字）
- `param-types`/`return-type`: MarshalType

---

## 2. Marshal Types

### MarshalType Enumeration

FFIでサポートするマーシャリング可能な型。

```lisp
(deftype marshal-type ()
  "Type identifier for FFI marshalling."
  '(member :fixnum :float :string :boolean :anyref :void))
```

### Type Mapping

| MarshalType | Lisp Runtime | Wasm Valtype | Notes |
|-------------|--------------|--------------|-------|
| :fixnum | i31ref | i32 | Sign-extended 31-bit integer |
| :float | (ref $float) | f64 | IEEE 754 double |
| :string | (ref $string) | externref | WasmGC array of i8 |
| :boolean | t / NIL singleton | i32 (1/0) | NIL = 0, T = 1 |
| :anyref | anyref | anyref | Passthrough, no conversion |
| :void | N/A | (none) | No return value |

### MarshalSpec

引数または戻り値のマーシャリング仕様。

```lisp
(defstruct (marshal-spec (:conc-name ms-))
  "Specification for marshalling a single value."
  (type nil :type marshal-type)          ; 型識別子
  (direction nil :type (member :in :out)) ; 入力 or 出力
  (position nil :type (or null fixnum))) ; 引数位置 (0-indexed) or nil for return
```

---

## 3. Wasm IR Extensions

### WasmImport

Wasm importエントリのIR表現。

```lisp
(defstruct (wasm-import (:conc-name wi-))
  "Wasm import entry."
  (module-name nil :type string)         ; Import module name
  (field-name nil :type string)          ; Import field name
  (kind nil :type (member :func :table :memory :global))
  (type-index nil :type (or null fixnum)) ; For :func kind
  (table-type nil)                        ; For :table kind
  (memory-type nil)                       ; For :memory kind
  (global-type nil))                      ; For :global kind
```

### WasmExport (既存拡張)

Wasm exportエントリは既存の3要素リスト形式を維持。

```lisp
;; Existing format: (name kind index)
;; Example: ("calculateTax" :func 42)
```

---

## 4. FFI Environment

### FFIEnvironment

FFI宣言を管理するコンパイル時環境。

```lisp
(defstruct (ffi-environment (:conc-name ffi-env-))
  "Compile-time environment for FFI declarations."
  (imports (make-hash-table :test 'equal) :type hash-table)  ; module.field -> ForeignFunctionDecl
  (exports nil :type list)                                    ; List of ExportDecl
  (next-import-func-index 0 :type fixnum)                    ; Next available import function index
  (type-cache (make-hash-table :test 'equal) :type hash-table)) ; Signature -> type index
```

**Lifecycle**:
1. コンパイル開始時に作成
2. `ffi:define-foreign-function`マクロ展開時にimportsに登録
3. `ffi:export-function`マクロ展開時にexportsに登録
4. コード生成時にImport/Exportセクション生成

---

## 5. Runtime Structures

### FFI Call Context

実行時のFFI呼び出しコンテキスト（将来のエラーハンドリング拡張用）。

```lisp
(defstruct (ffi-call-context (:conc-name fcc-))
  "Runtime context for FFI calls."
  (function-name nil :type string)       ; Called function name
  (timestamp nil :type (or null integer))) ; Call timestamp for debugging
```

### FFI Error Condition

ホスト関数呼び出しエラーを表すコンディション。

```lisp
(define-condition ffi-host-error (error)
  ((function-name :initarg :function-name :reader ffi-error-function-name)
   (message :initarg :message :reader ffi-error-message))
  (:report (lambda (c s)
             (format s "FFI call to ~A failed: ~A"
                     (ffi-error-function-name c)
                     (ffi-error-message c)))))
```

---

## 6. Entity Relationships

```
┌─────────────────────┐
│  FFIEnvironment     │
├─────────────────────┤
│ imports: hash-table ├──────┐
│ exports: list       │      │
└─────────────────────┘      │
                             ▼
          ┌──────────────────────────────┐
          │    ForeignFunctionDecl       │
          ├──────────────────────────────┤
          │ lisp-name: SYMBOL            │
          │ module-name: STRING          │
          │ field-name: STRING           │
          │ param-types: (MarshalType*)  │
          │ return-type: MarshalType     │
          │ type-index: FIXNUM           │
          └──────────────────────────────┘
                             │
                             ▼
          ┌──────────────────────────────┐
          │       WasmImport             │
          ├──────────────────────────────┤
          │ module-name: STRING          │
          │ field-name: STRING           │
          │ kind: :FUNC                  │
          │ type-index: FIXNUM           │
          └──────────────────────────────┘
```

---

## 7. State Transitions

### ForeignFunctionDecl Lifecycle

```
[Created] ─── ffi:define-foreign-function ───▶ [Registered]
                                                   │
                                    compile-module │
                                                   ▼
                                            [Type Assigned]
                                                   │
                                     emit-imports  │
                                                   ▼
                                            [Emitted]
```

### ExportDecl Lifecycle

```
[Created] ─── ffi:export-function ───▶ [Registered]
                                            │
                              compile-module │
                                            ▼
                                    [Wrapper Generated]
                                            │
                              emit-exports   │
                                            ▼
                                      [Emitted]
```
