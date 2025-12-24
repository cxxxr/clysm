# Research: FFI Foundation

**Date**: 2025-12-24
**Branch**: `012-ffi-foundation`

## Overview

FFI（Foreign Function Interface）実装のための技術調査結果。WasmGC環境でのホスト相互運用パターンを確立する。

---

## 1. Wasm Import/Export機構

### Decision: Core Wasm Import/Export（Component Modelではなく）

**Rationale**:
- wasmtimeとブラウザの両方で安定サポート
- WasmGC proposalとの親和性が高い
- Component Modelは将来拡張として位置付け

**Alternatives Considered**:
1. **Component Model + WIT**: より型安全だが、WasmGCとの統合がまだ成熟していない
2. **WASI Preview 2**: I/O機能に特化しており、汎用FFIには不適

### Import Section Encoding

```text
Import Section (ID 2):
  count: u32 (LEB128)
  entries:
    module_name: name (length-prefixed UTF-8)
    field_name: name
    import_desc: (0x00 typeidx | 0x01 tabletype | 0x02 memtype | 0x03 globaltype)
```

**For functions**: `import_desc = 0x00` followed by type index

### Export Section Encoding

```text
Export Section (ID 7):
  count: u32 (LEB128)
  entries:
    name: name (length-prefixed UTF-8)
    export_desc: (0x00 funcidx | 0x01 tableidx | 0x02 memidx | 0x03 globalidx)
```

---

## 2. Type Marshalling Strategy

### Decision: WasmGC Native Types（線形メモリ不使用）

**Rationale**:
- Constitution Principle I（WasmGC-First）に完全準拠
- ホストGCとの統合でメモリリーク防止
- 文字列・構造体の効率的な共有

### Type Mapping Table

| Lisp Type | Wasm Type | Direction | Notes |
|-----------|-----------|-----------|-------|
| fixnum | i32 | bidirectional | i31refから抽出/ボックス化 |
| float | f64 | bidirectional | $float構造体との相互変換 |
| string | (ref $string) / externref | bidirectional | WasmGC array (UTF-8) |
| boolean (t/nil) | i32 (1/0) | bidirectional | NILシングルトンとの比較 |
| anyref | anyref | passthrough | 変換なし、そのまま渡す |

### Marshalling Implementation

**Lisp → Wasm (outgoing)**:
1. 型検査（`ref.test`）
2. 型に応じた変換コード生成
3. Wasm関数引数として渡す

**Wasm → Lisp (incoming)**:
1. Wasm戻り値を受け取る
2. 型に応じたLispオブジェクト構築
3. anyrefとして返す

### String Handling (FR-006)

**Decision**: WasmGC array型を使用（線形メモリ不使用）

```wat
;; Lisp string type (既存)
(type $string (array (mut i8)))

;; String to host: pass (ref $string) directly as externref
;; Host to Lisp: receive externref, validate, cast to (ref $string)
```

**Rationale**:
- 線形メモリ不使用で Constitution準拠
- JavaScript側では `TextDecoder`/`TextEncoder` でUTF-8変換
- wasmtime側では `wasmtime::ExternRef` でGC参照を共有

---

## 3. Error Handling

### Decision: Wasm EH（Exception Handling）でホスト例外を捕捉

**Rationale**:
- Constitution Principle IV（Wasm制御フロー活用）に準拠
- `try_table`/`catch`は既存の`block/return-from`実装で使用済み
- 一貫したエラーハンドリングモデル

### Exception Propagation Flow

```
Host Exception (JS Error / Rust panic)
    ↓
Wasm trap
    ↓
try_table catch clause
    ↓
Lisp condition (ffi:host-error)
    ↓
handler-case / ignore-errors
```

**Implementation**:
```wat
(try_table (result anyref) (catch_all $handle_host_error)
  (call $imported_function ...)
)
(block $handle_host_error
  ;; Create ffi:host-error condition
  ;; Signal via standard mechanism
)
```

---

## 4. Module Naming Convention

### Decision: Dot-separated namespace（"host.module.function"）

**Rationale**:
- JavaScript/Node.jsの慣習に合致
- wasmtimeでも自然にマップ可能
- 階層的なAPI設計を許容

### Examples

```lisp
;; Import: "host.console.log" → module="host", field="console.log"
(ffi:define-foreign-function log-message "host.console.log" (:string) :void)

;; Import: "math.random" → module="math", field="random"
(ffi:define-foreign-function random-float "math.random" () :float)
```

**Parsing Rule**:
- 最初のドット前 = module name
- 最初のドット後 = field name

---

## 5. Export Function Wrapper

### Decision: トランポリン関数でクロージャを呼び出し

**Rationale**:
- Lisp関数はクロージャ構造体（Constitution III）
- Wasm exportはbare function
- ラッパー関数でブリッジ

### Implementation Pattern

```wat
;; Lisp function: (defun calculate-tax (amount) ...)
;; Stored in symbol function slot as $closure

;; Export wrapper (generated)
(func $export_calculateTax (param $amount i32) (result i32)
  ;; Load closure from symbol
  (local.get $amount)
  (call $marshal_i32_to_fixnum)  ;; Convert to Lisp fixnum
  (global.get $sym_calculate_tax)
  (struct.get $symbol $function)
  (ref.cast (ref $closure))
  (call_ref $func_1)            ;; Invoke closure with 1 arg
  (call $marshal_fixnum_to_i32)  ;; Convert result
)

(export "calculateTax" (func $export_calculateTax))
```

---

## 6. Host Environment Compatibility

### wasmtime Requirements

- Version: 27.0+ (WasmGC stable support)
- Features: `gc`, `exception-handling`, `reference-types`
- Host bindings: Rust `wasmtime` crate

### JavaScript/Browser Requirements

- Chrome 119+ / Firefox 120+ (WasmGC support)
- V8 flags not required for recent versions
- `WebAssembly.instantiate()` with imports object

### Node.js Requirements

- Node.js 22+ (V8 with WasmGC)
- Same imports pattern as browser

---

## 7. Testing Strategy

### Unit Tests (Common Lisp / rove)

- FFI type definitions
- Marshalling logic (pure Lisp transformation)
- Import/Export section generation

### Contract Tests (wasm-tools validate)

- Generated Wasm binary validation
- Section structure correctness
- Type index consistency

### Integration Tests (Host Environments)

**wasmtime (Rust)**:
```rust
let instance = Instance::new(&mut store, &module, &imports)?;
let result = instance.get_func(&mut store, "calculateTax")?
    .typed::<i32, i32>(&store)?
    .call(&mut store, 1000)?;
```

**Node.js (JavaScript)**:
```javascript
const { instance } = await WebAssembly.instantiate(wasmBuffer, {
  host: {
    'console.log': (msg) => console.log(msg),
    random: () => Math.random()
  }
});
const result = instance.exports.calculateTax(1000);
```

---

## Summary

| Topic | Decision | Impact |
|-------|----------|--------|
| Import mechanism | Core Wasm imports | Low complexity, broad compatibility |
| Type marshalling | WasmGC native types | Constitution compliant, no linear memory |
| String handling | WasmGC array (ref $string) | Direct sharing without copy |
| Error handling | Wasm EH (try_table) | Consistent with existing impl |
| Naming | Dot-separated namespace | Intuitive for host developers |
| Export wrapper | Trampoline function | Bridge closure to bare function |
| Host compatibility | wasmtime 27+, Chrome 119+, Node 22+ | Modern runtime requirements |
