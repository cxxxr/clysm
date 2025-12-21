# Contract: Wasm Binary Section Structure

**Date**: 2025-12-21
**Plan Reference**: [../plan.md](../plan.md)
**Validation Tool**: `wasm-tools validate`

## Overview

Clysmが生成するWebAssemblyバイナリのセクション構造契約。
すべての生成バイナリはこの契約に準拠し、`wasm-tools validate`をパスしなければならない。

---

## 1. Module Header

### Contract

```
Offset  Size  Value       Description
------  ----  ----------  -----------
0x00    4     0x00617363  Magic number "\0asm"
0x04    4     0x01000000  Version 1 (little-endian)
```

### Validation

```lisp
(deftest test-module-header
  (let ((module (emit-empty-module)))
    (ok (equalp #(#x00 #x61 #x73 #x6d) (subseq module 0 4))
        "Magic number must be \\0asm")
    (ok (equalp #(#x01 #x00 #x00 #x00) (subseq module 4 8))
        "Version must be 1")))
```

---

## 2. Section Order

### Contract

セクションは以下の順序で出力しなければならない（MUST）。
同一IDのセクションは連続してはならない（MUST NOT）。

| Order | ID | Name | Required |
|-------|-----|------|----------|
| 1 | 0 | Custom | Optional |
| 2 | 1 | Type | Required |
| 3 | 2 | Import | Optional |
| 4 | 3 | Function | Required (if Code exists) |
| 5 | 4 | Table | Optional |
| 6 | 5 | Memory | Optional |
| 7 | 6 | Global | Optional |
| 8 | 7 | Export | Optional |
| 9 | 8 | Start | Optional |
| 10 | 9 | Element | Optional |
| 11 | 10 | Code | Required (if Function exists) |
| 12 | 11 | Data | Optional |
| 13 | 12 | DataCount | Optional |
| 14 | 13 | Tag | Optional (EH Proposal) |

### Validation

```lisp
(deftest test-section-order
  (let ((sections (parse-sections (emit-module ...))))
    (ok (section-order-valid-p sections)
        "Sections must appear in ascending ID order")))

(defun section-order-valid-p (sections)
  (loop for (prev curr) on sections
        while curr
        always (< (section-id prev) (section-id curr))))
```

---

## 3. Section Format

### Contract

各セクションは以下の形式に従う:

```
Offset  Size      Description
------  --------  -----------
0       1         Section ID (0-13)
1       varuint   Content size (LEB128)
1+n     size      Section content
```

### Validation

```lisp
(deftest test-section-format
  (let ((section (first (parse-sections module))))
    (ok (<= 0 (section-id section) 13)
        "Section ID must be 0-13")
    (ok (= (length (section-content section))
           (section-declared-size section))
        "Content size must match declared size")))
```

---

## 4. Type Section (ID 1)

### Contract

```
Content:
  count: varuint32       ; number of types
  types: functype*       ; type definitions

functype:
  0x60                   ; func type tag
  params: vec(valtype)   ; parameter types
  results: vec(valtype)  ; result types

;; WasmGC extensions
structtype:
  0x5f                   ; struct type tag
  fields: vec(field)

arraytype:
  0x5e                   ; array type tag
  element: storagetype
```

### Required Types for Clysm

```wat
;; Minimum required types
(type $nil (struct))
(type $unbound (struct))
(type $cons (struct (field $car (mut anyref)) (field $cdr (mut anyref))))
(type $symbol (struct
  (field $name (ref $string))
  (field $value (mut anyref))
  (field $function (mut anyref))
  (field $plist (mut anyref))
  (field $package (mut anyref))))
(type $string (array (mut i8)))
```

### Validation

```lisp
(deftest test-type-section
  (let ((types (parse-type-section module)))
    (ok (find-type '$nil types) "Must define $nil type")
    (ok (find-type '$cons types) "Must define $cons type")
    (ok (find-type '$symbol types) "Must define $symbol type")))
```

---

## 5. Function Section (ID 3)

### Contract

```
Content:
  count: varuint32       ; number of functions
  types: varuint32*      ; type indices for each function
```

Function SectionとCode Sectionの要素数は一致しなければならない（MUST）。

### Validation

```lisp
(deftest test-function-section
  (let ((func-count (parse-function-section module))
        (code-count (parse-code-section module)))
    (ok (= func-count code-count)
        "Function and Code section counts must match")))
```

---

## 6. Global Section (ID 6)

### Contract

```
Content:
  count: varuint32       ; number of globals
  globals: global*

global:
  type: globaltype       ; type and mutability
  init: expr             ; initialization expression

globaltype:
  valtype                ; value type
  mut: 0x00 | 0x01      ; 0=immutable, 1=mutable
```

### Required Globals for Clysm

```wat
(global $NIL (ref $nil) (struct.new $nil))
(global $UNBOUND (ref $unbound) (struct.new $unbound))
(global $mv-count (mut i32) (i32.const 1))
(global $binding-stack (mut (ref null $binding-frame)) (ref.null $binding-frame))
```

### Validation

```lisp
(deftest test-global-section
  (let ((globals (parse-global-section module)))
    (ok (find-global '$NIL globals) "Must define $NIL global")
    (ok (find-global '$UNBOUND globals) "Must define $UNBOUND global")))
```

---

## 7. Export Section (ID 7)

### Contract

```
Content:
  count: varuint32       ; number of exports
  exports: export*

export:
  name: name             ; export name (UTF-8)
  kind: 0x00-0x03       ; 0=func, 1=table, 2=memory, 3=global
  index: varuint32       ; index of exported item
```

### Validation

```lisp
(deftest test-export-section
  (let ((exports (parse-export-section module)))
    (ok (every #'valid-utf8-p (mapcar #'export-name exports))
        "Export names must be valid UTF-8")))
```

---

## 8. Code Section (ID 10)

### Contract

```
Content:
  count: varuint32       ; number of function bodies
  bodies: code*

code:
  size: varuint32        ; body size in bytes
  body: func_body

func_body:
  locals: vec(local)     ; local variable declarations
  expr: expr             ; function body (ends with 0x0b)

local:
  count: varuint32       ; number of locals of this type
  type: valtype          ; type of locals
```

### Validation

```lisp
(deftest test-code-section
  (let ((code (parse-code-section module)))
    (ok (every #'ends-with-end-opcode-p code)
        "All function bodies must end with 0x0b (end)")))

(defun ends-with-end-opcode-p (body)
  (= #x0b (aref body (1- (length body)))))
```

---

## 9. Tag Section (ID 13) - EH Proposal

### Contract

```
Content:
  count: varuint32       ; number of tags
  tags: tag*

tag:
  attribute: 0x00        ; exception attribute
  type: varuint32        ; type index (function signature)
```

### Required Tags for Clysm

```wat
(tag $block-tag (param anyref anyref))  ; block/return-from
(tag $throw-tag (param anyref anyref))  ; catch/throw
(tag $error-tag (param anyref))         ; general error
```

### Validation

```lisp
(deftest test-tag-section
  (when (has-exception-handling-p module)
    (let ((tags (parse-tag-section module)))
      (ok (find-tag '$block-tag tags) "Must define $block-tag")
      (ok (find-tag '$throw-tag tags) "Must define $throw-tag"))))
```

---

## 10. LEB128 Encoding

### Contract

すべての可変長整数はLEB128形式でエンコードしなければならない（MUST）。

**Unsigned LEB128**:
- 7ビットずつ分割
- 継続ビット (MSB) は0で終端

**Signed LEB128**:
- 7ビットずつ分割（符号拡張）
- 継続ビット (MSB) は0で終端
- 最後のバイトの6ビット目が符号

### Test Vectors

| Value | Unsigned | Signed |
|-------|----------|--------|
| 0 | `00` | `00` |
| 1 | `01` | `01` |
| 127 | `7f` | `ff 00` |
| 128 | `80 01` | `80 01` |
| 255 | `ff 01` | `ff 01` |
| -1 | N/A | `7f` |
| -64 | N/A | `40` |
| -65 | N/A | `bf 7f` |
| 16384 | `80 80 01` | `80 80 01` |

### Validation

```lisp
(deftest test-leb128-roundtrip
  (loop for n in '(0 1 127 128 255 -1 -64 -65 16384)
        do (ok (= n (decode-leb128 (encode-leb128 n)))
               (format nil "Roundtrip for ~d" n))))
```

---

## 11. Validation Command

### Primary Validation

```bash
wasm-tools validate output.wasm
```

**Expected Output**: (exit code 0, no output on success)

### Verbose Validation

```bash
wasm-tools validate -v output.wasm
```

### WAT Conversion Check

```bash
wasm-tools print output.wasm > output.wat
wat2wasm output.wat -o output2.wasm
diff output.wasm output2.wasm
```

---

## 12. Error Conditions

### Section Order Violations

```
Error: section out of order: got ID 3 after ID 6
```

### Type Mismatch

```
Error: type mismatch: expected i32, got anyref
```

### Invalid LEB128

```
Error: invalid LEB128 encoding at offset 0x42
```

### Missing End Opcode

```
Error: function body missing end opcode
```

---

## 13. Contract Compliance Checklist

- [ ] Module header is `\0asm\01\00\00\00`
- [ ] Sections appear in ascending ID order
- [ ] Each section size matches content length
- [ ] Type section defines required GC types
- [ ] Function and Code section counts match
- [ ] All function bodies end with `0x0b`
- [ ] All LEB128 values are correctly encoded
- [ ] `wasm-tools validate` passes
- [ ] `wasm-tools print` produces valid WAT
