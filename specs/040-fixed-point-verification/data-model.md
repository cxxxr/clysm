# Data Model: Fixed-Point Verification

**Feature**: 040-fixed-point-verification
**Date**: 2025-12-27

## Entities

### 1. verification-result

Primary entity representing a single fixed-point verification run.

```lisp
(defstruct verification-result
  "Result of a fixed-point verification run."
  ;; Status
  (status nil :type (member :achieved :not-achieved :compilation-error :missing-dependency))
  (timestamp nil :type string)       ; ISO 8601 format

  ;; Binary Info
  (stage1-info nil :type binary-info)
  (stage2-info nil :type binary-info)

  ;; Comparison Results
  (identical-p nil :type boolean)
  (first-diff-offset nil :type (or null integer))
  (diff-byte-count 0 :type integer)

  ;; Compilation Stats
  (compilation-rate 0.0 :type float)  ; 0.0 - 1.0
  (modules-compiled 0 :type integer)
  (modules-total 0 :type integer)

  ;; Timing
  (stage2-gen-time-ms 0 :type integer)
  (comparison-time-ms 0 :type integer)

  ;; Errors (if any)
  (error-message nil :type (or null string)))
```

**Relationships**:
- Contains two `binary-info` entities (Stage 1 and Stage 2)
- May be appended to `verification-history`

**Validation Rules**:
- `status` must be one of the four defined values
- `compilation-rate` must be between 0.0 and 1.0
- If `identical-p` is T, `first-diff-offset` must be NIL and `diff-byte-count` must be 0
- If `status` is `:achieved`, `identical-p` must be T

---

### 2. binary-info (Extended from 039)

Information about a Stage binary.

```lisp
(defstruct binary-info
  "Metadata about a Wasm binary file."
  (path "" :type string)
  (size-bytes 0 :type integer)
  (exports nil :type list)           ; List of export names
  (types 0 :type integer)            ; Type section count
  (functions 0 :type integer)        ; Function count
  (valid-p nil :type boolean))       ; wasm-tools validation result
```

**Relationships**:
- Contained by `verification-result`
- Contained by `diff-report`

---

### 3. fixpoint-status

Enumeration for verification status.

```lisp
(deftype fixpoint-status ()
  "Possible verification statuses."
  '(member :achieved :not-achieved :compilation-error :missing-dependency))
```

**Exit Code Mapping**:
| Status | Exit Code |
|--------|-----------|
| `:achieved` | 0 |
| `:not-achieved` | 1 |
| `:compilation-error` | 2 |
| `:missing-dependency` | 3 |

---

### 4. byte-diff-info

Details about byte-level differences.

```lisp
(defstruct byte-diff-info
  "Detailed byte-level difference information."
  (first-offset nil :type (or null integer))  ; Offset of first difference
  (total-diff-bytes 0 :type integer)          ; Total differing bytes
  (size-mismatch-p nil :type boolean)         ; Files have different sizes
  (size1 0 :type integer)                     ; Stage 1 size
  (size2 0 :type integer))                    ; Stage 2 size
```

**Validation Rules**:
- If `size-mismatch-p` is T, `first-offset` should be NIL
- If `total-diff-bytes` is 0, `first-offset` should be NIL

---

### 5. verification-history-entry

Single entry in the verification history log.

```lisp
(defstruct verification-history-entry
  "Entry in the verification history log."
  (timestamp nil :type string)                ; ISO 8601
  (status nil :type fixpoint-status)
  (diff-bytes 0 :type integer)
  (compilation-rate 0.0 :type float))
```

**Storage**: Appended to `dist/verification-history.jsonl` as JSON Lines.

---

## State Transitions

### Verification Run States

```
                    ┌─────────────────┐
                    │     START       │
                    └────────┬────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │ Check Deps      │──────────────┐
                    │ (wasmtime,      │              │
                    │  wasm-tools,    │              │ Missing
                    │  Stage 1)       │              │
                    └────────┬────────┘              │
                             │ Found                 │
                             ▼                       ▼
                    ┌─────────────────┐     ┌───────────────┐
                    │ Validate        │     │ MISSING_DEP   │
                    │ Stage 1         │     │ (exit 3)      │
                    └────────┬────────┘     └───────────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │ Generate        │──────────────┐
                    │ Stage 2         │              │
                    └────────┬────────┘              │ Error
                             │ Success              │
                             ▼                       ▼
                    ┌─────────────────┐     ┌───────────────┐
                    │ Compare         │     │ COMPILE_ERR   │
                    │ Binaries        │     │ (exit 2)      │
                    └────────┬────────┘     └───────────────┘
                             │
             ┌───────────────┴───────────────┐
             │ Identical                     │ Different
             ▼                               ▼
    ┌─────────────────┐             ┌─────────────────┐
    │    ACHIEVED     │             │  NOT_ACHIEVED   │
    │    (exit 0)     │             │    (exit 1)     │
    └─────────────────┘             └─────────────────┘
```

---

## Data Persistence

| Entity | Storage Location | Format |
|--------|------------------|--------|
| Stage 1 binary | `dist/clysm-stage1.wasm` | Wasm binary |
| Stage 2 binary | `dist/clysm-stage2.wasm` | Wasm binary |
| Verification result | stdout | JSON |
| History log | `dist/verification-history.jsonl` | JSON Lines |
| Diff report | stdout or file | JSON or text |

---

## Relationships Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                   verification-result                        │
├─────────────────────────────────────────────────────────────┤
│ status: fixpoint-status                                      │
│ timestamp: string                                            │
│ identical-p: boolean                                         │
│ compilation-rate: float                                      │
│ ...                                                          │
├──────────────────┬─────────────────┬────────────────────────┤
│     stage1-info  │   stage2-info   │      byte-diff-info    │
│   (binary-info)  │  (binary-info)  │                        │
├──────────────────┼─────────────────┼────────────────────────┤
│ path             │ path            │ first-offset           │
│ size-bytes       │ size-bytes      │ total-diff-bytes       │
│ exports          │ exports         │ size-mismatch-p        │
│ valid-p          │ valid-p         │                        │
└──────────────────┴─────────────────┴────────────────────────┘
                              │
                              │ appended as
                              ▼
                ┌─────────────────────────┐
                │ verification-history-   │
                │ entry                   │
                ├─────────────────────────┤
                │ timestamp               │
                │ status                  │
                │ diff-bytes              │
                │ compilation-rate        │
                └─────────────────────────┘
```
