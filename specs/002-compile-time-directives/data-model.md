# Data Model: Compile-Time Directive Skip Integration

**Feature**: 002-compile-time-directives
**Date**: 2025-12-31

## Entities

### 1. Compilation Result

Represents the outcome of compiling a single form.

| Field | Type | Description |
|-------|------|-------------|
| form | source-form | The original form being compiled |
| form-id | string | Identifier (file:line format) |
| success-p | keyword | :compiled, :failed, or :skipped |
| bytes | (or null vector) | Wasm bytes if compiled, nil otherwise |
| error-message | (or null string) | Error details if failed |

**State Transitions**:
```
Form Submitted
     │
     ▼
┌─────────────────────┐
│ test-form-compilation│
└─────────────────────┘
     │
     ├──► nil bytes ──────► :skipped
     │
     ├──► error caught ───► :failed
     │
     └──► bytes valid ────► :compiled
```

**Validation Rules**:
- success-p MUST be one of :compiled, :failed, :skipped
- bytes MUST be non-nil when success-p is :compiled
- error-message SHOULD be set when success-p is :failed

---

### 2. Classification Statistics

Aggregated statistics from form classification.

| Field | Type | Description |
|-------|------|-------------|
| compiled | integer | Forms that produced valid Wasm |
| failed | integer | Forms that errored during compilation |
| skipped | integer | Directive forms returning nil |
| total | integer | Total forms processed |

**Invariant**: `compiled + failed + skipped = total`

**Derived Fields**:
- `coverage_pct = compiled / (total - skipped) * 100`

---

### 3. Stage 1 Report (JSON)

The report structure output to `dist/stage1-report.json`.

```json
{
  "timestamp": "ISO-8601 datetime",
  "stage0_version": "string",
  "summary": {
    "total_forms": "integer",
    "compiled": "integer",
    "failed": "integer",
    "skipped": "integer",
    "coverage_pct": "float",
    "top_blockers": [
      {
        "operator": "string",
        "count": "integer",
        "priority": "HIGH|MEDIUM|LOW"
      }
    ]
  },
  "modules": [
    {
      "path": "string",
      "compiled": "integer",
      "failed": "integer",
      "skipped": "integer",
      "total": "integer"
    }
  ]
}
```

**Changes from current format**:
- `skipped` field already exists (value: 48) - will increase with directive skips
- Per-module `skipped` field to be added
- `top_blockers` will no longer include DEFPACKAGE, IN-PACKAGE, DECLAIM

---

### 4. Directive Form

Predicate for identifying compile-time directives.

| Operator | Symbol Variants | HyperSpec |
|----------|-----------------|-----------|
| in-package | cl:in-package, in-package | [m_in_pkg.htm](resources/HyperSpec/Body/m_in_pkg.htm) |
| defpackage | cl:defpackage, defpackage | [m_defpkg.htm](resources/HyperSpec/Body/m_defpkg.htm) |
| declaim | cl:declaim, declaim | [m_declai.htm](resources/HyperSpec/Body/m_declai.htm) |
| proclaim | cl:proclaim, proclaim | [f_procla.htm](resources/HyperSpec/Body/f_procla.htm) |

**Detection Logic**:
```lisp
(and (consp form)
     (symbolp (car form))
     (member (car form) '(cl:in-package cl:defpackage cl:declaim cl:proclaim
                          in-package defpackage declaim proclaim)
             :test #'eq))
```

---

## Relationships

```
┌─────────────────┐       ┌──────────────────────┐
│  Source Form    │──1:1──│  Compilation Result  │
└─────────────────┘       └──────────────────────┘
                                    │
                                    │ aggregates
                                    ▼
                          ┌──────────────────────┐
                          │Classification Stats  │
                          └──────────────────────┘
                                    │
                                    │ serializes to
                                    ▼
                          ┌──────────────────────┐
                          │  Stage 1 Report JSON │
                          └──────────────────────┘
```

## Key Behaviors

1. **Form Classification**: Each form goes through `test-form-compilation` which determines its state
2. **Statistics Aggregation**: `classify-forms` collects all results and computes totals
3. **Report Generation**: `generate-progress-report` serializes stats to JSON
4. **Blocker Analysis**: `top_blockers` computed from failed forms only (excluding skipped)
