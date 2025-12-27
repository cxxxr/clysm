# Data Model: FORMAT Function Foundation

**Feature**: 032-format-function
**Date**: 2025-12-27

## Entity Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Format Execution                              │
├─────────────────────────────────────────────────────────────────────┤
│  Control String ──parse──> Directive Tree ──execute──> Output       │
│                                                                      │
│  "~A: ~{~A~^, ~}"  →  [aesthetic-dir, iteration-dir] → "X: a, b, c" │
└─────────────────────────────────────────────────────────────────────┘
```

## Core Entities

### format-directive (base struct)

The abstract base for all directive types.

| Field | Type | Description |
|-------|------|-------------|
| type | keyword | Directive identifier (:aesthetic, :iteration, etc.) |
| start | fixnum | Start position in control string |
| end | fixnum | End position in control string |

**Validation Rules**:
- start < end
- start >= 0
- end <= (length control-string)

---

### simple-directive

Directives that consume 0-1 arguments without nesting.

| Directive | Keyword | Args Consumed | Description |
|-----------|---------|---------------|-------------|
| ~A | :aesthetic | 1 | Print object human-readable |
| ~S | :standard | 1 | Print object machine-readable |
| ~D | :decimal | 1 | Print integer in decimal |
| ~% | :newline | 0 | Output newline |
| ~& | :fresh-line | 0 | Output newline if not at column 0 |
| ~~ | :tilde | 0 | Output literal tilde |

---

### iteration-directive

Directive for ~{body~} loops.

| Field | Type | Description |
|-------|------|-------------|
| type | keyword | Always :iteration |
| start | fixnum | Position of ~{ |
| end | fixnum | Position after ~} |
| body-directives | list | Parsed directives within body |
| body-string | string | Raw substring for error messages |

**Execution Semantics**:
1. Consume one argument (must be list)
2. For each element in list:
   - Bind element as single argument
   - Execute body-directives
3. ~^ exits iteration when list exhausted

**State Transitions**:
```
PARSING: Found ~{ → scan for matching ~} → parse body recursively → return iteration-directive
```

---

### escape-directive

Directive for ~^ (escape from iteration).

| Field | Type | Description |
|-------|------|-------------|
| type | keyword | Always :escape |
| start | fixnum | Position of ~^ |
| end | fixnum | Position after ~^ |

**Execution Semantics**:
- Only valid inside ~{~} body
- Terminates current iteration if no more elements remain
- Typically used for separators: `~{~A~^, ~}`

---

### conditional-directive

Directive for ~[clause~;clause~;...~] selection.

| Field | Type | Description |
|-------|------|-------------|
| type | keyword | Always :conditional |
| start | fixnum | Position of ~[ |
| end | fixnum | Position after ~] |
| clauses | list | List of clause-info structures |
| boolean-p | boolean | True if ~:[ form |
| default-index | fixnum or nil | Index of ~:; clause |

---

### clause-info

Single clause within conditional directive.

| Field | Type | Description |
|-------|------|-------------|
| index | fixnum | Clause position (0-based) |
| start | fixnum | Start position in control string |
| end | fixnum | End position in control string |
| directives | list | Parsed directives within clause |
| default-p | boolean | True if this is ~:; default clause |

**Execution Semantics**:
1. Consume one argument
2. If boolean-p: index = (if arg 1 0)
3. Else: index = arg (must be integer)
4. Select clause by index, or default if out of range
5. Execute selected clause directives

---

### recursive-directive

Directive for ~? (recursive format).

| Field | Type | Description |
|-------|------|-------------|
| type | keyword | Always :recursive |
| start | fixnum | Position of ~? |
| end | fixnum | Position after ~? |

**Execution Semantics**:
1. Consume two arguments:
   - First: format control string
   - Second: argument list for that string
2. Call FORMAT recursively with those arguments
3. Output is written to current destination

---

### format-string-info

Top-level parsed format string.

| Field | Type | Description |
|-------|------|-------------|
| control-string | string | Original format string |
| directives | list | Ordered list of parsed directives |

---

### format-error (condition)

Error condition for format-related failures.

| Slot | Type | Description |
|------|------|-------------|
| control-string | string | The problematic format string |
| position | fixnum | Position where error occurred |
| format-control | string | Error message template |
| format-arguments | list | Arguments for error message |

**Hierarchy**: format-error < simple-error < error < condition

**Signaled When**:
- Malformed format string (unclosed ~{, unmatched ~])
- Insufficient arguments for directives
- Type mismatch (non-integer for ~D, non-list for ~{)

---

## Relationships

```
format-string-info
    │
    └── directives: list of
            ├── simple-directive
            ├── iteration-directive
            │       └── body-directives: list of (any directive type)
            ├── escape-directive
            ├── conditional-directive
            │       └── clauses: list of clause-info
            │               └── directives: list of (any directive type)
            └── recursive-directive
```

## State: Stream Column Tracking

For ~& (fresh-line) to work correctly, streams must track output column.

| State | Condition | Next State After |
|-------|-----------|------------------|
| column = 0 | At start or after newline | write-char non-newline → column > 0 |
| column > 0 | After any non-newline output | write-char newline → column = 0 |

**Implementation Note**: For nil destination (string return), track column in local variable during format execution.

---

## Invariants

1. **Directive Ordering**: Directives in list are ordered by start position
2. **Non-Overlapping**: No two directives have overlapping (start, end) ranges
3. **Complete Coverage**: Literal text fills gaps between directive ranges
4. **Balanced Nesting**: Every ~{ has matching ~}, every ~[ has matching ~]
5. **Escape Context**: ~^ only valid within ~{~} body
6. **Clause Separation**: ~; only valid within ~[~] body
