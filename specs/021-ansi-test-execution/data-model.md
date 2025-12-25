# Data Model: ANSI Test Execution

**Feature**: 021-ansi-test-execution
**Date**: 2025-12-25

## Overview

This feature extends the existing 020-ansi-test data model with minimal refinements. The core entities (TestCase, TestResult, CategoryResult, SkipRegistry) remain unchanged; this document describes enhancements to their usage.

## Existing Entities (from 020-ansi-test)

### TestCase

```lisp
(defstruct test-case
  "A single test from the ANSI test suite."
  (name nil :type symbol)
  (category nil :type string)
  (source-file nil :type (or null pathname))
  (form nil :type t)           ; The Lisp form to evaluate
  (expected-values nil :type list))  ; List of expected result values
```

**No changes required.**

### TestResult

```lisp
(defstruct test-result
  "Result of executing a single test."
  (test-case nil :type test-case)
  (status nil :type (member :pass :fail :skip))
  (skip-reason nil :type (or null string))
  (actual-values nil :type list)
  (error-message nil :type (or null string))
  (execution-time-ms 0 :type (unsigned-byte 32)))
```

**No changes required.** Skip reasons are already strings.

### SkipRegistry

```lisp
(defstruct skip-registry
  "Registry of forms and categories to skip."
  (unsupported-forms nil :type list)
  (unsupported-categories nil :type list)
  (skipped-tests nil :type list)
  (timeout-seconds 30 :type (unsigned-byte 16)))
```

**Enhancement**: Expand `unsupported-forms` list (implementation detail, not schema change).

## Refined Skip Reason Taxonomy

The `skip-reason` field in TestResult uses a hierarchical string format:

### Level 1: Category-based skip
```
unsupported-category: <category-name>
```
Example: `"unsupported-category: streams"`

### Level 2: Form-based skip
```
unsupported-form: <form-name>
```
Example: `"unsupported-form: loop"`

### Level 3: Compilation error
```
compile-error: <error-message>
```
Example: `"compile-error: Unknown function: EQT"`

### Level 4: Result verification
```
unverifiable: <reason>
```
Example: `"unverifiable: expected symbol A"`

### Level 5: Execution error
```
runtime-error: <error-message>
```
Example: `"runtime-error: Wasm execution timed out"`

### Level 6: Parse error
```
parse-error: <error-message>
```
Example: `"parse-error: Could not parse wasmtime output"`

## Expected Value Classification

For result comparison, expected values are classified:

| Expected Type | Can Verify? | Action |
|--------------|-------------|--------|
| Fixnum | Yes | Compare exact integer match |
| T | Yes | Compare against "true" or non-NIL |
| NIL | Yes | Compare against sentinel or empty |
| Symbol (other) | No | Mark as unverifiable |
| Cons cell | No | Mark as unverifiable |
| List | No | Mark as unverifiable |
| String | No | Mark as unverifiable |
| Multiple values | No | Mark as unverifiable |

## Enhanced Skip Forms List

The following forms trigger automatic skip detection:

```lisp
;; Control flow (021 additions)
loop for while do do* dotimes dolist
let let* flet labels block return-from
tagbody go catch throw

;; Binding constructs
multiple-value-bind multiple-value-list
multiple-value-call destructuring-bind

;; Macros
macrolet symbol-macrolet

;; Side effects
incf decf push pop pushnew
setf setq psetf psetq

;; Error handling
handler-case handler-bind restart-case
ignore-errors unwind-protect

;; ANSI test framework macros
signals-error signals-type-error classify-error
eqt equalt notnot notnot-mv
expand-in-current-env def-fold-test

;; ANSI test globals (special variables)
*universe* *numbers* *symbols* *characters*
*conses* *arrays* *hash-tables*

;; Existing I/O forms (from 020)
format print prin1 princ write pprint
open close with-open-file with-open-stream
read read-line read-char peek-char unread-char

;; Existing CLOS forms (from 020)
defgeneric defmethod defclass
make-instance slot-value slot-boundp
```

## Relationships

```
TestCase
    |
    +-- form --> analyzed for unsupported constructs
    |
    +-- expected-values --> classified as verifiable/unverifiable
    |
    v
TestResult
    |
    +-- status (PASS/FAIL/SKIP)
    |
    +-- skip-reason (hierarchical string)
    |
    +-- actual-values (for comparison)
```

## Constraints

1. **Skip reasons must be actionable**: Each skip reason should indicate what would need to be implemented for the test to run
2. **No false failures**: If a test cannot be verified, it must be SKIP not FAIL
3. **Preserve existing behavior**: All 020-ansi-test functionality must continue to work
