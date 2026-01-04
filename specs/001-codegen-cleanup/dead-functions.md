# Dead Functions Documentation

**Feature**: 001-codegen-cleanup
**Updated**: 2026-01-04
**Status**: Phase 3 Detection Complete

## Overview

This document lists all functions registered in `*runtime-function-table*` and identifies the corresponding `compile-*` functions in `func-section.lisp` that are candidates for dead code removal.

## IMPORTANT DISCOVERY

**I/O, List, and Sequence compile-* functions have ALREADY been removed** in previous feature migrations (001-io-list-runtime, 001-sequence-runtime-migration, etc.). Only String and Numeric functions remain.

## Runtime Function Registrations

All functions below are registered via `register-runtime-function` in `src/clysm/compiler/codegen/func-section.lisp` (lines 112-310).

### I/O Functions (6 functions) - ✅ ALREADY REMOVED
Registration: `register-io-runtime-functions` (line 112)
Source: `src/clysm/lib/io-runtime.lisp`

| Function | Wasm Symbol | Status |
|----------|-------------|--------|
| princ | :$princ-rt | ✅ compile-princ already removed |
| prin1 | :$prin1-rt | ✅ compile-prin1 already removed |
| print | :$print-rt | ✅ compile-print already removed |
| write | :$write-rt | ✅ compile-write already removed |
| terpri | :$terpri-rt | ✅ compile-terpri already removed |
| format | :$format-rt | ✅ compile-format already removed |

### List Functions (13 functions) - ✅ ALREADY REMOVED
Registration: `register-list-runtime-functions` (line 126)
Source: `src/clysm/lib/list-runtime.lisp`

| Function | Wasm Symbol | Status |
|----------|-------------|--------|
| member | :$member-rt | ✅ compile-member already removed |
| member-if | :$member-if-rt | ✅ already removed |
| member-if-not | :$member-if-not-rt | ✅ already removed |
| assoc | :$assoc-rt | ✅ already removed |
| assoc-if | :$assoc-if-rt | ✅ already removed |
| rassoc | :$rassoc-rt | ✅ already removed |
| rassoc-if | :$rassoc-if-rt | ✅ already removed |
| find | :$find-rt | ✅ already removed |
| find-if | :$find-if-rt | ✅ already removed |
| find-if-not | :$find-if-not-rt | ✅ already removed |
| position | :$position-rt | ✅ already removed |
| position-if | :$position-if-rt | ✅ already removed |
| position-if-not | :$position-if-not-rt | ✅ already removed |

### Sequence Functions (15 functions) - ✅ ALREADY REMOVED
Registration: `register-sequence-runtime-functions` (line 149)
Source: `src/clysm/lib/sequence-runtime.lisp`

| Function | Wasm Symbol | Status |
|----------|-------------|--------|
| remove | :$remove-rt | ✅ already removed |
| remove-if | :$remove-if-rt | ✅ already removed |
| remove-if-not | :$remove-if-not-rt | ✅ already removed |
| count | :$count-rt | ✅ already removed |
| count-if | :$count-if-rt | ✅ already removed |
| count-if-not | :$count-if-not-rt | ✅ already removed |
| substitute | :$substitute-rt | ✅ already removed |
| substitute-if | :$substitute-if-rt | ✅ already removed |
| substitute-if-not | :$substitute-if-not-rt | ✅ already removed |
| delete | :$delete-rt | ✅ already removed |
| delete-if | :$delete-if-rt | ✅ already removed |
| delete-if-not | :$delete-if-not-rt | ✅ already removed |
| subseq | :$subseq-rt | ✅ already removed |
| copy-seq | :$copy-seq-rt | ✅ already removed |
| adjust-array | :$adjust-array-rt | ✅ already removed |

### String Functions (12 functions) - 🔴 TO REMOVE
Registration: `register-string-runtime-functions` (line 246)
Source: `src/clysm/lib/string-runtime.lisp`

| Function | Wasm Symbol | Dead compile-* | Line |
|----------|-------------|----------------|------|
| char/schar | :$string-char-rt | compile-string-char | 12858 |
| string-trim | :$string-trim-rt | compile-string-trim | 14485 |
| string-left-trim | :$string-left-trim-rt | compile-string-left-trim | 14263 |
| string-right-trim | :$string-right-trim-rt | compile-string-right-trim | 14373 |
| string-capitalize | :$string-capitalize-rt | compile-string-capitalize | 14035 |
| nstring-capitalize | :$nstring-capitalize-rt | compile-nstring-capitalize | 14764 |
| string-equal | :$string-equal-rt | compile-string-equal | 13270 |
| string-not-equal | :$string-not-equal-rt | compile-string-not-equal | 13310 |
| string-lessp | :$string-lessp-rt | compile-string-lessp | 13278 |
| string-greaterp | :$string-greaterp-rt | compile-string-greaterp | 13286 |
| string-not-lessp | :$string-not-lessp-rt | compile-string-not-lessp | 13294 |
| string-not-greaterp | :$string-not-greaterp-rt | compile-string-not-greaterp | 13302 |

**Note**: string-upcase, string-downcase, nstring-upcase, nstring-downcase are NOT registered in runtime - keep those compile-* functions.

### Numeric Functions (5 functions) - 🔴 TO REMOVE
Registration: `register-numeric-runtime-functions` (line 272)
Source: `src/clysm/lib/numeric-runtime.lisp`

| Function | Wasm Symbol | Dead compile-* | Line |
|----------|-------------|----------------|------|
| parse-integer | :$parse-integer-rt | compile-parse-integer | 7712 |
| write-to-string | :$write-to-string-rt | compile-write-to-string | 7498 |
| rationalize | :$rationalize-rt | compile-rationalize | 7306 |
| signum | :$signum-rt | compile-signum | 5662 |
| phase | :$phase-rt | compile-phase | 6720 |

### Package Functions (4 functions) - NOT dead code candidates
Registration: `register-package-runtime-functions` (line 182)
Note: Internal compiler functions, not user-facing - keep in func-section.lisp

### Lexenv Functions (3 functions) - NOT dead code candidates
Registration: `register-lexenv-runtime-functions` (line 199)
Note: Internal compiler functions for lexical environment - keep in func-section.lisp

### AST Functions (8 functions) - NOT dead code candidates
Registration: `register-ast-runtime-functions` (line 213)
Note: Core AST manipulation functions - keep in func-section.lisp

### Parser Functions (3 functions) - NOT dead code candidates
Registration: `register-parser-runtime-functions` (line 229)
Note: Parser infrastructure - keep in func-section.lisp

### Backend Functions (1 function) - NOT dead code candidates
Registration: `register-backend-runtime-functions` (line 240)
Note: Wasm emission infrastructure - keep in func-section.lisp

---

## Summary: Dead Code Candidates

**Status Update**: 34 of 53 functions ALREADY removed in previous migrations.

| Category | Count | Status | Estimated Lines |
|----------|-------|--------|-----------------|
| I/O | 6 | ✅ Already removed | 0 |
| List | 13 | ✅ Already removed | 0 |
| Sequence | 15 | ✅ Already removed | 0 |
| String | 12 | 🔴 To remove | ~1,800 |
| Numeric | 5 | 🔴 To remove | ~700 |
| **Total Remaining** | **17** | **To remove** | **~2,500** |

---

## Transitive Dead Helpers

### String Function Helpers (lines 13338-13732)
These helpers are used ONLY by the dead string comparison functions:

| Helper | Line | Called by |
|--------|------|-----------|
| compile-string-compare-ci | 13338 | string-equal, string-lessp, etc. |
| compile-string-compare-ci-at-end-equal | 13463 | compile-string-compare-ci |
| compile-string-compare-ci-at-end-prefix | 13479 | compile-string-compare-ci |
| compile-string-compare-ci-at-diff | 13509 | compile-string-compare-ci |

**Note**: compile-string-compare (13551) and related helpers may still be used by string=, string<, etc. Verify before removal.

### Numeric Function Helpers
- (None identified - numeric functions are standalone)

---

## Revised Batch Plan

Given that I/O, List, and Sequence batches are already complete:

### Batch 1: String Functions (12 + 4 helpers ≈ 1,800 lines)
1. Remove compile-string-char (line 12858)
2. Remove case-insensitive comparison functions (lines 13270-13310)
3. Remove transitive helper compile-string-compare-ci and children (lines 13338-13509)
4. Remove compile-string-capitalize (line 14035)
5. Remove compile-string-*-trim (lines 14263-14485)
6. Remove compile-nstring-capitalize (line 14764)

### Batch 2: Numeric Functions (5 functions ≈ 700 lines)
1. Remove compile-signum (line 5662)
2. Remove compile-phase (line 6720)
3. Remove compile-rationalize (line 7306)
4. Remove compile-write-to-string (line 7498)
5. Remove compile-parse-integer (line 7712)

### Estimated Impact
- Current: 15,973 lines
- After String batch: ~14,170 lines
- After Numeric batch: ~13,470 lines
- Still need ~5,470 additional lines removed to reach <8,000 target

---

## Dead Case Branches in compile-primitive-call

These case branches dispatch to dead functions and should be removed:

### String Function Branches (Batch 1)
| Branch | Line | Dispatches to |
|--------|------|---------------|
| char | 1431 | compile-string-char |
| schar | 1432 | compile-string-char |
| string-equal | 1441 | compile-string-equal |
| string-lessp | 1442 | compile-string-lessp |
| string-greaterp | 1443 | compile-string-greaterp |
| string-not-lessp | 1444 | compile-string-not-lessp |
| string-not-greaterp | 1445 | compile-string-not-greaterp |
| string-not-equal | 1446 | compile-string-not-equal |
| string-capitalize | 1452 | compile-string-capitalize |
| string-trim | 1454 | compile-string-trim |
| string-left-trim | 1455 | compile-string-left-trim |
| string-right-trim | 1456 | compile-string-right-trim |
| nstring-capitalize | 1460 | compile-nstring-capitalize |

### Numeric Function Branches (Batch 2)
| Branch | Line | Dispatches to |
|--------|------|---------------|
| signum | 1332 | compile-signum |
| write-to-string | 1465 | compile-write-to-string |
| phase | 1474 | compile-phase |
| rationalize | 1497 | compile-rationalize |
| parse-integer | 1498 | compile-parse-integer |

---

## Dead Primitive Registry Entries

These entries in `primitive-registry.lisp` should also be removed:

### String Entries
- Line 289: `(defprimitive char ...)`
- Line 290: `(defprimitive schar ...)`
- Lines 297-302: string-equal, string-lessp, string-greaterp, string-not-lessp, string-not-greaterp, string-not-equal
- Lines 307-310: string-capitalize, string-trim, string-left-trim, string-right-trim
- Line 313: `(defprimitive nstring-capitalize ...)`

### Numeric Entries
- Line 185: `(defprimitive signum ...)`
- Line 314: `(defprimitive write-to-string ...)`
- Line 329: `(defprimitive phase ...)`
- Line 372: `(defprimitive rationalize ...)`
- Line 373: `(defprimitive parse-integer ...)`
