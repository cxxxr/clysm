# Research: Fix FFI Streams Module

**Feature**: 018-fix-ffi-streams
**Date**: 2025-12-24

## Investigation Summary

The streams module was disabled in `clysm.asd` at lines 115-125 with the comment "TEMPORARILY DISABLED: Pre-existing issues need separate fix". This research identifies the specific issues that caused the disable.

## Identified Issues

### Issue 1: Test Package Configuration Missing

**Location**: `tests/package.lisp`

**Problem**: The unit tests for streams (`stream-types-test.lisp`, `stream-write-test.lisp`, etc.) are in package `#:clysm/tests` but access symbols like `clysm/streams:write-char` and `clysm/conditions:type-error` without proper imports.

**Evidence**:
- `tests/unit/stream-write-test.lisp:20` uses `clysm/conditions:type-error` without import
- `tests/unit/stream-write-test.lisp:21` uses `clysm/streams:write-char` without import
- `tests/package.lisp` has no package definitions for stream tests

**Solution Options**:

| Option | Description | Recommendation |
|--------|-------------|----------------|
| A | Add stream imports to `#:clysm/tests` package | **Recommended** - minimal change |
| B | Create separate `clysm/tests/unit/stream-*` packages | Consistent with other unit tests |
| C | Move tests to use `clysm/tests/streams` package | Breaks convention |

**Decision**: Option B - Create proper package definitions for each stream unit test file, following the existing pattern for other unit tests.

**Rationale**: Other unit tests have their own packages (e.g., `clysm/tests/unit/ffi-types`). Stream tests should follow this convention.

### Issue 2: Stream Test Packages Not Loaded

**Location**: `clysm.asd` lines 193-210

**Problem**: When streams module is commented out, the test packages are also commented out. But even when enabled, the `tests/streams/package.lisp` which defines `clysm/tests/streams` is only loaded if the streams module itself is loaded.

**Evidence**:
- `tests/streams/package.lisp` defines `clysm/tests/streams` with proper imports
- The module is in a separate `:module "streams"` block in the test system
- Unit tests in `tests/unit/` use wrong package (`clysm/tests`)

**Solution**:
1. Add stream-related package definitions to main `tests/package.lisp`
2. Update unit tests to use correct packages
3. Keep integration tests in `tests/streams/` as-is

### Issue 3: Potential Load Order Issue

**Location**: `src/clysm/streams/package.lisp`

**Problem**: The streams package imports from `clysm/ffi` and uses `clysm/conditions` types. These must be fully loaded before streams.

**Evidence**:
```lisp
(:import-from #:clysm/ffi
              #:define-foreign-function)
```

**Status**: ASDF load order looks correct (ffi and conditions are defined before streams in clysm.asd). This is likely NOT the root cause.

### Issue 4: Unit Tests Reference Non-existent Package Symbols

**Location**: `tests/unit/stream-types-test.lisp:13`

**Problem**: References `clysm/compiler/codegen/gc-types:+type-stream+` which may not be defined yet when streams module is disabled.

**Evidence**:
```lisp
(ok (= clysm/compiler/codegen/gc-types:+type-stream+ 19))
```

**Solution**: Verify `+type-stream+` is defined in `gc-types.lisp` and exported.

## Required Actions

### Action 1: Add Stream Test Package Definitions

Add to `tests/package.lisp`:

```lisp
;; Stream unit test packages (015-ffi-stream-io)
(defpackage #:clysm/tests/unit/stream-types
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/stream-write
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/stream-read
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/stream-format
  (:use #:cl #:rove))
```

### Action 2: Update Unit Test Package Declarations

Update each stream unit test file to use its correct package:
- `stream-types-test.lisp`: Change to `(in-package #:clysm/tests/unit/stream-types)`
- `stream-write-test.lisp`: Change to `(in-package #:clysm/tests/unit/stream-write)`
- `stream-read-test.lisp`: Change to `(in-package #:clysm/tests/unit/stream-read)`
- `stream-format-test.lisp`: Change to `(in-package #:clysm/tests/unit/stream-format)`

### Action 3: Verify gc-types Exports

Ensure `src/clysm/compiler/codegen/gc-types.lisp` exports:
- `+type-stream+`
- `make-stream-type`
- Related struct accessors

### Action 4: Enable Modules in clysm.asd

1. Uncomment streams module (lines 115-125)
2. Uncomment stream unit tests (lines 193-197)
3. Uncomment streams test module (lines 205-210)

## Verification Plan

1. Enable streams module in clysm.asd
2. Attempt to load system: `(asdf:load-system :clysm)`
3. If load fails, fix specific errors
4. Enable stream tests
5. Run full test suite: `(asdf:test-system :clysm)`
6. Fix any failing tests
7. Verify ANSI CL compliance with format examples

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Package conflicts | Low | Medium | Test iteratively |
| Missing gc-types export | Medium | Low | Add export if missing |
| FFI macro issues | Low | High | Test FFI in isolation |
| Test failures | Medium | Medium | Fix one test at a time |

## Alternatives Considered

### Alternative: Rewrite Streams Module
**Rejected**: The implementation is complete and follows spec. Only test configuration needs fixing.

### Alternative: Merge Streams into Main Package
**Rejected**: Violates separation of concerns. Streams should remain a separate module.

## Conclusion

The "pre-existing issues" are primarily **test package configuration problems**, not implementation bugs. The streams module implementation appears correct. The fix requires:
1. Adding proper package definitions for unit tests
2. Updating test files to use correct packages
3. Uncommenting the module and tests
4. Running tests and fixing any remaining issues

Estimated effort: Low (mostly configuration changes, minimal code changes).
