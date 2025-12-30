# Research: I/O Print Primitives

**Feature**: 001-io-print-primitives
**Date**: 2025-12-31

## Summary

This research documents the technical decisions for implementing I/O print primitive compilation in the Clysm compiler. Key finding: Most infrastructure already exists; the primary work is adding primitive handlers to the compiler's function section generator.

## Research Items

### 1. FFI I/O Infrastructure

**Question**: How are FFI I/O functions currently implemented?

**Decision**: Reuse existing FFI declarations in `src/clysm/streams/ffi-io.lisp`

**Rationale**:
- `%host-write-char` and `%host-write-string` are already defined
- These use the `clysm:io` module namespace matching the host shim
- Marshal types (`:fixnum`, `:anyref`) are correctly specified
- No new FFI declarations needed for basic print operations

**Alternatives Considered**:
- Create new print-specific FFI functions → Rejected: would duplicate existing I/O primitives
- Use direct Wasm instructions → Rejected: requires FFI for host I/O access

**Source Evidence**:
```lisp
;; From src/clysm/streams/ffi-io.lisp
(clysm/ffi:define-foreign-function %host-write-char
    "clysm:io.write-char"
    (:fixnum :fixnum)
    :void)

(clysm/ffi:define-foreign-function %host-write-string
    "clysm:io.write-string"
    (:fixnum :anyref)
    :void)
```

---

### 2. Format Function Implementation

**Question**: How should [format](resources/HyperSpec/Body/f_format.htm) be compiled?

**Decision**: Compile format as a special form with compile-time directive parsing

**Rationale**:
- Format strings are typically constant at compile time
- Directive parsing already implemented in `src/clysm/streams/format.lisp`
- Can generate optimized code for each directive without runtime parsing overhead
- Falls back to runtime interpretation for non-constant format strings

**Alternatives Considered**:
- Always compile as runtime function call → Rejected: inefficient for constant strings
- Full compile-time string generation → Rejected: requires complex static analysis

**Directive Mapping**:

| Directive | ANSI Spec | Implementation |
|-----------|-----------|----------------|
| `~A` | [format ~A](resources/HyperSpec/Body/22_cca.htm) | Call `princ-to-string`, concatenate |
| `~S` | [format ~S](resources/HyperSpec/Body/22_ccb.htm) | Call `prin1-to-string`, concatenate |
| `~D` | [format ~D](resources/HyperSpec/Body/22_cba.htm) | Call `write-to-string` with base 10 |
| `~%` | [format ~%](resources/HyperSpec/Body/22_cea.htm) | Insert newline character (code 10) |
| `~&` | [format ~&](resources/HyperSpec/Body/22_ceb.htm) | Conditional newline (check column) |
| `~~` | [format ~~](resources/HyperSpec/Body/22_cfa.htm) | Literal tilde character |

---

### 3. Print Function Semantics

**Question**: What are the exact semantics of [print](resources/HyperSpec/Body/f_wr_pr.htm), [prin1](resources/HyperSpec/Body/f_wr_pr.htm), [princ](resources/HyperSpec/Body/f_wr_pr.htm)?

**Decision**: Follow ANSI CL semantics exactly

**Rationale**:
- `print`: newline, then object with escape, then space; returns object
- `prin1`: object with escape; returns object
- `princ`: object without escape; returns object
- `terpri`: outputs newline; returns NIL

**Implementation Pattern**:
```
print(object, stream):
  write-char(newline, stream)
  write(object, :escape t, :stream stream)
  write-char(space, stream)
  return object

prin1(object, stream):
  write(object, :escape t, :stream stream)
  return object

princ(object, stream):
  write(object, :escape nil, :stream stream)
  return object

terpri(stream):
  write-char(newline, stream)
  return NIL
```

---

### 4. Primitive Handler Integration

**Question**: Where should print primitive handlers be added in the compiler?

**Decision**: Add handlers in `src/clysm/compiler/codegen/func-section.lisp`

**Rationale**:
- Consistent with existing primitive handling pattern (line 725-846)
- Uses `compile-primitive-call` dispatch mechanism (line 915)
- Allows FFI call generation via existing infrastructure

**Implementation Location**:
1. Add function names to primitive member list (around line 846)
2. Add handler functions after existing primitive handlers
3. Use `clysm/ffi:lookup-foreign-function` to get FFI indices
4. Generate `call_import` instructions via `generate-import-call`

**Alternatives Considered**:
- Add as macros in `lib/macros.lisp` → Rejected: need compiler-level handling for FFI
- Add in separate file → Rejected: fragmentation; primitives should stay together

---

### 5. Stream Destination Handling

**Question**: How should stream destinations (nil, t, stream) be handled?

**Decision**: Compile-time dispatch for constant destinations, runtime for dynamic

**Rationale**:
- `nil` (string output) is most common in self-hosting code
- `t` (standard output) requires FFI call with fd=1
- Explicit stream requires runtime stream-fd extraction

**Destination Mapping**:

| Destination | Behavior |
|-------------|----------|
| `nil` | Accumulate to string via `with-output-to-string` pattern |
| `t` | Use `*standard-output*` (fd=1) |
| stream object | Extract fd from stream struct |

---

### 6. Write Function Keywords

**Question**: Which [write](resources/HyperSpec/Body/f_wr_pr.htm) keyword arguments should be supported?

**Decision**: Support minimal set for print primitives

**Rationale**:
- `:stream` - required for stream destination
- `:escape` - required for prin1/princ distinction
- Other keywords (`:base`, `:radix`, etc.) deferred to future enhancement

**Alternatives Considered**:
- Full ANSI write keyword support → Rejected: scope creep; not needed for compilation rate target
- No keyword support → Rejected: need `:escape` for correct semantics

---

### 7. Host Shim Compatibility

**Question**: Does the host shim need modifications?

**Decision**: No modifications needed

**Rationale**:
- `host-shim/io-shim.js` already exports `writeChar` and `writeString`
- Module namespace `clysm:io` matches FFI declarations
- File descriptor convention (0=stdin, 1=stdout, 2=stderr) already established

**Verification**:
```javascript
// From host-shim/io-shim.js
export function writeChar(fd, codepoint) { ... }
export function writeString(fd, str) { ... }
export function getImports() {
  return {
    'clysm:io': {
      'write-char': writeChar,
      'write-string': writeString,
      ...
    }
  };
}
```

---

### 8. Testing Strategy

**Question**: What testing approach ensures correctness?

**Decision**: Three-tier testing following TDD (Constitution VII)

**Rationale**:
1. **Unit tests**: Verify compilation handlers generate correct instruction sequences
2. **Contract tests**: Verify generated Wasm structure and imports are valid
3. **Integration tests**: Verify end-to-end compilation and execution

**Test Priority**:
1. `(format nil "~A" 42)` returns "42" - primary acceptance criterion
2. `(defun greet (x) (print x) x)` compiles successfully
3. Each print function compiles and validates
4. Each format directive works correctly

---

## Resolved Unknowns

All initial unknowns from the Technical Context have been resolved:

| Unknown | Resolution |
|---------|------------|
| FFI infrastructure | Reuse existing `%host-write-char`, `%host-write-string` |
| Format implementation | Compile-time directive parsing with existing implementation |
| Print semantics | Follow ANSI CL exactly via FFI write operations |
| Handler location | Add to `func-section.lisp` primitive handlers |
| Stream handling | Compile-time dispatch for constant destinations |
| Write keywords | Minimal set (`:stream`, `:escape`) |
| Host shim | No changes needed |
| Testing | Three-tier TDD approach |

## Dependencies

| Dependency | Status | Notes |
|------------|--------|-------|
| FFI infrastructure (027) | ✓ Available | `define-foreign-function`, marshalling |
| Format implementation | ✓ Available | `src/clysm/streams/format.lisp` |
| write-to-string | ✓ Available | `src/clysm/streams/format.lisp:353` |
| Host shim I/O | ✓ Available | `host-shim/io-shim.js` |
| Stream types | ✓ Available | `src/clysm/streams/types.lisp` |
