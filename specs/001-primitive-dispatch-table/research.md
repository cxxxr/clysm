# Research: Primitive Dispatch Table

**Branch**: `001-primitive-dispatch-table`
**Date**: 2026-01-03

## Current Implementation Analysis

### Location
- **File**: `src/clysm/compiler/codegen/func-section.lisp`
- **Function**: `compile-primitive-call` (lines 1148-1505)

### Structure
1. **String-based cond block** (lines 1156-1181): 18 entries
   - `%SETF-AREF`, `%SETF-SVREF`, `%SETF-SCHAR`, `%SETF-ELT`, `%SETF-GETF`
   - `%SETF-ROW-MAJOR-AREF`
   - `COPY-SEQ`, `MAKE-INSTANCE*`, `SLOT-VALUE*`, `SET-SLOT-VALUE*`
   - `STANDARD-INSTANCE-P`, `INSTANCE-CLASS`, `CLASS-NAME`
   - `REGISTER-STRUCTURE-CLASS`, `DEFINE-CLASS*`, `REGISTER-SETF-EXPANDER`
   - `MAKE-SLOT-ACCESSOR-SETF-EXPANDER`

2. **Symbol-based case statement** (lines 1184-1505): 248 entries
   - Arithmetic: `+`, `-`, `*`, `/`, `truncate`, `mod`, `rem`, `floor`, `ceiling`, `round`
   - Comparison: `<`, `>`, `<=`, `>=`, `=`, `/=`
   - List operations: `cons`, `car`, `cdr`, `list`, `append`, `reverse`, `nth`
   - Type predicates: `consp`, `null`, `atom`, `integerp`, `floatp`, `symbolp`
   - String functions: `string=`, `string<`, `char`, `make-string`
   - Array operations: `aref`, `svref`, `elt`, `array-rank`
   - Higher-order: `mapcar`, `reduce`, `every`, `some`

### Existing Pattern: *runtime-function-table*
A similar pattern already exists for runtime library dispatch:
```lisp
(defparameter *runtime-function-table* (make-hash-table :test 'eq))

(defun register-runtime-function (symbol runtime-name &optional arity)
  (setf (gethash symbol *runtime-function-table*)
        (cons runtime-name arity)))

(defun runtime-function-p (symbol)
  (gethash symbol *runtime-function-table*))
```

This establishes precedent for hash-table based dispatch in the codebase.

## Design Decision: Two-Table vs Single-Table

### Decision: Two Separate Hash Tables

**Rationale**:
1. **Performance**: `eq` test is faster than `equal` for symbol lookup
2. **Semantics**: Symbol lookup (package-aware) differs from string lookup (name-only)
3. **Clarity**: Explicit separation of lookup strategies

**Alternatives Considered**:

| Approach | Pros | Cons |
|----------|------|------|
| Single table with mixed keys | Simpler code | Performance hit for symbol lookups |
| String-only table | Uniform access | Loses symbol identity, slower |
| Symbol-only with package prefixes | Fast lookup | Complex key construction |

## Design Decision: Registration Entry Structure

### Decision: Use defstruct

```lisp
(defstruct primitive-entry
  compiler-fn    ; (function (op args env) list)
  arity          ; (or fixnum null)
  flags)         ; plist for future extensions
```

**Rationale**:
1. **Type safety**: Structure provides clear type expectations
2. **Extensibility**: `flags` plist allows future additions without breaking changes
3. **Consistency**: Matches existing Clysm patterns (e.g., compilation-env)

**Alternatives Considered**:

| Approach | Pros | Cons |
|----------|------|------|
| Cons cell (fn . arity) | Simple, matches *runtime-function-table* | No room for flags |
| Plist | Maximum flexibility | No type structure |
| CLOS class | Full OO benefits | Overkill for data holder |

## Design Decision: API Design

### Decision: Single registration function with keyword arguments

```lisp
(register-primitive-compiler symbol compiler-fn &key arity flags string-name)
```

**Rationale**:
1. **Simplicity**: Single entry point for all registration needs
2. **Optional string-name**: Automatically registers in both tables when provided
3. **Familiar pattern**: Similar to existing `register-runtime-function`

### Query API

```lisp
(primitive-compiler-entry symbol)      ; Returns entry or NIL
(list-registered-primitives)           ; Returns list of registered symbols
(primitive-registered-p symbol)        ; Boolean check
```

## Design Decision: Migration Strategy

### Decision: Incremental Category-by-Category

**Phase 1**: Infrastructure
- Create dispatch tables and registration API
- Add parallel dispatch check in `compile-primitive-call`

**Phase 2**: Migrate by category (order chosen for risk/dependency management)
1. Arithmetic operators (lowest risk, well-tested)
2. Comparison operators
3. List operations
4. Type predicates
5. String/character functions
6. Array operations
7. Higher-order functions
8. String-matched primitives (cond block)

**Phase 3**: Cleanup
- Remove empty case statement
- Remove cond block
- Update documentation

**Rationale**:
1. **Reduced risk**: Each category can be verified independently
2. **Rollback capability**: Easy to revert individual categories
3. **Parallel testing**: Old and new paths can run simultaneously during migration

## Best Practices Applied

### Common Lisp Hash Table Optimization
- Use `:test 'eq` for symbol tables (fastest)
- Use `:test 'equal` for string tables (required for string comparison)
- Pre-size tables: `:size 300` to avoid rehashing

### Thread Safety Considerations
- SBCL hash tables are not thread-safe by default
- For single-threaded compilation, this is acceptable
- If parallelization needed later, use `sb-ext:with-locked-hash-table`

### Debugging Support
- Include primitive name in any error messages
- Provide `describe-primitive` function for REPL inspection
- Log registration during development (controlled by `*debug-primitive-dispatch*`)

## Performance Considerations

### Expected Performance Profile
- **Case statement**: O(log n) average due to compiler optimization, O(n) worst case
- **Hash table**: O(1) average, amortized constant time
- **Benefit**: More consistent performance, especially as primitive count grows

### Benchmark Strategy
1. Compile standard library with both implementations
2. Measure wall-clock time for 1000 iterations
3. Target: < 5% regression (expect improvement)

## References

- [CLHS: make-hash-table](resources/HyperSpec/Body/f_mk_has.htm)
- [CLHS: gethash](resources/HyperSpec/Body/f_gethas.htm)
- [CLHS: defstruct](resources/HyperSpec/Body/m_defstr.htm)
- Existing pattern: `*runtime-function-table*` in func-section.lisp:69-106
