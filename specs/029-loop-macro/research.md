# Research: LOOP Macro Implementation

**Feature**: 029-loop-macro
**Date**: 2025-12-27

## Research Areas

### 1. Existing Macro Expansion Pattern

**Decision**: Follow do/dolist/dotimes tagbody/go pattern in `src/clysm/lib/macros.lisp`

**Rationale**: The existing iteration macros provide a proven pattern:
- `dolist` (lines 120-141): List iteration with car/cdr stepping
- `dotimes` (lines 143-164): Numeric iteration with increment
- `do` (lines 232-265): Full parallel variable stepping with psetq

All use the same structure:
```lisp
(let ((vars...))
  (block nil
    (tagbody
      loop-start
      (if termination-test (go loop-end))
      body...
      stepping...
      (go loop-start)
      loop-end)
    result-form))
```

**Alternatives considered**:
- Recursive expansion: Rejected - would create deeply nested code
- Direct Wasm generation: Rejected - bypasses macro system, harder to debug
- Interpreter approach: Rejected - violates compile-time macro principle

### 2. Hash Table Iteration Strategy

**Decision**: Defer hash-table iteration to Phase 2; focus on core LOOP first

**Rationale**:
- Hash tables exist only at compile-time in current Clysm (used for registries)
- No WasmGC hash table type defined yet
- ANSI CL allows hash-table iteration to be implementation-defined for ordering
- Core LOOP (arithmetic, list, vector iteration) covers 90%+ of use cases

**Alternatives considered**:
- Implement WasmGC hash table first: Rejected - scope creep, separate feature
- Use association list fallback: Rejected - performance concerns
- Linear memory hash table: Rejected - violates WasmGC-First principle

**Follow-up**: Create separate feature issue for WasmGC hash table implementation

### 3. LOOP Clause Parser Architecture

**Decision**: Recursive descent parser with clause-type dispatching

**Rationale**: LOOP syntax is keyword-driven, making recursive descent natural:
```
loop-form := (loop clause*)
clause := for-clause | accumulation-clause | termination-clause | ...
for-clause := FOR var (IN list | ON list | FROM ... TO ... | ACROSS vector | ...)
```

Parser returns structured clause objects:
- `loop-clause-for-arithmetic`: var, from, to, by, direction
- `loop-clause-for-in`: var, list-form
- `loop-clause-collect`: expr, into-var
- etc.

**Implementation approach**:
1. `parse-loop-clauses`: Dispatches to specific parsers
2. `parse-for-clause`: Handles FOR/AS variations
3. `parse-accumulation-clause`: Handles COLLECT/SUM/COUNT/etc
4. `expand-loop-clauses`: Generates tagbody expansion from parsed clauses

**Alternatives considered**:
- State machine parser: Rejected - harder to extend for new clause types
- Macrolet-based expansion: Rejected - LOOP keywords aren't macros

### 4. Tagbody Strategy Selection

**Decision**: Use :simple-loop strategy for most LOOP forms; :dispatch for complex control flow

**Rationale**: From func-section.lisp (lines 4140-4341):
- **:sequential** (lines 4164-4174): No jumps - not applicable to LOOP
- **:simple-loop** (lines 4176-4213): Single backward jump - covers standard LOOP
- **:dispatch** (lines 4215-4301): Multiple tags, forward jumps - needed for LOOP-FINISH

Most LOOP forms only need:
- loop-start: Beginning of iteration
- loop-end: Normal termination

:simple-loop is optimal here. Only LOOP-FINISH (which jumps to epilogue) requires :dispatch.

**Alternatives considered**:
- Always use :dispatch: Rejected - unnecessary overhead for simple loops
- Separate LOOP-FINISH handling: Rejected - complicates code, inconsistent behavior

### 5. Accumulation Variable Management

**Decision**: Use gensyms for accumulator variables with proper initialization

**Rationale**: Each accumulation type needs specific initialization:

| Clause | Init Value | Update Operation |
|--------|------------|------------------|
| COLLECT | nil | (setq acc (nconc acc (list val))) or tail-pointer |
| SUM | 0 | (setq acc (+ acc val)) |
| COUNT | 0 | (setq acc (+ acc 1)) when val non-nil |
| MAXIMIZE | nil | (setq acc (if acc (max acc val) val)) |
| MINIMIZE | nil | (setq acc (if acc (min acc val) val)) |
| APPEND | nil | (setq acc (append acc val)) |
| NCONC | nil | (setq acc (nconc acc val)) |

For COLLECT efficiency, use tail-pointer technique:
```lisp
(let ((#:head (cons nil nil))
      (#:tail #:head))
  ...
  (setq #:tail (setf (cdr #:tail) (cons val nil)))
  ...
  (cdr #:head))
```

**Alternatives considered**:
- Always use append: Rejected - O(n^2) for COLLECT
- Reverse at end: Considered - simpler but slower for large collections

### 6. Parallel FOR Clause Stepping

**Decision**: Generate parallel stepping using psetq

**Rationale**: Multiple FOR clauses step in parallel per ANSI CL:
```lisp
(loop for x from 1 to 3
      for y in '(a b c)
      collect (list x y))
=> ((1 A) (2 B) (3 C))
```

All steppers advance simultaneously. Use psetq:
```lisp
(psetq x (+ x 1)
       y (cdr y))
```

Termination: Loop ends when ANY iterator exhausts.

**Alternatives considered**:
- Sequential stepping with setq: Rejected - violates ANSI CL semantics
- Separate step phase: Considered - more explicit but more code

### 7. Conditional Clause Compilation

**Decision**: Expand IF/WHEN/UNLESS to standard Lisp if/when/unless forms

**Rationale**: LOOP conditionals have scope over subsequent clauses until AND/ELSE/END:
```lisp
(loop for x in '(1 2 3 4 5)
      when (oddp x) collect x
      else collect (- x))
```

Expands to:
```lisp
(if (oddp x)
    (setq acc (nconc acc (list x)))
    (setq acc (nconc acc (list (- x)))))
```

AND chains multiple clauses in the same conditional branch.

**Alternatives considered**:
- Special conditional AST nodes: Rejected - reuse existing if/when/unless

### 8. Error Detection Strategy

**Decision**: Compile-time validation with descriptive error messages

**Rationale**: Per FR-021, conflicting accumulations must be detected:
```lisp
(loop for x in list collect x sum x)  ; ERROR: conflicting accumulations
```

Validation phases:
1. Parse-time: Syntax errors (malformed clauses)
2. Semantic check: Conflicting accumulations, invalid combinations
3. Type check: BY requires numeric, IN requires list

Use `error` with clear messages including source context.

**Alternatives considered**:
- Runtime checks: Rejected - too late, wastes compilation
- Warnings only: Rejected - ANSI CL requires errors for conflicts

## Dependencies Verified

| Dependency | Status | Location |
|------------|--------|----------|
| tagbody/go compilation | Available | func-section.lisp:4160-4341 |
| block/return-from | Available | func-section.lisp:3800-3950 |
| psetq | Available | macros.lisp:189-230 |
| car/cdr/cons | Available | func-section.lisp:658-698 |
| nconc/append | Available | func-section.lisp list functions |
| max/min | Available | numeric operations |
| length (for ACROSS) | Available | sequence functions |
| vector access (aref) | Available | setf-expanders.lisp:294-308 |
| hash-table iteration | NOT AVAILABLE | Deferred to future feature |

## Conclusion

All research areas resolved. LOOP implementation follows established patterns with no new infrastructure required except the LOOP macro expander itself. Hash-table iteration deferred to separate feature.
