# Research: LOOP Macro Extension

**Feature**: 001-loop-extension
**Date**: 2025-12-30

## Research Topics

### 1. Hash-Table Iteration Implementation

**Decision**: Use `maphash`-based expansion with accumulated pairs list

**Rationale**:
- ANSI CL's `with-hash-table-iterator` requires first-class multiple values, which Clysm supports
- However, implementing a generator-style iterator adds complexity
- `maphash` is semantically equivalent and already implemented in Clysm
- The expansion collects entries upfront, then iterates over the collected list

**Alternatives Considered**:
1. **Direct with-hash-table-iterator**: More ANSI-compliant but requires implementing the iterator macro first
2. **Native WasmGC hash iteration**: Would require runtime support changes
3. **maphash with callback**: Would break LOOP's tagbody/go control flow

**Implementation Approach**:

```lisp
;; Input:
(loop for k being the hash-keys of ht using (hash-value v)
      collect (cons k v))

;; Expanded to:
(let* ((#:entries (let ((#:acc nil))
                    (maphash (lambda (k v) (push (cons k v) #:acc)) ht)
                    (nreverse #:acc)))
       (#:iter #:entries)
       (k nil)
       (v nil)
       (#:result nil))
  (block nil
    (tagbody
     loop-start
       (when (null #:iter) (go loop-end))
       (setq k (caar #:iter))
       (setq v (cdar #:iter))
       ;; body
       (setq #:result (nconc #:result (list (cons k v))))
       ;; step
       (setq #:iter (cdr #:iter))
       (go loop-start)
     loop-end)
    #:result))
```

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm), [maphash](resources/HyperSpec/Body/f_maphas.htm)

---

### 2. WITH Clause Binding Semantics

**Decision**: Sequential bindings (let* semantics) by default; parallel (let semantics) when `and` is used

**Rationale**:
- ANSI CL specifies that `with` without `and` creates sequential bindings
- The parser already handles `and` correctly in `parse-with-clause`
- Current implementation appends to `with-bindings` list correctly

**Verification Points**:
- `with a = 1 with b = (1+ a)` → sequential (b sees a's value)
- `with a = 1 and b = 2` → parallel (b does NOT see a's value)

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm) section 6.1.1.7

---

### 3. FINALLY Clause Execution Semantics

**Decision**: FINALLY forms execute after loop termination but before returning

**Rationale**:
- ANSI CL specifies FINALLY executes on "normal" termination (not on RETURN from within loop body)
- However, RETURN within FINALLY itself works normally
- The current `expand-loop` places finally-forms after the tagbody, before result-form

**Current Code Analysis** (`expand-loop`, line 1182):
```lisp
`(let* ,all-bindings
   (block ,block-name
     (tagbody
        ,@(loop-context-initially-forms ctx)
        ,loop-start
        ,@termination-tests
        ,@body-code
        ,@acc-updates
        ,@step-code
        (go ,loop-start)
        ,loop-end)
     ,@(loop-context-finally-forms ctx)   ;; <-- FINALLY here
     ,result-form))
```

**Issue Found**: FINALLY forms should be able to use `return` to override the result. Current placement is correct but need to verify RETURN semantics.

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm) section 6.1.8.1

---

### 4. INTO Accumulator Variable Access

**Decision**: INTO variables are local to the LOOP and accessible in body and FINALLY

**Rationale**:
- INTO creates a named accumulator instead of implicit one
- The variable must be in scope for body references and FINALLY return
- Current implementation creates binding in `generate-accumulator-bindings`

**Current Implementation Check**:
- `loop-accumulation-clause-into-var` stores the INTO variable name
- If INTO specified, uses that; otherwise generates gensym
- `loop-accumulation-clause-acc-var` is set to INTO var or gensym

**Verification Points**:
1. `(loop ... collect x into results ... finally (return results))`
2. `(loop ... sum x into total ... do (print total))`
3. `(loop ... collect x into a sum y into b finally (return (list a b)))`

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm) section 6.1.3

---

### 5. Using Clause Parsing

**Decision**: Extend `parse-for-hash` to handle `using (hash-value v)` and `using (hash-key k)`

**Rationale**:
- ANSI CL syntax: `for var being {the|each} {hash-keys|hash-values} {of|in} ht [using ({hash-key|hash-value} other-var)]`
- Current `parse-for-hash` stops after parsing the hash-table form
- Need to check for `using` keyword and parse the secondary variable

**Implementation**:

```lisp
(defun parse-for-hash (var clauses)
  "Parse FOR var BEING THE HASH-KEYS/HASH-VALUES OF hash-table [USING ...]."
  (let* ((rest (rest clauses))  ; skip BEING
         ;; ... existing parsing ...
         (hash-form (first rest2))
         (final-rest (rest rest2))
         (using-var nil)
         (using-type nil))
    ;; Check for USING clause
    (when (loop-keyword-eq (first final-rest) 'using)
      (let ((using-spec (second final-rest)))  ; e.g., (HASH-VALUE v)
        (setf using-type (first using-spec))   ; HASH-VALUE or HASH-KEY
        (setf using-var (second using-spec))
        (setf final-rest (cddr final-rest))))
    (values (make-loop-iter-hash
             :var var
             :clause-type (if (eq mode :keys) :hash-keys :hash-values)
             :hash-form hash-form
             :mode mode
             :using-var using-var
             :using-type using-type)
            final-rest)))
```

**Reference**: [loop](resources/HyperSpec/Body/m_loop.htm) section 6.1.2.1.7

---

## Required Struct Changes

### loop-iter-hash Structure

Current fields (implied from `parse-for-hash`):
- `var` - primary iteration variable
- `clause-type` - `:hash-keys` or `:hash-values`
- `hash-form` - expression yielding hash-table
- `mode` - `:keys` or `:values`

**New fields needed**:
- `using-var` - secondary variable name (nil if not specified)
- `using-type` - `hash-key` or `hash-value` (determines which value goes to using-var)
- `entries-var` - gensym for collected entries list
- `iter-var` - gensym for current position in entries list

---

## Test Strategy

### Unit Tests (TDD Red-Green-Refactor)

1. **Hash-table keys iteration**
   ```lisp
   (loop for k being the hash-keys of ht collect k)
   ```

2. **Hash-table values iteration**
   ```lisp
   (loop for v being the hash-values of ht collect v)
   ```

3. **Hash-table keys with using (hash-value)**
   ```lisp
   (loop for k being the hash-keys of ht using (hash-value v) collect (cons k v))
   ```

4. **Empty hash-table**
   ```lisp
   (loop for k being the hash-keys of (make-hash-table) collect k) ;; => NIL
   ```

5. **WITH sequential binding**
   ```lisp
   (loop with a = 1 with b = (1+ a) return b) ;; => 2
   ```

6. **WITH parallel binding**
   ```lisp
   (loop with a = 1 and b = 2 return (+ a b)) ;; => 3
   ```

7. **FINALLY with return**
   ```lisp
   (loop for i from 1 to 3 finally (return 42)) ;; => 42
   ```

8. **INTO accumulator in body**
   ```lisp
   (loop for i from 1 to 3 sum i into total finally (return (* total 2))) ;; => 12
   ```

9. **Multiple INTO accumulators**
   ```lisp
   (loop for i from 1 to 3
         collect i into nums
         sum i into total
         finally (return (list nums total))) ;; => ((1 2 3) 6)
   ```

### Contract Tests (Wasm Validation)

1. Compiled LOOP with hash-table iteration passes `wasm-tools validate`
2. Generated Wasm correctly uses block/br for tagbody/go translation
3. Multiple values from maphash callback handled correctly

---

## Risks and Mitigations

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| maphash callback breaks control flow | Low | High | Pre-collect entries, iterate list |
| WITH binding order incorrect | Medium | Medium | Explicit test for let* vs let semantics |
| FINALLY executed on RETURN | Low | Medium | Test both normal and RETURN termination |
| INTO var name collision | Low | Low | Use gensym if not INTO, verify uniqueness |

---

## Dependencies

None - all required infrastructure exists:
- `maphash` - implemented in Clysm runtime
- `tagbody`/`go` - core special forms
- `let*` - core special form
- Multiple values - implemented (Phase 025)
