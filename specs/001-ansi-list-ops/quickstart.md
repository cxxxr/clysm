# Quickstart: Phase 15A - ANSI List Operations Extension

**Date**: 2025-12-30
**Branch**: `001-ansi-list-ops`

## Getting Started

### Prerequisites

1. Nix development environment active:
   ```bash
   nix develop
   ```

2. SBCL 2.4+ available:
   ```bash
   sbcl --version
   ```

3. Tests passing:
   ```bash
   sbcl --eval "(asdf:test-system :clysm)" --quit
   ```

### Development Workflow

#### 1. Run Existing Tests

Verify baseline before starting:

```bash
sbcl --eval "(asdf:test-system :clysm)" --quit
```

#### 2. TDD Cycle

For each function group, follow Red-Green-Refactor:

```lisp
;; 1. Write failing test (Red)
(deftest last-basic-test
  "Test (last '(a b c)) returns (c)."
  (testing "last with default n=1"
    (let ((wasm (clysm:compile-to-wasm '(last '(a b c)))))
      (ok wasm "Should compile"))))

;; 2. Run test - should fail
;; sbcl --eval "(asdf:test-system :clysm)" --quit

;; 3. Implement function (Green)
;; Add to src/clysm/lib/list-ops.lisp

;; 4. Run test - should pass

;; 5. Refactor if needed
```

#### 3. Wasm Validation

After implementing each function:

```bash
# Compile a test expression
sbcl --eval "(clysm:compile-to-wasm-file '(last (list 1 2 3)) \"/tmp/test.wasm\")" --quit

# Validate output
wasm-tools validate /tmp/test.wasm

# Inspect WAT (optional)
wasm-tools print /tmp/test.wasm
```

## Implementation Order

Follow priority order from spec:

### Phase 1: List Tail Operations (P1)

Files to modify:
- `src/clysm/lib/list-ops.lisp` (create)
- `tests/unit/list-ops/list-tail-test.lisp` (create)

Functions: `last`, `butlast`, `nbutlast`, `nth`, `nthcdr`

### Phase 2: Membership Operations (P1)

Files to modify:
- `src/clysm/lib/list-ops.lisp`
- `tests/unit/list-ops/member-test.lisp` (create)

Functions: `member`, `member-if`, `member-if-not`

### Phase 3: Association List Lookup (P1)

Files to modify:
- `src/clysm/lib/list-ops.lisp`
- `tests/unit/list-ops/assoc-test.lisp` (extend existing)

Functions: `assoc`, `assoc-if`, `rassoc`, `rassoc-if`

### Phase 4: Alist Construction (P2)

Files to modify:
- `src/clysm/lib/list-ops.lisp`
- `tests/unit/list-ops/alist-construct-test.lisp` (create)

Functions: `pairlis`, `acons`, `copy-alist`

### Phase 5: Set Operations (P2)

Files to modify:
- `src/clysm/lib/list-ops.lisp`
- `tests/unit/list-ops/set-ops-test.lisp` (create)

Functions: `intersection`, `union`, `set-difference`, `subsetp`, `adjoin`

### Phase 6: PUSHNEW Macro (P3)

Files to modify:
- `src/clysm/compiler/transform/macro.lisp`
- `tests/unit/list-ops/pushnew-test.lisp` (create)

Macro: `pushnew`

## Key Implementation Patterns

### Loop-Based List Traversal

```lisp
;; Pattern for traversing lists (avoid recursion)
(defun nthcdr-impl (n list)
  (loop repeat n
        while list
        do (setf list (cdr list)))
  list)
```

### Keyword Argument Handling

```lisp
;; Existing pattern for :test/:key
(defun member-impl (item list &key (test #'eql) (key #'identity))
  (loop for tail on list
        when (funcall test item (funcall key (car tail)))
          return tail))
```

### Wasm Loop Pattern

```wat
;; Expected output pattern for nthcdr
(block $done
  (loop $traverse
    (br_if $done (i32.eqz (local.get $n)))
    (local.set $list (struct.get 2 1
                       (ref.cast (ref 2) (local.get $list))))
    (local.set $n (i32.sub (local.get $n) (i32.const 1)))
    (br $traverse)))
```

## Verification Examples

After implementation, these should work:

```lisp
;; From spec SC-004
(intersection '(1 2 3) '(2 3 4))  ; => (2 3) or (3 2)
(member 2 '(1 2 3))               ; => (2 3)
(assoc 'b '((a . 1) (b . 2)))     ; => (B . 2)
```

## Debugging Tips

### Check Generated WAT

```lisp
(format t "~a" (clysm/compiler:compile-to-wat '(member 1 '(1 2 3))))
```

### Verify Type Indices

- $cons = type 2
- CAR = field 0
- CDR = field 1

### Common Issues

1. **NIL check**: Use `ref.eq` against global NIL, not `ref.is_null`
2. **Loop vs recursion**: Use loops to avoid stack overflow
3. **Type casting**: Use `ref.cast (ref 2)` before `struct.get`

## Resources

- [ANSI CL HyperSpec](resources/HyperSpec/) - Local reference
- [plan.md](./plan.md) - Full implementation plan
- [research.md](./research.md) - Research decisions
- [contracts/list-operations.md](./contracts/list-operations.md) - Function signatures
