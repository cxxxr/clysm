# Quickstart: I/O and List Operations Runtime Migration

**Date**: 2026-01-01
**Feature**: 001-io-list-runtime

## Prerequisites

1. SBCL 2.4+ with Clysm loaded
2. wasm-tools for validation
3. Node.js 20+ for integration tests

```bash
nix develop  # Enter development shell with all dependencies
```

## Getting Started

### 1. Verify Current State

```bash
# Check current func-section.lisp line count
wc -l src/clysm/compiler/codegen/func-section.lisp
# Expected: 18233 lines

# Run existing tests to establish baseline
sbcl --eval "(asdf:test-system :clysm)"
```

### 2. Create Runtime Files (TDD Approach)

**Step 2a: Write tests first**

```bash
# Create test directories
mkdir -p tests/unit/list-runtime
mkdir -p tests/unit/io-runtime
```

```lisp
;; tests/unit/list-runtime/member-test.lisp
(in-package :clysm/test)

(deftest member-basic-test
  "FR-005: member with default test"
  (ok (equal (member 'b '(a b c)) '(b c)))
  (ok (null (member 'd '(a b c)))))

(deftest member-with-key-test
  "FR-005: member with :key argument"
  (ok (equal (member 2 '((1 a) (2 b) (3 c)) :key #'car) '((2 b) (3 c)))))
```

**Step 2b: Create list-runtime.lisp**

```lisp
;; src/clysm/lib/list-runtime.lisp
(in-package :clysm)

;;; member - HyperSpec: resources/HyperSpec/Body/f_mem_m.htm
(defun member-rt (item list &key (test #'eql) test-not (key #'identity))
  "Runtime implementation of MEMBER using car/cdr primitives."
  (when test-not
    (setf test (complement test-not)))
  (let ((current list))
    (loop
      (when (null current) (return nil))
      (when (funcall test item (funcall key (car current)))
        (return current))
      (setf current (cdr current)))))
```

### 3. Update Compiler Dispatch

```lisp
;; In func-section.lisp, modify function dispatch:

(defparameter *runtime-functions*
  '(member member-if member-if-not
    assoc assoc-if rassoc rassoc-if
    find find-if find-if-not
    position position-if position-if-not
    princ prin1 print write format terpri)
  "Functions implemented in runtime library, not inline.")

(defun compile-function-call (fn-name args env)
  (if (member fn-name *runtime-functions*)
      (compile-runtime-call fn-name args env)
      (funcall (get-compile-function fn-name) args env)))

(defun compile-runtime-call (fn-name args env)
  "Emit call to runtime function."
  (let ((compiled-args (mapcar (lambda (a) (compile-to-instructions a env)) args)))
    (append (apply #'append compiled-args)
            `((:call ,(intern (format nil "$~A-rt" fn-name) :keyword))))))
```

### 4. Verify Migration

```bash
# Run tests
sbcl --eval "(asdf:test-system :clysm)"

# Validate generated Wasm
sbcl --load build/stage1-complete.lisp
wasm-tools validate dist/clysm-stage1.wasm

# Check line count reduction
wc -l src/clysm/compiler/codegen/func-section.lisp
# Target: < 11000 lines
```

### 5. Integration Test

```bash
# Test with sample program
cat > /tmp/test-migration.lisp <<EOF
(format t "Testing member: ~A~%" (member 'b '(a b c)))
(format t "Testing assoc: ~A~%" (assoc 'x '((x . 1) (y . 2))))
(format t "Testing find: ~A~%" (find 3 '(1 2 3 4 5)))
(format t "Testing position: ~A~%" (position #\a "banana"))
EOF

sbcl --eval "(clysm:compile-file-to-wasm \"/tmp/test-migration.lisp\" :output \"/tmp/test.wasm\")"
node host-shim/runner.js /tmp/test.wasm
```

## File Structure After Implementation

```
src/clysm/lib/
├── list-runtime.lisp    # NEW: member, assoc, find, position
├── io-runtime.lisp      # NEW: princ, print, write, format
└── list-ops.lisp        # EXISTING: reference implementation

tests/unit/
├── list-runtime/
│   ├── member-test.lisp
│   ├── assoc-test.lisp
│   ├── find-test.lisp
│   └── position-test.lisp
└── io-runtime/
    ├── princ-test.lisp
    ├── print-test.lisp
    ├── write-test.lisp
    └── format-test.lisp
```

## Common Issues

### Issue: "Undefined function $member-rt"

**Cause**: Runtime library not loaded before user code

**Solution**: Ensure runtime is linked before user module:
```lisp
(clysm:compile-to-wasm form :link-runtime t)
```

### Issue: Performance regression > 10%

**Cause**: Call overhead for hot paths

**Solution**: Consider specializing simple cases:
```lisp
;; Inline simple (member item list) with eql test
(defun compile-member-simple (args env)
  (when (= (length args) 2)
    (compile-runtime-call 'member args env)))
```

## Next Steps

After completing implementation:

1. Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"`
2. Verify Stage 1 generation: `sbcl --load build/stage1-complete.lisp`
3. Run performance benchmark: `sbcl --load tests/benchmark/io-list.lisp`
4. Update CLAUDE.md with feature completion
