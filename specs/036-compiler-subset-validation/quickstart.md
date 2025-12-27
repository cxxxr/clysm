# Quickstart: Compiler Subset Validation

**Feature**: 036-compiler-subset-validation
**Date**: 2025-12-27

## Prerequisites

1. Nix with Flakes enabled
2. wasm-tools in devShell (already configured)
3. SBCL 2.4+ with Quicklisp

## Quick Commands

```bash
# Enter development environment
nix develop

# Run static analysis on all modules
sbcl --load src/clysm/validation/analyzer.lisp \
     --eval '(clysm-validation:analyze-all)' \
     --quit

# Generate coverage report
sbcl --load src/clysm/validation/reporter.lisp \
     --eval '(clysm-validation:generate-report "coverage-report.md")' \
     --quit

# Run dependency-order compilation validation
sbcl --load src/clysm/validation/compiler-order.lisp \
     --eval '(clysm-validation:validate-all-modules)' \
     --quit

# Run all validation tests
sbcl --load tests/main.lisp \
     --eval '(rove:run :clysm-validation-tests)' \
     --quit
```

## Development Workflow

### 1. Static Analysis (Phase 11A)

Analyze which CL features are used in each module:

```lisp
;; Load the analyzer
(ql:quickload :clysm-validation)

;; Analyze a single file
(clysm-validation:analyze-file #p"src/clysm/backend/leb128.lisp")
;; => ((DEFUN . 6) (LET . 4) (ASH . 2) ...)

;; Analyze a directory
(clysm-validation:analyze-directory #p"src/clysm/backend/")
;; => hash-table of file -> symbol-usage

;; Check support status
(clysm-validation:feature-status 'defun)  ;; => :supported
(clysm-validation:feature-status 'loop)   ;; => :partial
```

### 2. Dependency-Order Compilation (Phase 11B)

Compile modules in order and validate Wasm output:

```lisp
;; Compile a single module
(clysm-validation:compile-and-validate #p"src/clysm/backend/leb128.lisp")
;; => (values T wasm-bytes) or (values NIL error-message)

;; Compile all in dependency order
(clysm-validation:validate-all-modules)
;; => summary of pass/fail per module
```

### 3. Wasm Validation (Phase 11C)

Validate compiled Wasm using wasm-tools:

```lisp
;; Validate Wasm bytes
(clysm-validation:validate-wasm #(0 97 115 109 ...))
;; => (values T nil) or (values NIL "error message")
```

### 4. Blessed Subset Generation (Phase 11D)

Generate the documentation file:

```lisp
;; Generate blessed-subset.lisp
(clysm-validation:generate-blessed-subset "docs/blessed-subset.lisp")
```

## Test Structure

```text
tests/
├── unit/validation/
│   ├── analyzer-test.lisp      # Symbol extraction tests
│   ├── feature-registry-test.lisp  # Registry lookup tests
│   └── reporter-test.lisp      # Report generation tests
├── contract/validation/
│   └── module-wasm-test.lisp   # Wasm output validation
└── integration/validation/
    └── self-compile-test.lisp  # Full pipeline test
```

## Running Tests

```bash
# Run all validation tests
sbcl --load tests/main.lisp \
     --eval '(rove:run :clysm-validation-tests)' \
     --quit

# Run specific test file
sbcl --load tests/unit/validation/analyzer-test.lisp \
     --eval '(rove:run :analyzer-test)' \
     --quit
```

## Expected Outputs

### Coverage Report (Markdown)

```markdown
# Feature Coverage Report
Generated: 2025-12-27

## Summary
- Total symbols analyzed: 156
- Supported: 142 (91%)
- Partial: 8 (5%)
- Unsupported: 6 (4%)

## Per-Module Coverage
...
```

### blessed-subset.lisp

```lisp
;;; Clysm Blessed Subset - Self-Compilable CL Features
;;; Generated: 2025-12-27

(defparameter *blessed-special-forms*
  '(if progn let let* lambda ...))

(defparameter *blessed-macros*
  '(defun defmacro when unless ...))
...
```

## Troubleshooting

### Common Issues

1. **wasm-tools not found**: Ensure you're in `nix develop` shell
2. **Compilation fails with "unsupported feature"**: Check the error message for the specific CL symbol
3. **Wasm validation fails**: Run `wasm-tools print output.wasm` to see the WAT for debugging

### Debug Mode

```lisp
;; Enable verbose logging
(setf clysm-validation:*verbose* t)

;; Trace symbol extraction
(trace clysm-validation:extract-symbols)
```
