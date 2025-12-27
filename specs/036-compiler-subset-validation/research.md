# Research: Compiler Subset Validation

**Date**: 2025-12-27
**Feature**: 036-compiler-subset-validation

## Research Areas

### 1. Static Analysis of CL Features

**Decision**: Use S-expression walker with symbol extraction

**Rationale**: Common Lisp code is already S-expressions, so we can leverage the standard Lisp reader to parse source files and walk the resulting forms to extract symbols. This is more reliable than regex-based approaches and handles nested forms, quoting, and macro-like patterns correctly.

**Alternatives Considered**:
- Regex-based extraction: Rejected - cannot handle nested parentheses, string escaping, comments
- Full macro expansion: Rejected - requires loading all dependencies, defeats purpose of static analysis

**Implementation Approach**:
```lisp
(defun extract-symbols (form)
  "Recursively extract all symbols from a Lisp form"
  (cond
    ((symbolp form) (list form))
    ((consp form) (append (extract-symbols (car form))
                          (extract-symbols (cdr form))))
    (t nil)))
```

### 2. Feature Registry Design

**Decision**: Maintain a declarative hash-table mapping CL symbols to support status

**Rationale**: A simple hash-table lookup provides O(1) classification. The registry can be manually curated based on Clysm's actual implementation status.

**Support Status Categories**:
- `:supported` - Full ANSI CL semantics implemented
- `:partial` - Subset of functionality implemented (e.g., limited keyword args)
- `:unsupported` - Not implemented in Clysm
- `:internal` - Clysm-specific, not standard CL

**Registry Structure**:
```lisp
(defparameter *clysm-features*
  (alexandria:alist-hash-table
   '((defun . :supported)
     (defmacro . :supported)
     (let . :supported)
     (let* . :supported)
     (lambda . :supported)
     (if . :supported)
     (progn . :supported)
     (loop . :partial)  ; limited clauses
     (format . :partial) ; limited directives
     ...)))
```

### 3. Dependency Graph Analysis

**Decision**: Use ASDF system definitions for inter-module dependencies

**Rationale**: The clysm.asd file already defines component ordering. We can parse this to determine the correct compilation order rather than manually specifying it.

**Alternatives Considered**:
- Manual ordering: Specified in FR-006, but may diverge from actual code
- Runtime dependency detection: Too complex, requires loading code

**Approach**:
1. Parse `clysm.asd` for `:components` and `:depends-on`
2. Build dependency DAG
3. Topological sort for compilation order

### 4. Wasm Validation Integration

**Decision**: Shell out to `wasm-tools validate` via `uiop:run-program`

**Rationale**: `wasm-tools` is already in the Nix devShell. Using it as an external validator ensures we test the exact same validation that CI uses.

**Alternatives Considered**:
- WebAssembly.validate() in browser: Would require browser/Node.js integration
- Custom WAT parser validation: Incomplete, doesn't validate binary encoding

**Integration Pattern**:
```lisp
(defun validate-wasm (wasm-bytes)
  "Validate Wasm binary using wasm-tools"
  (uiop:with-temporary-file (:pathname path :type "wasm" :direction :output)
    (write-sequence wasm-bytes path)
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list "wasm-tools" "validate" (namestring path))
                          :output :string :error-output :string)
      (values (zerop exit-code) error-output))))
```

### 5. Report Generation

**Decision**: Generate Markdown reports with per-file and per-module summaries

**Rationale**: Markdown is human-readable and can be viewed in GitHub. It integrates well with existing documentation.

**Report Structure**:
```markdown
# Feature Coverage Report

## Summary
- Total symbols analyzed: N
- Supported: X (Y%)
- Partial: A (B%)
- Unsupported: C (D%)

## Per-Module Coverage

### backend/
| File | Total | Supported | Partial | Unsupported |
|------|-------|-----------|---------|-------------|
| leb128.lisp | 45 | 42 (93%) | 2 (4%) | 1 (2%) |
...

## Unsupported Features Detail
| Symbol | Files | Occurrences |
|--------|-------|-------------|
| with-open-file | foo.lisp, bar.lisp | 5 |
```

### 6. Blessed Subset Documentation

**Decision**: Generate `blessed-subset.lisp` as a loadable Lisp file with structured comments

**Rationale**: A Lisp file can be loaded to verify syntax, and the structure allows programmatic querying of supported features.

**Format**:
```lisp
;;; blessed-subset.lisp - Self-Compilable Common Lisp Subset for Clysm
;;; Generated: YYYY-MM-DD
;;;
;;; This file documents which CL features are verified to work in
;;; Clysm's self-compilation process.

(in-package :clysm-validation)

(defparameter *blessed-special-forms*
  '(if progn let let* lambda quote function setq tagbody go block return-from
    catch throw unwind-protect multiple-value-call multiple-value-prog1))

(defparameter *blessed-macros*
  '(defun defmacro defvar defparameter defconstant
    when unless cond case typecase etypecase ctypecase
    and or not
    push pop incf decf
    dolist dotimes do do*
    loop ;; NOTE: limited clause support
    ...))

(defparameter *blessed-functions*
  '(;; Arithmetic
    + - * / mod rem floor ceiling truncate round
    < <= > >= = /=
    ...))

(defparameter *partial-support-notes*
  '((loop . "Only :for :collect :do :return :while :until supported")
    (format . "Only ~A ~S ~D ~% ~& supported")
    ...))
```

## Technical Decisions Summary

| Area | Decision | Key Reason |
|------|----------|------------|
| Symbol Extraction | S-expression walker | Handles all CL syntax correctly |
| Feature Registry | Hash-table with 4 categories | O(1) lookup, clear semantics |
| Dependency Order | ASDF-based + FR-006 override | Matches actual build system |
| Wasm Validation | External wasm-tools | Same as CI, guaranteed accuracy |
| Report Format | Markdown | Human-readable, GitHub-friendly |
| Blessed Subset | Loadable Lisp file | Verifiable syntax, queryable data |
