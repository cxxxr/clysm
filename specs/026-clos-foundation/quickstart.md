# Quickstart: CLOS Foundation Implementation

**Feature**: 026-clos-foundation
**Date**: 2025-12-27

## Prerequisites

```bash
# Enter development environment
nix develop

# Verify tools available
sbcl --version
wasm-tools --version
wasmtime --version
```

## Running Tests

```bash
# All tests
nix flake check

# CLOS-specific tests
sbcl --load run-tests.lisp --eval '(rove:run :clysm/test/clos)'

# Contract tests (Wasm validation)
sbcl --load run-tests.lisp --eval '(rove:run :clysm/test/contract/clos)'
```

## Key Files to Modify

### 1. WasmGC Types (gc-types.lisp)

**Location**: `src/clysm/compiler/codegen/gc-types.lisp`

Add type definitions for `$instance` (index 6) and `$standard-class` (index 7):

```lisp
;; Around line 47, update reserved type indices
(defconstant +type-instance+ 6)
(defconstant +type-standard-class+ 7)

;; Add new type generator functions
(defun make-instance-type ()
  "Generate $instance struct type definition"
  `(:struct
    (:field :class (:ref ,+type-standard-class+))
    (:field :slots (:ref ,+type-slot-vector+))))

(defun make-standard-class-type ()
  "Generate $standard-class struct type definition"
  `(:struct
    (:field :name (:ref ,+type-symbol+))
    (:field :superclass (:ref-null ,+type-standard-class+))
    (:field :slot-count :i32)
    (:field :initargs (:ref ,+type-keyword-array+))
    (:field :initforms (:ref ,+type-closure-array+))
    (:field :class-id :i32)))
```

### 2. Type Section Emission (compiler.lisp)

**Location**: `src/clysm/compiler/compiler.lisp`

Update `emit-type-section` to include CLOS types:

```lisp
;; In emit-type-section, around line 400
;; Replace nil placeholders for types 6-7
(emit-gc-struct-type stream (make-instance-type))       ;; Type 6
(emit-gc-struct-type stream (make-standard-class-type)) ;; Type 7
```

### 3. AST Nodes (ast.lisp)

**Location**: `src/clysm/compiler/ast.lisp`

Add AST structures for CLOS forms:

```lisp
;; Add near existing AST definitions
(defstruct (ast-defclass (:include ast))
  (name nil :type symbol)
  (superclass nil :type (or null symbol))
  (slots nil :type list)
  (options nil :type list))

(defstruct (ast-make-instance (:include ast))
  (class-name nil :type (or symbol ast))
  (initargs nil :type list))

(defstruct (ast-defmethod (:include ast))
  (name nil :type symbol)
  (specializers nil :type list)
  (lambda-list nil :type list)
  (body nil :type list))
```

### 4. Code Generation (func-section.lisp)

**Location**: `src/clysm/compiler/codegen/func-section.lisp`

Add compilation patterns for CLOS operations:

```lisp
;; make-instance codegen
(defmethod compile-form ((form ast-make-instance) env)
  (let* ((class-name (ast-make-instance-class-name form))
         (class-global (format nil "$class-~a" class-name)))
    `(;; Get class
      (:global.get ,class-global)
      ;; Get slot count
      (:struct.get ,+type-standard-class+ 2)
      ;; Create slot vector
      (:array.new_default ,+type-slot-vector+)
      ;; ... initialize slots from initargs ...
      ;; Create instance
      (:struct.new ,+type-instance+))))

;; Accessor reader codegen
(defun compile-slot-reader (class-name slot-name slot-index)
  `((:local.get 0)  ;; instance argument
    (:struct.get ,+type-instance+ 1)  ;; get slots
    (:i32.const ,slot-index)
    (:array.get ,+type-slot-vector+)))

;; Accessor writer codegen
(defun compile-slot-writer (class-name slot-name slot-index)
  `((:local.get 0)  ;; value
    (:local.get 1)  ;; instance
    (:struct.get ,+type-instance+ 1)  ;; get slots
    (:i32.const ,slot-index)
    (:local.get 0)  ;; value again
    (:array.set ,+type-slot-vector+)))
```

## Example: End-to-End Test

```lisp
;; tests/integration/clos-ansi-test.lisp

(deftest point-class-roundtrip
  "Verify (make-instance 'point :x 3 :y 4) works"
  (let* ((forms '(progn
                   (defclass point ()
                     ((x :initarg :x :accessor point-x)
                      (y :initarg :y :accessor point-y)))
                   (let ((p (make-instance 'point :x 3 :y 4)))
                     (list (point-x p) (point-y p)))))
         (result (compile-and-run forms)))
    (ok (equal result '(3 4)))))

(deftest method-dispatch
  "Verify basic method dispatch"
  (let* ((forms '(progn
                   (defclass point () ((x :initarg :x) (y :initarg :y)))
                   (defmethod area ((p point))
                     (* (slot-value p 'x) (slot-value p 'y)))
                   (area (make-instance 'point :x 3 :y 4))))
         (result (compile-and-run forms)))
    (ok (= result 12))))
```

## Debugging Tips

### Inspect Generated WAT

```lisp
;; Print WAT output for a form
(let ((wat (clysm/compiler:compile-to-wat
             '(defclass point () ((x :initarg :x))))))
  (format t "~a" wat))
```

### Validate Wasm Binary

```bash
# Validate binary
wasm-tools validate output.wasm

# Print as WAT for inspection
wasm-tools print output.wasm
```

### Check Type Section

```lisp
;; Inspect type section bytes
(let ((module (clysm/compiler:compile-module forms)))
  (clysm/compiler:dump-section module :type))
```

## Common Patterns

### Class Definition Pattern

```lisp
;; defclass expansion
(defclass point (geometric-object)
  ((x :initarg :x :accessor point-x :initform 0)
   (y :initarg :y :accessor point-y :initform 0)))

;; Compiles to:
;; 1. Global for class metadata
;; 2. Reader function for point-x
;; 3. Writer function for (setf point-x)
;; 4. Reader function for point-y
;; 5. Writer function for (setf point-y)
```

### Method Definition Pattern

```lisp
;; defmethod expansion
(defmethod print-object ((p point) stream)
  (format stream "#<POINT ~a ~a>" (point-x p) (point-y p)))

;; Compiles to:
;; 1. Method function (closure)
;; 2. Registration in generic function's method table
```

### Instance Creation Pattern

```lisp
;; make-instance call
(make-instance 'point :x 3 :y 4)

;; Compiles to:
;; 1. Global.get $class-point
;; 2. Array.new_default $slot-vector
;; 3. Initialize slot 0 with 3
;; 4. Initialize slot 1 with 4
;; 5. Struct.new $instance
```

## Architecture Overview

```
defclass form
    │
    ▼
┌─────────────┐     ┌─────────────┐
│ Parse       │────►│ AST Node    │
│ (defclass)  │     │ ast-defclass│
└─────────────┘     └─────────────┘
                           │
                           ▼
                    ┌─────────────┐
                    │ Finalize    │
                    │ Class       │
                    └─────────────┘
                           │
         ┌─────────────────┼─────────────────┐
         ▼                 ▼                 ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ Class       │    │ Accessor    │    │ Accessor    │
│ Global      │    │ Functions   │    │ Functions   │
│ (metadata)  │    │ (readers)   │    │ (writers)   │
└─────────────┘    └─────────────┘    └─────────────┘
```

## Checklist for Implementation

- [ ] Add `$instance` type (index 6) to gc-types.lisp
- [ ] Add `$standard-class` type (index 7) to gc-types.lisp
- [ ] Add `$slot-vector` array type (index 21)
- [ ] Update `emit-type-section` in compiler.lisp
- [ ] Add `ast-defclass` to ast.lisp
- [ ] Add `ast-make-instance` to ast.lisp
- [ ] Add `ast-defmethod` to ast.lisp
- [ ] Implement `compile-defclass` in func-section.lisp
- [ ] Implement `compile-make-instance` in func-section.lisp
- [ ] Implement accessor generation
- [ ] Implement method dispatch codegen
- [ ] Write unit tests for each component
- [ ] Write contract tests for Wasm validation
- [ ] Write integration tests for end-to-end scenarios
