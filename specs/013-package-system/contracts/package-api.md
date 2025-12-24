# Package System API Contract

**Date**: 2025-12-24
**Feature**: 013-package-system

## Overview

ANSI Common Lisp準拠のパッケージシステム内部API契約。全関数はCL仕様に準拠した動作を保証する。

## Package Management Functions

### make-package

```lisp
(make-package name &key nicknames use)
  → package

;; Parameters:
;;   name      - string designator (package name)
;;   nicknames - list of string designators (optional)
;;   use       - list of package designators (optional, default: nil)
;;
;; Returns:
;;   The newly created package
;;
;; Errors:
;;   - package-error if package with same name/nickname already exists
;;
;; Example:
;;   (make-package "MY-PKG" :nicknames '("MP") :use '("CL"))
```

### find-package

```lisp
(find-package name)
  → package or nil

;; Parameters:
;;   name - string designator or package
;;
;; Returns:
;;   The package object, or nil if not found
;;
;; Example:
;;   (find-package "CL") → #<PACKAGE "COMMON-LISP">
;;   (find-package :cl)  → #<PACKAGE "COMMON-LISP">
```

### delete-package

```lisp
(delete-package package)
  → t or nil

;; Parameters:
;;   package - package designator
;;
;; Returns:
;;   t if successfully deleted, nil if already deleted
;;
;; Errors:
;;   - package-error if package is used by other packages
;;
;; Side Effects:
;;   - All symbols become uninterned
;;   - Package removed from *packages* registry
```

### rename-package

```lisp
(rename-package package new-name &optional new-nicknames)
  → package

;; Parameters:
;;   package       - package designator
;;   new-name      - string designator
;;   new-nicknames - list of string designators (optional)
;;
;; Returns:
;;   The renamed package
;;
;; Errors:
;;   - package-error if new name/nickname conflicts
```

### list-all-packages

```lisp
(list-all-packages)
  → list of packages

;; Returns:
;;   A fresh list of all registered packages
```

## Symbol Interning Functions

### intern

```lisp
(intern string &optional package)
  → symbol, status

;; Parameters:
;;   string  - the symbol name (case-preserved, compared uppercase)
;;   package - package designator (default: *package*)
;;
;; Returns (multiple values):
;;   symbol - the interned symbol
;;   status - :internal, :external, :inherited, or nil (newly created)
;;
;; Behavior:
;;   1. Search package's external symbols
;;   2. Search package's internal symbols
;;   3. Search inherited symbols (use-list)
;;   4. If not found, create new internal symbol
;;
;; Example:
;;   (intern "FOO" :my-pkg) → MY-PKG::FOO, nil
;;   (intern "CAR" :cl-user) → CAR, :inherited
```

### find-symbol

```lisp
(find-symbol string &optional package)
  → symbol or nil, status or nil

;; Parameters:
;;   string  - the symbol name
;;   package - package designator (default: *package*)
;;
;; Returns (multiple values):
;;   symbol - the found symbol, or nil
;;   status - :internal, :external, :inherited, or nil
;;
;; Behavior:
;;   Same search order as intern, but never creates new symbols
```

### unintern

```lisp
(unintern symbol &optional package)
  → t or nil

;; Parameters:
;;   symbol  - the symbol to unintern
;;   package - package designator (default: *package*)
;;
;; Returns:
;;   t if symbol was present and removed, nil otherwise
;;
;; Side Effects:
;;   - Removes symbol from package's symbol tables
;;   - Sets symbol's home-package to nil if this was home package
;;   - May cause name conflict in using packages
```

## Export/Import Functions

### export

```lisp
(export symbols &optional package)
  → t

;; Parameters:
;;   symbols - symbol or list of symbols
;;   package - package designator (default: *package*)
;;
;; Returns:
;;   t (always succeeds or signals error)
;;
;; Errors:
;;   - package-error if name conflict in using packages
;;
;; Behavior:
;;   Moves symbols from internal to external table
```

### unexport

```lisp
(unexport symbols &optional package)
  → t

;; Parameters:
;;   symbols - symbol or list of symbols
;;   package - package designator (default: *package*)
;;
;; Returns:
;;   t
;;
;; Behavior:
;;   Moves symbols from external to internal table
```

### import

```lisp
(import symbols &optional package)
  → t

;; Parameters:
;;   symbols - symbol or list of symbols
;;   package - package designator (default: *package*)
;;
;; Returns:
;;   t
;;
;; Errors:
;;   - package-error if name conflict with existing different symbol
;;
;; Behavior:
;;   Adds foreign symbols to package's internal table
;;   Does not change symbol's home-package
```

### shadowing-import

```lisp
(shadowing-import symbols &optional package)
  → t

;; Parameters:
;;   symbols - symbol or list of symbols
;;   package - package designator (default: *package*)
;;
;; Returns:
;;   t
;;
;; Behavior:
;;   Like import, but adds to shadowing-symbols list
;;   Overwrites any existing symbol with same name
```

### shadow

```lisp
(shadow symbol-names &optional package)
  → t

;; Parameters:
;;   symbol-names - string designator or list thereof
;;   package      - package designator (default: *package*)
;;
;; Returns:
;;   t
;;
;; Behavior:
;;   Creates new internal symbols with given names
;;   Adds them to shadowing-symbols list
```

## Package Use Functions

### use-package

```lisp
(use-package packages-to-use &optional package)
  → t

;; Parameters:
;;   packages-to-use - package designator or list thereof
;;   package         - package designator (default: *package*)
;;
;; Returns:
;;   t
;;
;; Errors:
;;   - package-error if name conflict with existing symbols
;;
;; Behavior:
;;   Adds packages to use-list
;;   Makes their external symbols accessible as inherited
```

### unuse-package

```lisp
(unuse-package packages-to-unuse &optional package)
  → t

;; Parameters:
;;   packages-to-unuse - package designator or list thereof
;;   package           - package designator (default: *package*)
;;
;; Returns:
;;   t
;;
;; Behavior:
;;   Removes packages from use-list
```

## Package Information Functions

### package-name

```lisp
(package-name package)
  → string or nil

;; Returns the primary name of the package, or nil if deleted
```

### package-nicknames

```lisp
(package-nicknames package)
  → list of strings

;; Returns a fresh list of the package's nicknames
```

### package-use-list

```lisp
(package-use-list package)
  → list of packages

;; Returns a fresh list of packages this package uses
```

### package-used-by-list

```lisp
(package-used-by-list package)
  → list of packages

;; Returns a fresh list of packages that use this package
```

### package-shadowing-symbols

```lisp
(package-shadowing-symbols package)
  → list of symbols

;; Returns a fresh list of shadowing symbols
```

### packagep

```lisp
(packagep object)
  → boolean

;; Returns t if object is a package, nil otherwise
```

### symbol-package

```lisp
(symbol-package symbol)
  → package or nil

;; Returns the home package of the symbol, or nil if uninterned
```

## Macros

### defpackage

```lisp
(defpackage name &rest options)
  → package

;; Options:
;;   (:nicknames . names)
;;   (:use . packages)
;;   (:export . symbols)
;;   (:import-from package . symbols)
;;   (:shadow . symbols)
;;   (:shadowing-import-from package . symbols)
;;
;; Behavior:
;;   Creates or modifies a package with specified options
;;   Executed at compile-time and load-time
```

### in-package

```lisp
(in-package name)
  → package

;; Parameters:
;;   name - package designator (not evaluated)
;;
;; Behavior:
;;   Sets *package* to the named package
;;   Signals error if package doesn't exist
;;   Executed at compile-time and load-time
```

## Error Conditions

### package-error

```lisp
(define-condition package-error (error)
  ((package :initarg :package :reader package-error-package)))

;; Signaled for package-related errors:
;;   - Package not found
;;   - Name conflict
;;   - Invalid operation on deleted package
```

## Reader Integration

### Token Format

```lisp
;; Input: "cl:car"
;; Token: (:qualified-symbol :package-name "CL"
;;                           :symbol-name "CAR"
;;                           :external t
;;                           :line 1 :column 0)

;; Input: "pkg::internal"
;; Token: (:qualified-symbol :package-name "PKG"
;;                           :symbol-name "INTERNAL"
;;                           :external nil
;;                           :line 1 :column 0)

;; Input: ":keyword"
;; Token: (:keyword "KEYWORD")  ; existing format
```

### Resolution Rules

1. **External reference (single colon)**:
   - Package must exist → error if not
   - Symbol must be exported → error if not

2. **Internal reference (double colon)**:
   - Package must exist → error if not
   - Symbol interned if not present (no export check)

3. **Keyword**:
   - Always in KEYWORD package
   - Automatically exported and self-evaluating
