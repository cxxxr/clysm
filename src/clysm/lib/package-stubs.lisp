;;;; package-stubs.lisp - FFI-backed package operation stubs
;;;; Feature: 001-compiler-internal-consolidation (Phase 4, US3)
;;;;
;;;; Implements Wasm-compilable package stubs that delegate to host via FFI.
;;;; These functions resolve the "Undefined function: PACKAGEP*" blockers
;;;; (P951 in stage1-report.json, 25 occurrences, 2.80%).
;;;;
;;;; HyperSpec references:
;;;;   [find-package](resources/HyperSpec/Body/f_find_p.htm)
;;;;   [intern](resources/HyperSpec/Body/f_intern.htm)
;;;;   [packagep](resources/HyperSpec/Body/f_pkgp.htm)
;;;;
;;;; Pattern: Follow io-runtime.lisp FFI pattern with %host-* prefix.

(in-package #:clysm)

;;; ============================================================
;;; FFI Function Declarations (T030, T032, T034)
;;; ============================================================
;;; These functions are imported from the host environment.
;;; Module: "clysm:pkg", functions: find-package, intern, packagep

(defun %host-find-package (name)
  "FFI stub for host find-package operation.
   NAME: string designator for package name
   Returns: package object or nil"
  (declare (type string name))
  ;; This will be compiled to an FFI import call.
  ;; At host load time, we delegate to CL:FIND-PACKAGE.
  (find-package name))

(defun %host-intern (name package-designator)
  "FFI stub for host intern operation.
   NAME: string for symbol name
   PACKAGE-DESIGNATOR: package object or string
   Returns: symbol and status"
  (declare (type string name))
  ;; At host load time, we delegate to CL:INTERN.
  (intern name (if (stringp package-designator)
                   (find-package package-designator)
                   package-designator)))

(defun %host-packagep (object)
  "FFI stub for host packagep predicate.
   OBJECT: any object
   Returns: t if package, nil otherwise"
  ;; At host load time, we delegate to CL:PACKAGEP.
  (packagep object))

;;; ============================================================
;;; T031: find-package* - Package lookup (FR-013)
;;; ============================================================

(defun find-package* (name)
  "Find a package by NAME, returning the package object or nil.
   Wasm-compilable version that delegates to host FFI.

   Usage:
     (find-package* \"CL\")        => #<PACKAGE COMMON-LISP>
     (find-package* \"NONEXIST\")  => NIL

   See: [find-package](resources/HyperSpec/Body/f_find_p.htm)"
  (declare (type (or string symbol) name))
  (let ((name-string (if (symbolp name)
                         (symbol-name name)
                         name)))
    (%host-find-package name-string)))

;;; ============================================================
;;; T033: intern* - Symbol interning (FR-014)
;;; ============================================================

(defun intern* (name &optional (package *package*))
  "Intern a symbol with NAME in PACKAGE.
   Wasm-compilable version that delegates to host FFI.

   Usage:
     (intern* \"FOO\" \"CL-USER\")  => FOO, :INTERNAL or :EXTERNAL
     (intern* \"BAR\")             => BAR (in *package*)

   See: [intern](resources/HyperSpec/Body/f_intern.htm)"
  (declare (type string name))
  (let ((pkg (if (stringp package)
                 (find-package* package)
                 package)))
    (if pkg
        (%host-intern name pkg)
        (error "Package not found: ~A" package))))

;;; ============================================================
;;; T035: packagep* - Package predicate (FR-015)
;;; ============================================================

(defun packagep* (object)
  "Test if OBJECT is a package.
   Wasm-compilable version that delegates to host FFI.

   Usage:
     (packagep* (find-package \"CL\"))  => T
     (packagep* 'foo)                   => NIL

   See: [packagep](resources/HyperSpec/Body/f_pkgp.htm)"
  (%host-packagep object))

;;; ============================================================
;;; Additional Package Utilities (for compiler use)
;;; ============================================================

(defun package-name* (package)
  "Return the name of PACKAGE as a string.
   Wasm-compilable version."
  (if (packagep* package)
      (package-name package)
      (error "Not a package: ~A" package)))

(defun symbol-package* (symbol)
  "Return the home package of SYMBOL.
   Wasm-compilable version."
  (if (symbolp symbol)
      (symbol-package symbol)
      nil))

;;; ============================================================
;;; Export declarations
;;; ============================================================

(export '(find-package*
          intern*
          packagep*
          package-name*
          symbol-package*
          %host-find-package
          %host-intern
          %host-packagep))
