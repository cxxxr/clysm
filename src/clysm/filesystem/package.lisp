;;;; package.lisp - Package definition for filesystem module
;;;;
;;;; This file defines the filesystem package for Clysm.
;;;; Provides file I/O operations via FFI to host environments
;;;; (wasmtime WASI Preview2, browser Virtual FS).
;;;;
;;;; Feature: 035-ffi-filesystem

(in-package #:cl-user)

(defpackage #:clysm/filesystem
  (:use #:cl)
  ;; Shadow CL symbols that conflict with our definitions
  (:shadow #:file-stream)
  ;; Use shadowing-import-from to import file-error from clysm/conditions
  ;; This shadows cl:file-error and imports clysm/conditions:file-error
  (:shadowing-import-from #:clysm/conditions
                          #:file-error)
  (:import-from #:clysm/conditions
                #:clysm-file-error-pathname)
  (:export
   ;; Core file operations
   #:open-file
   #:close-file
   #:read-file-contents
   #:write-file-contents

   ;; Safe file handling macro
   #:with-open-file*

   ;; File stream structure (note: shadows cl:file-stream)
   #:file-stream
   #:file-stream-p
   #:make-file-stream
   #:file-stream-handle
   #:file-stream-direction
   #:file-stream-pathname
   #:file-stream-open-p

   ;; Conditions (re-exported from clysm/conditions)
   #:file-error
   #:clysm-file-error-pathname

   ;; Internal FFI functions (exported for testing)
   #:%open-file
   #:%close-file
   #:%read-all
   #:%write-all

   ;; Direction and mode keywords
   ;; :input :output :supersede :error :create
   ))
