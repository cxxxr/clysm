;;;; ffi.lisp - FFI declarations for filesystem operations
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Task: T009
;;;;
;;;; Declares FFI imports for host filesystem access under clysm:fs namespace.
;;;; These functions are implemented by the host environment:
;;;; - wasmtime: WASI Preview2 filesystem API
;;;; - browser: Virtual filesystem shim

(in-package #:clysm/filesystem)

;;; ============================================================
;;; FFI Imports (T009)
;;; ============================================================
;;;
;;; Host shim must provide these functions under "clysm:fs" namespace.
;;; FR-008: System MUST implement file operations via FFI to host environment.

#|
The following FFI functions are required from the host:

  clysm:fs.open (pathname direction if-exists if-does-not-exist) -> externref
    Opens a file and returns an opaque handle.
    - pathname: string - file path
    - direction: string - "input" or "output"
    - if-exists: string - "supersede" or "error"
    - if-does-not-exist: string - "error" or "create"
    Returns: opaque file handle
    Throws: on error (caught as ffi-host-error, translated to file-error)

  clysm:fs.close (handle) -> void
    Closes the file handle.
    - handle: externref - opaque file handle

  clysm:fs.read-all (handle) -> string
    Reads entire file contents as UTF-8 string.
    - handle: externref - opaque file handle
    Returns: string - file contents

  clysm:fs.write-all (handle contents) -> void
    Writes UTF-8 string to file, replacing contents.
    - handle: externref - opaque file handle
    - contents: string - data to write
|#

;;; ============================================================
;;; FFI Function Declarations
;;; ============================================================

;; These declarations register the FFI functions for use in compiled code.
;; The actual FFI mechanism depends on the clysm/ffi infrastructure.

(clysm/ffi:define-foreign-function %open-file
    "clysm:fs.open"
  (:string :string :string :string)  ; pathname, direction, if-exists, if-dne
  :anyref)                            ; returns opaque file handle (anyref for externref)

(clysm/ffi:define-foreign-function %close-file
    "clysm:fs.close"
  (:anyref)                           ; file handle
  :void)

(clysm/ffi:define-foreign-function %read-all
    "clysm:fs.read-all"
  (:anyref)                           ; file handle
  :string)                            ; returns file contents

(clysm/ffi:define-foreign-function %write-all
    "clysm:fs.write-all"
  (:anyref :string)                   ; file handle, contents
  :void)

;;; ============================================================
;;; FFI Error Translation
;;; ============================================================

(defmacro with-file-error-translation ((pathname) &body body)
  "Execute BODY, translating FFI errors to file-error conditions.
   PATHNAME is used in the file-error condition."
  (let ((pathname-var (gensym "PATHNAME")))
    `(let ((,pathname-var ,pathname))
       (handler-case
           (progn ,@body)
         (clysm/ffi:ffi-host-error (e)
           (cl:error (make-instance 'clysm/conditions:file-error
                                    :pathname ,pathname-var)))))))
