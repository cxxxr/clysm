;;;; ffi-io.lisp - FFI imports for host I/O functions
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/streams)

;;; ============================================================
;;; Host Module Configuration
;;; ============================================================

(defparameter *io-host-module* "clysm:io"
  "Host module name for I/O imports per FR-021")

;;; ============================================================
;;; Output FFI Imports (T011, T012)
;;; ============================================================

(clysm/ffi:define-foreign-function %host-write-char
    (:module "clysm:io"
     :name "write-char"
     :params ((:fd :i32) (:codepoint :i32))
     :results ())
  "Write a Unicode codepoint to file descriptor.
   T011: FFI import for character output.
   FR-022: UTF-8 encoding at FFI boundary.")

(clysm/ffi:define-foreign-function %host-write-string
    (:module "clysm:io"
     :name "write-string"
     :params ((:fd :i32) (:string :externref))
     :results ())
  "Write a string to file descriptor.
   T012: FFI import for string output.
   Uses WasmGC externref for string - no linear memory.")

;;; ============================================================
;;; Input FFI Imports (T013, T014)
;;; ============================================================

(clysm/ffi:define-foreign-function %host-read-char
    (:module "clysm:io"
     :name "read-char"
     :params ((:fd :i32))
     :results ((:codepoint :i32)))
  "Read a Unicode codepoint from file descriptor.
   T013: FFI import for character input.
   Returns -1 on EOF.")

(clysm/ffi:define-foreign-function %host-read-line
    (:module "clysm:io"
     :name "read-line"
     :params ((:fd :i32))
     :results ((:line :externref)))
  "Read a line from file descriptor.
   T014: FFI import for line input.
   Returns null externref on EOF.")
