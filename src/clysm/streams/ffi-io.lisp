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

;; Write a Unicode codepoint to file descriptor.
;; T011: FFI import for character output.
;; FR-022: UTF-8 encoding at FFI boundary.
(clysm/ffi:define-foreign-function %host-write-char
    "clysm:io.write-char"
    (:fixnum :fixnum)
    :void)

;; Write a string to file descriptor.
;; T012: FFI import for string output.
;; Uses WasmGC anyref for string - no linear memory.
(clysm/ffi:define-foreign-function %host-write-string
    "clysm:io.write-string"
    (:fixnum :anyref)
    :void)

;;; ============================================================
;;; Input FFI Imports (T013, T014)
;;; ============================================================

;; Read a Unicode codepoint from file descriptor.
;; T013: FFI import for character input.
;; Returns -1 on EOF.
(clysm/ffi:define-foreign-function %host-read-char
    "clysm:io.read-char"
    (:fixnum)
    :fixnum)

;; Read a line from file descriptor.
;; T014: FFI import for line input.
;; Returns null anyref on EOF.
(clysm/ffi:define-foreign-function %host-read-line
    "clysm:io.read-line"
    (:fixnum)
    :anyref)
