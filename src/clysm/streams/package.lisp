;;;; package.lisp - Package definition for stream I/O module
;;;; FFI-based stream I/O without linear memory (015-ffi-stream-io)

(in-package #:cl-user)

(defpackage #:clysm/streams
  (:use #:cl #:alexandria)
  (:shadow #:stream #:streamp
           #:input-stream-p #:output-stream-p
           #:*standard-input* #:*standard-output* #:*error-output*
           #:write-char #:write-string #:read-char #:read-line #:format
           #:princ-to-string #:prin1-to-string)
  (:import-from #:clysm/ffi
                #:define-foreign-function)
  ;; Note: Using CL condition types for host-side testing compatibility
  ;; clysm/conditions types are for compiled Wasm output modeling
  ;; Stream type and predicates (FR-014, US4)
  (:export #:stream
           #:streamp
           #:input-stream-p
           #:output-stream-p
           #:stream-fd
           #:stream-direction
           #:make-stream
           ;; Direction constants
           #:+direction-input+
           #:+direction-output+
           #:+direction-io+)
  ;; Output operations (FR-001, FR-002, FR-003, US1)
  (:export #:write-char
           #:write-string)
  ;; Input operations (FR-004, FR-005, FR-006, FR-007, US2)
  (:export #:read-char
           #:read-line)
  ;; Format function (FR-008 to FR-013, US3)
  (:export #:format)
  ;; Standard streams (FR-015, FR-016, FR-017)
  (:export #:*standard-input*
           #:*standard-output*
           #:*error-output*)
  ;; Note: Stream conditions use CL types (stream-error, end-of-file)
  ;; for host-side testing compatibility
  (:documentation "FFI-based stream I/O for clysm compiler.
   Implements Common Lisp stream operations via FFI calls to host environment.
   No linear memory is used; all data passes through WasmGC types.

   Features:
   - write-char, write-string for output (US1)
   - read-char, read-line for input (US2)
   - format with ~A, ~S, ~D, ~%, ~~ directives (US3)
   - Stream predicates and first-class stream values (US4)
   - Standard streams: *standard-input*, *standard-output*, *error-output*"))
