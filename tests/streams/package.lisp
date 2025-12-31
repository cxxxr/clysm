;;;; package.lisp - Test package for stream I/O tests
;;;; Tests for 015-ffi-stream-io feature

(in-package #:cl-user)

(defpackage #:clysm/tests/streams
  (:use #:cl #:rove #:clysm/streams)
  (:shadowing-import-from #:clysm/streams
                          #:stream #:streamp
                          #:input-stream-p #:output-stream-p
                          #:*standard-input* #:*standard-output* #:*error-output*
                          #:write-char #:write-string #:read-char #:read-line #:format
                          #:write-to-string #:prin1-to-string #:princ-to-string)
  ;; Use CL condition types for host-side testing
  (:import-from #:clysm/compiler/codegen/gc-types
                #:+type-stream+
                #:make-stream-type
                #:wasm-struct-type
                #:wasm-struct-type-name
                #:wasm-struct-type-fields
                #:wasm-field-name
                #:gc-type-index)
  (:documentation "Tests for FFI-based stream I/O module.
   Covers:
   - US1: write-char, write-string
   - US2: read-char, read-line
   - US3: format directives
   - US4: stream predicates and dynamic rebinding"))
