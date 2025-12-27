;;;; contracts/filesystem.lisp - API Contracts for FFI Filesystem Access
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Date: 2025-12-27
;;;;
;;;; This file defines the public API contracts for filesystem operations.
;;;; Implementation will be in src/clysm/ffi/filesystem.lisp

(in-package #:clysm/filesystem)

;;; ============================================================
;;; Condition Types
;;; ============================================================

(defclass file-error (error)
  ((pathname
    :initarg :pathname
    :reader file-error-pathname
    :type string
    :documentation "The pathname that caused the error"))
  (:documentation "Condition signaled for filesystem-related errors.
   FR-006: System MUST signal file-error condition when file operations fail."))

;;; ============================================================
;;; Core Types
;;; ============================================================

(defstruct file-stream
  "Represents an open file handle with metadata.
   FR-001: System MUST provide open-file function to obtain a file stream handle."
  (handle nil :type (or null externref))  ; Opaque host handle
  (direction nil :type (member :input :output))
  (pathname "" :type string)
  (open-p t :type boolean))

;;; ============================================================
;;; FFI Imports (low-level)
;;; ============================================================

;; Internal FFI declarations - not part of public API
;; (ffi:define-foreign-function %open-file "clysm:fs.open" ...)
;; (ffi:define-foreign-function %close-file "clysm:fs.close" ...)
;; (ffi:define-foreign-function %read-all "clysm:fs.read-all" ...)
;; (ffi:define-foreign-function %write-all "clysm:fs.write-all" ...)

;;; ============================================================
;;; Public API: File Operations
;;; ============================================================

(defun open-file (pathname &key (direction :input)
                                (if-exists :supersede)
                                (if-does-not-exist nil))
  "Open a file and return a file-stream object.

   PATHNAME: String naming the file to open.
   DIRECTION: :input for reading, :output for writing.
   IF-EXISTS: For output, :supersede (default) or :error.
   IF-DOES-NOT-EXIST: :error (input default), :create (output default).

   Returns: A file-stream object.
   Signals: file-error if file cannot be opened.

   FR-001: System MUST provide open-file function to obtain a file stream handle.
   FR-007: System MUST support :direction parameter with values :input and :output.
   FR-012: System MUST support :if-exists parameter.
   FR-013: System MUST support :if-does-not-exist parameter.

   Examples:
     (open-file \"data.txt\" :direction :input)
     (open-file \"output.txt\" :direction :output :if-exists :error)"
  (declare (type string pathname)
           (type (member :input :output) direction)
           (type (member :supersede :error) if-exists)
           (type (or null (member :error :create)) if-does-not-exist))
  ;; Default if-does-not-exist based on direction
  (let ((actual-if-dne (or if-does-not-exist
                           (if (eq direction :input) :error :create))))
    ;; Implementation calls %open-file FFI
    ;; Returns file-stream or signals file-error
    (error "Contract stub - implementation required")))

(defun close-file (stream)
  "Close a file stream and release the underlying handle.

   STREAM: A file-stream object returned by open-file.
   Returns: NIL.
   Signals: file-error if stream is already closed.

   FR-002: System MUST provide close-file function to release a file stream handle.
   FR-011: System MUST reject operations on closed file streams.

   Example:
     (let ((s (open-file \"data.txt\")))
       (close-file s))"
  (declare (type file-stream stream))
  ;; Implementation calls %close-file FFI
  (error "Contract stub - implementation required"))

(defun read-file-contents (pathname-or-stream)
  "Read the entire contents of a file as a string.

   PATHNAME-OR-STREAM: Either a pathname string or an open input file-stream.
   Returns: A string containing the file contents.
   Signals: file-error if file cannot be read.

   FR-003: System MUST provide read-file-contents function.
   FR-010: System MUST properly encode/decode UTF-8.

   Examples:
     (read-file-contents \"config.txt\")
     ;; => \"key=value\\n\"

     (with-open-file (s \"data.txt\" :direction :input)
       (read-file-contents s))"
  (declare (type (or string file-stream) pathname-or-stream))
  ;; If string, open-read-close
  ;; If stream, read from stream
  (error "Contract stub - implementation required"))

(defun write-file-contents (pathname-or-stream contents)
  "Write a string to a file, replacing any existing contents.

   PATHNAME-OR-STREAM: Either a pathname string or an open output file-stream.
   CONTENTS: String to write to the file.
   Returns: NIL.
   Signals: file-error if file cannot be written.

   FR-004: System MUST provide write-file-contents function.
   FR-010: System MUST properly encode/decode UTF-8.

   Examples:
     (write-file-contents \"output.txt\" \"Hello, World!\")

     (with-open-file (s \"data.txt\" :direction :output)
       (write-file-contents s \"line 1\\nline 2\\n\"))"
  (declare (type (or string file-stream) pathname-or-stream)
           (type string contents))
  ;; If string, open-write-close
  ;; If stream, write to stream
  (error "Contract stub - implementation required"))

;;; ============================================================
;;; Public API: Macro
;;; ============================================================

(defmacro with-open-file ((stream-var pathname &rest options) &body body)
  "Open a file, bind it to STREAM-VAR, execute BODY, then close the file.

   Uses unwind-protect to ensure the file is closed even if an error occurs.

   STREAM-VAR: Symbol to bind the file-stream to.
   PATHNAME: String naming the file to open.
   OPTIONS: Keyword arguments passed to open-file.
            :direction :input|:output
            :if-exists :supersede|:error
            :if-does-not-exist :error|:create

   FR-005: System MUST provide with-open-file macro using unwind-protect.

   Examples:
     ;; Read file contents
     (with-open-file (s \"data.txt\" :direction :input)
       (read-file-contents s))

     ;; Write to file
     (with-open-file (s \"output.txt\" :direction :output)
       (write-file-contents s \"Hello!\"))

     ;; Error handling - file is still closed
     (with-open-file (s \"data.txt\" :direction :input)
       (error \"Something went wrong\")
       ;; File is closed before error propagates
       )"
  `(let ((,stream-var (open-file ,pathname ,@options)))
     (unwind-protect
         (progn ,@body)
       (when ,stream-var
         (close-file ,stream-var)))))

;;; ============================================================
;;; FFI Host Interface (for host-shim implementation)
;;; ============================================================

#|
Host shim must provide these functions under "clysm:fs" namespace:

  open(pathname: string, direction: string, ifExists: string, ifDoesNotExist: string): externref
    Opens a file and returns an opaque handle.
    Throws on error (caught as ffi-host-error, translated to file-error).

  close(handle: externref): void
    Closes the file handle.

  read-all(handle: externref): string
    Reads entire file contents as UTF-8 string.

  write-all(handle: externref, contents: string): void
    Writes UTF-8 string to file, replacing contents.

JavaScript example:
  export function getImports() {
    return {
      'clysm:fs': {
        'open': (pathname, direction, ifExists, ifDoesNotExist) => { ... },
        'close': (handle) => { ... },
        'read-all': (handle) => { ... },
        'write-all': (handle, contents) => { ... }
      }
    };
  }
|#

;;; ============================================================
;;; Test Contracts (for verification)
;;; ============================================================

#|
Unit Tests Required:

1. open-file-input-existing
   Given: File "test.txt" exists
   When: (open-file "test.txt" :direction :input)
   Then: Returns file-stream with direction=:input, open-p=T

2. open-file-input-not-found
   Given: File "nonexistent.txt" does not exist
   When: (open-file "nonexistent.txt" :direction :input)
   Then: Signals file-error with pathname="nonexistent.txt"

3. open-file-output-create
   Given: File "new.txt" does not exist
   When: (open-file "new.txt" :direction :output)
   Then: Creates file, returns file-stream with direction=:output

4. close-file-success
   Given: Open file-stream s
   When: (close-file s)
   Then: s.open-p becomes NIL, returns NIL

5. close-file-already-closed
   Given: Closed file-stream s
   When: (close-file s)
   Then: Signals file-error

6. read-file-contents-string
   Given: File "test.txt" contains "Hello"
   When: (read-file-contents "test.txt")
   Then: Returns "Hello"

7. read-file-contents-utf8
   Given: File "unicode.txt" contains "日本語🎉"
   When: (read-file-contents "unicode.txt")
   Then: Returns "日本語🎉"

8. write-file-contents-string
   Given: Empty or nonexistent "output.txt"
   When: (write-file-contents "output.txt" "Test")
   Then: File contains "Test"

9. with-open-file-normal
   Given: File "test.txt" exists
   When: (with-open-file (s "test.txt") (read-file-contents s))
   Then: Returns file contents, stream is closed after

10. with-open-file-error
    Given: File "test.txt" exists
    When: (with-open-file (s "test.txt") (error "fail"))
    Then: Error propagates, but stream is still closed

Contract Tests Required:

1. filesystem-wasm-validation
   When: Compile file operations to Wasm
   Then: wasm-tools validate passes

2. filesystem-ffi-imports
   When: Compile file operations to Wasm
   Then: Module contains clysm:fs imports

Integration Tests Required:

1. read-write-roundtrip
   When: Write "content" to file, read back
   Then: Contents match exactly

2. cross-platform-behavior
   When: Same operations run on wasmtime and browser
   Then: Results are identical
|#
