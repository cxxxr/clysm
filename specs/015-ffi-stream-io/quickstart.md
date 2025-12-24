# Quickstart: FFI-based Stream I/O

**Date**: 2025-12-24
**Branch**: `015-ffi-stream-io`

## Overview

This guide covers implementing FFI-based stream I/O for clysm3 without linear memory.

---

## 1. Prerequisites

Ensure these features are complete:
- 012-ffi-foundation (FFI call mechanism)
- 014-condition-system (error signaling)
- 002-special-vars-compiler (dynamic variables)
- 008-character-string (character/string types)

Check flake.nix includes wasmtime:
```bash
nix develop
wasmtime --version
```

---

## 2. Add Stream Type

### 2.1 Define Type Index

In `src/clysm/compiler/codegen/gc-types.lisp`:

```lisp
(defconstant +type-stream+ 19 "Type index for stream struct")
```

### 2.2 Create Stream Type Constructor

```lisp
(defun make-stream-type ()
  "Create stream type for I/O channels."
  (make-wasm-struct-type
   :name '$stream
   :index +type-stream+
   :fields (list (make-wasm-field :name 'fd :type :i32 :mutable nil)
                 (make-wasm-field :name 'direction :type :i32 :mutable nil)
                 (make-wasm-field :name 'element-type :type :anyref :mutable nil))))
```

### 2.3 Register in Type Section

Add to `generate-type-definitions`:
```lisp
(make-stream-type)  ; type 19
```

---

## 3. Define FFI Imports

### 3.1 Create streams/ffi-io.lisp

```lisp
(in-package #:clysm/streams)

;; Host I/O imports
(ffi:define-foreign-function %host-write-char
  :host-name "clysm:io.write-char"
  :params (:fixnum :fixnum)   ; fd, codepoint
  :return :void)

(ffi:define-foreign-function %host-write-string
  :host-name "clysm:io.write-string"
  :params (:fixnum :anyref)   ; fd, string-ref
  :return :void)

(ffi:define-foreign-function %host-read-char
  :host-name "clysm:io.read-char"
  :params (:fixnum)           ; fd
  :return :fixnum)            ; codepoint or -1

(ffi:define-foreign-function %host-read-line
  :host-name "clysm:io.read-line"
  :params (:fixnum)           ; fd
  :return :anyref)            ; string-ref or null
```

---

## 4. Implement I/O Functions

### 4.1 write-char

```lisp
(defun write-char (character &optional (stream *standard-output*))
  "Write CHARACTER to STREAM."
  (check-type character character)
  (check-type stream stream)
  (unless (output-stream-p stream)
    (error 'type-error :datum stream :expected-type 'output-stream))
  (%host-write-char (stream-fd stream) (char-code character))
  character)
```

### 4.2 read-char

```lisp
(defun read-char (&optional (stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  "Read a character from STREAM."
  (declare (ignore recursive-p))
  (check-type stream stream)
  (unless (input-stream-p stream)
    (error 'type-error :datum stream :expected-type 'input-stream))
  (let ((codepoint (%host-read-char (stream-fd stream))))
    (if (= codepoint -1)
        (if eof-error-p
            (error 'end-of-file :stream stream)
            eof-value)
        (code-char codepoint))))
```

---

## 5. Implement Format

### 5.1 Compile-time Parser

```lisp
(defun parse-format-string (control-string)
  "Parse format string into segments and directives."
  (let ((segments nil)
        (directives nil)
        (pos 0)
        (len (length control-string))
        (arg-index 0))
    (loop while (< pos len)
          for tilde-pos = (position #\~ control-string :start pos)
          do (if tilde-pos
                 (progn
                   ;; Save literal segment before tilde
                   (when (> tilde-pos pos)
                     (push (subseq control-string pos tilde-pos) segments))
                   ;; Parse directive
                   (incf tilde-pos)
                   (let ((directive-char (char control-string tilde-pos)))
                     (push (make-format-directive
                            :character directive-char
                            :position tilde-pos
                            :arg-index (unless (member directive-char '(#\% #\~))
                                        (prog1 arg-index (incf arg-index))))
                           directives)
                     (setf pos (1+ tilde-pos))))
                 (progn
                   ;; No more tildes, save remaining text
                   (push (subseq control-string pos) segments)
                   (setf pos len))))
    (make-format-string-info
     :literal-segments (nreverse segments)
     :directives (nreverse directives)
     :arg-count arg-index)))
```

### 5.2 Format Macro

```lisp
(defmacro format (destination control-string &rest args)
  "Produce formatted output."
  (if (stringp control-string)
      ;; Compile-time optimization
      (let ((info (parse-format-string control-string)))
        (generate-format-code destination info args))
      ;; Runtime fallback
      `(format-runtime ,destination ,control-string ,@args)))
```

---

## 6. Initialize Standard Streams

### 6.1 Runtime Initialization

In `src/clysm/runtime/stream-vars.lisp`:

```lisp
(defvar *standard-input* nil)
(defvar *standard-output* nil)
(defvar *error-output* nil)

(defun initialize-standard-streams ()
  "Initialize standard stream special variables."
  (setf *standard-input*
        (make-stream :fd 0 :direction :input :element-type :character))
  (setf *standard-output*
        (make-stream :fd 1 :direction :output :element-type :character))
  (setf *error-output*
        (make-stream :fd 2 :direction :output :element-type :character)))
```

### 6.2 Call During Module Start

Add to Wasm module start function:
```lisp
(initialize-standard-streams)
```

---

## 7. Create Host Shim

### 7.1 host-shim/io-shim.js

```javascript
export const clysm_io = {
  "write-char": (fd, codepoint) => {
    const char = String.fromCodePoint(codepoint);
    if (fd === 1) process.stdout.write(char);
    else if (fd === 2) process.stderr.write(char);
  },

  "write-string": (fd, stringRef) => {
    if (!stringRef) return;
    const bytes = new Uint8Array(stringRef.length);
    for (let i = 0; i < stringRef.length; i++) {
      bytes[i] = stringRef.get(i);
    }
    const text = new TextDecoder('utf-8').decode(bytes);
    if (fd === 1) process.stdout.write(text);
    else if (fd === 2) process.stderr.write(text);
  },

  "read-char": (fd) => {
    // Implementation depends on Node.js readline
    return -1; // EOF placeholder
  },

  "read-line": (fd) => {
    // Implementation depends on Node.js readline
    return null; // EOF placeholder
  }
};
```

---

## 8. Testing

### 8.1 Unit Tests

```lisp
(deftest write-char-test
  (testing "write-char returns character"
    (ok (char= #\H (write-char #\H)))))

(deftest format-test
  (testing "format ~A"
    (ok (string= "test" (format nil "~A" "test"))))
  (testing "format ~D"
    (ok (string= "42" (format nil "~D" 42))))
  (testing "format ~%"
    (ok (string= (string #\Newline) (format nil "~%")))))
```

### 8.2 Integration Test with wasmtime

```bash
# Compile test program
nix develop
./compile-test.lisp stream-io-test.lisp -o test.wasm

# Run with host shim
node --experimental-wasm-gc run-with-shim.js test.wasm
```

---

## 9. Verification Checklist

- [ ] $stream type added at index 19
- [ ] FFI imports registered for clysm:io module
- [ ] write-char/write-string work with host shim
- [ ] read-char/read-line handle EOF correctly
- [ ] format supports ~A, ~S, ~D, ~%, ~~
- [ ] Standard stream variables initialized at startup
- [ ] All tests pass with wasmtime
- [ ] No linear memory used
