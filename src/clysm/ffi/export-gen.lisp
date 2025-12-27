;;;; export-gen.lisp - Wasm export section generation (T033-T035)
;;;;
;;;; Generates Wasm export section entries from FFI export declarations.
;;;; Exports allow host environments (JavaScript/wasmtime) to call Lisp functions.

(in-package #:clysm/ffi)

;;; ============================================================
;;; Export Collection (T034)
;;; ============================================================

(defun collect-ffi-exports (env)
  "Collect all FFI export declarations from the environment.
   Returns a list of ExportDecl structures."
  (ffi-env-exports env))

(defun get-ffi-export-count ()
  "Return the number of FFI exports currently registered."
  (length (ffi-env-exports *ffi-environment*)))

;;; ============================================================
;;; Export Index Assignment (T034)
;;; ============================================================

(defun assign-export-indices (env)
  "Assign wrapper function indices to all export declarations.
   Returns the number of exports processed."
  (let ((func-index 0))
    (dolist (decl (ffi-env-exports env))
      (setf (ed-wrapper-func-index decl) func-index)
      (incf func-index))
    func-index))

;;; ============================================================
;;; LEB128 Encoding Helpers
;;; ============================================================

(defun emit-export-leb128-unsigned (value buffer)
  "Emit unsigned LEB128 encoding of VALUE to BUFFER."
  (loop
    (let ((byte (logand value #x7f)))
      (setf value (ash value -7))
      (if (zerop value)
          (progn
            (vector-push-extend byte buffer)
            (return))
          (vector-push-extend (logior byte #x80) buffer)))))

(defun emit-export-string (string buffer)
  "Emit a UTF-8 string with length prefix to BUFFER."
  (let ((bytes (clysm/lib/utf8:string-to-utf8-octets string)))
    (emit-export-leb128-unsigned (length bytes) buffer)
    (loop for b across bytes do (vector-push-extend b buffer))))

;;; ============================================================
;;; Export Entry Encoding (T034)
;;; ============================================================

;; Wasm export kind constants
(defconstant +export-kind-func+ #x00)
(defconstant +export-kind-table+ #x01)
(defconstant +export-kind-memory+ #x02)
(defconstant +export-kind-global+ #x03)

(defun emit-export-entry (decl buffer)
  "Emit a single export entry to BUFFER.
   Format: name (vec<byte>) + exportdesc (kind + index)"
  ;; Export name (UTF-8 string with length prefix)
  (emit-export-string (ed-export-name decl) buffer)
  ;; Export kind (0x00 = function)
  (vector-push-extend +export-kind-func+ buffer)
  ;; Function index (wrapper function)
  (emit-export-leb128-unsigned (or (ed-wrapper-func-index decl) 0) buffer))

;;; ============================================================
;;; Export Section Emission (T034)
;;; ============================================================

(defconstant +section-id-export+ 7
  "Wasm Export section ID")

(defun emit-ffi-exports (env buffer)
  "Emit the Export section to BUFFER from FFI environment.
   Returns the number of exports emitted."
  (let ((exports (collect-ffi-exports env)))
    (when exports
      (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)))
        ;; Export count
        (emit-export-leb128-unsigned (length exports) content)
        ;; Export entries
        (dolist (export exports)
          (emit-export-entry export content))
        ;; Section ID (7 = Export)
        (vector-push-extend +section-id-export+ buffer)
        ;; Section size
        (emit-export-leb128-unsigned (length content) buffer)
        ;; Section content
        (loop for b across content do (vector-push-extend b buffer))))
    (length exports)))

;;; ============================================================
;;; T033: Export Wrapper Generation
;;; ============================================================

(defun generate-export-wrapper (decl)
  "Generate a wrapper function for an exported Lisp function.
   The wrapper handles:
   1. Unmarshalling arguments from Wasm types to Lisp types
   2. Calling the actual Lisp function
   3. Marshalling the return value back to Wasm type

   T052: Marshalling is now integrated.
   T056: Re-entrancy safety for callbacks (027-complete-ffi):
   - Wrappers use only local variables and function parameters
   - No global mutable state accessed during execution
   - Marshal/unmarshal operations are pure functions
   - Special variable bindings preserved via binding frame stack
   - Condition handlers preserved via handler stack globals
   - Safe for recursive callbacks (Lisp→Host→Lisp→Host→...)

   Returns Wasm instructions for the wrapper function."
  (let ((lisp-name (ed-lisp-name decl))
        (param-types (ed-param-types decl))
        (return-type (ed-return-type decl)))
    ;; Generate the wrapper with marshalling:
    ;; 1. Get parameters from locals (Wasm types from host)
    ;; 2. Unmarshal each parameter to Lisp type
    ;; 3. Call the Lisp function
    ;; 4. Marshal return value to Wasm type
    `(:func (:export ,(ed-export-name decl))
            (:params ,@(mapcar #'marshal-type-to-wasm-type param-types))
            (:result ,@(unless (eq return-type :void)
                         (list (marshal-type-to-wasm-type return-type))))
            ;; Body: unmarshal params, call Lisp function, marshal result
            ,@(generate-export-wrapper-body lisp-name param-types return-type))))

(defun generate-export-wrapper-body (lisp-name param-types return-type)
  "Generate the body of an export wrapper function.
   Returns a list of Wasm instructions."
  (let ((body '()))
    ;; For each parameter, get from local and unmarshal
    (loop for i from 0
          for param-type in param-types
          do
             ;; Get the parameter from local
             (push `(local.get ,i) body)
             ;; Unmarshal from Wasm type to Lisp type
             (let ((unmarshal-instrs (marshal-from-wasm param-type)))
               (when unmarshal-instrs
                 (dolist (instr unmarshal-instrs)
                   (push instr body)))))
    ;; Call the Lisp function
    (push `(:call-lisp ,lisp-name) body)
    ;; Marshal return value if not void
    (unless (eq return-type :void)
      (let ((marshal-instrs (marshal-to-wasm return-type)))
        (when marshal-instrs
          (dolist (instr marshal-instrs)
            (push instr body)))))
    (nreverse body)))

(defun marshal-type-to-wasm-type (marshal-type)
  "Convert a marshal type to its Wasm type representation."
  (ecase marshal-type
    (:fixnum 'i31ref)  ; i31ref for 31-bit integers
    (:float 'f64)      ; f64 for floats
    (:string 'externref)  ; externref for strings (WasmGC array)
    (:boolean 'i32)    ; i32 for booleans (0/1)
    (:anyref 'anyref)  ; anyref passthrough
    (:void nil)))      ; no type for void
