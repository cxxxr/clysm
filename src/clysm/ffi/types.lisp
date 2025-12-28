;;;; types.lisp - FFI type definitions (T007-T014)
;;;;
;;;; Defines MarshalType, ForeignFunctionDecl, ExportDecl, WasmImport,
;;;; FFIEnvironment structures, and FFI conditions.

(in-package #:clysm/ffi)

;;; ============================================================
;;; T007: MarshalType Type Specifier
;;; ============================================================

(deftype marshal-type ()
  "Type identifier for FFI marshalling.
   :fixnum  - i31ref <-> i32 (sign-extended 31-bit integer)
   :float   - (ref $float) <-> f64 (IEEE 754 double)
   :string  - (ref $string) <-> externref (WasmGC array of i8)
   :boolean - t/NIL <-> i32 (1/0)
   :anyref  - anyref <-> anyref (passthrough, no conversion)
   :void    - N/A (no return value)"
  '(member :fixnum :float :string :boolean :anyref :void))

(defun valid-marshal-type-p (type)
  "Check if TYPE is a valid marshal type."
  (typep type 'marshal-type))

;;; ============================================================
;;; T008: ForeignFunctionDecl Structure
;;; ============================================================

(defstruct (foreign-function-decl (:conc-name ffd-))
  "Declaration for an imported host function."
  (lisp-name nil :type symbol)           ; Lisp-side function name
  (module-name nil :type (or null string)) ; Wasm module name (e.g., "host")
  (field-name nil :type (or null string))  ; Wasm field name (e.g., "console.log")
  (param-types nil :type list)           ; List of marshal types
  (return-type nil :type (or null keyword)) ; Marshal type or :void
  (type-index nil :type (or null fixnum)) ; Generated type index (for import section)
  ;; 001-numeric-functions: Separate field for function index
  ;; In Wasm, imports get indices 0 to N-1, local functions get N onwards
  (func-index nil :type (or null fixnum))) ; Function index (for :call instructions)

(defun validate-foreign-function-decl (decl)
  "Validate a ForeignFunctionDecl, signaling error if invalid."
  (unless (symbolp (ffd-lisp-name decl))
    (error "lisp-name must be a symbol: ~A" (ffd-lisp-name decl)))
  (unless (and (stringp (ffd-module-name decl))
               (> (length (ffd-module-name decl)) 0))
    (error "module-name must be a non-empty string: ~A" (ffd-module-name decl)))
  (unless (and (stringp (ffd-field-name decl))
               (> (length (ffd-field-name decl)) 0))
    (error "field-name must be a non-empty string: ~A" (ffd-field-name decl)))
  (dolist (param-type (ffd-param-types decl))
    (unless (valid-marshal-type-p param-type)
      (error "Invalid param type: ~A (must be one of :fixnum :float :string :boolean :anyref :void)"
             param-type)))
  (unless (valid-marshal-type-p (ffd-return-type decl))
    (error "Invalid return type: ~A" (ffd-return-type decl)))
  t)

;;; ============================================================
;;; T009: ExportDecl Structure
;;; ============================================================

(defstruct (export-decl (:conc-name ed-))
  "Declaration for an exported Lisp function."
  (lisp-name nil :type symbol)           ; Lisp function symbol
  (export-name nil :type (or null string)) ; Wasm export name
  (param-types nil :type list)           ; List of marshal types
  (return-type nil :type (or null keyword)) ; Marshal type
  (wrapper-func-index nil :type (or null fixnum))) ; Generated wrapper function index

(defun validate-export-decl (decl)
  "Validate an ExportDecl, signaling error if invalid."
  (unless (symbolp (ed-lisp-name decl))
    (error "lisp-name must be a symbol: ~A" (ed-lisp-name decl)))
  (unless (and (stringp (ed-export-name decl))
               (> (length (ed-export-name decl)) 0))
    (error "export-name must be a non-empty string: ~A" (ed-export-name decl)))
  (dolist (param-type (ed-param-types decl))
    (unless (valid-marshal-type-p param-type)
      (error "Invalid param type: ~A" param-type)))
  (unless (valid-marshal-type-p (ed-return-type decl))
    (error "Invalid return type: ~A" (ed-return-type decl)))
  t)

;;; ============================================================
;;; T010: FFIEnvironment Structure
;;; ============================================================

(defstruct (ffi-environment (:conc-name ffi-env-))
  "Compile-time environment for FFI declarations."
  (imports (make-hash-table :test 'equal) :type hash-table)  ; lisp-name -> ForeignFunctionDecl
  (exports nil :type list)                                    ; List of ExportDecl
  (next-import-func-index 0 :type fixnum)                    ; Next available import function index
  (type-cache (make-hash-table :test 'equal) :type hash-table)) ; Signature -> type index

(defun register-foreign-function (env decl)
  "Register a ForeignFunctionDecl in the FFI environment.
   Validates the declaration and assigns an import function index."
  (validate-foreign-function-decl decl)
  (let ((key (ffd-lisp-name decl)))
    (when (gethash key (ffi-env-imports env))
      (warn "Redefining foreign function: ~A" key))
    (setf (gethash key (ffi-env-imports env)) decl)
    (incf (ffi-env-next-import-func-index env))
    decl))

(defun register-export (env decl)
  "Register an ExportDecl in the FFI environment.
   Validates the declaration and adds it to the exports list."
  (validate-export-decl decl)
  (push decl (ffi-env-exports env))
  decl)

(defun lookup-foreign-function (env lisp-name)
  "Look up a foreign function declaration by its Lisp name."
  (gethash lisp-name (ffi-env-imports env)))

;;; ============================================================
;;; T011: WasmImport Structure
;;; ============================================================

(defstruct (wasm-import (:conc-name wi-))
  "Wasm import entry."
  (module-name nil :type (or null string))  ; Import module name
  (field-name nil :type (or null string))   ; Import field name
  (kind nil :type (or null (member :func :table :memory :global)))
  (type-index nil :type (or null fixnum))   ; For :func kind
  (table-type nil)                           ; For :table kind
  (memory-type nil)                          ; For :memory kind
  (global-type nil))                         ; For :global kind

;;; ============================================================
;;; T019: Host Name Parsing
;;; ============================================================

(defun parse-host-name (host-name)
  "Parse a host name string into module and field components.
   'host.log' -> (values \"host\" \"log\")
   'env.console.log' -> (values \"env\" \"console.log\")
   'log' -> (values \"env\" \"log\")  ; Default module is 'env'"
  (let ((dot-pos (position #\. host-name)))
    (if dot-pos
        (values (subseq host-name 0 dot-pos)
                (subseq host-name (1+ dot-pos)))
        (values "env" host-name))))

;;; ============================================================
;;; T014: FFI Conditions
;;; ============================================================

(define-condition ffi-host-error (error)
  ((function-name :initarg :function-name
                  :reader ffi-host-error-function-name
                  :type string
                  :documentation "The name of the host function that failed")
   (message :initarg :message
            :reader ffi-host-error-message
            :type string
            :documentation "Error message from the host"))
  (:report (lambda (c s)
             (format s "FFI call to ~A failed: ~A"
                     (ffi-host-error-function-name c)
                     (ffi-host-error-message c))))
  (:documentation "Condition signaled when a host function call fails."))

(define-condition ffi-type-error (error)
  ((expected-type :initarg :expected-type
                  :reader ffi-type-error-expected-type
                  :documentation "The expected marshal type")
   (actual-value :initarg :actual-value
                 :reader ffi-type-error-actual-value
                 :documentation "The actual value that couldn't be marshalled"))
  (:report (lambda (c s)
             (format s "FFI type error: expected ~A but got ~A"
                     (ffi-type-error-expected-type c)
                     (ffi-type-error-actual-value c))))
  (:documentation "Condition signaled when a value cannot be marshalled to the expected type."))
