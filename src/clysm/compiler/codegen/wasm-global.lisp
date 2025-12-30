;;;; wasm-global.lisp - Wasm global section helpers
;;;;
;;;; Part of Phase 13D-4: Global Variable Definitions
;;;; Provides low-level Wasm global section encoding
;;;;
;;;; See: https://webassembly.github.io/gc/core/binary/modules.html#global-section

(in-package #:clysm/compiler/codegen/wasm-global)

;;; ============================================================
;;; Wasm Global Type Encoding
;;; ============================================================

;; Type encodings for global values
(defconstant +wasm-ref-null+ #x63
  "Wasm ref type constructor (ref null)")

(defconstant +wasm-any+ #x6E
  "Wasm any type (anyref)")

(defconstant +wasm-i31+ #x6C
  "Wasm i31ref type")

(defconstant +wasm-global-mutable+ #x01
  "Wasm global mutability: mutable")

(defconstant +wasm-global-immutable+ #x00
  "Wasm global mutability: immutable")

;;; ============================================================
;;; Global Type Encoding Functions
;;; ============================================================

(defun encode-ref-null-any-type ()
  "Encode (ref null any) type for special variables.
   Returns list of bytes."
  (list +wasm-ref-null+ +wasm-any+))

(defun encode-global-type (valtype-bytes mutability)
  "Encode a global type (valtype + mutability).
   VALTYPE-BYTES: List of bytes for value type
   MUTABILITY: :mutable or :immutable
   Returns list of bytes."
  (append valtype-bytes
          (list (if (eq mutability :mutable)
                    +wasm-global-mutable+
                    +wasm-global-immutable+))))

;;; ============================================================
;;; Init Expression Encoding
;;; ============================================================

;; Wasm instruction opcodes
(defconstant +wasm-end+ #x0B)
(defconstant +wasm-i32-const+ #x41)
(defconstant +wasm-global-get+ #x23)
(defconstant +wasm-ref-null-instr+ #xD0)
(defconstant +wasm-gc-prefix+ #xFB)
(defconstant +wasm-ref-i31+ #x1C)

(defun encode-ref-null-any-init ()
  "Encode ref.null any init expression.
   Returns list of bytes."
  (list +wasm-ref-null-instr+ +wasm-any+ +wasm-end+))

(defun encode-global-get-init (global-idx)
  "Encode global.get init expression.
   Returns list of bytes."
  (append (list +wasm-global-get+)
          (clysm/backend/leb128:encode-unsigned-leb128 global-idx)
          (list +wasm-end+)))

(defun encode-i31-const-init (value)
  "Encode i32.const + ref.i31 init expression for fixnum.
   Returns list of bytes."
  (append (list +wasm-i32-const+)
          (clysm/backend/leb128:encode-signed-leb128 value)
          (list +wasm-gc-prefix+ +wasm-ref-i31+)
          (list +wasm-end+)))

;;; ============================================================
;;; Global Entry Encoding
;;; ============================================================

(defun encode-global-entry (global-spec)
  "Encode a single global entry.
   GLOBAL-SPEC is plist with :type, :mutable, :init keys.
   Returns list of bytes."
  (let* ((type-bytes (encode-ref-null-any-type))
         (mutable-p (getf global-spec :mutable t))
         (init-instrs (getf global-spec :init))
         (type-with-mut (encode-global-type type-bytes
                                            (if mutable-p :mutable :immutable)))
         (init-bytes (encode-init-instructions init-instrs)))
    (append type-with-mut init-bytes)))

(defun encode-init-instructions (instrs)
  "Encode init expression instructions to bytes.
   INSTRS is list of instruction forms like ((i32.const 5) (ref.i31)).
   Returns list of bytes."
  (let ((bytes '()))
    (dolist (instr instrs)
      (setf bytes (append bytes (encode-single-init-instruction instr))))
    ;; Add end marker
    (append bytes (list +wasm-end+))))

(defun encode-single-init-instruction (instr)
  "Encode a single init instruction.
   Returns list of bytes (without end marker)."
  (case (first instr)
    (global.get
     (cons +wasm-global-get+
           (clysm/backend/leb128:encode-unsigned-leb128 (second instr))))
    (i32.const
     (cons +wasm-i32-const+
           (clysm/backend/leb128:encode-signed-leb128 (second instr))))
    (ref.i31
     (list +wasm-gc-prefix+ +wasm-ref-i31+))
    (ref.null
     (list +wasm-ref-null-instr+ +wasm-any+))
    (t
     (error "Unknown init instruction: ~S" instr))))

;;; ============================================================
;;; Special Variable Global Generation
;;; ============================================================

(defun encode-special-var-global (name init-type init-value)
  "Encode a special variable global.
   NAME: Symbol name (for debugging)
   INIT-TYPE: :constant, :deferred, or :none
   INIT-VALUE: Init value for constant, or NIL
   Returns list of bytes for global entry."
  (let ((type-bytes (encode-ref-null-any-type))
        (init-bytes (case init-type
                      (:constant
                       (cond
                         ;; Integer constant
                         ((integerp init-value)
                          (encode-i31-const-init init-value))
                         ;; NIL constant
                         ((null init-value)
                          (encode-global-get-init 0))  ; Global 0 is NIL
                         ;; Default to null
                         (t (encode-ref-null-any-init))))
                      (:deferred
                       (encode-ref-null-any-init))
                      (:none
                       (encode-global-get-init 1))  ; Global 1 is UNBOUND
                      (t
                       (encode-ref-null-any-init)))))
    (append type-bytes
            (list +wasm-global-mutable+)
            init-bytes)))

;;; ============================================================
;;; Global Section Assembly
;;; ============================================================

(defconstant +wasm-global-section-id+ 6
  "Wasm global section ID")

(defun assemble-global-section (base-globals special-globals)
  "Assemble complete global section.
   BASE-GLOBALS: List of base global entries (NIL, UNBOUND, etc.)
   SPECIAL-GLOBALS: List of special variable global entries
   Returns byte vector for global section."
  (let* ((all-globals (append base-globals special-globals))
         (global-count (length all-globals))
         (global-bytes '()))
    ;; Encode each global
    (dolist (global all-globals)
      (setf global-bytes (append global-bytes global)))
    ;; Build section
    (let* ((count-bytes (clysm/backend/leb128:encode-unsigned-leb128 global-count))
           (content-bytes (append count-bytes global-bytes))
           (content-size (length content-bytes))
           (size-bytes (clysm/backend/leb128:encode-unsigned-leb128 content-size)))
      ;; Section: ID + size + content
      (coerce (append (list +wasm-global-section-id+)
                      size-bytes
                      content-bytes)
              '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; Debug Helpers
;;; ============================================================

(defun format-global-entry (stream global-spec)
  "Format a global entry for debugging."
  (format stream "(global ~A (~A) ~A)~%"
          (getf global-spec :name "*unknown*")
          (if (getf global-spec :mutable) "mut" "")
          (getf global-spec :init)))
