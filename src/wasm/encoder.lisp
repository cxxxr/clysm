;;;; encoder.lisp - WebAssembly binary encoder

(in-package #:clysm/wasm)

;;; Section Encoding

(defun encode-section (buffer section-id content-fn)
  "Encode a section with its ID and content.
CONTENT-FN is called with a temporary buffer to generate content."
  (let ((content-buffer (make-byte-buffer)))
    (funcall content-fn content-buffer)
    (let ((content (buffer-contents content-buffer)))
      (when (plusp (length content))
        (buffer-write-byte buffer section-id)
        (buffer-write-uleb128 buffer (length content))
        (buffer-write-bytes buffer content)))))

;;; Type Section

(defun encode-type-section (buffer types)
  "Encode the type section."
  (encode-section buffer +section-type+
    (lambda (buf)
      (buffer-write-uleb128 buf (length types))
      (dolist (type types)
        (encode-type buf type)))))

(defun encode-type (buffer type)
  "Encode a single type definition."
  (etypecase type
    (func-type
     (buffer-write-byte buffer +type-func+)
     (encode-result-type buffer (func-type-params type))
     (encode-result-type buffer (func-type-results type)))
    (gc-struct-type
     (encode-gc-struct-type buffer type))
    (gc-array-type
     (encode-gc-array-type buffer type))))

(defun encode-result-type (buffer types)
  "Encode a result type (vector of value types)."
  (buffer-write-uleb128 buffer (length types))
  (dolist (type types)
    (buffer-write-byte buffer type)))

;;; Import Section

(defun encode-import-section (buffer imports)
  "Encode the import section."
  (when imports
    (encode-section buffer +section-import+
      (lambda (buf)
        (buffer-write-uleb128 buf (length imports))
        (dolist (import imports)
          (encode-import buf import))))))

(defun encode-import (buffer import)
  "Encode a single import."
  (buffer-write-name buffer (wasm-import-module import))
  (buffer-write-name buffer (wasm-import-name import))
  (buffer-write-byte buffer (wasm-import-kind import))
  (ecase (wasm-import-kind import)
    (#.+import-func+
     (buffer-write-uleb128 buffer (wasm-import-desc import)))
    (#.+import-table+
     (encode-table-type buffer (wasm-import-desc import)))
    (#.+import-memory+
     (encode-limits buffer (wasm-import-desc import)))
    (#.+import-global+
     (encode-global-type buffer (wasm-import-desc import)))))

;;; Function Section

(defun encode-function-section (buffer functions)
  "Encode the function section (type indices only)."
  (when functions
    (encode-section buffer +section-function+
      (lambda (buf)
        (buffer-write-uleb128 buf (length functions))
        (dolist (func functions)
          (buffer-write-uleb128 buf (wasm-func-type-idx func)))))))

;;; Table Section

(defun encode-table-section (buffer tables)
  "Encode the table section."
  (when tables
    (encode-section buffer +section-table+
      (lambda (buf)
        (buffer-write-uleb128 buf (length tables))
        (dolist (table tables)
          (encode-table-type buf table))))))

(defun encode-table-type (buffer table)
  "Encode a table type."
  (buffer-write-byte buffer (wasm-table-element-type table))
  (encode-limits buffer table))

;;; Memory Section

(defun encode-memory-section (buffer memories)
  "Encode the memory section."
  (when memories
    (encode-section buffer +section-memory+
      (lambda (buf)
        (buffer-write-uleb128 buf (length memories))
        (dolist (mem memories)
          (encode-limits buf mem))))))

(defun encode-limits (buffer obj)
  "Encode limits (min, optional max)."
  (let ((min (etypecase obj
               (wasm-memory (wasm-memory-min obj))
               (wasm-table (wasm-table-min obj))))
        (max (etypecase obj
               (wasm-memory (wasm-memory-max obj))
               (wasm-table (wasm-table-max obj)))))
    (if max
        (progn
          (buffer-write-byte buffer +limits-min-max+)
          (buffer-write-uleb128 buffer min)
          (buffer-write-uleb128 buffer max))
        (progn
          (buffer-write-byte buffer +limits-min-only+)
          (buffer-write-uleb128 buffer min)))))

;;; Global Section

(defun encode-global-section (buffer globals)
  "Encode the global section."
  (when globals
    (encode-section buffer +section-global+
      (lambda (buf)
        (buffer-write-uleb128 buf (length globals))
        (dolist (global globals)
          (encode-global buf global))))))

(defun encode-global (buffer global)
  "Encode a single global."
  (encode-global-type buffer global)
  (encode-init-expr buffer (wasm-global-init global)))

(defun encode-global-type (buffer global)
  "Encode a global type (value type + mutability)."
  (buffer-write-byte buffer (wasm-global-type global))
  (buffer-write-byte buffer (if (wasm-global-mutable global) +var+ +const+)))

(defun encode-init-expr (buffer expr)
  "Encode an initialization expression."
  (dolist (instr expr)
    (encode-instr buffer instr))
  (buffer-write-byte buffer +op-end+))

;;; Export Section

(defun encode-export-section (buffer exports)
  "Encode the export section."
  (when exports
    (encode-section buffer +section-export+
      (lambda (buf)
        (buffer-write-uleb128 buf (length exports))
        (dolist (export exports)
          (encode-export buf export))))))

(defun encode-export (buffer export)
  "Encode a single export."
  (buffer-write-name buffer (wasm-export-name export))
  (buffer-write-byte buffer (wasm-export-kind export))
  (buffer-write-uleb128 buffer (wasm-export-idx export)))

;;; Start Section

(defun encode-start-section (buffer start-idx)
  "Encode the start section."
  (when start-idx
    (encode-section buffer +section-start+
      (lambda (buf)
        (buffer-write-uleb128 buf start-idx)))))

;;; Element Section

(defun encode-element-section (buffer elements)
  "Encode the element section."
  (when elements
    (encode-section buffer +section-element+
      (lambda (buf)
        (buffer-write-uleb128 buf (length elements))
        (dolist (elem elements)
          (encode-element buf elem))))))

(defun encode-element (buffer elem)
  "Encode a single element segment.
   MVP format: table_idx, offset expr, vec of func indices."
  ;; Table index (always 0 for MVP, but can be specified)
  (buffer-write-uleb128 buffer (wasm-element-table-idx elem))
  ;; Offset expression
  (encode-init-expr buffer (wasm-element-offset elem))
  ;; Function indices
  (let ((indices (wasm-element-func-indices elem)))
    (buffer-write-uleb128 buffer (length indices))
    (dolist (idx indices)
      (buffer-write-uleb128 buffer idx))))

;;; Code Section

(defun encode-code-section (buffer functions)
  "Encode the code section."
  (when functions
    (encode-section buffer +section-code+
      (lambda (buf)
        (buffer-write-uleb128 buf (length functions))
        (dolist (func functions)
          (encode-func-body buf func))))))

;;; Data Section

(defun encode-data-section (buffer data-segments)
  "Encode the data section."
  (when data-segments
    (encode-section buffer +section-data+
      (lambda (buf)
        (buffer-write-uleb128 buf (length data-segments))
        (dolist (segment data-segments)
          (encode-data-segment buf segment))))))

(defun encode-data-segment (buffer segment)
  "Encode a single data segment.
   Active segment format: 0x00, offset expr, vec(byte)"
  ;; Segment flags: 0 = active segment with memory 0
  (buffer-write-uleb128 buffer 0)
  ;; Offset expression
  (encode-init-expr buffer (wasm-data-offset segment))
  ;; Data bytes
  (let ((data (wasm-data-data segment)))
    (buffer-write-uleb128 buffer (length data))
    (buffer-write-bytes buffer data)))

(defun encode-func-body (buffer func)
  "Encode a function body."
  (let ((body-buffer (make-byte-buffer)))
    ;; Encode locals
    (encode-locals body-buffer (wasm-func-locals func))
    ;; Encode body instructions
    (dolist (instr (wasm-func-body func))
      (encode-instr body-buffer instr))
    ;; End marker
    (buffer-write-byte body-buffer +op-end+)
    ;; Write size and content
    (let ((body (buffer-contents body-buffer)))
      (buffer-write-uleb128 buffer (length body))
      (buffer-write-bytes buffer body))))

(defun encode-locals (buffer locals)
  "Encode local variable declarations.
LOCALS is a list of (count . type) pairs."
  (buffer-write-uleb128 buffer (length locals))
  (dolist (local locals)
    (buffer-write-uleb128 buffer (car local))  ; count
    (buffer-write-byte buffer (cdr local))))   ; type

;;; Instruction Encoding

(defun encode-instr (buffer instr)
  "Encode a single instruction.
INSTR is either an opcode byte or a list (opcode . args)."
  (etypecase instr
    ((unsigned-byte 8)
     (buffer-write-byte buffer instr))
    (cons
     (let ((opcode (car instr))
           (args (cdr instr)))
       (buffer-write-byte buffer opcode)
       (dolist (arg args)
         (encode-instr-arg buffer arg opcode))))))

(defun encode-instr-arg (buffer arg opcode)
  "Encode an instruction argument based on the opcode."
  (cond
    ;; Block type
    ((member opcode (list +op-block+ +op-loop+ +op-if+))
     (encode-block-type buffer arg))
    ;; Signed immediates (const)
    ((member opcode (list +op-i32-const+ +op-i64-const+))
     (buffer-write-sleb128 buffer arg))
    ;; Float immediates
    ((eql opcode +op-f32-const+)
     (buffer-write-f32 buffer arg))
    ((eql opcode +op-f64-const+)
     (buffer-write-f64 buffer arg))
    ;; Memory args (align, offset)
    ((and (>= opcode #x28) (<= opcode #x3e))
     (buffer-write-uleb128 buffer arg))
    ;; All other args as unsigned LEB128
    (t
     (buffer-write-uleb128 buffer arg))))

(defun encode-block-type (buffer type)
  "Encode a block type."
  (etypecase type
    ;; Empty block (void)
    (null
     (buffer-write-byte buffer +type-void+))
    ;; Single value type
    ((unsigned-byte 8)
     (buffer-write-byte buffer type))
    ;; Type index (for multi-value)
    (integer
     (buffer-write-sleb128 buffer type))))

;;; Module Encoding

(defun encode-module (module)
  "Encode a complete WASM module to a byte vector."
  (let ((buffer (make-byte-buffer)))
    ;; Magic number
    (buffer-write-u32 buffer +wasm-magic+)
    ;; Version
    (buffer-write-u32 buffer +wasm-version+)
    ;; Sections (must be in order)
    (encode-type-section buffer (wasm-module-types module))
    (encode-import-section buffer (wasm-module-imports module))
    (encode-function-section buffer (wasm-module-functions module))
    (encode-table-section buffer (wasm-module-tables module))
    (encode-memory-section buffer (wasm-module-memories module))
    (encode-global-section buffer (wasm-module-globals module))
    (encode-export-section buffer (wasm-module-exports module))
    (encode-start-section buffer (wasm-module-start module))
    (encode-element-section buffer (wasm-module-elements module))
    (encode-code-section buffer (wasm-module-functions module))
    (encode-data-section buffer (wasm-module-data module))
    (buffer-contents buffer)))

;;; Convenience Functions

(defun save-module (module filename)
  "Save a WASM module to a file."
  (let ((bytes (encode-module module)))
    (with-open-file (out filename
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    filename))
