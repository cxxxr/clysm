;;;; ir.lisp - Wasm IR generator for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements T026: Wasm IR generator (cross-compiled)
;;;;
;;;; This generates Wasm IR from AST nodes.

(in-package #:clysm/stage0)

;;; ============================================================
;;; Wasm Instruction Opcodes
;;; ============================================================

(defparameter *wasm-opcodes*
  (alexandria:plist-hash-table
   '(;; Control
     :unreachable #x00
     :nop #x01
     :block #x02
     :loop #x03
     :if #x04
     :else #x05
     :end #x0B
     :br #x0C
     :br_if #x0D
     :br_table #x0E
     :return #x0F
     :call #x10
     :call_indirect #x11

     ;; Reference
     :ref.null #xD0
     :ref.is_null #xD1
     :ref.func #xD2
     :ref.eq #xD3

     ;; Parametric
     :drop #x1A
     :select #x1B

     ;; Variable
     :local.get #x20
     :local.set #x21
     :local.tee #x22
     :global.get #x23
     :global.set #x24

     ;; Memory (not used - WasmGC only)
     ;; :i32.load #x28

     ;; Numeric - i32
     :i32.const #x41
     :i32.eqz #x45
     :i32.eq #x46
     :i32.ne #x47
     :i32.lt_s #x48
     :i32.lt_u #x49
     :i32.gt_s #x4A
     :i32.gt_u #x4B
     :i32.le_s #x4C
     :i32.le_u #x4D
     :i32.ge_s #x4E
     :i32.ge_u #x4F
     :i32.add #x6A
     :i32.sub #x6B
     :i32.mul #x6C
     :i32.div_s #x6D
     :i32.div_u #x6E
     :i32.rem_s #x6F
     :i32.rem_u #x70

     ;; Numeric - i64
     :i64.const #x42
     :i64.add #x7C
     :i64.sub #x7D
     :i64.mul #x7E

     ;; Numeric - f64
     :f64.const #x44
     :f64.eq #x61
     :f64.ne #x62
     :f64.lt #x63
     :f64.gt #x64
     :f64.le #x65
     :f64.ge #x66
     :f64.add #xA0
     :f64.sub #xA1
     :f64.mul #xA2
     :f64.div #xA3))
  "Wasm opcode table")

;; GC instructions (prefixed with 0xFB)
(defparameter *wasm-gc-opcodes*
  (alexandria:plist-hash-table
   '(:struct.new #x00
     :struct.new_default #x01
     :struct.get #x02
     :struct.get_s #x03
     :struct.get_u #x04
     :struct.set #x05
     :array.new #x06
     :array.new_default #x07
     :array.new_fixed #x08
     :array.get #x0B
     :array.get_s #x0C
     :array.get_u #x0D
     :array.set #x0E
     :array.len #x0F
     :ref.cast #x17
     :ref.test #x15
     :ref.i31 #x1C
     :i31.get_s #x1D
     :i31.get_u #x1E))
  "WasmGC opcode table (prefixed with 0xFB)")

;;; ============================================================
;;; IR Generation
;;; ============================================================

(defun generate-wasm-ir (ir-form)
  "Generate Wasm instruction list from IR form"
  (cond
    ;; Literal
    ((and (consp ir-form) (eq :literal (first ir-form)))
     (generate-literal-ir (second ir-form)))

    ;; Variable reference
    ((and (consp ir-form) (eq :var-ref (first ir-form)))
     (generate-var-ref-ir (second ir-form)))

    ;; Function call
    ((and (consp ir-form) (eq :call (first ir-form)))
     (generate-call-ir (second ir-form) (third ir-form)))

    ;; If expression
    ((and (consp ir-form) (eq :if (first ir-form)))
     (generate-if-ir (second ir-form) (third ir-form) (fourth ir-form)))

    ;; Lambda
    ((and (consp ir-form) (eq :lambda (first ir-form)))
     (generate-lambda-ir (second ir-form) (third ir-form)))

    ;; Let binding
    ((and (consp ir-form) (eq :let (first ir-form)))
     (generate-let-ir (second ir-form) (third ir-form)))

    ;; Defun
    ((and (consp ir-form) (eq :defun (first ir-form)))
     (generate-defun-ir (second ir-form) (third ir-form) (fourth ir-form)))

    (t (error "Unknown IR form: ~S" ir-form))))

(defun generate-literal-ir (value)
  "Generate IR for literal value"
  (cond
    ((null value)
     ;; NIL - reference global 0
     '((:global.get 0)))

    ((integerp value)
     (if (<= -1073741824 value 1073741823)  ; i31 range
         `((:i32.const ,value) (:ref.i31))
         ;; Would need bignum - not supported in Stage 0
         (error "Integer out of i31 range: ~D" value)))

    ((floatp value)
     `((:f64.const ,value)
       (:struct.new ,+type-float+)))

    ((stringp value)
     ;; String literal - would need string table
     `((:string-literal ,value)))

    ((characterp value)
     `((:i32.const ,(char-code value)) (:ref.i31)))

    ((keywordp value)
     ;; Keyword - symbol lookup
     `((:keyword ,value)))

    (t (error "Cannot generate literal for: ~S" value))))

(defun generate-var-ref-ir (name)
  "Generate IR for variable reference"
  ;; Would need environment lookup
  `((:local.get ,name)))

(defun generate-call-ir (operator args)
  "Generate IR for function call"
  (let ((arg-irs (mapcan #'generate-wasm-ir args)))
    ;; Check for primitive operations
    (cond
      ((and (consp operator) (eq :var-ref (first operator)))
       (let ((op-name (second operator)))
         (case op-name
           (+ (append arg-irs '((:call $+))))
           (- (append arg-irs '((:call $-))))
           (* (append arg-irs '((:call $*))))
           (/ (append arg-irs '((:call $/))))
           (t (append arg-irs `((:call ,op-name)))))))
      (t
       ;; General function call
       (append arg-irs
               (generate-wasm-ir operator)
               '((:call_ref)))))))

(defun generate-if-ir (test then else)
  "Generate IR for if expression"
  (let ((test-ir (generate-wasm-ir test))
        (then-ir (generate-wasm-ir then))
        (else-ir (when else (generate-wasm-ir else))))
    (append test-ir
            `((:if :anyref))
            then-ir
            (when else-ir
              (cons '(:else) else-ir))
            '((:end)))))

(defun generate-lambda-ir (params body)
  "Generate IR for lambda expression"
  (declare (ignore params body))
  ;; Lambda needs closure creation
  '((:closure-create)))

(defun generate-let-ir (bindings body)
  "Generate IR for let binding"
  (let ((binding-irs
          (mapcan (lambda (b)
                    (append (generate-wasm-ir (second b))
                            `((:local.set ,(first b)))))
                  bindings))
        (body-irs (mapcan #'generate-wasm-ir body)))
    (append binding-irs body-irs)))

(defun generate-defun-ir (name params body)
  "Generate IR for function definition"
  (declare (ignore params))
  (let ((body-irs (mapcan #'generate-wasm-ir body)))
    `((:func ,name) ,@body-irs (:end))))

;;; ============================================================
;;; IR to Instructions
;;; ============================================================

(defun ir-to-instructions (ir)
  "Convert IR to Wasm byte sequence"
  (let ((bytes '()))
    (dolist (instr ir)
      (let ((op (first instr)))
        (cond
          ;; Regular opcode
          ((gethash op *wasm-opcodes*)
           (push (gethash op *wasm-opcodes*) bytes)
           ;; Add operands
           (dolist (operand (rest instr))
             (when (integerp operand)
               (dolist (b (encode-signed-leb128 operand))
                 (push b bytes)))))

          ;; GC opcode
          ((gethash op *wasm-gc-opcodes*)
           (push +gc-prefix+ bytes)
           (push (gethash op *wasm-gc-opcodes*) bytes)
           ;; Add operands
           (dolist (operand (rest instr))
             (when (integerp operand)
               (dolist (b (encode-unsigned-leb128 operand))
                 (push b bytes)))))

          ;; Unknown - skip with warning
          (t nil))))
    (nreverse bytes)))
