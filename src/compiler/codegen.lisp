;;;; codegen.lisp - WASM code generation from IR
;;;; Converted from CLOS generic functions to defstruct-based dispatch

(in-package #:clysm/compiler)

;;; Code Generation from IR

(defun generate-code (node)
  "Generate WASM instructions from an IR node."
  (etypecase node
    (ir-const
     (let ((value (ir-const-value node)))
       (cond
         ((null value)
          `((,+op-i32-const+ 0)))
         ((integerp value)
          `((,+op-i32-const+ ,value)))
         ((floatp value)
          `((,+op-f64-const+ ,(float value 1.0d0))))
         (t
          (error "Cannot generate code for constant: ~A" value)))))
    (ir-local-ref
     `((,+op-local-get+ ,(ir-local-index node))))
    (ir-local-set
     `(,@(generate-code (ir-local-set-value node))
       (,+op-local-set+ ,(ir-local-set-index node))))
    (ir-if
     (let ((test-code (generate-code (ir-if-test node)))
           (then-code (generate-code (ir-if-then node)))
           (else-code (generate-code (ir-if-else node))))
       `(,@test-code
         (,+op-if+ ,+type-i32+)
         ,@then-code
         ,+op-else+
         ,@else-code
         ,+op-end+)))
    (ir-seq
     (let ((code nil))
       (loop for (form . rest) on (ir-seq-forms node)
             do (setf code (append code (generate-code form)))
                (when rest
                  (setf code (append code `(,+op-drop+)))))
       code))
    (ir-primop
     (let ((op (ir-primop-op node))
           (args (ir-primop-args node)))
       (let ((arg-code (reduce #'append (mapcar #'generate-code args)
                               :initial-value nil)))
         (append arg-code
                 (list (primop-to-wasm-op op (length args)))))))
    (ir-call
     (let ((func-code (generate-code (ir-call-func node)))
           (arg-codes (mapcar #'generate-code (ir-call-args node))))
       `(,@(reduce #'append arg-codes :initial-value nil)
         ,@func-code
         ;; TODO: call_ref for indirect calls
         )))
    (ir-return
     `(,@(generate-code (ir-return-value node))
       ,+op-return+))
    (ir-block
     `((,+op-block+ ,(ir-node-type node))
       ,@(reduce #'append (mapcar #'generate-code (ir-block-body node))
                 :initial-value nil)
       ,+op-end+))
    (ir-loop
     `((,+op-loop+ ,+type-void+)
       ,@(reduce #'append (mapcar #'generate-code (ir-loop-body node))
                 :initial-value nil)
       ,+op-end+))
    (ir-br
     (if (ir-br-condition node)
         `(,@(generate-code (ir-br-condition node))
           (,+op-br-if+ ,(ir-br-label node)))
         `((,+op-br+ ,(ir-br-label node)))))))

;;; Primitive to WASM opcode mapping

(defun primop-to-wasm-op (op arg-count)
  "Map a Lisp primitive operation to WASM opcode."
  (declare (ignore arg-count))
  (case op
    (+ +op-i32-add+)
    (- +op-i32-sub+)
    (* +op-i32-mul+)
    (/ +op-i32-div-s+)
    (mod +op-i32-rem-s+)
    (rem +op-i32-rem-s+)
    (< +op-i32-lt-s+)
    (> +op-i32-gt-s+)
    (<= +op-i32-le-s+)
    (>= +op-i32-ge-s+)
    (= +op-i32-eq+)
    (/= +op-i32-ne+)
    (logand +op-i32-and+)
    (logior +op-i32-or+)
    (logxor +op-i32-xor+)
    (ash +op-i32-shl+)
    (not +op-i32-eqz+)
    (null +op-i32-eqz+)
    (t (error "Unknown primitive: ~A" op))))
