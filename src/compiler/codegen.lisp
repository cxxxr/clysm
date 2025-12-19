;;;; codegen.lisp - WASM code generation from IR

(in-package #:cl-wasm/compiler)

;;; Code Generation from IR

(defgeneric generate-code (node)
  (:documentation "Generate WASM instructions from an IR node."))

(defmethod generate-code ((node ir-const))
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

(defmethod generate-code ((node ir-local-ref))
  `((,+op-local-get+ ,(ir-local-index node))))

(defmethod generate-code ((node ir-local-set))
  `(,@(generate-code (ir-local-set-value node))
    (,+op-local-set+ ,(ir-local-set-index node))))

(defmethod generate-code ((node ir-if))
  (let ((test-code (generate-code (ir-if-test node)))
        (then-code (generate-code (ir-if-then node)))
        (else-code (generate-code (ir-if-else node))))
    `(,@test-code
      (,+op-if+ ,+type-i32+)
      ,@then-code
      ,+op-else+
      ,@else-code
      ,+op-end+)))

(defmethod generate-code ((node ir-seq))
  (let ((code nil))
    (loop for (form . rest) on (ir-seq-forms node)
          do (setf code (append code (generate-code form)))
             (when rest
               (setf code (append code `(,+op-drop+)))))
    code))

(defmethod generate-code ((node ir-primop))
  (let ((op (ir-primop-op node))
        (args (ir-primop-args node)))
    (let ((arg-code (reduce #'append (mapcar #'generate-code args)
                            :initial-value nil)))
      (append arg-code
              (list (primop-to-wasm-op op (length args)))))))

(defmethod generate-code ((node ir-call))
  (let ((func-code (generate-code (ir-call-func node)))
        (arg-codes (mapcar #'generate-code (ir-call-args node))))
    `(,@(reduce #'append arg-codes :initial-value nil)
      ,@func-code
      ;; TODO: call_ref for indirect calls
      )))

(defmethod generate-code ((node ir-return))
  `(,@(generate-code (ir-return-value node))
    ,+op-return+))

(defmethod generate-code ((node ir-block))
  `((,+op-block+ ,(ir-node-type node))
    ,@(reduce #'append (mapcar #'generate-code (ir-block-body node))
              :initial-value nil)
    ,+op-end+))

(defmethod generate-code ((node ir-loop))
  `((,+op-loop+ ,+type-void+)
    ,@(reduce #'append (mapcar #'generate-code (ir-loop-body node))
              :initial-value nil)
    ,+op-end+))

(defmethod generate-code ((node ir-br))
  (if (ir-br-condition node)
      `(,@(generate-code (ir-br-condition node))
        (,+op-br-if+ ,(ir-br-label node)))
      `((,+op-br+ ,(ir-br-label node)))))

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
