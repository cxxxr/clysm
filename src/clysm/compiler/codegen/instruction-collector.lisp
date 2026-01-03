;;;; Instruction Collector Macro
;;;; Provides O(n) instruction collection for Wasm code generation
;;;; Replaces O(n²) append-based patterns with push+nreverse

(in-package #:clysm/compiler/codegen/func-section)

(defmacro with-instruction-collector (&body body)
  "Establish a lexical scope for collecting Wasm instructions.

   Within the body, two local macros are available:
   - (emit opcode &rest operands) - Add a single instruction
   - (emit* instructions) - Add all instructions from a list

   Returns the collected instructions in order.

   The emit macro intelligently handles Wasm instruction formats:
   - (emit :i32.add) => bare keyword :i32.add (simple instruction)
   - (emit :i32.const 42) => list (:i32.const 42) (compound instruction)

   Example:
     (with-instruction-collector
       (emit :i32.const 42)
       (emit* (compile-to-instructions expr env))
       (emit :i32.add))
     => ((:i32.const 42) ... :i32.add)

   Performance: O(n) where n is total instructions collected.
   Uses push during collection and nreverse at completion."
  (let ((acc (gensym "INSTRUCTIONS"))
        (instr-var (gensym "INSTR"))
        ;; Use intern to create emit/emit* in the caller's package
        ;; This ensures macrolet defines the same symbols used in body
        (emit-sym (intern "EMIT"))
        (emit*-sym (intern "EMIT*")))
    `(let ((,acc '()))
       (macrolet ((,emit-sym (&rest instruction-parts)
                    ;; Smart instruction format handling:
                    ;; - Single keyword: emit bare keyword (simple instruction)
                    ;; - Multiple parts: emit as list (compound instruction)
                    (if (= (length instruction-parts) 1)
                        `(push ,(car instruction-parts) ,',acc)
                        `(push (list ,@instruction-parts) ,',acc)))
                  (,emit*-sym (list-form)
                    ;; Push all instructions from a list
                    ;; Use gensym for iteration variable to avoid capture
                    `(dolist (,',instr-var ,list-form)
                       (push ,',instr-var ,',acc))))
         ,@body
         (nreverse ,acc)))))
