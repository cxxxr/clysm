;;;; optimize.lisp - IR optimization passes
;;;; Converted from CLOS generic functions to defstruct-based dispatch

(in-package #:clysm/ir)

;;; IR Optimization

(defun optimize-ir (node)
  "Optimize an IR node."
  (etypecase node
    (ir-const
     node)
    (ir-local-ref
     node)
    (ir-local-set
     (make-ir-local-set
      :index (ir-local-set-index node)
      :value (optimize-ir (ir-local-set-value node))))
    (ir-if
     (let ((test (optimize-ir (ir-if-test node))))
       ;; Constant folding for if
       (if (ir-const-p test)
           (if (ir-const-value test)
               (optimize-ir (ir-if-then node))
               (optimize-ir (ir-if-else node)))
           (make-ir-if
            :test test
            :then (optimize-ir (ir-if-then node))
            :else (optimize-ir (ir-if-else node))))))
    (ir-seq
     (let ((forms (mapcar #'optimize-ir (ir-seq-forms node))))
       ;; Flatten nested sequences
       (let ((flattened
               (loop for form in forms
                     if (ir-seq-p form)
                       append (ir-seq-forms form)
                     else
                       collect form)))
         (if (= 1 (length flattened))
             (first flattened)
             (make-ir-seq :forms flattened)))))
    (ir-primop
     (let ((args (mapcar #'optimize-ir (ir-primop-args node))))
       ;; Constant folding for arithmetic
       (if (every #'ir-const-p args)
           (let ((values (mapcar #'ir-const-value args)))
             (make-ir-const
              :value (apply (ir-primop-op node) values)))
           (make-ir-primop :op (ir-primop-op node) :args args))))
    (ir-call
     (make-ir-call
      :func (optimize-ir (ir-call-func node))
      :args (mapcar #'optimize-ir (ir-call-args node))))
    (ir-return
     (make-ir-return :value (optimize-ir (ir-return-value node))))
    (ir-block
     (make-ir-block
      :label (ir-block-label node)
      :body (mapcar #'optimize-ir (ir-block-body node))))
    (ir-loop
     (make-ir-loop
      :label (ir-loop-label node)
      :body (mapcar #'optimize-ir (ir-loop-body node))))
    (ir-br
     (if (ir-br-condition node)
         (make-ir-br :label (ir-br-label node)
                     :condition (optimize-ir (ir-br-condition node)))
         node))))
