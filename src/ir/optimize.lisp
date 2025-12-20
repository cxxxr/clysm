;;;; optimize.lisp - IR optimization passes

(in-package #:clysm/ir)

;;; IR Optimization

(defgeneric optimize-ir (node)
  (:documentation "Optimize an IR node."))

(defmethod optimize-ir ((node ir-const))
  node)

(defmethod optimize-ir ((node ir-local-ref))
  node)

(defmethod optimize-ir ((node ir-local-set))
  (make-ir-local-set
   (ir-local-set-index node)
   (optimize-ir (ir-local-set-value node))))

(defmethod optimize-ir ((node ir-if))
  (let ((test (optimize-ir (ir-if-test node))))
    ;; Constant folding for if
    (if (typep test 'ir-const)
        (if (ir-const-value test)
            (optimize-ir (ir-if-then node))
            (optimize-ir (ir-if-else node)))
        (make-ir-if
         test
         (optimize-ir (ir-if-then node))
         (optimize-ir (ir-if-else node))))))

(defmethod optimize-ir ((node ir-seq))
  (let ((forms (mapcar #'optimize-ir (ir-seq-forms node))))
    ;; Flatten nested sequences
    (let ((flattened
            (loop for form in forms
                  if (typep form 'ir-seq)
                    append (ir-seq-forms form)
                  else
                    collect form)))
      (if (= 1 (length flattened))
          (first flattened)
          (make-ir-seq flattened)))))

(defmethod optimize-ir ((node ir-primop))
  (let ((args (mapcar #'optimize-ir (ir-primop-args node))))
    ;; Constant folding for arithmetic
    (if (every (lambda (a) (typep a 'ir-const)) args)
        (let ((values (mapcar #'ir-const-value args)))
          (make-ir-const
           (apply (ir-primop-op node) values)))
        (make-ir-primop (ir-primop-op node) args))))

(defmethod optimize-ir ((node ir-call))
  (make-ir-call
   (optimize-ir (ir-call-func node))
   (mapcar #'optimize-ir (ir-call-args node))))

(defmethod optimize-ir ((node ir-return))
  (make-ir-return (optimize-ir (ir-return-value node))))

(defmethod optimize-ir ((node ir-block))
  (make-ir-block
   (ir-block-label node)
   (mapcar #'optimize-ir (ir-block-body node))))

(defmethod optimize-ir ((node ir-loop))
  (make-ir-loop
   (ir-loop-label node)
   (mapcar #'optimize-ir (ir-loop-body node))))

(defmethod optimize-ir ((node ir-br))
  (if (ir-br-condition node)
      (make-ir-br (ir-br-label node)
                  (optimize-ir (ir-br-condition node)))
      node))
