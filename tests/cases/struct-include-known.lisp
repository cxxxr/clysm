;;; Accessors and predicates of a structure on an object known to be of a
;;; structure including it (the compiler reads the slot through the Wasm
;;; type of the including structure, and decides the predicate), and code
;;; whose tests the types decide (constant tests are left out of the code).
(defstruct node (kind 0 :type fixnum) next)
(defstruct (leaf (:include node)) (val 0 :type fixnum))
(defstruct (pair (:include node)) left right)
(defstruct (deep (:include pair)) extra)
(defstruct (same (:include node)))      ; no slot of its own

(defun leaf-sum (x)
  ;; LEAF-VAL first: X is a LEAF for the accessors of NODE after it
  (+ (leaf-val x) (node-kind x) (if (node-p x) 100 0) (if (leaf-p x) 1000 0)))

(defun parent-then-child (x)
  ;; NODE-KIND first, then the predicate of a structure including NODE
  (let ((k (node-kind x)))
    (cond ((leaf-p x) (list :leaf k (leaf-val x) (node-next x)))
          ((deep-p x) (list :deep k (deep-extra x) (pair-left x) (node-next x)))
          ((pair-p x) (list :pair k (pair-left x) (pair-right x)))
          ((same-p x) (list :same k (node-next x)))
          (t (list :node k)))))

(defun set-through-parent (x v)
  (when (pair-p x)
    (setf (node-next x) v)
    (setf (pair-right x) (node-next x))
    (incf (node-kind x) 5))
  (list (node-kind x) (node-next x) (and (pair-p x) (pair-right x))))

(defun walk (x)
  (let ((s 0))
    (loop while x do
      (incf s (node-kind x))
      (when (leaf-p x) (incf s (leaf-val x)))
      (when (pair-p x) (incf s (if (node-p x) 7 0)))
      (setq x (node-next x)))
    s))

(defun child-of-constructor ()
  ;; the constructor's value is known to be a DEEP
  (let ((d (make-deep :kind 3 :left 'l :right 'r :extra 'e)))
    (list (node-kind d) (pair-left d) (pair-right d) (deep-extra d) (node-p d) (pair-p d) (deep-p d) (leaf-p d))))

(print (leaf-sum (make-leaf :kind 2 :val 40)))
(print (mapcar #'parent-then-child
               (list (make-node :kind 1) (make-leaf :kind 2 :val 3 :next 'n) (make-pair :kind 4 :left 'a :right 'b)
                     (make-deep :kind 5 :left 'c :extra 'x :next 'm) (make-same :kind 6 :next 'z))))
(print (set-through-parent (make-pair :kind 1) 'new))
(print (set-through-parent (make-deep :kind 2) 'newer))
(print (set-through-parent (make-leaf :kind 3 :next 'kept) 'unused))
(print (walk (make-leaf :kind 1 :val 10 :next (make-pair :kind 2 :next (make-node :kind 3 :next (make-deep :kind 4))))))
(print (child-of-constructor))
;; an object that is not of the structure still signals
(print (handler-case (leaf-sum (make-pair :kind 1)) (type-error () :type-error)))
(print (handler-case (parent-then-child 42) (type-error () :type-error)))
(print (handler-case (walk (list 1 2)) (type-error () :type-error)))

;;; tests that the types decide, blocks that end with a branch, code after
;;; a call that does not return
(defun decided (x)
  (declare (fixnum x))
  (let ((r (if (integerp x) (if (typep x 'fixnum) (1+ x) :not-fixnum) :not-integer)))
    (if (consp r) (car r) r)))
(defun after-error (x)
  (block nil
    (when (minusp x) (return :negative))
    (if (zerop x)
        (error "zero")
        (return (* x 2)))
    :not-reached))
(defun tail-in-arms (x)
  (cond ((null x) (decided 1))
        ((consp x) (tail-in-arms (cdr x)))
        (t (after-error x))))
(print (list (decided 41) (after-error -1) (after-error 4) (handler-case (after-error 0) (error () :error))
             (tail-in-arms '(1 2 3)) (tail-in-arms 5)))

;;; FUNCALL of constant symbols and of symbols, closures and other objects
(defun twice (x) (* 2 x))
(defun call-them (f)
  (list (funcall 'twice 4) (funcall f 5) (funcall #'twice 6)
        (handler-case (funcall 'no-such-function-here 1) (undefined-function () :undefined))
        (handler-case (funcall 42 1) (error () :not-a-function))))
(print (call-them 'twice))
(print (call-them (lambda (x) (+ x 100))))
(print (call-them #'1+))
