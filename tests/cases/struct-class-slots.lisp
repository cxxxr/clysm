;;; Slots declared of a structure class: their reads are trusted to be of
;;; the class (the accessors of the value read are a field access, no test
;;; of its class), so a value of another type is a TYPE-ERROR where it is
;;; written (as in SBCL), never a trap of the code using it later.
(defstruct pt a b)
(defstruct fp (x 0d0 :type double-float) (y 0d0 :type double-float))
(defstruct node
  (pt (make-pt :a 1 :b 2) :type pt)
  (fp (make-fp) :type fp)
  (up nil :type (or null node)))
;; a slot of the structure being defined
(defstruct chain-end (tag :end))
(defstruct chain (val 0) (end (make-chain-end) :type chain-end) (next nil :type chain))
(defstruct ring (val 0) (self nil :type (or null ring)) (other (make-pt) :type pt))
;; including structures: a restated type, an instance of an including structure
(defstruct (pt3 (:include pt)) c)
(defstruct (sub (:include node (pt (make-pt3 :a 3 :b 4 :c 5) :type pt3))) z)

(defun pa (n) (let ((p (node-pt n))) (+ (pt-a p) (pt-b p))))
(defun fx (n) (let ((p (node-fp n))) (+ (fp-x p) (fp-y p))))
(defun pa2 (n) (pt-a (node-pt n)))
(defun up-pa (n) (let ((u (node-up n))) (if u (pa u) 0)))
(defun ring-other-a (r) (pt-a (ring-other r)))

;; compiled code checks declarations (the evaluator of the image, INTERP=1
;; tools/run-tests.sh, does not check the slot-value case)
(defmacro outcome (form)
  `(handler-case ,form
     (type-error () :type-error)
     (error (e) (list :other-error (type-of e)))))

(print (list (pa (make-node)) (fx (make-node)) (pa2 (make-node)) (up-pa (make-node :up (make-node)))))
(print (list (pa (make-sub)) (pt3-c (node-pt (make-sub))) (pa (make-node :pt (make-pt3 :a 10 :b 20 :c 0)))))
;; the constructor
(print (outcome (pa (make-node :pt 3))))
(print (outcome (fx (make-node :fp (make-pt)))))
(print (outcome (pa2 (make-node :pt "pt"))))
(print (outcome (pa (make-sub :pt (make-pt :a 1 :b 1)))))
(print (outcome (ring-other-a (make-ring :other (make-ring)))))
;; the SETF of the accessor, inline and as a function
(print (outcome (let ((n (make-node))) (setf (node-pt n) "foo") (pa n))))
(print (outcome (let ((n (make-node))) (setf (node-fp n) (make-pt)) (fx n))))
(print (outcome (let ((n (make-node))) (funcall #'(setf node-pt) 'zz n) (pa n))))
(print (outcome (let ((n (make-sub))) (setf (sub-pt n) (make-pt)) (pa n))))
(print (outcome (let ((n (make-sub))) (setf (node-pt n) 42) (pa n))))
;; SLOT-VALUE
(print (outcome (let ((n (make-node))) (setf (slot-value n 'pt) 7) (pa n))))
(print (outcome (let ((n (make-node))) (setf (slot-value n 'fp) 7) (fx n))))
(print (let ((n (make-node))) (setf (slot-value n 'pt) (make-pt :a 5 :b 6)) (pa n)))
;; a value of the type is stored
(print (let ((n (make-node))) (setf (node-pt n) (make-pt3 :a 7 :b 8 :c 9)) (pa n)))
(print (outcome (make-chain :next 1)))
(print (outcome (make-chain :end (make-chain-end) :next (make-chain-end))))
(print (outcome (make-chain :end 'end)))
(print (pa (copy-node (make-node :pt (make-pt :a 100 :b 1)))))
(terpri)
