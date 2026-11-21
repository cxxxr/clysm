;;; Slots declared simple arrays of one kind of storage: their reads are
;;; typed (inline element access), and a value of another type stored in
;;; one (SBCL signals the TYPE-ERROR where it is written, Clysm where it is
;;; read) is a TYPE-ERROR, never a trap.  (Code run by the interpreter
;;; reads such a slot as any other: the generic value.)
(defstruct zone
  (transitions (coerce #(10 20 30) '(simple-array (signed-byte 32) (*)))
   :type (simple-array (signed-byte 32) (*)))
  (subzones #(a b c) :type simple-vector)
  (name "utc" :type simple-string))

(defun probe (z x)
  (let ((v (zone-transitions z)))
    (loop for i below (length v) when (< x (elt v i)) return i)))
(defun first-subzone (z) (elt (zone-subzones z) 0))
(defun zone-initial (z) (char (zone-name z) 0))

;; compiled code checks declarations (the evaluator of the image, INTERP=1
;; tools/run-tests.sh, does not)
(defun checks-declarations-p ()
  (handler-case (let ((x (car (list 'foo)))) (declare (fixnum x)) (and x nil))
    (type-error () t)))
(defmacro outcome (form generic)
  `(let ((r (handler-case ,form
              (type-error () :type-error)
              (error (e) (list :other-error (type-of e))))))
     (if (or (eq r :type-error) (and (not (checks-declarations-p)) (equal r ,generic)))
         :type-error
         r)))

(let ((z (make-zone)))
  (print (list (probe z 15) (probe z (expt 2 40)) (probe z -5) (first-subzone z) (zone-initial z)))
  ;; through SETF of the accessor
  (print (outcome (progn (setf (zone-subzones z) (list 1 2)) (first-subzone z)) 1))
  (print (outcome (progn (setf (zone-transitions z) (vector 1 2)) (probe z 0)) 0))
  (print (outcome (progn (setf (zone-name z) 'utc) (zone-initial z)) :type-error)))
;; through the constructor
(print (outcome (first-subzone (make-zone :subzones "abc")) #\a))
(print (outcome (probe (make-zone :transitions (list 1 2 3)) 2) 1))
;; through SLOT-VALUE
(let ((z (make-zone)))
  (print (outcome (progn (setf (slot-value z 'name) 42) (zone-initial z)) :type-error)))
;; (SLOT-VALUE does not check it: the typed read does)
(let ((z (make-zone)))
  (print (outcome (progn (setf (slot-value z 'subzones) (list 7)) (first-subzone z)) 7)))
;; through a function that is not compiled inline
(let ((z (make-zone)))
  (print (outcome (progn (funcall #'(setf zone-subzones) nil z) (first-subzone z)) :type-error)))
(terpri)
