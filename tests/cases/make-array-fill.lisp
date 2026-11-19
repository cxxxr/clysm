;;; MAKE-ARRAY of a vector with a fill pointer or adjustable, with a constant
;;; element type (compiled without parsing its keywords at run time): the
;;; buffers of readers, what VECTOR-PUSH-EXTEND and ADJUST-ARRAY do with
;;; them, the order the arguments are evaluated in, and errors.
(defvar *order* '())
(defun note (x) (push x *order*) x)

(defun buffer () (make-array 4 :adjustable t :fill-pointer 0 :element-type 'character))
(let ((b (buffer)))
  (dolist (c (coerce "hello, world" 'list)) (vector-push-extend c b))
  (print (list b (length b) (fill-pointer b) (adjustable-array-p b) (array-element-type b)
               (stringp b) (simple-string-p b) (>= (array-total-size b) 12))))
;; two buffers do not share anything
(let ((a (buffer)) (b (buffer)))
  (vector-push-extend #\a a) (vector-push-extend #\b b) (vector-push-extend #\c b)
  (print (list a b)))
(print (let ((v (make-array 3 :fill-pointer t :initial-element 7)))
         (list v (fill-pointer v) (aref v 2))))
(print (let ((v (make-array 5 :adjustable t :fill-pointer 2 :initial-element 0 :element-type 'bit)))
         (setf (aref v 1) 1)
         (list v (array-element-type v) (bit-vector-p v))))
(print (let ((v (make-array (note 3) :fill-pointer (note 1) :initial-element (note #\x)
                            :element-type 'character)))
         (list v (reverse *order*))))
;; adjusted afterwards, the original's type is kept
(let ((v (make-array 2 :adjustable t :fill-pointer 0 :element-type 'character)))
  (vector-push-extend #\a v)
  (let ((w (adjust-array v 10 :fill-pointer 1)))
    (vector-push-extend #\b w)
    (print (list w (array-element-type w) (adjustable-array-p w)))))
;; a list of dimensions in a variable, and errors
(let ((dims (list 4)))
  (print (let ((v (make-array dims :adjustable t :fill-pointer 1 :element-type 'character :initial-element #\z)))
           (list v (array-dimensions v)))))
(print (handler-case (make-array 3 :fill-pointer 4 :adjustable t :element-type 'character)
         (error () :fill-pointer-error)))
(print (handler-case (make-array -1 :fill-pointer 0 :adjustable t :element-type 'character)
         (error () :dimension-error)))
;; such a buffer printed, also pretty (a string has no pretty printing)
(let ((b (buffer)) (*print-pretty* t))
  (dolist (c '(#\k #\" #\y)) (vector-push-extend c b))
  (print (list (princ-to-string b) (prin1-to-string b)
               (with-output-to-string (s) (princ b s) (prin1 b s))
               (subtypep (class-of b) (find-class 'string)))))
