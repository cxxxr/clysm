;;; Short method combinations called often (the discriminating functions
;;; compiled as code are compared with the generic function at the fourth
;;; call, and one that does not agree is replaced in the image after 500
;;; more: APPEND and NCONC, whose combinations are not
;;; :IDENTITY-WITH-ONE-ARGUMENT, never agreed, and the function made for
;;; them called itself without end after a thousand calls).

(defclass base () ((a :initform 1 :accessor a-of)))
(defclass mid (base) ())
(defclass top (mid) ())
(defvar *objs* (list (make-instance 'top) (make-instance 'mid) (make-instance 'base)))

(defmacro often (form) `(let ((r nil)) (dotimes (i 1500 r) (setq r ,form))))

(defgeneric parts (x y) (:method-combination append :most-specific-last))
(defmethod parts append ((x base) y) (list y))
(defmethod parts append ((x mid) y) (list 2 y))
(defmethod parts :around ((x top) y) (cdr (call-next-method)))
(print (often (mapcar (lambda (o) (parts o 7)) *objs*)))

(defgeneric pieces (x) (:method-combination nconc))
(defmethod pieces nconc ((x base)) (list :base))
(defmethod pieces nconc ((x mid)) (list :mid))
(print (often (mapcar #'pieces *objs*)))
;; one method alone: its list itself
(let ((l (list 1 2)))
  (defgeneric same (x) (:method-combination append))
  (defmethod same append ((x base)) l)
  (print (often (eq (same (car *objs*)) l))))

(defgeneric all (x) (:method-combination list))
(defmethod all list ((x base)) :base)
(defmethod all list ((x top)) :top)
(print (often (mapcar #'all *objs*)))

(defgeneric cost (x) (:method-combination +))
(defmethod cost + ((x base)) 1)
(defmethod cost + ((x mid)) 10)
(defmethod cost + ((x top)) 100)
(defgeneric most (x) (:method-combination max))
(defmethod most max ((x base)) 1)
(defmethod most max ((x top)) 3)
(defgeneric both (x) (:method-combination and))
(defmethod both and ((x base)) :base)
(defmethod both and ((x mid)) nil)
(defgeneric either (x) (:method-combination or :most-specific-last))
(defmethod either or ((x base)) nil)
(defmethod either or ((x mid)) :mid)
(defgeneric steps (x) (:method-combination progn))
(defvar *steps* 0)
(defmethod steps progn ((x base)) (incf *steps*))
(defmethod steps progn ((x top)) (incf *steps* 10))
(print (often (mapcar (lambda (o) (list (cost o) (most o) (both o) (either o))) *objs*)))
(print (list (often (mapcar #'steps *objs*)) *steps*))

;; a combination defined by the program, with and without the identity
(define-method-combination collect :operator list)
(define-method-combination total :operator + :identity-with-one-argument t)
(defgeneric gathered (x) (:method-combination collect))
(defmethod gathered collect ((x base)) :base)
(defgeneric summed (x) (:method-combination total))
(defmethod summed total ((x base)) 1)
(defmethod summed total ((x mid)) 2)
(print (often (list (mapcar #'gathered *objs*) (mapcar #'summed *objs*))))

;; a method added after many calls
(defmethod parts append ((x top) y) (list :top y))
(defmethod pieces nconc ((x top)) (list :top))
(print (often (list (mapcar (lambda (o) (parts o 7)) *objs*) (mapcar #'pieces *objs*))))
