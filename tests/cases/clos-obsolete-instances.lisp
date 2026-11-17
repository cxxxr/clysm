;;; Instances made before their class was redefined with other slots (CLHS
;;; 4.3.6) are updated when their slots are next accessed, by every way of
;;; accessing them: call sites of readers and writers, accessor functions,
;;; SLOT-VALUE with a constant or a computed name, SLOT-BOUNDP,
;;; SLOT-MAKUNBOUND, WITH-SLOTS, REINITIALIZE-INSTANCE.  (Without tracking
;;; instances, an instance is known to be obsolete when the number of its
;;; slots changed.)

(defvar *log* nil)
(defclass r () ((p :initarg :p :initform :p :accessor p-of)))
(defmethod update-instance-for-redefined-class :after ((o r) added discarded plist &key)
  (push (list :updated added discarded plist) *log*))
(defun set-p (o v) (setf (p-of o) v))
(defun get-p (o) (p-of o))
(defun slot-named (o name) (slot-value o name))
(defun bound-q-p (o) (slot-boundp o 'q))

(defvar *objs* (loop for i below 9 collect (make-instance 'r :p i)))
(print (mapcar #'get-p *objs*))

(defclass r () ((q :initform :q :accessor q-of) (p :initarg :p :initform :p :accessor p-of)
                (s :initform :s)))
(destructuring-bind (o0 o1 o2 o3 o4 o5 o6 o7 o8) *objs*
  (print (list (get-p o0)                          ; a reader's call site
               (set-p o1 :one) (get-p o1)          ; a writer's call site
               (slot-value o2 'p)                  ; a constant slot name
               (slot-named o3 'q)                  ; a computed one
               (bound-q-p o4) (q-of o4)
               (progn (slot-makunbound o5 'q) (bound-q-p o5))
               (with-slots (p q s) o6 (list p q s))
               (progn (reinitialize-instance o7 :p :seven) (list (get-p o7) (q-of o7)))
               (funcall #'p-of o8)))
  (print (length *log*))
  (print (remove-duplicates *log* :test #'equal)))

;; a slot discarded: its value in the property list
(setq *log* nil)
(defvar *old* (make-instance 'r :p 42))
(defclass r () ((q :initform :q :accessor q-of)))
(print (list (q-of *old*) (slot-exists-p *old* 'p) *log*))
(terpri)
