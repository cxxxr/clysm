;;;; sexp.lisp — a UI backend whose display is a tree of S-expressions
;;;;
;;;; For tests of components without a browser:
;;;;   (let* ((ui:*backend* (make-instance 'ui:sexp-backend))
;;;;          (root (ui:make-root)))
;;;;     (ui:mount (my-component) root)
;;;;     (print (ui:to-sexp root))
;;;;     (ui:fire (ui:find-node root "button") "click")   ; calls the listener, then FLUSH
;;;;     (print (ui:backend-log ui:*backend*)))            ; what was changed

(in-package "UI")

(defclass sexp-backend ()
  ((log :initform nil :accessor %backend-log)))

(defstruct snode tag text props listeners children parent)

(defun backend-log (backend)
  "The operations on the display since the last call, oldest first."
  (nreverse (shiftf (%backend-log backend) nil)))

(defun note (backend &rest operation)
  (push operation (%backend-log backend)))

(defun make-root () (make-snode :tag "root"))

(defun label (node)
  (or (snode-text node) (intern (string-upcase (snode-tag node)) "KEYWORD")))

(defun detach (node)
  (let ((parent (snode-parent node)))
    (when parent
      (setf (snode-children parent) (remove node (snode-children parent))
            (snode-parent node) nil))))

(defmethod make-element-node ((backend sexp-backend) tag)
  (note backend :create tag)
  (make-snode :tag tag))

(defmethod make-text-node ((backend sexp-backend) string)
  (note backend :create-text string)
  (make-snode :text string))

(defmethod set-node-text ((backend sexp-backend) node string)
  (note backend :text (snode-text node) string)
  (setf (snode-text node) string))

(defmethod set-property ((backend sexp-backend) node name value)
  (unless (and (live-property-p name) (eql (getf (snode-props node) name) value))
    (note backend :set (label node) name value)
    (setf (getf (snode-props node) name) value)))

(defmethod remove-property ((backend sexp-backend) node name old-value)
  (declare (ignore old-value))
  (unless (eq (getf (snode-props node) name '%none) '%none)
    (note backend :unset (label node) name)
    (remf (snode-props node) name)))

(defmethod set-listener ((backend sexp-backend) node event function)
  (push (cons event function) (snode-listeners node)))

(defmethod insert-node ((backend sexp-backend) parent node before)
  (note backend :insert (label node) :in (label parent))
  (detach node)
  (let ((tail (member before (snode-children parent))))
    (setf (snode-children parent)
          (append (ldiff (snode-children parent) tail) (list node) tail)
          (snode-parent node) parent)))

(defmethod remove-node ((backend sexp-backend) node)
  (note backend :remove (label node))
  (detach node))

(defmethod replace-node ((backend sexp-backend) old new)
  (note backend :replace (label old) (label new))
  (let ((parent (snode-parent old)))
    (detach new)
    (setf (snode-children parent) (substitute new old (snode-children parent))
          (snode-parent new) parent
          (snode-parent old) nil)))

(defmethod node-follows-p ((backend sexp-backend) node next)
  (eq (cadr (member node (snode-children (snode-parent node)))) next))

(defmethod request-flush ((backend sexp-backend))
  ;; tests call FLUSH (FIRE does)
  nil)

(defun to-sexp (node)
  "NODE of the display as (:tag (property value...) child...), text as strings."
  (if (snode-text node)
      (snode-text node)
      (list* (label node) (copy-list (snode-props node))
             (mapcar #'to-sexp (snode-children node)))))

(defun find-nodes (root tag &key class)
  "The elements under ROOT with the tag TAG (and, if given, the :CLASS property CLASS)."
  (let ((result '()))
    (labels ((visit (node)
               (when (and (equal (snode-tag node) tag)
                          (or (null class) (equal (getf (snode-props node) :class) class)))
                 (push node result))
               (mapc #'visit (snode-children node))))
      (visit root))
    (nreverse result)))

(defun find-node (root tag &key class (index 0))
  (nth index (find-nodes root tag :class class)))

(defun fire (node event &rest arguments)
  "Call the listener of NODE for EVENT with ARGUMENTS, then bring the display up to date."
  (let ((listener (cdr (assoc event (snode-listeners node) :test #'string=))))
    (unless listener (error "~S has no listener for ~A" (to-sexp node) event))
    (apply listener arguments)
    (flush)))
