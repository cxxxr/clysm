;;;; sexp.lisp — a UI backend whose display is a tree of S-expressions
;;;;
;;;; For tests of components without a browser:
;;;;   (let* ((ui:*backend* (make-instance 'ui:sexp-backend))
;;;;          (root (ui:make-root)))
;;;;     (ui:mount (my-component) root)
;;;;     (print (ui:to-sexp root))
;;;;     (ui:fire (ui:find-node root "button") "click")   ; calls the listener, then FLUSH
;;;;     (print (ui:backend-log ui:*backend*)))            ; what was changed
;;;; An error in a listener is noted in the log, and the restart that the
;;;; function (ui:backend-chooser backend) returns is invoked.

(in-package "UI")

(defclass sexp-backend ()
  ((log :initform nil :accessor %backend-log)
   ;; NIL, or a function of an error and its restarts that returns the
   ;; restart the "user" chooses (NIL: abort)
   (chooser :initform nil :initarg :chooser :accessor backend-chooser)
   ;; NIL, or a function called by WAIT-FOR in place of the user: it
   ;; does what gives the answer (FIRE of a button of the dialog)
   (waiter :initform nil :initarg :waiter :accessor backend-waiter)
   ;; (name text) of the file the "user" chooses, and the clipboard
   (file :initform nil :initarg :file :accessor backend-file)
   (clipboard :initform nil :accessor backend-clipboard)
   (overlay :initform nil)
   (global-listeners :initform nil :accessor backend-global-listeners)))

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
  (unless (and (live-property-p name) (equal (getf (snode-props node) name) value))
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

;;; for the panes: an event is a property list, like (:key "Enter" :value "text")

(defun event-keyword (name)
  (intern (string-upcase name) "KEYWORD"))

(defmethod event-get ((backend sexp-backend) event name)
  (and (listp event) (getf event (event-keyword name))))

(defmethod event-target-get ((backend sexp-backend) event name)
  (and (listp event) (getf event (event-keyword name))))

(defmethod stop-event ((backend sexp-backend) event)
  nil)

(defmethod node-call ((backend sexp-backend) node method &rest arguments)
  (apply #'note backend :call (label node) method arguments))

(defmethod overlay-container ((backend sexp-backend))
  (or (slot-value backend 'overlay)
      (setf (slot-value backend 'overlay) (make-snode :tag "overlay"))))

(defmethod set-global-listener ((backend sexp-backend) event function)
  (push (cons event function) (backend-global-listeners backend)))

(defmethod set-title ((backend sexp-backend) string)
  (note backend :title string))

(defmethod viewport-size ((backend sexp-backend))
  (values 1000 600))

(defmethod choose-file ((backend sexp-backend) continuation &key accept)
  (note backend :choose-file accept)
  (funcall continuation (first (backend-file backend)) (second (backend-file backend))))

(defmethod write-clipboard ((backend sexp-backend) text)
  (setf (backend-clipboard backend) text))

(defmethod read-clipboard ((backend sexp-backend) continuation)
  (funcall continuation (backend-clipboard backend)))

(defmethod can-wait-p ((backend sexp-backend))
  (and (backend-waiter backend) t))

(defmethod wait-for ((backend sexp-backend) function)
  (let ((answer nil) (answered nil))
    (funcall function (lambda (value) (setf answer value answered t)))
    (flush)
    (funcall (backend-waiter backend))
    (unless answered (error "the waiter of the backend did not give an answer"))
    answer))

(defmethod reveal-selection ((backend sexp-backend) event)
  (note backend :reveal-selection))

(defmethod event-position ((backend sexp-backend) event)
  (values (getf event :x) (getf event :y)))

(defmethod previous-sibling-size ((backend sexp-backend) event)
  (values (getf event :width) (getf event :height)))

(defun fire-global (backend event &rest arguments)
  "Call the global listeners of BACKEND for EVENT, then bring the display up to date."
  (loop for (name . function) in (backend-global-listeners backend)
        do (when (string= name event) (apply function arguments)))
  (flush))

(defun node-text (node)
  "The text under NODE of the display."
  (if (snode-text node)
      (snode-text node)
      (apply #'concatenate 'string (mapcar #'node-text (snode-children node)))))

(defmethod choose-restart ((backend sexp-backend) condition restarts)
  (note backend :error (princ-to-string condition) (mapcar #'princ-to-string restarts))
  (cond ((backend-chooser backend)
         (funcall (backend-chooser backend) condition restarts))
        ((and *restart-chooser* (cdr restarts) (can-wait-p backend))
         (funcall *restart-chooser* condition restarts))))

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
