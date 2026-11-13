;;;; present.lisp — presentations: the objects on the screen (after CLIM)
;;;;
;;;; What is shown with PRESENT (or WITH-OUTPUT-AS-PRESENTATION) remembers the
;;;; Lisp object it stands for and its presentation type:
;;;;
;;;;   (defcomponent contact-line (contact)
;;;;     (with-output-as-presentation (contact 'contact)
;;;;       (:li (contact-name contact))))
;;;;
;;;; A command says what its arguments are:
;;;;
;;;;   (define-command (delete-contact :menu "Contact") ((contact 'contact))
;;;;     ...)
;;;;
;;;; Invoked from the menu, DELETE-CONTACT has no argument: the user is asked
;;;; for a contact (ACCEPT), the presentations of contacts on the screen are
;;;; highlighted, and a click on one of them supplies it.  The other way
;;;; round, the menu of a presentation (right click) offers the commands that
;;;; take such an object, and
;;;;
;;;;   (define-presentation-to-command-translator select-contact
;;;;       (contact show-contact) (object)
;;;;     (list object))
;;;;
;;;; makes a click on a contact call SHOW-CONTACT.
;;;;
;;;; This file also has what is shown above the application (the overlay):
;;;; popup menus, the prompt of ACCEPT and the dialogs of dialog.lisp.
;;;;
;;;; Compile it after command.lisp, followed by pane.lisp and dialog.lisp.

(in-package "UI")

;;; ------------------------------------------------------------------
;;; The overlay

(defstate *dialogs* nil "The dialogs on the screen, the topmost last (dialog.lisp).")
(defstate *popup* nil "The popup menu: NIL or (x y item...).")
(defstate *input-context* nil "What ACCEPT is waiting for, or NIL.")

(defvar *accept-prompt* "Select ~A"
  "The format control of the prompt of ACCEPT for a presentation type (its description).")

(defstruct (menu-item (:constructor make-menu-item (label function &key (enabled t) key)))
  label function enabled key)

(defun key-label (key)
  "The key of a command as shown in a menu."
  (and key
       (let ((position (search "CmdOrCtrl" key)))
         (if position
             (concatenate 'string (subseq key 0 position) "Ctrl" (subseq key (+ position 9)))
             key))))

(defun command-menu-item (command &rest arguments)
  (make-menu-item (command-label command)
                  (lambda () (apply #'invoke-command (command-name command) arguments))
                  :enabled (command-enabled-p (command-name command))
                  :key (key-label (command-key command))))

(defun menu-list (items close)
  "The node of a menu of ITEMS (menu items, :SEPARATOR); CLOSE is called
before the function of the chosen item."
  (html
    (:ul :class "ui-menu" :role "menu"
      (mapcar (lambda (item)
                (if (eq item :separator)
                    (:li :class "ui-menu-separator" :role "separator")
                    (:li :class (if (menu-item-enabled item) "ui-menu-item" "ui-menu-item disabled")
                         :role "menuitem"
                         :on-click (lambda (&optional event)
                                     (declare (ignore event))
                                     (when (menu-item-enabled item)
                                       (funcall close)
                                       (funcall (menu-item-function item))))
                      (:span (menu-item-label item))
                      (when (menu-item-key item)
                        (:span :class "ui-menu-key" (menu-item-key item))))))
              items))))

(defun close-popup () (setf *popup* nil))

(defun cancel-accept ()
  "Give up what ACCEPT is waiting for."
  (setf *input-context* nil))

(defun accepting-p ()
  "True while the user is asked to choose a presentation."
  (and *input-context* t))

(defstruct input-context type prompt continuation)
(defstruct dialog interface continuation (modal t))

(defcomponent overlay ()
  (:div :class "ui-overlay"
    (mapcar (lambda (dialog) (keyed dialog (dialog-view dialog))) *dialogs*)
    (when *input-context*
      (:div :class "ui-accept-bar" :role "status"
        (:span (input-context-prompt *input-context*))
        (:button :type "button" :class "ui-push-button"
                 :on-click (lambda (&optional event) (declare (ignore event)) (cancel-accept))
                 *abort-label*)))
    (when *popup*
      (destructuring-bind (x y &rest items) *popup*
        (:div :class "ui-popup"
          (:div :class "ui-menu-backdrop"
                :on-click (lambda (&optional event) (declare (ignore event)) (close-popup))
                :on-contextmenu (lambda (&optional event)
                                  (when event (stop-event *backend* event))
                                  (close-popup)))
          (:div :class "ui-popup-menu"
                ;; inside the window
                :style (format nil "left:min(~Dpx, calc(100vw - 15rem));top:min(~Dpx, calc(100vh - ~Dpx))"
                               x y (+ 16 (* 30 (length items))))
            (menu-list items #'close-popup)))))))

(defvar *overlays* nil "(backend . the mounted overlay)...")

(defun ensure-overlay ()
  (unless (assoc *backend* *overlays*)
    (let ((backend *backend*))
      (push (cons backend (mount (overlay) (overlay-container backend) :backend backend))
            *overlays*)
      (set-global-listener backend "keydown"
                           (lambda (&optional event)
                             (when (and (or *popup* *input-context*)
                                        (equal (event-get backend event "key") "Escape"))
                               (if *popup* (close-popup) (cancel-accept))))))))

(defun popup-menu (items x y)
  "Show a menu of ITEMS at the position X, Y of the window.  An item is a
menu item (MAKE-MENU-ITEM label function &key enabled key), the name of a
command, a list (command argument...) or :SEPARATOR."
  (ensure-overlay)
  (setf *popup*
        (list* (round x) (round y)
               (mapcar (lambda (item)
                         (cond ((menu-item-p item) item)
                               ((eq item :separator) item)
                               ((consp item) (apply #'command-menu-item (find-command (car item)) (cdr item)))
                               (t (command-menu-item (find-command item)))))
                       items))))

;;; ------------------------------------------------------------------
;;; Presentation types
;;;
;;; A presentation type is a type specifier of Lisp (the name of a class,
;;; STRING, (MEMBER ...)...) or a name defined with DEFINE-PRESENTATION-TYPE.

(defmacro define-presentation-type (name &optional supertypes &key description)
  "Define NAME as a presentation type that is a subtype of SUPERTYPES.
DESCRIPTION is how it is called in a prompt.  (The name of a class is a
presentation type without this; it may still be given a description.)"
  `(progn
     (setf (get ',name 'presentation-supertypes) ',supertypes)
     ,@(when description `((setf (get ',name 'presentation-description) ,description)))
     ',name))

(defun presentation-type-description (type)
  (or (and (symbolp type) (get type 'presentation-description))
      (string-downcase (substitute #\Space #\- (princ-to-string (if (consp type) (car type) type))))))

(defun presentation-type-of (object)
  "The presentation type OBJECT is presented with by default."
  (typecase object
    (string 'string)
    (integer 'integer)
    (number 'number)
    (null 'null)
    (symbol 'symbol)
    (cons 'cons)
    (t (class-name (class-of object)))))

(defun presentation-subtypep (type supertype)
  (or (eq supertype t)
      (equal type supertype)
      (and (symbolp type)
           (some (lambda (super) (presentation-subtypep super supertype))
                 (get type 'presentation-supertypes)))
      (and (not (and (symbolp type) (get type 'presentation-supertypes)))
           (ignore-errors (values (subtypep type supertype))))))

(defun presentation-matches-p (object type context-type)
  "True if OBJECT, presented as TYPE, is acceptable where CONTEXT-TYPE is asked for."
  (if (and (consp context-type) (eq (car context-type) 'member))
      (and (member object (cdr context-type)) t)
      (presentation-subtypep type context-type)))

;;; ------------------------------------------------------------------
;;; ACCEPT: ask the user for an object

(defgeneric accept-presentation-type (name type prompt continuation)
  (:documentation "Ask the user for an object of the presentation type TYPE, whose
name is NAME (TYPE itself, or its first element), and call CONTINUATION
with it.  The default method lets the user click a presentation;
dialog.lisp has methods for strings, numbers, BOOLEAN and MEMBER."))

(defmethod accept-presentation-type (name type prompt continuation)
  (declare (ignore name))
  (ensure-overlay)
  (setf *input-context* (make-input-context :type type :prompt prompt :continuation continuation)))

(defun accept (type &key prompt continuation)
  "Ask the user for an object of the presentation type TYPE and call
CONTINUATION with it (nothing is called if the user cancels).  Returns at
once.  PROMPT is the text shown meanwhile."
  (accept-presentation-type (if (consp type) (car type) type)
                            type
                            (or prompt (format nil *accept-prompt* (presentation-type-description type)))
                            (or continuation #'identity))
  nil)

(defun supply (object)
  "The user has chosen OBJECT for the input context."
  (let ((context *input-context*))
    (setf *input-context* nil)
    (funcall (input-context-continuation context) object)))

;;; ------------------------------------------------------------------
;;; Translators: what a gesture on a presentation does

(defstruct translator name type command gesture label tester function)

(defvar *translators* nil)

(defmacro define-presentation-to-command-translator
    (name (type command &key (gesture :select) label tester) (object) &body body)
  "On the gesture GESTURE (:SELECT is a click, :MENU only the menu) on a
presentation whose type is a subtype of TYPE, call COMMAND with the
arguments that BODY returns as a list, OBJECT being the presented object.
The translator is in the menu of the presentation (with LABEL, by default
the label of the command), and applies when the command is enabled and
TESTER, a function of the object, returns true."
  `(progn
     (setf *translators*
           (append (remove ',name *translators* :key #'translator-name)
                   (list (make-translator :name ',name :type ',type :command ',command
                                          :gesture ,gesture :label ,label :tester ,tester
                                          :function (lambda (,object) ,@body)))))
     ',name))

(defun applicable-translators (object type)
  (remove-if-not (lambda (translator)
                   (and (presentation-subtypep type (translator-type translator))
                        (command-enabled-p (translator-command translator))
                        (or (null (translator-tester translator))
                            (funcall (translator-tester translator) object))))
                 *translators*))

(defun run-translator (translator object)
  (apply #'invoke-command (translator-command translator)
         (funcall (translator-function translator) object)))

(defun presentation-menu (object type)
  "The menu items for OBJECT presented as TYPE: its translators, and the
commands whose first parameter is of that type."
  (let* ((translators (applicable-translators object type))
         (items (mapcar (lambda (translator)
                          (make-menu-item (or (translator-label translator)
                                              (command-label (find-command (translator-command translator))))
                                          (lambda () (run-translator translator object))))
                        translators)))
    (dolist (command *commands*)
      (let ((parameter (first (command-parameters command))))
        (when (and parameter (second parameter)
                   (command-active-p command)
                   (presentation-matches-p object type (second parameter))
                   (command-enabled-p (command-name command))
                   (not (find (command-name command) translators :key #'translator-command)))
          (push (command-menu-item command object) items))))
    (nreverse items)))

;;; ------------------------------------------------------------------
;;; Presentations

;;; drag and drop

(defstruct (drop-translator (:include translator)) to-type)

(defvar *drop-translators* nil)
(defvar *dragged* nil "(object type) of the presentation that is dragged, or NIL.")

(defmacro define-drag-and-drop-translator
    (name (from-type to-type command &key tester) (object destination) &body body)
  "When a presentation whose type is a subtype of FROM-TYPE is dragged onto
one whose type is a subtype of TO-TYPE, call COMMAND with the arguments that
BODY returns as a list, OBJECT and DESTINATION being the two objects.  It
applies when the command is enabled and TESTER, a function of the two
objects, returns true."
  `(progn
     (setf *drop-translators*
           (append (remove ',name *drop-translators* :key #'translator-name)
                   (list (make-drop-translator
                          :name ',name :type ',from-type :to-type ',to-type :command ',command
                          :tester ,tester
                          :function (lambda (,object ,destination)
                                      (declare (ignorable ,object ,destination))
                                      ,@body)))))
     ',name))

(defun drop-translator-for (object type destination destination-type)
  (find-if (lambda (translator)
             (and (presentation-subtypep type (translator-type translator))
                  (presentation-subtypep destination-type (drop-translator-to-type translator))
                  (command-enabled-p (translator-command translator))
                  (or (null (translator-tester translator))
                      (funcall (translator-tester translator) object destination))))
           *drop-translators*))

;;; pointer documentation: what the pointer can do where it is

(defstate *pointer-documentation* nil "The text for the presentation under the pointer, or NIL.")

(defvar *pointer-documentation-labels* '("Click: ~A" "Right click: menu" "Drag to drop")
  "The format controls of the pointer documentation: for the command of a
click (its label), for the menu and for dragging.")

(defun pointer-documentation-for (object type)
  (let ((context *input-context*)
        (parts '()))
    (cond ((and context (presentation-matches-p object type (input-context-type context)))
           (push (format nil (first *pointer-documentation-labels*) (input-context-prompt context)) parts))
          (t
           (let ((translator (find :select (applicable-translators object type) :key #'translator-gesture)))
             (when translator
               (push (format nil (first *pointer-documentation-labels*)
                             (or (translator-label translator)
                                 (command-label (find-command (translator-command translator)))))
                     parts)))
           (when (presentation-menu object type)
             (push (second *pointer-documentation-labels*) parts))
           (when (find type *drop-translators* :key #'translator-type :test #'presentation-subtypep)
             (push (third *pointer-documentation-labels*) parts))))
    (and parts (format nil "~{~A~^ ・ ~}" (nreverse parts)))))

(defcomponent pointer-documentation-view ()
  "The line that says what the pointer can do (see the initarg
:POINTER-DOCUMENTATION of interfaces)."
  (:div :class "ui-pointer-documentation" :role "status"
    (or *pointer-documentation* " ")))

(defun add-class (node class)
  "Add CLASS to the classes of the element NODE (a description)."
  (let ((old (getf (element-props node) :class)))
    (setf (getf (element-props node) :class)
          (if (and old (plusp (length old))) (concatenate 'string old " " class) class))
    node))

(defun presenting (object type node)
  "Make the element NODE (the value of markup) a presentation of OBJECT with
the presentation type TYPE (NIL: PRESENTATION-TYPE-OF); return NODE."
  (let* ((type (or type (presentation-type-of object)))
         (context *input-context*)
         (on-click (getf (element-props node) :on-click))
         (on-contextmenu (getf (element-props node) :on-contextmenu)))
    (add-class node (if (and context (presentation-matches-p object type (input-context-type context)))
                        "ui-presentation ui-acceptable"
                        "ui-presentation"))
    ;; the pointer documentation
    (setf (getf (element-props node) :on-mouseenter)
          (lambda (&optional event)
            (declare (ignore event))
            (setf *pointer-documentation* (pointer-documentation-for object type)))
          (getf (element-props node) :on-mouseleave)
          (lambda (&optional event)
            (declare (ignore event))
            (setf *pointer-documentation* nil)))
    ;; drag and drop
    (when (find type *drop-translators* :key #'translator-type :test #'presentation-subtypep)
      (setf (getf (element-props node) :draggable) t
            (getf (element-props node) :on-dragstart)
            (lambda (&optional event)
              (when event
                (begin-drag *backend* event (let ((text (present-object object type)))
                                              (if (stringp text) text (princ-to-string object)))))
              (setf *dragged* (list object type)))
            (getf (element-props node) :on-dragend)
            (lambda (&optional event)
              (declare (ignore event))
              (setf *dragged* nil))))
    (when (find type *drop-translators* :key #'drop-translator-to-type :test #'presentation-subtypep)
      (flet ((translator ()
               (and *dragged*
                    (drop-translator-for (first *dragged*) (second *dragged*) object type))))
        (setf (getf (element-props node) :on-dragover)
              (lambda (&optional event)
                ;; a drop is possible here
                (when (and event (translator)) (stop-event *backend* event)))
              (getf (element-props node) :on-drop)
              (lambda (&optional event)
                (let ((translator (translator))
                      (dragged *dragged*))
                  (setf *dragged* nil)
                  (when translator
                    (when event (stop-event *backend* event))
                    (apply #'invoke-command (translator-command translator)
                           (funcall (translator-function translator) (first dragged) object))))))))
    (setf (getf (element-props node) :on-click)
          (lambda (&optional event)
            (let ((context *input-context*))
              (cond ((and context (presentation-matches-p object type (input-context-type context)))
                     (when event (stop-event *backend* event))
                     (supply object))
                    (t
                     (let ((translator (and (null context)
                                            (find :select (applicable-translators object type)
                                                  :key #'translator-gesture))))
                       (when translator
                         (when event (stop-event *backend* event))
                         (run-translator translator object)))
                     (when on-click (funcall on-click event))))))
          (getf (element-props node) :on-contextmenu)
          (lambda (&optional event)
            (let ((items (presentation-menu object type)))
              (cond (items
                     (when event (stop-event *backend* event))
                     (popup-menu items
                                 (or (event-get *backend* event "clientX") 0)
                                 (or (event-get *backend* event "clientY") 0)))
                    (on-contextmenu (funcall on-contextmenu event))))))
    node))

(defmacro with-output-as-presentation ((object &optional type) &body markup)
  "The element of MARKUP (evaluated as by HTML) as a presentation of OBJECT
with the presentation type TYPE (evaluated; by default PRESENTATION-TYPE-OF
the object)."
  `(presenting ,object ,type (html ,@markup)))

(defgeneric present-object (object type)
  (:documentation "What shows OBJECT as the presentation type TYPE: a string, a node
or a list of them.")
  (:method (object type)
    (declare (ignore type))
    (if (stringp object) object (princ-to-string object))))

(defun present (object &optional type)
  "A node that shows OBJECT (as PRESENT-OBJECT says) and is a presentation
of it with the presentation type TYPE."
  (let ((type (or type (presentation-type-of object))))
    (presenting object type (element "span" nil (present-object object type)))))
