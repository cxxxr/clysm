;;;; ui.lisp — a GUI framework for Clysm
;;;;
;;;; The state of an application lives in ordinary CLOS objects (DEFMODEL) and
;;;; variables (DEFSTATE); the screen is described by components
;;;; (DEFCOMPONENT) whose bodies are S-expression markup.  A component
;;;; remembers which slots and variables it read while rendering, and SETF of
;;;; one of them renders it again: there is no explicit "render" call.
;;;;
;;;;   (defmodel todo () ((text :initarg :text :accessor todo-text)
;;;;                      (done :initform nil :accessor todo-done)))
;;;;
;;;;   (defcomponent todo-item (todo)
;;;;     (:li :class (if (todo-done todo) "done" "")
;;;;       (:input :type "checkbox" :checked (todo-done todo)
;;;;               :on-change (lambda (e) (setf (todo-done todo) (not (todo-done todo)))))
;;;;       (todo-text todo)))
;;;;
;;;;   (mount (todo-item (make-instance 'todo :text "hello")) "#app")
;;;;
;;;; This file is independent of the display: it talks to a backend through
;;;; the generic functions below.  dom.lisp is the backend for the browser's
;;;; DOM, sexp.lisp one that builds S-expressions (for tests).

(defpackage "UI"
  (:use "CL")
  (:export
   ;; markup and components
   "HTML" "DEFCOMPONENT" "&STATE" "SELF" "KEYED" "RENDER" "MOUNTED" "UNMOUNTED" "COMPONENT"
   "MOUNT" "UNMOUNT"
   ;; state
   "DEFMODEL" "MODEL" "MODEL-CLASS" "DEFSTATE" "AUTORUN" "STOP" "FLUSH" "COMPUTED" "BINDING"
   ;; errors
   "WITH-ERROR-DIALOG" "CALL-WITH-ERROR-DIALOG" "*ABORT-LABEL*" "CHOOSE-RESTART"
   ;; commands (command.lisp)
   "DEFINE-COMMAND" "COMMAND" "INVOKE-COMMAND" "FIND-COMMAND" "COMMAND-ENABLED-P"
   "COMMAND-NAME" "COMMAND-LABEL" "COMMAND-MENU" "COMMAND-KEY" "COMMAND-SEPARATOR"
   "COMMAND-BUTTON" "COMMAND-MENUS" "COMMAND-FOR-KEY" "PARSE-KEY" "*COMMANDS*"
   "COMMAND-PARAMETERS" "COMMAND-TABLE" "*COMMAND-TABLES*" "COMMAND-ACTIVE-P"
   ;; presentations (present.lisp)
   "PRESENT" "PRESENT-OBJECT" "WITH-OUTPUT-AS-PRESENTATION" "PRESENTING"
   "DEFINE-PRESENTATION-TYPE" "PRESENTATION-TYPE-OF" "PRESENTATION-SUBTYPEP"
   "DEFINE-PRESENTATION-TO-COMMAND-TRANSLATOR" "ACCEPT" "ACCEPT-PRESENTATION-TYPE"
   "CANCEL-ACCEPT" "ACCEPTING-P" "POPUP-MENU" "MAKE-MENU-ITEM" "*ACCEPT-PROMPT*"
   "DEFINE-DRAG-AND-DROP-TRANSLATOR" "*POINTER-DOCUMENTATION-LABELS*"
   ;; panes and layouts (pane.lisp)
   "PANE" "RENDER-PANE" "PANE-NODE" "PANE-NAME" "PANE-INTERFACE" "PANE-TITLE" "PANE-ENABLED"
   "PANE-VISIBLE" "PANE-CLASS" "PANE-WIDTH" "PANE-HEIGHT" "PANE-STRETCH" "PANE-HELP"
   "TITLE-PANE" "TITLE-PANE-TEXT" "DISPLAY-PANE" "DISPLAY-PANE-TEXT"
   "PUSH-BUTTON" "CHECK-BUTTON" "BUTTON-TEXT" "BUTTON-SELECTED" "BUTTON-DATA" "BUTTON-COMMAND"
   "TEXT-INPUT-PANE" "TEXT-INPUT-PANE-TEXT" "MULTI-LINE-TEXT-INPUT-PANE"
   "CHOICE" "COLLECTION-ITEMS" "COLLECTION-PRINT-FUNCTION" "CHOICE-SELECTION"
   "CHOICE-SELECTED-ITEM" "CHOICE-SELECTED-ITEMS" "CHOICE-INTERACTION"
   "LIST-PANEL" "OPTION-PANE" "RADIO-BUTTON-PANEL" "CHECK-BUTTON-PANEL"
   "MULTI-COLUMN-LIST-PANEL" "LIST-PANEL-COLUMNS"
   "TREE-VIEW" "TREE-VIEW-ROOTS" "TREE-VIEW-EXPANDED-P"
   "SLIDER" "PROGRESS-BAR" "RANGE-VALUE" "RANGE-START" "RANGE-END"
   "APPLICATION-PANE" "APPLICATION-PANE-DISPLAY-FUNCTION" "TOOLBAR" "TOOLBAR-ITEMS"
   "LAYOUT" "LAYOUT-DESCRIPTION" "LAYOUT-RATIOS" "COLUMN-LAYOUT" "ROW-LAYOUT" "GRID-LAYOUT"
   "TAB-LAYOUT" "SWITCHABLE-LAYOUT" "SWITCHABLE-LAYOUT-VISIBLE-CHILD"
   "INTERFACE" "DEFINE-INTERFACE" "INTERFACE-TITLE" "INTERFACE-LAYOUT" "INTERFACE-MENU-BAR"
   "DISPLAY" "CONTAIN" "QUIT-INTERFACE" "FIND-PANE" "INTERFACE-COMMAND-TABLE"
   "LIST-PANEL-SORT-COLUMN" "LIST-PANEL-SORT-DESCENDING" "LIST-PANEL-FILTER"
   "PASSWORD-PANE" "EDITOR-PANE" "RADIO-BUTTON" "PUSH-BUTTON-PANEL" "TEXT-INPUT-CHOICE"
   "BROWSER-PANE" "BROWSER-PANE-URL" "IMAGE-PANE" "IMAGE-PANE-URL" "COLOR-PANE" "COLOR-PANE-COLOR"
   "SIMPLE-LAYOUT" "PINBOARD-LAYOUT" "INTERFACE-POINTER-DOCUMENTATION"
   "GRAPH-PANE" "GRAPH-PANE-ROOTS" "GRAPH-PANE-DIRECTION" "DRAW-IMAGE"
   ;; drawing (graphics.lisp)
   "OUTPUT-PANE" "OUTPUT-PANE-DISPLAY-CALLBACK" "DRAW-LINE" "DRAW-LINES" "DRAW-RECTANGLE"
   "DRAW-CIRCLE" "DRAW-ELLIPSE" "DRAW-POLYGON" "DRAW-STRING" "DRAW-PATH" "DRAW-ARC"
   "WITH-GRAPHICS-STATE" "WITH-GRAPHICS-TRANSLATION" "WITH-GRAPHICS-ROTATION"
   "WITH-GRAPHICS-SCALE" "WITH-DRAWING-GROUP"
   ;; dialogs (dialog.lisp)
   "DISPLAY-DIALOG" "EXIT-DIALOG" "ABORT-DIALOG" "DISPLAY-MESSAGE" "PROMPT-FOR-STRING"
   "PROMPT-FOR-CONFIRMATION" "PROMPT-WITH-LIST" "PROMPT-FOR-NUMBER"
   "COMMAND-PALETTE" "*COMMAND-PALETTE-PROMPT*"
   "PROMPT-FOR-VALUES" "PROMPT-FOR-COLOR" "PROMPT-FOR-FILE" "POPUP-CONFIRMER"
   "SET-CLIPBOARD" "CLIPBOARD" "CHOOSE-FILE" "WRITE-CLIPBOARD" "READ-CLIPBOARD"
   "*OK-LABEL*" "*YES-LABEL*" "*NO-LABEL*"
   ;; development
   "REFRESH"
   ;; the backend protocol
   "*BACKEND*" "MAKE-ELEMENT-NODE" "MAKE-TEXT-NODE" "SET-NODE-TEXT" "SET-PROPERTY"
   "REMOVE-PROPERTY" "SET-LISTENER" "INSERT-NODE" "REMOVE-NODE" "REPLACE-NODE"
   "NODE-FOLLOWS-P" "REQUEST-FLUSH" "RESOLVE-CONTAINER"
   "EVENT-GET" "EVENT-TARGET-GET" "STOP-EVENT" "NODE-CALL" "OVERLAY-CONTAINER"
   "SET-GLOBAL-LISTENER" "SET-TITLE" "PREVIOUS-SIBLING-SIZE"
   "BEGIN-DRAG" "REVEAL-SELECTION" "EVENT-POSITION" "NATIVE-MENUS-P" "*ERROR-MESSAGE-FUNCTION*"
   "CAN-WAIT-P" "WAIT-FOR" "WAIT-FOR-ANSWER" "*RESTART-CHOOSER*" "BACKEND-WAITER"
   ;; the backends: dom.lisp, sexp.lisp
   "DOM-BACKEND" "INSTALL-COMMANDS"
   "SEXP-BACKEND" "MAKE-ROOT" "TO-SEXP" "FIRE" "FIND-NODE" "FIND-NODES" "BACKEND-LOG"
   "FIRE-GLOBAL" "NODE-TEXT" "BACKEND-FILE"
   "BACKEND-CHOOSER"
   ;; the desktop application (desktop.lisp)
   "DESKTOP-BACKEND" "DESKTOP-P" "READ-FILE" "WRITE-FILE" "DESKTOP-FILE-ERROR"
   "CHOOSE-FILE-TO-OPEN" "CHOOSE-FILE-TO-SAVE" "MESSAGE-BOX" "USER-DATA-FILE"
   "ADD-RECENT-DOCUMENT"))

(in-package "UI")

;;; ------------------------------------------------------------------
;;; The backend protocol
;;;
;;; A backend makes and changes the nodes of the display.  NODE arguments are
;;; the objects it returned from MAKE-ELEMENT-NODE and MAKE-TEXT-NODE.

(defvar *backend* nil "The backend used by MOUNT.")

(defgeneric make-element-node (backend tag)
  (:documentation "Make an element; TAG is a lowercase string."))
(defgeneric make-text-node (backend string))
(defgeneric set-node-text (backend node string))
(defgeneric set-property (backend node name value)
  (:documentation "Set the property NAME (a keyword) of NODE to VALUE, which is not NIL."))
(defgeneric remove-property (backend node name old-value)
  (:documentation "Undo SET-PROPERTY of NAME; OLD-VALUE is the value it had (or NIL)."))
(defgeneric set-listener (backend node event function)
  (:documentation "Call FUNCTION with the event when EVENT (a string like \"click\")
happens on NODE.  Called once for every event of a node."))
(defgeneric insert-node (backend parent node before)
  (:documentation "Make NODE the child of PARENT in front of the child BEFORE
(at the end if BEFORE is NIL), moving it if it is in the tree."))
(defgeneric remove-node (backend node))
(defgeneric replace-node (backend old new))
(defgeneric node-follows-p (backend node next)
  (:documentation "True if the sibling after NODE is NEXT (NIL: NODE is the last child)."))
(defgeneric request-flush (backend)
  (:documentation "Arrange for FLUSH to be called soon (once the current event is handled)."))
(defgeneric resolve-container (backend container)
  (:documentation "The node for the CONTAINER argument of MOUNT.")
  (:method (backend container) container))

;;; What the panes, presentations and dialogs (pane.lisp, present.lisp,
;;; dialog.lisp) need in addition; EVENT is what a listener was called with.

(defgeneric event-get (backend event name)
  (:documentation "The property NAME (a string: \"key\", \"clientX\", \"shiftKey\"...) of EVENT."))
(defgeneric event-target-get (backend event name)
  (:documentation "The property NAME (\"value\", \"checked\", \"selectedIndex\") of the node EVENT happened on."))
(defgeneric stop-event (backend event)
  (:documentation "EVENT has been handled: no default action, and no other listener."))
(defgeneric node-call (backend node method &rest arguments)
  (:documentation "Call METHOD (a string: \"focus\", \"showModal\"...) of the backend's NODE."))
(defgeneric overlay-container (backend)
  (:documentation "The node above everything else: for dialogs and popup menus."))
(defgeneric set-global-listener (backend event function)
  (:documentation "Call FUNCTION with the event when EVENT happens anywhere."))
(defgeneric set-title (backend string)
  (:documentation "Set the title of the window."))
(defgeneric reveal-selection (backend event)
  (:documentation "Once the display is up to date, scroll the selected item of the
node EVENT happened on into view.")
  (:method (backend event) nil))
(defgeneric event-position (backend event)
  (:documentation "The position X, Y of the pointer EVENT in the coordinates of the
drawing (an OUTPUT-PANE) it happened on."))
(defgeneric native-menus-p (backend)
  (:documentation "True if the menus of the commands are shown outside the page.")
  (:method (backend) nil))
(defgeneric can-wait-p (backend)
  (:documentation "True if WAIT-FOR can wait now.")
  (:method (backend) nil))
(defgeneric wait-for (backend function)
  (:documentation "Call FUNCTION with a function of one argument, the answer, and
return the answer once that has been called — later, when the user has
done something; meanwhile the display works.  Only if CAN-WAIT-P."))
(defgeneric begin-drag (backend event text)
  (:documentation "The drag that EVENT starts carries TEXT (for other applications).")
  (:method (backend event text) nil))
(defgeneric choose-file (backend continuation &key accept)
  (:documentation "Let the user choose a file to read (ACCEPT: what kinds, like
\".json,.txt\") and call CONTINUATION with its name and its text, or with NIL
and NIL if none was chosen."))
(defgeneric write-clipboard (backend text)
  (:documentation "Put TEXT on the clipboard."))
(defgeneric read-clipboard (backend continuation)
  (:documentation "Call CONTINUATION with the text on the clipboard (NIL if it cannot be read)."))
(defgeneric previous-sibling-size (backend event)
  (:documentation "The width and height of the node in front of the one the pointer
EVENT happened on, which gets the following events of that pointer."))

;;; ------------------------------------------------------------------
;;; Errors: the user chooses a restart
;;;
;;; Event listeners, commands and rendering run inside WITH-ERROR-DIALOG.  An
;;; error there is not fatal: the backend shows its message with the restarts
;;; that are available — those the application established with
;;; RESTART-CASE around the failing operation, and ABORT, which gives up the
;;; operation — and the chosen restart is invoked.

(defgeneric choose-restart (backend condition restarts)
  (:documentation "Tell the user about CONDITION and ask which of RESTARTS (never
empty, the last is ABORT) to invoke; return it, or NIL for ABORT."))

(defvar *error-message-function* nil
  "NIL, or a function of a string that shows the message of an error that
has no restart to choose (dialog.lisp sets it).")

(defvar *restart-chooser* nil
  "NIL, or a function of an error and its restarts (more than one) that asks
the user in the page which to invoke and returns it (dialog.lisp sets it).
It is called where the backend can wait (CAN-WAIT-P).")

(defvar *abort-label* "Cancel" "How the ABORT restart of WITH-ERROR-DIALOG is described to the user.")

(defvar *error-dialog* nil "True inside WITH-ERROR-DIALOG.")

(defun report-error (condition)
  (let* ((restarts (compute-restarts condition))
         (abort (find 'abort restarts :key #'restart-name))
         ;; up to the ABORT of the dialog; what is outside is not offered
         (offered (ldiff restarts (cdr (member abort restarts))))
         (choice (and *backend* (choose-restart *backend* condition offered))))
    (invoke-restart-interactively (or choice abort))))

(defun call-with-error-dialog (function)
  (if *error-dialog*
      (funcall function)
      (let ((*error-dialog* t))
        (restart-case (handler-bind ((error #'report-error))
                        (funcall function))
          (abort ()
            :report (lambda (stream) (write-string *abort-label* stream))
            nil)))))

(defmacro with-error-dialog (&body body)
  "Evaluate BODY; if it signals an error, the user is shown the error and
chooses one of the active restarts.  The restart ABORT established here
returns NIL from the form."
  `(call-with-error-dialog (lambda () ,@body)))

;;; ------------------------------------------------------------------
;;; Observers: what runs again when the state it read changes

(defclass observer ()
  ((alive :initform t :accessor observer-alive)
   (dirty :initform nil :accessor observer-dirty)
   (depth :initform 0 :accessor observer-depth)
   (backend :initform *backend* :accessor observer-backend)))

(defgeneric run-observer (observer))

(defvar *observer* nil "The observer that is running: reads of state are recorded for it.")
(defvar *dirty* nil "The observers to run in the next FLUSH.")
(defvar *flush-requested* nil)

(defun invalidate (observer)
  (when (and (observer-alive observer) (not (observer-dirty observer)))
    (setf (observer-dirty observer) t)
    (push observer *dirty*)
    (unless *flush-requested*
      (setf *flush-requested* t)
      (request-flush (observer-backend observer)))))

(defvar *pending-mounts* nil "(function . node) for :ON-MOUNT, and components for MOUNTED.")

(defgeneric mounted (component)
  (:documentation "Called when the nodes of COMPONENT have been put on the display.")
  (:method (component) nil))
(defgeneric unmounted (component)
  (:documentation "Called when COMPONENT has been removed from the display.")
  (:method (component) nil))

(defun run-mount-hooks ()
  (loop while *pending-mounts*
        do (dolist (hook (nreverse (shiftf *pending-mounts* nil)))
             (if (consp hook)
                 (funcall (car hook) (cdr hook))
                 (mounted hook)))))

(defun flush ()
  "Bring the display up to date with the state: run the observers whose
state changed (parents before the components inside them)."
  (unwind-protect
       (let ((rounds 0))
         (loop while *dirty*
               do (when (> (incf rounds) 100)
                    (setf *dirty* nil)
                    (error "UI: rendering keeps changing the state it reads"))
                  (dolist (observer (sort (shiftf *dirty* nil) #'< :key #'observer-depth))
                    (when (and (observer-alive observer) (observer-dirty observer))
                      (let ((*backend* (observer-backend observer)))
                        ;; an observer that fails keeps what it showed
                        (with-error-dialog (run-observer observer))))))
         (with-error-dialog (run-mount-hooks)))
    (setf *flush-requested* nil)))

;;; AUTORUN: a function that runs again when what it read changes

(defclass reaction (observer)
  ((function :initarg :function)))

(defmethod run-observer ((reaction reaction))
  (setf (observer-dirty reaction) nil)
  (let ((*observer* reaction))
    (funcall (slot-value reaction 'function))))

(defun autorun (function)
  "Call FUNCTION now, and again (in FLUSH) whenever state it read has
changed.  STOP of the value ends that."
  (let ((reaction (make-instance 'reaction :function function)))
    ;; after the components
    (setf (observer-depth reaction) most-positive-fixnum)
    (with-error-dialog (run-observer reaction))
    reaction))

(defun stop (reaction)
  (setf (observer-alive reaction) nil))

;;; ------------------------------------------------------------------
;;; Models: classes whose slots are observed

(defclass model-class (standard-class) ()
  (:documentation "The metaclass of models: reading a slot of an instance records the
running observer, and writing it invalidates the observers that read it."))

(defmethod clysm-mop:validate-superclass ((class model-class) (superclass standard-class)) t)

(defclass model ()
  ((%observers :initform nil))          ; ((slot-name observer...)...)
  (:metaclass model-class))

(defun observe (object name)
  (let ((observer *observer*) (*observer* nil))
    (when (slot-boundp object '%observers)
      (let ((entry (assoc name (slot-value object '%observers))))
        (cond ((null entry)
               (push (list name observer) (slot-value object '%observers)))
              ((member observer (cdr entry)))
              (t (setf (cdr entry)
                       (cons observer (delete-if-not #'observer-alive (cdr entry))))))))))

(defun notify (object name)
  (let ((*observer* nil))
    (when (slot-boundp object '%observers)
      (let ((entry (assoc name (slot-value object '%observers))))
        (when (cdr entry)
          ;; the observers read the slot again when they run
          (dolist (observer (shiftf (cdr entry) nil))
            (invalidate observer)))))))

;;; A slot may hold a COMPUTED value: reading the slot calls its function
;;; (and the reader observes what the function read), so the slot follows
;;; other state.  With a setter (BINDING), writing the slot writes the place.

(defstruct (computed (:constructor make-computed (function &optional setter)))
  function setter)

(defmacro computed (form)
  "A value for a slot of a model: the slot then has the value of FORM,
evaluated whenever the slot is read.  Setting the slot replaces it."
  `(make-computed (lambda () ,form)))

(defmacro binding (place)
  "A value for a slot of a model: the slot then is PLACE; reading it reads
PLACE and setting it sets PLACE."
  (let ((value (gensym "VALUE")))
    `(make-computed (lambda () ,place) (lambda (,value) (setf ,place ,value)))))

(defvar *raw-slots* nil "True to read the COMPUTED object of a slot itself.")

(defmethod clysm-mop:slot-value-using-class ((class model-class) object slot)
  (when *observer*
    (let ((name (clysm-mop:slot-definition-name slot)))
      (unless (eq name '%observers) (observe object name))))
  (let ((value (call-next-method)))
    (if (and (computed-p value) (not *raw-slots*))
        (funcall (computed-function value))
        value)))

(defmethod (setf clysm-mop:slot-value-using-class) (value (class model-class) object slot)
  (let ((name (clysm-mop:slot-definition-name slot)))
    (if (eq name '%observers)
        (call-next-method)
        (let ((old (and (not (computed-p value))
                        (slot-boundp object name)
                        (let ((*raw-slots* t) (*observer* nil))
                          (slot-value object name)))))
          (cond ((and (computed-p old) (computed-setter old))
                 (funcall (computed-setter old) value)
                 value)
                (t (prog1 (call-next-method)
                     (notify object name))))))))

(defmacro defmodel (name superclasses slots &rest options)
  "Like DEFCLASS, for a class whose slots are observed: components that read
a slot of an instance are rendered again when the slot is set."
  `(defclass ,name (,@superclasses model) ,slots
     (:metaclass model-class)
     ,@options))

;;; DEFSTATE: an observed global variable

(defclass cell (model)
  ((value :initarg :value :accessor cell-value))
  (:metaclass model-class))

(defun make-cell (value) (make-instance 'cell :value value))

(defmacro defstate (name &optional value documentation)
  "Define NAME as an observed global place with the initial VALUE: components
that read it are rendered again when it is set (SETF, PUSH, INCF...).  NAME
is a symbol macro, not a special variable: LET of it is a lexical binding."
  (let ((cell (intern (format nil "%STATE-~A" (symbol-name name)) (symbol-package name))))
    `(progn
       (defvar ,cell (make-cell ,value) ,@(and documentation (list documentation)))
       (define-symbol-macro ,name (cell-value ,cell))
       ',name)))

;;; ------------------------------------------------------------------
;;; Nodes: the description of the display that components return

(defstruct node
  key                ; identifies the node among its siblings (EQUAL)
  dom                ; the backend's node, once created
  reused)            ; true once a later description has taken over the node

(defstruct (text (:include node) (:constructor %make-text (string)))
  string)

(defstruct (element (:include node) (:constructor %make-element (tag props children)))
  tag
  props              ; a plist
  children           ; a list of nodes
  listeners)         ; a plist: property -> (function), the cell the listener reads

(defstruct (call (:include node) (:constructor make-call (class args)))
  class args
  instance)          ; the component, once created

(defun collect-children (items)
  "The nodes for the children ITEMS: strings and numbers are text, NIL is
nothing, lists are spliced."
  (let ((result '()))
    (labels ((add (x)
               (cond ((null x))
                     ((node-p x) (push x result))
                     ((stringp x) (push (%make-text x) result))
                     ((listp x) (dolist (y x) (add y)))
                     (t (push (%make-text (princ-to-string x)) result)))))
      (add items))
    (nreverse result)))

(defun element (tag props &rest children)
  "Make the description of an element (what the markup (:TAG ...) expands to)."
  (let ((node (%make-element tag props (collect-children children)))
        (key (getf props :key)))
    (when key
      (setf (node-key node) key)
      (remf (element-props node) :key))
    node))

(defun keyed (key node)
  "NODE with the key KEY.  Among the children of an element, a node with a
key takes over the display (and the component state) of the previous child
with an EQUAL key, wherever that was."
  (setf (node-key node) key)
  node)

(defun as-node (x)
  (cond ((node-p x) x)
        ((null x) (%make-text ""))
        ((stringp x) (%make-text x))
        ((listp x) (error "UI: a component must return one node, not the list ~S" x))
        (t (%make-text (princ-to-string x)))))

;;; ------------------------------------------------------------------
;;; Markup: (:tag :property value ... child ...)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun walk-markup (form)
    "Replace the markup in the Lisp form FORM by calls of ELEMENT: every list
that starts with a keyword where a form is expected."
    (cond ((atom form) form)
          ((keywordp (car form))
           (let ((props '()) (rest (cdr form)))
             (loop while (keywordp (car rest))
                   do (when (null (cdr rest))
                        (error "UI: the property ~S of ~S has no value" (car rest) (car form)))
                      (push (car rest) props)
                      (push (walk-markup (cadr rest)) props)
                      (setf rest (cddr rest)))
             `(element ,(string-downcase (symbol-name (car form)))
                       (list ,@(nreverse props))
                       ,@(mapcar #'walk-markup rest))))
          ((eq (car form) 'quote) form)
          ((member (car form) '(case ecase ccase typecase etypecase ctypecase))
           ;; the keys of the clauses are not forms
           `(,(car form) ,(walk-markup (cadr form))
             ,@(mapcar (lambda (clause)
                         (if (consp clause)
                             (cons (car clause) (mapcar #'walk-markup (cdr clause)))
                             clause))
                       (cddr form))))
          ((null (cdr (last form)))
           (cons (walk-markup (car form)) (mapcar #'walk-markup (cdr form))))
          (t form)))

  (defun lambda-list-variables (lambda-list)
    (let ((vars '()))
      (dolist (item lambda-list (nreverse vars))
        (cond ((member item lambda-list-keywords))
              ((symbolp item) (push item vars))
              ((consp (car item)) (push (cadar item) vars))
              (t (push (car item) vars))))))

  (defun split-state (lambda-list)
    "The lambda list without &STATE and what follows it, and the state variables."
    ;; &STATE of any package
    (let ((tail (member "&STATE" lambda-list
                        :test (lambda (name x) (and (symbolp x) (string= name (symbol-name x)))))))
      (values (ldiff lambda-list tail)
              (mapcar (lambda (s) (if (consp s) s (list s nil))) (cdr tail))))))

(defmacro html (&body body)
  "Evaluate BODY, in which a list that starts with a keyword is markup:
  (:tag :property value ... child ...)
makes an element.  Properties are those of the backend's nodes (:CLASS,
:VALUE, :HIDDEN...; NIL removes one), :ON-<EVENT> a function called with the
event, :ON-MOUNT a function called with the backend's node when it is on the
display, :KEY the key (see KEYED).  A child is a node, a string or number
(text), NIL (nothing) or a list of children."
  `(progn ,@(mapcar #'walk-markup body)))

;;; ------------------------------------------------------------------
;;; Components

(defclass component (observer)
  ((args :initform nil :accessor component-args)
   (state :initform nil :accessor component-state)   ; a vector of cells
   (tree :initform nil :accessor component-tree)))   ; the node rendered last

(defgeneric render (component)
  (:documentation "The node that describes COMPONENT now."))
(defgeneric initial-state (component)
  (:method (component) nil))

(defun ensure-state (component count)
  "After a redefinition with other &STATE variables: keep the first values."
  (unless (= (length (component-state component)) count)
    (let ((old (component-state component))
          (new (initial-state component)))
      (replace new old)
      (setf (component-state component) new))))

(defmacro defcomponent (name lambda-list &body body)
  "Define the component NAME: a function with LAMBDA-LIST that returns a node
for the markup, and the class NAME of its instances on the display.
BODY is evaluated as by HTML, whenever state it read has changed or the
arguments are different, and returns one node.

After &STATE in LAMBDA-LIST come variables, as VAR or (VAR INITIAL-VALUE),
that keep their values as long as the instance is on the display; setting
one renders the component again.  SELF is the instance."
  (multiple-value-bind (params state) (split-state lambda-list)
    (let ((doc (and (stringp (car body)) (cdr body) (list (pop body))))
          (ignorable `(declare (ignorable ,@(lambda-list-variables params)))))
      `(progn
         (defclass ,name (component) ())
         ,@(when state
             `((defmethod initial-state ((self ,name))
                 (destructuring-bind ,params (component-args self)
                   ,ignorable
                   (vector ,@(mapcar (lambda (s) `(make-cell ,(second s))) state))))))
         (defmethod render ((self ,name))
           ,@(when state `((ensure-state self ,(length state))))
           (destructuring-bind ,params (component-args self)
             ,ignorable
             (symbol-macrolet ,(loop for s in state for i from 0
                                     collect `(,(first s) (cell-value (svref (component-state self) ,i))))
               ,@(mapcar #'walk-markup body))))
         (defun ,name (&rest args)
           ,@doc
           (make-call ',name args))
         ;; a redefinition shows at once
         (refresh)
         ',name))))

;;; ------------------------------------------------------------------
;;; Putting nodes on the display, and changing it to a new description

(defvar *parent* nil "The component whose nodes are being created or patched.")

(defun dom-of (node)
  (if (call-p node)
      (dom-of (component-tree (call-instance node)))
      (node-dom node)))

(defun render-component (component)
  (setf (observer-dirty component) nil)
  (let ((*observer* component))
    (as-node (render component))))

(defun event-property-p (key)
  (let ((name (symbol-name key)))
    (and (> (length name) 3) (string= "ON-" name :end2 3))))

(defun live-property-p (key)
  "Properties the user changes on the display: they are set again on every
patch (the backend compares with the value on the display)."
  (or (eq key :value) (eq key :checked)))

(defun apply-property (element key old new)
  (let ((dom (node-dom element)))
    (cond ((eq key :on-mount))
          ((event-property-p key)
           ;; one listener per event, which calls the function of the latest description
           (let ((cell (getf (element-listeners element) key)))
             (if cell
                 (setf (car cell) new)
                 (let ((cell (list new)))
                   (setf (getf (element-listeners element) key) cell)
                   (set-listener *backend* dom (string-downcase (subseq (symbol-name key) 3))
                                 (let ((backend *backend*))
                                   (lambda (&rest args)
                                     (when (car cell)
                                       (let ((*backend* backend))
                                         (with-error-dialog (apply (car cell) args))))
                                     ;; nothing to convert for the backend
                                     nil)))))))
          ((null new) (remove-property *backend* dom key old))
          (t (set-property *backend* dom key new)))))

(defun create (node)
  "Create the backend's nodes for NODE (not yet on the display); return the topmost."
  (etypecase node
    (text (setf (node-dom node) (make-text-node *backend* (text-string node))))
    (element
     (let ((dom (make-element-node *backend* (element-tag node))))
       (setf (node-dom node) dom)
       (loop for (key value) on (element-props node) by #'cddr
             do (cond ((eq key :on-mount)
                       (when value (push (cons value dom) *pending-mounts*)))
                      (value (apply-property node key nil value))))
       (dolist (child (element-children node))
         (insert-node *backend* dom (create child) nil))
       dom))
    (call
     (let ((component (make-instance (call-class node))))
       (setf (call-instance node) component
             (component-args component) (call-args node)
             (observer-backend component) *backend*
             (observer-depth component) (if *parent* (1+ (observer-depth *parent*)) 0)
             (component-state component) (initial-state component))
       (let ((tree (render-component component)))
         (setf (component-tree component) tree)
         (prog1 (let ((*parent* component)) (create tree))
           (push component *pending-mounts*)))))))

(defun destroy (node)
  "NODE has left the display: its components are dead."
  (typecase node
    (element
     (loop for cell in (cdr (element-listeners node)) by #'cddr
           do (setf (car cell) nil))
     (mapc #'destroy (element-children node)))
    (call
     (let ((component (call-instance node)))
       (setf (observer-alive component) nil)
       (destroy (component-tree component))
       (unmounted component)))))

(defun same-kind-p (old new)
  (and (equal (node-key old) (node-key new))
       (typecase old
         (text (text-p new))
         (element (and (element-p new) (string= (element-tag old) (element-tag new))))
         (call (and (call-p new) (eq (call-class old) (call-class new)))))))

(defun property-equal (a b)
  (or (eql a b) (and (stringp a) (stringp b) (string= a b))))

(defun patch-properties (old new)
  (let ((old-props (element-props old)) (new-props (element-props new)))
    (loop for (key value) on new-props by #'cddr
          do (let ((old-value (getf old-props key)))
               (unless (and (property-equal old-value value)
                            (not (live-property-p key))
                            (not (event-property-p key)))
                 (apply-property new key old-value value))))
    (loop for (key value) on old-props by #'cddr
          do (when (and value (eq (getf new-props key '%none) '%none))
               (apply-property new key value nil)))))

(defun patch-children (old new)
  (let* ((parent (node-dom new))
         (olds (element-children old))
         (news (element-children new))
         (unkeyed (remove-if #'node-key olds))
         (keyed (and (some #'node-key olds)
                     (let ((table (make-hash-table :test 'equal)))
                       (dolist (o olds table)
                         (when (node-key o) (setf (gethash (node-key o) table) o))))))
         (reorder (or keyed (some #'node-key news)))
         (fresh '()))
    ;; every new child takes over an old one of the same kind (by key, or the
    ;; next one without a key) or is created
    (dolist (n news)
      (let ((o (if (node-key n)
                   (and keyed (gethash (node-key n) keyed))
                   (pop unkeyed))))
        (cond ((and o (not (node-reused o)) (same-kind-p o n))
               (setf (node-reused o) t)
               (patch o n))
              (t (create n)
                 (push n fresh)))))
    (dolist (o olds)
      (unless (node-reused o)
        (remove-node *backend* (dom-of o))
        (destroy o)))
    ;; put the new and (with keys) the moved children in their places
    (when (or fresh reorder)
      (let ((next nil))
        (dolist (n (reverse news))
          (let ((dom (dom-of n)))
            (when (or (member n fresh)
                      (and reorder (not (node-follows-p *backend* dom next))))
              (insert-node *backend* parent dom next))
            (setf next dom)))))))

(defun update-component (component)
  (let ((tree (render-component component))
        (*parent* component))
    (setf (component-tree component) (patch (component-tree component) tree))))

(defmethod run-observer ((component component))
  (update-component component))

(defun patch (old new)
  "Change the display of the node OLD to that of NEW; return NEW."
  (cond ((eq old new))
        ((not (same-kind-p old new))
         (let ((old-dom (dom-of old)))
           (replace-node *backend* old-dom (create new))
           (destroy old)))
        (t
         (etypecase new
           (text
            (setf (node-dom new) (node-dom old))
            (unless (string= (text-string old) (text-string new))
              (set-node-text *backend* (node-dom new) (text-string new))))
           (element
            (setf (node-dom new) (node-dom old)
                  (element-listeners new) (element-listeners old))
            (patch-properties old new)
            (patch-children old new))
           (call
            (let ((component (call-instance old)))
              (setf (call-instance new) component)
              ;; with the same arguments, it renders again only for its own state
              (when (or (observer-dirty component)
                        (not (equal (component-args component) (call-args new))))
                (setf (component-args component) (call-args new))
                (update-component component)))))))
  new)

(defvar *roots* nil "The mounted nodes.")

(defun refresh ()
  "Render every component on the display again (after functions that they
call were redefined)."
  (labels ((visit (node)
             (typecase node
               (element (mapc #'visit (element-children node)))
               (call (let ((component (call-instance node)))
                       (invalidate component)
                       (visit (component-tree component)))))))
    (mapc #'visit *roots*)
    nil))

(defun mount (node container &key (backend *backend*))
  "Put NODE (the value of a component function or of HTML) on the display
as the last child of CONTAINER; from then on it follows the state.
Return the node, for UNMOUNT."
  (let* ((*backend* backend)
         (*parent* nil)
         (node (as-node node)))
    (insert-node backend (resolve-container backend container) (create node) nil)
    (push node *roots*)
    (run-mount-hooks)
    node))

(defun unmount (node &key (backend *backend*))
  (setf *roots* (remove node *roots*))
  (remove-node backend (dom-of node))
  (destroy node))

;;; ------------------------------------------------------------------
;;; Waiting for the user inside a computation

(defun wait-for-answer (function)
  "Call FUNCTION with a function of one argument, the answer; return the
answer once that has been called.  FUNCTION shows something (a dialog) whose
listeners give the answer; until then this computation waits while the
display works, as if it were not running: listeners, rendering and FLUSH
start afresh.  Only where (can-wait-p *backend*)."
  (let ((backend *backend*))
    (let ((*error-dialog* nil)
          (*observer* nil)
          (*parent* nil)
          (*pending-mounts* nil)
          (*flush-requested* nil))
      (wait-for backend function))))
