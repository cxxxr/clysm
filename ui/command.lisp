;;;; command.lisp — commands: the operations of an application
;;;;
;;;; A command is a function with a name, a label and, optionally, a place in
;;;; a menu, a key and a condition under which it is enabled:
;;;;
;;;;   (define-command (open-list :menu "File" :label "Open…" :key "CmdOrCtrl+O") ()
;;;;     ...)
;;;;   (define-command (clear-completed :label "Clear completed"
;;;;                                    :enabled (some #'todo-done *todos*)) ()
;;;;     ...)
;;;;
;;;; The same command is called from a button, (:button :on-click (command
;;;; clear-completed) ...) or (command-button 'clear-completed); from the
;;;; menu bar of the desktop application (desktop.lisp builds it from the
;;;; commands that have a :MENU); with its key; and from the REPL, as the
;;;; function (clear-completed).

(in-package "UI")

(defclass command ()
  ((name :initarg :name :reader command-name)
   (label :initarg :label :reader command-label)
   (menu :initarg :menu :reader command-menu)
   (key :initarg :key :reader command-key)
   (separator :initarg :separator :reader command-separator)
   (enabled :initarg :enabled :reader command-enabled)
   ;; the required parameters: (name type prompt), TYPE NIL if it has none
   (parameters :initarg :parameters :initform nil :reader command-parameters)
   ;; NIL, or the name of the command table it belongs to
   (table :initarg :table :initform nil :reader command-table)))

(defstate *command-tables* nil
  "The names of the command tables in use: those of the interfaces on the screen.")

(defun command-active-p (command)
  "True if COMMAND is offered to the user (in menus, by its key): it has no
command table, or its table is in use."
  (or (null (command-table command))
      (and (member (command-table command) *command-tables*) t)))

(defstate *commands* nil "The commands, in the order of their definition.")

(defun find-command (name)
  (find name *commands* :key #'command-name))

(defun register-command (name &key label menu key separator enabled parameters table)
  (let ((command (make-instance 'command :name name
                                         :label (or label (string-capitalize (substitute #\Space #\- (symbol-name name))))
                                         :menu menu :key key :separator separator :enabled enabled
                                         :parameters parameters :table table))
        (old (find-command name)))
    (setf *commands* (if old
                         (substitute command old *commands*)
                         (append *commands* (list command))))
    command))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-command-parameters (lambda-list)
    "The ordinary lambda list for LAMBDA-LIST, whose required parameters may
be (NAME TYPE &key PROMPT), and the forms for their (name type prompt)."
    (let ((required (loop for item in lambda-list
                          until (member item lambda-list-keywords)
                          collect item)))
      (values (append (mapcar (lambda (item) (if (consp item) (car item) item)) required)
                      (nthcdr (length required) lambda-list))
              (mapcar (lambda (item)
                        (if (consp item)
                            (destructuring-bind (name type &key prompt) item
                              `(list ',name ,type ,prompt))
                            `(list ',item nil nil)))
                      required)))))

(defmacro define-command (name-and-options lambda-list &body body)
  "Define the function NAME as a command.  NAME-AND-OPTIONS is NAME or
  (NAME :label string :menu string :key string :separator boolean :enabled form
        :table symbol)
LABEL is the text of its menu item or button, MENU the title of the menu it
is in, KEY its key (like \"CmdOrCtrl+Shift+S\"), SEPARATOR true to draw a line
above it in the menu.  The command can be invoked while ENABLED, a form
evaluated when needed (reading state in it is observed), is true.  With
TABLE, the name of a command table, the command is in menus and has its key
only while an interface with that :COMMAND-TABLE is on the screen.

A required parameter of LAMBDA-LIST may be (NAME TYPE &key PROMPT), where
TYPE is evaluated: when the command is invoked without that argument, the
user is asked for it (see ACCEPT in present.lisp) — by clicking an object
presented with that type or, for strings, numbers and (MEMBER ...), in a
dialog."
  (destructuring-bind (name &key label menu key separator (enabled nil enabledp) table)
      (if (consp name-and-options) name-and-options (list name-and-options))
    (multiple-value-bind (lambda-list parameters) (parse-command-parameters lambda-list)
      `(progn
         (defun ,name ,lambda-list ,@body)
         (register-command ',name :label ,label :menu ,menu :key ,key :separator ,separator
                                  :enabled ,(and enabledp `(lambda () ,enabled))
                                  :parameters (list ,@parameters) :table ,table)
         ',name))))

(defun command-enabled-p (name)
  (let ((command (find-command name)))
    (and command
         (or (null (command-enabled command))
             (and (funcall (command-enabled command)) t)))))

(defun invoke-command (name &rest arguments)
  "Call the command NAME with ARGUMENTS if it is enabled, as the user does:
an error is shown with the restarts to choose from, and the user is asked
for the missing arguments that have a type (the command is then called
later, when they are known)."
  (when (command-enabled-p name)
    (let* ((command (find-command name))
           (missing (car (nthcdr (length arguments) (command-parameters command)))))
      (if (and missing (second missing))
          (with-error-dialog
            ;; present.lisp
            (funcall 'accept (second missing)
                     :prompt (or (third missing)
                                 (format nil "~A: ~(~A~)" (command-label command) (first missing)))
                     :continuation (lambda (value)
                                     (apply #'invoke-command name (append arguments (list value)))))
            nil)
          (with-error-dialog (apply name arguments))))))

(defmacro command (name &rest arguments)
  "A function (for an :ON-<EVENT> property) that invokes the command NAME
with the values of ARGUMENTS, whatever it is called with."
  `(lambda (&rest event)
     (declare (ignore event))
     (invoke-command ',name ,@arguments)))

(defun command-button (name &rest arguments)
  "A button with the label of the command NAME that invokes it with
ARGUMENTS, disabled while the command is."
  (html (:button :disabled (not (command-enabled-p name))
                 :on-click (lambda (&rest event)
                             (declare (ignore event))
                             (apply #'invoke-command name arguments))
                 (command-label (find-command name)))))

(defun command-menus ()
  "The menus: a list of (title command...), in the order of definition."
  (let ((menus '()))
    (dolist (command *commands*)
      (when (and (command-menu command) (command-active-p command))
        (let ((menu (assoc (command-menu command) menus :test #'string=)))
          (if menu
              (push command (cdr menu))
              (push (list (command-menu command) command) menus)))))
    (nreverse (mapcar (lambda (menu) (cons (car menu) (reverse (cdr menu)))) menus))))

;;; keys

(defun parse-key (string)
  "The key STRING, like \"CmdOrCtrl+Shift+S\", as (key modifier...): the key
in upper case and the modifiers among :CTRL (also Cmd), :ALT and :SHIFT, sorted."
  (let ((parts '()) (start 0))
    (loop
      (let ((end (position #\+ string :start (min (1+ start) (length string)))))
        (push (subseq string start end) parts)
        (if end (setf start (1+ end)) (return))))
    (let ((modifiers '()))
      (dolist (part (cdr parts))
        (let ((m (cond ((member part '("Ctrl" "Control" "Cmd" "Command" "CmdOrCtrl" "CommandOrControl" "Meta")
                                :test #'string-equal)
                        :ctrl)
                       ((member part '("Alt" "Option") :test #'string-equal) :alt)
                       ((string-equal part "Shift") :shift))))
          (when m (pushnew m modifiers))))
      (cons (string-upcase (car parts)) (sort modifiers #'string<)))))

(defun command-for-key (key)
  "The enabled command whose key is KEY (as returned by PARSE-KEY), or NIL."
  (let ((command (find-if (lambda (command)
                            (and (command-key command)
                                 (command-active-p command)
                                 (equal (parse-key (command-key command)) key)))
                          *commands*)))
    (and command (command-enabled-p (command-name command)) command)))
