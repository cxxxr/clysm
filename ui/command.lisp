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
   (enabled :initarg :enabled :reader command-enabled)))

(defstate *commands* nil "The commands, in the order of their definition.")

(defun find-command (name)
  (find name *commands* :key #'command-name))

(defun register-command (name &key label menu key separator enabled)
  (let ((command (make-instance 'command :name name
                                         :label (or label (string-capitalize (substitute #\Space #\- (symbol-name name))))
                                         :menu menu :key key :separator separator :enabled enabled))
        (old (find-command name)))
    (setf *commands* (if old
                         (substitute command old *commands*)
                         (append *commands* (list command))))
    command))

(defmacro define-command (name-and-options lambda-list &body body)
  "Define the function NAME as a command.  NAME-AND-OPTIONS is NAME or
  (NAME :label string :menu string :key string :separator boolean :enabled form)
LABEL is the text of its menu item or button, MENU the title of the menu it
is in, KEY its key (like \"CmdOrCtrl+Shift+S\"), SEPARATOR true to draw a line
above it in the menu.  The command can be invoked while ENABLED, a form
evaluated when needed (reading state in it is observed), is true."
  (destructuring-bind (name &key label menu key separator (enabled nil enabledp))
      (if (consp name-and-options) name-and-options (list name-and-options))
    `(progn
       (defun ,name ,lambda-list ,@body)
       (register-command ',name :label ,label :menu ,menu :key ,key :separator ,separator
                                :enabled ,(and enabledp `(lambda () ,enabled)))
       ',name)))

(defun command-enabled-p (name)
  (let ((command (find-command name)))
    (and command
         (or (null (command-enabled command))
             (and (funcall (command-enabled command)) t)))))

(defun invoke-command (name &rest arguments)
  "Call the command NAME with ARGUMENTS if it is enabled, as the user does:
an error is shown with the restarts to choose from."
  (when (command-enabled-p name)
    (with-error-dialog (apply name arguments))))

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
      (when (command-menu command)
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
                                 (equal (parse-key (command-key command)) key)))
                          *commands*)))
    (and command (command-enabled-p (command-name command)) command)))
