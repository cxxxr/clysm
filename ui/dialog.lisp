;;;; dialog.lisp — dialogs (after CAPI)
;;;;
;;;; A dialog is an interface shown above the application, which waits until
;;;; the user has answered:
;;;;
;;;;   (prompt-for-string "Name of the new list:"
;;;;                      :continuation (lambda (name okp)
;;;;                                      (when okp (make-list-named name))))
;;;;
;;;; A page cannot wait inside a function, so these functions return at once
;;;; and call their CONTINUATION with the answer and whether the user gave
;;;; one (NIL NIL if the dialog was cancelled).  A command asks for its
;;;; arguments in this way by itself (see DEFINE-COMMAND):
;;;;
;;;;   (define-command (new-list :menu "File") ((name 'string :prompt "Name of the new list:"))
;;;;     (make-list-named name))
;;;;
;;;; Any interface is a dialog with DISPLAY-DIALOG; its callbacks answer with
;;;; (exit-dialog value) or (abort-dialog).
;;;;
;;;; Compile it after pane.lisp.

(in-package "UI")

(defvar *ok-label* "OK")
(defvar *yes-label* "Yes")
(defvar *no-label* "No")

(defcomponent dialog-view (dialog)
  (let ((interface (dialog-interface dialog)))
    (:dialog :class "ui-dialog" :aria-label (pane-title interface)
             :on-mount (lambda (node) (node-call *backend* node "showModal"))
             ;; Escape
             :on-cancel (lambda (&optional event)
                          (when event (stop-event *backend* event))
                          (abort-dialog dialog))
      (when (pane-title interface)
        (:header :class "ui-dialog-title" (pane-title interface)))
      (pane-node interface))))

(defun display-dialog (interface &key continuation)
  "Show INTERFACE as a dialog.  When it is answered with EXIT-DIALOG,
CONTINUATION is called with the value and T; when it is cancelled
(ABORT-DIALOG, the Escape key), with NIL and NIL."
  (ensure-overlay)
  (setf *dialogs* (append *dialogs* (list (make-dialog :interface interface :continuation continuation))))
  interface)

(defun close-dialog (dialog value okp)
  (when (member dialog *dialogs*)
    (setf *dialogs* (remove dialog *dialogs*))
    (when (dialog-continuation dialog)
      (funcall (dialog-continuation dialog) value okp))))

(defun top-dialog ()
  (first (last *dialogs*)))

(defun exit-dialog (value &optional (dialog (top-dialog)))
  "Answer the dialog (the topmost one) with VALUE."
  (when dialog (close-dialog dialog value t)))

(defun abort-dialog (&optional (dialog (top-dialog)))
  "Cancel the dialog (the topmost one)."
  (when dialog (close-dialog dialog nil nil)))

;;; the usual dialogs

(defun dialog-button (text function &optional default)
  (make-instance 'push-button :text text :default-p default :callback-type :none :callback function))

(defun make-dialog-interface (title message panes buttons)
  (make-instance 'interface
                 :title title
                 :layout (make-instance
                          'column-layout
                          :class "ui-dialog-body"
                          :description (append (and message (list (make-instance 'display-pane :text message)))
                                               panes
                                               (list (make-instance 'row-layout :class "ui-dialog-buttons"
                                                                                :stretch nil
                                                                                :description buttons))))))

(defun display-message (control &rest arguments)
  "Show the message made by FORMAT from CONTROL and ARGUMENTS until the user closes it."
  (display-dialog (make-dialog-interface nil (apply #'format nil control arguments) nil
                                         (list (dialog-button *ok-label* (lambda () (exit-dialog t)) t)))))

(defun prompt-for-confirmation (message &key continuation title)
  "Ask the yes-or-no question MESSAGE; CONTINUATION is called with the
answer (true or false) and T, or with NIL and NIL if the dialog was cancelled."
  (display-dialog (make-dialog-interface title message nil
                                         (list (dialog-button *no-label* (lambda () (exit-dialog nil)))
                                               (dialog-button *yes-label* (lambda () (exit-dialog t)) t)))
                  :continuation continuation))

(defun prompt-for-string (message &key (text "") continuation title placeholder
                                    (ok-check (constantly t)))
  "Ask for a string, initially TEXT; CONTINUATION is called with it and T, or
with NIL and NIL.  The dialog can be answered while OK-CHECK, a function of
the string, returns true."
  (let* ((input nil)
         (answer (lambda ()
                   (when (funcall ok-check (text-input-pane-text input))
                     (exit-dialog (text-input-pane-text input))))))
    (setf input (make-instance 'text-input-pane :text text :placeholder placeholder
                                                :callback-type :none :callback answer))
    (display-dialog (make-dialog-interface
                     title message (list input)
                     (list (dialog-button *abort-label* #'abort-dialog)
                           (make-instance 'push-button
                                          :text *ok-label* :default-p t :callback-type :none
                                          :callback answer
                                          :enabled (computed (and (funcall ok-check (text-input-pane-text input))
                                                                  t)))))
                    :continuation continuation)))

(defun prompt-for-number (message &key (value nil) continuation title (integer nil))
  "Ask for a number (an integer if INTEGER is true), like PROMPT-FOR-STRING."
  (flet ((number-of (string)
           (let ((number (parse-number string)))
             (and number (or (not integer) (integerp number)) number))))
    (prompt-for-string message
                       :text (if value (princ-to-string value) "")
                       :title title
                       :ok-check #'number-of
                       :continuation (lambda (string okp)
                                       (when continuation
                                         (funcall continuation (and okp (number-of string)) okp))))))

(defun prompt-with-list (items message &key continuation title print-function
                                         (selected-item (first items)))
  "Ask for one of ITEMS; CONTINUATION is called with it and T, or with NIL and NIL."
  (let* ((answer (lambda (item) (exit-dialog item)))
         (list (make-instance 'list-panel :items items :print-function print-function
                                          :selected-item selected-item
                                          :callback-type :data :action-callback answer
                                          :min-height 120 :max-height 320)))
    (display-dialog (make-dialog-interface
                     title message (list list)
                     (list (dialog-button *abort-label* #'abort-dialog)
                           (make-instance 'push-button
                                          :text *ok-label* :default-p t :callback-type :none
                                          :callback (lambda () (exit-dialog (choice-selected-item list)))
                                          :enabled (computed (and (choice-selected-item list) t)))))
                    :continuation continuation)))

;;; ACCEPT of the types that are typed or chosen rather than clicked

(defun when-answered (continuation)
  (lambda (value okp)
    (when okp (funcall continuation value))))

(defmethod accept-presentation-type ((name (eql 'string)) type prompt continuation)
  (prompt-for-string prompt :continuation (when-answered continuation)))

(defmethod accept-presentation-type ((name (eql 'integer)) type prompt continuation)
  (prompt-for-number prompt :integer t :continuation (when-answered continuation)))

(defmethod accept-presentation-type ((name (eql 'number)) type prompt continuation)
  (prompt-for-number prompt :continuation (when-answered continuation)))

(defmethod accept-presentation-type ((name (eql 'boolean)) type prompt continuation)
  (prompt-for-confirmation prompt :continuation (when-answered continuation)))

(defmethod accept-presentation-type ((name (eql 'member)) type prompt continuation)
  (prompt-with-list (cdr type) prompt :continuation (when-answered continuation)))

;;; ------------------------------------------------------------------
;;; An error without restarts to choose is shown in a dialog of the page

(setf *error-message-function*
      (lambda (message)
        ;; not more of them than can be read (an error in every rendering)
        (when (< (length *dialogs*) 3)
          (display-message "~A" message))))

;;; An error with restarts asks which to invoke in a dialog, where the
;;; failing computation can wait for the answer (see WAIT-FOR-ANSWER;
;;; otherwise the backend asks in its own way)

(defun choose-restart-in-dialog (condition restarts)
  "Show the message of CONDITION with a button for each of RESTARTS (the last
is ABORT); return the chosen one, or NIL if the dialog was cancelled."
  (wait-for-answer
   (lambda (answer)
     (display-dialog
      (make-dialog-interface
       nil (princ-to-string condition) nil
       (loop for (restart . more) on restarts
             collect (let ((restart restart))
                       (dialog-button (princ-to-string restart)
                                      (lambda () (exit-dialog restart))
                                      ;; the first is what the program suggests
                                      (and more (eq restart (first restarts)))))))
      :continuation (lambda (restart okp)
                      (funcall answer (and okp restart)))))))

(setf *restart-chooser* #'choose-restart-in-dialog)

;;; ------------------------------------------------------------------
;;; The command palette: every command by its name (as in the interactor
;;; of CLIM, a command is typed, and asks for its arguments)

(defvar *command-palette-prompt* "Type the name of a command")

(defun palette-commands (filter)
  (remove-if-not (lambda (command)
                   (and (not (eq (command-name command) 'command-palette))
                        (command-active-p command)
                        (command-enabled-p (command-name command))
                        (search filter (command-label command) :test #'char-equal)))
                 *commands*))

(define-command (command-palette :label "Command palette…" :key "CmdOrCtrl+Shift+P") ()
  (let* ((input nil)
         (list (make-instance 'list-panel
                              :items (computed (palette-commands (text-input-pane-text input)))
                              :print-function (lambda (command)
                                                (html (:span :class "ui-palette-item"
                                                        (:span (command-label command))
                                                        (:span :class "ui-menu-key"
                                                          (or (key-label (command-key command)) "")))))
                              :min-height 160 :max-height 320))
         (run (lambda (&optional (command (or (choice-selected-item list)
                                              (first (collection-items list)))))
                (when command
                  (exit-dialog nil)
                  (invoke-command (command-name command))))))
    (setf input (make-instance 'text-input-pane :placeholder *command-palette-prompt*
                                                :navigates list
                                                :callback-type :none :callback run)
          (pane-callback-type list) :data
          (choice-action-callback list) run)
    ;; what is typed selects the first command
    (setf (text-input-pane-change-callback input)
          (lambda () (setf (choice-selected-item list) (first (collection-items list)))))
    (display-dialog (make-instance 'interface
                                   :class "ui-command-palette"
                                   :layout (make-instance 'column-layout :description (list input list))))))
