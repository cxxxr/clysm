;;;; desktop.lisp — the UI backend for the desktop application (Electron)
;;;;
;;;; ui/electron/ is an Electron shell that shows a page; when a program runs
;;;; in it, this file makes DESKTOP-BACKEND the backend: the DOM backend, plus
;;;; a menu bar built from the commands, native dialogs (also for errors) and
;;;; files.  In a browser this file does nothing, and DESKTOP-P is false.
;;;;
;;;; Compile it after dom.lisp.

(in-package "UI")

(defclass desktop-backend (dom-backend)
  ((api :initform (js:global "desktop") :reader desktop-api)))

(defun desktop-p ()
  "True if the program runs in the desktop application."
  (typep *backend* 'desktop-backend))

(defun desktop-call (name &rest arguments)
  "Call the function NAME of the shell (see ui/electron/main.js)."
  (apply #'js:call (desktop-api *backend*) "call" name arguments))

;;; files

(define-condition desktop-file-error (error)
  ((path :initarg :path :reader desktop-file-error-path)
   (message :initarg :message :reader desktop-file-error-message))
  (:report (lambda (condition stream)
             (format stream "~A: ~A" (desktop-file-error-path condition)
                     (desktop-file-error-message condition)))))

(defun read-file (path)
  "The contents of the text file PATH, or NIL if it cannot be read."
  (desktop-call "readFile" path))

(defun write-file (path text)
  "Write TEXT to the file PATH; signal DESKTOP-FILE-ERROR if that fails."
  (let ((message (desktop-call "writeFile" path text)))
    (when (stringp message)
      (error 'desktop-file-error :path path :message message))
    path))

(defun user-data-file (name)
  "The path of the file NAME in the directory for the application's own data."
  (desktop-call "userDataFile" name))

(defun add-recent-document (path)
  (desktop-call "addRecentDocument" path))

;;; dialogs

(defun file-filters (filters)
  ;; ((name extension...)...)
  (mapcar (lambda (filter) (js:object "name" (car filter) "extensions" (cdr filter)))
          filters))

(defun choose-file-to-open (&key title filters)
  "Ask for a file to open; FILTERS is a list of (name extension...).
Return its path, or NIL."
  (desktop-call "openDialog" (js:object "title" title "filters" (file-filters filters))))

(defun choose-file-to-save (&key title default-name filters)
  "Ask for a file name to save to; return the path, or NIL."
  (desktop-call "saveDialog" (js:object "title" title "defaultName" default-name
                                        "filters" (file-filters filters))))

(defun message-box (message &key detail (buttons '("OK")) (type "info") (cancel (1- (length buttons))))
  "Show MESSAGE (and DETAIL) with BUTTONS; return the index of the chosen
button (CANCEL, by default the last, if the box is closed).  TYPE is
\"info\", \"warning\", \"error\" or \"question\"."
  (desktop-call "messageBox" (js:object "message" message "detail" detail "buttons" buttons
                                        "type" type "cancelId" cancel)))

(defmethod choose-restart ((backend desktop-backend) condition restarts)
  (let ((message (princ-to-string condition)))
    (js:call (js:global "console") "error" message)
    (nth (message-box message :buttons (mapcar #'princ-to-string restarts) :type "error")
         restarts)))

;;; the menu bar

(defun command-id (command)
  (let ((name (command-name command)))
    (format nil "~A::~A" (package-name (symbol-package name)) (symbol-name name))))

(defun menu-specification (labels)
  (js:object
   "menus" (mapcar (lambda (menu)
                     (js:object "label" (car menu)
                                "items" (mapcar (lambda (command)
                                                  (js:object "id" (command-id command)
                                                             "label" (command-label command)
                                                             "key" (command-key command)
                                                             "separator" (and (command-separator command) t)
                                                             "enabled" (if (command-enabled-p (command-name command))
                                                                           t :false)))
                                                (cdr menu))))
                   (command-menus))
   "labels" labels))

(defmethod install-commands ((backend desktop-backend) &key edit-label view-label quit-label)
  "Show the commands that have a menu in the menu bar, followed by the
standard Edit and View menus (titled EDIT-LABEL and VIEW-LABEL; the first
menu ends with Quit, QUIT-LABEL).  The menu bar follows the commands: their
definitions and whether they are enabled."
  (let ((*backend* backend))
    (js:call (desktop-api backend) "onMenu"
             (lambda (id)
               (let ((command (find id *commands* :key #'command-id :test #'string=)))
                 (when command (invoke-command (command-name command))))
               nil))
    (autorun (lambda ()
               (desktop-call "setMenu"
                             (menu-specification (js:object "edit" edit-label "view" view-label
                                                            "quit" quit-label)))))))

(when (js:js-value-p (js:global "desktop"))
  (setf *backend* (make-instance 'desktop-backend)))
