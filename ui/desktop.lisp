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
  ((api :initform (js:global "desktop") :reader desktop-api))
  (:documentation "The backend of a page in the desktop application (ui/electron): the
DOM backend with a native menu bar, native dialogs and files.  Loading
desktop.lisp makes it *BACKEND* when the program runs there."))

(defmethod native-menus-p ((backend desktop-backend)) t)

(defun desktop-p ()
  "True if the program runs in the desktop application."
  (typep *backend* 'desktop-backend))

(defun desktop-call (name &rest arguments)
  "Call the function NAME of the shell (see ui/electron/main.js)."
  (apply #'js:call (desktop-api *backend*) "call" name arguments))

;;; files

(define-condition desktop-file-error (error)
  (;; the file
   (path :initarg :path :reader desktop-file-error-path)
   ;; why it could not be written
   (message :initarg :message :reader desktop-file-error-message))
  (:documentation "Signalled by WRITE-FILE when a file cannot be written.")
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
  "Add the file PATH to the recent documents of the system (the menu of the
application in the dock or the task bar)."
  (desktop-call "addRecentDocument" path))

;;; dialogs

(defmethod choose-file ((backend desktop-backend) continuation &key accept)
  (declare (ignore accept))
  (let ((path (choose-file-to-open)))
    (if path
        (funcall continuation path (read-file path))
        (funcall continuation nil nil))))

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

(defvar *menu-functions* nil "The functions of the items of the menu bar, by their ids.")

(defun menu-item-specification (item)
  (if (eq item :separator)
      (js:object "separator" t)
      (let ((id (length *menu-functions*)))
        (setf *menu-functions* (append *menu-functions* (list (menu-item-function item))))
        (js:object "id" id
                   "label" (menu-item-label item)
                   "key" (menu-item-key item)
                   "enabled" (if (menu-item-enabled item) t :false)
                   "checked" (case (menu-item-checked item)
                               (:none nil)
                               ((nil) :false)
                               (t t))
                   "items" (and (menu-item-items item)
                                (mapcar #'menu-item-specification (menu-item-items item)))))))

(defun menu-specification (labels)
  (setf *menu-functions* nil)
  (js:object
   "menus" (mapcar (lambda (menu)
                     (js:object "label" (car menu)
                                "items" (mapcar #'menu-item-specification (cdr menu))))
                   (application-menus))
   "labels" labels))

(defmethod install-commands ((backend desktop-backend) &key edit-label view-label quit-label)
  "Show the menus of the application (APPLICATION-MENUS: those of the
commands, or of the interfaces on the screen) in the menu bar, followed by
the standard Edit and View menus (titled EDIT-LABEL and VIEW-LABEL; the first
menu ends with Quit, QUIT-LABEL).  The menu bar follows the state the menus
read: what there is and whether it is enabled or checked."
  ;; the keys of what is in the menus are those of the menu items
  (install-command-keys backend
                        :command-predicate (lambda (command) (null (command-menu command)))
                        :handlers nil)
  (let ((*backend* backend))
    (js:call (desktop-api backend) "onMenu"
             (lambda (id)
               (let ((function (and (integerp id) (nth id *menu-functions*))))
                 (when function
                   (let ((*backend* backend))
                     (with-error-dialog (funcall function))
                     (flush))))
               nil))
    (autorun (lambda ()
               (desktop-call "setMenu"
                             (menu-specification (js:object "edit" edit-label "view" view-label
                                                            "quit" quit-label)))))))

(when (js:js-value-p (js:global "desktop"))
  (setf *backend* (make-instance 'desktop-backend)))
