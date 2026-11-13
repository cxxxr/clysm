;;;; todo.lisp — a TODO list that runs in the browser
;;;;
;;;; The whole application is Lisp compiled to WebAssembly, written with the
;;;; UI framework (ui/): the state is CLOS objects and DEFSTATE variables, and
;;;; the components follow them — no code here updates the page.
;;;; Build with examples/todo/build.sh and open build/todo/index.html.
;;;; The same program is a desktop app in examples/todo/desktop (Electron),
;;;; where it saves to a file and answers the commands of the menu.

(defpackage "TODO"
  (:use "CL" "UI"))

(in-package "TODO")

;;; ------------------------------------------------------------------
;;; State

(defmodel todo ()
  ((text :initarg :text :accessor todo-text)
   (done :initarg :done :initform nil :accessor todo-done)))

(defstate *todos* nil "The tasks, oldest first.")
(defstate *filter* :all "One of :ALL, :ACTIVE and :DONE.")
(defstate *file* nil "In the desktop app, the path of the list file (NIL: none).")

(defun visible-todos ()
  (ecase *filter*
    (:all *todos*)
    (:active (remove-if #'todo-done *todos*))
    (:done (remove-if-not #'todo-done *todos*))))

(defun add-todo (text)
  (setf *todos* (append *todos* (list (make-instance 'todo :text text)))))

(defun delete-todo (todo)
  (setf *todos* (remove todo *todos*)))

;;; ------------------------------------------------------------------
;;; Persistence, as JSON: in a file when the page runs in the desktop app
;;; (examples/todo/desktop, which provides the global `desktop`), otherwise
;;; in localStorage

(defparameter +storage-key+ "clysm-todo")

(defun storage () (js:global "localStorage"))

(defun desktop ()
  "The desktop app's API (see desktop/preload.js), or NIL in a browser."
  (js:global "desktop"))

(defun todos-json ()
  (js:call (js:global "JSON") "stringify"
           (js:to-js (mapcar (lambda (todo)
                               (js:object "text" (todo-text todo)
                                          "done" (if (todo-done todo) t :false)))
                             *todos*))))

(defun parse-todos (json)
  "The tasks of a saved list; an error unless JSON is one."
  (let ((array (js:call (js:global "JSON") "parse" json)))
    (unless (js:call (js:global "Array") "isArray" array)
      (error "not a TODO list"))
    (loop for i below (js:get array "length")
          for item = (js:get array i)
          for text = (and (js:js-value-p item) (js:get item "text"))
          unless (stringp text) do (error "not a TODO list")
          collect (make-instance 'todo :text text :done (eq (js:get item "done") t)))))

(defun save ()
  "Called (by AUTORUN) whenever the tasks or the file have changed."
  (let ((json (todos-json)))
    (if (desktop)
        (when *file* (js:call (desktop) "save" *file* json))
        (ignore-errors (js:call (storage) "setItem" +storage-key+ json)))))

(defun alert (message) (js:call (js:global) "alert" message))

(defun load-todos ()
  (if (desktop)
      (let* ((last (js:call (desktop) "load"))
             (text (js:get last "text")))
        (setf *file* (js:get last "path"))
        (when (stringp text)
          (handler-case (setf *todos* (parse-todos text))
            (error ()
              (alert (format nil "~A を読み込めませんでした。" *file*))
              ;; leave the file alone
              (setf *file* nil)))))
      (let ((json (ignore-errors (js:call (storage) "getItem" +storage-key+))))
        (when (stringp json)
          (ignore-errors (setf *todos* (parse-todos json)))))))

;;; the commands of the desktop app's menu

(defun use-file (path)
  (setf *file* path)
  (js:call (desktop) "setFile" path))

(defun menu-command (command)
  (cond ((string= command "new")
         (let ((path (js:call (desktop) "chooseFile" "新しいリスト")))
           (when (stringp path)
             (setf *todos* nil)
             (use-file path))))
        ((string= command "open")
         (let ((file (js:call (desktop) "open")))
           (when (js:js-value-p file)
             (let ((path (js:get file "path")))
               (handler-case (setf *todos* (parse-todos (js:get file "text")))
                 (error ()
                   (alert (format nil "~A はTODOリストではありません。" path))
                   (return-from menu-command)))
               (use-file path)))))
        ((string= command "save-as")
         (let ((path (js:call (desktop) "chooseFile" "名前を付けて保存")))
           (when (stringp path) (use-file path))))))

(defun file-name (path)
  (subseq path (1+ (or (position-if (lambda (c) (member c '(#\/ #\\))) path :from-end t) -1))))

;;; ------------------------------------------------------------------
;;; Components

(defun enter-key-p (event)
  (and (string= (js:get event "key") "Enter")
       (not (js:get event "isComposing"))))      ; not the Enter that confirms IME input

(defun input-text (event)
  (string-trim " " (js:get (js:get event "target") "value")))

(defcomponent todo-item (todo &state (editing nil))
  (flet ((commit (event)
           (when editing
             (setf editing nil)
             (let ((text (input-text event)))
               (if (zerop (length text))
                   (delete-todo todo)
                   (setf (todo-text todo) text))))))
    (:li :class (if (todo-done todo) "item done" "item")
      (:input :type "checkbox" :checked (todo-done todo)
              :on-change (lambda (e)
                           (declare (ignore e))
                           (setf (todo-done todo) (not (todo-done todo)))))
      (if editing
          (:input :class "edit" :value (todo-text todo)
                  :on-mount (lambda (node) (js:call node "focus"))
                  :on-keydown (lambda (e)
                                (cond ((enter-key-p e) (commit e))
                                      ((string= (js:get e "key") "Escape") (setf editing nil))))
                  :on-blur #'commit)
          (:span :class "text" :title "ダブルクリックで編集"
                 :on-dblclick (lambda (e) (declare (ignore e)) (setf editing t))
                 (todo-text todo)))
      (:button :class "delete" :title "削除"
               :on-click (lambda (e) (declare (ignore e)) (delete-todo todo))
               "×"))))

(defcomponent filter-button (filter label)
  (:button :class (if (eq filter *filter*) "filter selected" "filter")
           :on-click (lambda (e) (declare (ignore e)) (setf *filter* filter))
           label))

(defcomponent note ()
  (:p :class "note" :id "note"
    "ダブルクリックで編集 ・ "
    (cond ((not (desktop)) "データはブラウザの localStorage に保存されます")
          (*file* (format nil "保存先: ~A" *file*))
          (t "保存先がありません（ファイル → 名前を付けて保存）"))))

(defcomponent app ()
  (let* ((visible (visible-todos))
         (active (count-if-not #'todo-done *todos*))
         (done (- (length *todos*) active)))
    (:div
      (:div :class "card"
        (:div :class "entry"
          (:input :type "checkbox" :id "toggle-all" :title "すべて完了/未完了にする"
                  :checked (and *todos* (zerop active))
                  :on-change (lambda (e)
                               (let ((done (js:get (js:get e "target") "checked")))
                                 (dolist (todo *todos*) (setf (todo-done todo) done)))))
          (:input :type "text" :id "new" :placeholder "やることを入力して Enter"
                  :autocomplete "off"
                  :on-mount (lambda (node) (js:call node "focus"))
                  :on-keydown (lambda (e)
                                (when (enter-key-p e)
                                  (let ((text (input-text e)))
                                    (when (plusp (length text))
                                      (add-todo text)
                                      (js:set (js:get e "target") "value" "")))))))
        (:ul :id "list"
          (mapcar (lambda (todo) (keyed todo (todo-item todo))) visible))
        (:p :id "empty" :hidden (and visible t)
          (ecase *filter*
            (:all "タスクはまだありません")
            (:active "未完了のタスクはありません")
            (:done "完了したタスクはありません")))
        (:footer :id "footer" :hidden (null *todos*)
          (:span :id "count" (format nil "残り ~D 件" active))
          (:span :id "filters"
            (filter-button :all "すべて")
            (filter-button :active "未完了")
            (filter-button :done "完了"))
          (:button :id "clear" :hidden (zerop done)
                   :on-click (lambda (e)
                               (declare (ignore e))
                               (setf *todos* (remove-if #'todo-done *todos*)))
                   "完了済みを削除")))
      (note))))

;;; ------------------------------------------------------------------
;;; Startup

(defun update-title ()
  (js:set (js:global "document") "title"
          (format nil "~@[~A — ~]TODO（残り ~D 件）"
                  (and *file* (file-name *file*))
                  (count-if-not #'todo-done *todos*))))

(when (desktop)
  (js:call (desktop) "onMenu" #'menu-command))
(load-todos)
(mount (app) "#app")
;; from now on, every change of the tasks is saved and shown in the title
(autorun #'save)
(autorun #'update-title)
