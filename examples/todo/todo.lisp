;;;; todo.lisp — a TODO list that runs in the browser
;;;;
;;;; The whole application is Lisp compiled to WebAssembly, written with the
;;;; UI framework (ui/): the state is CLOS objects and DEFSTATE variables, and
;;;; the components follow them — no code here updates the page.
;;;; Build with examples/todo/build.sh and open build/todo/index.html.
;;;; The same program is a desktop application (examples/todo/build-desktop.sh,
;;;; in the Electron shell ui/electron), where it saves to a file and its
;;;; commands are in the menu bar.

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

;;; ------------------------------------------------------------------
;;; Persistence, as JSON: in a file in the desktop application
;;; (ui/desktop.lisp), otherwise in localStorage

(defparameter +storage-key+ "clysm-todo")
(defparameter +filters+ '(("TODO リスト" "json")))

(defun storage () (js:global "localStorage"))

(define-condition not-a-todo-list (error)
  ((path :initarg :path :initform nil :reader not-a-todo-list-path))
  (:report (lambda (condition stream)
             (format stream "~@[~A は~]TODOリストではありません。" (not-a-todo-list-path condition)))))

(defun todos-json ()
  (js:call (js:global "JSON") "stringify"
           (js:to-js (mapcar (lambda (todo)
                               (js:object "text" (todo-text todo)
                                          "done" (if (todo-done todo) t :false)))
                             *todos*))))

(defun parse-todos (json &optional path)
  "The tasks of a saved list; the error NOT-A-TODO-LIST unless JSON is one."
  (flet ((fail () (error 'not-a-todo-list :path path)))
    (let ((array (handler-case (js:call (js:global "JSON") "parse" json)
                   (error () (fail)))))
      (unless (js:call (js:global "Array") "isArray" array) (fail))
      (loop for i below (js:get array "length")
            for item = (js:get array i)
            for text = (and (js:js-value-p item) (js:get item "text"))
            unless (stringp text) do (fail)
            collect (make-instance 'todo :text text :done (eq (js:get item "done") t))))))

(defun save ()
  "Called (by AUTORUN) whenever the tasks or the file have changed."
  (let ((json (todos-json)) (file *file*))
    (cond ((not (desktop-p))
           (ignore-errors (js:call (storage) "setItem" +storage-key+ json)))
          (file
           ;; if the file cannot be written, the user chooses what to do
           (restart-case (write-file file json)
             (retry ()
               :report "もう一度保存する"
               (save))
             (save-elsewhere ()
               :report "別の場所に保存する…"
               (save-list-as)))))))

;;; the desktop application remembers the file of the list in its settings

(defun settings-file () (user-data-file "settings.json"))

(defun last-file ()
  (or (let ((text (read-file (settings-file))))
        (and text
             (ignore-errors
              (let ((settings (js:call (js:global "JSON") "parse" text)))
                (and (js:js-value-p settings)
                     (stringp (js:get settings "file"))
                     (js:get settings "file"))))))
      (user-data-file "todos.json")))

(defun use-file (path)
  (setf *file* path)
  (add-recent-document path)
  (ignore-errors
   (write-file (settings-file)
               (js:call (js:global "JSON") "stringify" (js:object "file" path)))))

(defun load-file (path)
  "Show the list in the file PATH (an empty one if there is no such file)."
  (let ((text (read-file path)))
    (setf *todos* (and text (parse-todos text path))
          *file* path)))

(defun load-todos ()
  (if (desktop-p)
      ;; if this fails the list has no file: the file is left alone
      (load-file (last-file))
      (let ((json (ignore-errors (js:call (storage) "getItem" +storage-key+))))
        (when (stringp json)
          (ignore-errors (setf *todos* (parse-todos json)))))))

;;; ------------------------------------------------------------------
;;; Commands: called from the buttons, the menu bar of the desktop
;;; application, their keys, or the REPL

(define-command (new-list :menu "ファイル" :label "新しいリスト…" :key "CmdOrCtrl+N"
                          :enabled (desktop-p))
    ()
  (let ((path (choose-file-to-save :title "新しいリスト" :default-name "todo.json"
                                   :filters +filters+)))
    (when path
      (setf *todos* nil)
      (use-file path))))

(define-command (open-list :menu "ファイル" :label "開く…" :key "CmdOrCtrl+O"
                           :enabled (desktop-p))
    ()
  (let ((path (choose-file-to-open :filters +filters+)))
    (when path
      (restart-case (progn (load-file path)
                           (use-file path))
        (choose-another ()
          :report "別のファイルを選ぶ…"
          (open-list))))))

(define-command (save-list-as :menu "ファイル" :label "名前を付けて保存…" :key "CmdOrCtrl+Shift+S"
                              :enabled (desktop-p))
    ()
  (let ((path (choose-file-to-save :title "名前を付けて保存" :default-name "todo.json"
                                   :filters +filters+)))
    (when path (use-file path))))

(define-command (complete-all :menu "リスト" :label "すべて完了にする"
                              :enabled (notevery #'todo-done *todos*))
    ()
  (dolist (todo *todos*) (setf (todo-done todo) t)))

(define-command (clear-completed :menu "リスト" :label "完了済みを削除"
                                 :key "CmdOrCtrl+Shift+Backspace"
                                 :enabled (some #'todo-done *todos*))
    ()
  (setf *todos* (remove-if #'todo-done *todos*)))

(define-command delete-todo (todo)
  (setf *todos* (remove todo *todos*)))

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
      (:button :class "delete" :title "削除" :on-click (command delete-todo todo)
               "×"))))

(defcomponent filter-button (filter label)
  (:button :class (if (eq filter *filter*) "filter selected" "filter")
           :on-click (lambda (e) (declare (ignore e)) (setf *filter* filter))
           label))

(defcomponent note ()
  (:p :class "note" :id "note"
    "ダブルクリックで編集 ・ "
    (cond ((not (desktop-p)) "データはブラウザの localStorage に保存されます")
          (*file* (format nil "保存先: ~A" *file*))
          (t "保存先がありません（ファイル → 名前を付けて保存）"))))

(defcomponent app ()
  (let ((visible (visible-todos))
        (active (count-if-not #'todo-done *todos*)))
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
          (:button :id "clear" :hidden (not (command-enabled-p 'clear-completed))
                   :on-click (command clear-completed)
                   "完了済みを削除")))
      (note))))

;;; ------------------------------------------------------------------
;;; Startup

(defun update-title ()
  (js:set (js:global "document") "title"
          (format nil "~@[~A — ~]TODO（残り ~D 件）"
                  (and *file* (file-name *file*))
                  (count-if-not #'todo-done *todos*))))

(setf *abort-label* "キャンセル")
(with-error-dialog (load-todos))
(mount (app) "#app")
(install-commands *backend* :edit-label "編集" :view-label "表示" :quit-label "終了")
;; from now on, every change of the tasks is saved and shown in the title
(autorun #'save)
(autorun #'update-title)
