;;;; todo.lisp — a TODO list that runs in the browser
;;;;
;;;; The whole application is Lisp compiled to WebAssembly: it builds the DOM,
;;;; handles events and saves to localStorage through the JS package.
;;;; Build with examples/todo/build.sh and open build/todo/index.html.
;;;; The same program is a desktop app in examples/todo/desktop (Electron),
;;;; where it saves to a file and answers the commands of the menu.

;;; ------------------------------------------------------------------
;;; A small DOM layer

(defvar *document* (js:global "document"))

(defun $ (selector) (js:call *document* "querySelector" selector))

(defun on (element event handler)
  (js:call element "addEventListener" event handler))

(defun prop-name (key)
  (case key
    (:class "className")
    (:text "textContent")
    (:for "htmlFor")
    (t (string-downcase (symbol-name key)))))

(defun event-key-p (key)
  "Keys like :ON-CLICK attach event listeners (here for \"click\")."
  (let ((name (symbol-name key)))
    (and (> (length name) 3) (string= "ON-" name :end2 3))))

(defun h (tag props &rest children)
  "Make a TAG element. PROPS is a plist of properties and :ON-<EVENT>
handlers; CHILDREN are elements or strings (NIL is skipped)."
  (let ((e (js:call *document* "createElement" tag)))
    (loop for (k v) on props by #'cddr
          do (if (event-key-p k)
                 (on e (string-downcase (subseq (symbol-name k) 3)) v)
                 (js:set e (prop-name k) v)))
    (dolist (c children e)
      (when c (js:call e "append" c)))))

(defun js-bool (x) (if x t :false))

(defun replace-children (parent &rest children)
  (js:set parent "textContent" "")
  (dolist (c children) (js:call parent "append" c)))

;;; ------------------------------------------------------------------
;;; State

(defstruct todo id text done)

(defvar *todos* nil "The tasks, oldest first.")
(defvar *next-id* 1)
(defvar *filter* :all "One of :ALL, :ACTIVE and :DONE.")
(defvar *editing* nil "The id of the task being edited, or NIL.")

(defun visible-todos ()
  (remove-if-not (ecase *filter*
                   (:all (constantly t))
                   (:active (complement #'todo-done))
                   (:done #'todo-done))
                 *todos*))

(defun add-todo (text)
  (setf *todos* (append *todos* (list (make-todo :id *next-id* :text text))))
  (incf *next-id*))

(defun find-todo (id) (find id *todos* :key #'todo-id))

(defun delete-todo (id)
  (setf *todos* (remove id *todos* :key #'todo-id)))

;;; ------------------------------------------------------------------
;;; Persistence, as JSON: in a file when the page runs in the desktop app
;;; (examples/todo/desktop, which provides the global `desktop`), otherwise
;;; in localStorage

(defparameter +storage-key+ "clysm-todo")

(defun storage () (js:global "localStorage"))

(defun desktop ()
  "The desktop app's API (see desktop/preload.js), or NIL in a browser."
  (js:global "desktop"))

(defvar *file* nil "In the desktop app, the path of the list file (NIL: none).")

(defun todos-json ()
  (js:call (js:global "JSON") "stringify"
           (js:to-js (mapcar (lambda (todo)
                               (js:object "text" (todo-text todo)
                                          "done" (js-bool (todo-done todo))))
                             *todos*))))

(defun parse-todos (json)
  "The tasks of a saved list as (TEXT . DONE) pairs; an error unless JSON is one."
  (let ((array (js:call (js:global "JSON") "parse" json)))
    (unless (js:call (js:global "Array") "isArray" array)
      (error "not a TODO list"))
    (loop for i below (js:get array "length")
          for item = (js:get array i)
          for text = (and (js:js-value-p item) (js:get item "text"))
          unless (stringp text) do (error "not a TODO list")
          collect (cons text (eq (js:get item "done") t)))))

(defun set-todos (pairs)
  (setf *todos* nil *next-id* 1 *editing* nil)
  (loop for (text . done) in pairs
        do (add-todo text)
           (setf (todo-done (car (last *todos*))) done)))

(defun save ()
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
          (handler-case (set-todos (parse-todos text))
            (error ()
              (alert (format nil "~A を読み込めませんでした。" *file*))
              ;; leave the file alone
              (setf *file* nil)))))
      (let ((json (ignore-errors (js:call (storage) "getItem" +storage-key+))))
        (when (stringp json)
          (ignore-errors (set-todos (parse-todos json)))))))

;;; the commands of the desktop app's menu

(defun use-file (path)
  (setf *file* path)
  (js:call (desktop) "setFile" path)
  (render))

(defun menu-command (command)
  (cond ((string= command "new")
         (let ((path (js:call (desktop) "chooseFile" "新しいリスト")))
           (when (stringp path)
             (set-todos nil)
             (use-file path))))
        ((string= command "open")
         (let ((file (js:call (desktop) "open")))
           (when (js:js-value-p file)
             (let ((path (js:get file "path")) (text (js:get file "text")))
               (handler-case (set-todos (parse-todos text))
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
;;; Rendering

(defun commit-edit (id input)
  (when (eql *editing* id)
    (setf *editing* nil)
    (let ((text (string-trim " " (js:get input "value")))
          (todo (find-todo id)))
      (cond ((zerop (length text)) (delete-todo id))
            (todo (setf (todo-text todo) text))))
    (render)))

(defun edit-view (todo)
  (let* ((id (todo-id todo))
         (input (h "input" (list :class "edit" :value (todo-text todo)))))
    (on input "keydown"
        (lambda (e)
          (unless (js:get e "isComposing")      ; Enter that confirms IME input
            (let ((key (js:get e "key")))
              (cond ((string= key "Enter") (commit-edit id input))
                    ((string= key "Escape") (setf *editing* nil) (render)))))))
    (on input "blur" (lambda (e) (declare (ignore e)) (commit-edit id input)))
    input))

(defun todo-view (todo)
  (let ((id (todo-id todo)))
    (h "li" (list :class (if (todo-done todo) "item done" "item"))
       (h "input" (list :type "checkbox"
                        :checked (js-bool (todo-done todo))
                        :on-change (lambda (e)
                                     (declare (ignore e))
                                     (setf (todo-done todo) (not (todo-done todo)))
                                     (render))))
       (if (eql *editing* id)
           (edit-view todo)
           (h "span" (list :class "text"
                           :text (todo-text todo)
                           :title "ダブルクリックで編集"
                           :on-dblclick (lambda (e)
                                          (declare (ignore e))
                                          (setf *editing* id)
                                          (render)))))
       (h "button" (list :class "delete" :text "×" :title "削除"
                         :on-click (lambda (e)
                                     (declare (ignore e))
                                     (delete-todo id)
                                     (render)))))))

(defun filter-button (filter label)
  (h "button" (list :class (if (eq filter *filter*) "filter selected" "filter")
                    :text label
                    :on-click (lambda (e)
                                (declare (ignore e))
                                (setf *filter* filter)
                                (render)))))

(defun render ()
  (let* ((visible (visible-todos))
         (active (count-if-not #'todo-done *todos*))
         (done (- (length *todos*) active)))
    (apply #'replace-children ($ "#list") (mapcar #'todo-view visible))
    (js:set ($ "#empty") "hidden" (js-bool visible))
    (js:set ($ "#empty") "textContent"
            (ecase *filter*
              (:all "タスクはまだありません")
              (:active "未完了のタスクはありません")
              (:done "完了したタスクはありません")))
    (js:set ($ "#footer") "hidden" (js-bool (null *todos*)))
    (js:set ($ "#count") "textContent" (format nil "残り ~D 件" active))
    (js:set *document* "title"
            (format nil "~@[~A — ~]TODO（残り ~D 件）" (and *file* (file-name *file*)) active))
    (when (desktop)
      (js:set ($ "#note") "textContent"
              (if *file*
                  (format nil "ダブルクリックで編集 ・ 保存先: ~A" *file*)
                  "ダブルクリックで編集 ・ 保存先がありません（ファイル → 名前を付けて保存）")))
    (replace-children ($ "#filters")
                      (filter-button :all "すべて")
                      (filter-button :active "未完了")
                      (filter-button :done "完了"))
    (js:set ($ "#clear") "hidden" (js-bool (zerop done)))
    (js:set ($ "#toggle-all") "checked" (js-bool (and *todos* (zerop active))))
    ;; focus the field of the task being edited
    (let ((edit ($ "#list .edit")))
      (when (js:js-value-p edit) (js:call edit "focus")))
    (save)))

;;; ------------------------------------------------------------------
;;; Startup

(defun setup ()
  (let ((input ($ "#new")))
    (on input "keydown"
        (lambda (e)
          (when (and (string= (js:get e "key") "Enter")
                     (not (js:get e "isComposing")))
            (let ((text (string-trim " " (js:get input "value"))))
              (when (plusp (length text))
                (add-todo text)
                (js:set input "value" "")
                (render)))))))
  (on ($ "#toggle-all") "change"
      (lambda (e)
        (let ((done (eq (js:get (js:get e "target") "checked") t)))
          (dolist (todo *todos*) (setf (todo-done todo) done))
          (render))))
  (on ($ "#clear") "click"
      (lambda (e)
        (declare (ignore e))
        (setf *todos* (remove-if #'todo-done *todos*))
        (render)))
  (when (desktop)
    (js:call (desktop) "onMenu" #'menu-command))
  (load-todos)
  (render))

(setup)
