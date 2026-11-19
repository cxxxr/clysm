;;;; editor.lisp — a text editor written only in Common Lisp
;;;;
;;;; The editing component is ui/editor/ (the pane EDITOR:CODE-EDITOR); this
;;;; file is the application around it: a toolbar, a status line, and where
;;;; the text is kept.  In the browser the text is kept in localStorage; in
;;;; the desktop application (build-desktop.sh, Electron) it is a file, with
;;;; the menu bar and native dialogs.
;;;; Build with examples/editor/build.sh and open build/editor/index.html.

(defpackage "EDITOR-DEMO"
  (:use "CL" "UI"))

(in-package "EDITOR-DEMO")

(defparameter +sample+ ";;;; Clysm Editor へようこそ
;;;;
;;;; このエディタは、エディタ部品も含めてすべて Common Lisp で書かれ、
;;;; Clysm で WebAssembly にコンパイルされています。
;;;;
;;;; 試せること:
;;;;   Return / Tab        Lisp の字下げ
;;;;   C-M-Right / Left    S 式単位の移動 (C-M-f / C-M-b)
;;;;   Mod-/               コメントの切り替え
;;;;   M-Up / M-Down       行の移動
;;;;   Mod-Z / Mod-Shift-Z 元に戻す / やり直す
;;;;   ツールバーで Emacs のキーにも切り替えられます (C-x C-s, C-k, C-y...)

(defun fizzbuzz (n)
  \"1 から N までの FizzBuzz を返す。\"
  (loop for i from 1 to n
        collect (cond ((zerop (mod i 15)) \"FizzBuzz\")
                      ((zerop (mod i 3)) \"Fizz\")
                      ((zerop (mod i 5)) \"Buzz\")
                      (t i))))

(defmacro with-timing ((&key (label \"time\")) &body body)
  `(let ((start (get-internal-real-time)))
     (multiple-value-prog1 (progn ,@body)
       (format t \"~A: ~D~%\" ,label (- (get-internal-real-time) start)))))

#| 複数行の
   コメント |#
(defvar *table* (make-hash-table :test 'equal))

(flet ((square (x)
         (* x x)))
  (mapcar #'square '(1 2 3 #\\a :key)))
")

(defparameter +storage-key+ "clysm-editor-text")
(defparameter +filters+ '(("Lisp" "lisp" "asd" "lsp") ("テキスト" "txt" "md")))

(defstate *file* nil "In the desktop application, the path of the file (NIL: none).")
(defstate *modified* nil)
(defstate *find-text* "")

(defun file-name (path)
  (subseq path (1+ (or (position-if (lambda (c) (member c '(#\/ #\\))) path :from-end t) -1))))

(defun file-language (path)
  (let ((dot (position #\. path :from-end t)))
    (if dot (subseq path (1+ dot)) "text")))

(defun storage () (js:global "localStorage"))

(declaim (ftype function save-file))

(defvar *editor*
  (make-instance 'editor:code-editor
                 :text +sample+ :language "lisp"
                 :change-callback (lambda (pane)
                                    (setf *modified* (editor:editor-modified (editor:code-editor-editor pane)))
                                    (unless (desktop-p)
                                      (ignore-errors
                                       (js:call (storage) "setItem" +storage-key+ (editor:code-editor-text pane)))))
                 :save-callback (lambda (pane) (declare (ignore pane)) (save-file))))

(defun editor-model () (editor:code-editor-editor *editor*))

;;; ------------------------------------------------------------------
;;; Files (the desktop application)

(defun open-path (path)
  (let ((text (read-file path)))
    (when text
      (setf (editor:code-editor-text *editor*) text
            (editor:code-editor-language *editor*) (file-language path)
            *file* path
            *modified* nil)
      (add-recent-document path)
      (editor:code-editor-focus *editor*))))

(define-command (open-file :menu "ファイル" :label "開く…" :key "CmdOrCtrl+O" :enabled (desktop-p)) ()
  (let ((path (choose-file-to-open :filters +filters+)))
    (when path (open-path path))))

(define-command (save-file-as :menu "ファイル" :label "名前を付けて保存…" :key "CmdOrCtrl+Shift+S"
                              :enabled (desktop-p))
    ()
  (let ((path (choose-file-to-save :default-name (if *file* (file-name *file*) "untitled.lisp")
                                   :filters +filters+)))
    (when path
      (setf *file* path)
      (save-file))))

(define-command (save-file :menu "ファイル" :label "保存" :enabled (desktop-p)) ()
  (cond ((not (desktop-p))
         (setf (editor:editor-message (editor-model)) "ブラウザでは自動で localStorage に保存されます"))
        ((null *file*) (save-file-as))
        (t
         (restart-case (progn (write-file *file* (editor:code-editor-text *editor*))
                              (editor:mark-saved (editor-model))
                              (setf *modified* nil)
                              (setf (editor:editor-message (editor-model)) (format nil "~A に保存しました" (file-name *file*))))
           (save-elsewhere () :report "別の場所に保存する…" (save-file-as))))))

;;; ------------------------------------------------------------------
;;; The toolbar and the status line

(defun option (value label selected)
  (html (:option :value value :selected (and (equal value selected) t) label)))

(defcomponent toolbar ()
  (let ((language (editor::language-name (editor:code-editor-language *editor*))))
    (:header :class "toolbar"
      (:span :class "title"
        (if *file* (file-name *file*) "Clysm Editor")
        (when *modified* (:span :class "dot" :title "保存されていない変更があります" "●")))
      (when (desktop-p)
        (list (:button :on-click (command open-file) "開く")
              (:button :on-click (command save-file) "保存")))
      (:label "言語 "
        (:select :on-change (lambda (e)
                              (setf (editor:code-editor-language *editor*) (js:get (js:get e "target") "value"))
                              (editor:code-editor-focus *editor*))
          (option "lisp" "Common Lisp" (if (string= language "Common Lisp") "lisp" "text"))
          (option "text" "テキスト" (if (string= language "Common Lisp") "lisp" "text"))))
      (:label "キー "
        (:select :on-change (lambda (e)
                              (setf (editor:code-editor-keymap *editor*)
                                    (if (equal (js:get (js:get e "target") "value") "emacs") :emacs :standard))
                              (editor:code-editor-focus *editor*))
          (option "standard" "標準" (string-downcase (editor:code-editor-keymap *editor*)))
          (option "emacs" "Emacs" (string-downcase (editor:code-editor-keymap *editor*)))))
      (:label
        (:input :type "checkbox" :checked (and (editor:code-editor-wrap *editor*) t)
                :on-change (lambda (e)
                             (setf (editor:code-editor-wrap *editor*) (eq (js:get (js:get e "target") "checked") t))
                             (editor:code-editor-focus *editor*)))
        " 折り返す")
      (:input :type "search" :class "find" :placeholder "検索 (Enter / Shift+Enter)"
              :value *find-text*
              :on-input (lambda (e) (setf *find-text* (js:get (js:get e "target") "value")))
              :on-keydown (lambda (e)
                            (cond ((and (equal (js:get e "key") "Enter") (not (js:get e "isComposing")))
                                   (js:call e "preventDefault")
                                   (unless (editor:code-editor-find *editor* *find-text*
                                                                    :backward (eq (js:get e "shiftKey") t))
                                     (setf (editor:editor-message (editor-model)) "見つかりません")))
                                  ((equal (js:get e "key") "Escape")
                                   (editor:code-editor-focus *editor*))))))))

(defcomponent status-line ()
  (multiple-value-bind (line col) (editor:code-editor-cursor *editor*)
    (let* ((model (editor-model))
           (selected (and (not (editor:selection-empty-p model))
                          (length (editor:selected-text model)))))
      (:footer :class "status"
        (:span (format nil "~D 行, ~D 列" (1+ line) (1+ col)))
        (when selected (:span (format nil "~D 文字選択" selected)))
        (:span :class "message" (or (editor:code-editor-message *editor*) ""))
        (:span (format nil "~D 行" (editor:line-count (editor:editor-document model))))
        (:span (editor::language-name (editor:code-editor-language *editor*)))
        (:span (if (eq (editor:code-editor-keymap *editor*) :emacs) "Emacs" "標準"))))))

(defcomponent app ()
  (:div :class "app"
    (toolbar)
    (pane-node *editor*)
    (status-line)))

;;; ------------------------------------------------------------------
;;; Startup

(unless (desktop-p)
  (let ((saved (ignore-errors (js:call (storage) "getItem" +storage-key+))))
    (when (and (stringp saved) (plusp (length saved)))
      (setf (editor:code-editor-text *editor*) saved))))

;; the files the desktop application is asked to open: its command line
;; (electron . FILE), or a second start
(when (desktop-p)
  (let ((desktop (js:global "desktop")))
    (when (js:js-value-p (js:get desktop "onOpen"))
      (js:call desktop "onOpen"
               (lambda (path)
                 (with-error-dialog (open-path path))
                 nil)))))

(setf *abort-label* "キャンセル")
(mount (app) "#app")
(install-commands *backend* :edit-label "編集" :view-label "表示" :quit-label "終了")
(autorun (lambda ()
           (set-title *backend* (format nil "~:[~;● ~]~A — Clysm Editor"
                                        *modified* (if *file* (file-name *file*) "untitled")))))
(editor:code-editor-focus *editor*)
