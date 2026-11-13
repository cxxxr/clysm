;;;; contacts.lisp — an address book, written with the panes of the UI
;;;; framework (ui/pane.lisp, ui/present.lisp, ui/dialog.lisp)
;;;;
;;;; The window is an interface (DEFINE-INTERFACE): panes in layouts.  What
;;;; the panes show are slots of the contacts and of the interface (BINDING,
;;;; COMPUTED), so no code copies values to or from the screen.  The contacts
;;;; on the screen are presentations: a command that needs a contact asks the
;;;; user to click one, and a right click on a contact offers the commands
;;;; for it.
;;;; Build with examples/contacts/build.sh and serve build/contacts/, or with
;;;; examples/contacts/build-desktop.sh for the desktop application.

(defpackage "CONTACTS"
  (:use "CL" "UI"))

(in-package "CONTACTS")

;;; ------------------------------------------------------------------
;;; State

(defparameter +groups+ '("家族" "友人" "仕事"))

(defmodel contact ()
  ((name :initarg :name :accessor contact-name)
   (email :initarg :email :initform "" :accessor contact-email)
   (group :initarg :group :initform "友人" :accessor contact-group)
   (favorite :initarg :favorite :initform nil :accessor contact-favorite)
   (notes :initarg :notes :initform "" :accessor contact-notes)))

(defstate *contacts*
  (list (make-instance 'contact :name "湯川 秀樹" :email "yukawa@example.jp" :group "仕事" :favorite t
                                :notes "中間子の件で相談")
        (make-instance 'contact :name "樋口 一葉" :email "ichiyo@example.jp" :group "友人")
        (make-instance 'contact :name "南方 熊楠" :email "kumagusu@example.jp" :group "友人" :favorite t)
        (make-instance 'contact :name "津田 梅子" :email "umeko@example.jp" :group "仕事")
        (make-instance 'contact :name "母" :email "" :group "家族" :favorite t)))

(defun contacts-in (group)
  (remove group *contacts* :key #'contact-group :test-not #'equal))

(defun contact-label (contact)
  (format nil "~:[~;★ ~]~A" (contact-favorite contact) (contact-name contact)))

;;; a contact on the screen is a presentation of the type CONTACT
(define-presentation-type contact () :description "連絡先")
;; and the name of a group one of the type GROUP
(define-presentation-type group () :description "グループ")

(defun item-type (item)
  (if (stringp item) 'group 'contact))

(defmethod present-object ((contact contact) type)
  (contact-name contact))

;;; ------------------------------------------------------------------
;;; The window

(defvar *app* nil "The interface.")

(defun summary (interface)
  "What the tab 概要 shows: markup in which the contacts are presentations."
  (declare (ignore interface))
  (html
    (:div :class "summary"
      (:h3 "お気に入り")
      (:p (or (mapcar (lambda (contact) (list (present contact) " "))
                      (remove-if-not #'contact-favorite *contacts*))
              "まだありません"))
      (:h3 "グループ")
      (:ul (mapcar (lambda (group)
                     (:li group " — " (length (contacts-in group)) " 件"))
                   +groups+))
      (:p :class "hint"
        "名前を右クリックするとその連絡先のコマンドが出ます。"
        "メニューの「削除…」は、削除する連絡先を画面のどこからでもクリックで選べます。"
        "Ctrl+Shift+P でコマンドを名前から実行できます。"))))

(defun draw-chart (pane)
  "What the tab グラフ shows: a bar for every group, and its contacts as
circles that are presentations (click, right click) like their names."
  (let ((largest (max 1 (reduce #'max (mapcar (lambda (group) (length (contacts-in group))) +groups+)))))
    (draw-line pane 20 170 340 170 :opacity 0.4)
    (loop for group in +groups+
          for x from 50 by 100
          for contacts = (contacts-in group)
          for height = (* 130 (/ (length contacts) largest))
          do (draw-rectangle pane x (- 170 height) 60 height :radius 3 :filled t
                                                             :foreground "var(--ui-accent)" :opacity 0.25)
             (draw-string pane group (+ x 30) 188 :anchor :middle :font-size 11)
             (draw-string pane (length contacts) (+ x 30) (- 162 height) :anchor :middle :font-size 11
                                                                         :opacity 0.6)
             (loop for contact in contacts
                   for y downfrom 156 by 22
                   do (with-output-as-presentation (contact 'contact)
                        (with-drawing-group (pane :class "dot")
                          (draw-circle pane (+ x 30) y 8 :filled t
                                                         :foreground (if (contact-favorite contact)
                                                                         "var(--ui-accent)"
                                                                         "var(--ui-muted)"))
                          (draw-string pane (char (contact-name contact) 0) (+ x 30) y
                                       :anchor :middle :baseline :middle :font-size 9
                                       :foreground "var(--ui-accent-fg)")))))))

(define-interface address-book ()
  ((selected :initform nil :accessor selected-contact)
   (filter :initform "" :accessor filter)
   (favorites-only :initform nil :accessor favorites-only)
   (console-shown :initform t :accessor console-shown))
  (:panes
   (tools toolbar :items '(new-contact delete-contact :separator toggle-favorite move-to-group))
   (search text-input-pane :placeholder "検索" :text (binding (filter interface)))
   (people list-panel
           :items (computed (remove-if-not (lambda (contact)
                                             (and (search (filter interface) (contact-name contact))
                                                  (or (not (favorites-only interface))
                                                      (contact-favorite contact))))
                                           *contacts*))
           ;; (a right click on a contact shows its commands; elsewhere in the list, this menu)
           :pane-menu 'view-menu
           :print-function #'contact-label
           :selected-item (binding (selected-contact interface))
           :presentation-type 'contact)
   (groups tree-view
           :roots +groups+
           :children-function (lambda (item) (and (stringp item) (contacts-in item)))
           :expandp-function #'stringp
           :print-function (lambda (item) (if (stringp item) item (contact-label item)))
           :presentation-type #'item-type
           :callback-type :data
           :selection-callback (lambda (item)
                                 (when (typep item 'contact)
                                   (setf (selected-contact interface) item))))
   ;; the fields are the slots of the selected contact
   (name-field text-input-pane :text (binding (contact-name (selected-contact interface))))
   (email-field text-input-pane :text (binding (contact-email (selected-contact interface)))
                :placeholder "name@example.jp")
   (group-field option-pane :items +groups+ :test #'equal
                :selected-item (binding (contact-group (selected-contact interface))))
   (favorite-field check-button :text "お気に入り"
                   :selected (binding (contact-favorite (selected-contact interface))))
   (notes-field multi-line-text-input-pane :rows 6
                :text (binding (contact-notes (selected-contact interface))))
   (nobody title-pane :text "連絡先を選んでください" :class "nobody")
   (table multi-column-list-panel
          :items (computed *contacts*)
          :columns '((:title "名前" :width 160 :sort t) (:title "メール" :sort t)
                     (:title "グループ" :width 90 :sort t) (:title "★" :width 40 :adjust :center :sort t))
          :column-function (lambda (contact)
                             (list (contact-name contact) (contact-email contact)
                                   (contact-group contact) (if (contact-favorite contact) "★" "")))
          :selected-item (binding (selected-contact interface))
          :presentation-type 'contact)
   (overview application-pane :display-function #'summary)
   (chart output-pane :view-width 360 :view-height 200 :display-callback #'draw-chart
                      :max-width 720)
   (graph graph-pane
          :roots '(:all)
          :children-function (lambda (item)
                               (cond ((eq item :all) +groups+)
                                     ((stringp item) (contacts-in item))))
          :print-function (lambda (item)
                            (cond ((eq item :all) "アドレス帳")
                                  ((stringp item) item)
                                  (t (contact-name item))))
          :presentation-type (lambda (item) (if (eq item :all) 'symbol (item-type item))))
   ;; commands are typed here by their names, and print here
   (console interactor-pane :prompt "コマンド:" :height 150 :stretch nil
                            :visible (computed (console-shown interface))))
  ;; the menus of this window: entries with check marks that are slots of the
  ;; interface, a menu in a menu, and commands
  (:menus
   (view-menu "表示(&V)"
              (("お気に入りのみ(&F)" :checked (binding (favorites-only interface)) :key "Alt+F")
               ("コマンド行(&C)" :checked (binding (console-shown interface)))
               :separator
               tab-menu))
   (tab-menu "タブ(&T)"
             (("詳細" :data 0 :callback 'show-tab) ("表" :data 1 :callback 'show-tab)
              ("概要" :data 2 :callback 'show-tab) ("グラフ" :data 3 :callback 'show-tab)
              ("関係" :data 4 :callback 'show-tab)))
   (window-menu "ウィンドウ(&W)"
                (open-overview-window open-chart-window :separator tile cascade)))
  (:menu-bar :commands view-menu window-menu)
  (:layouts
   (main column-layout '(tools body console))
   (body row-layout '(sidebar :divider content) :gap 12)
   (sidebar column-layout '(search lists) :width 250 :stretch nil)
   (lists tab-layout '(("一覧" people) ("グループ" groups)))
   (content tab-layout '(("詳細" detail) ("表" table) ("概要" overview) ("グラフ" chart) ("関係" graph)))
   (detail switchable-layout '(nobody form)
           :visible-child (computed (if (selected-contact interface) 'form 'nobody)))
   (form grid-layout '("名前" name-field "メール" email-field "グループ" group-field
                       nil favorite-field "メモ" notes-field)
         :stretch nil))
  (:default-initargs :title "アドレス帳" :pointer-documentation t))

(defun show-tab (index interface)
  (let ((tabs (find-pane interface 'content)))
    (setf (choice-selected-item tabs) (nth index (collection-items tabs)))))

;;; what the interactor needs to read a contact that is typed: its name
(defmethod accept-from-string ((name (eql 'contact)) type string)
  (let ((contact (find string *contacts* :key #'contact-name :test #'string-equal)))
    (values contact (and contact t))))

(defmethod presentation-type-choices ((name (eql 'contact)) type)
  (mapcar #'contact-name *contacts*))

;;; windows: the same panes as in the tabs, in windows of their own
(define-command (open-overview-window :label "概要をウィンドウで開く") ()
  (display (make-instance 'interface :title "概要"
                                     :layout (make-instance 'application-pane :display-function #'summary))
           :window t :width 420 :height 300))

(define-command (open-chart-window :label "グラフをウィンドウで開く") ()
  (display (make-instance 'interface :title "グラフ"
                                     :layout (make-instance 'output-pane :view-width 360 :view-height 200
                                                                         :display-callback #'draw-chart))
           :window t :width 460 :height 300))

(define-command (tile :label "並べて表示" :enabled (and *windows* t)) () (tile-windows))
(define-command (cascade :label "重ねて表示" :enabled (and *windows* t)) () (cascade-windows))

(define-command (list-contacts :label "一覧") ()
  ;; written to the interactor: a table whose names are presentations
  (formatting-table (*standard-output*)
    (dolist (contact *contacts*)
      (formatting-row (*standard-output*)
        (formatting-cell (*standard-output*) (present contact 'contact :stream *standard-output*))
        (formatting-cell (*standard-output*) (write-string (contact-email contact)))
        (formatting-cell (*standard-output*) (write-string (contact-group contact)))))))

;;; ------------------------------------------------------------------
;;; Commands.  A parameter with a type is asked for when the command is
;;; invoked without it: a string in a dialog, a contact by clicking one.

(define-command (new-contact :menu "連絡先" :label "新規…" :key "Alt+N")
    ((name 'string :prompt "新しい連絡先の名前"))
  (let ((contact (make-instance 'contact :name name)))
    (setf *contacts* (append *contacts* (list contact))
          (selected-contact *app*) contact)))

(define-command (delete-contact :menu "連絡先" :label "削除…" :enabled (and *contacts* t))
    ((contact 'contact :prompt "削除する連絡先をクリックしてください"))
  (prompt-for-confirmation
   (format nil "「~A」を削除しますか？" (contact-name contact))
   :continuation (lambda (yes okp)
                   (declare (ignore okp))
                   (when yes
                     (setf *contacts* (remove contact *contacts*))
                     (when (eq contact (selected-contact *app*))
                       (setf (selected-contact *app*) nil))))))

(define-command (toggle-favorite :menu "連絡先" :label "お気に入りの切り替え" :separator t
                                 :enabled (and *contacts* t))
    ((contact 'contact :prompt "お気に入りを切り替える連絡先をクリックしてください"))
  (setf (contact-favorite contact) (not (contact-favorite contact)))
  (format t "~A: お気に入り~:[ではなくなりました~;になりました~]~%"
          (contact-name contact) (contact-favorite contact)))

(define-command (move-to-group :menu "連絡先" :label "グループを変更…" :enabled (and *contacts* t))
    ((contact 'contact :prompt "グループを変更する連絡先をクリックしてください")
     (group `(member ,@+groups+) :prompt "移動先のグループ"))
  (setf (contact-group contact) group)
  (format t "~A を「~A」へ移動しました~%" (contact-name contact) group))

(define-command (copy-email :menu "連絡先" :label "メールアドレスをコピー" :enabled (and *contacts* t))
    ((contact 'contact :prompt "メールアドレスをコピーする連絡先をクリックしてください"))
  (set-clipboard (contact-email contact)))

(define-command (import-contacts :menu "連絡先" :label "CSV を読み込む…" :separator t) ()
  ;; a file of lines  name,email,group
  (prompt-for-file
   :accept ".csv,.txt"
   :continuation
   (lambda (name text)
     (when name
       (let ((new '()))
         (with-input-from-string (stream text)
           (loop for line = (read-line stream nil)
                 while line
                 do (let* ((line (string-trim '(#\Return #\Space) line))
                           (c1 (position #\, line))
                           (c2 (and c1 (position #\, line :start (1+ c1)))))
                      (when (plusp (length line))
                        (push (make-instance 'contact
                                             :name (subseq line 0 c1)
                                             :email (if c1 (subseq line (1+ c1) c2) "")
                                             :group (let ((group (and c2 (subseq line (1+ c2)))))
                                                      (if (member group +groups+ :test #'equal) group "友人")))
                              new)))))
         (setf *contacts* (append *contacts* (reverse new)))
         (display-message "~A から ~D 件読み込みました。" name (length new)))))))

;; a contact dragged onto a group moves there
(define-drag-and-drop-translator drop-on-group (contact group move-to-group
                                                        :tester (lambda (contact group)
                                                                  (not (equal (contact-group contact) group))))
    (object destination)
  (list object destination))

(define-command (show-contact :label "詳細を表示") ((contact 'contact))
  (setf (selected-contact *app*) contact
        (choice-selected-item (find-pane *app* 'content)) (first (collection-items (find-pane *app* 'content)))))

(define-command (about :menu "ヘルプ" :label "このアプリについて") ()
  (display-message "アドレス帳~%~%Common Lisp を WebAssembly GC にコンパイルして動いています (Clysm)。~%連絡先は ~D 件です。"
                   (length *contacts*)))

;;; a click on a contact in the overview shows it
(define-presentation-to-command-translator click-contact (contact show-contact) (object)
  (list object))

;;; ------------------------------------------------------------------
;;; Startup

(setf *abort-label* "キャンセル" *yes-label* "はい" *no-label* "いいえ"
      *accept-prompt* "~Aをクリックしてください"
      *pointer-documentation-labels* '("クリック: ~A" "右クリック: メニュー" "ドラッグ: グループへ移動"))
(setf *app* (display (make-instance 'address-book)))
;; the keys of the commands; in the desktop application (ui/electron) also
;; its menu bar, and the interface then has none in the page
(install-commands *backend* :edit-label "編集" :view-label "表示" :quit-label "終了")
