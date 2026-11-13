;;;; Tests of the panes, presentations and dialogs on the S-expression
;;;; backend.  tools/test-ui.sh compares the output with pane.expected.

(defpackage "PANE-TEST" (:use "CL" "UI"))
(in-package "PANE-TEST")

(setf *backend* (make-instance 'sexp-backend))

(defun show (title root)
  (format t "~&~%== ~A~%~S~%" title (to-sexp root)))

(defun show-log (title)
  (format t "~&-- ~A~%~{   ~S~%~}" title (backend-log *backend*)))

(defun overlay () (overlay-container *backend*))

;;; ---- an interface: panes, layouts, callbacks, COMPUTED

(define-interface greeter ()
  ((count :initform 0 :accessor greeter-count))
  (:panes
   (name-input text-input-pane :placeholder "Your name" :accessor greeter-name-input
               :callback (lambda (data interface)
                           (format t "enter: ~S ~S~%" data (type-of interface))))
   (greet push-button :text "Greet" :accessor greeter-button
          :callback (lambda (data interface)
                      (format t "callback: ~S~%" data)
                      (incf (greeter-count interface))))
   (message title-pane
            :text (computed (format nil "Hello, ~A! (~D)"
                                    (text-input-pane-text name-input)
                                    (greeter-count interface)))))
  (:layouts
   (main column-layout '(top :separator message "plain text"))
   (top row-layout '(name-input greet) :gap 8))
  (:default-initargs :title "Greeter"))

(let* ((root (make-root))
       (greeter (display (make-instance 'greeter) :container root)))
  (show "interface" root)
  (show-log "displayed")
  (fire (find-node root "input") "input" '(:value "Lisp"))
  (show-log "typed")
  (fire (find-node root "input") "keydown" '(:key "a"))
  (fire (find-node root "input") "keydown" '(:key "Enter"))
  (fire (find-node root "button") "click")
  (show-log "clicked")
  ;; the accessors change the screen
  (setf (button-text (greeter-button greeter)) "Again"
        (pane-enabled (greeter-button greeter)) nil
        (interface-title greeter) "Greeter 2"
        (pane-visible (greeter-name-input greeter)) nil)
  (flush)
  (show-log "setf")
  (format t "find-pane: ~S ~S ~S~%" (pane-name (find-pane greeter 'top))
          (find-pane greeter 'count) (eq (pane-interface (find-pane greeter 'greet)) greeter))
  (quit-interface greeter)
  (show "quit" root))

;;; ---- BINDING: a slot that is another place

(defstate *filter* "")

(let* ((root (make-root))
       (input (make-instance 'text-input-pane :text (binding *filter*) :title "Filter"
                                              :title-position :left :width 200)))
  (contain input :container root)
  (show "binding" root)
  (backend-log *backend*)
  (fire (find-node root "input") "input" '(:value "abc"))
  (format t "*filter* = ~S~%" *filter*)
  (setf *filter* "xyz")
  (flush)
  (show-log "the place was set"))

;;; ---- choices

(defun choice-test (title pane)
  (let ((root (make-root)))
    (contain pane :container root)
    (show title root)
    (backend-log *backend*)
    root))

(let* ((list (make-instance 'list-panel :items '(apple banana cherry)
                                        :print-function #'string-capitalize
                                        :callback-type :data
                                        :selection-callback (lambda (item) (format t "selected ~S~%" item))
                                        :action-callback (lambda (item) (format t "action ~S~%" item))))
       (root (choice-test "list-panel" list)))
  (fire (find-node root "li" :index 1) "click")
  (show-log "banana clicked")
  (fire (find-node root "ul") "keydown" '(:key "ArrowDown"))
  (fire (find-node root "ul") "keydown" '(:key "ArrowDown"))
  (fire (find-node root "ul") "keydown" '(:key "Enter"))
  (fire (find-node root "ul") "keydown" '(:key "Home"))
  (fire (find-node root "li" :index 2) "dblclick")
  (format t "selected-item: ~S~%" (choice-selected-item list))
  (setf (collection-items list) '(banana date))
  (flush)
  (show "other items" root))

(let* ((list (make-instance 'list-panel :items '(1 2 3 4 5) :interaction :multiple-selection
                                        :selected-items '(2)))
       (root (choice-test "multiple selection" list)))
  (fire (find-node root "li" :index 3) "click" '(:shiftkey t))
  (format t "extended: ~S~%" (choice-selected-items list))
  (fire (find-node root "li" :index 2) "click" '(:ctrlkey t))
  (format t "toggled: ~S~%" (choice-selected-items list))
  (fire (find-node root "li" :index 0) "click")
  (format t "clicked: ~S~%" (choice-selected-items list)))

(let* ((option (make-instance 'option-pane :items '(:red :green :blue) :print-function #'string-downcase
                                           :callback-type :item-element
                                           :selection-callback (lambda (item pane)
                                                                 (format t "option ~S ~S~%" item (type-of pane)))))
       (root (choice-test "option-pane" option)))
  (format t "selected-item: ~S~%" (choice-selected-item option))
  (fire (find-node root "select") "change" '(:selectedindex 2))
  (show-log "blue"))

(let* ((radio (make-instance 'radio-button-panel :items '("S" "M" "L") :selected-item "M" :test #'equal))
       (checks (make-instance 'check-button-panel :items '(a b) :orientation :column))
       (root (choice-test "button panels"
                          (make-instance 'column-layout :description (list radio checks)))))
  (fire (find-node root "input" :index 2) "change")
  (fire (find-node root "input" :index 3) "change")
  (fire (find-node root "input" :index 4) "change")
  (fire (find-node root "input" :index 3) "change")
  (format t "radio ~S checks ~S~%" (choice-selected-item radio) (choice-selected-items checks)))

(let* ((table (make-instance 'multi-column-list-panel
                             :items '(("Ada" 36) ("Grace" 85))
                             :columns '((:title "Name" :width 120) (:title "Age" :adjust :right))
                             :column-function #'identity))
       (root (choice-test "multi-column-list-panel" table)))
  (fire (find-node root "tr" :index 2) "click")
  (format t "selected: ~S~%" (choice-selected-item table)))

(let* ((tree (make-instance 'tree-view :roots '(1)
                                       :children-function (lambda (n) (and (< n 4) (list (* 2 n) (1+ (* 2 n)))))
                                       :expandp-function (lambda (n) (= n 1))
                                       :print-function (lambda (n) (format nil "node ~D" n))))
       (root (choice-test "tree-view" tree)))
  (fire (find-node root "span" :class "ui-twisty" :index 0) "click")   ; node 2
  (show-log "node 2 opened")
  (format t "visible: ~S~%" (mapcar #'node-text (find-nodes root "span" :class "ui-item-text")))
  (fire (find-node root "div" :index 1) "click")
  (fire (find-node root "ul") "keydown" '(:key "ArrowDown"))
  (fire (find-node root "ul") "keydown" '(:key "ArrowLeft"))
  (format t "selected ~S, 2 open: ~S~%" (choice-selected-item tree) (tree-view-expanded-p tree 2))
  (format t "visible: ~S~%" (mapcar #'node-text (find-nodes root "span" :class "ui-item-text"))))

;;; ---- other panes

(let* ((check (make-instance 'check-button :text "Bold" :callback-type :data
                                           :callback (lambda (on) (format t "bold: ~S~%" on))))
       (slider (make-instance 'slider :start 0 :end 10 :value 3 :title "Size" :title-position :frame))
       (progress (make-instance 'progress-bar :start 0 :end 10 :value (computed (range-value slider))))
       (text (make-instance 'multi-line-text-input-pane :text "two
lines" :rows 3))
       (output (make-instance 'application-pane
                              :display-function (lambda (pane)
                                                  (declare (ignore pane))
                                                  (html (:p "size " (:b (round (range-value slider))))))))
       (root (make-root)))
  (contain (make-instance 'grid-layout :columns 2
                                       :description (list "Style" check "Size" slider nil progress
                                                          "Text" text "Output" output))
           :container root)
  (show "other panes" root)
  (backend-log *backend*)
  (fire (find-node root "input" :index 0) "change")
  (fire (find-node root "input" :index 1) "input" '(:value "7.5"))
  (show-log "slider moved")
  (format t "value: ~S, text ~S~%" (range-value slider) (text-input-pane-text text)))

;;; ---- layouts: tabs, switching, ratios and dividers

(let* ((a (make-instance 'title-pane :text "A"))
       (b (make-instance 'title-pane :text "B"))
       (tabs (make-instance 'tab-layout :items (list (list "First" a) (list "Second" b))))
       (switch (make-instance 'switchable-layout :description (list "one" (make-instance 'title-pane :text "two"))))
       (row (make-instance 'row-layout :description (list tabs :divider switch) :ratios '(1 nil 2)))
       (root (make-root)))
  (contain row :container root)
  (show "layouts" root)
  (backend-log *backend*)
  (fire (find-node root "button" :index 1) "click")
  (show-log "second tab")
  (setf (switchable-layout-visible-child switch) (second (layout-description switch)))
  (flush)
  (show-log "switched")
  (let ((divider (find-node root "div" :class "ui-divider")))
    (fire divider "pointerdown" '(:clientx 100 :width 240 :height 50))
    (fire divider "pointermove" '(:clientx 130))
    (fire divider "pointerup")
    (fire divider "pointermove" '(:clientx 500)))
  (show-log "divider dragged"))

;;; ---- commands with typed parameters, presentations, ACCEPT

(defmodel person ()
  ((name :initarg :name :accessor person-name)))

(define-presentation-type person () :description "a person")

(defmethod present-object ((person person) type)
  (person-name person))

(defstate *people* nil)
(defstate *log* nil)

(define-command (remove-person :menu "People" :label "Remove…") ((person 'person))
  (setf *people* (remove person *people*)))

(define-command (show-person :label "Show") ((person 'person))
  (push (list :show (person-name person)) *log*))

(define-command (rename-person :menu "People" :label "Rename…" :enabled (and *people* t))
    ((person 'person :prompt "Rename whom?") (name 'string :prompt "New name:"))
  (setf (person-name person) name))

(define-command (add-person :menu "People" :label "Add…" :key "CmdOrCtrl+N") ((name 'string) (age 'integer))
  (push (list :add name age) *log*)
  (setf *people* (append *people* (list (make-instance 'person :name name)))))

(define-command (paint :label "Paint") ((color '(member :red :green)))
  (push (list :paint color) *log*))

(define-presentation-to-command-translator select-person (person show-person) (object)
  (list object))

(defcomponent people ()
  (:div
    (:ul (mapcar (lambda (person)
                   (with-output-as-presentation (person 'person)
                     (:li (person-name person))))
                 *people*))
    (:p "first: " (and *people* (present (first *people*))) " and " (present 42))))

(let* ((root (make-root))
       (ada (make-instance 'person :name "Ada"))
       (bob (make-instance 'person :name "Bob")))
  (setf *people* (list ada bob))
  (mount (people) root)
  (show "presentations" root)
  (format t "parameters: ~S~%" (command-parameters (find-command 'rename-person)))
  ;; a click: the :select translator
  (fire (find-node root "li" :index 1) "click")
  (format t "log: ~S~%" *log*)
  ;; the menu of a presentation
  (fire (find-node root "li" :index 0) "contextmenu" '(:clientx 10 :clienty 20))
  (show "menu of Ada" (overlay))
  (fire (find-node (overlay) "li" :index 1) "click")        ; Remove…
  (format t "people: ~S~%" (mapcar #'person-name *people*))
  (show "after the menu" (overlay))
  ;; a command without its argument asks for it
  (backend-log *backend*)
  (invoke-command 'rename-person)
  (flush)
  (format t "accepting: ~S~%" (accepting-p))
  (show "accepting a person" root)
  (show "the prompt" (overlay))
  (fire (find-node root "span" :index 1) "click")           ; 42 is not a person
  (format t "accepting: ~S~%" (accepting-p))
  (fire (find-node root "li" :index 0) "click")             ; Bob
  (show "then a string" (overlay))
  (show-log "dialog")
  (fire (find-node (overlay) "input") "input" '(:value "Robert"))
  (fire (find-node (overlay) "input") "keydown" '(:key "Enter"))
  (format t "people: ~S, accepting: ~S~%" (mapcar #'person-name *people*) (accepting-p))
  (show "renamed" root)
  (show "no dialog" (overlay))
  ;; Escape cancels
  (invoke-command 'remove-person)
  (flush)
  (fire-global *backend* "keydown" '(:key "Escape"))
  (format t "accepting: ~S, people: ~S~%" (accepting-p) (mapcar #'person-name *people*))
  ;; two dialogs in a row; a number must be one
  (invoke-command 'add-person)
  (flush)
  (fire (find-node (overlay) "input") "input" '(:value "Cy"))
  (fire (find-node (overlay) "button" :index 1) "click")
  (fire (find-node (overlay) "input") "input" '(:value "old"))
  (format t "OK disabled: ~S~%" (getf (second (to-sexp (find-node (overlay) "button" :index 1))) :disabled))
  (fire (find-node (overlay) "input") "keydown" '(:key "Enter"))
  (fire (find-node (overlay) "input") "input" '(:value "41"))
  (fire (find-node (overlay) "button" :index 1) "click")
  (format t "log: ~S~%" *log*)
  ;; MEMBER: a list
  (invoke-command 'paint)
  (flush)
  (show "member" (overlay))
  (fire (find-node (overlay) "li" :index 1) "click")
  (fire (find-node (overlay) "button" :index 1) "click")
  (format t "log: ~S~%" (first *log*))
  ;; cancelled dialogs call nothing
  (invoke-command 'paint)
  (flush)
  (fire (find-node (overlay) "dialog") "cancel")
  (format t "log: ~S, dialogs left: ~D~%" (first *log*) (length (find-nodes (overlay) "dialog"))))

;;; ---- dialogs

(let ((answers '()))
  (flet ((note (&rest values) (push values answers)))
    (display-message "~D files copied" 3)
    (flush)
    (show "message" (overlay))
    (fire (find-node (overlay) "button") "click")
    (prompt-for-confirmation "Delete?" :continuation #'note)
    (flush)
    (fire (find-node (overlay) "button" :index 1) "click")
    (prompt-for-confirmation "Delete?" :continuation #'note)
    (flush)
    (fire (find-node (overlay) "button" :index 0) "click")
    (prompt-for-string "Name:" :text "x" :continuation #'note)
    (flush)
    (abort-dialog)
    (flush)
    (prompt-with-list '(a b c) "Which?" :continuation #'note)
    (flush)
    (fire (find-node (overlay) "li" :index 2) "dblclick")
    (format t "answers: ~S~%" (reverse answers))))

;;; an interface as a dialog

(define-interface login ()
  ()
  (:panes
   (user text-input-pane :title "User")
   (password text-input-pane :title "Password" :password t)
   (ok push-button :text "Log in" :default-p t
       :enabled (computed (plusp (length (text-input-pane-text user))))
       :callback (lambda (data interface)
                   (declare (ignore data interface))
                   (exit-dialog (list (text-input-pane-text user) (text-input-pane-text password))))))
  (:default-initargs :title "Log in"))

(let ((login (make-instance 'login)))
  (display-dialog login :continuation (lambda (value okp) (format t "login: ~S ~S~%" value okp)))
  (flush)
  (show "an interface as a dialog" (overlay))
  (fire (find-node (overlay) "input" :index 0) "input" '(:value "ada"))
  (fire (find-node (overlay) "input" :index 1) "input" '(:value "secret"))
  (fire (find-node (overlay) "button") "click")
  (display-dialog login :continuation (lambda (value okp) (format t "login: ~S ~S~%" value okp)))
  (flush)
  (quit-interface login))

;;; ---- the menu bar and a toolbar

(let ((root (make-root)))
  (setf *people* nil)
  (contain (make-instance 'toolbar :items '(add-person :separator rename-person (paint :red)))
           :menu-bar t :container root)
  (show "menu bar and toolbar" root)
  (fire (find-node root "button" :class "ui-menu-title") "click")
  (show "the menu is open" (find-node root "div" :class "ui-menu-bar"))
  (fire (find-node root "li" :index 2) "click")
  (format t "open menus: ~D, dialogs: ~D~%" (length (find-nodes root "ul"))
          (length (find-nodes (overlay) "dialog")))
  (abort-dialog)
  (fire (find-node root "button" :index 3) "click")
  (format t "log: ~S~%" (first *log*)))

;;; ---- sorting the columns of a table

(let* ((table (make-instance 'multi-column-list-panel
                             :items '(("Grace" 85) ("ada" 36) ("Barbara" 76))
                             :columns '((:title "Name" :sort t) (:title "Age" :sort t :adjust :right)
                                        (:title "Initial"))
                             :column-function (lambda (item) (append item (list (char (first item) 0))))))
       (root (choice-test "sortable columns" table)))
  (flet ((names () (mapcar #'node-text (find-nodes root "td" :class "ui-adjust-left"))))
    (fire (find-node root "th" :index 0) "click")
    (format t "by name: ~S~%" (names))
    (show-log "sorted")
    (fire (find-node root "th" :index 1) "click")
    (format t "by age: ~S~%" (names))
    (fire (find-node root "th" :index 1) "click")
    (format t "descending: ~S ~S~%" (names) (getf (second (to-sexp (find-node root "th" :index 1))) :aria-sort))
    ;; the keys follow what is shown
    (fire (find-node root "div" :class "ui-pane ui-multi-column-list-panel") "keydown" '(:key "Home"))
    (format t "first: ~S~%" (choice-selected-item table))
    (show-log "Home")
    (fire (find-node root "th" :index 1) "click")
    (format t "unsorted: ~S ~S~%" (names) (list-panel-sort-column table))))

;;; ---- drawing

(defstate *radius* 10)

(let* ((clicks '())
       (pane (make-instance 'output-pane
                            :view-width 100 :view-height 50
                            :press-callback (lambda (pane x y)
                                              (declare (ignore pane))
                                              (push (list x y) clicks))
                            :display-callback
                            (lambda (pane)
                              (draw-rectangle pane 0 0 100 50 :radius 4)
                              (with-graphics-state (pane :foreground "red" :thickness 2)
                                (draw-line pane 0 0 100 50)
                                (draw-lines pane '(0 50 50 25 100 50) :dashes '(4 2))
                                (draw-polygon pane '((10 10) (20 10) (15 20)) :filled t :foreground "blue"))
                              (with-output-as-presentation (*people* 'cons)
                                (draw-circle pane 50 25 *radius* :filled t :opacity 1/2))
                              (draw-ellipse pane 50 25 20 10 :fill "yellow")
                              (draw-arc pane 50 25 10 0 (/ pi 2))
                              (draw-path pane "M0 0 L5 5 Z" :class "mark")
                              (with-graphics-translation (pane 10 20)
                                (with-graphics-scale (pane 2)
                                  (draw-string pane "label" 0 0 :anchor :middle :baseline :middle
                                                                :font-size 6)))
                              (with-drawing-group (pane :class "group")
                                (draw-circle pane 1 1 1)
                                (draw-circle pane 2 2 1)))))
       (root (make-root)))
  (contain pane :container root)
  (show "drawing" root)
  (backend-log *backend*)
  (setf *radius* 12.5)
  (flush)
  (show-log "only the circle changes")
  (fire (find-node root "svg") "pointerdown" '(:x 30 :y 40))
  (format t "pressed at ~S~%" clicks)
  (format t "outside a pane: ~A~%" (handler-case (draw-line nil 0 0 1 1) (error (e) e))))

;;; ---- command tables

(define-command (zoom-in :menu "View" :key "CmdOrCtrl+=" :table 'viewer) ()
  (push :zoom *log*))

(define-interface viewer ()
  ()
  (:panes (text title-pane :text "a viewer"))
  (:default-initargs :command-table 'viewer))

(flet ((state ()
         (list (mapcar #'car (command-menus))
               (and (command-for-key (parse-key "Ctrl+=")) t)
               *command-tables*)))
  (let ((root (make-root))
        (viewer (make-instance 'viewer)))
    (format t "~&~%== command tables~%before: ~S~%" (state))
    (display viewer :container root)
    (format t "displayed: ~S~%" (state))
    (quit-interface viewer)
    (format t "quit: ~S~%" (state))))

;;; ---- the command palette

(progn
  (setf *people* (list (make-instance 'person :name "Eve")))
  (invoke-command 'command-palette)
  (flush)
  (flet ((labels-shown () (mapcar #'node-text (find-nodes (overlay) "li"))))
    (format t "~&~%== command palette~%commands: ~S~%" (labels-shown))
    (fire (find-node (overlay) "input") "input" '(:value "re"))
    (format t "matching \"re\": ~S~%" (labels-shown))
    (fire (find-node (overlay) "input") "keydown" '(:key "ArrowDown"))
    (format t "selected: ~S~%" (mapcar #'node-text (find-nodes (overlay) "li" :class "ui-item selected")))
    (fire (find-node (overlay) "input") "keydown" '(:key "Enter"))
    (format t "dialogs: ~D, accepting: ~S~%" (length (find-nodes (overlay) "dialog")) (accepting-p))
    (cancel-accept)
    (abort-dialog)
    (flush)))

;;; ---- an error asks for a restart in a dialog, and the computation goes on

(define-command (save-it :label "Save") ()
  (push (restart-case (error "The disk is full")
          (retry () :report "Try again" :retried)
          (elsewhere () :report "Save elsewhere…" :elsewhere))
        *log*))

(flet ((answer-with (index)
         (setf (backend-waiter *backend*)
               (lambda ()
                 (show "the restarts" (overlay))
                 (fire (find-node (overlay) "button" :index index) "click")))))
  (format t "~&~%== restarts in a dialog~%")
  (backend-log *backend*)
  (answer-with 1)
  (invoke-command 'save-it)
  (flush)
  (format t "chosen: ~S, dialogs left: ~D~%" (first *log*) (length (find-nodes (overlay) "dialog")))
  ;; the last button is ABORT: the command is given up
  (setf (backend-waiter *backend*)
        (lambda () (fire (find-node (overlay) "button" :index 2) "click")))
  (format t "aborted: ~S, log: ~S~%" (invoke-command 'save-it) (first *log*))
  ;; Escape too
  (setf (backend-waiter *backend*) (lambda () (fire (find-node (overlay) "dialog") "cancel")))
  (format t "cancelled: ~S, log: ~S~%" (invoke-command 'save-it) (first *log*))
  (setf (backend-waiter *backend*) nil))

;;; ---- more panes and layouts

(let* ((radios (list (make-instance 'radio-button :text "one" :selected t :group :n)
                     (make-instance 'radio-button :text "two" :group :n)
                     (make-instance 'radio-button :text "other" :group :m)))
       (buttons (make-instance 'push-button-panel :items '(:yes :no) :print-function #'string-capitalize
                                                  :callback-type :data
                                                  :selection-callback (lambda (item) (format t "pressed ~S~%" item))))
       (combo (make-instance 'text-input-choice :items '("red" "green") :text "blue"))
       (root (make-root)))
  (contain (make-instance
            'column-layout
            :description (append radios
                                 (list buttons combo
                                       (make-instance 'password-pane :text "secret")
                                       (make-instance 'editor-pane :text "(defun f ())" :rows 2)
                                       (make-instance 'color-pane :color "#ff0000")
                                       (make-instance 'image-pane :url "logo.png" :description "logo")
                                       (make-instance 'browser-pane :url "https://example.org/" :height 100)
                                       (make-instance 'simple-layout :title "Box" :title-position :frame
                                                                     :description '("inside"))
                                       (make-instance 'pinboard-layout
                                                      :description (list (list (make-instance 'title-pane :text "pinned")
                                                                               10 20 80))))))
           :container root)
  (show "more panes" root)
  (fire (find-node root "input" :index 1) "change")
  (format t "radios: ~S~%" (mapcar #'button-selected radios))
  (fire (find-node root "button" :index 1) "click")
  (fire (find-node root "input" :index 3) "input" '(:value "green"))
  (format t "combo: ~S~%" (text-input-pane-text combo)))

;;; a list with a filter

(let* ((list (make-instance 'list-panel :items '("apple" "banana" "apricot") :filter "Filter"))
       (root (choice-test "a list with a filter" list)))
  (fire (find-node root "input") "input" '(:value "AP"))
  (format t "filtered: ~S~%" (mapcar #'node-text (find-nodes root "li")))
  (fire (find-node root "input") "keydown" '(:key "ArrowDown"))
  (fire (find-node root "input") "keydown" '(:key "ArrowDown"))
  (format t "selected: ~S~%" (choice-selected-item list)))

;;; ---- a graph

(let* ((graph (make-instance 'graph-pane :roots '(a)
                                         :children-function (lambda (node)
                                                              (case node (a '(b c)) (b '(d)) (c '(d))))
                                         :print-function #'string-downcase
                                         :callback-type :data
                                         :action-callback (lambda (item) (format t "action ~S~%" item))))
       (root (choice-test "graph" graph)))
  (fire (find-node root "g" :index 3) "click")
  (format t "selected: ~S, nodes: ~S, edges: ~D~%" (choice-selected-item graph)
          (mapcar #'node-text (find-nodes root "g")) (length (find-nodes root "path")))
  (fire (find-node root "g" :index 0) "dblclick")
  (setf (graph-pane-direction graph) :top-down)
  (flush)
  (format t "top-down: ~S~%" (second (to-sexp (find-node root "rect" :index 1)))))

;;; ---- drag and drop, pointer documentation

(defmodel folder ()
  ((name :initarg :name :accessor folder-name)
   (people :initform nil :accessor folder-people)))

(define-command (file-person :label "File") ((person 'person) (folder 'folder))
  (push (person-name person) (folder-people folder)))

(define-drag-and-drop-translator drop-person (person folder file-person
                                                     :tester (lambda (person folder)
                                                               (not (member (person-name person)
                                                                            (folder-people folder)
                                                                            :test #'equal))))
    (object destination)
  (list object destination))

(let* ((root (make-root))
       (folder (make-instance 'folder :name "Inbox"))
       (eve (make-instance 'person :name "Eve")))
  (contain (make-instance 'application-pane
                          :display-function (lambda (pane)
                                              (declare (ignore pane))
                                              (html (:div (with-output-as-presentation (eve 'person)
                                                            (:b (person-name eve)))
                                                          (with-output-as-presentation (folder 'folder)
                                                            (:i (folder-name folder) " "
                                                                (length (folder-people folder))))))))
           :pointer-documentation t :container root)
  (show "drag and drop" root)
  (fire (find-node root "b") "mouseenter")
  (format t "documentation: ~S~%" (node-text (find-node root "div" :class "ui-pointer-documentation")))
  (fire (find-node root "b") "mouseleave")
  (fire (find-node root "i") "drop")                 ; nothing is dragged
  (fire (find-node root "b") "dragstart")
  (fire (find-node root "i") "dragover")
  (fire (find-node root "i") "drop")
  (format t "filed: ~S ~S~%" (folder-people folder) (node-text (find-node root "i")))
  (fire (find-node root "b") "dragstart")
  (fire (find-node root "i") "drop")                 ; the tester says no
  (format t "filed: ~S~%" (folder-people folder)))

;;; ---- more dialogs

(let ((answers '()))
  (flet ((note (&rest values) (push values answers)))
    (prompt-for-values '(("Name" string :default "Ada") ("Age" integer) ("Admin" boolean)
                         ("Colour" (member :red :green) :default :green))
                       :title "New user" :continuation #'note)
    (flush)
    (show "several values" (overlay))
    (format t "OK disabled: ~S~%" (getf (second (to-sexp (find-node (overlay) "button" :index 1))) :disabled))
    (fire (find-node (overlay) "input" :index 1) "input" '(:value "36"))
    (fire (find-node (overlay) "input" :index 2) "change")
    (fire (find-node (overlay) "button" :index 1) "click")
    (prompt-for-color "Colour:" :color "#00ff00" :continuation #'note)
    (flush)
    (fire (find-node (overlay) "input") "input" '(:value "#0000ff"))
    (fire (find-node (overlay) "button" :index 1) "click")
    (let ((slider (make-instance 'slider :value 5)))
      (popup-confirmer slider "How many?" :value-function #'range-value :ok-check #'plusp
                                          :continuation #'note)
      (flush)
      (fire (find-node (overlay) "input") "input" '(:value 0))
      (format t "OK disabled: ~S~%" (getf (second (to-sexp (find-node (overlay) "button" :index 1))) :disabled))
      (fire (find-node (overlay) "input") "input" '(:value 8))
      (fire (find-node (overlay) "button" :index 1) "click"))
    (setf (backend-file *backend*) '("notes.txt" "hello"))
    (prompt-for-file :accept ".txt" :continuation #'note)
    (set-clipboard "copied")
    (clipboard #'note)
    (format t "answers: ~S~%" (reverse answers))))

;;; a window: a dialog that is not modal

(let ((login (make-instance 'login)))
  (display-dialog login :modal nil :continuation (lambda (value okp) (format t "window: ~S ~S~%" value okp)))
  (flush)
  (backend-log *backend*)
  (display-message "above the window")
  (flush)
  (format t "~&~%== a window~%~S~%" (remove :call (backend-log *backend*) :key #'car :test-not #'eq))
  (format t "dialogs: ~S~%" (mapcar (lambda (node) (getf (second (to-sexp node)) :class))
                                    (find-nodes (overlay) "dialog")))
  ;; EXIT-DIALOG answers the modal one
  (exit-dialog t)
  (fire (find-node (overlay) "button" :class "ui-window-close") "click")
  (format t "dialogs left: ~D~%" (length (find-nodes (overlay) "dialog"))))
