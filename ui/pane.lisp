;;;; pane.lisp — panes, layouts and interfaces (after CAPI)
;;;;
;;;; A pane is an object that is on the screen: a button, a text field, a
;;;; list.  A layout is a pane that arranges other panes, and an interface a
;;;; window made of them:
;;;;
;;;;   (define-interface greeter ()
;;;;     ((count :initform 0 :accessor greeter-count))
;;;;     (:panes
;;;;      (name-input text-input-pane :placeholder "Your name")
;;;;      (greet push-button :text "Greet"
;;;;             :callback (lambda (data interface)
;;;;                         (incf (greeter-count interface))))
;;;;      (message title-pane
;;;;               :text (computed (format nil "Hello, ~A! (~D)"
;;;;                                       (text-input-pane-text name-input)
;;;;                                       (greeter-count interface)))))
;;;;     (:layouts
;;;;      (main column-layout '(top message))
;;;;      (top row-layout '(name-input greet)))
;;;;     (:default-initargs :title "Greeter"))
;;;;
;;;;   (display (make-instance 'greeter))
;;;;
;;;; Panes are models (DEFMODEL): what they show is in their slots, and the
;;;; screen follows SETF of an accessor, (setf (button-text greet) "Again").
;;;; A slot can be given a value that follows other state, (COMPUTED form), or
;;;; that is another place, (BINDING place).
;;;;
;;;; Every pane is rendered by a method of RENDER-PANE that returns markup, so
;;;; a new kind of pane is a class and that method.
;;;;
;;;; Compile it after present.lisp.

(in-package "UI")

;;; ------------------------------------------------------------------
;;; Panes

(defmodel pane ()
  ((name :initarg :name :initform nil :accessor pane-name)
   (interface :initarg :interface :initform nil :accessor pane-interface)
   (title :initarg :title :initform nil :accessor pane-title)
   ;; :TOP, :LEFT or :FRAME (a box around the pane)
   (title-position :initarg :title-position :initform :top :accessor pane-title-position)
   (enabled :initarg :enabled :initform t :accessor pane-enabled)
   (visible :initarg :visible :initform t :accessor pane-visible)
   (help :initarg :help :initform nil :accessor pane-help)
   (class :initarg :class :initform nil :accessor pane-class)
   ;; sizes: a number of pixels or a CSS length
   (width :initarg :width :initform nil :accessor pane-width)
   (height :initarg :height :initform nil :accessor pane-height)
   (min-width :initarg :min-width :initform nil :accessor pane-min-width)
   (min-height :initarg :min-height :initform nil :accessor pane-min-height)
   (max-width :initarg :max-width :initform nil :accessor pane-max-width)
   (max-height :initarg :max-height :initform nil :accessor pane-max-height)
   ;; whether it takes the free space of its row or column: T, or a weight
   (stretch :initarg :stretch :initform nil :accessor pane-stretch))
  (:documentation "Something on the screen.  TITLE is a label shown with it, HELP a
tooltip, CLASS additional CSS classes."))

(defgeneric render-pane (pane)
  (:documentation "The element (markup) that shows PANE now."))

(defgeneric pane-shows-title-p (pane)
  (:method (pane) t))

(defun css-length (x)
  (if (numberp x) (format nil "~Dpx" x) x))

(defun pane-style (pane flex)
  "The CSS of the sizes of PANE; FLEX is what its layout says about it."
  (let ((stretch (pane-stretch pane)))
    (format nil "~@[~A;~]~@[width:~A;~]~@[height:~A;~]~@[min-width:~A;~]~@[min-height:~A;~]~@[max-width:~A;~]~@[max-height:~A;~]"
            (or flex
                (cond ((numberp stretch) (format nil "flex:~D 1 0" stretch))
                      (stretch "flex:1 1 0")))
            (css-length (pane-width pane)) (css-length (pane-height pane))
            (css-length (pane-min-width pane)) (css-length (pane-min-height pane))
            (css-length (pane-max-width pane)) (css-length (pane-max-height pane)))))

(defun pane-element (pane flex)
  (let ((node (render-pane pane))
        (title (and (pane-shows-title-p pane) (pane-title pane)))
        (style (pane-style pane flex)))
    (when title
      (setf (getf (element-props node) :aria-label) title)
      (setf node (if (eq (pane-title-position pane) :frame)
                     (html (:fieldset :class "ui-frame" (:legend title) node))
                     (html (:div :class (if (eq (pane-title-position pane) :left)
                                            "ui-titled ui-title-left"
                                            "ui-titled")
                             (:label :class "ui-title" title)
                             node)))))
    (setf (getf (element-props node) :class)
          (format nil "ui-pane ~A~@[ ~A~]" (getf (element-props node) :class "") (pane-class pane)))
    (when (plusp (length style))
      (setf (getf (element-props node) :style)
            (concatenate 'string style (or (getf (element-props node) :style) ""))))
    (unless (pane-visible pane)
      (setf (getf (element-props node) :hidden) t))
    (when (pane-help pane)
      (setf (getf (element-props node) :title) (pane-help pane)))
    node))

(defcomponent pane-view (pane flex)
  (pane-element pane flex))

(defun pane-node (pane &optional flex)
  "The node that shows PANE, for markup: (:div (pane-node my-pane))."
  (keyed pane (pane-view pane flex)))

;;; callbacks

(defun callback-type-list (type)
  "The arguments of a callback: a list of :DATA, :INTERFACE, :ITEM and
:ELEMENT, or a keyword made of them like :INTERFACE-DATA, or :NONE."
  (cond ((listp type) type)
        ((eq type :none) nil)
        (t (let ((name (symbol-name type)) (result '()) (start 0))
             (loop
               (let ((end (position #\- name :start start)))
                 (push (intern (subseq name start end) "KEYWORD") result)
                 (if end (setf start (1+ end)) (return))))
             (nreverse result)))))

(defun call-callback (pane function &key data (item data))
  (when function
    (apply function
           (mapcar (lambda (key)
                     (ecase key
                       (:data data)
                       (:item item)
                       (:interface (pane-interface pane))
                       (:element pane)))
                   (callback-type-list (pane-callback-type pane))))))

(defmodel callbacks ()
  ((callback-type :initarg :callback-type :initform '(:data :interface) :accessor pane-callback-type))
  (:documentation "A pane with callbacks, which are called with what CALLBACK-TYPE
says: by default the data and the interface."))

;;; ------------------------------------------------------------------
;;; Text

(defmodel title-pane (pane)
  ((text :initarg :text :initform "" :accessor title-pane-text))
  (:documentation "A line of text."))

(defun text-of (x)
  "X as the text of a pane: a string as it is, a number in decimal notation."
  (cond ((stringp x) x)
        ((null x) "")
        ((floatp x) (let ((text (format nil "~F" x)))
                      ;; 7.0 is 7
                      (if (and (> (length text) 2) (string= ".0" text :start2 (- (length text) 2)))
                          (subseq text 0 (- (length text) 2))
                          text)))
        (t (princ-to-string x))))

(defmethod render-pane ((pane title-pane))
  (html (:span :class "ui-title-pane" (text-of (title-pane-text pane)))))

(defmodel display-pane (pane)
  ((text :initarg :text :initform "" :accessor display-pane-text))
  (:documentation "Text of several lines that the user reads."))

(defmethod render-pane ((pane display-pane))
  (html (:div :class "ui-display-pane" (text-of (display-pane-text pane)))))

(defmodel text-input-pane (pane callbacks)
  ((text :initarg :text :initform "" :accessor text-input-pane-text)
   (placeholder :initarg :placeholder :initform nil :accessor text-input-pane-placeholder)
   (password :initarg :password :initform nil :accessor text-input-pane-password)
   (read-only :initarg :read-only :initform nil :accessor text-input-pane-read-only)
   ;; called with the text when the user presses Enter
   (callback :initarg :callback :initform nil :accessor text-input-pane-callback)
   ;; called with the text whenever the user changes it
   (change-callback :initarg :change-callback :initform nil :accessor text-input-pane-change-callback)
   ;; a choice (a list) whose selection the arrow keys move while the user types here
   (navigates :initarg :navigates :initform nil :accessor text-input-pane-navigates))
  (:documentation "A line of text that the user edits."))

(defun text-changed (pane event)
  (let ((text (event-target-get *backend* event "value")))
    (setf (text-input-pane-text pane) text)
    (call-callback pane (text-input-pane-change-callback pane) :data text)))

(defmethod render-pane ((pane text-input-pane))
  (html (:input :type (if (text-input-pane-password pane) "password" "text")
                :class "ui-text-input-pane"
                :value (text-of (text-input-pane-text pane))
                :placeholder (text-input-pane-placeholder pane)
                :disabled (not (pane-enabled pane))
                :readonly (and (text-input-pane-read-only pane) t)
                :on-input (lambda (&optional event) (text-changed pane event))
                :on-keydown (lambda (&optional event)
                              (let ((key (event-get *backend* event "key"))
                                    (list (text-input-pane-navigates pane)))
                                (cond ((event-get *backend* event "isComposing"))   ; IME input
                                      ((equal key "Enter")
                                       (when (text-input-pane-callback pane)
                                         ;; the key is used up: it must not also press
                                         ;; a button that the callback gives the focus
                                         (when event (stop-event *backend* event))
                                         (call-callback pane (text-input-pane-callback pane)
                                                        :data (text-input-pane-text pane))))
                                      ((and list (member key '("ArrowDown" "ArrowUp") :test #'equal))
                                       (move-selection list key event))))))))

(defmodel multi-line-text-input-pane (text-input-pane)
  ((rows :initarg :rows :initform 4 :accessor text-input-pane-rows))
  (:documentation "Text of several lines that the user edits."))

(defmethod render-pane ((pane multi-line-text-input-pane))
  (html (:textarea :class "ui-multi-line-text-input-pane"
                   :value (text-of (text-input-pane-text pane))
                   :rows (text-input-pane-rows pane)
                   :placeholder (text-input-pane-placeholder pane)
                   :disabled (not (pane-enabled pane))
                   :readonly (and (text-input-pane-read-only pane) t)
                   :on-input (lambda (&optional event) (text-changed pane event)))))

;;; ------------------------------------------------------------------
;;; Buttons

(defmodel push-button (pane callbacks)
  ((text :initarg :text :initform nil :accessor button-text)
   (data :initarg :data :initform nil :accessor button-data)
   (callback :initarg :callback :initform nil :accessor button-callback)
   ;; the name of a command, or (name argument...): the button invokes it,
   ;; has its label and is enabled while the command is
   (command :initarg :command :initform nil :accessor button-command)
   (default-p :initarg :default-p :initform nil :accessor button-default-p))
  (:documentation "A button.  Its callback is called with its DATA (by default its text)."))

(defmethod render-pane ((pane push-button))
  (let* ((command (button-command pane))
         (name (if (consp command) (car command) command))
         (arguments (and (consp command) (cdr command)))
         (text (or (button-text pane)
                   (and name (command-label (find-command name)))
                   "")))
    (html (:button :type "button"
                   :class (if (button-default-p pane) "ui-push-button ui-default" "ui-push-button")
                   :disabled (not (and (pane-enabled pane)
                                       (or (null name) (command-enabled-p name))))
                   :on-click (lambda (&optional event)
                               (declare (ignore event))
                               (if name
                                   (apply #'invoke-command name arguments)
                                   (call-callback pane (button-callback pane)
                                                  :data (or (button-data pane) text))))
            text))))

(defmodel check-button (pane callbacks)
  ((text :initarg :text :initform "" :accessor button-text)
   (selected :initarg :selected :initform nil :accessor button-selected)
   (callback :initarg :callback :initform nil :accessor button-callback))
  (:documentation "A check box.  Its callback is called with whether it is selected."))

(defmethod render-pane ((pane check-button))
  (html (:label :class "ui-check-button"
          (:input :type "checkbox" :checked (and (button-selected pane) t)
                  :disabled (not (pane-enabled pane))
                  :on-change (lambda (&optional event)
                               (declare (ignore event))
                               (let ((selected (not (button-selected pane))))
                                 (setf (button-selected pane) selected)
                                 (call-callback pane (button-callback pane) :data selected))))
          (:span (text-of (button-text pane))))))

;;; ------------------------------------------------------------------
;;; Choices: panes that show items, some of which are selected

(defmodel choice (pane callbacks)
  ((items :initarg :items :initform nil :accessor collection-items)
   ;; a function of an item that returns its text (or a node)
   (print-function :initarg :print-function :initform nil :accessor collection-print-function)
   (test :initarg :test :initform #'eql :accessor collection-test)
   ;; :SINGLE-SELECTION or :MULTIPLE-SELECTION
   (interaction :initarg :interaction :initform :single-selection :accessor choice-interaction)
   ;; the selected item (or NIL), with :MULTIPLE-SELECTION the list of them
   (selection :initarg :selected-item :initarg :selected-items :initform nil
              :accessor choice-selection)
   ;; called with the item that was selected
   (selection-callback :initarg :selection-callback :initform nil :accessor choice-selection-callback)
   ;; called with the item that was double-clicked (or Enter pressed on)
   (action-callback :initarg :action-callback :initform nil :accessor choice-action-callback)
   ;; if true, the items are presentations of this type (T: PRESENTATION-TYPE-OF)
   (presentation-type :initarg :presentation-type :initform nil :accessor choice-presentation-type)))

(defun multiple-selection-p (choice)
  (eq (choice-interaction choice) :multiple-selection))

(defgeneric choice-selected-items (choice)
  (:method ((choice choice))
    (let ((selection (choice-selection choice)))
      (cond ((multiple-selection-p choice) selection)
            (selection (list selection))))))

(defgeneric choice-selected-item (choice)
  (:method ((choice choice))
    (if (multiple-selection-p choice)
        (first (choice-selection choice))
        (choice-selection choice))))

(defun (setf choice-selected-items) (items choice)
  (setf (choice-selection choice) (if (multiple-selection-p choice) items (first items)))
  items)

(defun (setf choice-selected-item) (item choice)
  (setf (choice-selection choice) (if (multiple-selection-p choice) (and item (list item)) item))
  item)

(defun item-selected-p (choice item)
  (and (member item (choice-selected-items choice) :test (collection-test choice)) t))

(defun print-item (choice item)
  (let ((function (collection-print-function choice)))
    (cond (function (funcall function item))
          (t (text-of item)))))

(defun select-item (choice item &key toggle extend)
  "The user selects ITEM: with TOGGLE (and multiple selection) in addition
to the selection or no longer, with EXTEND all the items up to it."
  (let ((test (collection-test choice)))
    (cond ((not (multiple-selection-p choice))
           (setf (choice-selection choice) item))
          (toggle
           (setf (choice-selection choice)
                 (if (item-selected-p choice item)
                     (remove item (choice-selection choice) :test test)
                     (append (choice-selection choice) (list item)))))
          ((and extend (choice-selection choice))
           (let* ((items (choice-visible-items choice))
                  (from (position (first (choice-selection choice)) items :test test))
                  (to (position item items :test test)))
             (setf (choice-selection choice)
                   (if (and from to)
                       (let ((range (subseq items (min from to) (1+ (max from to)))))
                         (if (< to from) (reverse range) range))
                       (list item)))))
          (t (setf (choice-selection choice) (list item)))))
  (call-callback choice (choice-selection-callback choice) :data item))

(defgeneric choice-visible-items (choice)
  (:documentation "The items the user sees, from top to bottom.")
  (:method ((choice choice)) (collection-items choice)))

(defun click-item (choice item event)
  (select-item choice item
               :toggle (or (event-get *backend* event "ctrlKey") (event-get *backend* event "metaKey"))
               :extend (event-get *backend* event "shiftKey")))

(defun activate-item (choice item)
  (call-callback choice (choice-action-callback choice) :data item))

(defun move-selection (choice key event)
  "The keys of a list: the arrows, Home and End select, Enter activates.
True if KEY was one of them."
  (let* ((items (choice-visible-items choice))
         (current (first (last (choice-selected-items choice))))
         (index (and current (position current items :test (collection-test choice))))
         (target (cond ((null items) nil)
                       ((equal key "ArrowDown") (if index (min (1+ index) (1- (length items))) 0))
                       ((equal key "ArrowUp") (if index (max (1- index) 0) (1- (length items))))
                       ((equal key "Home") 0)
                       ((equal key "End") (1- (length items))))))
    (cond (target
           (when event (stop-event *backend* event))
           (unless (eql target index)
             (select-item choice (nth target items))
             (reveal-selection *backend* event))
           t)
          ((and (equal key "Enter") current)
           (activate-item choice current)
           t))))

(defun item-element (choice item node)
  "NODE, the element of ITEM, as a presentation if the items of CHOICE are."
  (let ((type (choice-presentation-type choice)))
    (if type
        (presenting item (if (eq type t) nil type) node)
        node)))

;;; a list

(defmodel list-panel (choice) ()
  (:default-initargs :stretch t)
  (:documentation "A list of items."))

(defmethod render-pane ((pane list-panel))
  (html
    (:ul :class "ui-list-panel" :role "listbox" :tabindex (if (pane-enabled pane) 0 -1)
         :aria-multiselectable (if (multiple-selection-p pane) "true" nil)
         :on-keydown (lambda (&optional event)
                       (move-selection pane (event-get *backend* event "key") event))
      (mapcar (lambda (item)
                (let ((selected (item-selected-p pane item)))
                  (item-element
                   pane item
                   (:li :class (if selected "ui-item selected" "ui-item") :role "option"
                        :aria-selected (if selected "true" "false")
                        :on-click (lambda (&optional event) (click-item pane item event))
                        :on-dblclick (lambda (&optional event)
                                       (declare (ignore event))
                                       (activate-item pane item))
                     (print-item pane item)))))
              (collection-items pane)))))

;;; a table

(defmodel multi-column-list-panel (list-panel)
  (;; a list of (:title string :width size :adjust :left/:right/:center)
   (columns :initarg :columns :initform nil :accessor list-panel-columns)
   ;; a function of an item that returns the list of what its columns show
   (column-function :initarg :column-function :initform nil :accessor list-panel-column-function)
   ;; the index of the column the items are sorted by (NIL: as they are)
   (sort-column :initarg :sort-column :initform nil :accessor list-panel-sort-column)
   (sort-descending :initarg :sort-descending :initform nil :accessor list-panel-sort-descending))
  (:documentation "A list of items with several columns.  A click on the title of a
column with :SORT sorts the items by it (again: the other way round, then
not at all): :SORT T compares what the column shows, a function what it
returns for an item — numbers by size, everything else as text."))

(defun item-cells (pane item)
  (let ((function (list-panel-column-function pane)))
    (if function (funcall function item) (list (print-item pane item)))))

(defun sort-value-less-p (a b)
  (if (and (realp a) (realp b))
      (< a b)
      (and (string-lessp (text-of a) (text-of b)) t)))

(defmethod choice-visible-items ((pane multi-column-list-panel))
  (let* ((items (collection-items pane))
         (index (list-panel-sort-column pane))
         (sort (and index (getf (nth index (list-panel-columns pane)) :sort))))
    (if (null sort)
        items
        (let ((sorted (stable-sort (copy-list items) #'sort-value-less-p
                                   :key (if (functionp sort)
                                            sort
                                            (lambda (item) (nth index (item-cells pane item)))))))
          (if (list-panel-sort-descending pane) (reverse sorted) sorted)))))

(defun sort-by-column (pane index)
  "The user clicked the title of the column INDEX."
  (cond ((not (eql index (list-panel-sort-column pane)))
         (setf (list-panel-sort-column pane) index
               (list-panel-sort-descending pane) nil))
        ((list-panel-sort-descending pane)
         (setf (list-panel-sort-column pane) nil
               (list-panel-sort-descending pane) nil))
        (t (setf (list-panel-sort-descending pane) t))))

(defmethod render-pane ((pane multi-column-list-panel))
  (let ((columns (list-panel-columns pane))
        (sort-column (list-panel-sort-column pane))
        (descending (list-panel-sort-descending pane)))
    (html
      (:div :class "ui-multi-column-list-panel" :tabindex (if (pane-enabled pane) 0 -1)
            :on-keydown (lambda (&optional event)
                          (move-selection pane (event-get *backend* event "key") event))
        (:table :role "grid"
          (:thead
            (:tr (loop for column in columns
                       for index from 0
                       collect (let ((index index)
                                     (sorted (eql index sort-column)))
                                 (:th :style (let ((width (getf column :width)))
                                               (and width (format nil "width:~A" (css-length width))))
                                      :class (format nil "ui-adjust-~(~A~)~:[~; ui-sortable~]"
                                                     (getf column :adjust :left) (getf column :sort))
                                      :aria-sort (and sorted (if descending "descending" "ascending"))
                                      :on-click (and (getf column :sort)
                                                     (lambda (&optional event)
                                                       (declare (ignore event))
                                                       (sort-by-column pane index)))
                                   (getf column :title "")
                                   (and sorted (:span :class "ui-sort-mark" (if descending "▼" "▲"))))))))
          (:tbody
            (mapcar (lambda (item)
                      (let ((selected (item-selected-p pane item))
                            (cells (item-cells pane item)))
                        (item-element
                         pane item
                         (:tr :class (if selected "ui-item selected" "ui-item")
                              :aria-selected (if selected "true" "false")
                              :on-click (lambda (&optional event) (click-item pane item event))
                              :on-dblclick (lambda (&optional event)
                                             (declare (ignore event))
                                             (activate-item pane item))
                           (loop for cell in cells
                                 for index from 0
                                 for column = (nth index columns)
                                 collect (:td :class (format nil "ui-adjust-~(~A~)" (getf column :adjust :left))
                                           (if (node-p cell) cell (text-of cell))))))))
                    (choice-visible-items pane))))))))

;;; a menu of items

(defmodel option-pane (choice) ()
  (:documentation "A menu that shows the selected one of its items."))

(defmethod choice-selected-item ((pane option-pane))
  (or (choice-selection pane) (first (collection-items pane))))

(defmethod choice-selected-items ((pane option-pane))
  (let ((item (choice-selected-item pane)))
    (and item (list item))))

(defmethod render-pane ((pane option-pane))
  (let ((items (collection-items pane))
        (selected (choice-selected-item pane)))
    (html
      (:select :class "ui-option-pane" :disabled (not (pane-enabled pane))
               :on-change (lambda (&optional event)
                            (let ((index (event-target-get *backend* event "selectedIndex")))
                              (when (and index (< -1 index (length items)))
                                (select-item pane (nth index items)))))
        (mapcar (lambda (item)
                  (:option :selected (and (funcall (collection-test pane) item selected) t)
                    (print-item pane item)))
                items)))))

;;; buttons for the items

(defvar *button-panels* 0)

(defmodel radio-button-panel (choice)
  ((orientation :initarg :orientation :initform :row :accessor button-panel-orientation)
   (id :initform (incf *button-panels*) :reader button-panel-id))
  (:documentation "A button for every item, one of which is selected."))

(defmodel check-button-panel (radio-button-panel) ()
  (:default-initargs :interaction :multiple-selection)
  (:documentation "A check box for every item."))

(defmethod render-pane ((pane radio-button-panel))
  (let ((multiple (multiple-selection-p pane)))
    (html
      (:div :class (format nil "ui-button-panel ui-~(~A~)" (button-panel-orientation pane))
            :role (if multiple "group" "radiogroup")
        (mapcar (lambda (item)
                  (:label :class "ui-check-button"
                    (:input :type (if multiple "checkbox" "radio")
                            :name (format nil "ui-button-panel-~D" (button-panel-id pane))
                            :checked (item-selected-p pane item)
                            :disabled (not (pane-enabled pane))
                            :on-change (lambda (&optional event)
                                         (declare (ignore event))
                                         (select-item pane item :toggle multiple)))
                    (:span (print-item pane item))))
                (collection-items pane))))))

;;; a tree

(defmodel tree-view (choice)
  (;; a function of an item that returns the items under it
   (children-function :initarg :children-function :initform (constantly nil)
                      :accessor tree-view-children-function)
   ;; a function of an item: whether it is open before the user says so
   (expandp-function :initarg :expandp-function :initform (constantly nil)
                     :accessor tree-view-expandp-function)
   ;; what the user said: ((item . open)...)
   (expanded :initform nil :accessor tree-view-expanded))
  (:default-initargs :stretch t)
  (:documentation "The items :ROOTS and what CHILDREN-FUNCTION returns for them, as a tree."))

(defmethod initialize-instance :after ((pane tree-view) &key (roots nil rootsp))
  (when rootsp (setf (collection-items pane) roots)))

(defun tree-view-roots (pane) (collection-items pane))
(defun (setf tree-view-roots) (roots pane) (setf (collection-items pane) roots))

(defun tree-view-expanded-p (pane item)
  (let ((entry (assoc item (tree-view-expanded pane) :test (collection-test pane))))
    (if entry
        (cdr entry)
        (and (funcall (tree-view-expandp-function pane) item) t))))

(defun (setf tree-view-expanded-p) (open pane item)
  (setf (tree-view-expanded pane)
        (acons item open (remove item (tree-view-expanded pane)
                                 :key #'car :test (collection-test pane))))
  open)

(defun tree-children (pane item)
  (funcall (tree-view-children-function pane) item))

(defmethod choice-visible-items ((pane tree-view))
  (let ((result '()))
    (labels ((visit (item)
               (push item result)
               (when (tree-view-expanded-p pane item)
                 (mapc #'visit (tree-children pane item)))))
      (mapc #'visit (collection-items pane)))
    (nreverse result)))

(defun tree-key (pane key event)
  (let ((item (choice-selected-item pane)))
    (cond ((move-selection pane key event))
          ((null item))
          ((and (equal key "ArrowRight") (tree-children pane item))
           (setf (tree-view-expanded-p pane item) t))
          ((equal key "ArrowLeft")
           (setf (tree-view-expanded-p pane item) nil)))))

(defmethod render-pane ((pane tree-view))
  (html
   (labels ((row (item depth)
             (let* ((children (tree-children pane item))
                    (open (and children (tree-view-expanded-p pane item)))
                    (selected (item-selected-p pane item)))
               (:li :role "treeitem" :aria-expanded (and children (if open "true" "false"))
                    :aria-selected (if selected "true" "false")
                 (item-element
                  pane item
                  (:div :class (if selected "ui-item selected" "ui-item")
                        :style (format nil "padding-left:~Dpx" (+ 6 (* 16 depth)))
                        :on-click (lambda (&optional event) (click-item pane item event))
                        :on-dblclick (lambda (&optional event)
                                       (declare (ignore event))
                                       (when children
                                         (setf (tree-view-expanded-p pane item) (not open)))
                                       (activate-item pane item))
                    (:span :class (cond ((null children) "ui-twisty ui-leaf")
                                        (open "ui-twisty ui-open")
                                        (t "ui-twisty"))
                           :on-click (lambda (&optional event)
                                       (when children
                                         (when event (stop-event *backend* event))
                                         (setf (tree-view-expanded-p pane item) (not open)))))
                    (:span :class "ui-item-text" (print-item pane item))))
                 (when open
                   (:ul :role "group"
                     (mapcar (lambda (child) (row child (1+ depth))) children)))))))
     (:ul :class "ui-tree-view" :role "tree" :tabindex (if (pane-enabled pane) 0 -1)
          :on-keydown (lambda (&optional event)
                        (tree-key pane (event-get *backend* event "key") event))
       (mapcar (lambda (root) (row root 0)) (collection-items pane))))))

;;; ------------------------------------------------------------------
;;; Ranges

(defmodel range (pane callbacks)
  ((start :initarg :start :initform 0 :accessor range-start)
   (end :initarg :end :initform 100 :accessor range-end)
   (value :initarg :value :initform 0 :accessor range-value)))

(defmodel slider (range)
  ((step :initarg :step :initform 1 :accessor slider-step)
   ;; called with the value when the user moves the slider
   (callback :initarg :callback :initform nil :accessor slider-callback))
  (:documentation "A number between START and END that the user sets."))

(defun parse-number (string)
  "The number STRING is the decimal notation of, or NIL."
  (let* ((string (string-trim " " string))
         (dot (position #\. string))
         (integer (parse-integer string :end dot :junk-allowed t))
         (negative (and (plusp (length string)) (char= (char string 0) #\-))))
    (cond ((null integer) nil)
          ((null dot) (and (every (lambda (c) (or (digit-char-p c) (find c "+-"))) string) integer))
          (t (let ((fraction (subseq string (1+ dot))))
               (and (every #'digit-char-p fraction)
                    (if (zerop (length fraction))
                        integer
                        (let ((value (/ (parse-integer fraction) (expt 10.0d0 (length fraction)))))
                          (if negative (- integer value) (+ integer value))))))))))

(defmethod render-pane ((pane slider))
  (html (:input :type "range" :class "ui-slider"
                :min (range-start pane) :max (range-end pane) :step (slider-step pane)
                :value (text-of (range-value pane))
                :disabled (not (pane-enabled pane))
                :on-input (lambda (&optional event)
                            (let* ((value (event-target-get *backend* event "value"))
                                   (number (if (stringp value) (parse-number value) value)))
                              (when number
                                (setf (range-value pane) number)
                                (call-callback pane (slider-callback pane) :data number)))))))

(defmodel progress-bar (range) ()
  (:documentation "How far something is, from START to END."))

(defmethod render-pane ((pane progress-bar))
  (html (:progress :class "ui-progress-bar"
                   :max (- (range-end pane) (range-start pane))
                   :value (- (range-value pane) (range-start pane)))))

;;; ------------------------------------------------------------------
;;; Markup in a pane

(defmodel application-pane (pane)
  (;; a function of the pane that returns what it shows: a node or a list of nodes
   (display-function :initarg :display-function :initform (constantly nil)
                     :accessor application-pane-display-function))
  (:default-initargs :stretch t)
  (:documentation "A pane that shows the markup its DISPLAY-FUNCTION returns (see
HTML and PRESENT); it is called again when state it read has changed."))

(defmethod render-pane ((pane application-pane))
  (html (:div :class "ui-application-pane"
          (funcall (application-pane-display-function pane) pane))))

;;; ------------------------------------------------------------------
;;; Commands in panes

(defun command-button-node (item)
  (let* ((name (if (consp item) (car item) item))
         (arguments (and (consp item) (cdr item)))
         (command (or (find-command name) (error "UI: ~S is not a command" name))))
    (html (:button :type "button" :class "ui-push-button"
                   :disabled (not (command-enabled-p name))
                   :title (key-label (command-key command))
                   :on-click (lambda (&optional event)
                               (declare (ignore event))
                               (apply #'invoke-command name arguments))
            (command-label command)))))

(defmodel toolbar (pane)
  (;; the names of commands, (name argument...), panes and :SEPARATOR
   (items :initarg :items :initform nil :accessor toolbar-items))
  (:documentation "A row of buttons for commands."))

(defmethod render-pane ((pane toolbar))
  (html (:div :class "ui-toolbar" :role "toolbar"
          (mapcar (lambda (item)
                    (cond ((eq item :separator) (:span :class "ui-toolbar-separator"))
                          ((typep item 'pane) (pane-node item))
                          (t (command-button-node item))))
                  (toolbar-items pane)))))

(defcomponent menu-bar-view (&state (open nil))
  "The menus of the commands (COMMAND-MENUS), in the page."
  (flet ((close-menu () (setf open nil)))
    (:div :class "ui-menu-bar" :role "menubar"
      (mapcar (lambda (menu)
                (let ((title (car menu)))
                  (:div :class "ui-menu-bar-item"
                    (:button :type "button" :role "menuitem"
                             :class (if (equal open title) "ui-menu-title ui-open" "ui-menu-title")
                             :on-click (lambda (&optional event)
                                         (declare (ignore event))
                                         (setf open (if (equal open title) nil title)))
                             :on-mouseenter (lambda (&optional event)
                                              (declare (ignore event))
                                              (when open (setf open title)))
                      title)
                    (when (equal open title)
                      (menu-list (loop for command in (cdr menu)
                                       when (command-separator command) collect :separator
                                       collect (command-menu-item command))
                                 #'close-menu)))))
              (command-menus))
      (when open
        (:div :class "ui-menu-backdrop"
              :on-click (lambda (&optional event) (declare (ignore event)) (close-menu)))))))

;;; ------------------------------------------------------------------
;;; Layouts

(defmodel layout (pane)
  (;; the panes: a pane, the name of a pane of the interface or a string;
   ;; in rows and columns also :DIVIDER (the user moves it) and :SEPARATOR
   (description :initarg :description :initform nil :accessor layout-description)
   ;; how the free space is divided: a weight (or NIL) for every element of
   ;; the description; without it, the panes that stretch share it
   (ratios :initarg :ratios :initform nil :accessor layout-ratios)
   ;; where the panes are across the direction of the layout: NIL (each
   ;; kind of pane as usual), :START, :CENTER, :END or :STRETCH
   (adjust :initarg :adjust :initform nil :accessor layout-adjust)
   (gap :initarg :gap :initform nil :accessor layout-gap)
   ;; the sizes the user gave with dividers: ((index . pixels)...)
   (sizes :initform nil :accessor layout-sizes)
   (drag :initform nil :accessor layout-drag))
  (:default-initargs :stretch t)
  (:documentation "A pane that arranges the panes of its DESCRIPTION."))

(defun resolve-child (layout child)
  "The pane for the element CHILD of the description of LAYOUT."
  (let* ((interface (pane-interface layout))
         (pane (if (symbolp child)
                   (or (and interface (find-pane interface child))
                       (error "UI: the layout ~S has no pane named ~S" (pane-name layout) child))
                   child)))
    (when (and interface (null (pane-interface pane)))
      (setf (pane-interface pane) interface))
    pane))

(defun child-node (layout child &optional flex)
  "The node for the element CHILD (a pane, the name of one or a string) of
the description of LAYOUT."
  (if (stringp child)
      (html (:span :class "ui-pane ui-title-pane" child))
      (pane-node (resolve-child layout child) flex)))

(defun layout-class (layout class)
  (format nil "~A~@[ ui-adjust-~(~A~)~]" class (layout-adjust layout)))

(defun layout-style (layout)
  (and (layout-gap layout) (format nil "gap:~A;" (css-length (layout-gap layout)))))

(defun divider (layout index horizontal)
  "The divider at INDEX of the description of LAYOUT: dragging it sets the
size of the pane in front of it."
  (flet ((position-of (event)
           (or (event-get *backend* event (if horizontal "clientX" "clientY")) 0)))
    (html
      (:div :class "ui-divider" :role "separator"
            :aria-orientation (if horizontal "vertical" "horizontal")
            :on-pointerdown (lambda (&optional event)
                              (multiple-value-bind (width height) (previous-sibling-size *backend* event)
                                (when event (stop-event *backend* event))
                                (setf (layout-drag layout)
                                      (list index (position-of event) (if horizontal width height)))))
            :on-pointermove (lambda (&optional event)
                              (let ((drag (layout-drag layout)))
                                (when (and drag (= (first drag) index))
                                  (let ((size (max 24 (round (+ (third drag)
                                                                (- (position-of event) (second drag)))))))
                                    (setf (layout-sizes layout)
                                          (acons (1- index) size
                                                 (remove (1- index) (layout-sizes layout) :key #'car)))))))
            :on-pointerup (lambda (&optional event)
                            (declare (ignore event))
                            (setf (layout-drag layout) nil))
            :on-pointercancel (lambda (&optional event)
                                (declare (ignore event))
                                (setf (layout-drag layout) nil))))))

(defun render-box (layout class horizontal)
  (let ((ratios (layout-ratios layout))
        (sizes (layout-sizes layout)))
    (html
      (:div :class (layout-class layout class) :style (layout-style layout)
        (loop for child in (layout-description layout)
              for index from 0
              for ratio = (nth index ratios)
              for size = (cdr (assoc index sizes))
              collect (cond ((null child) nil)
                            ((eq child :divider) (divider layout index horizontal))
                            ((eq child :separator) (:div :class "ui-separator" :role "separator"))
                            (t (child-node layout child
                                           (cond (size (format nil "flex:0 0 ~Dpx" size))
                                                 (ratio (format nil "flex:~D 1 0" ratio)))))))))))

(defmodel column-layout (layout) ()
  (:documentation "The panes one below the other."))

(defmethod render-pane ((pane column-layout))
  (render-box pane "ui-column-layout" nil))

(defmodel row-layout (layout) ()
  (:documentation "The panes side by side."))

(defmethod render-pane ((pane row-layout))
  (render-box pane "ui-row-layout" t))

(defmodel grid-layout (layout)
  ((columns :initarg :columns :initform 2 :accessor grid-layout-columns))
  (:documentation "The panes in rows of COLUMNS columns.  RATIOS are the weights of the
columns (NIL: as wide as needed); by default the last column takes the rest."))

(defmethod render-pane ((pane grid-layout))
  (let* ((columns (grid-layout-columns pane))
         (ratios (or (layout-ratios pane)
                     (append (make-list (1- columns)) (list 1)))))
    (html
      (:div :class (layout-class pane "ui-grid-layout")
            :style (format nil "~@[~A~]grid-template-columns:~{~A~^ ~}"
                           (layout-style pane)
                           (loop for i below columns
                                 for ratio = (nth i ratios)
                                 collect (if ratio (format nil "minmax(0,~Dfr)" ratio) "auto")))
        (mapcar (lambda (child)
                  (cond ((null child) (:span))
                        (t (child-node pane child))))
                (layout-description pane))))))

(defmodel switchable-layout (layout)
  ((visible-child :initarg :visible-child :initform nil :accessor switchable-layout-visible-child))
  (:documentation "Shows one of its panes, VISIBLE-CHILD (by default the first)."))

(defmethod render-pane ((pane switchable-layout))
  (let ((child (or (switchable-layout-visible-child pane)
                   (first (layout-description pane)))))
    (html (:div :class "ui-switchable-layout"
            (and child (child-node pane child "flex:1 1 0"))))))

(defmodel tab-layout (choice layout)
  (;; a function of an item that returns its pane (or the name of one)
   (visible-child-function :initarg :visible-child-function :initform #'second
                           :accessor tab-layout-visible-child-function))
  (:default-initargs :print-function #'first)
  (:documentation "A tab for every item — by default an item is (title pane) — and the
pane of the selected one."))

;; in DEFINE-INTERFACE, the description of a tab layout is its items
(defmethod initialize-instance :after ((pane tab-layout) &key)
  (when (and (null (collection-items pane)) (layout-description pane))
    (setf (collection-items pane) (layout-description pane))))

(defmethod choice-selected-item ((pane tab-layout))
  (or (choice-selection pane) (first (collection-items pane))))

(defmethod render-pane ((pane tab-layout))
  (let ((selected (choice-selected-item pane)))
    (html
      (:div :class "ui-tab-layout"
        (:div :class "ui-tabs" :role "tablist"
          (mapcar (lambda (item)
                    (let ((current (funcall (collection-test pane) item selected)))
                      (:button :type "button" :role "tab"
                               :class (if current "ui-tab selected" "ui-tab")
                               :aria-selected (if current "true" "false")
                               :on-click (lambda (&optional event)
                                           (declare (ignore event))
                                           (select-item pane item))
                        (print-item pane item))))
                  (collection-items pane)))
        (:div :class "ui-tab-body" :role "tabpanel"
          (and selected
               (child-node pane (funcall (tab-layout-visible-child-function pane) selected)
                           "flex:1 1 0")))))))

;;; ------------------------------------------------------------------
;;; Interfaces

(defmodel interface (pane)
  (;; the pane it shows, or the name of one of its panes
   (layout :initarg :layout :initform nil :accessor interface-layout)
   ;; true to show the menus of the commands above the layout (in the
   ;; desktop application they are in its menu bar instead)
   (menu-bar :initarg :menu-bar :initform nil :accessor interface-menu-bar)
   ;; the name of a command table: its commands are offered (menus, keys)
   ;; while the interface is on the screen
   (command-table :initarg :command-table :initform nil :accessor interface-command-table)
   (root :initform nil :accessor interface-root)
   (title-reaction :initform nil :accessor interface-title-reaction))
  (:default-initargs :stretch t)
  (:documentation "A window: panes in a layout, with a title.  See DEFINE-INTERFACE."))

(defun interface-title (interface) (pane-title interface))
(defun (setf interface-title) (title interface) (setf (pane-title interface) title))

(defmethod pane-shows-title-p ((pane interface)) nil)

(defgeneric make-interface-panes (interface)
  (:documentation "Make the panes and layouts of INTERFACE (DEFINE-INTERFACE defines a method).")
  (:method (interface) nil))

(defmethod initialize-instance :after ((interface interface) &key)
  (setf (pane-interface interface) interface)
  (make-interface-panes interface))

(defgeneric find-pane (interface name)
  (:documentation "The pane or layout of INTERFACE that is named NAME, or NIL.")
  (:method ((interface interface) name)
    (and (slot-exists-p interface name)
         (slot-boundp interface name)
         (let ((pane (slot-value interface name)))
           (and (typep pane 'pane) pane)))))

(defmethod render-pane ((interface interface))
  (let ((layout (interface-layout interface)))
    (html
      (:div :class "ui-interface"
        (when (and (interface-menu-bar interface) (not (native-menus-p *backend*)))
          (menu-bar-view))
        (and layout (child-node interface layout "flex:1 1 0"))))))

(defmacro define-interface (name superclasses slots &rest options)
  "Define the class NAME of interfaces, like DEFMODEL with the options
  (:panes (name class initarg...)...)
  (:layouts (name class description initarg...)...)
Every instance has these panes and layouts, made with MAKE-INSTANCE from the
initargs (:ACCESSOR and :READER name functions for the slot instead) and
kept in the slots NAME.  The initarg forms are evaluated when the instance
is made, with INTERFACE bound to it and its slots, also the panes made so
far, as variables.  The interface shows the first layout (or the one named
by the initarg :LAYOUT); without layouts, its panes in a column."
  (let* ((panes (cdr (assoc :panes options)))
         (layouts (cdr (assoc :layouts options)))
         (others (remove-if (lambda (option) (member (car option) '(:panes :layouts))) options))
         (interface 'interface)
         (slot-names (append (mapcar (lambda (slot) (if (consp slot) (car slot) slot)) slots)
                             (mapcar #'car panes) (mapcar #'car layouts))))
    (dolist (pane (append panes layouts))
      (when (member (car pane) '(name interface title title-position enabled visible help class
                                 width height min-width min-height max-width max-height stretch
                                 layout menu-bar command-table root title-reaction))
        (error "DEFINE-INTERFACE ~S: a pane cannot be named ~S, which is a slot of every interface"
               name (car pane))))
    (flet ((slot-options (initargs)
             (loop for (key value) on initargs by #'cddr
                   when (member key '(:accessor :reader)) collect key and collect value))
           (initargs (initargs)
             (loop for (key value) on initargs by #'cddr
                   unless (member key '(:accessor :reader)) collect key and collect value)))
      `(progn
         (defmodel ,name (,@superclasses interface)
           (,@slots
            ,@(mapcar (lambda (pane) `(,(car pane) :initform nil ,@(slot-options (cddr pane)))) panes)
            ,@(mapcar (lambda (layout) `(,(car layout) :initform nil ,@(slot-options (cdddr layout))))
                      layouts))
           ,@others)
         (defmethod make-interface-panes ((,interface ,name))
           (call-next-method)
           (with-slots ,slot-names ,interface
             ,@(mapcar (lambda (pane)
                         (destructuring-bind (pane-name class &rest initargs) pane
                           `(setf ,pane-name (make-instance ',class :name ',pane-name :interface ,interface
                                                                    ,@(initargs initargs)))))
                       panes)
             ,@(mapcar (lambda (layout)
                         (destructuring-bind (layout-name class description &rest initargs) layout
                           `(setf ,layout-name (make-instance ',class :name ',layout-name
                                                                      :interface ,interface
                                                                      :description ,description
                                                                      ,@(initargs initargs)))))
                       layouts))
           (unless (interface-layout ,interface)
             (setf (interface-layout ,interface)
                   ,(if layouts
                        `',(car (first layouts))
                        `(make-instance 'column-layout :interface ,interface
                                                       :description ',(mapcar #'car panes))))))
         ',name))))

(defvar *command-table-users* nil "The interfaces on the screen that have a command table.")

(defun use-command-table (interface use)
  (when (interface-command-table interface)
    (setf *command-table-users* (remove interface *command-table-users*))
    (when use (push interface *command-table-users*))
    (setf *command-tables*
          (remove-duplicates (mapcar #'interface-command-table *command-table-users*)))))

(defun display (interface &key (container "#app"))
  "Show INTERFACE in CONTAINER (for the DOM, a CSS selector or a node); the
title of the window is its title.  Return the interface."
  (let ((backend *backend*))
    (use-command-table interface t)
    (setf (interface-root interface) (mount (pane-node interface) container)
          (interface-title-reaction interface)
          (autorun (lambda ()
                     (let ((title (pane-title interface)))
                       (when title (set-title backend title))))))
    interface))

(defun contain (pane &rest initargs)
  "Show PANE in an interface made for it (with INITARGS, like :TITLE);
return the interface."
  (let ((container (getf initargs :container "#app"))
        (initargs (loop for (key value) on initargs by #'cddr
                        unless (eq key :container) collect key and collect value)))
    (display (apply #'make-instance 'interface :layout pane initargs) :container container)))

(defun quit-interface (interface)
  "Take INTERFACE (or the interface of a pane) off the screen."
  (let ((interface (pane-interface interface)))
    (cond ((interface-root interface)
           (unmount (interface-root interface))
           (use-command-table interface nil)
           (stop (interface-title-reaction interface))
           (setf (interface-root interface) nil))
          ;; dialog.lisp
          (t (let ((dialog (find interface *dialogs* :key #'dialog-interface)))
               (when dialog (abort-dialog dialog)))))
    nil))
