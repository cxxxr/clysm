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
  (;; its name in its interface (see FIND-PANE)
   (name :initarg :name :initform nil :accessor pane-name)
   ;; the interface it belongs to
   (interface :initarg :interface :initform nil :accessor pane-interface)
   ;; a label shown with it, or NIL
   (title :initarg :title :initform nil :accessor pane-title)
   ;; :TOP, :LEFT or :FRAME (a box around the pane)
   (title-position :initarg :title-position :initform :top :accessor pane-title-position)
   ;; false to show it disabled: the user cannot use it
   (enabled :initarg :enabled :initform t :accessor pane-enabled)
   ;; false to hide it
   (visible :initarg :visible :initform t :accessor pane-visible)
   ;; the text of its tooltip, or NIL
   (help :initarg :help :initform nil :accessor pane-help)
   ;; more CSS classes of its element, or NIL
   (class :initarg :class :initform nil :accessor pane-class)
   ;; its width: a number of pixels or a CSS length
   (width :initarg :width :initform nil :accessor pane-width)
   ;; its height: a number of pixels or a CSS length
   (height :initarg :height :initform nil :accessor pane-height)
   ;; the smallest width it gets
   (min-width :initarg :min-width :initform nil :accessor pane-min-width)
   ;; the smallest height it gets
   (min-height :initarg :min-height :initform nil :accessor pane-min-height)
   ;; the largest width it gets
   (max-width :initarg :max-width :initform nil :accessor pane-max-width)
   ;; the largest height it gets
   (max-height :initarg :max-height :initform nil :accessor pane-max-height)
   ;; whether it takes the free space of its row or column: T, or a weight
   (stretch :initarg :stretch :initform nil :accessor pane-stretch)
   ;; the menu of the pane (right click): a menu, the name of one of the
   ;; interface, or a function of the pane that returns the items of a
   ;; popup menu (see POPUP-MENU)
   (pane-menu :initarg :pane-menu :initform nil :accessor pane-menu))
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
    (when (pane-menu pane)
      (setf (getf (element-props node) :on-contextmenu)
            (lambda (&optional event)
              (let ((items (pane-menu-items pane)))
                (when items
                  (when event (stop-event *backend* event))
                  (popup-menu items
                              (or (event-get *backend* event "clientX") 0)
                              (or (event-get *backend* event "clientY") 0)))))))
    node))

(defcomponent pane-view (pane flex)
  (pane-element pane flex))

(defgeneric pane-mounted (pane)
  (:documentation "Called when PANE has been put on the display.")
  (:method (pane) nil))
(defgeneric pane-unmounted (pane)
  (:documentation "Called when PANE has been removed from the display.")
  (:method (pane) nil))

(defmethod mounted ((self pane-view))
  (pane-mounted (first (component-args self))))
(defmethod unmounted ((self pane-view))
  (pane-unmounted (first (component-args self))))

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
  (;; what the callbacks are called with: a list of :DATA, :ITEM, :INTERFACE
   ;; and :ELEMENT (the pane), a keyword made of them like :DATA-INTERFACE, or :NONE
   (callback-type :initarg :callback-type :initform '(:data :interface) :accessor pane-callback-type))
  (:documentation "A pane with callbacks, which are called with what CALLBACK-TYPE
says: by default the data and the interface."))

;;; ------------------------------------------------------------------
;;; Text

(defmodel title-pane (pane)
  (;; the text: a string, or a number or other object that is printed
   (text :initarg :text :initform "" :accessor title-pane-text))
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
  (;; the text; a newline in it starts a new line
   (text :initarg :text :initform "" :accessor display-pane-text))
  (:documentation "Text of several lines that the user reads."))

(defmethod render-pane ((pane display-pane))
  (html (:div :class "ui-display-pane" (text-of (display-pane-text pane)))))

(defmodel text-input-pane (pane callbacks)
  (;; the text in the field; it changes as the user types
   (text :initarg :text :initform "" :accessor text-input-pane-text)
   ;; the text shown, dimmed, while the field is empty
   (placeholder :initarg :placeholder :initform nil :accessor text-input-pane-placeholder)
   ;; true to hide what is typed
   (password :initarg :password :initform nil :accessor text-input-pane-password)
   ;; true if the user cannot change the text
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
  (;; how many lines high it is
   (rows :initarg :rows :initform 4 :accessor text-input-pane-rows))
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
  (;; the label (by default that of its command)
   (text :initarg :text :initform nil :accessor button-text)
   ;; what the callback gets as the data (by default the text)
   (data :initarg :data :initform nil :accessor button-data)
   ;; called with the data when the button is pressed
   (callback :initarg :callback :initform nil :accessor button-callback)
   ;; the name of a command, or (name argument...): the button invokes it,
   ;; has its label and is enabled while the command is
   (command :initarg :command :initform nil :accessor button-command)
   ;; true for the button that answers its dialog: it is emphasized
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
  (;; the label
   (text :initarg :text :initform "" :accessor button-text)
   ;; whether it is checked
   (selected :initarg :selected :initform nil :accessor button-selected)
   ;; called with whether it is checked when the user changes it
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
  (;; the items: any objects
   (items :initarg :items :initform nil :accessor collection-items)
   ;; a function of an item that returns its text (or a node)
   (print-function :initarg :print-function :initform nil :accessor collection-print-function)
   ;; how items are compared (selection, keys of the display)
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
   ;; if true, the items are presentations of this type (T: PRESENTATION-TYPE-OF;
   ;; a function: of what it returns for the item)
   (presentation-type :initarg :presentation-type :initform nil :accessor choice-presentation-type))
  (:documentation "The superclass of the panes that show items of which the user selects
some: lists, tables, trees, option panes, button panels, tabs, graphs."))

(defun multiple-selection-p (choice)
  (eq (choice-interaction choice) :multiple-selection))

(defgeneric choice-selected-items (choice)
  (:documentation "The list of the selected items of CHOICE.")
  (:method ((choice choice))
    (let ((selection (choice-selection choice)))
      (cond ((multiple-selection-p choice) selection)
            (selection (list selection))))))

(defgeneric choice-selected-item (choice)
  (:documentation "The selected item of CHOICE (the first, with multiple selection), or NIL.")
  (:method ((choice choice))
    (if (multiple-selection-p choice)
        (first (choice-selection choice))
        (choice-selection choice))))

(defun (setf choice-selected-items) (items choice)
  "Select the ITEMS of CHOICE (with single selection, the first of them)."
  (setf (choice-selection choice) (if (multiple-selection-p choice) items (first items)))
  items)

(defun (setf choice-selected-item) (item choice)
  "Select ITEM of CHOICE, and nothing else (NIL: nothing)."
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
        (presenting item (cond ((eq type t) nil)
                               ((functionp type) (funcall type item))
                               (t type))
                    node)
        node)))

;;; a list

(defmodel list-panel (choice)
  (;; true to show a field above the list: only the items whose text contains
   ;; what the user typed there are shown
   (filter :initarg :filter :initform nil :accessor list-panel-filter)
   ;; what the user typed in the field of the filter
   (filter-text :initform "" :accessor list-panel-filter-text))
  (:default-initargs :stretch t)
  (:documentation "A list of items."))

(defun item-text (pane item)
  (let ((printed (print-item pane item)))
    (if (stringp printed) printed (text-of item))))

(defmethod choice-visible-items ((pane list-panel))
  (let ((text (list-panel-filter-text pane)))
    (if (and (list-panel-filter pane) (plusp (length text)))
        (remove-if-not (lambda (item) (search text (item-text pane item) :test #'char-equal))
                       (collection-items pane))
        (collection-items pane))))

(defmethod render-pane ((pane list-panel))
  (let ((list (render-list pane)))
    (if (list-panel-filter pane)
        (html
          (:div :class "ui-filtered-list"
            (:input :type "search" :class "ui-text-input-pane ui-list-filter"
                    :value (list-panel-filter-text pane)
                    :placeholder (if (stringp (list-panel-filter pane)) (list-panel-filter pane) nil)
                    :on-input (lambda (&optional event)
                                (setf (list-panel-filter-text pane)
                                      (event-target-get *backend* event "value")))
                    :on-keydown (lambda (&optional event)
                                  (move-selection pane (event-get *backend* event "key") event)))
            list))
        list)))

(defun render-list (pane)
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
              (choice-visible-items pane)))))

;;; a table

(defmodel multi-column-list-panel (list-panel)
  (;; a list of (:title string :width size :adjust :left/:right/:center)
   (columns :initarg :columns :initform nil :accessor list-panel-columns)
   ;; a function of an item that returns the list of what its columns show
   (column-function :initarg :column-function :initform nil :accessor list-panel-column-function)
   ;; the index of the column the items are sorted by (NIL: as they are)
   (sort-column :initarg :sort-column :initform nil :accessor list-panel-sort-column)
   ;; true if they are sorted from the largest to the smallest
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
  (let* ((items (call-next-method))
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
  (;; :ROW or :COLUMN
   (orientation :initarg :orientation :initform :row :accessor button-panel-orientation)
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

(defun tree-view-roots (pane)
  "The items at the top of the tree PANE (its COLLECTION-ITEMS)."
  (collection-items pane))
(defun (setf tree-view-roots) (roots pane)
  (setf (collection-items pane) roots))

(defun tree-view-expanded-p (pane item)
  "True if ITEM of the tree PANE is open: the items under it are shown.
SETF of it opens or closes it."
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
  (;; the smallest value
   (start :initarg :start :initform 0 :accessor range-start)
   ;; the largest value
   (end :initarg :end :initform 100 :accessor range-end)
   ;; the value
   (value :initarg :value :initform 0 :accessor range-value))
  (:documentation "The superclass of the panes that show a number between START and END."))

(defmodel slider (range)
  (;; the steps in which the value changes
   (step :initarg :step :initform 1 :accessor slider-step)
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

;;; ------------------------------------------------------------------
;;; Menus (the :MENUS and :MENU-BAR of DEFINE-INTERFACE)

(defmodel menu ()
  (;; its name in its interface
   (name :initarg :name :initform nil :accessor menu-name)
   ;; the interface it belongs to
   (interface :initarg :interface :initform nil :accessor menu-interface)
   ;; the character after & is the mnemonic: Alt and it opens the menu
   (title :initarg :title :initform "" :accessor menu-title)
   ;; menu entries, :SEPARATOR, menus (or their names in the interface: they
   ;; open beside this one), names of commands and (command argument...)
   (items :initarg :items :initform nil :accessor menu-items)
   ;; false to show it disabled
   (enabled :initarg :enabled :initform t :accessor menu-enabled))
  (:documentation "A menu of an interface."))

(defmodel menu-entry (callbacks)
  (;; the interface it belongs to: its callback gets it
   (interface :initarg :interface :initform nil :accessor menu-entry-interface)
   ;; the text; the character after & is its mnemonic
   (title :initarg :title :initform "" :accessor menu-entry-title)
   ;; what the callback gets as the data (by default the title)
   (data :initarg :data :initform nil :accessor menu-entry-data)
   ;; called with the data (by default the title; for an entry with a check
   ;; mark, whether it is checked now) and the interface
   (callback :initarg :callback :initform nil :accessor menu-entry-callback)
   ;; false to show it disabled
   (enabled :initarg :enabled :initform t :accessor menu-entry-enabled)
   ;; like "CmdOrCtrl+E"
   (key :initarg :key :initform nil :accessor menu-entry-key)
   ;; :NONE, or whether the entry has a check mark; choosing it changes that
   (checked :initarg :checked :initform :none :accessor menu-entry-checked))
  (:documentation "An entry of a menu.  Like the slots of panes, ENABLED and CHECKED can
be (COMPUTED form) or (BINDING place)."))

(defun choose-menu-entry (entry)
  (let ((checked (menu-entry-checked entry))
        (interface (menu-entry-interface entry)))
    (flet ((call (data)
             (when (menu-entry-callback entry)
               (apply (menu-entry-callback entry)
                      (mapcar (lambda (key)
                                (ecase key
                                  ((:data :item) data)
                                  (:interface interface)
                                  (:element entry)))
                              (callback-type-list (pane-callback-type entry)))))))
      (cond ((eq checked :none)
             (call (or (menu-entry-data entry) (parse-mnemonic (menu-entry-title entry)))))
            (t (setf (menu-entry-checked entry) (not checked))
               (call (not checked)))))))

(defun resolve-menu-item (interface item)
  "The menu item (of menus on the screen) for the element ITEM of a menu of INTERFACE."
  (cond ((eq item :separator) item)
        ((menu-item-p item) item)
        ((typep item 'menu-entry)
         (make-menu-item (menu-entry-title item)
                         (lambda () (choose-menu-entry item))
                         :enabled (and (menu-entry-enabled item) t)
                         :key (menu-entry-key item)
                         :checked (let ((checked (menu-entry-checked item)))
                                    (if (eq checked :none) :none (and checked t)))))
        ((typep item 'menu)
         (make-menu-item (menu-title item) nil
                         :enabled (and (menu-enabled item) t)
                         :items (resolve-menu-items interface (menu-items item))))
        ((consp item)
         (apply #'command-menu-item (or (find-command (car item))
                                        (error "UI: ~S is not a command" (car item)))
                (cdr item)))
        ((and interface (find-menu interface item))
         (resolve-menu-item interface (find-menu interface item)))
        ((find-command item)
         (let ((menu-item (command-menu-item (find-command item))))
           (setf (menu-item-key menu-item) (command-key (find-command item)))
           menu-item))
        (t (error "UI: ~S is neither a menu of the interface nor a command" item))))

(defun resolve-menu-items (interface items)
  (mapcar (lambda (item) (resolve-menu-item interface item)) items))

(defun find-menu (interface name)
  "The menu of INTERFACE that is named NAME, or NIL."
  (and (symbolp name)
       (slot-exists-p interface name)
       (slot-boundp interface name)
       (let ((menu (slot-value interface name)))
         (and (typep menu 'menu) menu))))

(defun interface-menus (interface)
  "The menus of the menu bar of INTERFACE as ((title item...)...): the menus
that its :MENU-BAR names, and for T or :COMMANDS in it those of the commands."
  (let ((bar (interface-menu-bar interface)))
    (cond ((null bar) nil)
          ((eq bar t) (command-menu-items))
          (t (loop for name in bar
                   append (if (eq name :commands)
                              (command-menu-items)
                              (let ((menu (if (typep name 'menu)
                                              name
                                              (or (find-menu interface name)
                                                  (error "UI: ~S is not a menu of the interface" name)))))
                                (list (cons (menu-title menu)
                                            (resolve-menu-items interface (menu-items menu)))))))))))

(defun pane-menu-items (pane)
  (let ((menu (pane-menu pane))
        (interface (pane-interface pane)))
    (cond ((functionp menu) (funcall menu pane))
          ((typep menu 'menu) (resolve-menu-items interface (menu-items menu)))
          (t (let ((found (and interface (find-menu interface menu))))
               (and found (resolve-menu-items interface (menu-items found))))))))

(defstate *interfaces* nil "The interfaces on the screen, the latest first.")
(defstate *open-menu* nil "(interface . title) of the menu of a menu bar that is open, or NIL.")

(defun displayed-menus ()
  "The menus of the interfaces on the screen (for a menu bar outside the
page), in the order of their menu bars; the menus of the commands are there
once, where the first menu bar says :COMMANDS (or is T) — and alone if no
interface has a menu bar."
  (let ((menus '()) (commands nil))
    (flet ((add-commands ()
             (unless commands
               (setf commands t)
               (dolist (menu (command-menu-items)) (push menu menus)))))
      (dolist (interface (reverse *interfaces*))
        (let ((bar (interface-menu-bar interface)))
          (cond ((eq bar t) (add-commands))
                ((consp bar)
                 (dolist (name bar)
                   (if (eq name :commands)
                       (add-commands)
                       (let ((menu (if (typep name 'menu) name (find-menu interface name))))
                         (when menu
                           (push (cons (menu-title menu)
                                       (resolve-menu-items interface (menu-items menu)))
                                 menus))))))))))
    (if menus (nreverse menus) (command-menu-items))))

(setf *menu-bar-function* #'displayed-menus)

;;; the keys of menu entries

(defun menu-key-action (key)
  (labels ((search-items (interface items)
             (dolist (item items)
               (cond ((typep item 'menu-entry)
                      (when (and (menu-entry-key item)
                                 (equal (parse-key (menu-entry-key item)) key)
                                 (menu-entry-enabled item))
                        (return (lambda () (choose-menu-entry item)))))
                     ((or (typep item 'menu) (and (symbolp item) (find-menu interface item)))
                      (let* ((menu (if (typep item 'menu) item (find-menu interface item)))
                             (found (search-items interface (menu-items menu))))
                        (when found (return found))))))))
    (dolist (interface *interfaces*)
      (let ((bar (interface-menu-bar interface)))
        (when (consp bar)
          (let ((found (search-items interface (remove :commands bar))))
            (when found (return found))))))))

(defun mnemonic-key-action (key)
  "Alt and the mnemonic of a menu of a menu bar opens it; the mnemonic of an
item of the open menu chooses it."
  (flet ((mnemonic-p (label)
           (multiple-value-bind (text position) (parse-mnemonic label)
             (and position (= (length (car key)) 1)
                  (char-equal (char text position) (char (car key) 0))))))
    (cond ((native-menus-p *backend*) nil)
          ((equal (cdr key) '(:alt))
           (dolist (interface *interfaces*)
             (let ((menu (find-if #'mnemonic-p (interface-menus interface) :key #'car)))
               (when menu
                 (return (lambda () (setf *open-menu* (cons interface (car menu)))))))))
          ((and *open-menu* (null (cdr key)))
           (let* ((menu (assoc (cdr *open-menu*) (interface-menus (car *open-menu*)) :test #'equal))
                  (item (find-if (lambda (item)
                                   (and (menu-item-p item) (menu-item-enabled item)
                                        (null (menu-item-items item))
                                        (mnemonic-p (menu-item-label item))))
                                 (cdr menu))))
             (cond (item (lambda ()
                           (setf *open-menu* nil)
                           (funcall (menu-item-function item))))
                   ((equal (car key) "ESCAPE") (lambda () (setf *open-menu* nil)))))))))

(push #'menu-key-action *key-handlers*)
(push #'mnemonic-key-action *key-handlers*)

(defcomponent menu-bar-view (interface)
  "The menu bar of INTERFACE, in the page."
  (let ((open (and *open-menu* (eq (car *open-menu*) interface) (cdr *open-menu*))))
    (flet ((close-menu () (setf *open-menu* nil)))
      (:div :class "ui-menu-bar" :role "menubar"
        (mapcar (lambda (menu)
                  (let ((title (car menu)))
                    (:div :class "ui-menu-bar-item"
                      (:button :type "button" :role "menuitem"
                               :class (if (equal open title) "ui-menu-title ui-open" "ui-menu-title")
                               :on-click (lambda (&optional event)
                                           (declare (ignore event))
                                           (setf *open-menu* (if (equal open title) nil (cons interface title))))
                               :on-mouseenter (lambda (&optional event)
                                                (declare (ignore event))
                                                (when open (setf *open-menu* (cons interface title))))
                        (label-children title))
                      (when (equal open title)
                        (menu-list (cdr menu) #'close-menu)))))
                (interface-menus interface))
        (when open
          (:div :class "ui-menu-backdrop"
                :on-click (lambda (&optional event) (declare (ignore event)) (close-menu))))))))

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
   ;; the space between the panes: pixels or a CSS length (NIL: the usual)
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
  (;; how many panes there are in a row
   (columns :initarg :columns :initform 2 :accessor grid-layout-columns))
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
  (;; the pane it shows (or the name of one in the interface)
   (visible-child :initarg :visible-child :initform nil :accessor switchable-layout-visible-child))
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
;;; More panes

(defmodel password-pane (text-input-pane) ()
  (:default-initargs :password t)
  (:documentation "A text input pane that hides what is typed."))

(defmodel editor-pane (multi-line-text-input-pane) ()
  (:default-initargs :rows 12 :class "ui-editor-pane" :stretch t)
  (:documentation "Text of many lines that the user edits, in a fixed-width font."))

(defmodel radio-button (check-button)
  (;; the buttons with the same GROUP (any object) exclude each other
   (group :initarg :group :initform nil :accessor radio-button-group))
  (:documentation "A button that is selected instead of the others of its GROUP in the
same interface.  Its callback is called with its data when it is selected."))

(defvar *radio-buttons* nil "Every radio button that was shown.")

(defmethod render-pane ((pane radio-button))
  (pushnew pane *radio-buttons*)
  (html (:label :class "ui-check-button ui-radio-button"
          (:input :type "radio" :checked (and (button-selected pane) t)
                  :disabled (not (pane-enabled pane))
                  :on-change (lambda (&optional event)
                               (declare (ignore event))
                               (dolist (other *radio-buttons*)
                                 (when (and (not (eq other pane))
                                            (eq (pane-interface other) (pane-interface pane))
                                            (equal (radio-button-group other) (radio-button-group pane)))
                                   (setf (button-selected other) nil)))
                               (setf (button-selected pane) t)
                               (call-callback pane (button-callback pane) :data (button-text pane))))
          (:span (text-of (button-text pane))))))

(defmodel push-button-panel (choice)
  (;; :ROW or :COLUMN
   (orientation :initarg :orientation :initform :row :accessor button-panel-orientation))
  (:documentation "A push button for every item; the selection callback is called with
the item whose button was pressed."))

(defmethod render-pane ((pane push-button-panel))
  (html
    (:div :class (format nil "ui-button-panel ui-~(~A~)" (button-panel-orientation pane)) :role "group"
      (mapcar (lambda (item)
                (:button :type "button" :class "ui-push-button"
                         :disabled (not (pane-enabled pane))
                         :on-click (lambda (&optional event)
                                     (declare (ignore event))
                                     (call-callback pane (choice-selection-callback pane) :data item))
                  (print-item pane item)))
              (collection-items pane)))))

(defvar *text-input-choices* 0)

(defmodel text-input-choice (text-input-pane)
  (;; what the user can choose instead of typing
   (items :initarg :items :initform nil :accessor collection-items)
   (id :initform (incf *text-input-choices*) :reader text-input-choice-id))
  (:documentation "A text input pane with a list of texts to choose from."))

(defmethod render-pane ((pane text-input-choice))
  (let ((input (call-next-method))
        (id (format nil "ui-choices-~D" (text-input-choice-id pane))))
    (setf (getf (element-props input) :list) id)
    (html (:span :class "ui-text-input-choice"
            input
            (:datalist :id id
              (mapcar (lambda (item) (:option :value (text-of item))) (collection-items pane)))))))

(defmodel browser-pane (pane)
  (;; the address of the page
   (url :initarg :url :initform "about:blank" :accessor browser-pane-url))
  (:default-initargs :stretch t)
  (:documentation "A pane that shows the page at URL."))

(defmethod render-pane ((pane browser-pane))
  (html (:iframe :class "ui-browser-pane" :src (browser-pane-url pane))))

(defmodel image-pane (pane)
  (;; the address of the image
   (url :initarg :url :initform nil :accessor image-pane-url)
   ;; what the image shows, for those who do not see it
   (description :initarg :description :initform "" :accessor image-pane-description))
  (:documentation "A pane that shows the image at URL."))

(defmethod render-pane ((pane image-pane))
  (html (:img :class "ui-image-pane" :src (image-pane-url pane) :alt (image-pane-description pane))))

(defmodel color-pane (pane callbacks)
  (;; like "#3366cc"
   (color :initarg :color :initform "#000000" :accessor color-pane-color)
   ;; called with the colour when the user changes it
   (callback :initarg :callback :initform nil :accessor color-pane-callback))
  (:documentation "A colour that the user chooses."))

(defmethod render-pane ((pane color-pane))
  (html (:input :type "color" :class "ui-color-pane" :value (color-pane-color pane)
                :disabled (not (pane-enabled pane))
                :on-input (lambda (&optional event)
                            (let ((color (event-target-get *backend* event "value")))
                              (setf (color-pane-color pane) color)
                              (call-callback pane (color-pane-callback pane) :data color))))))

(defmodel simple-layout (layout) ()
  (:documentation "A layout of one pane (with :TITLE and :TITLE-POSITION :FRAME, a box around it)."))

(defmethod render-pane ((pane simple-layout))
  (html (:div :class (layout-class pane "ui-simple-layout")
          (let ((child (first (layout-description pane))))
            (and child (child-node pane child "flex:1 1 0"))))))

(defmodel pinboard-layout (layout) ()
  (:documentation "Its panes are where they say: an element of the description is
(pane x y) or (pane x y width height), in pixels."))

(defmethod render-pane ((pane pinboard-layout))
  (html
    (:div :class "ui-pinboard-layout"
      (mapcar (lambda (item)
                (destructuring-bind (child x y &optional width height) (if (consp item) item (list item 0 0))
                  (child-node pane child
                              (format nil "position:absolute;left:~Dpx;top:~Dpx~@[;width:~Dpx~]~@[;height:~Dpx~]"
                                      x y width height))))
              (layout-description pane)))))

(defmodel docking-layout (layout)
  (;; :COLUMN (the panes one below the other) or :ROW
   (orientation :initarg :orientation :initform :column :accessor docking-layout-orientation)
   ;; (item . interface) of the panes that are in windows of their own
   (undocked :initform nil :accessor docking-layout-undocked))
  (:documentation "Panes with a title bar each — an element of the description is
(title pane) — that the user takes out into a window of its own (and back by
closing that window)."))

(defun undock (layout item)
  (let* ((pane (resolve-child layout (second item)))
         (interface (make-instance 'interface
                                   :title (first item) :layout pane
                                   :destroy-callback
                                   (lambda (interface)
                                     (declare (ignore interface))
                                     (setf (docking-layout-undocked layout)
                                           (remove item (docking-layout-undocked layout) :key #'car))))))
    (setf (docking-layout-undocked layout) (acons item interface (docking-layout-undocked layout)))
    (display interface :window t)))

(defmethod render-pane ((pane docking-layout))
  (html
    (:div :class (format nil "ui-docking-layout ui-~(~A~)" (docking-layout-orientation pane))
      (mapcar (lambda (item)
                (unless (assoc item (docking-layout-undocked pane))
                  (:section :class "ui-dock"
                    (:header :class "ui-dock-title"
                      (:span (first item))
                      (:button :type "button" :class "ui-window-button" :aria-label "undock"
                               :on-click (lambda (&optional event)
                                           (declare (ignore event))
                                           (undock pane item))
                        "⇱"))
                    (child-node pane (second item) "flex:1 1 0"))))
              (layout-description pane)))))

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
   (title-reaction :initform nil :accessor interface-title-reaction)
   ;; called with the interface when it is taken off the screen
   (destroy-callback :initarg :destroy-callback :initform nil :accessor interface-destroy-callback)
   ;; true to show, below the layout, what the pointer can do where it is
   (pointer-documentation :initarg :pointer-documentation :initform nil
                          :accessor interface-pointer-documentation))
  (:default-initargs :stretch t)
  (:documentation "A window: panes in a layout, with a title.  See DEFINE-INTERFACE."))

(defun interface-title (interface)
  "The title of INTERFACE: of the window of the page, or of its window or dialog."
  (pane-title interface))
(defun (setf interface-title) (title interface)
  (setf (pane-title interface) title))

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
          (menu-bar-view interface))
        (and layout (child-node interface layout "flex:1 1 0"))
        (when (interface-pointer-documentation interface) (pointer-documentation-view))))))

(defmacro define-interface (name superclasses slots &rest options)
  "Define the class NAME of interfaces, like DEFMODEL with the options
  (:panes (name class initarg...)...)
  (:layouts (name class description initarg...)...)
  (:menus (name title (item...) initarg...)...)
  (:menu-bar menu-name...)
An item of a menu is (\"title\" initarg...) — a MENU-ENTRY with :CALLBACK,
:KEY, :ENABLED, :CHECKED... —, the name of another menu (which opens beside
it) or of a command, (command argument...) or :SEPARATOR.  The menu bar
shows the named menus, and the menus of the commands where it says :COMMANDS.
Every instance has these panes and layouts, made with MAKE-INSTANCE from the
initargs (:ACCESSOR and :READER name functions for the slot instead) and
kept in the slots NAME.  The initarg forms are evaluated when the instance
is made, with INTERFACE bound to it and its slots, also the panes made so
far, as variables.  The interface shows the first layout (or the one named
by the initarg :LAYOUT); without layouts, its panes in a column."
  (let* ((panes (cdr (assoc :panes options)))
         (layouts (cdr (assoc :layouts options)))
         (menus (cdr (assoc :menus options)))
         (menu-bar (assoc :menu-bar options))
         (others (remove-if (lambda (option) (member (car option) '(:panes :layouts :menus :menu-bar)))
                            options))
         (interface 'interface)
         (slot-names (append (mapcar (lambda (slot) (if (consp slot) (car slot) slot)) slots)
                             (mapcar #'car panes) (mapcar #'car layouts) (mapcar #'car menus))))
    (dolist (pane (append panes layouts menus))
      (when (member (car pane) '(name interface title title-position enabled visible help class
                                 width height min-width min-height max-width max-height stretch
                                 layout menu-bar command-table root title-reaction
                                 pointer-documentation destroy-callback))
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
                      layouts)
            ,@(mapcar (lambda (menu) `(,(car menu) :initform nil)) menus))
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
                       layouts)
             ;; (name title (item...) initarg...): an item is ("title" initarg...),
             ;; the name of a menu or of a command, (command argument...) or :SEPARATOR
             ,@(mapcar (lambda (menu)
                         (destructuring-bind (menu-name title items &rest initargs) menu
                           `(setf ,menu-name
                                  (make-instance
                                   'menu :name ',menu-name :interface ,interface :title ,title
                                   :items (list ,@(mapcar (lambda (item)
                                                            (if (and (consp item) (stringp (car item)))
                                                                `(make-instance 'menu-entry
                                                                                :interface ,interface
                                                                                :title ,(car item)
                                                                                ,@(cdr item))
                                                                `',item))
                                                          items))
                                   ,@initargs))))
                       menus))
           ,@(when menu-bar
               `((unless (interface-menu-bar ,interface)
                   (setf (interface-menu-bar ,interface) ',(cdr menu-bar)))))
           (unless (interface-layout ,interface)
             (setf (interface-layout ,interface)
                   ,(if layouts
                        `',(car (first layouts))
                        `(make-instance 'column-layout :interface ,interface
                                                       :description ',(mapcar #'car panes))))))
         ',name))))

;;; a window: an interface above the application that the user moves, resizes
;;; and closes

(defmodel window ()
  (;; the interface it shows
   (interface :initarg :interface :reader window-interface)
   ;; the position of its left side, in pixels from the left of the page
   (x :initarg :x :accessor window-x)
   ;; the position of its top, in pixels from the top of the page
   (y :initarg :y :accessor window-y)
   ;; its width in pixels
   (width :initarg :width :accessor window-width)
   ;; its height in pixels
   (height :initarg :height :accessor window-height)
   ;; true while it covers the page
   (maximized :initform nil :accessor window-maximized)
   ;; the window with the largest number is on top
   (z :initarg :z :accessor window-z)
   ;; (kind pointer-x pointer-y x y width height) while it is moved or resized
   (drag :initform nil :accessor window-drag))
  (:documentation "A window of an interface above the application (DISPLAY with :WINDOW T);
see INTERFACE-WINDOW."))

(defvar *top-z* 0)

(defun interface-window (interface)
  "The window of INTERFACE (see the argument :WINDOW of DISPLAY), or NIL."
  (find (pane-interface interface) *windows* :key #'window-interface))

(defun raise-interface (interface)
  "Put the window of INTERFACE on top of the others."
  (let ((window (interface-window interface)))
    (when (and window (< (window-z window) *top-z*))
      (setf (window-z window) (incf *top-z*)))
    interface))

(defun window-pointer (window kind event)
  "The pointer was pressed to move (KIND :MOVE) or resize WINDOW."
  (when event
    (stop-event *backend* event)
    (capture-pointer *backend* event))
  (setf (window-drag window)
        (list kind
              (or (event-get *backend* event "clientX") 0) (or (event-get *backend* event "clientY") 0)
              (window-x window) (window-y window) (window-width window) (window-height window))))

(defun window-pointer-moved (window event)
  (let ((drag (window-drag window)))
    (when (and drag (not (window-maximized window)))
      (destructuring-bind (kind px py x y width height) drag
        (let ((dx (round (- (or (event-get *backend* event "clientX") px) px)))
              (dy (round (- (or (event-get *backend* event "clientY") py) py))))
          (ecase kind
            (:move (setf (window-x window) (+ x dx)
                         (window-y window) (max 0 (+ y dy))))
            (:resize (setf (window-width window) (max 180 (+ width dx))
                           (window-height window) (max 90 (+ height dy))))))))))

(defcomponent window-view (window)
  (let* ((interface (window-interface window))
         (top (>= (window-z window) (reduce #'max *windows* :key #'window-z)))
         (maximized (window-maximized window)))
    (flet ((released (&optional event)
             (declare (ignore event))
             (setf (window-drag window) nil)))
      (:div :class (format nil "ui-top-window~:[~; ui-active~]~:[~; ui-maximized~]" top maximized)
            :role "dialog" :aria-label (pane-title interface)
            :style (format nil "z-index:~D;~:[left:~Dpx;top:~Dpx;width:~Dpx;height:~Dpx~;~]"
                           ;; above the application, in their order, below menus
                           (+ 100 (count-if (lambda (other) (< (window-z other) (window-z window)))
                                            *windows*))
                           maximized
                           (window-x window) (window-y window)
                           (window-width window) (window-height window))
            :on-pointerdown (lambda (&optional event)
                              (declare (ignore event))
                              (raise-interface interface))
        (:header :class "ui-window-title"
                 :on-pointerdown (lambda (&optional event)
                                   (raise-interface interface)
                                   (window-pointer window :move event))
                 :on-pointermove (lambda (&optional event) (window-pointer-moved window event))
                 :on-pointerup #'released
                 :on-pointercancel #'released
                 :on-dblclick (lambda (&optional event)
                                (declare (ignore event))
                                (setf (window-maximized window) (not maximized)))
          (:span :class "ui-window-name" (or (pane-title interface) ""))
          (:button :type "button" :class "ui-window-button" :aria-label "maximize"
                   :on-pointerdown (lambda (&optional event) (when event (stop-event *backend* event)))
                   :on-click (lambda (&optional event)
                               (declare (ignore event))
                               (setf (window-maximized window) (not maximized)))
            (if maximized "❐" "□"))
          (:button :type "button" :class "ui-window-button ui-window-close" :aria-label *abort-label*
                   :on-pointerdown (lambda (&optional event) (when event (stop-event *backend* event)))
                   :on-click (lambda (&optional event)
                               (declare (ignore event))
                               (quit-interface interface))
            "×"))
        (:div :class "ui-window-body" (pane-node interface "flex:1 1 0"))
        (unless maximized
          (:div :class "ui-window-resize"
                :on-pointerdown (lambda (&optional event) (window-pointer window :resize event))
                :on-pointermove (lambda (&optional event) (window-pointer-moved window event))
                :on-pointerup #'released
                :on-pointercancel #'released))))))

(defun arrange-windows (function)
  "Call FUNCTION with every window, its index, the number of windows and the
size of the screen."
  (multiple-value-bind (width height) (viewport-size *backend*)
    (let ((windows (sort (copy-list *windows*) #'< :key #'window-z)))
      (loop for window in windows
            for index from 0
            do (setf (window-maximized window) nil)
               (funcall function window index (length windows) width height)))))

(defun cascade-windows ()
  "Put the windows one behind the other, each a little further right and down."
  (arrange-windows (lambda (window index count width height)
                     (declare (ignore count))
                     (setf (window-x window) (+ 40 (* 28 index))
                           (window-y window) (+ 40 (* 28 index))
                           (window-width window) (max 240 (round (* width 0.5)))
                           (window-height window) (max 160 (round (* height 0.5)))))))

(defun tile-windows ()
  "Put the windows side by side so that they cover the screen."
  (arrange-windows (lambda (window index count width height)
                     (let* ((columns (ceiling (sqrt count)))
                            (rows (ceiling count columns))
                            (w (floor width columns))
                            (h (floor height rows)))
                       (setf (window-x window) (* w (mod index columns))
                             (window-y window) (* h (floor index columns))
                             (window-width window) w
                             (window-height window) h)))))

(defvar *command-table-users* nil "The interfaces on the screen that have a command table.")

(defun use-command-table (interface use)
  (when (interface-command-table interface)
    (setf *command-table-users* (remove interface *command-table-users*))
    (when use (push interface *command-table-users*))
    (setf *command-tables*
          (remove-duplicates (mapcar #'interface-command-table *command-table-users*)))))

(defun display (interface &key (container "#app") window x y (width 480) (height 320))
  "Show INTERFACE in CONTAINER (for the DOM, a CSS selector or a node); the
title of the window is its title.  With WINDOW true, show it instead in a
window of its own above the application, at X, Y with the size WIDTH,
HEIGHT: the user moves it by its title, resizes it at its corner, maximizes
and closes it (see also RAISE-INTERFACE, TILE-WINDOWS, CASCADE-WINDOWS).
Return the interface."
  (let ((backend *backend*))
    (use-command-table interface t)
    (setf *interfaces* (cons interface (remove interface *interfaces*)))
    (cond (window
           (ensure-overlay)
           (let ((offset (* 28 (mod (length *windows*) 8))))
             (setf *windows*
                   (append *windows*
                           (list (make-instance 'window :interface interface
                                                        :x (or x (+ 80 offset)) :y (or y (+ 60 offset))
                                                        :width width :height height
                                                        :z (incf *top-z*)))))))
          (t
           (setf (interface-root interface) (mount (pane-node interface) container)
                 (interface-title-reaction interface)
                 (autorun (lambda ()
                            (let ((title (pane-title interface)))
                              (when title (set-title backend title))))))))
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
    (flet ((gone ()
             (use-command-table interface nil)
             (setf *interfaces* (remove interface *interfaces*))
             (when (interface-destroy-callback interface)
               (funcall (interface-destroy-callback interface) interface))))
      (cond ((interface-root interface)
             (unmount (interface-root interface))
             (stop (interface-title-reaction interface))
             (setf (interface-root interface) nil)
             (gone))
            ((interface-window interface)
             (setf *windows* (remove (interface-window interface) *windows*))
             (gone))
          ;; dialog.lisp
            (t (let ((dialog (find interface *dialogs* :key #'dialog-interface)))
                 (when dialog (abort-dialog dialog))))))
    nil))
