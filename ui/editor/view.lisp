;;;; view.lisp — the editor on the page: the pane CODE-EDITOR
;;;;
;;;;   (make-instance 'editor:code-editor :text "(defun f ())" :language "lisp"
;;;;                                       :keymap :emacs :change-callback #'changed)
;;;;
;;;; is a pane of the UI framework (ui/): put it in an interface, a layout,
;;;; or markup with PANE-NODE.  Everything is Lisp: the page is reached
;;;; through the DOM's own methods (JS:CALL, JS:GET), and no JavaScript is
;;;; written here.
;;;;
;;;; How it is shown.  The lines are components (EDITOR-LINE) of the UI
;;;; framework, and only the lines in view are on the page (with a few more
;;;; above and below): an element as high as the whole text gives the
;;;; scroller its height, and the lines in view are moved into place.  A line
;;;; is rendered again only when its arguments change (its text, tokens, the
;;;; part of the selection on it, the cursor), so a key typed changes a line.
;;;; Rows have a fixed height (LINE-HEIGHT): a position on the page is a
;;;; row by division.  When the lines are wrapped (:WRAP T), a line is cut
;;;; into rows here (WRAP-STRING of commands.lisp, from the width of a
;;;; character), not by the browser, so that the height of every line is
;;;; known without asking the page.  The column under the mouse is found by the browser
;;;; (caretPositionFromPoint), which knows the widths of the characters, wide
;;;; ones included.
;;;;
;;;; How it is typed into.  The keys go to a textarea that stays at the
;;;; cursor (so that an input method shows its window there): a key with a
;;;; modifier, or without a character, is looked up in the keymap on its
;;;; keydown event; the characters come from the input events of the
;;;; textarea, and a composed text when the composition ends.  Copy, cut and
;;;; paste are the textarea's clipboard events.

(in-package "EDITOR")

(export '(code-editor code-editor-editor code-editor-text code-editor-language
          code-editor-keymap code-editor-read-only code-editor-line-numbers
          code-editor-change-callback code-editor-cursor-callback code-editor-save-callback
          code-editor-line-height code-editor-font-size code-editor-focused
          code-editor-version code-editor-message code-editor-cursor code-editor-focus
          code-editor-run code-editor-select code-editor-reveal code-editor-find
          code-editor-wrap code-editor-cursor-rect))

(ui:defmodel code-editor (ui:pane)
  (;; the model (buffer.lisp); it is not observed: VERSION is
   (editor :initform nil :accessor code-editor-editor)
   ;; incremented whenever the text, the selection or the options change
   (version :initform 0 :accessor code-editor-version)
   ;; the keymap: :STANDARD, :EMACS, or a keymap
   (keymap :initarg :keymap :initform :standard :accessor %code-editor-keymap)
   (line-numbers :initarg :line-numbers :initform t :accessor code-editor-line-numbers)
   ;; in pixels: every line is LINE-HEIGHT high
   (line-height :initarg :line-height :initform 20 :accessor code-editor-line-height)
   (font-size :initarg :font-size :initform 14 :accessor code-editor-font-size)
   ;; true: long lines are wrapped to the width of the pane
   (wrap :initarg :wrap :initform nil :accessor %code-editor-wrap)
   ;; the cells a row of text has on the page (when wrapped)
   (columns :initform nil :accessor wrap-columns)
   ;; called with the pane after the user changed the text
   (change-callback :initarg :change-callback :initform nil :accessor code-editor-change-callback)
   ;; called with the pane after the cursor or the selection moved
   (cursor-callback :initarg :cursor-callback :initform nil :accessor code-editor-cursor-callback)
   ;; called with the pane by the command SAVE (Mod-S, C-x C-s)
   (save-callback :initarg :save-callback :initform nil :accessor code-editor-save-callback)
   ;; what the page shows
   (scroll-top :initform 0 :accessor scroll-top)
   (viewport-height :initform 600 :accessor viewport-height)
   (focused :initform nil :accessor code-editor-focused)
   (composing :initform nil :accessor composing))
  (:default-initargs :stretch t :class "ce-pane")
  (:documentation "A code editor: the text of an EDITOR (the model of buffer.lisp) with
highlighting, line numbers, the selection, undo, and the commands and keys
of commands.lisp."))

;;; the parts of the page, and what is not shown (not observed)
(defclass code-editor-dom ()
  ((root :initform nil :accessor dom-root)
   (scroller :initform nil :accessor dom-scroller)
   (input :initform nil :accessor dom-input)
   (resize-observer :initform nil :accessor dom-resize-observer)
   ;; :CHAR, :WORD or :LINE while the mouse selects
   (drag :initform nil :accessor dom-drag)
   (drag-origin :initform nil :accessor dom-drag-origin)
   ;; (time x y count) of the last press, to count double and triple clicks
   ;; (the detail of pointer events is not the count in every browser)
   (last-press :initform nil :accessor dom-last-press)
   (reveal :initform nil :accessor dom-reveal)
   (frame-requested :initform nil :accessor dom-frame-requested)
   ;; the keyboard was asked for before the pane was on the page
   (focus-pending :initform nil :accessor dom-focus-pending)
   ;; the width of a character in pixels
   (char-width :initform nil :accessor dom-char-width)
   ;; (version width . vector): the first row of every line, and the rows in all
   (row-offsets :initform nil :accessor dom-row-offsets)
   (text-version :initform -1 :accessor dom-text-version)))

(defvar *doms* (make-hash-table :test 'eq) "code-editor -> code-editor-dom")
(defun dom (pane)
  (or (gethash pane *doms*) (setf (gethash pane *doms*) (make-instance 'code-editor-dom))))

(defun resolve-keymap (keymap)
  (case keymap
    (:emacs *emacs-keymap*)
    ((:standard nil) *standard-keymap*)
    (t keymap)))

(defun resolve-language (language)
  (cond ((null language) *plain-text*)
        ((typep language 'language) language)
        (t (find-language (string language)))))

(defmethod initialize-instance :after ((pane code-editor) &key (text "") language read-only
                                                              (tab-width 2) (auto-pair t))
  (let ((editor (make-editor :text text :language (resolve-language language)
                             :keymap (resolve-keymap (%code-editor-keymap pane))
                             :read-only read-only :tab-width tab-width :auto-pair auto-pair)))
    (setf (code-editor-editor pane) editor)
    (setf (editor-property editor :save)
          (lambda (editor)
            (declare (ignore editor))
            (let ((save (code-editor-save-callback pane)))
              (if save (funcall save pane) (setf (editor-message editor) "No place to save to")))))
    (setf (editor-property editor :clipboard-write)
          (lambda (text) (ignore-errors (ui:write-clipboard ui:*backend* text))))))

;;; ------------------------------------------------------------------
;;; The interface for programs

(defun touch (pane)
  "The model of PANE changed: show it, and tell the callbacks."
  (let* ((editor (code-editor-editor pane))
         (dom (dom pane))
         (text-changed (/= (dom-text-version dom) (length (undo-stack editor)))))
    (setf (dom-text-version dom) (length (undo-stack editor)))
    (incf (code-editor-version pane))
    (when (and text-changed (code-editor-change-callback pane))
      (funcall (code-editor-change-callback pane) pane))
    (when (code-editor-cursor-callback pane)
      (funcall (code-editor-cursor-callback pane) pane))))

(defun code-editor-text (pane)
  "The text of the editor PANE."
  (code-editor-version pane)
  (editor-text (code-editor-editor pane)))

(defun (setf code-editor-text) (text pane)
  (setf (editor-text (code-editor-editor pane)) text)
  (setf (scroll-top pane) 0)
  (let ((scroller (dom-scroller (dom pane))))
    (when scroller (js:set scroller "scrollTop" 0)))
  (touch pane)
  text)

(defun code-editor-language (pane)
  (code-editor-version pane)
  (editor-language (code-editor-editor pane)))

(defun (setf code-editor-language) (language pane)
  (let ((editor (code-editor-editor pane)))
    (setf (editor-language editor) (resolve-language language))
    (clrhash (token-cache editor))
    (syntax-changed editor 0)
    (incf (code-editor-version pane))
    language))

(defun code-editor-keymap (pane) (%code-editor-keymap pane))
(defun (setf code-editor-keymap) (keymap pane)
  (setf (editor-keymap (code-editor-editor pane)) (resolve-keymap keymap))
  (setf (%code-editor-keymap pane) keymap))

(defun code-editor-read-only (pane)
  (code-editor-version pane)
  (editor-read-only (code-editor-editor pane)))
(defun (setf code-editor-read-only) (value pane)
  (setf (editor-read-only (code-editor-editor pane)) value)
  (incf (code-editor-version pane))
  value)

(defun code-editor-wrap (pane) (%code-editor-wrap pane))
(defun (setf code-editor-wrap) (value pane)
  (setf (%code-editor-wrap pane) value)
  (sync-wrap pane)
  (after-command pane)
  value)

(defun code-editor-message (pane)
  "The message of the last command (\"Mark set\", \"C-x-\"...), or NIL."
  (code-editor-version pane)
  (editor-message (code-editor-editor pane)))

(defun code-editor-cursor (pane)
  "The line and the column of the cursor (from 0)."
  (code-editor-version pane)
  (let ((head (editor-head (code-editor-editor pane))))
    (values (pos-line head) (pos-col head))))

(defun code-editor-run (pane command)
  "Run the editor command COMMAND (a symbol of commands.lisp, or a function
of the editor) on PANE."
  (let ((editor (code-editor-editor pane)))
    (if (functionp command) (funcall command editor) (run-command editor command))
    (after-command pane)))

(defun code-editor-select (pane start-line start-col &optional (end-line start-line) (end-col start-col))
  "Select from the start to the end (positions from 0), and show it."
  (set-selection (code-editor-editor pane) (make-pos start-line start-col) (make-pos end-line end-col))
  (after-command pane))

(defun code-editor-find (pane string &key backward)
  "Select the next (or previous) STRING; true if found."
  (let ((editor (code-editor-editor pane)))
    (multiple-value-bind (start end)
        (if backward (search-backward editor string) (search-forward editor string))
      (when start
        (setf (editor-mark-active editor) nil)
        (set-selection editor start end)
        (after-command pane)
        t))))

(defun code-editor-cursor-rect (pane)
  "Where the cursor is on the page: its left, top and bottom (in the
coordinates of the window), or NIL if it is not shown."
  (let* ((root (dom-root (dom pane)))
         (cursor (and root (js:call root "querySelector" ".ce-cursor"))))
    (when (and cursor (js:js-value-p cursor))
      (let ((rect (js:call cursor "getBoundingClientRect")))
        (values (js:get rect "left") (js:get rect "top") (js:get rect "bottom"))))))

(defun code-editor-focus (pane)
  "Give the keyboard to PANE (when it is put on the page, if it is not yet)."
  (let ((input (dom-input (dom pane))))
    (if input
        (js:call input "focus" (js:object "preventScroll" t))
        (setf (dom-focus-pending (dom pane)) t))))

;;; ------------------------------------------------------------------
;;; Scrolling

(defun line-count* (pane) (line-count (editor-document (code-editor-editor pane))))

(defun sync-wrap (pane)
  "Tell the model how wide a row is (NIL: lines are not wrapped)."
  (setf (editor-property (code-editor-editor pane) :wrap-width)
        (and (%code-editor-wrap pane) (wrap-columns pane))))

(defun row-offsets (pane)
  "When the lines are wrapped, a vector of the first row of every line (and
the number of rows after them); NIL when they are not."
  (let ((editor (code-editor-editor pane)))
    (when (editor-property editor :wrap-width)
      (let* ((dom (dom pane))
             (width (editor-property editor :wrap-width))
             (cache (dom-row-offsets dom)))
        (if (and cache (eql (first cache) (editor-version editor)) (eql (second cache) width))
            (cddr cache)
            (let* ((count (line-count* pane))
                   (offsets (make-array (1+ count)))
                   (row 0))
              (dotimes (line count)
                (setf (aref offsets line) row)
                (incf row (1+ (length (line-rows editor line)))))
              (setf (aref offsets count) row)
              (setf (dom-row-offsets dom) (list* (editor-version editor) width offsets))
              offsets))))))

(defun line-at-row (offsets row)
  "The line that has the row ROW (OFFSETS of ROW-OFFSETS)."
  (let ((low 0) (high (1- (length offsets))))
    ;; the last line whose first row is not after ROW
    (loop while (> (- high low) 1)
          do (let ((mid (floor (+ low high) 2)))
               (if (<= (aref offsets mid) row) (setf low mid) (setf high mid))))
    low))

(defun line-top (pane line &optional (offsets (row-offsets pane)))
  "The first row of LINE."
  (if offsets (aref offsets line) line))

(defun visible-range (pane)
  "The first line and the line after the last that are on the page."
  (let* ((lh (code-editor-line-height pane))
         (first (floor (scroll-top pane) lh))
         (rows (ceiling (viewport-height pane) lh))
         (overscan 8)
         (offsets (row-offsets pane))
         (count (line-count* pane)))
    (if offsets
        (values (max 0 (- (line-at-row offsets first) overscan))
                (min count (+ (line-at-row offsets (+ first rows)) 1 overscan)))
        (values (max 0 (- first overscan))
                (min count (+ first rows overscan))))))

(defun code-editor-reveal (pane)
  "Scroll so that the cursor is in view."
  (let* ((scroller (dom-scroller (dom pane)))
         (editor (code-editor-editor pane))
         (lh (code-editor-line-height pane))
         (head (editor-head editor))
         (offsets (row-offsets pane))
         (row (if offsets
                  (+ (aref offsets (pos-line head)) (row-of (line-rows editor (pos-line head)) (pos-col head)))
                  (pos-line head)))
         (top (* row lh))
         (height (viewport-height pane))
         (scroll (scroll-top pane))
         (margin (min (* 2 lh) (floor height 4))))
    (setf (dom-reveal (dom pane)) t)
    (let ((new (cond ((< top (+ scroll margin)) (max 0 (- top margin)))
                     ((> (+ top lh) (- (+ scroll height) margin)) (+ (- top height) lh margin))
                     (t scroll))))
      (when (/= new scroll)
        (setf (scroll-top pane) new)
        (when scroller (js:set scroller "scrollTop" new))))))

(defun measure (pane)
  "Find how many cells a row has on the page (when the lines are wrapped)."
  (let* ((dom (dom pane))
         (root (dom-root dom))
         (scroller (dom-scroller dom)))
    (when (and root (%code-editor-wrap pane))
      (unless (dom-char-width dom)
        (let ((probe (js:call root "querySelector" ".ce-measure")))
          (when (js:js-value-p probe)
            (let ((width (js:get (js:call probe "getBoundingClientRect") "width")))
              (when (and (numberp width) (plusp width))
                (setf (dom-char-width dom) (/ width 10)))))))
      (let* ((ch (dom-char-width dom))
             (gutter (js:call root "querySelector" ".ce-gutter"))
             (gutter-width (if (js:js-value-p gutter) (js:get gutter "offsetWidth") 0))
             (width (js:get scroller "clientWidth")))
        (when (and ch (numberp width) (plusp width))
          ;; the text has 1ch before it, and 2ch and the cursor after it
          (let ((columns (max 8 (- (floor (- width gutter-width) ch) 4))))
            (unless (eql columns (wrap-columns pane))
              (setf (wrap-columns pane) columns)
              (sync-wrap pane)
              (code-editor-reveal pane)
              (request-frame pane))))))))

(defun after-command (pane)
  (setf (editor-property (code-editor-editor pane) :page-lines)
        (max 1 (floor (viewport-height pane) (code-editor-line-height pane))))
  (code-editor-reveal pane)
  (touch pane)
  (request-frame pane))

(defun request-frame (pane)
  "After the page shows the new state, put the textarea at the cursor (and
bring the cursor into view sideways)."
  (let ((dom (dom pane)))
    (unless (dom-frame-requested dom)
      (setf (dom-frame-requested dom) t)
      (js:call (js:global) "requestAnimationFrame"
               (lambda (&rest args)
                 (declare (ignore args))
                 (setf (dom-frame-requested dom) nil)
                 (ignore-errors (measure pane))
                 (ignore-errors (place-input pane))
                 nil)))))

(defun place-input (pane)
  (let* ((dom (dom pane))
         (root (dom-root dom))
         (input (dom-input dom))
         (cursor (and root (js:call root "querySelector" ".ce-cursor"))))
    (when (and input cursor (js:js-value-p cursor))
      (let* ((sizer (js:get input "parentNode"))
             (c (js:call cursor "getBoundingClientRect"))
             (s (js:call sizer "getBoundingClientRect"))
             (style (js:get input "style")))
        (js:set style "left" (format nil "~Dpx" (round (- (js:get c "left") (js:get s "left")))))
        (js:set style "top" (format nil "~Dpx" (round (- (js:get c "top") (js:get s "top")))))
        (when (dom-reveal dom)
          (setf (dom-reveal dom) nil)
          (js:call cursor "scrollIntoView" (js:object "block" "nearest" "inline" "nearest")))))))

;;; ------------------------------------------------------------------
;;; Rendering

(defun token-class (class)
  (with-output-to-string (out)
    (let ((start 0))
      (loop
        (let ((space (position #\Space class :start start)))
          (format out "~:[ ~;~]ce-~A" (zerop start) (subseq class start space))
          (if space (setf start (1+ space)) (return)))))))

(defun line-segments (text tokens selection cursor brackets &optional (from 0) (to (length text)) (last t))
  "The spans of a line (or of its row from FROM to TO; LAST: the row ends the
line): runs of TEXT with the classes of their tokens, the selection (start
end) and the brackets, and the cursor at its column."
  (let ((cuts (list from to)))
    (dolist (token tokens) (push (first token) cuts) (push (second token) cuts))
    (when selection (push (first selection) cuts) (push (min (length text) (second selection)) cuts))
    (when cursor (push cursor cuts))
    (dolist (b brackets) (push b cuts) (push (1+ b) cuts))
    (setf cuts (sort (remove-duplicates (remove-if-not (lambda (c) (<= from c to)) cuts)) #'<))
    (let ((nodes '()))
      (loop for (start end) on cuts
            while end
            do (when (eql cursor start) (push (ui:html (:span :class "ce-cursor")) nodes))
               (let* ((token (token-at tokens start))
                      (class (format nil "~@[~A~]~:[~; ce-selected~]~:[~; ce-matching~]"
                                     (and token (token-class (third token)))
                                     (and selection (<= (first selection) start)
                                          (< start (second selection)))
                                     (member start brackets))))
                 (push (ui:html (:span :class (if (string= class "") nil (string-left-trim " " class))
                                  (subseq text start end)))
                       nodes)))
      (when (and last (eql cursor to)) (push (ui:html (:span :class "ce-cursor")) nodes))
      ;; the newline at the end of the line is selected
      (when (and last selection (third selection))
        (push (ui:html (:span :class "ce-selected ce-eol" " ")) nodes))
      (nreverse nodes))))

(defun row-nodes (text tokens selection cursor brackets rows)
  "The rows of a wrapped line: ROWS are the columns they start at after the first."
  (loop for (from to) on (cons 0 (append rows (list (length text))))
        while to
        collect (let ((last (= to (length text))))
                  (ui:html (:div :class "ce-row"
                             (line-segments text tokens selection cursor brackets from to last))))))

(ui:defcomponent editor-line (number text tokens selection cursor brackets current gutter rows)
  (:div :class (if current "ce-line ce-current" "ce-line") :data-line number
    (when gutter (:span :class "ce-gutter" (1+ number)))
    (:span :class "ce-text"
      (if rows
          (row-nodes text tokens selection cursor brackets rows)
          (line-segments text tokens selection cursor brackets)))))

(defun line-selection (editor line)
  "The part of the selection on LINE: (start end newline-p), or NIL."
  (unless (selection-empty-p editor)
    (let ((start (selection-start editor)) (end (selection-end editor))
          (len (line-length (editor-document editor) line)))
      (when (<= (pos-line start) line (pos-line end))
        (let ((from (if (= line (pos-line start)) (pos-col start) 0))
              (to (if (= line (pos-line end)) (pos-col end) len)))
          (list from to (< line (pos-line end))))))))

(defun on-mount (pane node)
  (let ((dom (dom pane)))
    (setf (dom-root dom) node
          (dom-scroller dom) (js:call node "querySelector" ".ce-scroller")
          (dom-input dom) (js:call node "querySelector" ".ce-input"))
    (let ((scroller (dom-scroller dom)))
      (setf (viewport-height pane) (max 1 (js:get scroller "clientHeight")))
      (let ((observer (js:new (js:global "ResizeObserver")
                              (lambda (&rest args)
                                (declare (ignore args))
                                (let ((height (js:get scroller "clientHeight")))
                                  (when (and (numberp height) (plusp height)
                                             (/= height (viewport-height pane)))
                                    (setf (viewport-height pane) height)))
                                (ignore-errors (measure pane))
                                nil))))
        (js:call observer "observe" scroller)
        (setf (dom-resize-observer dom) observer))
      (js:set scroller "scrollTop" (scroll-top pane)))
    (when (dom-focus-pending dom)
      (setf (dom-focus-pending dom) nil)
      (code-editor-focus pane))
    (request-frame pane)))

(defmethod ui:pane-unmounted ((pane code-editor))
  (let ((dom (dom pane)))
    (when (dom-resize-observer dom)
      (js:call (dom-resize-observer dom) "disconnect"))
    (remhash pane *doms*)))

(defmethod ui:render-pane ((pane code-editor))
  (code-editor-version pane)
  (let* ((editor (code-editor-editor pane))
         (doc (editor-document editor))
         (lh (code-editor-line-height pane))
         (count (line-count doc))
         (head (editor-head editor))
         (focused (code-editor-focused pane))
         (gutter (code-editor-line-numbers pane))
         (digits (max 2 (length (princ-to-string count))))
         (pair (and (selection-empty-p editor) (bracket-pair-near editor head)))
         (wrap (progn (sync-wrap pane) (editor-property editor :wrap-width)))
         (offsets (row-offsets pane))
         (total (if offsets (aref offsets count) count)))
    (multiple-value-bind (first last) (visible-range pane)
      (ui:html
        (:div :class (format nil "ce-editor~:[~; ce-focused~]~:[~; ce-read-only~]~:[ ce-no-gutter~;~]~:[~; ce-wrap~]"
                             focused (editor-read-only editor) gutter (%code-editor-wrap pane))
              :style (format nil "--ce-line-height:~Dpx;--ce-font-size:~Dpx;--ce-gutter-chars:~D;"
                             lh (code-editor-font-size pane) digits)
              :on-mount (lambda (node) (on-mount pane node))
          (:span :class "ce-measure" :aria-hidden "true" "0000000000")
          (:div :class "ce-scroller"
                :on-scroll (lambda (event) (on-scroll pane event))
                :on-pointerdown (lambda (event) (on-pointer-down pane event))
                :on-pointermove (lambda (event) (on-pointer-move pane event))
                :on-pointerup (lambda (event) (on-pointer-up pane event))
                :on-pointercancel (lambda (event) (on-pointer-up pane event))
            (:div :class "ce-sizer" :style (format nil "height:~Dpx;" (* total lh))
              (:div :class "ce-lines" :style (format nil "transform:translateY(~Dpx);" (* (line-top pane first offsets) lh))
                (loop for line from first below last
                      collect (ui:keyed line
                                        (editor-line line (line-string doc line) (line-tokens editor line)
                                                     (line-selection editor line)
                                                     (and (= line (pos-line head)) (pos-col head))
                                                     (loop for p in pair
                                                           when (and p (= (pos-line p) line))
                                                             collect (pos-col p))
                                                     (and focused (selection-empty-p editor)
                                                          (= line (pos-line head)))
                                                     gutter
                                                     (and wrap (line-rows editor line))))))
              (:textarea :class (if (composing pane) "ce-input ce-composing" "ce-input")
                         :autocomplete "off" :autocapitalize "off" :spellcheck "false"
                         :aria-label "editor" :rows 1 :wrap "off"
                         :readonly (and (editor-read-only editor) t)
                         :on-keydown (lambda (event) (on-key-down pane event))
                         :on-input (lambda (event) (on-input pane event))
                         :on-compositionstart (lambda (event)
                                                (declare (ignore event))
                                                (setf (composing pane) t))
                         :on-compositionend (lambda (event) (on-composition-end pane event))
                         :on-copy (lambda (event) (on-copy pane event nil))
                         :on-cut (lambda (event) (on-copy pane event t))
                         :on-paste (lambda (event) (on-paste pane event))
                         :on-focus (lambda (event)
                                     (declare (ignore event))
                                     (setf (code-editor-focused pane) t)
                                     (request-frame pane))
                         :on-blur (lambda (event)
                                    (declare (ignore event))
                                    (setf (code-editor-focused pane) nil))))))))))

(defun on-scroll (pane event)
  (let ((top (js:get (js:get event "currentTarget") "scrollTop")))
    (when (and (numberp top) (/= top (scroll-top pane)))
      (setf (scroll-top pane) top))))

;;; ------------------------------------------------------------------
;;; The keyboard

(defparameter *dom-key-names*
  '(("ArrowLeft" . "Left") ("ArrowRight" . "Right") ("ArrowUp" . "Up") ("ArrowDown" . "Down")
    ("Enter" . "Return") (" " . "Space") ("Esc" . "Escape")))

(defparameter *modifier-key-names*
  '("Shift" "Control" "Alt" "Meta" "CapsLock" "Hyper" "Super" "OS" "AltGraph" "Process"
    "Dead" "Unidentified" "Fn" "NumLock"))

(defun flag (event name) (eq (js:get event name) t))

(defun event-key-name (event)
  "The key of the keydown EVENT as a key of commands.lisp (\"C-x\", \"S-Left\"), or NIL."
  (let ((key (js:get event "key"))
        (code (js:get event "code"))
        (ctrl (flag event "ctrlKey")) (alt (flag event "altKey"))
        (meta (flag event "metaKey")) (shift (flag event "shiftKey")))
    (when (and (stringp key) (not (member key *modifier-key-names* :test #'string=)))
      (let ((name (or (cdr (assoc key *dom-key-names* :test #'string=)) key)))
        ;; Option on a Mac types another character: M-f is the key F
        (when (and alt (stringp code) (> (length code) 3)
                   (or (string= code "Key" :end1 3 :end2 3)
                       (and (> (length code) 5) (string= code "Digit" :end1 5 :end2 5))))
          (setf name (string-downcase (subseq code (if (char= (char code 0) #\K) 3 5)))))
        (when (= (length name) 1)
          (if (alpha-char-p (char name 0))
              (setf name (string-downcase name))
              ;; a character typed with Shift is that character: M-< not M-S-<
              (setf shift nil)))
        (format nil "~:[~;C-~]~:[~;M-~]~:[~;s-~]~:[~;S-~]~A" ctrl alt meta shift name)))))

(defun typed-character-p (event)
  "True if EVENT types a character (that comes as an input event)."
  (let ((key (js:get event "key")))
    (and (stringp key) (= (length key) 1)
         (not (flag event "ctrlKey")) (not (flag event "metaKey"))
         (or (not (flag event "altKey"))
             ;; Option-character on a Mac, unless the keymap has M- for it
             (and *mac-p* (not (lookup-key (editor-keymap* event) (event-key-name event))))))))

(defvar *key-pane* nil)
(defun editor-keymap* (event)
  (declare (ignore event))
  (if *key-pane* (or (editor-keymap (code-editor-editor *key-pane*)) *standard-keymap*) *standard-keymap*))

(defun on-key-down (pane event)
  (unless (or (composing pane) (flag event "isComposing")
              ;; the key that starts a composition
              (equal (js:get event "keyCode") 229))
    (let ((*key-pane* pane)
          (editor (code-editor-editor pane)))
      (unless (and (typed-character-p event) (null (editor-prefix editor)))
        (let ((key (event-key-name event)))
          (when key
            (if (handle-key editor key)
                (progn
                  (js:call event "preventDefault")
                  (js:call event "stopPropagation")
                  (after-command pane))
                ;; an unbound key after a prefix shows its message
                (when (editor-message editor) (touch pane)))))))))

(defun take-input (event)
  "The text in the textarea of EVENT, which is emptied."
  (let* ((target (js:get event "target"))
         (text (js:get target "value")))
    (js:set target "value" "")
    (and (stringp text) (plusp (length text)) text)))

(defun on-input (pane event)
  (unless (or (composing pane) (flag event "isComposing"))
    (let ((text (take-input event)))
      (when text
        (let ((editor (code-editor-editor pane)))
          (if (editor-prefix editor)
              ;; C-x h: the key after a prefix is a key
              (handle-key editor text)
              (type-text editor text)))
        (after-command pane)))))

(defun on-composition-end (pane event)
  (setf (composing pane) nil)
  (let ((text (js:get event "data")))
    (js:set (js:get event "target") "value" "")
    (when (and (stringp text) (plusp (length text)))
      (type-text (code-editor-editor pane) text)
      (after-command pane))))

(defun on-copy (pane event cut)
  (let* ((editor (code-editor-editor pane))
         (data (js:get event "clipboardData")))
    (unless (selection-empty-p editor)
      (js:call event "preventDefault")
      (when (js:js-value-p data)
        (js:call data "setData" "text/plain" (selected-text editor)))
      (when (and cut (not (editor-read-only editor)))
        (with-undo-group (editor) (replace-selection editor ""))
        (after-command pane)))))

(defun on-paste (pane event)
  (js:call event "preventDefault")
  (let* ((data (js:get event "clipboardData"))
         (text (and (js:js-value-p data) (js:call data "getData" "text/plain")))
         (editor (code-editor-editor pane)))
    (when (and (stringp text) (plusp (length text)) (not (editor-read-only editor)))
      (with-undo-group (editor) (replace-selection editor text))
      (after-command pane))))

;;; ------------------------------------------------------------------
;;; The mouse

(defun event-pos (pane event)
  "The position in the text under the mouse EVENT."
  (let* ((dom (dom pane))
         (editor (code-editor-editor pane))
         (doc (editor-document editor))
         (lh (code-editor-line-height pane))
         (sizer (js:call (dom-root dom) "querySelector" ".ce-sizer"))
         (rect (js:call sizer "getBoundingClientRect"))
         (x (js:get event "clientX"))
         (y (js:get event "clientY"))
         (offsets (row-offsets pane))
         (row (floor (- y (js:get rect "top")) lh))
         (line (if (and offsets (< -1 row (aref offsets (line-count doc))))
                   (line-at-row offsets row)
                   row))
         (sub (if offsets (- row (line-top pane line offsets)) 0)))
    (cond ((< row 0) (make-pos 0 0))
          ((>= line (line-count doc)) (document-end doc))
          ((and offsets (>= row (aref offsets (line-count doc)))) (document-end doc))
          (t (make-pos line (column-at pane line sub x (+ (js:get rect "top") (* row lh) (/ lh 2))))))))

(defun column-at (pane line sub x y)
  "The column of LINE (in its row SUB, if it is wrapped) nearest to the point X, Y of the page."
  (let* ((root (dom-root (dom pane)))
         (editor (code-editor-editor pane))
         (element (find-row root line))
         (rows (line-rows editor line))
         (len (line-length (editor-document editor) line))
         (from (if (plusp sub) (or (nth (1- sub) rows) len) 0))
         (last (>= sub (length rows)))
         (to (if last len (nth sub rows)))
         (text (and element
                    (if rows
                        (js:call (js:call element "querySelectorAll" ".ce-row") "item" sub)
                        (js:call element "querySelector" ".ce-text")))))
    (if (not (js:js-value-p text))
        from
        (let* ((rect (js:call text "getBoundingClientRect"))
               (left (js:get rect "left"))
               ;; the end of a row that is not the last is the start of the next
               (end (if last to (max from (1- to)))))
          (cond ((<= x left) from)
                ((>= x (js:get rect "right")) end)
                (t (let ((caret (caret-at x y)))
                     (if caret
                         (min end (+ from (caret-column text (car caret) (cdr caret))))
                         end))))))))

(defun find-row (root line)
  "The element of LINE on the page, or NIL."
  (let ((row (js:call root "querySelector" (format nil ".ce-line[data-line=\"~D\"]" line))))
    (and (js:js-value-p row) row)))

(defun caret-at (x y)
  "(node . offset) of the text under the point X, Y, from the browser."
  (let* ((document (js:global "document"))
         (position (if (eq (js:typeof (js:get document "caretPositionFromPoint")) :function)
                       (js:call document "caretPositionFromPoint" x y)
                       nil)))
    (cond ((js:js-value-p position)
           (cons (js:get position "offsetNode") (js:get position "offset")))
          ((eq (js:typeof (js:get document "caretRangeFromPoint")) :function)
           (let ((range (js:call document "caretRangeFromPoint" x y)))
             (and (js:js-value-p range)
                  (cons (js:get range "startContainer") (js:get range "startOffset"))))))))

(defun caret-column (text node offset)
  "The column in the line of the element TEXT (.ce-text) of the OFFSET in NODE."
  (let ((column 0))
    (loop for span = (js:get text "firstChild") then (js:get span "nextSibling")
          while (js:js-value-p span)
          do (let ((content (js:get span "textContent")))
               (when (or (js:call span "contains" node))
                 (let ((data (js:get node "textContent")))
                   (return-from caret-column
                     (+ column (if (stringp data)
                                   ;; OFFSET counts UTF-16 units; the column, characters
                                   (length (js:call (js:get node "textContent") "substring" 0 offset))
                                   0)))))
               (unless (js:call span "matches" ".ce-eol")
                 (incf column (if (stringp content) (length content) 0)))))
    column))

(defun on-pointer-down (pane event)
  (when (and (eql (js:get event "button") 0)
             ;; not on the scrollbars
             (let ((target (js:get event "currentTarget")))
               (and (< (js:get event "offsetX") (js:get target "clientWidth"))
                    (< (js:get event "offsetY") (js:get target "clientHeight")))))
    (js:call event "preventDefault")
    (code-editor-focus pane)
    (let* ((editor (code-editor-editor pane))
           (dom (dom pane))
           (pos (event-pos pane event))
           (clicks (click-count dom event)))
      (setf (editor-mark-active editor) nil
            (editor-goal-column editor) nil)
      (cond ((and (numberp clicks) (= (mod (1- clicks) 3) 1))
             (setf (dom-drag dom) :word)
             (multiple-value-bind (start end) (word-at editor pos)
               (setf (dom-drag-origin dom) (cons start end))
               (set-selection editor start end)))
            ((and (numberp clicks) (= (mod (1- clicks) 3) 2))
             (setf (dom-drag dom) :line)
             (let ((start (make-pos (pos-line pos) 0))
                   (end (or (next-pos (editor-document editor)
                                      (make-pos (pos-line pos) (line-length (editor-document editor) (pos-line pos))))
                            (document-end (editor-document editor)))))
               (setf (dom-drag-origin dom) (cons start end))
               (set-selection editor start end)))
            (t
             (setf (dom-drag dom) :char)
             (if (flag event "shiftKey")
                 (move-cursor editor pos :extend t)
                 (set-selection editor pos))
             (setf (dom-drag-origin dom) (cons (editor-anchor editor) (editor-anchor editor)))))
      (ui:capture-pointer ui:*backend* event)
      (after-command pane))))

(defun click-count (dom event)
  "1, 2 or 3 for a single, double or triple click (and so on, in turn)."
  (let ((time (js:get event "timeStamp"))
        (x (js:get event "clientX")) (y (js:get event "clientY"))
        (last (dom-last-press dom)))
    (let ((count (if (and last (< (- time (first last)) 450)
                          (< (abs (- x (second last))) 5) (< (abs (- y (third last))) 5))
                     (1+ (fourth last))
                     1)))
      (setf (dom-last-press dom) (list time x y count))
      count)))

(defun word-at (editor pos)
  "The start and end of the word at POS (or of the character)."
  (let* ((doc (editor-document editor))
         (s (line-string doc (pos-line pos)))
         (col (pos-col pos))
         (word (lambda (i) (and (< -1 i (length s)) (word-p editor (char s i))))))
    (cond ((or (funcall word col) (funcall word (1- col)))
           (let ((start (if (funcall word col) col (1- col))) (end col))
             (loop while (funcall word (1- start)) do (decf start))
             (loop while (funcall word end) do (incf end))
             (values (make-pos (pos-line pos) start) (make-pos (pos-line pos) end))))
          ((< col (length s)) (values pos (make-pos (pos-line pos) (1+ col))))
          (t (values pos pos)))))

(defun on-pointer-move (pane event)
  (let ((dom (dom pane)))
    (when (and (dom-drag dom) (plusp (js:get event "buttons")))
      (let* ((editor (code-editor-editor pane))
             (pos (event-pos pane event))
             (origin (dom-drag-origin dom)))
        (ecase (dom-drag dom)
          (:char (move-cursor editor pos :extend t))
          (:word (multiple-value-bind (start end) (word-at editor pos)
                   (if (pos< start (car origin))
                       (set-selection editor (cdr origin) start)
                       (set-selection editor (car origin) end))))
          (:line (let* ((doc (editor-document editor))
                        (start (make-pos (pos-line pos) 0))
                        (end (or (next-pos doc (make-pos (pos-line pos) (line-length doc (pos-line pos))))
                                 (document-end doc))))
                   (if (pos< start (car origin))
                       (set-selection editor (cdr origin) start)
                       (set-selection editor (car origin) end)))))
        (after-command pane)))))

(defun on-pointer-up (pane event)
  (declare (ignore event))
  (setf (dom-drag (dom pane)) nil))

;;; the platform
(let ((navigator (ignore-errors (js:global "navigator"))))
  (when (js:js-value-p navigator)
    (let ((platform (js:get navigator "platform")))
      (setf *mac-p* (and (stringp platform) (search "Mac" platform) t)))))
