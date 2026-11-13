;;; stream.lisp — panes that are output streams, and the interactor (after CLIM)
;;;
;;; A STREAM-PANE is a character output stream: what is written to it with
;;; FORMAT, PRINT... is what it shows, and what is written inside
;;; WITH-OUTPUT-AS-PRESENTATION (or by PRESENT with :STREAM) is a
;;; presentation, like everything else on the screen:
;;;
;;;   (format pane "~D contacts, the first is " (length *contacts*))
;;;   (present (first *contacts*) 'contact :stream pane)
;;;   (formatting-table (pane)
;;;     (dolist (contact *contacts*)
;;;       (formatting-row (pane)
;;;         (formatting-cell (pane) (present contact 'contact :stream pane))
;;;         (formatting-cell (pane :align :right) (princ (contact-age contact) pane)))))
;;;
;;; With a :DISPLAY-FUNCTION, the pane shows what that function writes, and
;;; it is called again when state it read has changed.
;;;
;;; An INTERACTOR-PANE also has a line in which the user types commands by
;;; their names, with their arguments:  Rename contact Ada "Ada Lovelace".
;;; An argument that is missing is asked for there (or clicked), and what a
;;; command prints is shown in the pane.
;;;
;;; Compile it after dialog.lisp.

(in-package "UI")

;;; ------------------------------------------------------------------
;;; Output records: what was written

(defstruct record
  kind          ; :TEXT :NEWLINE :PRESENTATION :STYLE :TABLE :ROW :CELL :ITEMS :MARKUP
  a b           ; the string; the object and type; the style; the function...
  children)     ; a sink

(defstruct sink (records '()))          ; the latest first

(defstruct stream-state
  (root (make-sink))
  (sinks nil)                             ; the sinks that are open, the innermost first
  (buffer (make-string-output-stream))    ; the characters written since the last record
  (column 0)
  (input-node nil))                       ; of an interactor: the backend's node of its line

(defmodel stream-pane (pane gray:fundamental-character-output-stream)
  (;; a function of the pane that writes what it shows; it is called again
   ;; when state it read has changed.  Without it, the pane shows what was
   ;; written to it since WINDOW-CLEAR.
   (display-function :initarg :display-function :initform nil :accessor stream-pane-display-function)
   ;; what was written (not observed: VERSION is what the display follows)
   (state :initform (make-stream-state) :reader stream-pane-state)
   ;; changes with every output: what makes the pane show it
   (version :initform 0 :accessor stream-pane-version))
  (:default-initargs :stretch t)
  (:documentation "A pane that is a character output stream."))

(defun stream-pane-root (pane) (stream-state-root (stream-pane-state pane)))
(defun (setf stream-pane-root) (value pane) (setf (stream-state-root (stream-pane-state pane)) value))
(defun stream-pane-sinks (pane) (stream-state-sinks (stream-pane-state pane)))
(defun (setf stream-pane-sinks) (value pane) (setf (stream-state-sinks (stream-pane-state pane)) value))
(defun stream-pane-buffer (pane) (stream-state-buffer (stream-pane-state pane)))
(defun stream-pane-column (pane) (stream-state-column (stream-pane-state pane)))
(defun (setf stream-pane-column) (value pane) (setf (stream-state-column (stream-pane-state pane)) value))

(defvar *redisplaying* nil "The stream pane whose display function is running.")

(defun stream-changed (pane)
  (unless (eq *redisplaying* pane)
    (incf (stream-pane-version pane))))

(defun current-sink (pane)
  (or (first (stream-pane-sinks pane)) (stream-pane-root pane)))

(defun flush-text (pane)
  (let ((text (get-output-stream-string (stream-pane-buffer pane))))
    (when (plusp (length text))
      (push (make-record :kind :text :a text) (sink-records (current-sink pane))))))

(defun add-record (pane record)
  (flush-text pane)
  (push record (sink-records (current-sink pane)))
  (stream-changed pane)
  record)

(defmethod gray:stream-write-char ((pane stream-pane) char)
  (cond ((char= char #\Newline)
         (add-record pane (make-record :kind :newline))
         (setf (stream-pane-column pane) 0))
        (t (write-char char (stream-pane-buffer pane))
           (incf (stream-pane-column pane))
           (stream-changed pane)))
  char)

(defmethod gray:stream-line-column ((pane stream-pane))
  (stream-pane-column pane))

(defun call-with-records (pane kind a b function)
  "Add a record of KIND whose children are what FUNCTION writes to PANE."
  (let ((record (add-record pane (make-record :kind kind :a a :b b :children (make-sink)))))
    (push (record-children record) (stream-pane-sinks pane))
    (unwind-protect (funcall function)
      (flush-text pane)
      (pop (stream-pane-sinks pane))
      (stream-changed pane))
    record))

(defun window-clear (pane)
  "Forget what was written to the stream pane PANE."
  (get-output-stream-string (stream-pane-buffer pane))
  (setf (stream-pane-root pane) (make-sink)
        (stream-pane-sinks pane) nil
        (stream-pane-column pane) 0)
  (stream-changed pane)
  pane)

;;; ------------------------------------------------------------------
;;; Writing more than characters

(defun call-with-output-as-presentation (first second third function markup)
  ;; (stream object type) or (object type): see WITH-OUTPUT-AS-PRESENTATION
  (if (typep first 'stream-pane)
      (call-with-records first :presentation second (or third (presentation-type-of second)) function)
      (presenting first second (funcall markup))))

(defmacro with-output-as-presentation ((first &optional second third) &body body)
  "(with-output-as-presentation (object [type]) markup...) is the element of
the markup (evaluated as by HTML) as a presentation of OBJECT with the
presentation type TYPE (evaluated; by default PRESENTATION-TYPE-OF the object).
(with-output-as-presentation (stream object [type]) form...) makes what the
forms write to the stream pane STREAM a presentation of OBJECT."
  `(call-with-output-as-presentation ,first ,second ,third
                                     (lambda () ,@body)
                                     (lambda () (html ,@body))))

(defun present-to-stream (object type pane)
  (call-with-records pane :presentation object type
                     (lambda ()
                       (let ((shown (present-object object type)))
                         (if (stringp shown)
                             (write-string shown pane)
                             ;; markup: it is made again whenever the pane is shown
                             (add-record pane (make-record :kind :markup
                                                           :a (lambda () (present-object object type)))))))))

(defun write-markup (pane function)
  "Write to the stream pane PANE what FUNCTION returns (a node of HTML, or a
list of nodes) whenever the pane is shown."
  (add-record pane (make-record :kind :markup :a function)))

(defmacro with-text-style ((pane &rest style &key bold italic color size class) &body body)
  "What BODY writes to the stream pane PANE is BOLD, ITALIC, of the COLOR (a
CSS colour), SIZE (a CSS length or a number of pixels) or has the CSS CLASS."
  (declare (ignore bold italic color size class))
  `(call-with-records ,pane :style (list ,@style) nil (lambda () ,@body)))

(defmacro formatting-table ((pane) &body body)
  "What BODY writes to the stream pane PANE with FORMATTING-ROW and
FORMATTING-CELL is a table."
  `(call-with-records ,pane :table nil nil (lambda () ,@body)))

(defmacro formatting-row ((pane) &body body)
  "A row of the FORMATTING-TABLE: its cells are what BODY writes with FORMATTING-CELL."
  `(call-with-records ,pane :row nil nil (lambda () ,@body)))

(defmacro formatting-cell ((pane &key (align :left)) &body body)
  "A cell of a table or item list: what BODY writes, ALIGNed :LEFT, :RIGHT or :CENTER."
  `(call-with-records ,pane :cell ,align nil (lambda () ,@body)))

(defmacro formatting-item-list ((pane &key (columns 3)) &body body)
  "What BODY writes with FORMATTING-CELL is laid out in COLUMNS columns."
  `(call-with-records ,pane :items ,columns nil (lambda () ,@body)))

;;; ------------------------------------------------------------------
;;; Showing the records

(defun style-css (style)
  (format nil "~:[~;font-weight:600;~]~:[~;font-style:italic;~]~@[color:~A;~]~@[font-size:~A;~]"
          (getf style :bold) (getf style :italic) (getf style :color) (css-length (getf style :size))))

(defun record-nodes (records)
  "The nodes for RECORDS (the latest first)."
  (let ((nodes '()))
    (dolist (record records nodes)
      (flet ((children () (record-nodes (sink-records (record-children record)))))
        (push (ecase (record-kind record)
                (:text (record-a record))
                (:newline (element "br" nil))
                (:markup (funcall (record-a record)))
                (:presentation
                 (presenting (record-a record) (record-b record) (element "span" nil (children))))
                (:style (element "span" (list :class (getf (record-a record) :class)
                                              :style (style-css (record-a record)))
                                 (children)))
                (:table (element "table" (list :class "ui-formatted-table")
                                 (element "tbody" nil (children))))
                (:row (element "tr" nil (children)))
                (:cell (element "td" (list :class (format nil "ui-adjust-~(~A~)" (record-a record)))
                                (children)))
                (:items (element "div" (list :class "ui-item-list"
                                             :style (format nil "grid-template-columns:repeat(~D,auto)"
                                                            (record-a record)))
                                 ;; its cells are DIVs
                                 (mapcar (lambda (child)
                                           (if (and (element-p child) (string= (element-tag child) "td"))
                                               (apply #'element "div" (element-props child)
                                                      (element-children child))
                                               child))
                                         (children)))))
              nodes)))))

(defun stream-pane-nodes (pane)
  "The nodes for what the stream pane PANE shows now."
  (let ((function (stream-pane-display-function pane)))
    (cond (function
           ;; what the function writes now
           (let ((*redisplaying* pane))
             (setf (stream-pane-root pane) (make-sink)
                   (stream-pane-sinks pane) nil
                   (stream-pane-column pane) 0)
             (get-output-stream-string (stream-pane-buffer pane))
             (funcall function pane)
             (flush-text pane)))
          (t (stream-pane-version pane)       ; shown again after every output
             (flush-text pane)))
    (record-nodes (sink-records (stream-pane-root pane)))))

(defmethod render-pane ((pane stream-pane))
  (html (:div :class "ui-stream-pane"
          ;; (the box is turned upside down, so that it stays at its end)
          (:div :class "ui-stream-output"
            (:div :class "ui-stream-text" (stream-pane-nodes pane))))))

;;; PRESENT with :STREAM

(defun present (object &optional type &key stream)
  "A node that shows OBJECT (as PRESENT-OBJECT says) and is a presentation
of it with the presentation type TYPE.  With STREAM, a stream pane, write
that to it instead."
  (let ((type (or type (presentation-type-of object))))
    (if stream
        (present-to-stream object type stream)
        (presenting object type (element "span" nil (present-object object type))))))

;;; ------------------------------------------------------------------
;;; Reading an object that the user typed

(defgeneric accept-from-string (name type string)
  (:documentation "The object of the presentation type TYPE (whose name is NAME) that
STRING denotes, and true — or NIL and NIL if it denotes none.  Methods for
the types of an application let the user type its objects in the interactor.")
  (:method (name type string)
    (declare (ignore name type string))
    (values nil nil)))

(defmethod accept-from-string ((name (eql 'string)) type string)
  (values string t))

(defmethod accept-from-string ((name (eql 'integer)) type string)
  (let ((number (parse-number string)))
    (if (integerp number) (values number t) (values nil nil))))

(defmethod accept-from-string ((name (eql 'number)) type string)
  (let ((number (parse-number string)))
    (if number (values number t) (values nil nil))))

(defmethod accept-from-string ((name (eql 'boolean)) type string)
  (cond ((member string '("yes" "y" "true" "t") :test #'string-equal) (values t t))
        ((member string '("no" "n" "false" "nil") :test #'string-equal) (values nil t))
        (t (values nil nil))))

(defmethod accept-from-string ((name (eql 'member)) type string)
  (let ((tail (member string (cdr type) :test (lambda (string item) (string-equal string (text-of item))))))
    (if tail (values (car tail) t) (values nil nil))))

(defgeneric presentation-type-choices (name type)
  (:documentation "The strings the user may type for an object of the presentation
type TYPE: what the interactor offers as completions.")
  (:method (name type) (declare (ignore name type)) nil))

(defmethod presentation-type-choices ((name (eql 'member)) type)
  (mapcar #'text-of (cdr type)))

(defmethod presentation-type-choices ((name (eql 'boolean)) type)
  '("yes" "no"))

(defun type-name (type) (if (consp type) (car type) type))

;;; ------------------------------------------------------------------
;;; The interactor

(defmodel interactor-pane (stream-pane)
  (;; what is shown in front of the line
   (prompt :initarg :prompt :initform "Command:" :accessor interactor-prompt)
   ;; the text in the line
   (input :initform "" :accessor interactor-input)
   ;; what was entered, the latest first, and where the arrow keys are in it
   (history :initform nil :accessor interactor-history)
   (history-index :initform nil :accessor interactor-history-index))
  (:documentation "A stream pane with a line in which the user types commands: the name
of a command (its label, or the name of its function) and its arguments,
strings with spaces in double quotes.  The Tab key completes, the arrows
recall what was entered.  While an argument is asked for (ACCEPT), the line
asks for it: the user types it or clicks a presentation."))

(defmethod pane-mounted ((pane interactor-pane))
  (setf *interactors* (cons pane (remove pane *interactors*))
        *command-output* pane))

(defmethod pane-unmounted ((pane interactor-pane))
  (setf *interactors* (remove pane *interactors*))
  (when (eq *command-output* pane)
    (setf *command-output* (first *interactors*))))

(defun focus-interactors ()
  "Give the focus to the line of the interactor (the latest on the screen)."
  (let* ((pane (first (active-interactors)))
         (node (and pane (stream-state-input-node (stream-pane-state pane)))))
    (when node (node-call *backend* node "focus"))))

(defun command-typed-name (command)
  "What the user types for COMMAND: its label without the mnemonic mark and the dots."
  (string-right-trim "….。 " (parse-mnemonic (command-label command))))

(defun command-names (command)
  (remove-duplicates
   (list (command-typed-name command)
         (substitute #\Space #\- (string-downcase (symbol-name (command-name command)))))
   :test #'string-equal))

(defun typable-commands ()
  (remove-if-not (lambda (command)
                   (and (command-active-p command)
                        (not (eq (command-name command) 'command-palette))))
                 *commands*))

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun parse-command-name (text)
  "The command whose name TEXT starts with (the longest such name) and the
rest of TEXT; or the only command whose name starts with TEXT."
  (let ((best nil) (best-length -1))
    (dolist (command (typable-commands))
      (dolist (name (command-names command))
        (when (and (prefix-p name text)
                   (or (= (length name) (length text))
                       (char= (char text (length name)) #\Space))
                   (> (length name) best-length))
          (setf best command best-length (length name)))))
    (if best
        (values best (string-left-trim " " (subseq text best-length)))
        (let ((matching (remove-if-not (lambda (command)
                                         (some (lambda (name) (prefix-p text name)) (command-names command)))
                                       (typable-commands))))
          (if (and matching (null (cdr matching)))
              (values (first matching) "")
              (values nil text))))))

(defun split-arguments (text)
  "The words of TEXT; a word in double quotes may contain spaces."
  (let ((words '()) (word (make-string-output-stream)) (quoted nil) (any nil))
    (flet ((end-word ()
             (when any (push (get-output-stream-string word) words))
             (setf any nil)))
      (loop for char across text
            do (cond ((char= char #\") (setf quoted (not quoted) any t))
                     ((and (char= char #\Space) (not quoted)) (end-word))
                     (t (write-char char word) (setf any t))))
      (end-word))
    (nreverse words)))

(defun interactor-completions (pane)
  "What the line of PANE can be completed to: (text-to-insert . label)..."
  (let ((text (interactor-input pane)))
    (cond ((zerop (length text)) nil)
          (*input-context*
           (let ((type (input-context-type *input-context*)))
             (loop for choice in (presentation-type-choices (type-name type) type)
                   when (and (prefix-p text choice) (string-not-equal text choice))
                     collect (cons choice choice))))
          (t
           (multiple-value-bind (command rest) (parse-command-name text)
             (if (and command (find #\Space text)
                      (prefix-p (command-typed-name command) text))
                 ;; the argument that is typed now
                 (let* ((words (split-arguments rest))
                        (typing (not (char= (char text (1- (length text))) #\Space)))
                        (index (if typing (1- (length words)) (length words)))
                        (word (if typing (first (last words)) ""))
                        (parameter (nth index (command-parameters command)))
                        (type (second parameter))
                        (head (subseq text 0 (- (length text) (length word)))))
                   (and type
                        (loop for choice in (presentation-type-choices (type-name type) type)
                              when (and (prefix-p word choice) (string-not-equal word choice))
                                collect (cons (format nil "~A~:[~A~;\"~A\"~]" head (find #\Space choice) choice)
                                              choice))))
                 (loop for command in (typable-commands)
                       for name = (find-if (lambda (name) (prefix-p text name)) (command-names command))
                       when (and name (command-enabled-p (command-name command)))
                         collect (cons (format nil "~A " name) name))))))))

(defun interactor-say (pane control &rest arguments)
  (fresh-line pane)
  (with-text-style (pane :class "ui-interactor-note")
    (apply #'format pane control arguments))
  (terpri pane))

(defun interactor-echo (pane prompt text)
  (fresh-line pane)
  (with-text-style (pane :class "ui-interactor-echo")
    (format pane "~A ~A" prompt text))
  (terpri pane))

(defun interactor-enter (pane)
  "The user pressed Enter in the line of PANE."
  (let ((text (string-trim " " (interactor-input pane))))
    (setf (interactor-input pane) ""
          (interactor-history-index pane) nil)
    (cond
      ;; an argument that ACCEPT asks for
      (*input-context*
       (let ((type (input-context-type *input-context*))
             (prompt (input-context-prompt *input-context*)))
         (multiple-value-bind (object found) (accept-from-string (type-name type) type text)
           (cond (found
                  (interactor-echo pane prompt text)
                  (supply object))
                 (t (interactor-say pane "~S is not ~A" text (presentation-type-description type)))))))
      ((zerop (length text)))
      (t
       (push text (interactor-history pane))
       (interactor-echo pane (interactor-prompt pane) text)
       (multiple-value-bind (command rest) (parse-command-name text)
         (cond ((null command) (interactor-say pane "No command: ~A" text))
               ((not (command-enabled-p (command-name command)))
                (interactor-say pane "~A is not available now" (command-typed-name command)))
               (t
                (let ((words (split-arguments rest))
                      (parameters (command-parameters command))
                      (arguments '()))
                  (loop for word in words
                        for parameter = (pop parameters)
                        do (let ((type (and parameter (second parameter))))
                             (if (null type)
                                 (push word arguments)
                                 (multiple-value-bind (object found)
                                     (accept-from-string (type-name type) type word)
                                   (unless found
                                     (interactor-say pane "~S is not ~A" word
                                                     (presentation-type-description type))
                                     (return-from interactor-enter nil))
                                   (push object arguments)))))
                  (apply #'invoke-command (command-name command) (nreverse arguments))))))))))

(defun interactor-key (pane event)
  (let ((key (event-get *backend* event "key")))
    (cond ((event-get *backend* event "isComposing"))
          ((equal key "Enter")
           (when event (stop-event *backend* event))
           (interactor-enter pane))
          ((equal key "Tab")
           (when event (stop-event *backend* event))
           (let ((completions (interactor-completions pane)))
             (when completions
               (setf (interactor-input pane) (car (first completions))))))
          ((equal key "Escape")
           (if *input-context* (cancel-accept) (setf (interactor-input pane) "")))
          ((member key '("ArrowUp" "ArrowDown") :test #'equal)
           (let* ((history (interactor-history pane))
                  (index (interactor-history-index pane))
                  (next (if (equal key "ArrowUp")
                            (if index (min (1+ index) (1- (length history))) 0)
                            (and index (plusp index) (1- index)))))
             (when history
               (when event (stop-event *backend* event))
               (setf (interactor-history-index pane) next
                     (interactor-input pane) (if next (nth next history) ""))))))))

(defmethod render-pane ((pane interactor-pane))
  (let ((completions (interactor-completions pane)))
    (html
      (:div :class "ui-stream-pane ui-interactor-pane"
        (:div :class "ui-stream-output"
          (:div :class "ui-stream-text" (stream-pane-nodes pane)))
        ;; (always there, so that the line below is not made anew, which
        ;; would take the focus from it)
        (:ul :class "ui-completions" :role "listbox" :hidden (null completions)
          (loop for (text . label) in completions
                for index from 0 below 8
                collect (let ((text text))
                          (:li :class "ui-item" :role "option"
                               :on-click (lambda (&optional event)
                                           (declare (ignore event))
                                           (setf (interactor-input pane) text))
                            label))))
        (:label :class "ui-interactor-line"
          (:span :class "ui-interactor-prompt"
            (if *input-context* (input-context-prompt *input-context*) (interactor-prompt pane)))
          (:input :type "text" :class "ui-interactor-input" :value (interactor-input pane)
                  :autocomplete "off"
                  :on-mount (lambda (node)
                              (setf (stream-state-input-node (stream-pane-state pane)) node))
                  :on-input (lambda (&optional event)
                              (setf (interactor-input pane) (event-target-get *backend* event "value")))
                  :on-keydown (lambda (&optional event) (interactor-key pane event))))))))
