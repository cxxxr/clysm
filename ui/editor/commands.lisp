;;;; commands.lisp — the commands of the editor, and the keys that run them
;;;;
;;;; A command is a function of the editor, named by a symbol
;;;; (DEFINE-EDITOR-COMMAND).  A keymap maps keys to commands, or to other
;;;; keymaps for prefixes like C-x.  A key is a string: the modifiers C-
;;;; (Control), M- (Alt/Option), s- (Command/Super) and S- (Shift), in this
;;;; order, then the name of the key: a character ("a", "<"), or "Left",
;;;; "Right", "Up", "Down", "Home", "End", "PageUp", "PageDown", "Backspace",
;;;; "Delete", "Return", "Tab", "Escape", "Space".  In a definition, Mod- is
;;;; Command on a Mac and Control elsewhere.

(in-package "EDITOR")

(defvar *mac-p* nil "True on macOS: Mod- in keys is Command (s-) instead of Control.")
(defvar *standard-keymap*)
(defvar *kill-ring* '() "The texts killed and copied, the latest first.")
(defparameter *kill-ring-max* 60)

;;; ------------------------------------------------------------------
;;; Commands

(defvar *editor-commands* (make-hash-table :test 'equal) "name -> (function . documentation)")

(defmacro define-editor-command (name (editor &rest options) &body body)
  "Define the command NAME, a function of the editor.  OPTIONS: :UNDO-KIND
merges the following runs of the command into one step of undo; :KEEP-GOAL
keeps the column of vertical motion; :EDIT marks a command that changes the
text (it does nothing in a read-only editor)."
  (let ((doc (and (stringp (car body)) (list (pop body)))))
    `(progn
       (defun ,name (,editor) ,@doc
         ,@(when (getf options :edit)
             `((when (editor-read-only ,editor)
                 (setf (editor-message ,editor) "Read-only")
                 (return-from ,name nil))))
         (with-undo-group (,editor ,(getf options :undo-kind))
           ,@body))
       (setf (gethash (symbol-name ',name) *editor-commands*)
             (list #',name ,(car doc) ,(getf options :keep-goal)))
       ',name)))

(defun find-editor-command (name)
  "The entry (function documentation keep-goal) of the command NAME (a symbol or string)."
  (gethash (string-upcase (string name)) *editor-commands*))

(defun editor-commands ()
  "The names of all the commands, sorted."
  (sort (loop for k being the hash-keys of *editor-commands* collect k) #'string<))

(defun run-command (editor name)
  "Run the command NAME on EDITOR."
  (let ((entry (find-editor-command name)))
    (unless entry (error "No editor command ~A" name))
    (setf (editor-message editor) nil)
    (unless (third entry) (setf (editor-goal-column editor) nil))
    (prog1 (funcall (first entry) editor)
      (setf (last-command editor) (string-upcase (string name))))))

;;; ------------------------------------------------------------------
;;; Keymaps

(defstruct (keymap (:constructor %make-keymap (name parent)))
  name parent (table (make-hash-table :test 'equal)))

(defun make-keymap (&key name parent) (%make-keymap name parent))

(defun define-key (keymap key command)
  "Bind KEY (\"C-a\", or a sequence \"C-x C-s\") to COMMAND (a symbol, a function
of the editor, or NIL to unbind)."
  (let* ((keys (split-keys key)))
    (loop for (k . more) on keys
          do (if more
                 (setf keymap (let ((sub (gethash k (keymap-table keymap))))
                                (if (keymap-p sub)
                                    sub
                                    (setf (gethash k (keymap-table keymap))
                                          (make-keymap :name k)))))
                 (setf (gethash k (keymap-table keymap)) command)))
    command))

(defun split-keys (string)
  (let ((keys '()) (start 0))
    (loop
      (let ((space (position #\Space string :start start)))
        ;; "C-x Space": a key name never contains a space
        (push (subseq string start space) keys)
        (if space (setf start (1+ space)) (return))))
    (remove "" (nreverse keys) :test #'string=)))

(defun mod-form (key)
  "KEY with the modifier of Mod- written Mod-, if it has it."
  (let ((mod (if *mac-p* "s-" "C-")))
    ;; the modifiers are in the order C- M- s- S-
    (let ((p (search mod key)))
      (if (and p (every (lambda (i) (char= (char key (1+ i)) #\-)) (loop for i from 0 below p by 2 collect i)))
          (concatenate 'string (subseq key 0 p) "Mod-" (subseq key (+ p 2)))
          key))))

(defun lookup-key (keymap key)
  "The binding of KEY in KEYMAP or its parents."
  (loop for map = keymap then (keymap-parent map)
        while map
        do (multiple-value-bind (binding found) (gethash (mod-form key) (keymap-table map))
             (when found (return binding)))
           (multiple-value-bind (binding found) (gethash key (keymap-table map))
             (when found (return binding)))))

(defun handle-key (editor key)
  "Run what KEY is bound to in the keymap of EDITOR; true if it was bound."
  (let* ((map (or (editor-prefix editor) (editor-keymap editor) *standard-keymap*))
         (binding (lookup-key map key)))
    (setf (editor-prefix editor) nil)
    (cond ((keymap-p binding)
           (setf (editor-prefix editor) binding
                 (editor-message editor) (format nil "~A-" key))
           (incf (editor-version editor))
           t)
          ((null binding)
           (when (and (not (eq map (editor-keymap editor))) (not (eq map *standard-keymap*)))
             (setf (editor-message editor) (format nil "~A is undefined" key))
             (incf (editor-version editor)))
           nil)
          ((functionp binding)
           (funcall binding editor) t)
          (t (run-command editor binding) t))))

;;; ------------------------------------------------------------------
;;; Motion

(defun doc (editor) (editor-document editor))
(defun head (editor) (editor-head editor))

(defmacro define-motion (name select-name doc &body body)
  "Define the command NAME, which moves the cursor to the position BODY
returns (EDITOR is bound), and SELECT-NAME, which extends the selection
there."
  (let ((keep (and (eq (car body) :keep-goal) (pop body))))
    `(progn
       (defun ,(intern (format nil "%~A" name)) (editor)
         (declare (ignorable editor))
         ,@body)
       (define-editor-command ,name (editor :keep-goal ,keep) ,doc
         (let ((pos (,(intern (format nil "%~A" name)) editor)))
           (when pos
             (if (and (not (selection-empty-p editor)) (not (editor-mark-active editor))
                      (member ',name '(forward-char backward-char)))
                 ;; collapse the selection to its side
                 (move-cursor editor (if (eq ',name 'forward-char)
                                         (selection-end editor)
                                         (selection-start editor)))
                 (move-cursor editor pos)))))
       (define-editor-command ,select-name (editor :keep-goal ,keep) ,doc
         (let ((pos (,(intern (format nil "%~A" name)) editor)))
           (when pos (move-cursor editor pos :extend t)))))))

(define-motion forward-char select-forward-char "Move one character forward."
  (or (next-pos (doc editor) (head editor)) (head editor)))

(define-motion backward-char select-backward-char "Move one character backward."
  (or (previous-pos (doc editor) (head editor)) (head editor)))

(defun vertical (editor lines)
  (let* ((head (head editor))
         (goal (or (editor-goal-column editor) (setf (editor-goal-column editor) (pos-col head))))
         (line (+ (pos-line head) lines))
         (count (line-count (doc editor))))
    (cond ((< line 0) (make-pos 0 0))
          ((>= line count) (document-end (doc editor)))
          (t (make-pos line (min goal (line-length (doc editor) line)))))))

(define-motion next-line select-next-line "Move one line down." :keep-goal (vertical editor 1))
(define-motion previous-line select-previous-line "Move one line up." :keep-goal (vertical editor -1))

(defun page-lines (editor) (max 1 (1- (editor-property editor :page-lines 20))))

(define-motion page-down select-page-down "Move one screen down."
  :keep-goal (vertical editor (page-lines editor)))
(define-motion page-up select-page-up "Move one screen up."
  :keep-goal (vertical editor (- (page-lines editor))))

(define-motion beginning-of-line select-beginning-of-line
    "Move to the first non-blank character of the line, or to its start if already there."
  (let* ((head (head editor))
         (indent (line-indentation editor (pos-line head))))
    (make-pos (pos-line head) (if (= (pos-col head) indent) 0 indent))))

(define-motion line-start select-line-start "Move to the start of the line."
  (make-pos (pos-line (head editor)) 0))

(define-motion end-of-line select-end-of-line "Move to the end of the line."
  (let ((line (pos-line (head editor))))
    (make-pos line (line-length (doc editor) line))))

(define-motion beginning-of-buffer select-beginning-of-buffer "Move to the start of the text."
  (make-pos 0 0))
(define-motion end-of-buffer select-end-of-buffer "Move to the end of the text."
  (document-end (doc editor)))

(defun word-p (editor char)
  (and char (word-char-p (editor-language editor) char)))

(defun forward-word-pos (editor pos)
  (let ((doc (doc editor)))
    (loop while (and (char-at doc pos) (not (word-p editor (char-at doc pos))))
          do (setf pos (next-pos doc pos)))
    (loop while (and (char-at doc pos) (word-p editor (char-at doc pos)))
          do (setf pos (next-pos doc pos)))
    pos))

(defun backward-word-pos (editor pos)
  (let ((doc (doc editor)))
    (flet ((before (p) (let ((q (previous-pos doc p))) (and q (char-at doc q)))))
      (loop while (and (previous-pos doc pos) (not (word-p editor (before pos))))
            do (setf pos (previous-pos doc pos)))
      (loop while (and (previous-pos doc pos) (word-p editor (before pos)))
            do (setf pos (previous-pos doc pos)))
      pos)))

(define-motion forward-word select-forward-word "Move to the end of the word."
  (forward-word-pos editor (head editor)))
(define-motion backward-word select-backward-word "Move to the start of the word."
  (backward-word-pos editor (head editor)))

(define-motion forward-sexp select-forward-sexp "Move over the S-expression after the cursor."
  (or (forward-sexp-pos editor (head editor))
      (progn (setf (editor-message editor) "No S-expression after the cursor") nil)))
(define-motion backward-sexp select-backward-sexp "Move over the S-expression before the cursor."
  (or (backward-sexp-pos editor (head editor))
      (progn (setf (editor-message editor) "No S-expression before the cursor") nil)))
(define-motion up-list select-up-list "Move to the start of the list around the cursor."
  (enclosing-open editor (head editor)))

(define-editor-command select-all (editor)
  "Select the whole text."
  (setf (editor-mark-active editor) nil)
  (set-selection editor (make-pos 0 0) (document-end (doc editor))))

(define-editor-command set-mark (editor)
  "Start a selection at the cursor that motion extends (Emacs's mark)."
  (setf (editor-mark-active editor) t)
  (set-selection editor (head editor))
  (setf (editor-message editor) "Mark set"))

(define-editor-command exchange-point-and-mark (editor)
  "Swap the cursor and the other end of the selection."
  (set-selection editor (head editor) (editor-anchor editor))
  (setf (editor-mark-active editor) t))

(define-editor-command keyboard-quit (editor)
  "Cancel: forget the selection and a prefix key."
  (setf (editor-mark-active editor) nil
        (editor-prefix editor) nil)
  (set-selection editor (head editor)))

;;; ------------------------------------------------------------------
;;; Typing

(defun next-char (editor) (char-at (doc editor) (head editor)))
(defun previous-char (editor)
  (let ((p (previous-pos (doc editor) (head editor)))) (and p (char-at (doc editor) p))))

(defun type-text (editor text)
  "Insert TEXT as typed by the user: it replaces the selection, and a bracket
or quote typed alone gets its closing one (EDITOR-AUTO-PAIR)."
  (when (editor-read-only editor)
    (setf (editor-message editor) "Read-only")
    (return-from type-text nil))
  (setf (editor-message editor) nil (editor-goal-column editor) nil)
  (let* ((pairs (language-pairs (editor-language editor)))
         (char (and (= (length text) 1) (char text 0)))
         (open (and char (editor-auto-pair editor) (assoc char pairs)))
         (closes (and char (editor-auto-pair editor) (rassoc char pairs))))
    (with-undo-group (editor :type)
      (cond
        ;; typing the closing character that is already there moves over it
        ((and closes (selection-empty-p editor) (eql (next-char editor) char)
              (or (not (eql char #\"))
                  (equal (char-class editor (head editor)) "string")))
         (move-cursor editor (next-pos (doc editor) (head editor))))
        ;; around the selection
        ((and open (not (selection-empty-p editor)))
         (let* ((start (selection-start editor))
                (inner (selected-text editor))
                (end (replace-range editor start (selection-end editor)
                                    (concatenate 'string (string char) inner (string (cdr open))))))
           (set-selection editor (next-pos (doc editor) start) (previous-pos (doc editor) end))))
        ((and open
              (let ((next (next-char editor)))
                (or (null next) (member next '(#\Space #\Tab #\Newline #\) #\] #\}))))
              ;; not a quote that closes a string, nor one after a backslash or a word
              (not (and (eql char #\") (equal (char-class editor (head editor)) "string")))
              (not (eql (previous-char editor) #\\))
              (not (and (eql char #\") (word-p editor (previous-char editor)))))
         (let ((after (replace-selection editor (concatenate 'string (string char) (string (cdr open))))))
           (move-cursor editor (previous-pos (doc editor) after))))
        (t (replace-selection editor text))))
    (setf (editor-mark-active editor) nil)
    (setf (last-command editor) "TYPE-TEXT")
    t))

(defun indent-line (editor line)
  "Indent LINE as the language says; return true if it changed."
  (let ((column (compute-indentation (editor-language editor) editor line)))
    (when column
      (let ((current (line-indentation editor line))
            (s (line-string (doc editor) line)))
        (unless (and (= current column) (every (lambda (c) (char= c #\Space)) (subseq s 0 current)))
          (let* ((head (head editor)) (anchor (editor-anchor editor)))
            (replace-range editor (make-pos line 0) (make-pos line current)
                           (make-string column :initial-element #\Space))
            (flet ((adjust (p)
                     (if (= (pos-line p) line)
                         (make-pos line (max column (+ (pos-col p) (- column current))))
                         p)))
              (set-selection editor (adjust anchor) (adjust head))))
          t)))))

(define-editor-command newline (editor :edit t)
  "Insert a newline and indent the new line."
  (let ((between (and (selection-empty-p editor)
                      (member (previous-char editor) '(#\{ #\[))
                      (member (next-char editor) '(#\} #\])))))
    (replace-selection editor (string #\Newline))
    (let ((line (pos-line (head editor))))
      ;; the blanks left at the end of the line before
      (let* ((before (line-string (doc editor) (1- line)))
             (trimmed (string-right-trim " 	" before)))
        (when (and (< (length trimmed) (length before)) (plusp (length trimmed)))
          (delete-range editor (make-pos (1- line) (length trimmed)) (make-pos (1- line) (length before)))))
      (when between
        ;; {|} becomes three lines, the cursor in the middle one
        (insert-text editor (head editor) (string #\Newline))
        (indent-line editor (1+ line))
        (set-selection editor (make-pos line 0)))
      (indent-line editor line))))

(define-editor-command open-line (editor :edit t)
  "Insert a newline after the cursor."
  (let ((here (head editor)))
    (replace-selection editor (string #\Newline))
    (set-selection editor here)))

(define-editor-command delete-backward-char (editor :edit t :undo-kind :delete)
  "Delete the selection, or the character before the cursor."
  (cond ((not (selection-empty-p editor)) (replace-selection editor ""))
        ((previous-pos (doc editor) (head editor))
         (let* ((head (head editor))
                (prev (previous-pos (doc editor) head))
                (pc (char-at (doc editor) prev))
                (pair (assoc pc (language-pairs (editor-language editor)))))
           ;; an empty pair goes at once
           (if (and pair (editor-auto-pair editor) (eql (next-char editor) (cdr pair)))
               (delete-range editor prev (next-pos (doc editor) head))
               (delete-range editor prev head))
           (move-cursor editor prev)))))

(define-editor-command delete-forward-char (editor :edit t :undo-kind :delete)
  "Delete the selection, or the character after the cursor."
  (cond ((not (selection-empty-p editor)) (replace-selection editor ""))
        ((next-pos (doc editor) (head editor))
         (delete-range editor (head editor) (next-pos (doc editor) (head editor))))))

(define-editor-command delete-word-backward (editor :edit t :undo-kind :delete)
  "Delete to the start of the word."
  (if (selection-empty-p editor)
      (let ((start (backward-word-pos editor (head editor))))
        (delete-range editor start (head editor))
        (move-cursor editor start))
      (replace-selection editor "")))

(define-editor-command delete-word-forward (editor :edit t :undo-kind :delete)
  "Delete to the end of the word."
  (if (selection-empty-p editor)
      (delete-range editor (head editor) (forward-word-pos editor (head editor)))
      (replace-selection editor "")))

(defun add-to-kill-ring (editor text &key append)
  (if (and append *kill-ring*)
      (setf (first *kill-ring*) (concatenate 'string (first *kill-ring*) text))
      (progn (push text *kill-ring*)
             (when (> (length *kill-ring*) *kill-ring-max*)
               (setf *kill-ring* (subseq *kill-ring* 0 *kill-ring-max*)))))
  (let ((write (editor-property editor :clipboard-write)))
    (when write (funcall write (first *kill-ring*)))))

(define-editor-command kill-line (editor :edit t)
  "Delete to the end of the line (the newline, at the end), and keep it in the kill ring."
  (let* ((head (head editor))
         (end (if (= (pos-col head) (line-length (doc editor) (pos-line head)))
                  (or (next-pos (doc editor) head) head)
                  (make-pos (pos-line head) (line-length (doc editor) (pos-line head))))))
    (unless (pos= head end)
      (add-to-kill-ring editor (text-range (doc editor) head end)
                        :append (equal (last-command editor) "KILL-LINE"))
      (delete-range editor head end)
      (move-cursor editor head))))

(define-editor-command kill-region (editor :edit t)
  "Cut the selection to the kill ring."
  (unless (selection-empty-p editor)
    (add-to-kill-ring editor (selected-text editor))
    (replace-selection editor "")))

(define-editor-command copy-region (editor)
  "Copy the selection to the kill ring."
  (unless (selection-empty-p editor)
    (add-to-kill-ring editor (selected-text editor))
    (setf (editor-mark-active editor) nil)
    (set-selection editor (head editor))))

(define-editor-command yank (editor :edit t)
  "Insert the text killed last."
  (when *kill-ring*
    (replace-selection editor (first *kill-ring*))))

(defun selected-lines (editor)
  "The first and last line of the selection (the last one not counted if
the selection ends at its start)."
  (let ((start (selection-start editor)) (end (selection-end editor)))
    (values (pos-line start)
            (if (and (> (pos-line end) (pos-line start)) (zerop (pos-col end)))
                (1- (pos-line end))
                (pos-line end)))))

(define-editor-command indent (editor :edit t)
  "Indent the line, or the selected lines, as the language says; in plain
text, insert spaces up to the next tab stop."
  (multiple-value-bind (first last) (selected-lines editor)
    (cond ((typep (editor-language editor) 'plain-text)
           (if (selection-empty-p editor)
               (let* ((w (editor-tab-width editor))
                      (col (pos-col (head editor))))
                 (replace-selection editor (make-string (- w (mod col w)) :initial-element #\Space)))
               (loop for line from first to last
                     do (insert-text editor (make-pos line 0)
                                     (make-string (editor-tab-width editor) :initial-element #\Space)))))
          (t
           (loop for line from first to last do (indent-line editor line))
           (let ((head (head editor)))
             (when (and (selection-empty-p editor)
                        (< (pos-col head) (line-indentation editor (pos-line head))))
               (move-cursor editor (make-pos (pos-line head) (line-indentation editor (pos-line head))))))))))

(define-editor-command dedent (editor :edit t)
  "Remove one level of indentation from the line or the selected lines."
  (multiple-value-bind (first last) (selected-lines editor)
    (loop for line from first to last
          for n = (min (editor-tab-width editor) (line-indentation editor line))
          do (when (plusp n) (delete-range editor (make-pos line 0) (make-pos line n))))))

(define-editor-command toggle-comment (editor :edit t)
  "Comment out the line or the selected lines, or uncomment them."
  (let ((start (language-comment-start (editor-language editor))))
    (when start
      (let ((bare (string-right-trim " " start)))
        (multiple-value-bind (first last) (selected-lines editor)
          (let* ((lines (loop for l from first to last
                              unless (= (line-indentation editor l) (line-length (doc editor) l))
                                collect l))
                 (commented (and lines
                                 (every (lambda (l)
                                          (let ((s (line-string (doc editor) l))
                                                (i (line-indentation editor l)))
                                            (string= bare s :start2 i :end2 (min (length s) (+ i (length bare))))))
                                        lines)))
                 (column (and lines (reduce #'min lines :key (lambda (l) (line-indentation editor l))))))
            (dolist (l lines)
              (let ((i (line-indentation editor l)) (s (line-string (doc editor) l)))
                (if commented
                    (let ((n (if (and (< (+ i (length start)) (1+ (length s)))
                                      (string= start s :start2 i :end2 (min (length s) (+ i (length start)))))
                                 (length start)
                                 (length bare))))
                      (delete-range editor (make-pos l i) (make-pos l (min (length s) (+ i n)))))
                    (insert-text editor (make-pos l column) start))))))))))

(defun swap-lines (editor first last direction)
  "Move the lines FIRST to LAST one line in DIRECTION (-1 or 1)."
  (let* ((doc (doc editor))
         (block (text-range doc (make-pos first 0) (make-pos last (line-length doc last))))
         (anchor (editor-anchor editor)) (head (head editor)))
    (if (minusp direction)
        (let ((above (line-string doc (1- first))))
          (replace-range editor (make-pos (1- first) 0) (make-pos last (line-length doc last))
                         (format nil "~A~%~A" block above)))
        (let ((below (line-string doc (1+ last))))
          (replace-range editor (make-pos first 0) (make-pos (1+ last) (line-length doc (1+ last)))
                         (format nil "~A~%~A" below block))))
    (flet ((move (p) (make-pos (+ (pos-line p) direction) (pos-col p))))
      (set-selection editor (move anchor) (move head)))))

(define-editor-command move-line-up (editor :edit t)
  "Move the line, or the selected lines, up."
  (multiple-value-bind (first last) (selected-lines editor)
    (when (plusp first) (swap-lines editor first last -1))))

(define-editor-command move-line-down (editor :edit t)
  "Move the line, or the selected lines, down."
  (multiple-value-bind (first last) (selected-lines editor)
    (when (< last (1- (line-count (doc editor)))) (swap-lines editor first last 1))))

(define-editor-command duplicate-line (editor :edit t)
  "Copy the line, or the selected lines, below them."
  (multiple-value-bind (first last) (selected-lines editor)
    (let* ((doc (doc editor))
           (text (text-range doc (make-pos first 0) (make-pos last (line-length doc last))))
           (anchor (editor-anchor editor)) (head (head editor))
           (n (1+ (- last first))))
      (insert-text editor (make-pos last (line-length doc last)) (format nil "~%~A" text))
      (flet ((down (p) (make-pos (+ (pos-line p) n) (pos-col p))))
        (set-selection editor (down anchor) (down head))))))

(define-editor-command undo-command (editor)
  "Undo the last change."
  (unless (undo editor) (setf (editor-message editor) "Nothing to undo")))

(define-editor-command redo-command (editor)
  "Redo the change undone last."
  (unless (redo editor) (setf (editor-message editor) "Nothing to redo")))

(define-editor-command save (editor)
  "Save the text: call the :SAVE property of the editor."
  (let ((save (editor-property editor :save)))
    (if save
        (funcall save editor)
        (setf (editor-message editor) "No place to save to"))))

;;; ------------------------------------------------------------------
;;; Search

(defun smart-case-test (string)
  (if (some #'upper-case-p string) #'char= #'char-equal))

(defun search-forward (editor string &key (from (selection-end editor)) (wrap t))
  "The start and end of the first STRING after FROM (case-insensitive if
STRING has no upper-case letter), or NIL."
  (when (plusp (length string))
    (let* ((doc (doc editor))
           (text (document-string doc))
           (test (smart-case-test string))
           (start (pos-offset doc from))
           (found (or (search string text :start2 (min start (length text)) :test test)
                      (and wrap (search string text :test test)))))
      (when found
        (values (offset-pos doc found) (offset-pos doc (+ found (length string))))))))

(defun search-backward (editor string &key (from (selection-start editor)) (wrap t))
  (when (plusp (length string))
    (let* ((doc (doc editor))
           (text (document-string doc))
           (test (smart-case-test string))
           (end (pos-offset doc from))
           (found (or (search string text :end2 end :from-end t :test test)
                      (and wrap (search string text :from-end t :test test)))))
      (when found
        (values (offset-pos doc found) (offset-pos doc (+ found (length string))))))))

;;; ------------------------------------------------------------------
;;; The keymaps

(defvar *standard-keymap* (make-keymap :name "standard")
  "The keys of most editors: arrows, Shift to select, Mod-Z to undo...")

(defvar *emacs-keymap* (make-keymap :name "emacs" :parent *standard-keymap*)
  "Emacs's keys, over the standard ones.")

(loop for (key command) on
      '("Left" backward-char "Right" forward-char "Up" previous-line "Down" next-line
        "S-Left" select-backward-char "S-Right" select-forward-char
        "S-Up" select-previous-line "S-Down" select-next-line
        "Home" beginning-of-line "End" end-of-line
        "S-Home" select-beginning-of-line "S-End" select-end-of-line
        "PageUp" page-up "PageDown" page-down "S-PageUp" select-page-up "S-PageDown" select-page-down
        "C-Home" beginning-of-buffer "C-End" end-of-buffer
        "C-S-Home" select-beginning-of-buffer "C-S-End" select-end-of-buffer
        "C-Left" backward-word "C-Right" forward-word
        "C-S-Left" select-backward-word "C-S-Right" select-forward-word
        "M-Left" backward-word "M-Right" forward-word
        "M-S-Left" select-backward-word "M-S-Right" select-forward-word
        "s-Left" beginning-of-line "s-Right" end-of-line
        "s-S-Left" select-beginning-of-line "s-S-Right" select-end-of-line
        "s-Up" beginning-of-buffer "s-Down" end-of-buffer
        "s-S-Up" select-beginning-of-buffer "s-S-Down" select-end-of-buffer
        "C-M-Left" backward-sexp "C-M-Right" forward-sexp
        "C-M-S-Left" select-backward-sexp "C-M-S-Right" select-forward-sexp
        "M-Up" move-line-up "M-Down" move-line-down "M-S-Down" duplicate-line
        "Return" newline "S-Return" newline "Backspace" delete-backward-char
        "S-Backspace" delete-backward-char "Delete" delete-forward-char
        "C-Backspace" delete-word-backward "M-Backspace" delete-word-backward
        "C-Delete" delete-word-forward "M-Delete" delete-word-forward
        "Tab" indent "S-Tab" dedent "Escape" keyboard-quit
        "Mod-a" select-all "Mod-z" undo-command "Mod-S-z" redo-command "Mod-y" redo-command
        "Mod-/" toggle-comment "Mod-s" save "Mod-]" indent "Mod-[" dedent)
      by #'cddr
      do (define-key *standard-keymap* key command))

(loop for (key command) on
      '("C-f" forward-char "C-b" backward-char "C-n" next-line "C-p" previous-line
        "C-a" line-start "C-e" end-of-line "M-f" forward-word "M-b" backward-word
        "M-<" beginning-of-buffer "M->" end-of-buffer "C-v" page-down "M-v" page-up
        "C-d" delete-forward-char "M-d" delete-word-forward "M-Backspace" delete-word-backward
        "C-h" delete-backward-char
        "C-k" kill-line "C-w" kill-region "M-w" copy-region "C-y" yank
        "C-Space" set-mark "C-@" set-mark "C-g" keyboard-quit "C-o" open-line
        "C-/" undo-command "C-_" undo-command "C-S-/" redo-command "C-?" redo-command
        "C-M-f" forward-sexp "C-M-b" backward-sexp "C-M-u" up-list
        "C-M-S-f" select-forward-sexp "C-M-S-b" select-backward-sexp
        "C-j" newline "C-m" newline "C-i" indent "M-;" toggle-comment
        "C-x C-s" save "C-x h" select-all "C-x C-x" exchange-point-and-mark
        "C-x u" undo-command)
      by #'cddr
      do (define-key *emacs-keymap* key command))
