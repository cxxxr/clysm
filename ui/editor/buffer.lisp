;;;; buffer.lisp — the text of the editor component, and its undo history
;;;;
;;;; This file and syntax.lisp and commands.lisp are portable Common Lisp:
;;;; they know nothing of the page, and run (and are tested) on any
;;;; implementation.  view.lisp shows an editor on the page.
;;;;
;;;; A document is a vector of lines (strings without newlines).  A position
;;;; is a line and a column (a character index in the line).  Every change
;;;; of the text goes through APPLY-CHANGE, which records it for undo and
;;;; tells the syntax cache which lines changed.

(defpackage "EDITOR"
  (:use "CL")
  (:export
   ;; positions
   "POS" "MAKE-POS" "POS-LINE" "POS-COL" "POS=" "POS<" "POS<=" "POS-MIN" "POS-MAX"
   ;; documents
   "DOCUMENT" "MAKE-DOCUMENT" "DOCUMENT-LINES" "LINE-COUNT" "LINE-STRING" "LINE-LENGTH"
   "DOCUMENT-STRING" "TEXT-RANGE" "CLAMP-POS" "DOCUMENT-END" "CHAR-AT"
   "POS-OFFSET" "OFFSET-POS" "NEXT-POS" "PREVIOUS-POS"
   ;; the editor
   "EDITOR" "MAKE-EDITOR" "EDITOR-DOCUMENT" "EDITOR-TEXT" "EDITOR-ANCHOR" "EDITOR-HEAD"
   "EDITOR-LANGUAGE" "EDITOR-VERSION" "EDITOR-READ-ONLY" "EDITOR-TAB-WIDTH"
   "EDITOR-AUTO-PAIR" "EDITOR-MARK-ACTIVE" "EDITOR-CHANGE-HOOKS" "EDITOR-MODIFIED"
   "EDITOR-KEYMAP" "EDITOR-PROPERTIES" "EDITOR-PROPERTY" "MARK-SAVED"
   "SELECTION-START" "SELECTION-END" "SELECTION-EMPTY-P" "SELECTED-TEXT" "SET-SELECTION"
   "MOVE-CURSOR" "INSERT-TEXT" "DELETE-RANGE" "REPLACE-SELECTION" "REPLACE-RANGE"
   "UNDO" "REDO" "CAN-UNDO-P" "CAN-REDO-P" "WITH-UNDO-GROUP"
   ;; syntax.lisp
   "LANGUAGE" "PLAIN-TEXT" "LISP-LANGUAGE" "*LISP-LANGUAGE*" "*PLAIN-TEXT*"
   "LINE-TOKENS" "MATCHING-BRACKET" "FIND-LANGUAGE"
   ;; commands.lisp
   "DEFINE-EDITOR-COMMAND" "FIND-EDITOR-COMMAND" "RUN-COMMAND" "EDITOR-COMMANDS"
   "MAKE-KEYMAP" "DEFINE-KEY" "LOOKUP-KEY" "*STANDARD-KEYMAP*" "*EMACS-KEYMAP*"
   "*MAC-P*" "HANDLE-KEY" "TYPE-TEXT" "SEARCH-FORWARD" "SEARCH-BACKWARD"
   "INDENT-LINE" "LINE-INDENTATION" "*KILL-RING*" "EDITOR-MESSAGE"))

(in-package "EDITOR")

;;; ------------------------------------------------------------------
;;; Positions

(defstruct (pos (:constructor make-pos (line col)))
  (line 0 :type fixnum)
  (col 0 :type fixnum))

(defun pos= (a b) (and (= (pos-line a) (pos-line b)) (= (pos-col a) (pos-col b))))
(defun pos< (a b)
  (or (< (pos-line a) (pos-line b))
      (and (= (pos-line a) (pos-line b)) (< (pos-col a) (pos-col b)))))
(defun pos<= (a b) (not (pos< b a)))
(defun pos-min (a b) (if (pos< b a) b a))
(defun pos-max (a b) (if (pos< a b) b a))

;;; ------------------------------------------------------------------
;;; Documents

(defun split-lines (string)
  "The lines of STRING, a list of fresh strings (CRLF and CR count as newlines)."
  (let ((lines '()) (start 0) (n (length string)))
    (loop for i from 0 below n
          for c = (char string i)
          do (cond ((char= c #\Newline)
                    (push (subseq string start (if (and (> i start) (char= (char string (1- i)) #\Return))
                                                   (1- i) i))
                          lines)
                    (setf start (1+ i)))
                   ((and (char= c #\Return) (or (= (1+ i) n) (char/= (char string (1+ i)) #\Newline)))
                    (push (subseq string start i) lines)
                    (setf start (1+ i)))))
    (push (subseq string start) lines)
    (nreverse lines)))

(defclass document ()
  ((lines :initarg :lines :accessor document-lines))
  (:documentation "The text: a vector of lines, never empty."))

(defun make-lines-vector (list)
  (let ((v (make-array (length list) :adjustable t :fill-pointer (length list))))
    (replace v list)
    v))

(defun make-document (&optional (text ""))
  (make-instance 'document :lines (make-lines-vector (split-lines text))))

(defun line-count (doc) (length (document-lines doc)))
(defun line-string (doc line) (aref (document-lines doc) line))
(defun line-length (doc line) (length (line-string doc line)))

(defun document-string (doc)
  (let ((lines (document-lines doc)))
    (with-output-to-string (out)
      (loop for i from 0 below (length lines)
            do (when (plusp i) (write-char #\Newline out))
               (write-string (aref lines i) out)))))

(defun document-end (doc)
  (let ((last (1- (line-count doc))))
    (make-pos last (line-length doc last))))

(defun clamp-pos (doc pos)
  (let* ((line (max 0 (min (pos-line pos) (1- (line-count doc)))))
         (col (max 0 (min (pos-col pos) (line-length doc line)))))
    (if (and (= line (pos-line pos)) (= col (pos-col pos))) pos (make-pos line col))))

(defun char-at (doc pos)
  "The character at POS (#\\Newline at the end of a line), or NIL at the end."
  (let ((s (line-string doc (pos-line pos))))
    (cond ((< (pos-col pos) (length s)) (char s (pos-col pos)))
          ((< (pos-line pos) (1- (line-count doc))) #\Newline)
          (t nil))))

(defun next-pos (doc pos)
  "The position after the character at POS, or NIL at the end."
  (cond ((< (pos-col pos) (line-length doc (pos-line pos)))
         (make-pos (pos-line pos) (1+ (pos-col pos))))
        ((< (pos-line pos) (1- (line-count doc))) (make-pos (1+ (pos-line pos)) 0))
        (t nil)))

(defun previous-pos (doc pos)
  "The position before POS, or NIL at the start."
  (cond ((plusp (pos-col pos)) (make-pos (pos-line pos) (1- (pos-col pos))))
        ((plusp (pos-line pos))
         (let ((line (1- (pos-line pos)))) (make-pos line (line-length doc line))))
        (t nil)))

(defun pos-offset (doc pos)
  "The index of POS in the text of DOC (a newline counts as one character)."
  (+ (loop for i from 0 below (pos-line pos) sum (1+ (line-length doc i))) (pos-col pos)))

(defun offset-pos (doc offset)
  (loop for line from 0 below (line-count doc)
        for len = (line-length doc line)
        do (if (<= offset len)
               (return (make-pos line offset))
               (decf offset (1+ len)))
        finally (return (document-end doc))))

(defun text-range (doc start end)
  "The text from START to END."
  (let ((l1 (pos-line start)) (l2 (pos-line end)))
    (if (= l1 l2)
        (subseq (line-string doc l1) (pos-col start) (pos-col end))
        (with-output-to-string (out)
          (write-string (line-string doc l1) out :start (pos-col start))
          (loop for l from (1+ l1) below l2
                do (write-char #\Newline out) (write-string (line-string doc l) out))
          (write-char #\Newline out)
          (write-string (line-string doc l2) out :end (pos-col end))))))

(defun %delete (doc start end)
  "Delete the text from START to END."
  (let* ((lines (document-lines doc))
         (l1 (pos-line start)) (l2 (pos-line end))
         (joined (concatenate 'string
                              (subseq (aref lines l1) 0 (pos-col start))
                              (subseq (aref lines l2) (pos-col end))))
         (removed (- l2 l1)))
    (setf (aref lines l1) joined)
    (when (plusp removed)
      (replace lines lines :start1 (1+ l1) :start2 (1+ l2))
      (setf (fill-pointer lines) (- (length lines) removed)))))

(defun %insert (doc pos string)
  "Insert STRING at POS; return the position after it."
  (let* ((lines (document-lines doc))
         (new (split-lines string))
         (line (aref lines (pos-line pos)))
         (before (subseq line 0 (pos-col pos)))
         (after (subseq line (pos-col pos))))
    (if (null (cdr new))
        (progn (setf (aref lines (pos-line pos)) (concatenate 'string before (car new) after))
               (make-pos (pos-line pos) (+ (pos-col pos) (length (car new)))))
        (let* ((added (1- (length new)))
               (old-length (length lines))
               (last (car (last new))))
          (dotimes (i added) (vector-push-extend "" lines))
          (replace lines lines :start1 (+ (pos-line pos) 1 added) :start2 (1+ (pos-line pos))
                               :end2 old-length)
          (setf (aref lines (pos-line pos)) (concatenate 'string before (car new)))
          (loop for s in (butlast (cdr new))
                for i from (1+ (pos-line pos))
                do (setf (aref lines i) s))
          (setf (aref lines (+ (pos-line pos) added)) (concatenate 'string last after))
          (make-pos (+ (pos-line pos) added) (length last))))))

;;; ------------------------------------------------------------------
;;; The editor: a document, a selection and an undo history

(defclass editor ()
  ((document :initarg :document :accessor editor-document)
   ;; the selection: from ANCHOR to HEAD; HEAD is the cursor
   (anchor :initform (make-pos 0 0) :accessor editor-anchor)
   (head :initform (make-pos 0 0) :accessor editor-head)
   ;; the column that vertical motion keeps, or NIL
   (goal-column :initform nil :accessor editor-goal-column)
   ;; Emacs's mark: motion extends the selection while it is active
   (mark-active :initform nil :accessor editor-mark-active)
   (language :initarg :language :accessor editor-language)
   (read-only :initarg :read-only :initform nil :accessor editor-read-only)
   (tab-width :initarg :tab-width :initform 2 :accessor editor-tab-width)
   ;; insert the closing bracket or quote with the opening one
   (auto-pair :initarg :auto-pair :initform t :accessor editor-auto-pair)
   (keymap :initarg :keymap :initform nil :accessor editor-keymap)
   ;; incremented by every change of the text or of the selection
   (version :initform 0 :accessor editor-version)
   ;; (lambda (editor)) called after each change of the text
   (change-hooks :initform '() :accessor editor-change-hooks)
   ;; undo: lists of groups; a group is (changes selection-before selection-after kind time)
   (undo-stack :initform '() :accessor undo-stack)
   (redo-stack :initform '() :accessor redo-stack)
   (current-group :initform nil :accessor current-group)
   (last-kind :initform nil :accessor last-kind)
   ;; the undo group that is saved, to know whether the text is modified
   (saved-group :initform nil :accessor saved-group)
   ;; the first line whose syntax state may be stale (syntax.lisp)
   (syntax-valid :initform 0 :accessor syntax-valid)
   (syntax-states :initform (make-array 1 :adjustable t :fill-pointer 1 :initial-element nil)
                  :accessor syntax-states)
   (token-cache :initform (make-hash-table :test 'eq) :accessor token-cache)
   ;; the last command run, and a message for the user (commands.lisp)
   (last-command :initform nil :accessor last-command)
   (message :initform nil :accessor editor-message)
   ;; a pending prefix keymap (C-x ...)
   (prefix :initform nil :accessor editor-prefix)
   (properties :initform '() :accessor editor-properties))
  (:documentation "The state of an editor: its text, selection, undo history and options."))

(defun make-editor (&key (text "") (language nil) (keymap nil) (read-only nil)
                         (tab-width 2) (auto-pair t))
  (make-instance 'editor :document (make-document text)
                         :language (or language (symbol-value (find-symbol "*PLAIN-TEXT*" "EDITOR")))
                         :keymap keymap :read-only read-only :tab-width tab-width
                         :auto-pair auto-pair))

(defun editor-property (editor key &optional default)
  (getf (editor-properties editor) key default))
(defun (setf editor-property) (value editor key &optional default)
  (declare (ignore default))
  (setf (getf (editor-properties editor) key) value))

(defun editor-text (editor) (document-string (editor-document editor)))

(defun (setf editor-text) (text editor)
  "Replace the whole text; the undo history is forgotten."
  (setf (editor-document editor) (make-document text)
        (editor-anchor editor) (make-pos 0 0)
        (editor-head editor) (make-pos 0 0)
        (undo-stack editor) '() (redo-stack editor) '()
        (saved-group editor) nil
        (current-group editor) nil
        (editor-mark-active editor) nil)
  (syntax-changed editor 0)
  (clrhash (token-cache editor))
  (incf (editor-version editor))
  (run-change-hooks editor)
  text)

(defun run-change-hooks (editor)
  (dolist (hook (editor-change-hooks editor)) (funcall hook editor)))

(defun selection-start (editor) (pos-min (editor-anchor editor) (editor-head editor)))
(defun selection-end (editor) (pos-max (editor-anchor editor) (editor-head editor)))
(defun selection-empty-p (editor) (pos= (editor-anchor editor) (editor-head editor)))

(defun selected-text (editor)
  (text-range (editor-document editor) (selection-start editor) (selection-end editor)))

(defun set-selection (editor anchor &optional (head anchor))
  (let ((doc (editor-document editor)))
    (setf (editor-anchor editor) (clamp-pos doc anchor)
          (editor-head editor) (clamp-pos doc head))
    (incf (editor-version editor))))

(defun move-cursor (editor pos &key extend)
  "Move the cursor to POS; with EXTEND (or an active mark) the anchor stays."
  (let ((pos (clamp-pos (editor-document editor) pos)))
    (if (or extend (editor-mark-active editor))
        (setf (editor-head editor) pos)
        (setf (editor-anchor editor) pos (editor-head editor) pos))
    (incf (editor-version editor))
    pos))

;;; ------------------------------------------------------------------
;;; Changes and undo
;;;
;;; A change is (start deleted inserted): the text DELETED at START was
;;; replaced by INSERTED.  A group is the changes of one command.

(defgeneric syntax-changed (editor line)
  (:documentation "The text of LINE and the lines after it may have changed.")
  (:method (editor line)
    (setf (syntax-valid editor) (min (syntax-valid editor) line))))

(defun end-of-text (start text)
  "The position after TEXT inserted at START."
  (let ((newlines (count #\Newline text)))
    (if (zerop newlines)
        (make-pos (pos-line start) (+ (pos-col start) (length text)))
        (make-pos (+ (pos-line start) newlines)
                  (- (length text) (1+ (position #\Newline text :from-end t)))))))

(defun map-pos (pos start old-end new-end)
  "Where POS is after the text from START to OLD-END became that from START to NEW-END."
  (cond ((pos<= pos start) pos)
        ((pos< pos old-end) new-end)
        ((= (pos-line pos) (pos-line old-end))
         (make-pos (pos-line new-end) (+ (pos-col new-end) (- (pos-col pos) (pos-col old-end)))))
        (t (make-pos (+ (pos-line pos) (- (pos-line new-end) (pos-line old-end))) (pos-col pos)))))

(defun %apply (editor start end text)
  "Replace the text from START to END by TEXT; return the end of the new text.
The selection follows the text around it."
  (let ((doc (editor-document editor)))
    (unless (pos= start end) (%delete doc start end))
    (let ((new-end (if (zerop (length text)) start (%insert doc start text))))
      (setf (editor-anchor editor) (map-pos (editor-anchor editor) start end new-end)
            (editor-head editor) (map-pos (editor-head editor) start end new-end))
      (syntax-changed editor (pos-line start))
      (incf (editor-version editor))
      new-end)))

(defun make-group (editor kind)
  (list '() (cons (editor-anchor editor) (editor-head editor)) nil kind (get-universal-time)))

(defmacro with-undo-group ((editor &optional kind) &body body)
  "Make the changes of BODY one step of undo.  Steps of the same KIND (a
symbol, such as :TYPE) that follow each other are merged."
  (let ((e (gensym)) (outer (gensym)))
    `(let* ((,e ,editor) (,outer (current-group ,e)))
       (if ,outer
           (progn ,@body)
           (progn
             (setf (current-group ,e) (make-group ,e ,kind))
             (unwind-protect (progn ,@body)
               (close-group ,e)))))))

(defun close-group (editor)
  (let ((group (shiftf (current-group editor) nil)))
    (when (first group)
      (setf (third group) (cons (editor-anchor editor) (editor-head editor)))
      (let ((top (first (undo-stack editor)))
            (kind (fourth group)))
        (if (and top kind (eq kind (fourth top)) (eq (last-kind editor) kind)
                 (not (eq top (saved-group editor)))
                 (<= (- (fifth group) (fifth top)) 2))
            ;; merge with the step before
            (setf (first top) (append (first group) (first top))
                  (third top) (third group)
                  (fifth top) (fifth group))
            (push group (undo-stack editor))))
      (setf (redo-stack editor) '())
      (run-change-hooks editor))
    ;; a step without changes (typing over a closing bracket) does not end
    ;; the run of steps of its kind
    (setf (last-kind editor) (if (or (first group) (null (fourth group)))
                                 (fourth group)
                                 (and (eq (fourth group) (last-kind editor)) (fourth group))))))

(defun replace-range (editor start end text)
  "Replace the text from START to END by TEXT, as a change that can be
undone; return the end of the new text."
  (let* ((doc (editor-document editor))
         (start (clamp-pos doc start))
         (end (clamp-pos doc end)))
    (when (pos< end start) (rotatef start end))
    (if (and (pos= start end) (zerop (length text)))
        start
        (with-undo-group (editor)
          (let ((deleted (text-range doc start end)))
            (push (list start deleted text) (first (current-group editor)))
            (%apply editor start end text))))))

(defun insert-text (editor pos text) (replace-range editor pos pos text))
(defun delete-range (editor start end) (replace-range editor start end ""))

(defun replace-selection (editor text)
  "Replace the selection by TEXT and put the cursor after it."
  (let ((end (replace-range editor (selection-start editor) (selection-end editor) text)))
    (setf (editor-mark-active editor) nil)
    (set-selection editor end)
    end))

(defun undo-group (editor group inverse)
  "Undo (or with INVERSE false, redo) the changes of GROUP."
  (let ((changes (if inverse (first group) (reverse (first group)))))
    (dolist (change changes)
      (destructuring-bind (start deleted inserted) change
        (if inverse
            (%apply editor start (end-of-text start inserted) deleted)
            (%apply editor start (end-of-text start deleted) inserted))))
    (let ((selection (if inverse (second group) (third group))))
      (set-selection editor (car selection) (cdr selection)))
    (setf (editor-mark-active editor) nil
          (last-kind editor) nil)
    (run-change-hooks editor)))

(defun can-undo-p (editor) (and (undo-stack editor) t))
(defun can-redo-p (editor) (and (redo-stack editor) t))

(defun undo (editor)
  (let ((group (pop (undo-stack editor))))
    (when group
      (undo-group editor group t)
      (push group (redo-stack editor))
      t)))

(defun redo (editor)
  (let ((group (pop (redo-stack editor))))
    (when group
      (undo-group editor group nil)
      (push group (undo-stack editor))
      t)))

(defun mark-saved (editor)
  "The text is now saved: EDITOR-MODIFIED is false until it changes."
  (setf (saved-group editor) (first (undo-stack editor))
        (last-kind editor) nil))

(defun editor-modified (editor)
  (not (eq (saved-group editor) (first (undo-stack editor)))))
