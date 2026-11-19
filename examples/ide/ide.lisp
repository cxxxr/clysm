;;;; ide.lisp — Clysm IDE: a development environment for Common Lisp,
;;;; written in Common Lisp
;;;;
;;;; The editor is the pane EDITOR:CODE-EDITOR (ui/editor/), the screen is
;;;; made with the UI framework (ui/), and the Lisp is the IDE's own image
;;;; (lisp.lisp).  The parts:
;;;;
;;;;   the sidebar   the files of the project, and the definitions of the
;;;;                 current file
;;;;   the editor    a tab for each open file; keys evaluate, complete,
;;;;                 show argument lists and go to definitions
;;;;   the REPL      the log of evaluations (values can be inspected) and an
;;;;                 input; the debugger is shown above it when an error waits
;;;;                 for a restart
;;;;
;;;; In the desktop application (Electron, with Node.js in the page) the
;;;; project is a folder of the computer.  In a browser it is kept in
;;;; localStorage.

(in-package "CLYSM-IDE")

;;; ------------------------------------------------------------------
;;; Settings (kept in localStorage, in both)

(defun storage () (ignore-errors (js:global "localStorage")))

(defun setting (name &optional default)
  (let* ((store (storage))
         (value (and store (js:js-value-p store)
                     (ignore-errors (js:call store "getItem" (concatenate 'string "clysm-ide:" name))))))
    (if (stringp value) value default)))

(defun (setf setting) (value name)
  (let ((store (storage)))
    (when (and store (js:js-value-p store))
      (ignore-errors (js:call store "setItem" (concatenate 'string "clysm-ide:" name) value))))
  value)

(defstate *key-style* (if (equal (setting "keys") "emacs") :emacs :standard))
(defstate *wrap* (equal (setting "wrap") "t"))

;;; ------------------------------------------------------------------
;;; Files.  A file is named by a string: on the desktop its absolute path,
;;; in a browser its name in the project.

(defstate *root* nil "On the desktop, the folder of the project (ending in /), or NIL.")
(defstate *files* '() "The files of the project (their names), sorted.")

(defparameter +ignored-directories+ '("node_modules" "build" "dist" "target"))
(defparameter +file-limit+ 3000)

(defun virtual-p () (not (desktop-p)))

(defun file-name (path)
  (subseq path (1+ (or (position #\/ path :from-end t) -1))))

(defun relative-name (path)
  "PATH for the eyes: relative to the project if it is in it."
  (let ((root *root*))
    (if (and root (> (length path) (length root)) (string= root path :end2 (length root)))
        (subseq path (length root))
        path)))

(defun file-extension (path)
  (let ((dot (position #\. (file-name path) :from-end t)))
    (and dot (string-downcase (subseq (file-name path) (1+ dot))))))

(defun lisp-file-p (path)
  (member (file-extension path) '("lisp" "lsp" "cl" "asd" "l") :test #'equal))

(defun ignored-path-p (relative)
  (let ((start 0))
    (loop
      (let ((slash (position #\/ relative :start start)))
        (unless slash (return nil))
        (let ((part (subseq relative start slash)))
          (when (or (and (plusp (length part)) (char= (char part 0) #\.))
                    (member part +ignored-directories+ :test #'string=))
            (return t)))
        (setf start (1+ slash))))))

(defun scan-files ()
  "The files of the project."
  (cond ((virtual-p) (virtual-index))
        ((null *root*) '())
        (t (let ((files '()) (n 0))
             (dolist (p (ignore-errors (directory (concatenate 'string *root* "**/*.*"))))
               (let* ((name (namestring p)) (rel (relative-name name)))
                 (unless (or (zerop (length (file-name name)))
                             (char= (char (file-name name) 0) #\.)
                             (ignored-path-p rel)
                             (>= n +file-limit+))
                   (incf n)
                   (push name files))))
             (sort files #'string< :key #'relative-name)))))

(defun refresh-files () (setf *files* (scan-files)))

(defun read-text (path)
  (if (virtual-p)
      (or (setting (concatenate 'string "file:" path)) "")
      (read-file path)))

(defun write-text (path text)
  (if (virtual-p)
      (progn (setf (setting (concatenate 'string "file:" path)) text)
             (unless (member path (virtual-index) :test #'string=)
               (setf (virtual-index) (sort (cons path (virtual-index)) #'string<))))
      (write-file path text)))

(defun virtual-index ()
  (let ((index (setting "index" nil)))
    (if index (split-string index #\Newline) '())))

(defun (setf virtual-index) (names)
  (setf (setting "index") (format nil "~{~A~^~%~}" names))
  (setf *files* names)
  names)

(defun split-string (string char)
  (loop with start = 0
        for end = (position char string :start start)
        collect (subseq string start end) into parts
        do (if end (setf start (1+ end)) (return (remove "" parts :test #'string=)))))

;;; ------------------------------------------------------------------
;;; Buffers: an open file (or a scratch buffer), with its editor pane

(defmodel buffer ()
  ((path :initarg :path :initform nil :accessor buffer-path)
   (name :initarg :name :accessor buffer-name)
   (pane :initform nil :accessor buffer-pane)
   (modified :initform nil :accessor buffer-modified)))

(defstate *buffers* '())
(defstate *current* nil)
(defstate *message* nil "The message of the status line.")
(defstate *eldoc* nil "The argument list of the function around the cursor.")

(declaim (ftype function ide-keymap buffer-changed buffer-cursor-moved save-buffer
                update-eldoc close-completion completion-follow focus-repl open-quick-open
                describe-at-cursor macroexpand-at-cursor cycle-tab repl-keymap busy-p
                refresh-outline-names save-buffer-as))
(defvar *repl-input*)
(defvar *eval-origin* nil "Where the last evaluation was asked: :REPL or a buffer.")
(defvar +scratch+)

(defun buffer-of (thing)
  "The buffer of an editor pane or of a model editor."
  (find-if (lambda (b)
             (let ((pane (buffer-pane b)))
               (or (eq pane thing) (eq (editor:code-editor-editor pane) thing))))
           *buffers*))

(defun make-buffer (name text &key path)
  (let* ((buffer (make-instance 'buffer :path path :name name))
         (pane (make-instance 'editor:code-editor
                              :text text
                              :language (if (or (null path) (lisp-file-p path)) "lisp" "text")
                              :keymap (ide-keymap)
                              :wrap *wrap*
                              :change-callback #'buffer-changed
                              :cursor-callback #'buffer-cursor-moved
                              :save-callback (lambda (pane) (save-buffer (buffer-of pane))))))
    (setf (buffer-pane buffer) pane)
    (setf *buffers* (append *buffers* (list buffer)))
    buffer))

(defun buffer-editor (buffer) (editor:code-editor-editor (buffer-pane buffer)))
(defun buffer-text (buffer) (editor:code-editor-text (buffer-pane buffer)))

(defun buffer-changed (pane)
  (let ((buffer (buffer-of pane)))
    (when buffer
      (setf (buffer-modified buffer) (editor:editor-modified (editor:code-editor-editor pane)))
      ;; a scratch buffer of the browser is kept
      (when (and (virtual-p) (null (buffer-path buffer)))
        (setf (setting "scratch") (editor:code-editor-text pane))))))

(defun remember-open-files ()
  (setf (setting "open") (format nil "~{~A~^~%~}" (remove nil (mapcar #'buffer-path *buffers*)))))

(defun switch-to (buffer)
  (close-completion)
  (setf *current* buffer)
  (when buffer
    (setf (setting "current") (or (buffer-path buffer) ""))
    (remember-open-files)
    (editor:code-editor-focus (buffer-pane buffer))
    (update-eldoc buffer)))

(defun find-file (path)
  "Show the file PATH in a buffer, opening it if needed; return the buffer."
  (let ((buffer (or (find path *buffers* :key #'buffer-path :test #'equal)
                    (let ((text (read-text path)))
                      (make-buffer (file-name path) text :path path)))))
    (switch-to buffer)
    buffer))

(defun scratch-buffer ()
  (or (find nil *buffers* :key #'buffer-path)
      (make-buffer "scratch" (if (virtual-p) (setting "scratch" +scratch+) +scratch+))))

(defun close-buffer (buffer)
  (flet ((close ()
           (let ((position (position buffer *buffers*)))
             (setf *buffers* (remove buffer *buffers*))
             (remember-open-files)
             (when (eq *current* buffer)
               (switch-to (or (nth (min (or position 0) (1- (length *buffers*))) *buffers*)
                              (scratch-buffer)))))))
    (if (and (buffer-modified buffer) (buffer-path buffer))
        (prompt-for-confirmation (format nil "~A は保存されていません。閉じますか？" (buffer-name buffer))
                                 :continuation (lambda (yes ok) (when (and yes ok) (close))))
        (close))))

(defun save-buffer (buffer)
  (when buffer
    (let ((path (buffer-path buffer)))
      (if (null path)
          (save-buffer-as buffer)
          (progn
            (write-text path (buffer-text buffer))
            (editor:mark-saved (buffer-editor buffer))
            (setf (buffer-modified buffer) nil)
            (setf *message* (format nil "~A を保存しました" (relative-name path))))))))

(defun save-buffer-as (buffer)
  (let ((path (if (virtual-p)
                  nil
                  (choose-file-to-save :default-name "untitled.lisp" :filters '(("Lisp" "lisp"))))))
    (flet ((save-to (path)
             (setf (buffer-path buffer) path
                   (buffer-name buffer) (file-name path))
             (save-buffer buffer)
             (refresh-files)))
      (cond (path (save-to path))
            ((virtual-p)
             (prompt-for-string "ファイルの名前" :text "untitled.lisp"
                                :continuation (lambda (name ok)
                                                (when (and ok (plusp (length name)))
                                                  (save-to name)))))))))

;;; ------------------------------------------------------------------
;;; Reading the text around the cursor

(defun cursor (buffer) (editor:editor-head (buffer-editor buffer)))

(defun line-of (editor line) (editor:line-string (editor:editor-document editor) line))

(defun buffer-package (editor &optional (line (editor:pos-line (editor:editor-head editor))))
  "The name of the package of the text at LINE: that of the last IN-PACKAGE before it."
  (loop for i from line downto 0
        do (let ((s (line-of editor i)))
             (when (and (> (length s) 12) (string-equal "(in-package " s :end2 12))
               (let* ((name (string-trim " )'\"#:" (subseq s 12)))
                      (end (position-if (lambda (c) (member c '(#\Space #\) #\"))) name)))
                 (return-from buffer-package (string-upcase (subseq name 0 end)))))))
  "CL-USER")

(defun symbol-at (editor pos)
  "The symbol name around POS, and the columns where it starts and ends."
  (let* ((s (line-of editor (editor:pos-line pos)))
         (col (min (editor:pos-col pos) (length s)))
         (start col) (end col))
    (loop while (and (> start 0) (symbol-char-p (char s (1- start)))) do (decf start))
    (loop while (and (< end (length s)) (symbol-char-p (char s end))) do (incf end))
    (values (subseq s start end) start end)))

(defun toplevel-form (editor)
  "The start and end of the form of the top level at the cursor."
  (let* ((head (editor:editor-head editor))
         (open (loop with last = nil
                     for p = (editor::enclosing-open editor head) then (editor::enclosing-open editor p)
                     while p do (setf last p)
                     finally (return last))))
    (if open
        (values open (or (editor::forward-sexp-pos editor open)
                         (editor:document-end (editor:editor-document editor))))
        ;; between forms: the one just before the cursor on its line, or the next
        (let* ((s (line-of editor (editor:pos-line head)))
               (before (string-right-trim " " (subseq s 0 (editor:pos-col head)))))
          (if (and (plusp (length before)) (char= (char before (1- (length before))) #\)))
              (let ((start (editor::backward-sexp-pos editor head)))
                (values start (editor::forward-sexp-pos editor start)))
              (let ((end (editor::forward-sexp-pos editor head)))
                (and end (values (editor::backward-sexp-pos editor end) end))))))))

(defun last-sexp (editor)
  (let* ((head (editor:editor-head editor))
         (start (editor::backward-sexp-pos editor head)))
    (and start (values start head))))

(defun text-between (editor start end)
  (editor:text-range (editor:editor-document editor) start end))

(defun pos-offset* (editor pos) (editor:pos-offset (editor:editor-document editor) pos))

;;; ------------------------------------------------------------------
;;; Evaluating from the editor

(defun eval-region (buffer start end &key (what "式") label)
  (let* ((editor (buffer-editor buffer))
         (source (text-between editor start end))
         (package (buffer-package editor (editor:pos-line start)))
         (path (or (buffer-path buffer) (buffer-name buffer))))
    (cond
      ((busy-p)
       (setf *message* "評価中です（■ 中断 で止められます）"))
      ((not (complete-input-p source))
       (setf *message* "括弧が閉じていません"))
      (t
        (progn
          (setf *eval-origin* buffer)
          (setf *message* (format nil "~A を評価しています…" what))
          (evaluate-source source
                           :package package
                           :label (or label (one-line source))
                           :offset (pos-offset* editor start)
                           :on-definition (lambda (name kind offset)
                                            (remember-definition name kind path offset))
                           :on-done (lambda (values ok)
                                      (setf *message*
                                            (if ok
                                                (format nil "⇒ ~:[値なし~;~:*~{~A~^, ~}~]"
                                                        (mapcar (lambda (v) (one-line (print-value v package) 80)) values))
                                                "評価をやめました"))
                                      (refresh-outline-names))))))))

(defun busy-p () (and *busy* (null *debuggers*)))

(defun refocus ()
  "Give the keyboard back to where the last evaluation was asked."
  (if (and (buffer-p* *eval-origin*) (member *eval-origin* *buffers*))
      (switch-to *eval-origin*)
      (focus-repl)))

(defun buffer-p* (x) (typep x 'buffer))

(defun eval-toplevel-form (buffer)
  (let ((editor (buffer-editor buffer)))
    (if (not (editor:selection-empty-p editor))
        (eval-region buffer (editor:selection-start editor) (editor:selection-end editor) :what "選択")
        (multiple-value-bind (start end) (toplevel-form editor)
          (if start
              (eval-region buffer start end :what "フォーム")
              (setf *message* "評価する式がありません"))))))

(defun eval-whole-buffer (buffer)
  (let ((editor (buffer-editor buffer)))
    (eval-region buffer (editor:make-pos 0 0) (editor:document-end (editor:editor-document editor))
                 :what (buffer-name buffer) :label (format nil "ファイル ~A" (buffer-name buffer)))))

;;; ------------------------------------------------------------------
;;; The argument list of the function around the cursor

(defun update-eldoc (buffer)
  (setf *eldoc*
        (ignore-errors
         (let* ((editor (buffer-editor buffer))
                (head (editor:editor-head editor))
                (open (editor::enclosing-open editor head)))
           (when (and open (< (- (editor:pos-line head) (editor:pos-line open)) 200))
             (let* ((after (editor:make-pos (editor:pos-line open) (1+ (editor:pos-col open))))
                    (name (symbol-at editor after))
                    (symbol (and (plusp (length name))
                                 (find-symbol* name (package-named (buffer-package editor))))))
               (when (and symbol (or (fboundp symbol) (special-operator-p symbol)))
                 ;; the argument the cursor is at: the forms that end before it
                 (let ((index -1) (p after))
                   (loop
                     (let ((end (editor::forward-sexp-pos editor p)))
                       (when (or (null end) (not (editor:pos< end head))) (return))
                       (incf index)
                       (setf p end)))
                   (arglist-string symbol index)))))))))

(defun buffer-cursor-moved (pane)
  (let ((buffer (buffer-of pane)))
    (when (and buffer (eq buffer *current*))
      (update-eldoc buffer)
      (completion-follow buffer))))

;;; ------------------------------------------------------------------
;;; Completion: a list under the cursor

(defstruct completion buffer items (index 0) line start x y typed)
(defstate *completion* nil)
(defvar *completion-keymap* nil)

(defun completion-prefix (buffer)
  (let ((editor (buffer-editor buffer)))
    (multiple-value-bind (name start) (symbol-at editor (editor:editor-head editor))
      (let ((col (editor:pos-col (editor:editor-head editor))))
        (values (subseq name 0 (- col start)) start)))))

(defun start-completion (buffer)
  (multiple-value-bind (prefix start) (completion-prefix buffer)
    (let* ((editor (buffer-editor buffer))
           (items (completions prefix (package-named (buffer-package editor)))))
      (cond ((null items) (setf *message* (format nil "「~A」の補完はありません" prefix)))
            ((null (rest items)) (insert-completion buffer (first items)))
            (t
             (let ((common (common-prefix items)))
               (when (> (length common) (length prefix))
                 (insert-completion buffer common)
                 (setf prefix common)))
             (multiple-value-bind (x top bottom) (editor:code-editor-cursor-rect (buffer-pane buffer))
               (declare (ignore top))
               (setf *completion* (make-completion :buffer buffer :items items :typed prefix
                                                   :line (editor:pos-line (editor:editor-head editor))
                                                   :start start :x (or x 100) :y (or bottom 100))))
             (setf (editor:code-editor-keymap (buffer-pane buffer)) *completion-keymap*))))))

(defun common-prefix (strings)
  (reduce (lambda (a b) (subseq a 0 (or (mismatch a b :test #'char-equal) (length a)))) strings))

(defun insert-completion (buffer text)
  (let* ((editor (buffer-editor buffer))
         (head (editor:editor-head editor)))
    (multiple-value-bind (name start) (symbol-at editor head)
      (declare (ignore name))
      (editor:with-undo-group (editor)
        (editor:replace-range editor (editor:make-pos (editor:pos-line head) start) head text)))))

(defun close-completion ()
  (let ((completion *completion*))
    (when completion
      (setf *completion* nil)
      (let ((buffer (completion-buffer completion)))
        (when (member buffer *buffers*)
          (setf (editor:code-editor-keymap (buffer-pane buffer)) (ide-keymap)))))))

(defun completion-follow (buffer)
  "After the cursor moved or the text changed: filter the list, or close it."
  (let ((completion *completion*))
    (when (and completion (eq buffer (completion-buffer completion)))
      (let ((head (editor:editor-head (buffer-editor buffer))))
        (multiple-value-bind (prefix start) (completion-prefix buffer)
          (cond
            ((or (/= (editor:pos-line head) (completion-line completion))
                 (/= start (completion-start completion))
                 (zerop (length prefix)))
             (close-completion))
            ((equal prefix (completion-typed completion)))
            (t
              (let ((items (completions prefix (package-named (buffer-package (buffer-editor buffer))))))
                (if items
                    (setf *completion* (make-completion :buffer buffer :items items :index 0 :typed prefix
                                                        :line (completion-line completion) :start start
                                                        :x (completion-x completion) :y (completion-y completion)))
                    (close-completion))))))))))

(defun completion-move (n)
  (let ((c *completion*))
    (when c
      (setf *completion* (let ((new (copy-completion c)))
                           (setf (completion-index new) (mod (+ (completion-index c) n) (length (completion-items c))))
                           new)))))

(defun completion-accept ()
  (let ((c *completion*))
    (when c
      (close-completion)
      (insert-completion (completion-buffer c) (nth (completion-index c) (completion-items c))))))

;;; ------------------------------------------------------------------
;;; Definitions

(defstate *jumps* '() "Where M-. jumped from: ((buffer . pos) ...).")

(defun goto-offset (path offset)
  (let* ((buffer (find-file path))
         (editor (buffer-editor buffer))
         (doc (editor:editor-document editor))
         (pos (editor:offset-pos doc (max 0 (min offset (length (editor:editor-text editor)))))))
    (editor:code-editor-select (buffer-pane buffer) (editor:pos-line pos) (editor:pos-col pos))
    buffer))

(defun goto-line (path line)
  (let ((buffer (find-file path)))
    (editor:code-editor-select (buffer-pane buffer) line 0)
    buffer))

(defun search-definition (name)
  "(path line) of a line that defines NAME in an open buffer or a file of the project."
  (let ((pattern (string-downcase name)))
    (flet ((search-lines (lines)
             (loop for line in lines for i from 0
                   do (let ((s (string-downcase line)))
                        (when (and (> (length s) 5) (string= "(def" s :end2 4))
                          (let* ((space (position #\Space s))
                                 (rest (and space (string-left-trim " (" (subseq s space))))
                                 (end (and rest (position-if (lambda (c) (member c '(#\Space #\) #\())) rest))))
                            (when (and rest (string= pattern (subseq rest 0 end)))
                              (return i)))))))
           (lines-of (text) (split-string-keep text)))
      (or (loop for b in *buffers*
                for line = (search-lines (lines-of (buffer-text b)))
                when (and line (buffer-path b)) return (list (buffer-path b) line))
          (loop for path in *files*
                when (lisp-file-p path)
                  do (let ((line (search-lines (lines-of (read-text path)))))
                       (when line (return (list path line)))))))))

(defun split-string-keep (text)
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (subseq text start end)
        while end do (setf start (1+ end))))

(defun find-definition (buffer)
  (let* ((editor (buffer-editor buffer))
         (name (symbol-at editor (editor:editor-head editor))))
    (when (zerop (length name))
      (setf *message* "カーソルの位置に名前がありません")
      (return-from find-definition))
    (let* ((symbol (find-symbol* name (package-named (buffer-package editor))))
           (places (and symbol (definition-places symbol)))
           (here (cons buffer (editor:editor-head editor))))
      (flet ((jumped () (push here *jumps*)))
        (cond ((loop for (kind path offset) in places
                     when (or (member path *buffers* :key #'buffer-path :test #'equal)
                              (member path *files* :test #'equal)
                              (and (desktop-p) (ignore-errors (probe-file path))))
                       do (jumped) (goto-offset path offset)
                          (setf *message* (format nil "~A ~A" kind (string-downcase name)))
                          (return t)))
              ((let ((found (search-definition (string-left-trim ":" (subseq name (1+ (or (position #\: name :from-end t) -1)))))))
                 (when found
                   (jumped)
                   (goto-line (first found) (second found))
                   t)))
              (t (setf *message* (format nil "~A の定義が見つかりません" name))))))))

(defun pop-definition ()
  (let ((jump (pop *jumps*)))
    (if (and jump (member (car jump) *buffers*))
        (progn (switch-to (car jump))
               (let ((pos (cdr jump)))
                 (editor:code-editor-select (buffer-pane (car jump)) (editor:pos-line pos) (editor:pos-col pos))))
        (setf *message* "戻る場所がありません"))))

;;; The definitions of the current buffer, for the sidebar

(defstate *outline-version* 0)
(defun refresh-outline-names () (incf *outline-version*))

(defun outline (buffer)
  "((kind name line) ...) of the definitions at the top level of BUFFER."
  (let ((editor (buffer-editor buffer)))
    (loop for i from 0 below (editor:line-count (editor:editor-document editor))
          for s = (line-of editor i)
          when (and (> (length s) 5) (char= (char s 0) #\() (string-equal "def" s :start2 1 :end2 4))
            collect (let* ((space (position #\Space s))
                           (kind (subseq s 1 (or space (length s))))
                           (rest (if space (string-left-trim " " (subseq s space)) ""))
                           (rest (if (and (plusp (length rest)) (char= (char rest 0) #\())
                                     (string-left-trim "(" rest)
                                     rest))
                           (end (position-if (lambda (c) (member c '(#\Space #\) #\())) rest)))
                      (list (string-downcase kind) (subseq rest 0 end) i)))))

;;; ------------------------------------------------------------------
;;; Keys

(defvar *standard-ide-keymap* (editor:make-keymap :name "IDE" :parent editor:*standard-keymap*))
(defvar *emacs-ide-keymap* (editor:make-keymap :name "IDE (Emacs)" :parent editor:*emacs-keymap*))

(defun ide-keymap ()
  (if (eq *key-style* :emacs) *emacs-ide-keymap* *standard-ide-keymap*))

(defmacro define-ide-command (name doc &body body)
  "An editor command that works on the buffer of the editor (BUFFER)."
  `(editor:define-editor-command ,name (editor)
     ,doc
     (let ((buffer (buffer-of editor)))
       (declare (ignorable buffer))
       (when buffer ,@body))))

(define-ide-command ide-eval-defun "Evaluate the form of the top level at the cursor (or the selection)."
  (eval-toplevel-form buffer))
(define-ide-command ide-eval-last-sexp "Evaluate the form before the cursor."
  (multiple-value-bind (start end) (last-sexp editor)
    (if start (eval-region buffer start end) (setf *message* "カーソルの前に式がありません"))))
(define-ide-command ide-eval-buffer "Evaluate the whole file."
  (eval-whole-buffer buffer))
(define-ide-command ide-complete "Complete the symbol before the cursor."
  (start-completion buffer))
(define-ide-command ide-find-definition "Go to the definition of the name at the cursor."
  (find-definition buffer))
(define-ide-command ide-pop-definition "Go back to where the last M-. was typed."
  (pop-definition))
(define-ide-command ide-describe "Describe the symbol at the cursor in the REPL."
  (describe-at-cursor buffer))
(define-ide-command ide-focus-repl "Go to the input of the REPL."
  (focus-repl))
(define-ide-command ide-quick-open "Open a file of the project by its name."
  (open-quick-open))
(define-ide-command ide-macroexpand "Expand the macro form at the cursor once, in the REPL."
  (macroexpand-at-cursor buffer))
(define-ide-command ide-next-tab "Show the next tab."
  (cycle-tab 1))
(define-ide-command ide-previous-tab "Show the previous tab."
  (cycle-tab -1))
(define-ide-command ide-close-tab "Close this tab."
  (close-buffer buffer))

(editor:define-editor-command ide-completion-next (editor) "Select the next completion."
  (completion-move 1))
(editor:define-editor-command ide-completion-previous (editor) "Select the previous completion."
  (completion-move -1))
(editor:define-editor-command ide-completion-accept (editor) "Insert the selected completion."
  (completion-accept))
(editor:define-editor-command ide-completion-cancel (editor) "Close the list of completions."
  (close-completion))

(defun bind (keymap &rest bindings)
  (loop for (key command) on bindings by #'cddr do (editor:define-key keymap key command)))

(bind *standard-ide-keymap*
      "Mod-Return" 'ide-eval-defun "Mod-S-Return" 'ide-eval-buffer "Mod-e" 'ide-eval-last-sexp
      "C-Space" 'ide-complete "F12" 'ide-find-definition "M-." 'ide-find-definition "M-," 'ide-pop-definition
      "F1" 'ide-describe "Mod-j" 'ide-focus-repl "Mod-p" 'ide-quick-open "Mod-m" 'ide-macroexpand
      "C-Tab" 'ide-next-tab "C-S-Tab" 'ide-previous-tab "Mod-w" 'ide-close-tab
      ;; and the keys of SLIME
      "C-c C-c" 'ide-eval-defun "C-c C-k" 'ide-eval-buffer "C-c C-z" 'ide-focus-repl)
(bind *emacs-ide-keymap*
      "C-c C-c" 'ide-eval-defun "C-M-x" 'ide-eval-defun "C-x C-e" 'ide-eval-last-sexp
      "C-c C-k" 'ide-eval-buffer "C-M-i" 'ide-complete "M-Tab" 'ide-complete "Mod-Return" 'ide-eval-defun
      "M-." 'ide-find-definition "M-," 'ide-pop-definition "C-c C-d d" 'ide-describe
      "C-c C-m" 'ide-macroexpand "C-c C-z" 'ide-focus-repl "C-x C-f" 'ide-quick-open
      "C-Tab" 'ide-next-tab "C-S-Tab" 'ide-previous-tab)

(setf *completion-keymap* (editor:make-keymap :name "completion" :parent *standard-ide-keymap*))
(bind *completion-keymap*
      "Down" 'ide-completion-next "Up" 'ide-completion-previous "C-n" 'ide-completion-next
      "C-p" 'ide-completion-previous "Return" 'ide-completion-accept "Tab" 'ide-completion-accept
      "Escape" 'ide-completion-cancel "C-g" 'ide-completion-cancel)

(defun set-key-style (style)
  (setf *key-style* style
        (setting "keys") (string-downcase style))
  (setf (editor::keymap-parent *completion-keymap*) (ide-keymap))
  (dolist (b *buffers*) (setf (editor:code-editor-keymap (buffer-pane b)) (ide-keymap)))
  (setf (editor:code-editor-keymap *repl-input*) (repl-keymap)))

(defun set-wrap (wrap)
  (setf *wrap* wrap (setting "wrap") (if wrap "t" "nil"))
  (dolist (b *buffers*) (setf (editor:code-editor-wrap (buffer-pane b)) wrap)))

(defun cycle-tab (n)
  (let ((buffers *buffers*))
    (when buffers
      (switch-to (nth (mod (+ (or (position *current* buffers) 0) n) (length buffers)) buffers)))))

;;; Describe, macroexpand: in the REPL

(defun symbol-at-cursor (buffer)
  (let* ((editor (buffer-editor buffer))
         (name (symbol-at editor (editor:editor-head editor))))
    (and (plusp (length name)) (find-symbol* name (package-named (buffer-package editor))))))

(defun describe-at-cursor (buffer)
  (let ((symbol (symbol-at-cursor buffer)))
    (if symbol
        (progn (add-entry :eval :text (format nil "describe ~(~A~)" (symbol-name symbol)))
               (add-entry :output :text (describe-to-string symbol)))
        (setf *message* "カーソルの位置に知られた名前がありません"))))

(defun macroexpand-at-cursor (buffer)
  (let* ((editor (buffer-editor buffer))
         (head (editor:editor-head editor))
         (open (if (eql (editor:char-at (editor:editor-document editor) head) #\()
                   head
                   (editor::enclosing-open editor head))))
    (if (null open)
        (setf *message* "カーソルの位置にフォームがありません")
        (let* ((source (text-between editor open (or (editor::forward-sexp-pos editor open) head)))
               (*package* (package-named (buffer-package editor))))
          (handler-case
              (let ((form (read-from-string source)))
                (add-entry :eval :text (format nil "macroexpand-1 ~A" (one-line source)))
                (add-entry :output :text (let ((*print-case* :downcase) (*print-pretty* t))
                                           (prin1-to-string (macroexpand-1 form)))))
            (error (e) (setf *message* (princ-to-string e))))))))

;;; ------------------------------------------------------------------
;;; The REPL's input: an editor too

(defvar *history* (let ((saved (setting "history")))
                    (if saved (split-string saved (code-char 30)) '()))
  "The inputs of the REPL, the latest first.")
(defvar *history-index* -1)
(defvar *repl-standard-keymap* (editor:make-keymap :name "REPL" :parent *standard-ide-keymap*))
(defvar *repl-emacs-keymap* (editor:make-keymap :name "REPL (Emacs)" :parent *emacs-ide-keymap*))
(defun repl-keymap () (if (eq *key-style* :emacs) *repl-emacs-keymap* *repl-standard-keymap*))

(defvar *repl-input*
  (make-instance 'editor:code-editor :text "" :language "lisp" :line-numbers nil
                                     :keymap :standard
                                     :class "ce-pane repl-input"))

(defun repl-editor () (editor:code-editor-editor *repl-input*))

(defun focus-repl () (editor:code-editor-focus *repl-input*))

(defun repl-send (&optional force)
  (let* ((editor (repl-editor))
         (text (editor:editor-text editor)))
    (cond ((zerop (length (string-trim '(#\Space #\Newline #\Tab) text))) nil)
          ((and (not force) (not (complete-input-p text)))
           (editor:run-command editor 'editor::newline))
          ((busy-p) (setf *message* "評価中です（■ 中断 で止められます）"))
          (t
           (setf *history* (cons text (remove text *history* :test #'string=))
                 *history-index* -1)
           (setf (setting "history")
                 (format nil (concatenate 'string "~{~A~^" (string (code-char 30)) "~}")
                         (subseq *history* 0 (min 100 (length *history*)))))
           (setf (editor:code-editor-text *repl-input*) "")
           (setf *eval-origin* :repl)
           (evaluate-in-repl text)))))

(defun repl-history (n)
  (let ((index (max -1 (min (1- (length *history*)) (+ *history-index* n)))))
    (setf *history-index* index)
    (setf (editor:code-editor-text *repl-input*) (if (< index 0) "" (nth index *history*)))
    (editor:run-command (repl-editor) 'editor::end-of-buffer)))

(editor:define-editor-command ide-repl-send (editor) "Evaluate the input, if it is complete."
  (repl-send))
(editor:define-editor-command ide-repl-send-now (editor) "Evaluate the input."
  (repl-send t))
(editor:define-editor-command ide-repl-previous (editor) "The previous input (on the first line)."
  (if (zerop (editor:pos-line (editor:editor-head editor)))
      (repl-history 1)
      (editor:run-command editor 'editor::previous-line)))
(editor:define-editor-command ide-repl-next (editor) "The next input (on the last line)."
  (if (= (editor:pos-line (editor:editor-head editor))
         (1- (editor:line-count (editor:editor-document editor))))
      (repl-history -1)
      (editor:run-command editor 'editor::next-line)))
(editor:define-editor-command ide-repl-history-previous (editor) "The previous input."
  (repl-history 1))
(editor:define-editor-command ide-repl-history-next (editor) "The next input."
  (repl-history -1))
(editor:define-editor-command ide-repl-to-editor (editor) "Go back to the editor."
  (when *current* (editor:code-editor-focus (buffer-pane *current*))))
(editor:define-editor-command ide-repl-complete (editor) "Complete in the REPL."
  (let ((items (multiple-value-bind (name start) (symbol-at editor (editor:editor-head editor))
                 (let ((prefix (subseq name 0 (- (editor:pos-col (editor:editor-head editor)) start))))
                   (completions prefix (package-named *repl-package*))))))
    (cond ((null items) (setf *message* "補完はありません"))
          (t (let* ((common (if (rest items) (common-prefix items) (first items)))
                    (head (editor:editor-head editor)))
               (multiple-value-bind (name start) (symbol-at editor head)
                 (declare (ignore name))
                 (editor:replace-range editor (editor:make-pos (editor:pos-line head) start) head common))
               (when (rest items)
                 (setf *message* (format nil "~{~A~^  ~}" (subseq items 0 (min 12 (length items)))))))))))

(bind *repl-standard-keymap*
      "Return" 'ide-repl-send "Mod-Return" 'ide-repl-send-now "S-Return" 'editor::newline
      "Up" 'ide-repl-previous "Down" 'ide-repl-next "M-p" 'ide-repl-history-previous
      "M-n" 'ide-repl-history-next "Mod-j" 'ide-repl-to-editor "Tab" 'ide-repl-complete
      "C-Space" 'ide-repl-complete)
(bind *repl-emacs-keymap*
      "Return" 'ide-repl-send "C-Return" 'ide-repl-send-now "S-Return" 'editor::newline
      "M-p" 'ide-repl-history-previous "M-n" 'ide-repl-history-next
      "C-c C-z" 'ide-repl-to-editor "Tab" 'ide-repl-complete "C-M-i" 'ide-repl-complete)

(setf (editor:code-editor-keymap *repl-input*) (repl-keymap))

;;; ------------------------------------------------------------------
;;; Quick open: a list of the files filtered by what is typed

(defstate *quick-open* nil "NIL, or (query . index).")

(defun open-quick-open ()
  (refresh-files)
  (setf *quick-open* (cons "" 0)))

(defun quick-open-matches (query)
  (let ((q (string-downcase query)))
    (flet ((matches (path)
             ;; the characters of Q in order
             (let ((name (string-downcase (relative-name path))) (i 0))
               (loop for c across q
                     do (let ((found (position c name :start i)))
                          (if found (setf i (1+ found)) (return-from matches nil))))
               t)))
      (let ((found (remove-if-not #'matches *files*)))
        (subseq found 0 (min 50 (length found)))))))

(defun quick-open-choose (path)
  (setf *quick-open* nil)
  (when path (find-file path)))

;;; ------------------------------------------------------------------
;;; The inspector

(defstate *inspected* '() "The objects inspected, the current first.")

(defun inspect-object (object) (push object *inspected*))

;;; ------------------------------------------------------------------
;;; Commands of the menus and the toolbar

(define-command (new-file :menu "ファイル" :label "新しいファイル…") ()
  (if (virtual-p)
      (prompt-for-string "ファイルの名前" :text "new.lisp"
                         :continuation (lambda (name ok)
                                         (when (and ok (plusp (length name)))
                                           (unless (member name *files* :test #'string=)
                                             (write-text name (format nil "(in-package \"CL-USER\")~%~%")))
                                           (find-file name))))
      (let ((path (choose-file-to-save :default-name "new.lisp" :filters '(("Lisp" "lisp")))))
        (when path
          (unless (probe-file path) (write-text path (format nil "(in-package \"CL-USER\")~%~%")))
          (refresh-files)
          (find-file path)))))

(define-command (open-folder :menu "ファイル" :label "フォルダを開く…" :enabled (desktop-p)) ()
  (let ((path (choose-directory :title "プロジェクトのフォルダ")))
    (when path (set-root path))))

(define-command (open-file :menu "ファイル" :label "ファイルを開く…") ()
  (if (virtual-p)
      (open-quick-open)
      (let ((path (choose-file-to-open :filters '(("Lisp" "lisp" "asd" "lsp" "cl") ("すべて" "*")))))
        (when path (find-file path)))))

(define-command (save-current :menu "ファイル" :label "保存") ()
  (save-buffer *current*))

(define-command (eval-form-command :menu "Lisp" :label "フォームを評価" :separator nil) ()
  (when *current* (eval-toplevel-form *current*)))

(define-command (eval-file-command :menu "Lisp" :label "ファイルを評価") ()
  (when *current* (eval-whole-buffer *current*)))

(define-command (interrupt-command :menu "Lisp" :label "中断" :enabled *busy*) ()
  (if *debuggers*
      (choose-restart* (first *debuggers*) (find-restart 'abort (debugger-condition (first *debuggers*))))
      (interrupt-evaluation)))

(define-command (clear-repl :menu "Lisp" :label "REPL を消去") ()
  (clear-log))

(defun set-root (path)
  (let ((root (if (char= (char path (1- (length path))) #\/) path (concatenate 'string path "/"))))
    (setf *root* root
          (setting "root") root)
    (refresh-files)
    (ignore-errors
     (let ((process (js:global "process")))
       (when (js:js-value-p process) (js:call process "chdir" root))))
    (setf *default-pathname-defaults* (pathname root))))

;;; ------------------------------------------------------------------
;;; The screen

(defun target-value (event) (js:get (js:get event "target") "value"))
(defun cancel-event (event) (js:call event "preventDefault") (js:call event "stopPropagation"))

(defcomponent ide-toolbar ()
  (:header :class "ide-toolbar"
    (:span :class "ide-brand" "λ Clysm IDE")
    (:span :class "ide-project" (cond (*root* (file-name (string-right-trim "/" *root*)))
                                      ((virtual-p) "ブラウザのプロジェクト")
                                      (t "")))
    (:div :class "ide-group"
      (when (desktop-p)
        (:button :title "フォルダを開く" :on-click (lambda (e) (declare (ignore e)) (open-folder)) "開く"))
      (:button :title "新しいファイル" :on-click (lambda (e) (declare (ignore e)) (new-file)) "新規")
      (:button :title "保存 (Mod-S)" :on-click (lambda (e) (declare (ignore e)) (save-current)) "保存"))
    (:div :class "ide-group"
      (:button :class "primary" :title "カーソルのフォームを評価 (Mod-Return, C-c C-c)"
               :disabled (and (busy-p) t)
               :on-click (lambda (e) (declare (ignore e)) (eval-form-command)) "▶ 評価")
      (:button :title "ファイル全体を評価 (Mod-Shift-Return, C-c C-k)" :disabled (and (busy-p) t)
               :on-click (lambda (e) (declare (ignore e)) (eval-file-command)) "ファイルを評価")
      (:button :class (if *busy* "danger" nil) :title "評価を止める" :disabled (not *busy*)
               :on-click (lambda (e) (declare (ignore e)) (interrupt-command)) "■ 中断"))
    (:div :class "ide-group right"
      (:label "キー "
        (:select :on-change (lambda (e) (set-key-style (if (equal (target-value e) "emacs") :emacs :standard)))
          (:option :value "standard" :selected (eq *key-style* :standard) "標準")
          (:option :value "emacs" :selected (eq *key-style* :emacs) "Emacs")))
      (:label (:input :type "checkbox" :checked (and *wrap* t)
                      :on-change (lambda (e) (set-wrap (eq (js:get (js:get e "target") "checked") t))))
              " 折り返す"))))

;;; The sidebar

(defstate *collapsed* '() "The directories of the file tree that are closed.")

(defun directory-of (relative)
  (let ((slash (position #\/ relative :from-end t)))
    (and slash (subseq relative 0 (1+ slash)))))

(defun file-rows (files)
  "The rows of the tree: (:dir name depth) and (:file path depth)."
  (let ((rows '()) (shown '()))
    (dolist (path files (nreverse rows))
      (let* ((rel (relative-name path))
             (dir (directory-of rel)))
        ;; the directories of REL not shown yet
        (when dir
          (loop for end = (position #\/ dir) then (position #\/ dir :start (1+ end))
                while end
                do (let ((d (subseq dir 0 (1+ end))))
                     (unless (member d shown :test #'string=)
                       (push d shown)
                       (unless (some (lambda (c) (and (< (length c) (length d)) (string= c d :end2 (length c))))
                                     *collapsed*)
                         (push (list :dir d (count #\/ d :end end)) rows))))))
        (unless (and dir (some (lambda (c) (string= c dir :end2 (min (length dir) (length c))))
                               (remove-if (lambda (c) (> (length c) (length dir))) *collapsed*)))
          (push (list :file path (count #\/ rel)) rows))))))

(defcomponent file-tree ()
  (let ((current (and *current* (buffer-path *current*))))
    (:div :class "ide-files"
      (if (and (desktop-p) (null *root*))
          (:div :class "ide-empty"
            (:p "フォルダが開かれていません")
            (:button :on-click (lambda (e) (declare (ignore e)) (open-folder)) "フォルダを開く"))
          (mapcar (lambda (row)
                    (destructuring-bind (kind name depth) row
                      (keyed name
                             (if (eq kind :dir)
                                 (let ((closed (member name *collapsed* :test #'string=)))
                                   (:div :class "ide-row dir" :style (format nil "padding-left:~Dpx" (+ 8 (* 12 depth)))
                                         :on-click (lambda (e)
                                                     (declare (ignore e))
                                                     (setf *collapsed* (if closed
                                                                           (remove name *collapsed* :test #'string=)
                                                                           (cons name *collapsed*))))
                                     (if closed "▸ " "▾ ")
                                     (file-name (string-right-trim "/" name))))
                                 (:div :class (if (equal name current) "ide-row file current" "ide-row file")
                                       :style (format nil "padding-left:~Dpx" (+ 20 (* 12 depth)))
                                       :title (relative-name name)
                                       :on-click (lambda (e) (declare (ignore e)) (find-file name))
                                   (:span :class (if (lisp-file-p name) "icon lisp" "icon") (if (lisp-file-p name) "λ" "·"))
                                   (file-name name))))))
                  (file-rows *files*))))))

(defcomponent outline-view ()
  (let ((buffer *current*))
    *outline-version*
    (:div :class "ide-outline"
      (when buffer
        (editor:code-editor-version (buffer-pane buffer))
        (let ((items (outline buffer)))
          (if (null items)
              (:div :class "ide-empty" "定義はありません")
              (mapcar (lambda (item)
                        (destructuring-bind (kind name line) item
                          (:div :class "ide-row def"
                                :on-click (lambda (e)
                                            (declare (ignore e))
                                            (editor:code-editor-select (buffer-pane buffer) line 0)
                                            (editor:code-editor-focus (buffer-pane buffer)))
                            (:span :class "kind" (kind-abbreviation kind))
                            (:span :class "name" name))))
                      items)))))))

(defun kind-abbreviation (kind)
  (cond ((string= kind "defun") "f")
        ((string= kind "defmacro") "m")
        ((member kind '("defvar" "defparameter" "defconstant" "defstate") :test #'string=) "v")
        ((member kind '("defclass" "defstruct" "defmodel" "define-condition" "deftype") :test #'string=) "c")
        ((member kind '("defgeneric" "defmethod") :test #'string=) "g")
        ((string= kind "defpackage") "p")
        (t "·")))

(defcomponent sidebar ()
  (:aside :class "ide-sidebar"
    (:div :class "ide-section-title" "ファイル"
      (:button :class "small" :title "一覧を読み直す"
               :on-click (lambda (e) (declare (ignore e)) (refresh-files)) "↻"))
    (file-tree)
    (:div :class "ide-section-title" "定義")
    (outline-view)))

;;; The tabs and the editor

(defcomponent tabs ()
  (:div :class "ide-tabs"
    (mapcar (lambda (buffer)
              (keyed buffer
                     (:div :class (if (eq buffer *current*) "ide-tab current" "ide-tab")
                           :title (if (buffer-path buffer) (relative-name (buffer-path buffer)) "保存されていないバッファ")
                           :on-click (lambda (e) (declare (ignore e)) (switch-to buffer))
                       (:span (buffer-name buffer))
                       (when (buffer-modified buffer) (:span :class "dot" "●"))
                       (:button :class "close" :title "閉じる"
                                :on-click (lambda (e) (cancel-event e) (close-buffer buffer))
                                "×"))))
            *buffers*)))

(defcomponent completion-popup ()
  (let ((c *completion*))
    (when c
      (:div :class "ide-completion"
            :style (format nil "left:~Dpx;top:~Dpx" (round (completion-x c)) (round (completion-y c)))
        (loop for item in (completion-items c)
              for i from 0 below 60
              collect (let ((item item) (i i))
                        (:div :class (if (= i (completion-index c)) "item selected" "item")
                              :on-mousedown (lambda (e)
                                              (cancel-event e)
                                              (setf (completion-index *completion*) i)
                                              (completion-accept)
                                              (editor:code-editor-focus (buffer-pane (completion-buffer c))))
                          item)))))))

;;; The REPL

(defcomponent log-entry (entry)
  (let ((text (entry-text entry)))
    (case (entry-kind entry)
      (:input (:div :class "e-input"
                (:span :class "prompt" (format nil "~A>" (entry-package entry)))
                (:pre text)))
      (:eval (:div :class "e-eval" (:span :class "prompt" "評価") (:pre text)))
      (:output (:pre :class "e-output" text))
      (:values
       (let ((objects (entry-objects entry)))
         (:div :class "e-values"
           (if (null objects)
               (:span :class "none" "; 値なし")
               (mapcar (lambda (object)
                         (:div :class "value" :title "クリックで調べる"
                               :on-click (lambda (e) (declare (ignore e)) (inspect-object object))
                           (:span :class "arrow" "⇒ ")
                           (:span (print-value object (entry-package entry)))))
                       objects)))))
      (t (:div :class (format nil "e-~(~A~)" (entry-kind entry)) text)))))

(defvar *log-node* nil)
(defvar *log-scroll-requested* nil)

(defun scroll-log ()
  (unless *log-scroll-requested*
    (setf *log-scroll-requested* t)
    (js:call (js:global) "requestAnimationFrame"
             (lambda (&rest args)
               (declare (ignore args))
               (setf *log-scroll-requested* nil)
               (when *log-node*
                 (js:set *log-node* "scrollTop" (js:get *log-node* "scrollHeight")))
               nil))))

(defcomponent repl-log ()
  (let ((entries (reverse *log*)))
    (scroll-log)
    (:div :class "ide-log" :on-mount (lambda (node) (setf *log-node* node))
          :on-click (lambda (e)
                      ;; a click that does not select text goes to the input
                      (let ((selection (js:call (js:global) "getSelection")))
                        (when (and (js:js-value-p selection) (eq (js:get selection "isCollapsed") t)
                                   (not (equal (js:get (js:get e "target") "tagName") "SPAN")))
                          (focus-repl))))
      (if (null entries)
          (:div :class "e-note welcome"
            "Common Lisp の REPL です。式を入力して Return で評価します（Shift-Return で改行、↑↓ で履歴）。")
          (mapcar (lambda (entry) (keyed entry (log-entry entry))) entries)))))

(defcomponent debugger-view ()
  (let ((debugger (first *debuggers*)))
    (when debugger
      (let ((condition (debugger-condition debugger)))
        (:div :class "ide-debugger"
          (:div :class "title"
            (:span :class "badge" (format nil "デバッガ ~D" (debugger-level debugger)))
            (:span :class "type" (condition-type-name condition)))
          (:pre :class "message" (or (ignore-errors (princ-to-string condition)) "（表示できない状態）"))
          (:div :class "restarts"
            (loop for restart in (debugger-restarts debugger)
                  for i from 0
                  collect (let ((restart restart))
                            (:button :class (if (eq (restart-name restart) 'abort) "restart abort" "restart")
                                     :on-click (lambda (e)
                                                 (declare (ignore e))
                                                 (refocus)
                                                 (choose-restart* debugger restart))
                              (:span :class "n" i)
                              (or (ignore-errors (princ-to-string restart)) (string (restart-name restart)))))))
          (:details :class "backtrace"
            (:summary (format nil "バックトレース（~D）" (length (debugger-backtrace debugger))))
            (:ol (mapcar (lambda (frame) (:li frame)) (debugger-backtrace debugger))))
          (:div :class "inspect-condition"
            (:button :on-click (lambda (e) (declare (ignore e)) (inspect-object condition))
              "状態を調べる")))))))

(defcomponent inspector-view ()
  (let ((objects *inspected*))
    (when objects
      (let* ((object (first objects))
             (parts (inspect-parts object)))
        (:div :class "ide-inspector"
          (:div :class "title"
            (when (rest objects)
              (:button :class "small" :title "戻る" :on-click (lambda (e) (declare (ignore e)) (pop *inspected*)) "←"))
            (:span :class "type" (car parts))
            (:button :class "small close" :title "閉じる"
                     :on-click (lambda (e) (declare (ignore e)) (setf *inspected* '())) "×"))
          (:pre :class "printed" (one-line (print-value object) 400))
          (:table
            (mapcar (lambda (part)
                      (let ((value (cdr part)))
                        (:tr :on-click (lambda (e) (declare (ignore e)) (inspect-object value))
                          (:th (car part))
                          (:td (one-line (print-value value) 120)))))
                    (cdr parts))))))))

(defcomponent repl-view ()
  (:div :class "ide-repl"
    (:div :class "ide-section-title" "REPL"
      (:span :class "package" *repl-package*)
      (when *busy* (:span :class "busy" (format nil "評価中: ~A" *busy*)))
      (:button :class "small" :title "消去" :on-click (lambda (e) (declare (ignore e)) (clear-log)) "消去"))
    (debugger-view)
    (repl-log)
    (:div :class "ide-input"
      (:span :class "prompt" (format nil "~A>" *repl-package*))
      (:div :class "ide-input-editor"
            :style (progn (editor:code-editor-version *repl-input*)
                          (format nil "height:~Dpx"
                                  (min 164 (+ 4 (* 20 (editor:line-count (editor:editor-document (repl-editor))))))))
        (pane-node *repl-input*)))))

;;; The splitter between the editor and the REPL

(defstate *bottom-height* (or (ignore-errors (parse-integer (setting "bottom" "260"))) 260))

(defcomponent splitter ()
  (:div :class "ide-splitter"
        :on-pointerdown (lambda (e)
                          (js:call (js:get e "target") "setPointerCapture" (js:get e "pointerId"))
                          (cancel-event e))
        :on-pointermove (lambda (e)
                          (when (plusp (js:get e "buttons"))
                            (let* ((center (js:get (js:get e "target") "parentNode"))
                                   (rect (js:call center "getBoundingClientRect"))
                                   (height (round (- (js:get rect "bottom") (js:get e "clientY")))))
                              (setf *bottom-height* (max 80 (min height (- (round (js:get rect "height")) 120)))))))
        :on-pointerup (lambda (e)
                        (declare (ignore e))
                        (setf (setting "bottom") (princ-to-string *bottom-height*)))))

(defcomponent center ()
  (:main :class "ide-center"
    (tabs)
    (:div :class "ide-editor"
      (if *current* (pane-node (buffer-pane *current*)) nil))
    (splitter)
    (:div :class "ide-bottom" :style (format nil "height:~Dpx" *bottom-height*)
      (repl-view)
      (inspector-view))))

(defcomponent status-line ()
  (let ((buffer *current*))
    (:footer :class "ide-status"
      (:span :class "eldoc" (or *eldoc* ""))
      (:span :class "message" (or *message* (and buffer (editor:code-editor-message (buffer-pane buffer))) ""))
      (when buffer
        (multiple-value-bind (line col) (editor:code-editor-cursor (buffer-pane buffer))
          (list (:span (buffer-package (buffer-editor buffer)))
                (:span (format nil "~D:~D" (1+ line) (1+ col))))))
      (:span (if (eq *key-style* :emacs) "Emacs" "標準")))))

(defcomponent quick-open ()
  (let ((state *quick-open*))
    (when state
      (let* ((matches (quick-open-matches (car state)))
             (index (min (cdr state) (max 0 (1- (length matches))))))
        (:div :class "ide-overlay" :on-mousedown (lambda (e)
                                                   (when (equal (js:get (js:get e "target") "className") "ide-overlay")
                                                     (setf *quick-open* nil)))
          (:div :class "ide-palette"
            (:input :class "query" :placeholder "ファイル名" :value (car state)
                    :on-mount (lambda (node) (js:call node "focus"))
                    :on-input (lambda (e) (setf *quick-open* (cons (target-value e) 0)))
                    :on-keydown (lambda (e)
                                  (let ((key (js:get e "key")))
                                    (cond ((equal key "ArrowDown") (cancel-event e)
                                           (setf *quick-open* (cons (car state) (min (1+ index) (1- (length matches))))))
                                          ((equal key "ArrowUp") (cancel-event e)
                                           (setf *quick-open* (cons (car state) (max 0 (1- index)))))
                                          ((equal key "Enter") (cancel-event e) (quick-open-choose (nth index matches)))
                                          ((equal key "Escape") (cancel-event e) (quick-open-choose nil)
                                           (when *current* (editor:code-editor-focus (buffer-pane *current*))))))))
            (:div :class "list"
              (if (null matches)
                  (:div :class "ide-empty" "見つかりません")
                  (loop for path in matches for i from 0
                        collect (let ((path path))
                                  (:div :class (if (= i index) "item selected" "item")
                                        :on-click (lambda (e) (declare (ignore e)) (quick-open-choose path))
                                    (:span :class "name" (file-name path))
                                    (:span :class "dir" (or (directory-of (relative-name path)) "")))))))))))))

(defcomponent app ()
  (:div :class "ide"
    (ide-toolbar)
    (:div :class "ide-body"
      (sidebar)
      (center))
    (status-line)
    (completion-popup)
    (quick-open)))

;;; ------------------------------------------------------------------
;;; The files of a new project in the browser

(defparameter +scratch+ ";;; scratch — 保存しないメモ。Mod-Return でカーソルのフォームを評価します。

(+ 1 2)
")

(defparameter +samples+
  (list
   (cons "tutorial.lisp"
         ";;;; Clysm IDE へようこそ
;;;;
;;;; この IDE は、エディタも画面も評価する Lisp も、すべて Common Lisp で
;;;; 書かれ、Clysm で WebAssembly にコンパイルされています。評価した式は
;;;; その場で WebAssembly にコンパイルされて動きます。
;;;;
;;;; キー（標準 / Emacs）
;;;;   Mod-Return / C-c C-c    カーソルのフォーム（か選択）を評価
;;;;   Mod-Shift-Return / C-c C-k  ファイル全体を評価
;;;;   Mod-e / C-x C-e         カーソルの前の式を評価
;;;;   C-Space / C-M-i         補完
;;;;   F12, M-. / M-,          定義へ移動 / 戻る
;;;;   F1 / C-c C-d d          describe
;;;;   Mod-m / C-c C-m         マクロを1段展開
;;;;   Mod-j / C-c C-z         REPL へ
;;;;   Mod-p / C-x C-f         ファイルを名前で開く
;;;; 関数の引数リストは、カーソルを括弧の中に置くとステータス行に出ます。

(in-package \"CL-USER\")

(defun fact (n)
  \"N の階乗。\"
  (if (< n 2)
      1
      (* n (fact (1- n)))))

(fact 30)

;;; 値をクリックすると、インスペクタで中を調べられます
(defstruct point x y)

(list (make-point :x 1 :y 2) #(1 2 3) (make-hash-table))

;;; エラーはデバッガに出ます。再開（リスタート）を選んでください
(defun safe-divide (a b)
  (restart-case (/ a b)
    (use-value (v)
      :report \"代わりの値を使う（1）\"
      :interactive (lambda () (list 1))
      v)))

(safe-divide 10 0)

;;; 終わらない計算も、■ 中断 で止められます（IDE は動き続けます）
(defun forever ()
  (loop for i from 0
        when (zerop (mod i 10000000))
          do (format t \"~D~%\" i)))

;; (forever)
")
   (cons "life.lisp"
         ";;;; ライフゲーム

(defpackage \"LIFE\"
  (:use \"CL\")
  (:export \"RUN\"))

(in-package \"LIFE\")

(defparameter *width* 20)
(defparameter *height* 10)

(defun make-board ()
  (make-array (list *height* *width*) :initial-element 0))

(defun glider ()
  (let ((board (make-board)))
    (loop for (y x) in '((0 1) (1 2) (2 0) (2 1) (2 2))
          do (setf (aref board y x) 1))
    board))

(defun neighbours (board y x)
  (loop for dy from -1 to 1
        sum (loop for dx from -1 to 1
                  unless (and (zerop dy) (zerop dx))
                    sum (aref board (mod (+ y dy) *height*) (mod (+ x dx) *width*)))))

(defun step-board (board)
  (let ((next (make-board)))
    (dotimes (y *height* next)
      (dotimes (x *width*)
        (let ((n (neighbours board y x)))
          (setf (aref next y x)
                (if (or (= n 3) (and (= n 2) (= (aref board y x) 1))) 1 0)))))))

(defun show (board)
  (dotimes (y *height*)
    (dotimes (x *width*)
      (write-char (if (= (aref board y x) 1) #\\# #\\.)))
    (terpri)))

(defun run (&optional (generations 4))
  (let ((board (glider)))
    (dotimes (i generations)
      (format t \"~&第 ~D 世代~%\" i)
      (show board)
      (setf board (step-board board)))))

;; (life:run)
")))

;;; ------------------------------------------------------------------
;;; Startup

(defun start ()
  (setf *abort-label* "キャンセル")
  (cond ((virtual-p)
         (when (null (virtual-index))
           (dolist (sample +samples+) (write-text (car sample) (cdr sample))))
         (setf *files* (virtual-index)))
        (t
         (let ((root (setting "root")))
           (when (and root (ignore-errors (probe-file root)))
             (set-root root)))))
  ;; the files that were open
  (dolist (path (split-string (setting "open" "") #\Newline))
    (when (or (member path *files* :test #'string=)
              (and (desktop-p) (ignore-errors (probe-file path))))
      (unless (find path *buffers* :key #'buffer-path :test #'equal)
        (make-buffer (file-name path) (read-text path) :path path))))
  (let ((current (setting "current")))
    (cond ((and current (plusp (length current))
                (or (member current *files* :test #'string=)
                    (and (desktop-p) (ignore-errors (probe-file current)))))
           (find-file current))
          ((and (virtual-p) (member "tutorial.lisp" *files* :test #'string=))
           (find-file "tutorial.lisp"))
          (t (switch-to (scratch-buffer)))))
  (mount (app) "#app")
  (install-commands *backend* :edit-label "編集" :view-label "表示" :quit-label "終了")
  (autorun (lambda ()
             (set-title *backend* (format nil "~:[~;● ~]~A — Clysm IDE"
                                          (and *current* (buffer-modified *current*))
                                          (if *current* (buffer-name *current*) "")))))
  ;; the folders and files the desktop application is asked to open
  (when (desktop-p)
    (let ((desktop (js:global "desktop")))
      (when (js:js-value-p (js:get desktop "onOpen"))
        (js:call desktop "onOpen"
                 (lambda (path)
                   (with-error-dialog
                     (if (ignore-errors (probe-file (concatenate 'string path "/")))
                         (set-root path)
                         (progn
                           (unless *root* (set-root (subseq path 0 (1+ (position #\/ path :from-end t)))))
                           (find-file path))))
                   nil)))))
  (when *current* (editor:code-editor-focus (buffer-pane *current*))))

(start)
