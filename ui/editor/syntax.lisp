;;;; syntax.lisp — languages: highlighting, brackets and indentation
;;;;
;;;; A language tokenizes one line at a time, from the state at the start of
;;;; the line (inside a string, a block comment...) to the state at its end.
;;;; The editor keeps the state at the start of every line, computed lazily
;;;; and recomputed from the first line that changed; the tokens of a line
;;;; are cached for its string (a changed line is a new string).
;;;;
;;;; A token is (start end class): CLASS is a string, the CSS class of the
;;;; text on the page (view.lisp prefixes it with "ce-").  Brackets inside
;;;; strings and comments are not brackets: the matching of brackets, the
;;;; motion over S-expressions and the indentation of Lisp only look at the
;;;; characters outside the tokens of the classes in NON-CODE-CLASSES.

(in-package "EDITOR")

(defclass language ()
  ((name :initarg :name :reader language-name)
   ;; the text a line comment starts with, or NIL
   (comment-start :initarg :comment-start :initform nil :reader language-comment-start)
   ;; (open . close) pairs
   (brackets :initarg :brackets :initform '((#\( . #\)) (#\[ . #\]) (#\{ . #\}))
             :reader language-brackets)
   ;; the characters that auto-pair inserts the closing one of
   (pairs :initarg :pairs :initform '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\" . #\"))
          :reader language-pairs))
  (:documentation "The syntax of the text of an editor."))

(defgeneric initial-syntax-state (language)
  (:method (language) nil))

(defgeneric tokenize-line (language string state)
  (:documentation "The tokens of the line STRING, which starts in STATE, and the
state at its end.")
  (:method (language string state) (declare (ignore string)) (values '() state)))

(defgeneric compute-indentation (language editor line)
  (:documentation "The column the text of LINE should start at, or NIL to leave it."))

(defgeneric word-char-p (language char)
  (:method (language char) (or (alphanumericp char) (char= char #\_))))

(defparameter *non-code-classes* '("comment" "string" "char"))

(defclass plain-text (language) ()
  (:default-initargs :name "Text"))

(defvar *plain-text* (make-instance 'plain-text))

;;; ------------------------------------------------------------------
;;; The state of each line, and the cached tokens

(defun %line-info (editor line state)
  "(tokens end-state brackets) of LINE starting in STATE, cached."
  (let* ((string (line-string (editor-document editor) line))
         (cache (token-cache editor))
         (entry (gethash string cache)))
    (if (and entry (equal (car entry) state))
        (cdr entry)
        (multiple-value-bind (tokens end) (tokenize-line (editor-language editor) string state)
          (let ((info (list tokens end (line-brackets (editor-language editor) string tokens))))
            (when (> (hash-table-count cache) (+ 2000 (* 4 (line-count (editor-document editor)))))
              (clrhash cache))
            (setf (gethash string cache) (cons state info))
            info)))))

(defun line-start-state (editor line)
  "The syntax state at the start of LINE."
  (let ((states (syntax-states editor))
        (count (line-count (editor-document editor))))
    ;; the vector has a state for every line and the end
    (unless (= (length states) (1+ count))
      (when (< (length states) (1+ count))
        (setf (syntax-valid editor) (min (syntax-valid editor) (max 0 (1- (length states))))))
      (adjust-array states (1+ count) :fill-pointer (1+ count) :initial-element nil))
    (when (zerop (syntax-valid editor))
      (setf (aref states 0) (initial-syntax-state (editor-language editor))))
    (loop while (< (syntax-valid editor) line)
          do (let ((i (syntax-valid editor)))
               (setf (aref states (1+ i)) (second (%line-info editor i (aref states i))))
               (incf (syntax-valid editor))))
    (aref states line)))

(defun line-info (editor line)
  (%line-info editor line (line-start-state editor line)))

(defun line-tokens (editor line)
  "The tokens (start end class) of LINE."
  (first (line-info editor line)))

(defun line-brackets* (editor line)
  "The brackets of LINE outside strings and comments: a list of (col . char)."
  (third (line-info editor line)))

(defun line-brackets (language string tokens)
  (let ((brackets (language-brackets language)) (result '()))
    (loop for i from 0 below (length string)
          for c = (char string i)
          do (when (and (or (assoc c brackets) (rassoc c brackets))
                        (not (non-code-at-p tokens i)))
               (push (cons i c) result)))
    (nreverse result)))

(defun token-at (tokens col)
  (find-if (lambda (token) (and (<= (first token) col) (< col (second token)))) tokens))

(defun non-code-at-p (tokens col)
  (let ((token (token-at tokens col)))
    (and token (member (third token) *non-code-classes* :test #'string=) token)))

(defun char-class (editor pos)
  "The class of the character at POS: :CODE, or the class of its token
(\"string\", \"comment\", \"char\")."
  (let ((token (non-code-at-p (line-tokens editor (pos-line pos)) (pos-col pos))))
    (if token (third token) :code)))

;;; ------------------------------------------------------------------
;;; Brackets

(defparameter *bracket-scan-lines* 5000 "How far brackets are looked for, in lines.")

(defun open-bracket-p (editor char) (assoc char (language-brackets (editor-language editor))))
(defun close-bracket-p (editor char) (rassoc char (language-brackets (editor-language editor))))

(defun scan-brackets-backward (editor pos fn)
  "Call FN with (line col char) for every bracket before POS, nearest first,
until it returns true; return that value."
  (let ((first (max 0 (- (pos-line pos) *bracket-scan-lines*))))
    (loop for line from (pos-line pos) downto first
          do (dolist (b (reverse (line-brackets* editor line)))
               (when (or (< line (pos-line pos)) (< (car b) (pos-col pos)))
                 (let ((result (funcall fn line (car b) (cdr b))))
                   (when result (return-from scan-brackets-backward result))))))
    nil))

(defun scan-brackets-forward (editor pos fn)
  "Call FN with (line col char) for every bracket at or after POS, nearest
first, until it returns true; return that value."
  (let ((last (min (1- (line-count (editor-document editor))) (+ (pos-line pos) *bracket-scan-lines*))))
    (loop for line from (pos-line pos) to last
          do (dolist (b (line-brackets* editor line))
               (when (or (> line (pos-line pos)) (>= (car b) (pos-col pos)))
                 (let ((result (funcall fn line (car b) (cdr b))))
                   (when result (return-from scan-brackets-forward result))))))
    nil))

(defun enclosing-open (editor pos)
  "The position of the innermost open bracket around POS, or NIL."
  (let ((depth 0))
    (scan-brackets-backward editor pos
                            (lambda (line col char)
                              (cond ((close-bracket-p editor char) (incf depth) nil)
                                    ((zerop depth) (make-pos line col))
                                    (t (decf depth) nil))))))

(defun bracket-at-p (editor pos)
  (let ((line (pos-line pos)) (col (pos-col pos)))
    (find col (line-brackets* editor line) :key #'car)))

(defun matching-bracket (editor pos)
  "If there is a bracket at POS, the position of the bracket that matches it
(or NIL if none does), as the second value T."
  (let ((b (bracket-at-p editor pos)))
    (when b
      (let ((char (cdr b)) (depth 0))
        (values
         (if (open-bracket-p editor char)
             (scan-brackets-forward editor (next-pos (editor-document editor) pos)
                                    (lambda (line col c)
                                      (cond ((open-bracket-p editor c) (incf depth) nil)
                                            ((plusp depth) (decf depth) nil)
                                            ((eql c (cdr (open-bracket-p editor char))) (make-pos line col))
                                            (t :mismatch))))
             (scan-brackets-backward editor pos
                                     (lambda (line col c)
                                       (cond ((close-bracket-p editor c) (incf depth) nil)
                                             ((plusp depth) (decf depth) nil)
                                             ((eql c (car (close-bracket-p editor char))) (make-pos line col))
                                             (t :mismatch)))))
         t)))))

(defun bracket-pair-near (editor pos)
  "The brackets to highlight for the cursor at POS: (this other), OTHER
being NIL if nothing matches; or NIL."
  (let ((doc (editor-document editor)))
    (flet ((try (p)
             (when p
               (multiple-value-bind (other found) (matching-bracket editor p)
                 (when found
                   (list p (if (eq other :mismatch) nil other)))))))
      (or (and (plusp (pos-col pos))
               (let ((before (previous-pos doc pos)))
                 (and (close-bracket-p editor (char-at doc before)) (try before))))
          (and (char-at doc pos) (try pos))))))

;;; ------------------------------------------------------------------
;;; Indentation

(defun line-indentation (editor line)
  "The number of spaces (and tabs) the text of LINE starts with."
  (let ((s (line-string (editor-document editor) line)))
    (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) s) (length s))))

(defmethod compute-indentation ((language language) editor line)
  ;; the indentation of the line before, deeper after an open bracket; a
  ;; line that starts with a closing bracket is under the line of the open one
  (let* ((indent (line-indentation editor line))
         (s (line-string (editor-document editor) line))
         (open (and (< indent (length s))
                    (close-bracket-p editor (char s indent))
                    (matching-bracket editor (make-pos line indent)))))
    (when (pos-p open)
      (return-from compute-indentation (line-indentation editor (pos-line open)))))
  (if (zerop line)
      0
      (let* ((previous (line-string (editor-document editor) (1- line)))
             (trimmed (string-right-trim " 	" previous))
             (base (line-indentation editor (1- line))))
        (if (and (plusp (length trimmed))
                 (member (char trimmed (1- (length trimmed))) '(#\( #\[ #\{ #\:)))
            (+ base (editor-tab-width editor))
            base))))

;;; ------------------------------------------------------------------
;;; Common Lisp
;;;
;;; The state is (MODE DEPTH): MODE is :CODE, :STRING or the nesting of a
;;; #| comment, DEPTH the depth of the parentheses (for their colours).

(defclass lisp-language (language) ()
  (:default-initargs :name "Common Lisp" :comment-start ";; "
                     :brackets '((#\( . #\)))
                     :pairs '((#\( . #\)) (#\" . #\"))))

(defvar *lisp-language* (make-instance 'lisp-language))

(defun find-language (name)
  "The language called NAME (\"lisp\", \"text\"), by name or file extension."
  (let ((name (string-downcase name)))
    (cond ((member name '("lisp" "common lisp" "cl" "lsp" "asd" "el" "scm" "clj") :test #'string=)
           *lisp-language*)
          (t *plain-text*))))

(defparameter *lisp-builtins*
  (let ((table (make-hash-table :test 'equalp)))
    (dolist (name '("defun" "defmacro" "defvar" "defparameter" "defconstant" "defclass"
                    "defmethod" "defgeneric" "defstruct" "deftype" "defpackage" "define-condition"
                    "define-symbol-macro" "define-compiler-macro" "define-modify-macro"
                    "define-setf-expander" "defsetf" "in-package"
                    "lambda" "let" "let*" "flet" "labels" "macrolet" "symbol-macrolet"
                    "if" "when" "unless" "cond" "case" "ecase" "ccase" "typecase" "etypecase"
                    "ctypecase" "and" "or" "not" "progn" "prog1" "prog2" "block" "return" "return-from"
                    "loop" "do" "do*" "dolist" "dotimes" "tagbody" "go" "catch" "throw"
                    "unwind-protect" "handler-case" "handler-bind" "restart-case" "restart-bind"
                    "ignore-errors" "multiple-value-bind" "multiple-value-list" "destructuring-bind"
                    "setf" "setq" "psetf" "incf" "decf" "push" "pop" "pushnew" "quote" "function"
                    "the" "declare" "locally" "eval-when" "load-time-value" "progv" "values"
                    "with-open-file" "with-output-to-string" "with-input-from-string"
                    "with-slots" "with-accessors" "with-standard-io-syntax" "with-simple-restart"
                    "assert" "check-type" "error" "warn" "signal" "funcall" "apply"
                    "make-instance" "call-next-method" "format"
                    "defcomponent" "defmodel" "defstate" "define-interface" "define-command"
                    "define-editor-command" "html"))
      (setf (gethash name table) t))
    table))

(defun def-form-p (name)
  (let ((n (string-downcase name)))
    (or (and (>= (length n) 3) (string= n "def" :end1 3)))))

(defun lisp-delimiter-p (c)
  (member c '(#\Space #\Tab #\( #\) #\" #\; #\' #\` #\,)))

(defun number-token-p (s)
  (let ((start (if (and (plusp (length s)) (member (char s 0) '(#\+ #\-))) 1 0)))
    (and (< start (length s))
         (digit-char-p (char s start))
         (every (lambda (c) (or (digit-char-p c) (member c '(#\. #\/ #\e #\E #\d #\D)))) s))))

(defmethod initial-syntax-state ((language lisp-language)) (list :code 0))

(defmethod word-char-p ((language lisp-language) char)
  (or (alphanumericp char) (find char "-_*+!?<>=/%&$")))

(defmethod tokenize-line ((language lisp-language) s state)
  (let ((mode (first state)) (depth (second state))
        (i 0) (n (length s)) (tokens '()) (after-def nil))
    (flet ((token (start end class) (push (list start end class) tokens)))
      (loop
        (cond
          ((>= i n) (return))
          ;; inside a string
          ((eq mode :string)
           (let ((start i))
             (loop while (< i n)
                   do (let ((c (char s i)))
                        (cond ((char= c #\\) (incf i 2))
                              ((char= c #\") (incf i) (setf mode :code) (return))
                              (t (incf i)))))
             (token start (min i n) "string")))
          ;; inside #| |#, MODE is the nesting
          ((integerp mode)
           (let ((start i))
             (loop while (< i n)
                   do (cond ((and (< (1+ i) n) (char= (char s i) #\|) (char= (char s (1+ i)) #\#))
                             (incf i 2)
                             (when (zerop (decf mode)) (setf mode :code) (return)))
                            ((and (< (1+ i) n) (char= (char s i) #\#) (char= (char s (1+ i)) #\|))
                             (incf i 2) (incf mode))
                            (t (incf i))))
             (token start (min i n) "comment")))
          (t
           (let ((c (char s i)))
             (cond
               ((member c '(#\Space #\Tab)) (incf i))
               ((char= c #\;) (token i n "comment") (setf i n))
               ((char= c #\") (setf mode :string) (token i (1+ i) "string") (incf i)
                ;; the rest of the string is the next token: merge them
                (let ((start (first (first tokens))))
                  (pop tokens)
                  (let ((j (1+ start)))
                    (loop while (< j n)
                          do (let ((d (char s j)))
                               (cond ((char= d #\\) (incf j 2))
                                     ((char= d #\") (incf j) (setf mode :code) (return))
                                     (t (incf j)))))
                    (setf i (min j n))
                    (token start i "string"))))
               ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\|))
                (setf mode 1)
                (let ((start i))
                  (incf i 2)
                  (loop while (< i n)
                        do (cond ((and (< (1+ i) n) (char= (char s i) #\|) (char= (char s (1+ i)) #\#))
                                  (incf i 2)
                                  (when (zerop (decf mode)) (setf mode :code) (return)))
                                 ((and (< (1+ i) n) (char= (char s i) #\#) (char= (char s (1+ i)) #\|))
                                  (incf i 2) (incf mode))
                                 (t (incf i))))
                  (token start (min i n) "comment")))
               ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\\))
                ;; a character: #\x, #\Space, #\(
                (let ((start i))
                  (incf i 3)
                  (loop while (and (< i n) (not (lisp-delimiter-p (char s i))))
                        do (incf i))
                  (token start (min i n) "char")))
               ((char= c #\()
                (token i (1+ i) (format nil "paren paren-~D" (mod depth 6)))
                (incf depth) (incf i))
               ((char= c #\))
                (setf depth (max 0 (1- depth)))
                (token i (1+ i) (format nil "paren paren-~D" (mod depth 6)))
                (incf i))
               ((member c '(#\' #\` #\,))
                (let ((start i))
                  (incf i)
                  (when (and (< i n) (char= (char s i) #\@)) (incf i))
                  (token start i "quote")))
               (t
                (let ((start i))
                  (loop while (and (< i n) (not (lisp-delimiter-p (char s i))))
                        do (if (char= (char s i) #\|)
                               ;; |escaped symbol|
                               (let ((close (position #\| s :start (1+ i))))
                                 (setf i (if close (1+ close) n)))
                               (incf i)))
                  (let* ((word (subseq s start i))
                         (class (cond (after-def "def-name")
                                      ((char= (char word 0) #\:) "keyword")
                                      ((char= (char word 0) #\&) "lambda-key")
                                      ((char= (char word 0) #\#) "reader")
                                      ((number-token-p word) "number")
                                      ((gethash word *lisp-builtins*) "builtin")
                                      ((def-form-p word) "builtin")
                                      ((and (> (length word) 2) (char= (char word 0) #\*)
                                            (char= (char word (1- (length word))) #\*))
                                       "special")
                                      ((member word '("t" "nil") :test #'string-equal) "constant"))))
                    (setf after-def (and (or (gethash word *lisp-builtins*) (def-form-p word))
                                         (def-form-p word)
                                         (not (member word '("defpackage" "in-package") :test #'string-equal))))
                    (when class (token start i class))
                    (when (string= class "def-name") (setf after-def nil)))))))))))
    (values (nreverse tokens) (list mode depth))))

(defparameter *lisp-body-forms*
  (let ((table (make-hash-table :test 'equalp)))
    (dolist (name '("lambda" "let" "let*" "flet" "labels" "macrolet" "symbol-macrolet" "when" "unless"
                    "progn" "prog1" "prog2" "block" "loop" "do" "do*" "dolist" "dotimes" "tagbody"
                    "catch" "unwind-protect" "handler-case" "handler-bind" "restart-case"
                    "multiple-value-bind" "destructuring-bind" "case" "ecase" "ccase" "typecase"
                    "etypecase" "ctypecase" "eval-when" "locally" "progv" "with-open-file"
                    "with-output-to-string" "with-input-from-string" "with-slots" "with-accessors"
                    "ignore-errors" "html"))
      (setf (gethash name table) t))
    table))

(defun lisp-body-form-p (name)
  (let ((n (string-downcase name)))
    (or (gethash n *lisp-body-forms*)
        (def-form-p n)
        (and (> (length n) 5) (string= n "with-" :end1 5))
        (and (> (length n) 3) (string= n "do-" :end1 3)))))

(defun symbol-after (editor pos)
  "The symbol that starts at POS, or NIL."
  (let* ((s (line-string (editor-document editor) (pos-line pos)))
         (start (pos-col pos))
         (end (or (position-if #'lisp-delimiter-p s :start start) (length s))))
    (and (< start end) (subseq s start end))))

(defmethod compute-indentation ((language lisp-language) editor line)
  (let ((state (line-start-state editor line)))
    (unless (eq (first state) :code)
      ;; inside a string or a comment the text is left alone
      (return-from compute-indentation nil))
    (let ((open (enclosing-open editor (make-pos line 0))))
      (if (null open)
          0
          (let* ((doc (editor-document editor))
                 (s (line-string doc (pos-line open)))
                 (col (pos-col open))
                 (prefix (and (plusp col) (char s (1- col))))
                 (operator (symbol-after editor (make-pos (pos-line open) (1+ col)))))
            (cond
              ;; a quoted list or a vector is data
              ((member prefix '(#\' #\` #\#)) (1+ col))
              ((null operator) (1+ col))
              ;; a local function of FLET and friends: (flet ((name (args) body...
              ((let* ((parent (enclosing-open editor open))
                      (grand (and parent (enclosing-open editor parent))))
                 (and grand
                      (= (pos-line parent) (pos-line open))
                      (member (symbol-after editor (make-pos (pos-line grand) (1+ (pos-col grand))))
                              '("flet" "labels" "macrolet") :test #'equalp)))
               (+ col 2))
              ((lisp-body-form-p operator) (+ col 2))
              (t
               ;; under the first argument if it is on the same line
               (let* ((after (+ col 1 (length operator)))
                      (arg (position-if-not (lambda (c) (member c '(#\Space #\Tab))) s :start after)))
                 (if (and arg (not (char= (char s arg) #\;)))
                     arg
                     (1+ col))))))))))

;;; ------------------------------------------------------------------
;;; S-expressions

(defun skip-space-forward (editor pos)
  "The first position at or after POS that is not blank, a newline or in a comment."
  (let ((doc (editor-document editor)))
    (loop for p = pos then (next-pos doc p)
          while p
          do (let ((c (char-at doc p)))
               (unless (or (member c '(#\Space #\Tab #\Newline))
                           (equal (char-class editor p) "comment"))
                 (return p)))
          finally (return (document-end doc)))))

(defun skip-space-backward (editor pos)
  (let ((doc (editor-document editor)))
    (loop for p = pos then prev
          for prev = (previous-pos doc p)
          while prev
          do (let ((c (char-at doc prev)))
               (unless (or (member c '(#\Space #\Tab #\Newline))
                           (equal (char-class editor prev) "comment"))
                 (return p)))
          finally (return (make-pos 0 0)))))

(defun forward-sexp-pos (editor pos)
  "The end of the S-expression after POS, or NIL."
  (let* ((doc (editor-document editor))
         (p (skip-space-forward editor pos))
         (c (char-at doc p)))
    (cond ((null c) nil)
          ((and (eq (char-class editor p) :code) (close-bracket-p editor c)) nil)
          ((and (eq (char-class editor p) :code) (open-bracket-p editor c))
           (let ((m (matching-bracket editor p)))
             (and (pos-p m) (next-pos doc m))))
          ((member (char-class editor p) '("string" "char") :test #'equal)
           (let ((class (char-class editor p)))
             (loop for q = (next-pos doc p) then (next-pos doc q)
                   while (and q (equal (char-class editor q) class)
                              (not (and (equal class "string") (char= (char-at doc q) #\")
                                        (not (eql (char-at doc (previous-pos doc q)) #\\)))))
                   finally (return (if q (or (next-pos doc q) q) (document-end doc))))))
          ((member c '(#\' #\` #\, #\@ #\#))
           (forward-sexp-pos editor (next-pos doc p)))
          (t
           (loop for q = p then (next-pos doc q)
                 while (and q (char-at doc q)
                            (eq (char-class editor q) :code)
                            (not (lisp-delimiter-p (char-at doc q)))
                            (char/= (char-at doc q) #\Newline))
                 finally (return (or q (document-end doc))))))))

(defun backward-sexp-pos (editor pos)
  "The start of the S-expression before POS, or NIL."
  (let* ((doc (editor-document editor))
         (p (skip-space-backward editor pos))
         (q (previous-pos doc p)))
    (when q
      (let ((c (char-at doc q)) (class (char-class editor q)))
        (let ((start
                (cond ((and (eq class :code) (open-bracket-p editor c)) nil)
                      ((and (eq class :code) (close-bracket-p editor c))
                       (let ((m (matching-bracket editor q))) (and (pos-p m) m)))
                      ((not (eq class :code))
                       (loop for r = q then prev
                             for prev = (previous-pos doc r)
                             while (and prev (equal (char-class editor prev) class)
                                        (not (and (equal class "string") (char= (char-at doc r) #\")
                                                  (not (pos= r q)))))
                             finally (return r)))
                      (t
                       (loop for r = q then prev
                             for prev = (previous-pos doc r)
                             while (and prev (eq (char-class editor prev) :code)
                                        (not (lisp-delimiter-p (char-at doc prev)))
                                        (char/= (char-at doc prev) #\Newline))
                             finally (return r))))))
          ;; with the quotes before it
          (when start
            (loop for prev = (previous-pos doc start)
                  while (and prev (member (char-at doc prev) '(#\' #\` #\, #\@ #\#))
                             (eq (char-class editor prev) :code))
                  do (setf start prev)))
          start)))))
