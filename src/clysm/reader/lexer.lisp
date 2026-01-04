;;;; reader/lexer.lisp - S-expression Lexer
;;;;
;;;; Tokenizes text input into a stream of tokens for the parser.
;;;; No external dependencies.

(in-package #:clysm)

;;; ============================================================
;;; Token Types
;;; ============================================================

(defstruct token
  "A lexical token."
  type      ; Token type keyword
  value     ; Token value
  line      ; Line number
  column)   ; Column number

;;; Token types:
;;; :number      - integer or float
;;; :symbol      - symbol name (string)
;;; :string      - string literal
;;; :char        - character literal
;;; :lparen      - (
;;; :rparen      - )
;;; :dot         - .
;;; :quote       - '
;;; :backquote   - `
;;; :comma       - ,
;;; :comma-at    - ,@
;;; :eof         - end of input

;;; ============================================================
;;; Lexer State
;;; ============================================================

(defstruct lexer
  "Lexer state for reading tokens from a string."
  input       ; Input string
  position    ; Current position in string
  length      ; Length of input string
  line        ; Current line number (1-based)
  column      ; Current column number (1-based)
  peeked)     ; Peeked token (for lookahead)

(defun make-lexer-from-string (string)
  "Create a new lexer for the given string."
  (make-lexer :input string
              :position 0
              :length (length string)
              :line 1
              :column 1
              :peeked nil))

;;; ============================================================
;;; Character Access
;;; ============================================================

(defun lexer-eof-p (lexer)
  "Check if lexer has reached end of input."
  (>= (lexer-position lexer) (lexer-length lexer)))

(defun lexer-peek-char (lexer)
  "Peek at the current character without consuming it."
  (if (lexer-eof-p lexer)
      nil
      (char (lexer-input lexer) (lexer-position lexer))))

(defun lexer-read-char (lexer)
  "Read and consume the current character."
  (let ((ch (lexer-peek-char lexer)))
    (when ch
      (incf (lexer-position lexer))
      (if (char= ch #\Newline)
          (progn
            (incf (lexer-line lexer))
            (setf (lexer-column lexer) 1))
          (incf (lexer-column lexer))))
    ch))

(defun lexer-peek-char-at (lexer offset)
  "Peek at character at position + offset."
  (let ((pos (+ (lexer-position lexer) offset)))
    (if (< pos (lexer-length lexer))
        (char (lexer-input lexer) pos)
        nil)))

;;; ============================================================
;;; Character Classification
;;; ============================================================

(defun whitespacep (ch)
  "Check if character is whitespace."
  (and ch (member ch '(#\Space #\Tab #\Newline #\Return))))

(defun delimiterp (ch)
  "Check if character is a delimiter."
  (and ch (member ch '(#\( #\) #\' #\` #\, #\" #\; #\Space #\Tab #\Newline #\Return))))

(defun digit-char-p* (ch)
  "Check if character is a digit. Returns digit value or nil."
  (and ch
       (char<= #\0 ch #\9)
       (- (char-code ch) (char-code #\0))))

(defun symbol-char-p (ch)
  "Check if character can be part of a symbol."
  (and ch
       (not (whitespacep ch))
       (not (member ch '(#\( #\) #\' #\` #\, #\" #\;)))))

;;; ============================================================
;;; Skip Whitespace and Comments
;;; ============================================================

(defun skip-whitespace-and-comments (lexer)
  "Skip whitespace and line comments."
  (loop
    (let ((ch (lexer-peek-char lexer)))
      (cond
        ((null ch)
         (return))
        ((whitespacep ch)
         (lexer-read-char lexer))
        ((char= ch #\;)
         ;; Skip line comment
         (loop
           (let ((c (lexer-read-char lexer)))
             (when (or (null c) (char= c #\Newline))
               (return)))))
        (t
         (return))))))

;;; ============================================================
;;; Token Reading
;;; ============================================================

(defun read-number-token (lexer)
  "Read a number token (integer or float)."
  (let ((start-line (lexer-line lexer))
        (start-col (lexer-column lexer))
        (chars nil)
        (has-dot nil))
    ;; Read sign if present
    (when (member (lexer-peek-char lexer) '(#\+ #\-))
      (push (lexer-read-char lexer) chars))
    ;; Read digits
    (loop
      (let ((ch (lexer-peek-char lexer)))
        (cond
          ((digit-char-p* ch)
           (push (lexer-read-char lexer) chars))
          ((and (eql ch #\.) (not has-dot))
           ;; Check if this is a decimal point or a dot delimiter
           (let ((next (lexer-peek-char-at lexer 1)))
             (if (and next (digit-char-p* next))
                 (progn
                   (setf has-dot t)
                   (push (lexer-read-char lexer) chars))
                 (return))))
          (t
           (return)))))
    (let ((str (coerce (nreverse chars) 'string)))
      (make-token :type :number
                  :value (if has-dot
                             (read-from-string str)
                             (parse-integer str))
                  :line start-line
                  :column start-col))))

(defun read-string-token (lexer)
  "Read a string literal token."
  (let ((start-line (lexer-line lexer))
        (start-col (lexer-column lexer))
        (chars nil))
    ;; Skip opening quote
    (lexer-read-char lexer)
    ;; Read string contents
    (loop
      (let ((ch (lexer-read-char lexer)))
        (cond
          ((null ch)
           (error "Unterminated string starting at line ~D, column ~D"
                  start-line start-col))
          ((char= ch #\")
           (return))
          ((char= ch #\\)
           ;; Escape sequence
           (let ((escaped (lexer-read-char lexer)))
             (case escaped
               (#\n (push #\Newline chars))
               (#\t (push #\Tab chars))
               (#\r (push #\Return chars))
               (#\" (push #\" chars))
               (#\\ (push #\\ chars))
               (t (push escaped chars)))))
          (t
           (push ch chars)))))
    (make-token :type :string
                :value (coerce (nreverse chars) 'string)
                :line start-line
                :column start-col)))

(defun read-symbol-token (lexer)
  "Read a symbol token."
  (let ((start-line (lexer-line lexer))
        (start-col (lexer-column lexer))
        (chars nil))
    (loop
      (let ((ch (lexer-peek-char lexer)))
        (if (symbol-char-p ch)
            (push (lexer-read-char lexer) chars)
            (return))))
    (let ((str (string-upcase (coerce (nreverse chars) 'string))))
      (make-token :type :symbol
                  :value str
                  :line start-line
                  :column start-col))))

(defun read-char-token (lexer)
  "Read a character literal token (#\\x)."
  (let ((start-line (lexer-line lexer))
        (start-col (lexer-column lexer)))
    ;; Skip #
    (lexer-read-char lexer)
    ;; Expect backslash
    (let ((ch (lexer-read-char lexer)))
      (unless (and ch (char= ch #\\))
        (error "Expected \\ after # at line ~D, column ~D" start-line start-col)))
    ;; Read first character (always read at least one)
    (let ((first-char (lexer-read-char lexer)))
      (unless first-char
        (error "Expected character after #\\ at line ~D, column ~D" start-line start-col))
      ;; Check if it's a multi-char name or single char
      (let ((next (lexer-peek-char lexer)))
        (if (and next (alpha-char-p next) (alpha-char-p first-char))
            ;; Multi-char name like "Space", "Newline"
            (let ((chars (list first-char)))
              (loop
                (let ((ch (lexer-peek-char lexer)))
                  (if (and ch (alpha-char-p ch))
                      (push (lexer-read-char lexer) chars)
                      (return))))
              (let* ((name (string-upcase (coerce (nreverse chars) 'string)))
                     (char-value
                       (cond
                         ((string= name "SPACE") #\Space)
                         ((string= name "NEWLINE") #\Newline)
                         ((string= name "TAB") #\Tab)
                         ((string= name "RETURN") #\Return)
                         (t (error "Unknown character name: ~A" name)))))
                (make-token :type :char
                            :value char-value
                            :line start-line
                            :column start-col)))
            ;; Single character - preserve case
            (make-token :type :char
                        :value first-char
                        :line start-line
                        :column start-col))))))

(defun read-token (lexer)
  "Read the next token from the lexer."
  ;; Return peeked token if available
  (when (lexer-peeked lexer)
    (return-from read-token (prog1 (lexer-peeked lexer)
                              (setf (lexer-peeked lexer) nil))))

  (skip-whitespace-and-comments lexer)

  (let ((line (lexer-line lexer))
        (col (lexer-column lexer))
        (ch (lexer-peek-char lexer)))
    (cond
      ((null ch)
       (make-token :type :eof :value nil :line line :column col))

      ((char= ch #\()
       (lexer-read-char lexer)
       (make-token :type :lparen :value #\( :line line :column col))

      ((char= ch #\))
       (lexer-read-char lexer)
       (make-token :type :rparen :value #\) :line line :column col))

      ((char= ch #\')
       (lexer-read-char lexer)
       (make-token :type :quote :value #\' :line line :column col))

      ((char= ch #\`)
       (lexer-read-char lexer)
       (make-token :type :backquote :value #\` :line line :column col))

      ((char= ch #\,)
       (lexer-read-char lexer)
       (if (eql (lexer-peek-char lexer) #\@)
           (progn
             (lexer-read-char lexer)
             (make-token :type :comma-at :value ",@" :line line :column col))
           (make-token :type :comma :value #\, :line line :column col)))

      ((char= ch #\")
       (read-string-token lexer))

      ((char= ch #\#)
       ;; Dispatch macro character
       (let ((next (lexer-peek-char-at lexer 1)))
         (cond
           ((eql next #\\)
            (read-char-token lexer))
           ((eql next #\')
            ;; #'function
            (lexer-read-char lexer)
            (lexer-read-char lexer)
            (make-token :type :function :value "#'" :line line :column col))
           (t
            (error "Unknown dispatch character: #~A at line ~D, column ~D"
                   next line col)))))

      ((char= ch #\.)
       ;; Check if it's a dot or start of a number
       (let ((next (lexer-peek-char-at lexer 1)))
         (if (and next (digit-char-p* next))
             (read-number-token lexer)
             (progn
               (lexer-read-char lexer)
               (make-token :type :dot :value #\. :line line :column col)))))

      ((or (digit-char-p* ch)
           (and (member ch '(#\+ #\-))
                (let ((next (lexer-peek-char-at lexer 1)))
                  (and next (digit-char-p* next)))))
       (read-number-token lexer))

      ((symbol-char-p ch)
       (read-symbol-token lexer))

      (t
       (error "Unexpected character: ~A at line ~D, column ~D" ch line col)))))

(defun peek-token (lexer)
  "Peek at the next token without consuming it."
  (unless (lexer-peeked lexer)
    (setf (lexer-peeked lexer) (read-token lexer)))
  (lexer-peeked lexer))
