;;;; tokenizer.lisp - Lexical analysis for Clysm Lisp reader

(in-package #:clysm/reader/tokenizer)

;;; Tokenizer structure

(defstruct tokenizer
  "Tokenizer state."
  (input "" :type string)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 0 :type fixnum))

(defun make-tokenizer-from-string (string)
  "Create a tokenizer from a string."
  (make-tokenizer :input string))

;;; Character predicates

(defun whitespace-char-p (char)
  "Check if character is whitespace."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun terminating-char-p (char)
  "Check if character terminates a token."
  (or (whitespace-char-p char)
      (member char '(#\( #\) #\' #\` #\, #\" #\; nil))))

(defun symbol-char-p (char)
  "Check if character is valid in a symbol."
  (and char
       (not (terminating-char-p char))
       (not (member char '(#\[ #\] #\{ #\})))))

(defun digit-char-p* (char)
  "Check if character is a digit."
  (and char (digit-char-p char)))

;;; Character reading

(defun peek-char* (tokenizer)
  "Peek at the current character without consuming."
  (let ((pos (tokenizer-position tokenizer))
        (input (tokenizer-input tokenizer)))
    (when (< pos (length input))
      (char input pos))))

(defun read-char* (tokenizer)
  "Read and consume the current character."
  (let ((char (peek-char* tokenizer)))
    (when char
      (incf (tokenizer-position tokenizer))
      (if (char= char #\Newline)
          (progn
            (incf (tokenizer-line tokenizer))
            (setf (tokenizer-column tokenizer) 0))
          (incf (tokenizer-column tokenizer))))
    char))

(defun unread-char* (tokenizer)
  "Push back one character (decrement position)."
  (when (> (tokenizer-position tokenizer) 0)
    (decf (tokenizer-position tokenizer))
    (decf (tokenizer-column tokenizer))))

;;; Skip whitespace and comments

(defun skip-whitespace (tokenizer)
  "Skip whitespace and comments."
  (loop
    (let ((char (peek-char* tokenizer)))
      (cond
        ((null char) (return))
        ((whitespace-char-p char)
         (read-char* tokenizer))
        ((char= char #\;)
         ;; Skip to end of line
         (loop (let ((c (read-char* tokenizer)))
                 (when (or (null c) (char= c #\Newline))
                   (return)))))
        (t (return))))))

;;; Token reading

(defun read-string-token (tokenizer)
  "Read a string token."
  (read-char* tokenizer)  ; consume opening quote
  (let ((chars '()))
    (loop
      (let ((char (read-char* tokenizer)))
        (cond
          ((null char)
           (error "Unterminated string"))
          ((char= char #\")
           (return (list :string (coerce (nreverse chars) 'string))))
          ((char= char #\\)
           ;; Handle escape sequences
           (let ((next (read-char* tokenizer)))
             (cond
               ((null next) (error "Unterminated string"))
               ((char= next #\n) (push #\Newline chars))
               ((char= next #\t) (push #\Tab chars))
               ((char= next #\") (push #\" chars))
               ((char= next #\\) (push #\\ chars))
               (t (push next chars)))))
          (t (push char chars)))))))

(defun read-number-or-symbol (tokenizer)
  "Read a number or symbol token."
  (let ((chars '())
        (start-line (tokenizer-line tokenizer))
        (start-column (tokenizer-column tokenizer)))
    (loop
      (let ((char (peek-char* tokenizer)))
        (if (symbol-char-p char)
            (progn
              (push (read-char* tokenizer) chars)
              t)
            (return))))
    (let* ((str (coerce (nreverse chars) 'string))
           (upcase-str (string-upcase str)))
      ;; Try to parse as number
      (cond
        ;; Empty string shouldn't happen
        ((zerop (length str))
         nil)
        ;; Negative number
        ((and (char= (char str 0) #\-)
              (> (length str) 1)
              (every #'digit-char-p (subseq str 1)))
         (list :number (parse-integer str) start-line start-column))
        ;; Positive number
        ((every #'digit-char-p str)
         (list :number (parse-integer str) start-line start-column))
        ;; Symbol
        (t
         (list :symbol upcase-str start-line start-column))))))

(defun read-keyword-token (tokenizer)
  "Read a keyword token."
  (read-char* tokenizer)  ; consume colon
  (let ((chars '()))
    (loop
      (let ((char (peek-char* tokenizer)))
        (if (symbol-char-p char)
            (push (read-char* tokenizer) chars)
            (return))))
    (list :keyword (string-upcase (coerce (nreverse chars) 'string)))))

;;; Main tokenization

(defun next-token (tokenizer)
  "Get the next token from the tokenizer.
   Returns a token as (type value [line column]) or NIL at end."
  (skip-whitespace tokenizer)
  (let ((char (peek-char* tokenizer))
        (line (tokenizer-line tokenizer))
        (column (tokenizer-column tokenizer)))
    (cond
      ((null char) nil)
      ;; String
      ((char= char #\")
       (read-string-token tokenizer))
      ;; Left paren
      ((char= char #\()
       (read-char* tokenizer)
       (list :lparen nil line column))
      ;; Right paren
      ((char= char #\))
       (read-char* tokenizer)
       (list :rparen nil line column))
      ;; Quote
      ((char= char #\')
       (read-char* tokenizer)
       (list :quote nil line column))
      ;; Backquote
      ((char= char #\`)
       (read-char* tokenizer)
       (list :backquote nil line column))
      ;; Comma (unquote or unquote-splicing)
      ((char= char #\,)
       (read-char* tokenizer)
       (if (eql (peek-char* tokenizer) #\@)
           (progn
             (read-char* tokenizer)
             (list :unquote-splicing nil line column))
           (list :unquote nil line column)))
      ;; Dot
      ((char= char #\.)
       (read-char* tokenizer)
       ;; Check if it's a standalone dot or part of a symbol/number
       (if (terminating-char-p (peek-char* tokenizer))
           (list :dot nil line column)
           (progn
             (unread-char* tokenizer)
             (read-number-or-symbol tokenizer))))
      ;; Keyword
      ((char= char #\:)
       (read-keyword-token tokenizer))
      ;; Number or symbol
      (t
       (read-number-or-symbol tokenizer)))))

(defun tokenize (string)
  "Tokenize a string into a list of tokens."
  (let ((tokenizer (make-tokenizer :input string))
        (tokens '()))
    (loop
      (let ((token (next-token tokenizer)))
        (if token
            (push token tokens)
            (return (nreverse tokens)))))))
