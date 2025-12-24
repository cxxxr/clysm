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
  "Read a number or symbol token.
   Handles:
   - Integers: 123, -456
   - Ratios: 1/2, -3/4 (T017)
   - Floats: 1.5, -2.5, 1.5d0, 1.5e10 (T017)
   - Symbols: foo, *bar*"
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
        ;; Try parsing as a number (integer, ratio, or float)
        ((parse-numeric-literal str start-line start-column))
        ;; Symbol
        (t
         (list :symbol upcase-str start-line start-column))))))

;;; ============================================================
;;; Numeric Literal Parsing (T017)
;;; ============================================================

(defun parse-numeric-literal (str line column)
  "Parse a string as a numeric literal (T017).
   Returns token list or NIL if not a valid numeric literal.
   Handles:
   - Integers: 123, -456
   - Ratios: 1/2, -3/4
   - Floats: 1.5, -2.5, 1.5d0, 1.5D0, 1.5e10, 1.5E10
   Note: Does NOT treat symbols containing 'd' or 'e' as floats."
  (let* ((len (length str))
         (has-sign (and (> len 0)
                        (or (char= (char str 0) #\-)
                            (char= (char str 0) #\+))))
         (sign (if (and has-sign (char= (char str 0) #\-)) -1 1))
         (start (if has-sign 1 0)))
    (when (>= start len)
      (return-from parse-numeric-literal nil))

    ;; First character after sign must be a digit for any numeric literal
    (unless (digit-char-p (char str start))
      (return-from parse-numeric-literal nil))

    ;; Check for ratio: digits/digits
    (let ((slash-pos (position #\/ str :start start)))
      (when slash-pos
        (return-from parse-numeric-literal
          (parse-ratio-literal str slash-pos sign start line column))))

    ;; Check for float: must have a dot, OR all digits followed by exponent marker + digits
    ;; Examples: 1.5, 1.5d0, 1.5e10, 1d0, 1e10
    (let ((dot-pos (position #\. str :start start)))
      ;; If there's a dot, it's a float candidate
      (when dot-pos
        (return-from parse-numeric-literal
          (parse-float-literal str dot-pos nil sign start line column)))

      ;; Check for exponent-only format: NNNdNN or NNNeNN
      ;; The part before the exponent marker must be all digits
      (let ((exp-pos nil))
        (loop for i from start below len
              for ch = (char str i)
              do (cond
                   ((digit-char-p ch) nil)  ; OK
                   ((member ch '(#\e #\E #\d #\D))
                    (setf exp-pos i)
                    (return))
                   (t (return))))  ; non-digit, non-exponent = not a number
        (when (and exp-pos
                   (> exp-pos start)  ; Must have digits before exponent
                   (every #'digit-char-p (subseq str start exp-pos)))
          (return-from parse-numeric-literal
            (parse-float-literal str nil exp-pos sign start line column)))))

    ;; Try integer - all digits
    (let ((digits (subseq str start)))
      (when (and (> (length digits) 0)
                 (every #'digit-char-p digits))
        (return-from parse-numeric-literal
          (list :number (* sign (parse-integer digits)) line column))))

    ;; Not a number
    nil))

(defun parse-ratio-literal (str slash-pos sign start line column)
  "Parse a ratio literal like 1/2 or -3/4 (T017)."
  (let ((num-str (subseq str start slash-pos))
        (den-str (subseq str (1+ slash-pos))))
    (when (and (> (length num-str) 0)
               (every #'digit-char-p num-str)
               (> (length den-str) 0)
               (every #'digit-char-p den-str))
      (let ((numerator (* sign (parse-integer num-str)))
            (denominator (parse-integer den-str)))
        (when (> denominator 0)
          (list :ratio (/ numerator denominator) line column))))))

(defun parse-float-literal (str dot-pos exp-pos sign start line column)
  "Parse a float literal like 1.5, 1.5d0, 1.5e10 (T017)."
  (let* ((len (length str))
         ;; Extract mantissa and exponent parts
         (mantissa-end (or exp-pos len))
         (mantissa-str (subseq str start mantissa-end))
         (exp-value 0))
    ;; Parse exponent if present
    (when exp-pos
      (let ((exp-str (subseq str (1+ exp-pos))))
        (when (and (> (length exp-str) 0)
                   (or (every #'digit-char-p exp-str)
                       (and (or (char= (char exp-str 0) #\-)
                                (char= (char exp-str 0) #\+))
                            (> (length exp-str) 1)
                            (every #'digit-char-p (subseq exp-str 1)))))
          (setf exp-value (parse-integer exp-str)))))

    ;; Validate mantissa
    (when dot-pos
      ;; Must have digits on at least one side of the dot
      (let* ((local-dot (- dot-pos start))
             (before-dot (subseq mantissa-str 0 local-dot))
             (after-dot (subseq mantissa-str (1+ local-dot))))
        (unless (and (or (> (length before-dot) 0) (> (length after-dot) 0))
                     (or (zerop (length before-dot))
                         (every #'digit-char-p before-dot))
                     (or (zerop (length after-dot))
                         (every #'digit-char-p after-dot)))
          (return-from parse-float-literal nil))))

    ;; Parse and construct the float
    (let ((mantissa (if dot-pos
                        (read-from-string mantissa-str)
                        (parse-integer mantissa-str))))
      (when mantissa
        (let ((result (* sign mantissa (expt 10.0d0 exp-value))))
          (list :float result line column))))))

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

;;; ============================================================
;;; Character Literal Reading (008-character-string)
;;; ============================================================

(defparameter *named-characters*
  '(("SPACE" . #\Space)
    ("NEWLINE" . #\Newline)
    ("LINEFEED" . #\Newline)
    ("TAB" . #\Tab)
    ("RETURN" . #\Return)
    ("PAGE" . #\Page)
    ("BACKSPACE" . #\Backspace)
    ("RUBOUT" . #\Rubout))
  "Named character mappings for #\\Name syntax.")

(defun read-character-token (tokenizer)
  "Read a character literal token (#\\x or #\\Name).
   Handles:
   - Single characters: #\\a, #\\A, #\\0, #\\!, etc.
   - Named characters: #\\Space, #\\Newline, #\\Tab, #\\Return, etc."
  ;; At this point, we've consumed '#' and '\\'
  (let* ((line (tokenizer-line tokenizer))
         (column (tokenizer-column tokenizer))
         (first-char (read-char* tokenizer)))
    (cond
      ((null first-char)
       (error "Unexpected end of input after #\\"))
      ;; Check if it's a single character (followed by terminator)
      ((terminating-char-p (peek-char* tokenizer))
       (list :character first-char line column))
      ;; Could be a named character - collect the full name
      (t
       (let ((chars (list first-char)))
         (loop
           (let ((char (peek-char* tokenizer)))
             (if (and char (alphanumericp char))
                 (push (read-char* tokenizer) chars)
                 (return))))
         (let* ((name (string-upcase (coerce (nreverse chars) 'string)))
                (named-char (cdr (assoc name *named-characters* :test #'string=))))
           (if named-char
               (list :character named-char line column)
               ;; Single character case: name is just one char
               (if (= 1 (length name))
                   (list :character (char name 0) line column)
                   (error "Unknown character name: ~A" name)))))))))

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
      ;; Dispatch macro (#\, #', etc.) - 008-character-string
      ((char= char #\#)
       (read-char* tokenizer)  ; consume #
       (let ((dispatch-char (peek-char* tokenizer)))
         (cond
           ;; Character literal: #\x
           ((eql dispatch-char #\\)
            (read-char* tokenizer)  ; consume backslash
            (read-character-token tokenizer))
           ;; Function reference: #'fn (placeholder for now)
           ((eql dispatch-char #\')
            (read-char* tokenizer)  ; consume quote
            (list :function nil line column))
           ;; Hexadecimal: #xNNN
           ((eql dispatch-char #\x)
            (read-char* tokenizer)  ; consume x
            (let ((chars '()))
              (loop
                (let ((c (peek-char* tokenizer)))
                  (if (and c (or (digit-char-p c) (find c "abcdefABCDEF")))
                      (push (read-char* tokenizer) chars)
                      (return))))
              (if (null chars)
                  (error "Invalid hexadecimal number after #x")
                  (list :number (parse-integer (coerce (nreverse chars) 'string) :radix 16)
                        line column))))
           ;; Complex number literal: #C(real imag) - T017
           ((or (eql dispatch-char #\c) (eql dispatch-char #\C))
            (read-char* tokenizer)  ; consume c/C
            (list :complex-start nil line column))
           (t
            (error "Unknown dispatch macro character: #~A" dispatch-char)))))
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
