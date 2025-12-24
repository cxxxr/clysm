;;;; parser.lisp - S-expression parsing for Clysm Lisp reader

(in-package #:clysm/reader/parser)

;;; Parser error condition

(define-condition parse-error (error)
  ((message :initarg :message :reader parse-error-message)
   (line :initarg :line :reader parse-error-line :initform nil)
   (column :initarg :column :reader parse-error-column :initform nil))
  (:report (lambda (c s)
             (if (parse-error-line c)
                 (format s "Parse error at line ~D, column ~D: ~A"
                         (parse-error-line c)
                         (parse-error-column c)
                         (parse-error-message c))
                 (format s "Parse error: ~A" (parse-error-message c))))))

(defun parser-error (message &optional line column)
  "Signal a parse error."
  (error 'parse-error :message message :line line :column column))

;;; Parser state

(defstruct parser-state
  "Parser state tracking token position."
  (tokens nil :type list)
  (position 0 :type fixnum))

(defun current-token (state)
  "Get current token without consuming."
  (nth (parser-state-position state) (parser-state-tokens state)))

(defun advance-token (state)
  "Consume current token and return it."
  (let ((token (current-token state)))
    (when token
      (incf (parser-state-position state)))
    token))

(defun token-type (token)
  "Get the type of a token."
  (first token))

(defun token-value (token)
  "Get the value of a token."
  (second token))

(defun token-line (token)
  "Get the line of a token."
  (third token))

(defun token-column (token)
  "Get the column of a token."
  (fourth token))

;;; Parsing functions

(defun parse-atom (token)
  "Parse an atomic token into a Lisp value."
  (let ((type (token-type token))
        (value (token-value token)))
    (case type
      (:number value)
      (:ratio value)    ; T017: ratio literal
      (:float value)    ; T017: float literal
      (:string value)
      (:character value)  ; 008-character-string: pass character through
      (:symbol
       ;; Handle special symbols
       (cond
         ((string= value "NIL") nil)
         ((string= value "T") t)
         (t (intern value))))
      (:keyword
       (intern value "KEYWORD"))
      (otherwise
       (parser-error (format nil "Unexpected token type: ~A" type)
                     (token-line token) (token-column token))))))

(defun parse-list (state)
  "Parse a list (after opening paren has been consumed)."
  (let ((elements '()))
    (loop
      (let ((token (current-token state)))
        (cond
          ;; End of tokens
          ((null token)
           (parser-error "Unexpected end of input, expected )"))
          ;; End of list
          ((eq (token-type token) :rparen)
           (advance-token state)
           (return (nreverse elements)))
          ;; Dotted pair
          ((eq (token-type token) :dot)
           (advance-token state)
           ;; Parse the cdr
           (let ((cdr-value (parse-expr state)))
             ;; Expect closing paren
             (let ((close-token (current-token state)))
               (unless (and close-token (eq (token-type close-token) :rparen))
                 (parser-error "Expected ) after dotted pair"
                               (when close-token (token-line close-token))
                               (when close-token (token-column close-token))))
               (advance-token state)
               ;; Build improper list - elements is in reverse order
               ;; We need to build: (e1 e2 ... eN . cdr-value)
               ;; elements = (eN ... e2 e1)
               (return
                 (if (null elements)
                     cdr-value
                     ;; Build from the end: start with cdr-value, prepend elements
                     (let ((result cdr-value))
                       (dolist (elem elements)  ; elements already in reverse order
                         (setf result (cons elem result)))
                       result))))))
          ;; Regular element
          (t
           (push (parse-expr state) elements)))))))

(defun parse-quoted (state quote-symbol)
  "Parse a quoted expression."
  (list quote-symbol (parse-expr state)))

(defun parse-complex-literal (state)
  "Parse a complex literal #C(real imag) (T017).
   Called after #C has been consumed."
  ;; Expect opening paren
  (let ((lparen (advance-token state)))
    (unless (and lparen (eq (token-type lparen) :lparen))
      (parser-error "Expected ( after #C"
                    (when lparen (token-line lparen))
                    (when lparen (token-column lparen))))
    ;; Parse real part
    (let ((real-part (parse-expr state)))
      ;; Parse imaginary part
      (let ((imag-part (parse-expr state)))
        ;; Expect closing paren
        (let ((rparen (advance-token state)))
          (unless (and rparen (eq (token-type rparen) :rparen))
            (parser-error "Expected ) after complex imaginary part"
                          (when rparen (token-line rparen))
                          (when rparen (token-column rparen))))
          ;; Construct complex number
          (complex real-part imag-part))))))

(defun parse-expr (state)
  "Parse a single expression from the parser state."
  (let ((token (advance-token state)))
    (unless token
      (parser-error "Unexpected end of input"))
    (case (token-type token)
      ;; Atoms
      ((:number :ratio :float :string :symbol :keyword :character)
       (parse-atom token))
      ;; List
      (:lparen
       (parse-list state))
      ;; Quote shorthands
      (:quote
       (parse-quoted state 'quote))
      (:backquote
       (parse-quoted state 'quasiquote))
      (:unquote
       (parse-quoted state 'unquote))
      (:unquote-splicing
       (parse-quoted state 'unquote-splicing))
      ;; Complex literal #C(real imag) - T017
      (:complex-start
       (parse-complex-literal state))
      ;; Unexpected tokens
      (:rparen
       (parser-error "Unexpected )"
                     (token-line token) (token-column token)))
      (:dot
       (parser-error "Unexpected . outside of list"
                     (token-line token) (token-column token)))
      (otherwise
       (parser-error (format nil "Unknown token type: ~A" (token-type token))
                     (token-line token) (token-column token))))))

;;; Main parsing entry point

(defun parse (tokens)
  "Parse tokens into an S-expression.
   Returns NIL for empty input."
  (when (null tokens)
    (return-from parse nil))
  (let ((state (make-parser-state :tokens tokens)))
    (parse-expr state)))

(defun parse-all (tokens)
  "Parse all expressions from tokens.
   Returns a list of expressions."
  (when (null tokens)
    (return-from parse-all nil))
  (let ((state (make-parser-state :tokens tokens))
        (results '()))
    (loop
      (unless (current-token state)
        (return (nreverse results)))
      (push (parse-expr state) results))))
