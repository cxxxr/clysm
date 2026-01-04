;;;; reader/parser.lisp - S-expression Parser
;;;;
;;;; Parses tokens into Lisp data structures.
;;;; No external dependencies.

(in-package #:clysm)

;;; ============================================================
;;; Parser State
;;; ============================================================

(defstruct reader-state
  "Reader state including symbol table and packages."
  (symbols (make-hash-table :test 'equal))   ; String -> Symbol
  (current-package "CLYSM-USER"))            ; Current package name

(defvar *reader-state* nil
  "Current reader state for symbol interning.")

;;; ============================================================
;;; Symbol Interning
;;; ============================================================

(defun ensure-reader-state ()
  "Ensure *reader-state* is initialized."
  (unless *reader-state*
    (setf *reader-state* (make-reader-state))))

(defun reader-intern (name &optional (state *reader-state*))
  "Intern a symbol name into the reader's symbol table.
Returns the interned symbol. Uses host Lisp's intern for now."
  (ensure-reader-state)
  (let* ((key (string-upcase name))
         (table (reader-state-symbols state)))
    (or (gethash key table)
        (setf (gethash key table)
              ;; For now, use host Lisp's intern
              ;; This will be replaced with Wasm symbols later
              (intern key :clysm-user)))))

(defun reader-find-symbol (name &optional (state *reader-state*))
  "Find a symbol by name without interning."
  (ensure-reader-state)
  (gethash (string-upcase name) (reader-state-symbols state)))

;;; ============================================================
;;; S-Expression Parser
;;; ============================================================

(defun parse-sexpr (lexer)
  "Parse a single S-expression from the lexer."
  (let ((token (read-token lexer)))
    (case (token-type token)
      (:eof
       (values nil nil))  ; No more expressions

      (:number
       (values (token-value token) t))

      (:string
       (values (token-value token) t))

      (:char
       (values (token-value token) t))

      (:symbol
       (let ((name (token-value token)))
         (values
          (cond
            ((string= name "NIL") nil)
            ((string= name "T") t)
            (t (reader-intern name)))
          t)))

      (:quote
       (multiple-value-bind (expr valid)
           (parse-sexpr lexer)
         (unless valid
           (error "Expected expression after ' at line ~D, column ~D"
                  (token-line token) (token-column token)))
         (values (list 'quote expr) t)))

      (:backquote
       (multiple-value-bind (expr valid)
           (parse-sexpr lexer)
         (unless valid
           (error "Expected expression after ` at line ~D, column ~D"
                  (token-line token) (token-column token)))
         ;; Expand backquote using host Lisp's quasiquote
         (values (expand-backquote expr) t)))

      (:comma
       ;; Comma outside backquote - will be handled by expand-backquote
       (multiple-value-bind (expr valid)
           (parse-sexpr lexer)
         (unless valid
           (error "Expected expression after , at line ~D, column ~D"
                  (token-line token) (token-column token)))
         (values (list 'clysm-unquote expr) t)))

      (:comma-at
       ;; Comma-at outside backquote
       (multiple-value-bind (expr valid)
           (parse-sexpr lexer)
         (unless valid
           (error "Expected expression after ,@ at line ~D, column ~D"
                  (token-line token) (token-column token)))
         (values (list 'clysm-unquote-splicing expr) t)))

      (:function
       ;; #'function -> (function ...)
       (multiple-value-bind (expr valid)
           (parse-sexpr lexer)
         (unless valid
           (error "Expected expression after #' at line ~D, column ~D"
                  (token-line token) (token-column token)))
         (values (list 'function expr) t)))

      (:lparen
       (values (parse-list lexer token) t))

      (:rparen
       (error "Unexpected ) at line ~D, column ~D"
              (token-line token) (token-column token)))

      (:dot
       (error "Unexpected . at line ~D, column ~D"
              (token-line token) (token-column token)))

      (t
       (error "Unexpected token: ~S" token)))))

(defun parse-list (lexer start-token)
  "Parse a list after the opening parenthesis."
  (let ((elements nil))
    (loop
      (let ((token (peek-token lexer)))
        (case (token-type token)
          (:eof
           (error "Unterminated list starting at line ~D, column ~D"
                  (token-line start-token) (token-column start-token)))

          (:rparen
           (read-token lexer)  ; Consume the )
           (return (nreverse elements)))

          (:dot
           (read-token lexer)  ; Consume the .
           ;; Parse the cdr
           (multiple-value-bind (cdr-expr valid)
               (parse-sexpr lexer)
             (unless valid
               (error "Expected expression after . at line ~D, column ~D"
                      (token-line token) (token-column token)))
             ;; Expect closing paren
             (let ((close-token (read-token lexer)))
               (unless (eq (token-type close-token) :rparen)
                 (error "Expected ) after dotted pair at line ~D, column ~D"
                        (token-line close-token) (token-column close-token))))
             ;; Build dotted list
             (return
               (let ((result cdr-expr))
                 (dolist (elem elements result)
                   (setf result (cons elem result)))))))

          (t
           (multiple-value-bind (expr valid)
               (parse-sexpr lexer)
             (unless valid
               (error "Unexpected end of input in list"))
             (push expr elements))))))))

;;; ============================================================
;;; Backquote Expansion
;;; ============================================================

(defun expand-backquote (form)
  "Expand backquoted form into list/cons/append operations."
  (cond
    ((null form)
     nil)

    ((atom form)
     (list 'quote form))

    ((eq (car form) 'clysm-unquote)
     ;; ,x -> x
     (second form))

    ((eq (car form) 'clysm-unquote-splicing)
     ;; ,@x outside list context
     (error ",@ not inside a list"))

    ((consp form)
     ;; Process list elements
     (expand-backquote-list form))

    (t
     (list 'quote form))))

(defun expand-backquote-list (forms)
  "Expand a backquoted list."
  (let ((parts nil))
    (dolist (elem forms)
      (cond
        ((and (consp elem) (eq (car elem) 'clysm-unquote))
         ;; ,x -> (list x)
         (push (list 'list (second elem)) parts))

        ((and (consp elem) (eq (car elem) 'clysm-unquote-splicing))
         ;; ,@x -> x (directly spliced)
         (push (second elem) parts))

        (t
         ;; Regular element -> (list (quote elem)) or recurse
         (if (atom elem)
             (push (list 'list (list 'quote elem)) parts)
             (push (list 'list (expand-backquote elem)) parts)))))

    ;; Combine parts with append
    (setf parts (nreverse parts))
    (cond
      ((null parts)
       nil)
      ((null (cdr parts))
       ;; Single part - unwrap if it's (list x)
       (let ((part (car parts)))
         (if (and (consp part) (eq (car part) 'list) (= (length part) 2))
             (second part)
             part)))
      (t
       ;; Multiple parts - use append
       (cons 'append parts)))))

;;; ============================================================
;;; High-Level Reader API
;;; ============================================================

(defun clysm-read-from-string (string &key (start 0))
  "Read a single S-expression from a string.
Returns two values: the expression and the position after reading."
  (ensure-reader-state)
  (let* ((input (subseq string start))
         (lexer (make-lexer-from-string input)))
    (multiple-value-bind (expr valid)
        (parse-sexpr lexer)
      (if valid
          (values expr (+ start (lexer-position lexer)))
          (values nil nil)))))

(defun clysm-read-all-from-string (string)
  "Read all S-expressions from a string.
Returns a list of expressions."
  (ensure-reader-state)
  (let ((lexer (make-lexer-from-string string))
        (result nil))
    (loop
      (multiple-value-bind (expr valid)
          (parse-sexpr lexer)
        (unless valid
          (return (nreverse result)))
        (push expr result)))))

(defun clysm-read (&optional (stream *standard-input*))
  "Read a single S-expression from a stream."
  (let ((string (read-line stream nil nil)))
    (if string
        (clysm-read-from-string string)
        (values nil nil))))
