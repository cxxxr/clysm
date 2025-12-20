;;;; wasm-reader.lisp - S-expression reader that compiles to WASM
;;;;
;;;; This file contains the reader implementation that will be compiled
;;;; to WASM for self-hosting. It uses the reader primitives defined in
;;;; primitives.lisp.
;;;;
;;;; Note: Uses recursion instead of loop to avoid tagbody stack issues.

;;; Constants for special return values
;;; Since we can't easily use symbols yet, we use negative numbers as markers
;;; -1 = EOF
;;; -2 = unexpected )
;;; -3 = unexpected EOF in list
;;; -4 = expected ) after dot
;;; -5 = unknown character

;;; Skip whitespace and comments (recursive version)
(defun skip-whitespace-helper (rs)
  "Helper for skipping whitespace recursively."
  (let ((ch (reader-state-peek-char rs)))
    (cond
      ;; EOF
      ((= ch -1) nil)
      ;; Whitespace - skip and continue
      ((whitespace-char-p ch)
       (reader-state-read-char rs)
       (skip-whitespace-helper rs))
      ;; Semicolon comment - skip to end of line
      ((= ch 59)  ; ';'
       (reader-state-read-char rs)
       (skip-to-newline rs)
       (skip-whitespace-helper rs))
      ;; Hash - check for block comment #|
      ((= ch 35)  ; '#'
       (reader-state-read-char rs)
       (let ((next (reader-state-peek-char rs)))
         (if (= next 124)  ; '|'
             (progn
               (reader-state-read-char rs)
               (skip-block-comment rs 1)
               (skip-whitespace-helper rs))
             ;; Not a block comment - unread the # and stop
             (progn
               (reader-state-unread-char rs)
               nil))))
      ;; Not whitespace - done
      (t nil))))

(defun skip-to-newline (rs)
  "Skip characters until newline or EOF."
  (let ((ch (reader-state-read-char rs)))
    (cond
      ((= ch -1) nil)     ; EOF
      ((= ch 10) nil)     ; newline
      (t (skip-to-newline rs)))))

;;; Character literal #\x
(defun read-character-literal (rs)
  "Read a character literal after #\\. Returns the character code."
  (let ((ch (reader-state-read-char rs)))
    (cond
      ((= ch -1) -1)  ; EOF
      ;; Check for named characters (Space, Newline, Tab, Return)
      ((alpha-char-p ch)
       (let ((start (- (reader-state-position rs) 1)))
         ;; Peek to see if more letters follow
         (let ((next (reader-state-peek-char rs)))
           (if (alpha-char-p next)
               ;; Named character - read the rest
               (read-named-character rs ch)
               ;; Single letter character
               ch))))
      ;; Single non-alpha character
      (t ch))))

(defun read-named-character (rs first-char)
  "Read a named character like Space, Newline, Tab."
  ;; Read remaining characters
  (let ((name (read-character-name-rest rs (list first-char))))
    (let ((upcased (upcase-char-list name)))
      (cond
        ((char-list-equal upcased (list 83 80 65 67 69))  ; "SPACE"
         32)
        ((char-list-equal upcased (list 78 69 87 76 73 78 69))  ; "NEWLINE"
         10)
        ((char-list-equal upcased (list 84 65 66))  ; "TAB"
         9)
        ((char-list-equal upcased (list 82 69 84 85 82 78))  ; "RETURN"
         13)
        ((char-list-equal upcased (list 76 73 78 69 70 69 69 68))  ; "LINEFEED"
         10)
        ((char-list-equal upcased (list 80 65 71 69))  ; "PAGE"
         12)
        ((char-list-equal upcased (list 82 85 66 79 85 84))  ; "RUBOUT"
         127)
        ((char-list-equal upcased (list 66 65 67 75 83 80 65 67 69))  ; "BACKSPACE"
         8)
        ((char-list-equal upcased (list 78 85 76))  ; "NUL"
         0)
        ;; Unknown named character - return first char
        (t first-char)))))

(defun read-character-name-rest (rs acc)
  "Read remaining characters of a named character."
  (let ((ch (reader-state-peek-char rs)))
    (if (alpha-char-p ch)
        (progn
          (reader-state-read-char rs)
          (read-character-name-rest rs (cons ch acc)))
        (reverse-list acc))))

(defun upcase-char-list (chars)
  "Convert a list of character codes to uppercase."
  (if (null chars)
      nil
      (cons (upcase-char (car chars))
            (upcase-char-list (cdr chars)))))

(defun upcase-char (ch)
  "Convert a single character code to uppercase."
  (if (and (>= ch 97) (<= ch 122))  ; a-z
      (- ch 32)
      ch))

(defun char-list-equal (a b)
  "Compare two lists of character codes for equality."
  (cond
    ((and (null a) (null b)) t)
    ((or (null a) (null b)) nil)
    ((= (car a) (car b))
     (char-list-equal (cdr a) (cdr b)))
    (t nil)))

(defun reverse-list (lst)
  "Reverse a list."
  (reverse-list-helper lst nil))

(defun reverse-list-helper (lst acc)
  (if (null lst)
      acc
      (reverse-list-helper (cdr lst) (cons (car lst) acc))))

;;; Block comment #|...|# (supports nesting)
(defun skip-block-comment (rs depth)
  "Skip block comment. DEPTH tracks nesting level."
  (let ((ch (reader-state-read-char rs)))
    (cond
      ;; EOF - error but just return
      ((= ch -1) nil)
      ;; Potential end: |#
      ((= ch 124)  ; '|'
       (let ((next (reader-state-peek-char rs)))
         (if (= next 35)  ; '#'
             (progn
               (reader-state-read-char rs)
               (if (= depth 1)
                   nil  ; End of outermost comment
                   (skip-block-comment rs (- depth 1))))
             (skip-block-comment rs depth))))
      ;; Potential nested start: #|
      ((= ch 35)  ; '#'
       (let ((next (reader-state-peek-char rs)))
         (if (= next 124)  ; '|'
             (progn
               (reader-state-read-char rs)
               (skip-block-comment rs (+ depth 1)))
             (skip-block-comment rs depth))))
      ;; Any other character
      (t (skip-block-comment rs depth)))))

(defun skip-whitespace (rs)
  "Skip whitespace characters in reader state RS."
  (skip-whitespace-helper rs))

;;; Read an integer (recursive version)
(defun read-integer-helper (rs result)
  "Helper for reading integer digits recursively."
  (let ((d (digit-char-p (reader-state-peek-char rs))))
    (if d
        (progn
          (reader-state-read-char rs)
          (read-integer-helper rs (+ (* result 10) (- d 1))))
        result)))

(defun read-integer (rs)
  "Read an integer from reader state RS. Assumes positioned at digit or sign."
  (let ((ch (reader-state-peek-char rs)))
    (cond
      ;; Negative number
      ((= ch 45)  ; '-'
       (reader-state-read-char rs)
       (- 0 (read-integer-helper rs 0)))
      ;; Positive with explicit sign
      ((= ch 43)  ; '+'
       (reader-state-read-char rs)
       (read-integer-helper rs 0))
      ;; Just digits
      (t (read-integer-helper rs 0)))))

;;; Read a symbol as a string
(defun skip-symbol-chars (rs)
  "Skip over symbol constituent characters, return count."
  (let ((ch (reader-state-peek-char rs)))
    (if (and (/= ch -1) (symbol-constituent-p ch))
        (progn
          (reader-state-read-char rs)
          (+ 1 (skip-symbol-chars rs)))
        0)))

(defun read-symbol-as-string (rs)
  "Read a symbol and return it as an uppercase string."
  (let ((start (reader-state-position rs)))
    (skip-symbol-chars rs)
    (let ((end (reader-state-position rs)))
      (reader-state-substring rs start end))))

;;; Read a string literal
(defun read-string-char (rs)
  "Read a single character from a string literal, handling escapes."
  (let ((ch (reader-state-read-char rs)))
    (if (= ch 92)  ; backslash
        (let ((escaped (reader-state-read-char rs)))
          (cond
            ((= escaped 110) 10)   ; \n -> newline
            ((= escaped 116) 9)    ; \t -> tab
            ((= escaped 114) 13)   ; \r -> carriage return
            ((= escaped 34) 34)    ; \" -> "
            ((= escaped 92) 92)    ; \\ -> \
            (t escaped)))          ; unknown escape, return as-is
        ch)))

(defun count-string-chars (rs start-pos)
  "Count characters in a string literal (for allocation)."
  ;; Reset to start
  (reader-state-set-position rs start-pos)
  (count-string-chars-helper rs 0))

(defun count-string-chars-helper (rs count)
  "Helper to count string characters."
  (let ((ch (reader-state-read-char rs)))
    (cond
      ((= ch -1) count)  ; EOF (error, but return count)
      ((= ch 34) count)  ; closing quote
      ((= ch 92)         ; backslash - skip next char
       (reader-state-read-char rs)
       (count-string-chars-helper rs (+ count 1)))
      (t (count-string-chars-helper rs (+ count 1))))))

(defun read-string-literal (rs)
  "Read a string literal from RS. Assumes opening quote was consumed."
  ;; For simplicity, we'll use a fixed approach:
  ;; Record start position, count chars, allocate, then copy
  (let ((start-pos (reader-state-position rs)))
    ;; Count characters first
    (let ((len (count-string-chars rs start-pos)))
      ;; Reset to start
      (reader-state-set-position rs start-pos)
      ;; Allocate string
      (let ((str (make-string-buffer len)))
        ;; Fill string
        (fill-string-buffer rs str 0 len)
        str))))

(defun make-string-buffer (len)
  "Allocate a string buffer of given length. Returns string pointer."
  ;; We need to allocate [length:i32][bytes...]
  ;; For now, use a simple approach with heap allocation
  ;; This will be done inline since we can't easily call alloc
  nil)  ; Placeholder - will use inline allocation in read-form

;;; Read a list (recursive)
(defun read-list-elements (rs)
  "Read list elements until closing paren."
  (skip-whitespace rs)
  (let ((ch (reader-state-peek-char rs)))
    (cond
      ;; End of list
      ((= ch 41)  ; ')'
       (reader-state-read-char rs)
       nil)
      ;; EOF - error
      ((= ch -1)
       -3)  ; Error code for unexpected EOF
      ;; Dot notation
      ((= ch 46)  ; '.'
       (reader-state-read-char rs)
       (skip-whitespace rs)
       (let ((cdr-val (read-form rs)))
         (skip-whitespace rs)
         (if (= (reader-state-peek-char rs) 41)  ; ')'
             (progn
               (reader-state-read-char rs)
               cdr-val)
             -4)))  ; Error: expected ) after dot
      ;; Read element and continue
      (t
       (let ((element (read-form rs)))
         (cons element (read-list-elements rs)))))))

;;; Main read function
(defun read-form (rs)
  "Read a single Lisp form from reader state RS."
  (skip-whitespace rs)
  (let ((ch (reader-state-peek-char rs)))
    (cond
      ;; EOF
      ((= ch -1) -1)
      ;; Left paren - start list
      ((= ch 40)  ; '('
       (reader-state-read-char rs)
       (read-list-elements rs))
      ;; Quote
      ((= ch 39)  ; '\''
       (reader-state-read-char rs)
       (let ((quoted (read-form rs)))
         ;; Return (quote <form>) as a list
         ;; We use the string "QUOTE" as the symbol for now
         (cons (read-symbol-from-string "QUOTE") (cons quoted nil))))
      ;; Backquote
      ((= ch 96)  ; '`'
       (reader-state-read-char rs)
       (let ((form (read-form rs)))
         (cons (read-symbol-from-string "BACKQUOTE") (cons form nil))))
      ;; Comma (unquote)
      ((= ch 44)  ; ','
       (reader-state-read-char rs)
       (let ((next (reader-state-peek-char rs)))
         (if (= next 64)  ; '@'
             (progn
               (reader-state-read-char rs)
               (let ((form (read-form rs)))
                 (cons (read-symbol-from-string "SPLICE-UNQUOTE") (cons form nil))))
             (let ((form (read-form rs)))
               (cons (read-symbol-from-string "UNQUOTE") (cons form nil))))))
      ;; Hash dispatch (#' #\ #|)
      ((= ch 35)  ; '#'
       (reader-state-read-char rs)
       (let ((dispatch (reader-state-peek-char rs)))
         (cond
           ;; #' - function quote
           ((= dispatch 39)  ; '\''
            (reader-state-read-char rs)
            (let ((func-form (read-form rs)))
              (cons (read-symbol-from-string "FUNCTION") (cons func-form nil))))
           ;; #\ - character literal
           ((= dispatch 92)  ; '\\'
            (reader-state-read-char rs)
            (read-character-literal rs))
           ;; #| - block comment (already consumed in skip-whitespace, but handle here too)
           ((= dispatch 124)  ; '|'
            (reader-state-read-char rs)
            (skip-block-comment rs 1)
            (read-form rs))  ; Continue reading after comment
           ;; Unknown dispatch - error
           (t -5))))
      ;; String literal
      ((= ch 34)  ; '"'
       (reader-state-read-char rs)
       (read-simple-string rs))
      ;; Number starting with digit
      ((digit-char-p ch)
       (read-integer rs))
      ;; Number or symbol starting with sign
      ((or (= ch 45) (= ch 43))  ; '-' or '+'
       (reader-state-read-char rs)
       (let ((next (reader-state-peek-char rs)))
         (reader-state-unread-char rs)
         (if (digit-char-p next)
             (read-integer rs)
             (read-symbol-as-string rs))))
      ;; Right paren - error
      ((= ch 41)  ; ')'
       -2)  ; Error: unexpected )
      ;; Symbol
      ((symbol-constituent-p ch)
       (read-symbol-as-string rs))
      ;; Unknown
      (t -5))))  ; Error: unknown character

;;; Helper to create symbol from known string
(defun read-symbol-from-string (name)
  "Create a symbol string. For now just returns the string itself."
  name)  ; In a full implementation, this would intern the symbol

;;; Simple string reader (builds string in-place)
(defun read-simple-string (rs)
  "Read a simple string literal. Assumes opening quote was consumed."
  ;; First, find the end to calculate length
  (let ((start (reader-state-position rs)))
    ;; Scan to find length (without processing escapes for now)
    (let ((len (scan-string-length rs)))
      ;; Reset position
      (reader-state-set-position rs start)
      ;; Now create and fill the string using reader-state-substring
      ;; But we need to handle the closing quote...
      ;; For simplicity, read character by character
      (skip-to-string-end rs)
      ;; Create substring from start to current-1 (without uppercase conversion)
      (reader-state-substring-raw rs start (- (reader-state-position rs) 1)))))

(defun scan-string-length (rs)
  "Scan forward to find string length (bytes until closing quote)."
  (scan-string-length-helper rs 0))

(defun scan-string-length-helper (rs count)
  (let ((ch (reader-state-read-char rs)))
    (cond
      ((= ch -1) count)     ; EOF
      ((= ch 34) count)     ; closing quote
      ((= ch 92)            ; backslash - skip next
       (reader-state-read-char rs)
       (scan-string-length-helper rs (+ count 1)))
      (t (scan-string-length-helper rs (+ count 1))))))

(defun skip-to-string-end (rs)
  "Skip to the end of a string literal (after closing quote)."
  (let ((ch (reader-state-read-char rs)))
    (cond
      ((= ch -1) nil)       ; EOF
      ((= ch 34) nil)       ; closing quote - done
      ((= ch 92)            ; backslash - skip next
       (reader-state-read-char rs)
       (skip-to-string-end rs))
      (t (skip-to-string-end rs)))))

;;; Entry point
(defun read-from-string-impl (str)
  "Read a single form from string STR."
  (let ((rs (make-reader-state str)))
    (read-form rs)))
