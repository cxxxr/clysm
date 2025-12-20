;;;; wasm-reader.lisp - S-expression reader that compiles to WASM
;;;;
;;;; This file contains the reader implementation that will be compiled
;;;; to WASM for self-hosting. It uses the reader primitives defined in
;;;; primitives.lisp.
;;;;
;;;; Note: Uses recursion instead of loop to avoid tagbody stack issues.

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
      ;; Not whitespace - done
      (t nil))))

(defun skip-to-newline (rs)
  "Skip characters until newline or EOF."
  (let ((ch (reader-state-read-char rs)))
    (cond
      ((= ch -1) nil)     ; EOF
      ((= ch 10) nil)     ; newline
      (t (skip-to-newline rs)))))

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

;;; Read a symbol name as a hash (for now)
(defun read-symbol-helper (rs hash)
  "Helper for reading symbol characters recursively."
  (let ((ch (reader-state-peek-char rs)))
    (if (and (/= ch -1) (symbol-constituent-p ch))
        (progn
          (reader-state-read-char rs)
          ;; Simple hash: hash * 31 + uppercase(ch)
          (read-symbol-helper rs (+ (* hash 31) (char-upcase ch))))
        hash)))

(defun read-symbol-name (rs)
  "Read a symbol name from RS and return hash code as identifier."
  (read-symbol-helper rs 0))

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
       (cons 12345 (cons (read-form rs) nil)))  ; 12345 = placeholder for QUOTE symbol
      ;; Number starting with digit
      ((digit-char-p ch)
       (read-integer rs))
      ;; Number starting with sign
      ((or (= ch 45) (= ch 43))  ; '-' or '+'
       (reader-state-read-char rs)
       (let ((next (reader-state-peek-char rs)))
         (reader-state-unread-char rs)
         (if (digit-char-p next)
             (read-integer rs)
             (read-symbol-name rs))))
      ;; Right paren - error
      ((= ch 41)  ; ')'
       -2)  ; Error: unexpected )
      ;; Symbol
      ((symbol-constituent-p ch)
       (read-symbol-name rs))
      ;; Unknown
      (t -5))))  ; Error: unknown character

;;; Entry point
(defun read-from-string-impl (str)
  "Read a single form from string STR."
  (let ((rs (make-reader-state str)))
    (read-form rs)))
