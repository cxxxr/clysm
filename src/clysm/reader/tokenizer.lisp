;;;; tokenizer.lisp - Lexical analysis

(in-package #:clysm/reader/tokenizer)

(defstruct tokenizer
  "Tokenizer state."
  (input "" :type string)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 0 :type fixnum))

(defun make-tokenizer-from-string (string)
  "Create a tokenizer from a string."
  (make-tokenizer :input string))

(defun next-token (tokenizer)
  "Get the next token from the tokenizer."
  ;; TODO: Implement
  (declare (ignore tokenizer))
  nil)

(defun tokenize (string)
  "Tokenize a string into a list of tokens."
  ;; TODO: Implement
  (declare (ignore string))
  nil)
