;;;; reader.lisp - S-expression reader for Clysm

(in-package #:clysm/reader)

(defun read-from-string* (string)
  "Read an S-expression from a string."
  (let ((tokens (clysm/reader/tokenizer:tokenize string)))
    (clysm/reader/parser:parse tokens)))

(defun read* (&optional (stream *standard-input*))
  "Read an S-expression from a stream."
  ;; Read all available input
  (let ((buffer (make-array 0 :element-type 'character
                              :fill-pointer 0
                              :adjustable t)))
    ;; For interactive streams, read until complete expression
    ;; For now, just read a line
    (loop for char = (read-char stream nil nil)
          while (and char (not (char= char #\Newline)))
          do (vector-push-extend char buffer))
    (when (> (length buffer) 0)
      (read-from-string* buffer))))

(defun read-all* (string)
  "Read all S-expressions from a string."
  (let ((tokens (clysm/reader/tokenizer:tokenize string)))
    (clysm/reader/parser:parse-all tokens)))
