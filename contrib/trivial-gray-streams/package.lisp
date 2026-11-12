(defpackage #:trivial-gray-streams
  (:use #:cl)
  (:import-from #:gray
   #:fundamental-stream #:fundamental-input-stream #:fundamental-output-stream
   #:fundamental-character-stream #:fundamental-binary-stream
   #:fundamental-character-input-stream #:fundamental-character-output-stream
   #:fundamental-binary-input-stream #:fundamental-binary-output-stream
   #:stream-read-char #:stream-unread-char #:stream-read-char-no-hang
   #:stream-peek-char #:stream-listen #:stream-read-line #:stream-clear-input
   #:stream-write-char #:stream-line-column #:stream-start-line-p
   #:stream-write-string #:stream-terpri #:stream-fresh-line
   #:stream-finish-output #:stream-force-output #:stream-clear-output
   #:stream-advance-to-column #:stream-read-byte #:stream-write-byte
   #:stream-read-sequence #:stream-write-sequence #:stream-file-position)
  (:export
   #:fundamental-stream #:fundamental-input-stream #:fundamental-output-stream
   #:fundamental-character-stream #:fundamental-binary-stream
   #:fundamental-character-input-stream #:fundamental-character-output-stream
   #:fundamental-binary-input-stream #:fundamental-binary-output-stream
   #:stream-read-char #:stream-unread-char #:stream-read-char-no-hang
   #:stream-peek-char #:stream-listen #:stream-read-line #:stream-clear-input
   #:stream-write-char #:stream-line-column #:stream-start-line-p
   #:stream-write-string #:stream-terpri #:stream-fresh-line
   #:stream-finish-output #:stream-force-output #:stream-clear-output
   #:stream-advance-to-column #:stream-read-byte #:stream-write-byte
   #:stream-read-sequence #:stream-write-sequence #:stream-file-position
   #:trivial-gray-stream-mixin))

(in-package #:trivial-gray-streams)

;;; deprecated, kept for compatibility
(defclass trivial-gray-stream-mixin () ())
