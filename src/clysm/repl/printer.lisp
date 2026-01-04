;;;; repl/printer.lisp - Lisp Object Printer
;;;;
;;;; Prints Lisp objects in a human-readable format.
;;;; Handles both compile-time values and runtime results.

(in-package #:clysm)

;;; ============================================================
;;; Print Settings
;;; ============================================================

(defvar *clysm-print-readably* nil
  "If true, print objects in a way that can be read back.")

(defvar *clysm-print-circle* nil
  "If true, detect and print circular structures.")

(defvar *clysm-print-length* nil
  "Maximum number of elements to print in a list (nil = unlimited).")

(defvar *clysm-print-level* nil
  "Maximum depth to print nested structures (nil = unlimited).")

;;; ============================================================
;;; Object Printer
;;; ============================================================

(defun clysm-prin1 (object &optional (stream *standard-output*))
  "Print OBJECT to STREAM in a readable format.
Returns OBJECT."
  (clysm-write object stream)
  object)

(defun clysm-princ (object &optional (stream *standard-output*))
  "Print OBJECT to STREAM without escape characters.
Returns OBJECT."
  (let ((*clysm-print-readably* nil))
    (clysm-write object stream))
  object)

(defun clysm-print (object &optional (stream *standard-output*))
  "Print a newline, then OBJECT, then a space.
Returns OBJECT."
  (terpri stream)
  (clysm-prin1 object stream)
  (write-char #\Space stream)
  object)

(defun clysm-write (object stream &key (level 0))
  "Write OBJECT to STREAM with proper formatting."
  (when (and *clysm-print-level* (>= level *clysm-print-level*))
    (write-char #\# stream)
    (return-from clysm-write))

  (typecase object
    (null
     (write-string "NIL" stream))

    ((eql t)
     (write-string "T" stream))

    (integer
     (format stream "~D" object))

    (float
     (format stream "~F" object))

    (character
     (format stream "#\\~A"
             (case object
               (#\Space "Space")
               (#\Newline "Newline")
               (#\Tab "Tab")
               (#\Return "Return")
               (t object))))

    (string
     (if *clysm-print-readably*
         (write-string-escaped object stream)
         (write-string object stream)))

    (symbol
     (write-string (symbol-name object) stream))

    (cons
     (write-list object stream level))

    (function
     (format stream "#<FUNCTION>"))

    (t
     (format stream "#<~A>" (type-of object)))))

(defun write-string-escaped (string stream)
  "Write STRING with escape characters."
  (write-char #\" stream)
  (loop for char across string do
    (case char
      (#\Newline (write-string "\\n" stream))
      (#\Tab (write-string "\\t" stream))
      (#\Return (write-string "\\r" stream))
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (t (write-char char stream))))
  (write-char #\" stream))

(defun write-list (list stream level)
  "Write a list/cons cell to STREAM."
  (write-char #\( stream)
  (let ((count 0)
        (current list))
    (loop
      (when (null current)
        (return))
      (when (and *clysm-print-length* (>= count *clysm-print-length*))
        (when (> count 0)
          (write-char #\Space stream))
        (write-string "..." stream)
        (return))
      (when (> count 0)
        (write-char #\Space stream))
      (cond
        ((consp current)
         (clysm-write (car current) stream :level (1+ level))
         (setf current (cdr current))
         (incf count))
        (t
         ;; Dotted pair
         (write-string ". " stream)
         (clysm-write current stream :level (1+ level))
         (return)))))
  (write-char #\) stream))

;;; ============================================================
;;; Object to String
;;; ============================================================

(defun clysm-prin1-to-string (object)
  "Return a string representation of OBJECT."
  (with-output-to-string (stream)
    (clysm-prin1 object stream)))

(defun clysm-princ-to-string (object)
  "Return a string representation of OBJECT without escapes."
  (with-output-to-string (stream)
    (clysm-princ object stream)))

;;; ============================================================
;;; Runtime Value Decoding
;;; ============================================================

;;; When running Wasm modules with wasmtime, results are returned as
;;; i32/i64 values. These need to be decoded based on our runtime
;;; representation:
;;;
;;; i31ref (fixnum):  tagged integer, low bit = 1
;;; structref:        heap reference, low bit = 0
;;;
;;; For now, we'll handle simple fixnum results.

(defun decode-wasm-result (value)
  "Decode a Wasm result value to a Lisp value.
VALUE is the raw i32 returned from wasmtime."
  (cond
    ;; Fixnum: i31ref is an unboxed 31-bit integer
    ;; In wasmtime output, fixnums appear directly as integers
    ((integerp value)
     value)

    ;; Boolean: wasmtime might return 0 or 1 for i32
    ((eql value 0)
     nil)
    ((eql value 1)
     t)

    ;; String result from wasmtime (parsed)
    ((stringp value)
     (parse-wasmtime-result value))

    (t
     value)))

(defun parse-wasmtime-result (output)
  "Parse wasmtime output to extract result value."
  (let ((trimmed (string-trim '(#\Space #\Newline #\Tab) output)))
    (cond
      ;; Empty output
      ((string= trimmed "")
       nil)

      ;; i32 result like "42" or "-10"
      ((every (lambda (c) (or (digit-char-p c) (char= c #\-))) trimmed)
       (parse-integer trimmed))

      ;; ref.i31 result (depends on wasmtime version)
      ;; Format might be "ref.i31 42" or similar
      ((string-prefix-p "ref.i31" trimmed)
       (let ((num-start (position-if #'digit-char-p trimmed)))
         (when num-start
           (parse-integer trimmed :start num-start :junk-allowed t))))

      ;; ref.null
      ((string-prefix-p "ref.null" trimmed)
       nil)

      ;; Unknown format - return as string
      (t
       trimmed))))
