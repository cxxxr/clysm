;;;; io-runtime.lisp - Runtime library I/O functions
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;;
;;;; Implements I/O functions (princ, prin1, print, write, format, terpri)
;;;; using FFI primitives %host-write-char and %host-write-string.
;;;; These functions are compiled to Wasm and called via runtime dispatch.
;;;;
;;;; HyperSpec references:
;;;;   [princ](resources/HyperSpec/Body/f_wr_pr.htm)
;;;;   [prin1](resources/HyperSpec/Body/f_wr_pr.htm)
;;;;   [print](resources/HyperSpec/Body/f_wr_pr.htm)
;;;;   [write](resources/HyperSpec/Body/f_wr_pr.htm)
;;;;   [format](resources/HyperSpec/Body/f_format.htm)
;;;;   [terpri](resources/HyperSpec/Body/f_terpri.htm)

(in-package #:clysm)

;;; ============================================================
;;; FFI Function Aliases
;;; ============================================================
;;; The FFI primitives are defined in clysm/streams package.
;;; We create local aliases for convenience.

(defmacro write-char-ffi (fd char-code)
  "Write character with given code to file descriptor.
   Wrapper for clysm/streams::%host-write-char."
  `(clysm/streams::%host-write-char ,fd ,char-code))

(defmacro write-string-ffi (fd string)
  "Write string to file descriptor.
   Wrapper for clysm/streams::%host-write-string."
  `(clysm/streams::%host-write-string ,fd ,string))

;;; ============================================================
;;; Constants
;;; ============================================================

(defconstant +stdout-fd+ 1
  "File descriptor for standard output.")

(defconstant +newline-char-code+ 10
  "Character code for newline.")

(defconstant +space-char-code+ 32
  "Character code for space.")

(defconstant +double-quote-char-code+ 34
  "Character code for double quote.")

;;; ============================================================
;;; T018: princ-rt - Output without escape characters (FR-001)
;;; ============================================================

(defun princ-rt (object)
  "Output OBJECT without escape characters.
   Uses %host-write-string for strings, converts other types to string first.
   Returns OBJECT."
  (cond
    ((stringp object)
     ;; Direct string output
     (write-string-ffi +stdout-fd+ object))
    ((characterp object)
     ;; Single character output
     (write-char-ffi +stdout-fd+ (char-code object)))
    ((integerp object)
     ;; Convert integer to string and output
     (let ((str (integer-to-string object)))
       (write-string-ffi +stdout-fd+ str)))
    ((symbolp object)
     ;; Output symbol name
     (write-string-ffi +stdout-fd+ (symbol-name object)))
    ((null object)
     ;; Output NIL
     (write-string-ffi +stdout-fd+ "NIL"))
    ((consp object)
     ;; Output list representation
     (princ-list-rt object))
    (t
     ;; Default: output type name
     (write-string-ffi +stdout-fd+ "#<OBJECT>")))
  object)

(defun princ-list-rt (list)
  "Helper to output a list without escape."
  (write-char-ffi +stdout-fd+ 40)  ; (
  (loop for rest on list
        for first = t then nil
        do (unless first
             (write-char-ffi +stdout-fd+ +space-char-code+))
           (princ-rt (car rest))
        finally (when (cdr rest)
                  ;; Dotted list
                  (write-string-ffi +stdout-fd+ " . ")
                  (princ-rt (cdr rest))))
  (write-char-ffi +stdout-fd+ 41)) ; )

;;; ============================================================
;;; T019: prin1-rt - Output with escape characters
;;; ============================================================

(defun prin1-rt (object)
  "Output OBJECT with escape characters (readable form).
   Strings are surrounded by double quotes.
   Returns OBJECT."
  (cond
    ((stringp object)
     ;; Output with surrounding quotes
     (write-char-ffi +stdout-fd+ +double-quote-char-code+)
     (write-string-ffi +stdout-fd+ object)
     (write-char-ffi +stdout-fd+ +double-quote-char-code+))
    ((characterp object)
     ;; Output character syntax #\x
     (write-string-ffi +stdout-fd+ "#\\")
     (write-char-ffi +stdout-fd+ (char-code object)))
    ((integerp object)
     ;; Integers same as princ
     (let ((str (integer-to-string object)))
       (write-string-ffi +stdout-fd+ str)))
    ((symbolp object)
     ;; Output symbol name (no package prefix for now)
     (write-string-ffi +stdout-fd+ (symbol-name object)))
    ((null object)
     (write-string-ffi +stdout-fd+ "NIL"))
    ((consp object)
     (prin1-list-rt object))
    (t
     (write-string-ffi +stdout-fd+ "#<OBJECT>")))
  object)

(defun prin1-list-rt (list)
  "Helper to output a list with escape."
  (write-char-ffi +stdout-fd+ 40)  ; (
  (loop for rest on list
        for first = t then nil
        do (unless first
             (write-char-ffi +stdout-fd+ +space-char-code+))
           (prin1-rt (car rest))
        finally (when (cdr rest)
                  (write-string-ffi +stdout-fd+ " . ")
                  (prin1-rt (cdr rest))))
  (write-char-ffi +stdout-fd+ 41)) ; )

;;; ============================================================
;;; T020: print-rt - Newline, object, space (FR-002)
;;; ============================================================

(defun print-rt (object)
  "Output newline, then OBJECT in readable form, then space.
   Returns OBJECT."
  (write-char-ffi +stdout-fd+ +newline-char-code+)
  (prin1-rt object)
  (write-char-ffi +stdout-fd+ +space-char-code+)
  object)

;;; ============================================================
;;; T021: write-rt - Output with keyword arguments (FR-003)
;;; ============================================================

(defun write-rt (object &key (stream t) (escape t) (radix nil)
                             (base 10) (circle nil) (pretty nil)
                             (level nil) (length nil))
  "Output OBJECT with specified options.
   Currently supports :stream and :escape.
   Returns OBJECT."
  (declare (ignore radix base circle pretty level length))
  (when (eq stream t)
    (if escape
        (prin1-rt object)
        (princ-rt object)))
  object)

;;; ============================================================
;;; T022: terpri-rt - Output newline
;;; ============================================================

(defun terpri-rt (&optional stream)
  "Output a newline to STREAM (default stdout).
   Returns NIL."
  (declare (ignore stream))
  (write-char-ffi +stdout-fd+ +newline-char-code+)
  nil)

;;; ============================================================
;;; T023: format-rt - Basic format directives (FR-004)
;;; ============================================================

(defun format-rt (destination control-string &rest args)
  "Format output according to CONTROL-STRING with ARGS.
   Supports directives: ~A, ~S, ~D, ~%, ~&, ~~
   If DESTINATION is nil, returns string.
   If DESTINATION is t, outputs to stdout."
  (let ((arg-idx 0)
        (result-chars nil)
        (to-string (null destination)))
    ;; Parse control string
    (loop with len = (length control-string)
          for i from 0 below len
          for ch = (char control-string i)
          do (if (char= ch #\~)
                 ;; Process directive
                 (let ((directive (when (< (1+ i) len)
                                   (char control-string (1+ i)))))
                   (case directive
                     ((#\A #\a)
                      ;; Aesthetic - no escape
                      (let ((arg (nth arg-idx args)))
                        (incf arg-idx)
                        (format-output-value arg nil to-string result-chars)))
                     ((#\S #\s)
                      ;; Standard - with escape
                      (let ((arg (nth arg-idx args)))
                        (incf arg-idx)
                        (format-output-value arg t to-string result-chars)))
                     ((#\D #\d)
                      ;; Decimal integer
                      (let ((arg (nth arg-idx args)))
                        (incf arg-idx)
                        (format-output-integer arg 10 to-string result-chars)))
                     ((#\%)
                      ;; Newline
                      (format-output-char +newline-char-code+ to-string result-chars))
                     ((#\&)
                      ;; Fresh line (simplified: just newline)
                      (format-output-char +newline-char-code+ to-string result-chars))
                     ((#\~)
                      ;; Literal tilde
                      (format-output-char 126 to-string result-chars)))
                   (incf i))  ; Skip directive character
                 ;; Regular character
                 (format-output-char (char-code ch) to-string result-chars)))
    ;; Return string if destination was nil
    (when to-string
      (coerce (nreverse result-chars) 'string))))

(defun format-output-value (value escape to-string result-chars)
  "Output VALUE, optionally escaped."
  (declare (ignore result-chars))
  (unless to-string
    (if escape
        (prin1-rt value)
        (princ-rt value))))

(defun format-output-integer (value radix to-string result-chars)
  "Output integer VALUE in given RADIX."
  (declare (ignore radix result-chars))
  (unless to-string
    (let ((str (integer-to-string value)))
      (write-string-ffi +stdout-fd+ str))))

(defun format-output-char (char-code to-string result-chars)
  "Output character with given code."
  (declare (ignore result-chars))
  (unless to-string
    (write-char-ffi +stdout-fd+ char-code)))

;;; ============================================================
;;; T024-T025: Extended format directives
;;; ============================================================

;; Note: Extended directives (~B, ~O, ~X, ~R, ~C, ~F, ~E, ~G, ~$)
;; will be implemented incrementally as needed.

;;; ============================================================
;;; Helper: Integer to String Conversion
;;; ============================================================

(defun integer-to-string (n)
  "Convert integer N to decimal string representation."
  (cond
    ((zerop n) "0")
    ((minusp n)
     (concatenate 'string "-" (positive-integer-to-string (- n))))
    (t
     (positive-integer-to-string n))))

(defun positive-integer-to-string (n)
  "Convert positive integer N to decimal string."
  (let ((digits nil))
    (loop while (plusp n)
          do (push (code-char (+ 48 (mod n 10))) digits)
             (setf n (floor n 10)))
    (coerce digits 'string)))
