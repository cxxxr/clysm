;;;; string-runtime.lisp - Runtime library string functions
;;;; Feature: 001-string-runtime-migration
;;;;
;;;; Implements string operations (char, trim, capitalize, compare)
;;;; using Layer 1 primitives only (char, schar, length, make-string).
;;;; These functions are compiled to Wasm and called via runtime dispatch.
;;;;
;;;; HyperSpec references:
;;;;   [char](resources/HyperSpec/Body/f_char_.htm)
;;;;   [schar](resources/HyperSpec/Body/f_char_.htm)
;;;;   [string-trim](resources/HyperSpec/Body/f_stg_tr.htm)
;;;;   [string-left-trim](resources/HyperSpec/Body/f_stg_tr.htm)
;;;;   [string-right-trim](resources/HyperSpec/Body/f_stg_tr.htm)
;;;;   [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm)
;;;;   [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm)
;;;;   [string-equal](resources/HyperSpec/Body/f_stgeq_.htm)
;;;;   [string-not-equal](resources/HyperSpec/Body/f_stgeq_.htm)
;;;;   [string-lessp](resources/HyperSpec/Body/f_stgeq_.htm)
;;;;   [string-greaterp](resources/HyperSpec/Body/f_stgeq_.htm)
;;;;   [string-not-lessp](resources/HyperSpec/Body/f_stgeq_.htm)
;;;;   [string-not-greaterp](resources/HyperSpec/Body/f_stgeq_.htm)

(in-package #:clysm)

;;; ============================================================
;;; Helper Functions (Layer 1 primitives only)
;;; ============================================================

(defun utf8-continuation-byte-p (byte)
  "Return T if BYTE is a UTF-8 continuation byte (10xxxxxx pattern).
   Continuation bytes have their top 2 bits as 10, range 0x80-0xBF.
   Uses only Layer 1 primitives: logand, =."
  (= (logand byte #xC0) #x80))

(defun decode-utf8-char (string byte-index)
  "Decode UTF-8 character at BYTE-INDEX in STRING.
   Returns two values: the decoded character and bytes consumed.
   Uses only Layer 1 primitives: char, char-code, code-char, logand, ash.

   UTF-8 encoding:
   - 1 byte:  0xxxxxxx (ASCII, 0x00-0x7F)
   - 2 bytes: 110xxxxx 10xxxxxx (0xC0-0xDF start)
   - 3 bytes: 1110xxxx 10xxxxxx 10xxxxxx (0xE0-0xEF start)
   - 4 bytes: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx (0xF0-0xF7 start)"
  (let* ((first-char (char string byte-index))
         (first-byte (char-code first-char)))
    (cond
      ;; ASCII (0x00-0x7F): 1 byte
      ((< first-byte #x80)
       (values first-char 1))

      ;; 2-byte sequence (0xC0-0xDF)
      ((< first-byte #xE0)
       (let* ((b1 first-byte)
              (b2 (char-code (char string (1+ byte-index))))
              (codepoint (logior (ash (logand b1 #x1F) 6)
                                 (logand b2 #x3F))))
         (values (code-char codepoint) 2)))

      ;; 3-byte sequence (0xE0-0xEF)
      ((< first-byte #xF0)
       (let* ((b1 first-byte)
              (b2 (char-code (char string (+ byte-index 1))))
              (b3 (char-code (char string (+ byte-index 2))))
              (codepoint (logior (ash (logand b1 #x0F) 12)
                                 (ash (logand b2 #x3F) 6)
                                 (logand b3 #x3F))))
         (values (code-char codepoint) 3)))

      ;; 4-byte sequence (0xF0-0xF7)
      (t
       (let* ((b1 first-byte)
              (b2 (char-code (char string (+ byte-index 1))))
              (b3 (char-code (char string (+ byte-index 2))))
              (b4 (char-code (char string (+ byte-index 3))))
              (codepoint (logior (ash (logand b1 #x07) 18)
                                 (ash (logand b2 #x3F) 12)
                                 (ash (logand b3 #x3F) 6)
                                 (logand b4 #x3F))))
         (values (code-char codepoint) 4))))))

(defun char-in-bag-p (char character-bag)
  "Return T if CHAR is in CHARACTER-BAG string.
   Uses only Layer 1 primitives: char, length, char=.
   Linear search through the character bag."
  (let ((len (length character-bag)))
    (dotimes (i len nil)
      (when (char= char (char character-bag i))
        (return t)))))

(defun alpha-char-p-ascii (char)
  "Return T if CHAR is ASCII alphabetic (A-Z or a-z).
   Uses only Layer 1 primitives: char-code, and, or, <=.
   Avoids using the standard ALPHA-CHAR-P for self-hosting."
  (let ((code (char-code char)))
    (or (and (<= 65 code) (<= code 90))   ; A-Z
        (and (<= 97 code) (<= code 122))))) ; a-z

;;; ============================================================
;;; String Character Access ([char](resources/HyperSpec/Body/f_char_.htm))
;;;
;;; Returns the character at a given position in a string.
;;; ============================================================

(defun string-char-rt (string index)
  "Return the character at INDEX in STRING.
   See [char](resources/HyperSpec/Body/f_char_.htm).
   Uses only Layer 1 primitives: char, length, error."
  (let ((len (length string)))
    (when (or (< index 0) (>= index len))
      (error "Index ~A out of bounds for string of length ~A" index len))
    (char string index)))

;;; ============================================================
;;; String Trim Family ([string-trim](resources/HyperSpec/Body/f_stg_tr.htm))
;;;
;;; Returns copies of strings with specified characters removed.
;;; Non-destructive operations.
;;; ============================================================

(defun string-left-trim-rt (character-bag string start end)
  "Return STRING with leading characters in CHARACTER-BAG removed.
   See [string-left-trim](resources/HyperSpec/Body/f_stg_tr.htm).
   Uses only Layer 1 primitives: char, length, char-in-bag-p, subseq."
  (let* ((len (length string))
         (actual-start (or start 0))
         (actual-end (or end len))
         (trim-start actual-start))
    ;; Validate bounds
    (when (> actual-start actual-end)
      (error "Start ~A is greater than end ~A" actual-start actual-end))
    (when (> actual-end len)
      (error "End ~A is greater than string length ~A" actual-end len))
    ;; Find first non-bag character
    (loop while (and (< trim-start actual-end)
                     (char-in-bag-p (char string trim-start) character-bag))
          do (incf trim-start))
    ;; Return substring from trim-start to end
    (subseq string trim-start actual-end)))

(defun string-right-trim-rt (character-bag string start end)
  "Return STRING with trailing characters in CHARACTER-BAG removed.
   See [string-right-trim](resources/HyperSpec/Body/f_stg_tr.htm).
   Uses only Layer 1 primitives: char, length, char-in-bag-p, subseq."
  (let* ((len (length string))
         (actual-start (or start 0))
         (actual-end (or end len))
         (trim-end actual-end))
    ;; Validate bounds
    (when (> actual-start actual-end)
      (error "Start ~A is greater than end ~A" actual-start actual-end))
    (when (> actual-end len)
      (error "End ~A is greater than string length ~A" actual-end len))
    ;; Find last non-bag character (work backwards)
    (loop while (and (> trim-end actual-start)
                     (char-in-bag-p (char string (1- trim-end)) character-bag))
          do (decf trim-end))
    ;; Return substring from start to trim-end
    (subseq string actual-start trim-end)))

(defun string-trim-rt (character-bag string start end)
  "Return STRING with leading and trailing characters in CHARACTER-BAG removed.
   See [string-trim](resources/HyperSpec/Body/f_stg_tr.htm).
   Uses string-left-trim-rt and string-right-trim-rt."
  (let* ((len (length string))
         (actual-start (or start 0))
         (actual-end (or end len)))
    ;; Validate bounds
    (when (> actual-start actual-end)
      (error "Start ~A is greater than end ~A" actual-start actual-end))
    (when (> actual-end len)
      (error "End ~A is greater than string length ~A" actual-end len))
    ;; Trim left then right (or we could just inline the logic)
    (let* ((left-trimmed (string-left-trim-rt character-bag string actual-start actual-end))
           (result (string-right-trim-rt character-bag left-trimmed nil nil)))
      result)))

;;; ============================================================
;;; String Capitalize Family ([string-capitalize](resources/HyperSpec/Body/f_stg_up.htm))
;;;
;;; Returns/modifies strings with word capitalization.
;;; ============================================================

(defun string-capitalize-rt (string start end)
  "Return STRING with first character of each word uppercase, rest lowercase.
   See [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm).
   Word boundaries are non-alphabetic characters.
   Uses only Layer 1 primitives: char, length, make-string, char-upcase, char-downcase."
  (let* ((len (length string))
         (actual-start (or start 0))
         (actual-end (or end len))
         (result (make-string len)))
    ;; Validate bounds
    (when (> actual-start len)
      (error "Start ~A is greater than string length ~A" actual-start len))
    (when (> actual-end len)
      (error "End ~A is greater than string length ~A" actual-end len))
    ;; Copy string first
    (dotimes (i len)
      (setf (char result i) (char string i)))
    ;; Capitalize within range
    (let ((in-word nil))
      (loop for i from actual-start below actual-end
            for c = (char result i)
            do (if (alpha-char-p-ascii c)
                   (if in-word
                       ;; Not first char of word: lowercase
                       (setf (char result i) (char-downcase c))
                       ;; First char of word: uppercase
                       (progn
                         (setf (char result i) (char-upcase c))
                         (setf in-word t)))
                   ;; Non-alphabetic: word boundary
                   (setf in-word nil))))
    result))

(defun nstring-capitalize-rt (string start end)
  "Destructively capitalize STRING (first char of word up, rest down).
   See [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm).
   Modifies STRING in place and returns it."
  (let* ((len (length string))
         (actual-start (or start 0))
         (actual-end (or end len)))
    ;; Validate bounds
    (when (> actual-start len)
      (error "Start ~A is greater than string length ~A" actual-start len))
    (when (> actual-end len)
      (error "End ~A is greater than string length ~A" actual-end len))
    ;; Capitalize in place within range
    (let ((in-word nil))
      (loop for i from actual-start below actual-end
            for c = (char string i)
            do (if (alpha-char-p-ascii c)
                   (if in-word
                       ;; Not first char of word: lowercase
                       (setf (char string i) (char-downcase c))
                       ;; First char of word: uppercase
                       (progn
                         (setf (char string i) (char-upcase c))
                         (setf in-word t)))
                   ;; Non-alphabetic: word boundary
                   (setf in-word nil))))
    string))

;;; ============================================================
;;; String Comparison (Case-Insensitive)
;;; ([string-equal](resources/HyperSpec/Body/f_stgeq_.htm))
;;;
;;; Case-insensitive string comparison functions.
;;; ============================================================

(defun char-equal-ci (c1 c2)
  "Case-insensitive character comparison.
   Uses char-upcase for comparison."
  (char= (char-upcase c1) (char-upcase c2)))

(defun string-compare-ci-rt (string1 string2 start1 end1 start2 end2 comparison)
  "Case-insensitive string comparison.
   COMPARISON is one of :equal, :not-equal, :lt, :gt, :le, :ge.
   See [string-equal](resources/HyperSpec/Body/f_stgeq_.htm).
   Returns T/NIL for :equal, mismatch index or NIL for others."
  (let* ((len1 (length string1))
         (len2 (length string2))
         (s1 (or start1 0))
         (e1 (or end1 len1))
         (s2 (or start2 0))
         (e2 (or end2 len2))
         (span1 (- e1 s1))
         (span2 (- e2 s2)))
    ;; Validate bounds
    (when (> s1 e1)
      (error "Start1 ~A is greater than end1 ~A" s1 e1))
    (when (> s2 e2)
      (error "Start2 ~A is greater than end2 ~A" s2 e2))
    (case comparison
      (:equal
       ;; Must have same length and all chars equal
       (and (= span1 span2)
            (loop for i from 0 below span1
                  always (char-equal-ci (char string1 (+ s1 i))
                                        (char string2 (+ s2 i))))))
      (:not-equal
       ;; Return mismatch index if different, NIL if equal
       (if (not (= span1 span2))
           ;; Different lengths means not equal at min length
           (min span1 span2)
           (loop for i from 0 below span1
                 when (not (char-equal-ci (char string1 (+ s1 i))
                                          (char string2 (+ s2 i))))
                   return i
                 finally (return nil))))
      (:lt
       ;; string1 < string2 case-insensitively
       (let ((min-len (min span1 span2)))
         (loop for i from 0 below min-len
               for c1 = (char-upcase (char string1 (+ s1 i)))
               for c2 = (char-upcase (char string2 (+ s2 i)))
               when (char< c1 c2) return i
               when (char> c1 c2) return nil
               finally (return (if (< span1 span2) min-len nil)))))
      (:gt
       ;; string1 > string2 case-insensitively
       (let ((min-len (min span1 span2)))
         (loop for i from 0 below min-len
               for c1 = (char-upcase (char string1 (+ s1 i)))
               for c2 = (char-upcase (char string2 (+ s2 i)))
               when (char> c1 c2) return i
               when (char< c1 c2) return nil
               finally (return (if (> span1 span2) min-len nil)))))
      (:le
       ;; string1 <= string2 (not greater)
       (let ((min-len (min span1 span2)))
         (loop for i from 0 below min-len
               for c1 = (char-upcase (char string1 (+ s1 i)))
               for c2 = (char-upcase (char string2 (+ s2 i)))
               when (char< c1 c2) return i
               when (char> c1 c2) return nil
               finally (return (if (<= span1 span2) min-len nil)))))
      (:ge
       ;; string1 >= string2 (not less)
       (let ((min-len (min span1 span2)))
         (loop for i from 0 below min-len
               for c1 = (char-upcase (char string1 (+ s1 i)))
               for c2 = (char-upcase (char string2 (+ s2 i)))
               when (char> c1 c2) return i
               when (char< c1 c2) return nil
               finally (return (if (>= span1 span2) min-len nil)))))
      (otherwise
       (error "Invalid comparison type: ~A" comparison)))))

;;; Wrapper functions for individual comparison operations

(defun string-equal-rt (string1 string2 start1 end1 start2 end2)
  "Case-insensitive string equality test.
   See [string-equal](resources/HyperSpec/Body/f_stgeq_.htm)."
  (string-compare-ci-rt string1 string2 start1 end1 start2 end2 :equal))

(defun string-not-equal-rt (string1 string2 start1 end1 start2 end2)
  "Case-insensitive string inequality test.
   See [string-not-equal](resources/HyperSpec/Body/f_stgeq_.htm)."
  (string-compare-ci-rt string1 string2 start1 end1 start2 end2 :not-equal))

(defun string-lessp-rt (string1 string2 start1 end1 start2 end2)
  "Case-insensitive string less-than test.
   See [string-lessp](resources/HyperSpec/Body/f_stgeq_.htm)."
  (string-compare-ci-rt string1 string2 start1 end1 start2 end2 :lt))

(defun string-greaterp-rt (string1 string2 start1 end1 start2 end2)
  "Case-insensitive string greater-than test.
   See [string-greaterp](resources/HyperSpec/Body/f_stgeq_.htm)."
  (string-compare-ci-rt string1 string2 start1 end1 start2 end2 :gt))

(defun string-not-lessp-rt (string1 string2 start1 end1 start2 end2)
  "Case-insensitive string not-less-than test.
   See [string-not-lessp](resources/HyperSpec/Body/f_stgeq_.htm)."
  (string-compare-ci-rt string1 string2 start1 end1 start2 end2 :ge))

(defun string-not-greaterp-rt (string1 string2 start1 end1 start2 end2)
  "Case-insensitive string not-greater-than test.
   See [string-not-greaterp](resources/HyperSpec/Body/f_stgeq_.htm)."
  (string-compare-ci-rt string1 string2 start1 end1 start2 end2 :le))
