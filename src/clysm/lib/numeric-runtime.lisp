;;;; numeric-runtime.lisp - Runtime library numeric functions
;;;; Feature: 001-numeric-runtime-migration
;;;;
;;;; Implements numeric operations (parse-integer, write-to-string, rationalize,
;;;; signum, phase) using Layer 1 primitives only (arithmetic, type predicates).
;;;; These functions are compiled to Wasm and called via runtime dispatch.
;;;;
;;;; HyperSpec references:
;;;;   [parse-integer](resources/HyperSpec/Body/f_parse_.htm)
;;;;   [write-to-string](resources/HyperSpec/Body/f_wr_to_.htm)
;;;;   [rationalize](resources/HyperSpec/Body/f_ration.htm)
;;;;   [signum](resources/HyperSpec/Body/f_signum.htm)
;;;;   [phase](resources/HyperSpec/Body/f_phase.htm)

(in-package #:clysm)

;;; ============================================================
;;; Constants
;;; ============================================================

(defconstant +min-radix+ 2
  "Minimum allowed radix for parse-integer and write-to-string.")

(defconstant +max-radix+ 36
  "Maximum allowed radix for parse-integer and write-to-string.")

(defconstant +default-radix+ 10
  "Default radix for parse-integer.")

(defconstant +pi+ 3.141592653589793d0
  "Pi constant for phase function.")

;;; ============================================================
;;; Helper Functions (Layer 1 primitives only)
;;; ============================================================

;;; T030: digit-char-to-weight - Convert character to numeric value
(defun digit-char-to-weight (char radix)
  "Convert CHAR to its numeric weight for RADIX.
   Returns nil if CHAR is not valid for RADIX.
   Uses only Layer 1 primitives."
  (let ((code (char-code char)))
    (cond
      ;; Digits 0-9: ASCII 48-57
      ((and (>= code 48) (<= code 57))
       (let ((weight (- code 48)))
         (if (< weight radix) weight nil)))
      ;; Uppercase letters A-Z: ASCII 65-90
      ((and (>= code 65) (<= code 90))
       (let ((weight (+ 10 (- code 65))))
         (if (< weight radix) weight nil)))
      ;; Lowercase letters a-z: ASCII 97-122
      ((and (>= code 97) (<= code 122))
       (let ((weight (+ 10 (- code 97))))
         (if (< weight radix) weight nil)))
      (t nil))))

;;; T031: skip-whitespace - Skip leading whitespace characters
(defun skip-whitespace (string start end)
  "Return index of first non-whitespace character in STRING between START and END.
   Whitespace: space (32), tab (9), newline (10), carriage-return (13).
   Uses only Layer 1 primitives."
  (let ((i start))
    (loop while (< i end)
          do (let ((code (char-code (char string i))))
               (if (or (= code 32) (= code 9) (= code 10) (= code 13))
                   (setf i (+ i 1))
                   (return i))))
    i))

;;; T033: integer-to-digit - Convert numeric value to character
(defun integer-to-digit (weight)
  "Convert numeric WEIGHT (0-35) to its character representation.
   Uses only Layer 1 primitives."
  (if (< weight 10)
      (code-char (+ 48 weight))  ; '0' = 48
      (code-char (+ 55 weight)))) ; 'A' = 65, so 10 -> 65 means +55

;;; ============================================================
;;; parse-integer ([parse-integer](resources/HyperSpec/Body/f_parse_.htm))
;;; ============================================================

(defun parse-integer-rt (string &key (start 0) end (radix 10) junk-allowed)
  "Parse an integer from STRING.
   Returns two values: the integer and the index where parsing stopped.
   Uses only Layer 1 primitives.

   HyperSpec: [parse-integer](resources/HyperSpec/Body/f_parse_.htm)"
  (let* ((str-end (or end (length string)))
         (i (skip-whitespace string start str-end))
         (sign 1)
         (result 0)
         (found-digit nil))
    ;; Handle optional sign
    (when (< i str-end)
      (let ((c (char string i)))
        (cond
          ((char= c #\+) (setf i (+ i 1)))
          ((char= c #\-) (setf sign -1) (setf i (+ i 1))))))
    ;; Parse digits
    (loop while (< i str-end)
          do (let* ((c (char string i))
                    (weight (digit-char-to-weight c radix)))
               (if weight
                   (progn
                     (setf result (+ (* result radix) weight))
                     (setf found-digit t)
                     (setf i (+ i 1)))
                   (return))))
    ;; Skip trailing whitespace
    (let ((after-ws (skip-whitespace string i str-end)))
      ;; Check for valid parse
      (cond
        ((not found-digit)
         (if junk-allowed
             (values nil i)
             (error "No integer found in ~S" string)))
        ((and (not junk-allowed) (< after-ws str-end))
         (error "Junk in string ~S at position ~D" string i))
        (t
         (values (* sign result) after-ws))))))

;;; ============================================================
;;; write-to-string ([write-to-string](resources/HyperSpec/Body/f_wr_to_.htm))
;;; ============================================================

(defun write-to-string-rt (object &key (base 10))
  "Convert OBJECT to its string representation.
   For integers, outputs in specified BASE (2-36).
   Uses only Layer 1 primitives.

   HyperSpec: [write-to-string](resources/HyperSpec/Body/f_wr_to_.htm)"
  (cond
    ;; Integer case - convert to string in given base
    ((integerp object)
     (if (zerop object)
         "0"
         (let ((negative (< object 0))
               (n (abs object))
               (chars nil))
           ;; Build list of characters (reversed)
           (loop while (> n 0)
                 do (let ((digit (mod n base)))
                      (push (integer-to-digit digit) chars)
                      (setf n (floor n base))))
           ;; Add sign if negative
           (when negative (push #\- chars))
           ;; Convert to string
           (coerce chars 'string))))
    ;; Default: delegate to format for other types
    (t (format nil "~A" object))))

;;; ============================================================
;;; rationalize ([rationalize](resources/HyperSpec/Body/f_ration.htm))
;;; ============================================================

;;; T052: continued-fraction helper for rationalize
(defun continued-fraction-rationalize (x epsilon)
  "Convert float X to rational using continued fraction algorithm.
   EPSILON is the tolerance for approximation.
   Uses only Layer 1 primitives."
  (let ((sign (if (< x 0) -1 1))
        (x (abs x)))
    (if (< x epsilon)
        0
        (let ((a0 (floor x))
              (h-2 0) (h-1 1)
              (k-2 1) (k-1 0)
              (x-frac (- x (floor x))))
          (if (< x-frac epsilon)
              (* sign a0)
              (let ((h a0)
                    (k 1))
                ;; Limit iterations to prevent infinite loops
                (loop for iter from 0 below 50
                      while (> (abs x-frac) epsilon)
                      do (let* ((x-inv (/ 1.0d0 x-frac))
                                (a (floor x-inv)))
                           (setf x-frac (- x-inv a))
                           ;; Update convergents
                           (let ((h-new (+ (* a h) h-1))
                                 (k-new (+ (* a k) k-1)))
                             (setf h-2 h-1 h-1 h h h-new)
                             (setf k-2 k-1 k-1 k k k-new)
                             ;; Check if close enough
                             (when (< (abs (- (/ h k) (abs x))) epsilon)
                               (return)))))
                (* sign (/ h k))))))))

(defun rationalize-rt (number)
  "Convert NUMBER to a rational approximation.
   For integers/ratios, returns input unchanged.
   For floats, uses continued fraction algorithm.
   Uses only Layer 1 primitives.

   HyperSpec: [rationalize](resources/HyperSpec/Body/f_ration.htm)"
  (cond
    ;; Integers pass through unchanged
    ((integerp number) number)
    ;; Ratios pass through unchanged
    ((rationalp number) number)
    ;; Floats use continued fraction
    ((floatp number)
     ;; Use appropriate epsilon based on float precision
     (let ((epsilon (if (typep number 'single-float)
                        1.0e-6
                        1.0d-14)))
       (continued-fraction-rationalize number epsilon)))
    ;; Default: return as-is
    (t number)))

;;; ============================================================
;;; signum ([signum](resources/HyperSpec/Body/f_signum.htm))
;;; ============================================================

;;; T045: signum-integer helper
(defun signum-integer (n)
  "Return sign of integer N as integer (-1, 0, or 1)."
  (cond ((< n 0) -1)
        ((> n 0) 1)
        (t 0)))

;;; T046: signum-float helper
(defun signum-float (x)
  "Return sign of float X as float (-1.0, 0.0, or 1.0)."
  (cond ((< x 0.0) -1.0)
        ((> x 0.0) 1.0)
        (t 0.0)))

;;; T047: signum-complex helper (z/|z|)
(defun signum-complex (z)
  "Return z/|z| for complex Z, or 0 if Z is zero."
  (let ((r (realpart z))
        (i (imagpart z)))
    (if (and (zerop r) (zerop i))
        #C(0 0)
        (let ((mag (sqrt (+ (* r r) (* i i)))))
          (complex (/ r mag) (/ i mag))))))

;;; T048: Full signum with type dispatch
(defun signum-rt (number)
  "Return the sign of NUMBER in the same numeric type.
   Uses only Layer 1 primitives.

   HyperSpec: [signum](resources/HyperSpec/Body/f_signum.htm)"
  (cond
    ;; Integer: return -1, 0, or 1
    ((integerp number) (signum-integer number))
    ;; Ratio: return integer sign (ANSI CL spec)
    ((rationalp number) (signum-integer (if (< number 0) -1 (if (> number 0) 1 0))))
    ;; Float: return float sign
    ((floatp number) (signum-float number))
    ;; Complex: return z/|z|
    ((complexp number) (signum-complex number))
    ;; Default: delegate to CL signum
    (t (signum number))))

;;; ============================================================
;;; phase ([phase](resources/HyperSpec/Body/f_phase.htm))
;;; ============================================================

;;; T049: phase for real numbers (0 or pi)
(defun phase-real (x)
  "Return phase of real number X: 0.0 for non-negative, pi for negative."
  (if (< x 0) +pi+ 0.0d0))

;;; T050: phase for complex numbers (atan2)
(defun phase-complex (z)
  "Return phase (angle) of complex Z using atan2."
  (atan (imagpart z) (realpart z)))

;;; T051: Full phase with type dispatch
(defun phase-rt (number)
  "Return the angle (in radians) of NUMBER in the complex plane.
   Uses only Layer 1 primitives.

   HyperSpec: [phase](resources/HyperSpec/Body/f_phase.htm)"
  (cond
    ;; Complex: use atan2
    ((complexp number) (phase-complex number))
    ;; Real (integer, ratio, float): 0 or pi
    ((realp number) (phase-real number))
    ;; Default: delegate to CL phase
    (t (phase number))))

;;; ============================================================
;;; Registration function (called from func-section.lisp)
;;; ============================================================

;; Note: Registration is done in func-section.lisp via register-numeric-runtime-functions
