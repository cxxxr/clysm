;;;; bootstrap-source.lisp - Minimal bootstrap forms for Stage 0
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Task T044: Define bootstrap compiler source forms
;;;;
;;;; These forms represent the minimal Lisp subset that Stage 0 can compile.
;;;; The goal is to produce Stage 1 >= 1KB that can reproduce itself (fixed-point).
;;;;
;;;; Bootstrap forms support:
;;;; - Fixnum arithmetic (+, -, *, /)
;;;; - Comparison operators (<, >, =)
;;;; - Control structures (if, let, let*)
;;;; - Function definitions (defun, lambda)
;;;; - List operations (cons, car, cdr)
;;;; - Basic predicates (eq)
;;;; - Quote and symbol references

(in-package #:clysm/stage0)

;;; ============================================================
;;; Bootstrap Source Definition
;;; ============================================================

(defparameter *bootstrap-source-forms*
  '(
    ;; ======== Arithmetic Functions ========
    ;; These functions demonstrate the arithmetic capabilities

    ;; Simple addition wrapper
    (defun add2 (a b)
      (+ a b))

    ;; Multi-argument sum using nested calls
    (defun add3 (a b c)
      (+ a (+ b c)))

    ;; Subtraction
    (defun sub2 (a b)
      (- a b))

    ;; Multiplication
    (defun mul2 (a b)
      (* a b))

    ;; Division
    (defun div2 (a b)
      (/ a b))

    ;; ======== Comparison Functions ========

    (defun less-than (a b)
      (< a b))

    (defun greater-than (a b)
      (> a b))

    (defun equal-num (a b)
      (= a b))

    ;; ======== Control Flow Functions ========

    ;; Absolute value using if
    (defun abs (x)
      (if (< x 0)
          (- 0 x)
          x))

    ;; Maximum of two numbers
    (defun max2 (a b)
      (if (> a b)
          a
          b))

    ;; Minimum of two numbers
    (defun min2 (a b)
      (if (< a b)
          a
          b))

    ;; Sign function
    (defun sign (x)
      (if (< x 0)
          -1
          (if (> x 0)
              1
              0)))

    ;; ======== Let Bindings ========

    ;; Simple let usage
    (defun double (x)
      (let ((y (+ x x)))
        y))

    ;; Nested let
    (defun triple (x)
      (let ((y (+ x x)))
        (let ((z (+ y x)))
          z)))

    ;; Let* for sequential bindings
    (defun quadruple (x)
      (let* ((y (+ x x))
             (z (+ y y)))
        z))

    ;; ======== List Operations ========

    ;; Create a pair
    (defun make-pair (a b)
      (cons a b))

    ;; Get first element
    (defun first (pair)
      (car pair))

    ;; Get second element
    (defun second (pair)
      (cdr pair))

    ;; Swap a pair
    (defun swap-pair (pair)
      (cons (cdr pair) (car pair)))

    ;; ======== Composite Functions ========

    ;; Factorial-like computations (inline, no cross-references)
    (defun fact4 ()
      (* (* (* 4 3) 2) 1))

    (defun fact5 ()
      (* (* (* (* 5 4) 3) 2) 1))

    ;; Fibonacci-like computation
    (defun fib-step (a b)
      (cons b (+ a b)))

    ;; Distance squared (no sqrt in i31)
    (defun dist-squared (x y)
      (+ (* x x) (* y y)))

    ;; Average of two numbers (integer only)
    (defun avg2 (a b)
      (/ (+ a b) 2))

    ;; ======== Identity and Projections ========

    (defun identity (x) x)

    (defun const0 (x) 0)

    (defun const1 (x) 1)

    (defun zero-p (x)
      (= x 0))

    (defun positive-p (x)
      (> x 0))

    (defun negative-p (x)
      (< x 0))

    ;; ======== Compound Predicates ========

    (defun in-range (x low high)
      (if (< x low)
          nil
          (if (> x high)
              nil
              t)))

    ;; ======== Higher-order style (with inline lambdas) ========
    ;; These test lambda compilation

    (defun apply-twice (f x)
      ;; This would need proper function application
      ;; Simplified version just doubles
      (+ x x))

    ;; ======== Expression Tests ========
    ;; These exercise various combinations

    (defun complex-expr-1 ()
      (+ (* 2 3) (* 4 5)))

    (defun complex-expr-2 ()
      (- (* 10 10) (+ 50 50)))

    (defun complex-expr-3 (x)
      (let ((a (+ x 1))
            (b (+ x 2)))
        (+ a b)))

    ;; ======== Self-Referential Core ========
    ;; These represent minimal compiler operations

    ;; Check if something is a fixnum (simplified)
    (defun fixnum-p (x)
      ;; In real implementation, would use type testing
      ;; Here we just return t for now
      t)

    ;; Simple evaluator dispatch (pattern)
    (defun eval-constant (x)
      x)

    ;; Compile constant to Wasm IR (pattern)
    (defun compile-const (n)
      ;; Returns "IR" as number representing instruction
      (+ 65 n))  ; 65 = i32.const base

    )
  "List of bootstrap source forms that Stage 0 can compile.
   These forms use only the subset supported by Stage 0:
   - Fixnum literals and arithmetic
   - Comparison operators
   - if/let/let*/defun
   - cons/car/cdr
   - eq for equality
   - quote for symbols")

(defun get-bootstrap-forms ()
  "Return the list of bootstrap source forms."
  *bootstrap-source-forms*)

(defun bootstrap-form-count ()
  "Return the number of bootstrap forms."
  (length *bootstrap-source-forms*))

;;; ============================================================
;;; Bootstrap Form Compilation (using host compiler)
;;; ============================================================

(defun compile-bootstrap-forms ()
  "Compile all bootstrap forms using the host compiler.
   Returns a list of (form-name . wasm-bytes) pairs."
  (let ((results '()))
    (dolist (form *bootstrap-source-forms*)
      (let ((name (if (and (consp form) (eq (first form) 'defun))
                      (second form)
                      'anonymous)))
        (handler-case
            (let ((bytes (clysm:compile-to-wasm form)))
              (push (cons name bytes) results))
          (error (e)
            (format *error-output* "~&Warning: Failed to compile ~A: ~A~%"
                    name e)))))
    (nreverse results)))

(defun estimate-bootstrap-size ()
  "Estimate the size of compiled bootstrap forms."
  (let ((total 0))
    (dolist (form *bootstrap-source-forms*)
      (handler-case
          (let ((bytes (clysm:compile-to-wasm form)))
            (incf total (length bytes)))
        (error (e)
          (declare (ignore e)))))
    total))
