;;;; primitives.lisp - Primitive operations for Stage 0 interpreter
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Task T008: Primitive implementations
;;;;
;;;; Implements primitives per spec.md:
;;;; - Arithmetic: +, -, *, /
;;;; - Comparison: <, >, =
;;;; - List: car, cdr, cons
;;;; - Equality: eq

(in-package #:clysm/stage0)

;;; ============================================================
;;; Primitive Registry
;;; ============================================================

(defvar *primitives* (make-hash-table :test #'eq)
  "Hash table mapping primitive symbols to implementation functions")

(defun register-primitive (name fn)
  "Register a primitive function by name"
  (setf (gethash name *primitives*) fn))

(defun get-primitive (name)
  "Get primitive function by name, or NIL if not found"
  (gethash name *primitives*))

(defun primitive-p (name)
  "Check if NAME is a registered primitive"
  (not (null (gethash name *primitives*))))

;;; ============================================================
;;; T022-T025 [US1]: Arithmetic Primitives
;;; ============================================================

(defun prim-add (args)
  "Addition primitive: (+ &rest numbers) -> sum"
  (if (null args)
      0  ; (+) -> 0
      (reduce #'+ args)))

(defun prim-sub (args)
  "Subtraction primitive: (- number &rest numbers) -> difference"
  (cond
    ((null args) (error "- requires at least one argument"))
    ((null (cdr args)) (- (car args)))  ; (- x) -> -x
    (t (reduce #'- args))))

(defun prim-mul (args)
  "Multiplication primitive: (* &rest numbers) -> product"
  (if (null args)
      1  ; (*) -> 1
      (reduce #'* args)))

(defun prim-div (args)
  "Division primitive: (/ number &rest numbers) -> quotient"
  (cond
    ((null args) (error "/ requires at least one argument"))
    ((null (cdr args)) (/ 1 (car args)))  ; (/ x) -> 1/x
    (t (reduce (lambda (a b)
                 (if (zerop b)
                     (error "Division by zero")
                     (truncate a b)))  ; Integer division for bootstrap
               args))))

;;; ============================================================
;;; T058-T060 [US4]: Comparison Primitives
;;; ============================================================

(defun prim-lt (args)
  "Less-than primitive: (< a b) -> boolean"
  (when (< (length args) 2)
    (error "< requires at least two arguments"))
  (loop for (a b) on args
        while b
        always (< a b)))

(defun prim-gt (args)
  "Greater-than primitive: (> a b) -> boolean"
  (when (< (length args) 2)
    (error "> requires at least two arguments"))
  (loop for (a b) on args
        while b
        always (> a b)))

(defun prim-num-eq (args)
  "Numeric equality primitive: (= a b) -> boolean"
  (when (< (length args) 2)
    (error "= requires at least two arguments"))
  (loop for (a b) on args
        while b
        always (= a b)))

;;; ============================================================
;;; T083-T085 [US6]: List Primitives
;;; ============================================================

(defun prim-cons (args)
  "Cons primitive: (cons a b) -> cons cell"
  (unless (= (length args) 2)
    (error "cons requires exactly two arguments"))
  (cons (first args) (second args)))

(defun prim-car (args)
  "Car primitive: (car cons) -> first element"
  (unless (= (length args) 1)
    (error "car requires exactly one argument"))
  (let ((cell (first args)))
    (if (consp cell)
        (car cell)
        (error "car: argument is not a cons cell"))))

(defun prim-cdr (args)
  "Cdr primitive: (cdr cons) -> rest"
  (unless (= (length args) 1)
    (error "cdr requires exactly one argument"))
  (let ((cell (first args)))
    (if (consp cell)
        (cdr cell)
        (error "cdr: argument is not a cons cell"))))

;;; ============================================================
;;; T086 [US6]: Equality Primitive
;;; ============================================================

(defun prim-eq (args)
  "Eq primitive: (eq a b) -> boolean"
  (unless (= (length args) 2)
    (error "eq requires exactly two arguments"))
  (eq (first args) (second args)))

;;; ============================================================
;;; Primitive Registration
;;; ============================================================

(defun initialize-primitives ()
  "Register all primitive functions"
  ;; Arithmetic
  (register-primitive '+ #'prim-add)
  (register-primitive '- #'prim-sub)
  (register-primitive '* #'prim-mul)
  (register-primitive '/ #'prim-div)
  ;; Comparison
  (register-primitive '< #'prim-lt)
  (register-primitive '> #'prim-gt)
  (register-primitive '= #'prim-num-eq)
  ;; List operations
  (register-primitive 'cons #'prim-cons)
  (register-primitive 'car #'prim-car)
  (register-primitive 'cdr #'prim-cdr)
  ;; Equality
  (register-primitive 'eq #'prim-eq))

;; Initialize on load
(initialize-primitives)
