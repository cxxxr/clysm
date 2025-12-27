;;;; interpreter-macros.lisp - Macro system for Tier 1 interpreter
;;;; Feature 044: Interpreter Bootstrap Strategy

(in-package #:clysm/eval/interpreter)

;;; ============================================================
;;; Macro Expander Structure
;;; ============================================================

(defstruct (macro-expander (:constructor %make-macro-expander))
  "A macro registered for expansion during interpretation."
  (name nil :type symbol)
  (lambda-list nil :type list)
  (body nil :type list)
  (env nil :type (or null interpreter-env))
  (whole-var nil :type (or null symbol))
  (env-var nil :type (or null symbol)))

;;; ============================================================
;;; Macro Registry
;;; ============================================================

(defvar *macro-registry* (make-hash-table :test 'eq)
  "Global hash table mapping macro names to macro-expander structs.")

(defun register-macro (name expander)
  "Register EXPANDER under NAME in the macro registry."
  (setf (gethash name *macro-registry*) expander)
  name)

(defun lookup-macro (name)
  "Look up macro by NAME, returning macro-expander or nil."
  (gethash name *macro-registry*))

(defun macro-registered-p (name)
  "Check if NAME is a registered macro."
  (nth-value 1 (gethash name *macro-registry*)))

(defun clear-macro-registry ()
  "Clear all registered macros."
  (clrhash *macro-registry*))

;;; ============================================================
;;; Macro Lambda-List Parsing
;;; ============================================================

(defun parse-macro-lambda-list (lambda-list)
  "Parse a macro lambda-list, extracting &whole, &environment, and body.
   Returns (values whole-var env-var remaining-lambda-list)."
  (let ((whole-var nil)
        (env-var nil)
        (remaining lambda-list))
    ;; Extract &whole (must be first if present)
    (when (and remaining (eq (first remaining) '&whole))
      (unless (cdr remaining)
        (error "Missing variable after &whole"))
      (setf whole-var (second remaining))
      (setf remaining (cddr remaining)))
    ;; Extract &environment from anywhere in the list
    (let ((env-pos (position '&environment remaining)))
      (when env-pos
        (unless (< env-pos (1- (length remaining)))
          (error "Missing variable after &environment"))
        (setf env-var (nth (1+ env-pos) remaining))
        ;; Remove &environment and its argument from the list
        (setf remaining (append (subseq remaining 0 env-pos)
                               (subseq remaining (+ env-pos 2))))))
    (values whole-var env-var remaining)))

;;; ============================================================
;;; Macro Expansion
;;; ============================================================

(defvar *macro-expansion-limit* 1000
  "Maximum depth for macro expansion to prevent infinite loops.")

(define-condition macro-expansion-depth-exceeded (error)
  ((macro-name :initarg :macro-name :reader mede-macro-name)
   (depth :initarg :depth :reader mede-depth))
  (:report (lambda (c s)
             (format s "Macro expansion depth exceeded (~D) for ~S"
                     (mede-depth c) (mede-macro-name c)))))

(defun maybe-macroexpand (form env &optional (depth 0))
  "Macroexpand FORM if it's a macro call, recursively until no longer a macro.
   Returns the fully expanded form."
  (when (>= depth *macro-expansion-limit*)
    (error 'macro-expansion-depth-exceeded
           :macro-name (when (consp form) (first form))
           :depth depth))
  (if (and (consp form) (symbolp (first form)))
      (let ((expander (lookup-macro (first form))))
        (if expander
            (let ((expanded (apply-macro-expander expander form env)))
              (maybe-macroexpand expanded env (1+ depth)))
            form))
      form))

(defun macroexpand-1-interpreter (form &optional env)
  "Perform one step of macro expansion.
   Returns (values expanded-form expanded-p)."
  (if (and (consp form) (symbolp (first form)))
      (let ((expander (lookup-macro (first form))))
        (if expander
            (values (apply-macro-expander expander form env) t)
            (values form nil)))
      (values form nil)))

(defun macroexpand-interpreter (form &optional env)
  "Fully macroexpand FORM.
   Returns (values expanded-form expanded-p)."
  (let ((expanded-p nil)
        (current form))
    (loop for depth from 0 below *macro-expansion-limit*
          do (multiple-value-bind (new-form did-expand)
                 (macroexpand-1-interpreter current env)
               (if did-expand
                   (setf current new-form
                         expanded-p t)
                   (return)))
          finally (error 'macro-expansion-depth-exceeded
                        :macro-name (when (consp form) (first form))
                        :depth *macro-expansion-limit*))
    (values current expanded-p)))

;;; ============================================================
;;; Macro Application
;;; ============================================================

(defun apply-macro-expander (expander form env)
  "Apply EXPANDER to FORM in environment ENV.
   Returns the expanded form."
  (let* ((whole-var (macro-expander-whole-var expander))
         (env-var (macro-expander-env-var expander))
         (lambda-list (macro-expander-lambda-list expander))
         (body (macro-expander-body expander))
         (macro-env (macro-expander-env expander))
         (args (rest form)))
    ;; Build bindings for macro expansion
    (let ((bindings nil))
      ;; Bind &whole if present
      (when whole-var
        (push (cons whole-var form) bindings))
      ;; Bind &environment if present
      (when env-var
        (push (cons env-var env) bindings))
      ;; Bind regular parameters from lambda-list
      (setf bindings (nconc bindings
                           (bind-macro-args lambda-list args)))
      ;; Create expansion environment and evaluate body
      (let ((expansion-env (extend-env macro-env bindings)))
        (interpret-progn body expansion-env)))))

;; NOTE: bind-macro-args is defined in interpreter.lisp
;; It has been unified to support both 2-arg calls (from apply-macro-expander)
;; and 3-arg calls (from expand-macro-call) via optional whole-form parameter.

;;; ============================================================
;;; Standard Macro Registration
;;; ============================================================

(defun install-standard-macros-interpreter ()
  "Install standard CL macros into the interpreter macro registry."
  ;; when
  (register-macro 'when
    (%make-macro-expander
     :name 'when
     :lambda-list '(test &body body)
     :body '((list 'if test (cons 'progn body)))))

  ;; unless
  (register-macro 'unless
    (%make-macro-expander
     :name 'unless
     :lambda-list '(test &body body)
     :body '((list 'if test nil (cons 'progn body)))))

  ;; and - expands to nested if
  (register-macro 'and
    (%make-macro-expander
     :name 'and
     :lambda-list '(&rest forms)
     :body '((cond
              ((null forms) t)
              ((null (cdr forms)) (car forms))
              (t (list 'if (car forms)
                       (cons 'and (cdr forms))
                       nil))))))

  ;; or - expands to let + if
  (register-macro 'or
    (%make-macro-expander
     :name 'or
     :lambda-list '(&rest forms)
     :body '((cond
              ((null forms) nil)
              ((null (cdr forms)) (car forms))
              (t (let ((temp (gensym "OR")))
                   (list 'let (list (list temp (car forms)))
                         (list 'if temp temp
                               (cons 'or (cdr forms))))))))))

  ;; prog1 - save first value
  (register-macro 'prog1
    (%make-macro-expander
     :name 'prog1
     :lambda-list '(first-form &body body)
     :body '((let ((result-var (gensym "PROG1")))
               (list 'let (list (list result-var first-form))
                     (cons 'progn (append body (list result-var))))))))

  ;; push
  (register-macro 'push
    (%make-macro-expander
     :name 'push
     :lambda-list '(item place)
     :body '((list 'setq place (list 'cons item place)))))

  ;; pop
  (register-macro 'pop
    (%make-macro-expander
     :name 'pop
     :lambda-list '(place)
     :body '((let ((temp (gensym "POP")))
               (list 'let (list (list temp (list 'car place)))
                     (list 'setq place (list 'cdr place))
                     temp)))))

  ;; incf
  (register-macro 'incf
    (%make-macro-expander
     :name 'incf
     :lambda-list '(place &optional (delta 1))
     :body '((list 'setq place (list '+ place delta)))))

  ;; decf
  (register-macro 'decf
    (%make-macro-expander
     :name 'decf
     :lambda-list '(place &optional (delta 1))
     :body '((list 'setq place (list '- place delta)))))

  ;; return - expands to return-from nil
  (register-macro 'return
    (%make-macro-expander
     :name 'return
     :lambda-list '(&optional value)
     :body '((list 'return-from nil value)))))
