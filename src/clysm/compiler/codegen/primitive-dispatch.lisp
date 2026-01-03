;;;; primitive-dispatch.lisp - Hash-table driven primitive dispatch
;;;;
;;;; Part of the Clysm compiler (001-primitive-dispatch-table)
;;;; Replaces monolithic case statement with O(1) hash-table lookup.

(in-package #:clysm/compiler/codegen/primitive-dispatch)

;;; Constants
(defconstant +primitive-symbol-table-size+ 300
  "Initial size for symbol dispatch table.")

(defconstant +primitive-string-table-size+ 50
  "Initial size for string dispatch table.")

;;; Primitive Entry Structure

(defstruct primitive-entry
  "Entry in the primitive dispatch table."
  (compiler-fn nil :type (or function null))  ; (op args env) -> instructions
  (arity nil :type (or fixnum null))          ; Expected arg count, nil = variadic
  (flags nil :type list))                     ; Optional plist for metadata

;;; Dispatch Tables

(defvar *primitive-symbol-table*
  (make-hash-table :test 'eq :size +primitive-symbol-table-size+)
  "Symbol-keyed dispatch table for standard primitives.")

(defvar *primitive-string-table*
  (make-hash-table :test 'equal :size +primitive-string-table-size+)
  "String-keyed dispatch table for cross-package symbols.")

;;; Registration API

(defun register-primitive-compiler (symbol compiler-fn &key arity flags string-name)
  "Register a primitive compiler function.
   SYMBOL: The primitive operation symbol
   COMPILER-FN: Function (op args env) -> instruction list
   ARITY: Optional expected argument count (nil = variadic)
   FLAGS: Optional plist of metadata
   STRING-NAME: If provided, also register with this string key
   Returns: The registered primitive-entry"
  (check-type symbol symbol)
  (check-type compiler-fn function)
  (check-type arity (or null (integer 0)))
  (let ((entry (make-primitive-entry :compiler-fn compiler-fn
                                     :arity arity
                                     :flags flags)))
    (setf (gethash symbol *primitive-symbol-table*) entry)
    (when string-name
      (check-type string-name string)
      (setf (gethash string-name *primitive-string-table*) entry))
    entry))

(defun unregister-primitive-compiler (symbol &key string-name)
  "Remove a primitive compiler registration.
   Returns T if entry was found and removed, NIL otherwise."
  (let ((removed (remhash symbol *primitive-symbol-table*)))
    (when string-name
      (remhash string-name *primitive-string-table*))
    removed))

;;; Query API

(defun primitive-compiler-entry (op)
  "Look up a primitive compiler entry.
   Tries symbol lookup first, then string lookup.
   Returns: primitive-entry or NIL"
  (or (gethash op *primitive-symbol-table*)
      (gethash (symbol-name op) *primitive-string-table*)))

(defun primitive-registered-p (op)
  "Check if a primitive is registered.
   Returns T if registered, NIL otherwise."
  (not (null (primitive-compiler-entry op))))

(defun list-registered-primitives (&key (table :all))
  "List all registered primitive keys.
   TABLE: :symbol, :string, or :all (default)"
  (let ((result '()))
    (when (member table '(:symbol :all))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (push k result))
               *primitive-symbol-table*))
    (when (member table '(:string :all))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (push k result))
               *primitive-string-table*))
    (nreverse result)))

;;; Dispatch Function

(defun dispatch-primitive (op args env)
  "Main dispatch function for primitive compilation.
   OP: The primitive operation symbol
   ARGS: Argument forms
   ENV: Compilation environment
   Returns: Instruction list if dispatched, NIL if not found"
  (let ((entry (primitive-compiler-entry op)))
    (when entry
      (funcall (primitive-entry-compiler-fn entry) op args env))))

;;; Internal/Debug API

(defun clear-primitive-tables ()
  "Clear all primitive registrations (for testing)."
  (clrhash *primitive-symbol-table*)
  (clrhash *primitive-string-table*)
  (values))

(defun describe-primitive (op &optional (stream *standard-output*))
  "Print information about a registered primitive."
  (let ((entry (primitive-compiler-entry op)))
    (if entry
        (format stream "~&Primitive: ~S~%  Compiler: ~S~%  Arity: ~A~%  Flags: ~S~%"
                op
                (primitive-entry-compiler-fn entry)
                (or (primitive-entry-arity entry) "variadic")
                (primitive-entry-flags entry))
        (format stream "~&Primitive ~S is not registered.~%" op))))

;;; Error Conditions

(define-condition primitive-registration-error (error)
  ((symbol :initarg :symbol :reader registration-error-symbol)
   (reason :initarg :reason :reader registration-error-reason))
  (:report (lambda (c s)
             (format s "Primitive registration error for ~S: ~A"
                     (registration-error-symbol c)
                     (registration-error-reason c)))))

;;; Exports handled by package.lisp
