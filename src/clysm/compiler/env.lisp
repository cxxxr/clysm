;;;; env.lisp - Lexical environment management

(in-package #:clysm/compiler/env)

(defstruct (binding (:conc-name binding-))
  "Variable binding information."
  (name nil :type symbol)
  (kind :lexical :type (member :lexical :special :function :macro))
  (index nil :type (or null fixnum))
  (mutable-p nil :type boolean))

(defstruct (lexical-env (:conc-name lexical-env-))
  "Lexical environment for compilation."
  (bindings nil :type list)
  (parent nil :type (or null lexical-env)))

(defun make-empty-env ()
  "Create an empty lexical environment."
  (make-lexical-env))

(defun extend-env (env bindings)
  "Extend environment with new bindings."
  (make-lexical-env :bindings bindings :parent env))

(defun lookup-binding (env name)
  "Look up a binding in the environment."
  (loop for e = env then (lexical-env-parent e)
        while e
        do (let ((binding (find name (lexical-env-bindings e)
                               :key #'binding-name)))
             (when binding
               (return binding)))))

;;; ============================================================
;;; Special Variable Registry (T005-T008)
;;; ============================================================

(defvar *special-variables* (make-hash-table :test 'eq)
  "Registry of declared special variables (T005).
   Maps symbol names to special-info structures.")

(defstruct (special-info (:conc-name special-info-))
  "Information about a declared special variable."
  (declared-p nil :type boolean)
  (has-init-form nil :type boolean)
  (source-location nil :type t))

(defun register-special-variable (name &key (has-init-form nil) (source-location nil))
  "Mark a symbol as a special variable (T006).
   Called when processing defvar/defparameter."
  (check-type name symbol)
  (setf (gethash name *special-variables*)
        (make-special-info :declared-p t
                           :has-init-form has-init-form
                           :source-location source-location))
  name)

(defun special-variable-p (name)
  "Check if a symbol is declared as special (T007).
   Returns T if NAME has been declared via defvar/defparameter."
  (let ((info (gethash name *special-variables*)))
    (and info (special-info-declared-p info))))

(defun get-special-info (name)
  "Get the special-info for a symbol, or NIL if not special."
  (gethash name *special-variables*))

(defun clear-special-variables ()
  "Clear the special variable registry (T008).
   Used to reset state between compilation units."
  (clrhash *special-variables*))
