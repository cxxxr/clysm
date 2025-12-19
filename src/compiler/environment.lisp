;;;; environment.lisp - Compilation environment

(in-package #:cl-wasm/compiler)

;;; Compilation Environment

(defstruct compile-env
  "Environment for compilation."
  (locals nil :type list)           ; Alist of (name . local-info)
  (functions nil :type list)        ; Alist of (name . func-info)
  (globals nil :type list)          ; Alist of (name . global-info)
  (types nil :type list)            ; Type definitions
  (module nil)                      ; The wasm-module being built
  (local-count 0 :type integer)
  (func-count 0 :type integer)
  (blocks nil :type list)           ; List of (name . depth) for block/return-from
  (block-depth 0 :type integer))    ; Current block nesting depth

(defstruct local-info
  "Information about a local variable."
  (index 0 :type integer)
  (type nil)
  (mutable t :type boolean))

(defstruct func-info
  "Information about a function."
  (index 0 :type integer)
  (type-index 0 :type integer)
  (params nil :type list)
  (results nil :type list))

;;; Environment Operations

(defun env-lookup (env name &optional (namespace :variable))
  "Look up a name in the environment."
  (ecase namespace
    (:variable
     (cdr (assoc name (compile-env-locals env))))
    (:function
     (cdr (assoc name (compile-env-functions env))))
    (:global
     (cdr (assoc name (compile-env-globals env))))))

(defun env-extend (env name info &optional (namespace :variable))
  "Extend the environment with a new binding."
  (let ((new-env (copy-compile-env env)))
    (ecase namespace
      (:variable
       (push (cons name info) (compile-env-locals new-env)))
      (:function
       (push (cons name info) (compile-env-functions new-env)))
      (:global
       (push (cons name info) (compile-env-globals new-env))))
    new-env))

(defun env-add-local (env name type)
  "Add a local variable to the environment. Returns (new-env . index)."
  (let* ((index (compile-env-local-count env))
         (info (make-local-info :index index :type type))
         (new-env (env-extend env name info :variable)))
    (incf (compile-env-local-count new-env))
    (values new-env index)))

(defun env-add-function (env name params results)
  "Register a function in the environment. Returns (new-env . index)."
  (let* ((module (compile-env-module env))
         (type-idx (add-func-type module params results))
         (func-idx (compile-env-func-count env))
         (info (make-func-info :index func-idx
                               :type-index type-idx
                               :params params
                               :results results))
         (new-env (env-extend env name info :function)))
    (incf (compile-env-func-count new-env))
    (values new-env func-idx type-idx)))

(defun make-initial-env (module)
  "Create an initial compilation environment with a module."
  (make-compile-env :module module))

;;; Environment with primitives

(defun env-with-primitives (env)
  "Add primitive function bindings to the environment."
  ;; For now, primitives are handled specially during codegen
  ;; This is a placeholder for when we have actual primitive functions
  env)

;;; Lambda/Closure Support Variables
;;; Defined here so they're available when special-forms.lisp is loaded

(defvar *pending-lambdas* nil
  "Box containing list of pending lambda functions to add after defuns.
   Set by compile-module during two-pass compilation.")
