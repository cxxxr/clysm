;;;; environment.lisp - Compilation environment

(in-package #:clysm/compiler)

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

(defstruct global-info
  "Information about a global variable."
  (index 0 :type integer)
  (type +type-i32+)
  (mutable t :type boolean)
  (constant-p nil :type boolean))

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

(defun env-add-global (env name type mutable init-value &key constant-p)
  "Add a global variable to the environment and module. Returns (new-env . index).
   INIT-VALUE must be a constant expression (just the value for now)."
  (let* ((module (compile-env-module env))
         ;; Count existing globals (runtime globals like heap pointer come first)
         (global-idx (length (wasm-module-globals module)))
         (info (make-global-info :index global-idx
                                 :type type
                                 :mutable mutable
                                 :constant-p constant-p))
         (new-env (env-extend env name info :global)))
    ;; Add the global to the WASM module
    (add-global module type mutable `((,+op-i32-const+ ,init-value)))
    (values new-env global-idx)))

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

;;; Symbol Table for Compilation
;;; Tracks interned symbols and their memory addresses

(defvar *symbol-table* nil
  "Hash table mapping symbol names (strings) to their addresses.
   Reset at the start of each compile-module.")

(defvar *string-table* nil
  "Hash table mapping strings to their addresses in the data section.
   Reset at the start of each compile-module.")

(defvar *static-data-offset* 0
  "Current offset in static data area (starts after heap base).")

(defun reset-symbol-table ()
  "Reset symbol and string tables for a fresh compilation."
  (setf *symbol-table* (make-hash-table :test 'equal))
  (setf *string-table* (make-hash-table :test 'equal))
  (setf *static-data-offset* 256))  ; Reserve first 256 bytes

(defun intern-compile-time-string (string)
  "Intern a string at compile time, returning its address.
   Strings are stored as: [length:i32][utf8-bytes...]"
  (or (gethash string *string-table*)
      (let* ((bytes (flexi-streams:string-to-octets string :external-format :utf-8))
             (len (length bytes))
             (addr *static-data-offset*))
        ;; Allocate space: 4 bytes for length + bytes
        (incf *static-data-offset* (+ 4 len))
        ;; Align to 4 bytes
        (setf *static-data-offset*
              (* 4 (ceiling *static-data-offset* 4)))
        (setf (gethash string *string-table*)
              (list addr len bytes))
        (gethash string *string-table*))))

(defun intern-compile-time-symbol (symbol)
  "Intern a symbol at compile time, returning its address.
   Symbols are stored as: [name-ptr:i32][value:i32][function:i32][plist:i32]"
  (let ((name (symbol-name symbol)))
    (or (gethash name *symbol-table*)
        (let* ((string-info (intern-compile-time-string name))
               (string-addr (first string-info))
               (sym-addr *static-data-offset*))
          ;; Allocate 16 bytes for symbol
          (incf *static-data-offset* 16)
          (setf (gethash name *symbol-table*)
                (list sym-addr string-addr symbol))
          (gethash name *symbol-table*)))))
