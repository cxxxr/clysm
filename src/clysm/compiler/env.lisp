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
