;;;; workflow.lisp - SBCL-based CLI entry point for Clysm
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; T030: SBCL-based CLI entry script
;;;;
;;;; Usage:
;;;;   sbcl --load build/workflow.lisp -- compile 'src/**/*.lisp' -o output.wasm
;;;;
;;;; This script loads Clysm and invokes the CLI main function with
;;;; command-line arguments.

(require :asdf)

;; Load the Clysm system
(handler-case
    (asdf:load-system "clysm")
  (error (e)
    (format *error-output* "Error loading Clysm: ~A~%" e)
    (uiop:quit 1)))

;; Get command-line arguments (after --)
(defun get-cli-args ()
  "Get command-line arguments after --."
  (let ((args (uiop:command-line-arguments)))
    ;; Skip everything before --
    (loop while (and args (not (equal (car args) "--")))
          do (pop args))
    ;; Skip the -- itself
    (when args (pop args))
    args))

;; Run the CLI
(let* ((args (get-cli-args))
       (exit-code (clysm/cli:main args)))
  (uiop:quit exit-code))
