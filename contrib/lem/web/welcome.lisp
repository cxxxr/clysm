;;;; Welcome to Lem, the Common Lisp editor, running in your browser.
;;;;
;;;; Lem is compiled to WebAssembly by Clysm (https://github.com/cxxxr/clysm),
;;;; a Common Lisp implementation; nothing runs on a server.  The editor's
;;;; screen is drawn with Clysm's GUI framework.
;;;;
;;;; The keys are Emacs's:
;;;;   C-x C-f  open a file           C-x C-s  save        C-x b  switch buffer
;;;;   C-x 2    split the window      C-x o    other window C-x 1  one window
;;;;   C-s      search                C-/      undo        M-x    run a command
;;;;
;;;; Files live in your browser: what you save in the home directory is kept
;;;; there.  M-x open-local-file opens a file of your computer, and
;;;; M-x download-file saves the buffer to your computer.
;;;;
;;;; Lisp is evaluated in the image Lem itself runs in, compiled to
;;;; WebAssembly on the fly.  Put the cursor after a form below and press
;;;; C-x C-e, or anywhere in a definition and press C-M-x:

(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 25)

(loop for i from 1 to 10 collect (expt 2 i))

;;; The editor is in the same image, so you can change it as it runs:

(lem:message "Hello from ~A ~A!" (lisp-implementation-type) (lisp-implementation-version))

(lem:define-command hello () ()
  "Say hello: try M-x hello after evaluating this."
  (lem:message "Hello, world"))

;;; M-x lisp-repl opens a REPL of the same image.
