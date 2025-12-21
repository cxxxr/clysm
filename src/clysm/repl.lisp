;;;; repl.lisp - Read-Eval-Print Loop

(in-package #:clysm/repl)

(defun repl ()
  "Start the Clysm REPL."
  (format t "~%Clysm REPL - WebAssembly GC Common Lisp Compiler~%")
  (format t "Type (quit) to exit.~%~%")
  (loop
    (format t "CLYSM> ")
    (force-output)
    (handler-case
        (let* ((input (read))
               (result (eval* input)))
          (format t "~S~%~%" result))
      (end-of-file ()
        (return))
      (error (e)
        (format t "Error: ~A~%~%" e)))))

(defun start-repl ()
  "Alias for repl."
  (repl))
