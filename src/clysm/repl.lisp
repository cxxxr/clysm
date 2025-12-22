;;;; repl.lisp - Read-Eval-Print Loop for Clysm

(in-package #:clysm/repl)

;;; REPL History

(defvar *repl-history* '()
  "History of REPL inputs.")

(defvar *repl-results* '()
  "History of REPL results.")

;;; Helper functions

(defun read-complete-expression (&optional (stream *standard-input*))
  "Read a complete S-expression, handling multi-line input."
  (let ((buffer (make-array 0 :element-type 'character
                              :fill-pointer 0
                              :adjustable t))
        (paren-depth 0)
        (in-string nil))
    ;; Read until we have a complete expression
    (loop
      (let ((char (read-char stream nil nil)))
        (cond
          ;; EOF
          ((null char)
           (if (zerop (length buffer))
               (return nil)
               (return (coerce buffer 'string))))
          ;; Newline - check if expression complete
          ((char= char #\Newline)
           (vector-push-extend char buffer)
           (when (and (not in-string) (zerop paren-depth) (> (length buffer) 1))
             (return (coerce buffer 'string))))
          ;; String handling
          ((char= char #\")
           (vector-push-extend char buffer)
           (setf in-string (not in-string)))
          ;; Escape in string
          ((and in-string (char= char #\\))
           (vector-push-extend char buffer)
           (let ((next (read-char stream nil nil)))
             (when next
               (vector-push-extend next buffer))))
          ;; Open paren
          ((and (not in-string) (char= char #\())
           (incf paren-depth)
           (vector-push-extend char buffer))
          ;; Close paren
          ((and (not in-string) (char= char #\)))
           (decf paren-depth)
           (vector-push-extend char buffer))
          ;; Other characters
          (t
           (vector-push-extend char buffer)))))))

;;; Main REPL

(defun eval* (form &optional env)
  "Evaluate a form using the Clysm interpreter.
   If ENV is provided, use it as the evaluation environment."
  ;; In a full implementation, this would:
  ;; 1. Compile the form to Wasm
  ;; 2. Execute the Wasm
  ;; 3. Return the result
  ;; For now, use the interpreter
  (clysm/eval/interpreter:interpret form env))

(defun repl ()
  "Start the Clysm REPL."
  (format t "~%Clysm REPL - WebAssembly GC Common Lisp Compiler~%")
  (format t "Type (quit) or :q to exit.~%~%")
  (loop
    (format t "CLYSM> ")
    (force-output)
    (handler-case
        (let ((input-string (read-complete-expression)))
          (cond
            ;; EOF
            ((null input-string)
             (format t "~%Goodbye!~%")
             (return))
            ;; Empty input
            ((string= (string-trim '(#\Space #\Tab #\Newline) input-string) "")
             nil)
            ;; Quit command
            ((or (string= (string-trim '(#\Space #\Tab #\Newline) input-string) ":q")
                 (string= (string-trim '(#\Space #\Tab #\Newline) input-string) ":quit"))
             (format t "Goodbye!~%")
             (return))
            ;; Normal expression
            (t
             (let ((expr (clysm/reader:read-from-string* input-string)))
               ;; Check for (quit) command
               (when (and (consp expr) (eq (car expr) 'quit))
                 (format t "Goodbye!~%")
                 (return))
               ;; Push to history
               (push input-string *repl-history*)
               ;; Evaluate and print
               (let ((result (eval* expr)))
                 (push result *repl-results*)
                 (clysm/runtime/printer:print* result))))))
      (clysm/reader/parser:parse-error (e)
        (format t "Read error: ~A~%~%" e))
      (error (e)
        (format t "Error: ~A~%~%" e)))))

(defun start-repl ()
  "Alias for repl."
  (repl))

;;; Compile and run interface

(defun compile-and-eval (string)
  "Compile and evaluate a string of Lisp code.
   Returns the result."
  (let ((expr (clysm/reader:read-from-string* string)))
    (eval* expr)))
