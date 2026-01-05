;;;; repl/core.lisp - Clysm REPL Core
;;;;
;;;; Read-Eval-Print-Loop for interactive Clysm development.
;;;; Integrates reader, compiler, and wasmtime runtime.

(in-package #:clysm)

;;; ============================================================
;;; REPL State
;;; ============================================================

(defvar *repl-history* nil
  "History of evaluated forms.")

(defvar *repl-results* nil
  "Results of evaluated forms (* ** ***).")

(defvar *repl-prompt* "clysm> "
  "REPL prompt string.")

(defvar *repl-running* nil
  "Flag to control REPL loop.")

(defvar *wasmtime-path* "wasmtime"
  "Path to wasmtime executable.")

(defvar *temp-wasm-path* "/tmp/clysm-repl.wasm"
  "Path for temporary Wasm files.")

;;; ============================================================
;;; Wasm Execution
;;; ============================================================

(defun run-wasm-module (wasm-bytes &key export-name)
  "Run a Wasm module and return the result.
WASM-BYTES is a list of bytes.
EXPORT-NAME is the function to call (default: _start or main)."
  ;; Write bytes to temp file
  (write-file-bytes *temp-wasm-path* wasm-bytes)

  ;; Run with wasmtime (GC and function-references features required for WasmGC)
  (let* ((export (or export-name "_start"))
         (command (format nil "~A --wasm gc=y --wasm function-references=y --invoke ~A ~A 2>&1"
                          *wasmtime-path* export *temp-wasm-path*))
         (output (run-shell-command command)))
    (parse-wasmtime-result output)))

(defun read-wasm-result-string (length)
  "Read the result string from a Wasm module by calling get_char.
The module must already be written to *temp-wasm-path*.
LENGTH is the string length returned by _start.
Each get_char call recomputes the expression (needed for CLI wasmtime)."
  (when (and (integerp length) (> length 0))
    (with-output-to-string (result)
      (dotimes (i length)
        (let* ((command (format nil "~A --wasm gc=y --wasm function-references=y --invoke get_char ~A ~D 2>&1"
                                *wasmtime-path* *temp-wasm-path* i))
               (output (run-shell-command command))
               (char-code (parse-wasmtime-result output)))
          (when (and (integerp char-code) (>= char-code 0))
            (write-char (code-char char-code) result)))))))

(defun run-wasm-with-print (wasm-bytes)
  "Run a Wasm module and return the printed string result.
First calls _start to get the length, then reads characters via get_char.
Each character retrieval recomputes the expression (needed for CLI wasmtime)."
  ;; Write bytes to temp file
  (write-file-bytes *temp-wasm-path* wasm-bytes)

  ;; Run _start to get length
  (let* ((command (format nil "~A --wasm gc=y --wasm function-references=y --invoke _start ~A 2>&1"
                          *wasmtime-path* *temp-wasm-path*))
         (output (run-shell-command command))
         (length (parse-wasmtime-result output)))
    (if (and (integerp length) (> length 0))
        (read-wasm-result-string length)
        "NIL")))

(defun run-shell-command (command)
  "Run a shell command and return its output as a string."
  ;; Using SBCL's run-program or UIOP if available
  ;; For portability, we use a simple approach
  (with-output-to-string (output)
    (let ((process (sb-ext:run-program
                    "/bin/sh"
                    (list "-c" command)
                    :output output
                    :error :output
                    :wait t)))
      (declare (ignore process)))))

(defun validate-wasm-module (wasm-bytes)
  "Validate a Wasm module using wasm-validate.
Returns T if valid, signals error otherwise."
  (write-file-bytes *temp-wasm-path* wasm-bytes)
  (let* ((command (format nil "wasm-validate ~A 2>&1" *temp-wasm-path*))
         (output (run-shell-command command)))
    (if (string= (string-trim '(#\Space #\Newline) output) "")
        t
        (error 'clysm-wasm-error
               :message (format nil "Invalid Wasm: ~A" output)))))

;;; ============================================================
;;; Compile and Run
;;; ============================================================

(defun clysm-eval (form)
  "Compile and evaluate a single Lisp form.
Returns the printed string representation of the result."
  ;; Wrap form in a function that returns the value
  (let* ((wrapped-forms
           (list form))  ; For now, just compile the form
         (wasm-bytes (compile-to-wasm wrapped-forms)))
    ;; Run and get the printed result string
    (run-wasm-with-print wasm-bytes)))

(defun clysm-eval-string (string)
  "Read and evaluate a string containing Lisp code.
Returns the result of the last form."
  (let ((forms (clysm-read-all-from-string string)))
    (let ((result nil))
      (dolist (form forms result)
        (setf result (clysm-eval form))))))

;;; ============================================================
;;; REPL Core
;;; ============================================================

(defun repl-read ()
  "Read a form from the user.
Returns the form or :EOF on end of input."
  (format t "~A" *repl-prompt*)
  (force-output)
  (handler-case
      (let ((line (read-line *standard-input* nil :eof)))
        (if (eq line :eof)
            :eof
            (let ((form (clysm-read-from-string line)))
              form)))
    (error (e)
      (format t "Read error: ~A~%" e)
      :error)))

(defun repl-eval (form)
  "Evaluate FORM and return the result.
Updates REPL history and result variables."
  (handler-case
      (let ((result (clysm-eval form)))
        ;; Update history
        (push form *repl-history*)
        ;; Update result variables (shift)
        (setf *repl-results*
              (list result
                    (first *repl-results*)
                    (second *repl-results*)))
        result)
    (error (e)
      (format t "Error: ~A~%" e)
      :error)))

(defun repl-print (result)
  "Print the result of evaluation."
  (unless (eq result :error)
    (if (stringp result)
        ;; Result is a printed string from Wasm
        (write-string result)
        ;; Fallback for host-evaluated results
        (clysm-print result))
    (terpri)))

(defun repl ()
  "Start the Clysm REPL."
  (format t "~%Clysm REPL v0.1.0~%")
  (format t "Type expressions to evaluate, or :quit to exit.~%~%")

  (setf *repl-running* t)
  (setf *repl-history* nil)
  (setf *repl-results* nil)

  (loop while *repl-running* do
    (let ((form (repl-read)))
      (cond
        ((eq form :eof)
         (format t "~%Goodbye!~%")
         (setf *repl-running* nil))

        ((eq form :error)
         ;; Error already printed, continue
         nil)

        ((and (symbolp form)
              (string= (symbol-name form) "QUIT"))
         (format t "Goodbye!~%")
         (setf *repl-running* nil))

        (t
         (let ((result (repl-eval form)))
           (repl-print result)))))))

;;; ============================================================
;;; Batch Evaluation
;;; ============================================================

(defun clysm-load (pathname)
  "Load and evaluate a Clysm source file."
  (with-open-file (stream pathname :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (let ((forms (clysm-read-all-from-string content)))
        (dolist (form forms)
          (clysm-eval form))))))

(defun clysm-compile-file (input-pathname &optional output-pathname)
  "Compile a Clysm source file to Wasm.
Returns the output pathname."
  (let ((output (or output-pathname
                    (make-pathname :defaults input-pathname
                                   :type "wasm"))))
    (with-open-file (stream input-pathname :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (let ((forms (clysm-read-all-from-string content)))
          (compile-to-wasm forms output))))
    output))

;;; ============================================================
;;; Simple Evaluation (No Wasm Runtime)
;;; ============================================================

;;; For development and testing, we can evaluate simple forms
;;; using the host Lisp when possible.

(defun clysm-eval-host (form)
  "Evaluate FORM using host Lisp.
Only works for constant expressions."
  (cond
    ;; Self-evaluating forms
    ((null form) nil)
    ((eq form t) t)
    ((numberp form) form)
    ((stringp form) form)
    ((characterp form) form)

    ;; Quoted forms
    ((and (consp form) (eq (car form) 'quote))
     (second form))

    ;; List construction
    ((and (consp form) (eq (car form) 'list))
     (mapcar #'clysm-eval-host (cdr form)))

    ;; Arithmetic (constant folding)
    ((and (consp form)
          (member (car form) '(+ - * /))
          (every #'numberp (cdr form)))
     (apply (car form) (cdr form)))

    ;; Cannot evaluate
    (t
     (error "Cannot evaluate ~S in host" form))))
