;;;; lisp-mode.lisp — editing Lisp, and evaluating it in the image Lem runs in
;;;;
;;;; Lem's own Lisp mode talks to a Lisp process through micros (its
;;;; SWANK).  Here Lem itself runs in a Lisp, Clysm's WebAssembly image,
;;;; which compiles what it evaluates to new WebAssembly modules: the mode
;;;; evaluates forms in that image.  Syntax highlighting and indentation
;;;; are those of Lem's Lisp mode (lem-lisp-syntax and its grammar).

(defpackage :lem-clysm/lisp-mode
  (:use :cl :lem :lem/language-mode :lem/listener-mode)
  (:export #:lisp-mode #:*lisp-mode-keymap* #:lisp-eval-last-expression #:lisp-eval-defun
           #:lisp-eval-region #:lisp-load-file #:lisp-eval-string #:lisp-repl
           #:lisp-repl-mode))
(in-package :lem-clysm/lisp-mode)

(set-syntax-parser lem-lisp-syntax:*syntax-table* (lem-lisp-mode/grammar:make-tmlanguage-lisp))

(define-major-mode lisp-mode language-mode
    (:name "Lisp"
     :description "Editing Common Lisp, evaluated in the image of Lem."
     :keymap *lisp-mode-keymap*
     :syntax-table lem-lisp-syntax:*syntax-table*
     :mode-hook *lisp-mode-hook*)
  (setf (variable-value 'indent-tabs-mode) nil
        (variable-value 'enable-syntax-highlight) t
        (variable-value 'calc-indent-function) 'lem-lisp-syntax:calc-indent
        (variable-value 'indent-when-yank) t
        (variable-value 'line-comment) ";"
        (variable-value 'insertion-line-comment) ";; "
        (variable-value 'language-mode-tag) 'lisp-mode
        (variable-value 'beginning-of-defun-function) 'lisp-beginning-of-defun
        (variable-value 'end-of-defun-function) 'lisp-end-of-defun))

(define-file-type ("lisp" "lsp" "l" "cl" "asd" "ros" "lem") lisp-mode)

(define-key *lisp-mode-keymap* "C-M-x" 'lisp-eval-defun)
(define-key *lisp-mode-keymap* "C-c C-c" 'lisp-eval-defun)
(define-key *lisp-mode-keymap* "C-x C-e" 'lisp-eval-last-expression)
(define-key *lisp-mode-keymap* "C-c C-e" 'lisp-eval-last-expression)
(define-key *lisp-mode-keymap* "C-c C-r" 'lisp-eval-region)
(define-key *lisp-mode-keymap* "C-c C-l" 'lisp-load-file)
(define-key *lisp-mode-keymap* "C-c C-z" 'lisp-repl)
(define-key *lisp-mode-keymap* "C-M-q" 'lisp-indent-sexp)
(define-key *global-keymap* "M-:" 'lisp-eval-string)

;;; top-level forms: they start with a parenthesis in the first column

(defun top-level-form-start-p (point)
  (and (start-line-p point) (eql (character-at point) #\()))

(defun lisp-beginning-of-defun (point n)
  (loop repeat n
        do (loop (unless (line-offset point -1) (return))
                 (when (top-level-form-start-p point) (return)))))

(defun lisp-end-of-defun (point n)
  (loop repeat n
        do (loop (unless (line-offset point 1) (buffer-end point) (return))
                 (when (top-level-form-start-p point) (return)))))

(defun defun-region (point)
  "The start and end of the top-level form at POINT."
  (let ((start (copy-point point :temporary))
        (end (copy-point point :temporary)))
    (line-start start)
    (loop until (or (top-level-form-start-p start) (not (line-offset start -1))))
    (move-point end start)
    (unless (form-offset end 1)
      (editor-error "No form here"))
    (values start end)))

(define-command lisp-indent-sexp () ()
  "Indent the form after the cursor."
  (with-point ((end (current-point)))
    (when (form-offset end 1)
      (indent-points (current-point) end))))

;;; evaluation

(defun buffer-package-name (point)
  "The name in the last IN-PACKAGE form before POINT, or NIL."
  (with-point ((p point))
    (when (search-backward-regexp p "^\\(in-package\\s")
      (let ((form (ignore-errors
                   (let ((*read-eval* nil))
                     (read-from-string (points-to-string p (or (form-offset (copy-point p :temporary) 1) p)))))))
        (and (consp form) (cdr form) (string (second form)))))))

(defun point-package (point)
  (or (let ((name (buffer-package-name point)))
        (and name (find-package (string-upcase name))))
      (find-package :cl-user)))

(defun eval-string (string package)
  "Evaluate the forms in STRING with *PACKAGE* bound to PACKAGE.  Return
what they printed and the printed values of the last form."
  (let* ((*package* package)
         (values '())
         (output (with-output-to-string (out)
                   (let ((*standard-output* out)
                         (*error-output* out)
                         (*trace-output* out))
                     (with-input-from-string (in string)
                       (loop with eof = in
                             for form = (read in nil eof)
                             until (eq form eof)
                             do (setf values (multiple-value-list (eval form)))))))))
    (values output
            (format nil "~{~S~^, ~}" values)
            *package*)))

(defun show-result (output printed)
  (when (plusp (length output))
    (with-pop-up-typeout-window (stream (make-buffer "*lisp-output*") :erase t)
      (write-string output stream)))
  (message "=> ~A" (if (string= printed "") "; No value" printed)))

(defun eval-in-buffer (string point)
  (handler-case
      (multiple-value-bind (output printed) (eval-string string (point-package point))
        (show-result output printed))
    (error (condition)
      (message "Error: ~A" condition))))

(define-command lisp-eval-last-expression () ()
  "Evaluate the form before the cursor."
  (with-point ((start (current-point)))
    (unless (form-offset start -1)
      (editor-error "No form before the cursor"))
    (eval-in-buffer (points-to-string start (current-point)) (current-point))))

(define-command lisp-eval-defun () ()
  "Evaluate the top-level form around the cursor."
  (multiple-value-bind (start end) (defun-region (current-point))
    (eval-in-buffer (points-to-string start end) start)))

(define-command lisp-eval-region (start end) (:region)
  "Evaluate the forms of the region."
  (eval-in-buffer (points-to-string start end) start))

(define-command lisp-load-file () ()
  "Evaluate the forms of the buffer."
  (with-point ((start (current-point)) (end (current-point)))
    (buffer-start start)
    (buffer-end end)
    (eval-in-buffer (points-to-string start end) start)))

(define-command lisp-eval-string (string) ((:string "Eval: "))
  "Evaluate the forms of STRING."
  (eval-in-buffer string (current-point)))

;;; the REPL: a listener whose expressions are evaluated in the image

(defvar *repl-package* (find-package :cl-user))

(define-major-mode lisp-repl-mode lisp-mode
    (:name "REPL"
     :keymap *lisp-repl-mode-keymap*
     :syntax-table lem-lisp-syntax:*syntax-table*)
  (setf (variable-value 'listener-set-prompt-function) 'repl-set-prompt
        (variable-value 'listener-check-input-function) 'repl-input-complete-p
        (variable-value 'listener-execute-function) 'repl-eval)
  (start-listener-mode (merge-pathnames "history/clysm-repl" (lem-home))))

(defun package-prompt-name (package)
  (let ((name (package-name package)))
    (if (string= name "COMMON-LISP-USER") "CL-USER" name)))

(defun repl-set-prompt (point)
  (insert-string point (format nil "~A> " (package-prompt-name *repl-package*)))
  point)

(defun repl-input-complete-p (point)
  (with-point ((start (input-start-point (point-buffer point))))
    (when (point<= start point)
      (let ((state (parse-partial-sexp start point)))
        (and (not (pps-state-string-or-comment-p state))
             (>= 0 (pps-state-paren-depth state)))))))

(defun repl-eval (point string)
  (let ((text (handler-case
                  (multiple-value-bind (output printed package)
                      (eval-string string *repl-package*)
                    (setf *repl-package* package)
                    (format nil "~A~:[~;~%~]~A" output
                            (and (plusp (length output))
                                 (char/= (char output (1- (length output))) #\Newline))
                            (if (string= printed "") "; No value" printed)))
                (error (condition)
                  (format nil "; Error: ~A" condition)))))
    (buffer-end point)
    (insert-string point text)
    (refresh-prompt (point-buffer point))))

(define-command lisp-repl () ()
  "Show the REPL of the image Lem runs in."
  (let ((buffer (or (get-buffer "*lisp-repl*")
                    (let ((buffer (make-buffer "*lisp-repl*")))
                      (change-buffer-mode buffer 'lisp-repl-mode)
                      (insert-string (buffer-point buffer)
                                     (format nil "; ~A ~A~%; Lem's own image: what is evaluated here changes the editor.~%"
                                             (lisp-implementation-type) (lisp-implementation-version)))
                      (refresh-prompt buffer nil)
                      buffer))))
    (switch-to-window (pop-to-buffer buffer))
    (buffer-end (buffer-point buffer))))
