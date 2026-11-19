;;;; tests/lem/buffer-bench.lisp — Lem's own code on a large Lisp file,
;;;; timed the same way in SBCL and in Clysm (tools/bench-lem.sh, which
;;;; loads it with tests/lem/load-lem.lisp): the editor's actions that the
;;;; scenario of a person using Lem on Clysm (tools/lem-scenario.sh) found
;;;; to take the time, without a display (Lem's fake display of its tests,
;;;; whose objects have the width of their text, as on a page).
;;;;
;;;; (lem-buffer-bench:main FILE) prints "l-NAME MS RESULT" lines, as the
;;;; files of bench/ do; RESULT, a number computed by the action, is the
;;;; same in both Lisps.

(defpackage :lem-buffer-bench
  (:use :cl)
  (:export :main))
(in-package :lem-buffer-bench)

(defclass bench-interface (lem-fake-interface:fake-interface)
  ((display-width :initform 120)
   (display-height :initform 50)))

;;; objects as wide as their text, as the frontend of Lem on Clysm measures them
(defmethod lem-if:object-width ((implementation bench-interface) object)
  (if (typep object 'lem-core/display:text-object)
      (lem:string-width (lem-core/display:text-object-string object))
      1))

(defparameter *text*
  "(defun scenario-sum (list &key (key #'identity))
(let ((total 0))
(dolist (x list total)
(when (numberp (funcall key x))
(incf total (funcall key x))))))
(scenario-sum (list 1 2 3) :key #'1+)
"
  "What the scenario types, in the middle of a function.")

(defvar *file-text* nil)

(defun lisp-buffer (name)
  "A buffer of the file's text, with the syntax table and the highlighting of
the Lisp mode, shown in the current window at its start."
  (let ((buffer (lem:make-buffer name :enable-undo-p nil)))
    (lem:erase-buffer buffer)
    (lem:insert-string (lem:buffer-point buffer) *file-text*)
    (setf (lem:buffer-syntax-table buffer) lem-lisp-syntax:*syntax-table*)
    (setf (lem:variable-value 'lem:enable-syntax-highlight :buffer buffer) t)
    (setf (lem:variable-value 'lem:calc-indent-function :buffer buffer) 'lem-lisp-syntax:calc-indent)
    (lem:buffer-start (lem:buffer-point buffer))
    (lem:switch-to-buffer buffer)
    (lem:redraw-display :force t)
    buffer))

(defun drop-buffer (buffer)
  (lem:delete-buffer buffer))

(defmacro timed (name &body body)
  "Run BODY, print its time and value (an integer)."
  `(let ((t0 (get-internal-real-time)))
     (let ((result (progn ,@body)))
       (format t "~&l-~A ~D ~D~%" ,name
               (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)
               result))))

(defun command (name)
  "Call the command NAME, as a command of its own (a C-n after a C-n would
take the column it saved, which a call outside the command loop does not
keep)."
  (setf lem-core::*last-flags* nil
        lem-core::*curr-flags* nil)
  (lem:call-command name nil))

(defun page-down (buffer)
  "C-v through the whole file, redisplaying each page (the new lines are
highlighted when the window scrolls): the lines on the screen, summed."
  (lem:buffer-start (lem:buffer-point buffer))
  (lem:redraw-display :force t)
  (loop repeat 150
        sum (progn (command 'lem:next-page) (lem:redraw-display)
                   (lem:line-number-at-point (lem:buffer-point buffer)))))

(defun line-down (buffer)
  "C-n over 600 lines, redisplaying after each."
  (lem:buffer-start (lem:buffer-point buffer))
  (lem:redraw-display :force t)
  (loop repeat 600
        sum (progn (command 'lem:next-line) (lem:redraw-display)
                   (lem:point-charpos (lem:buffer-point buffer)))))

(defun type-lisp (buffer)
  "Type *TEXT* on a new line inside a function in the middle of the file,
as the Lisp mode does: Return indents, a space asks for the argument list
(PARSE-FOR-AUTODOC), each key redisplays.  Values: the time of the autodoc
parses, of the indentation, of the rest (ms), and the length of the
contexts parsed."
  (let ((point (lem:buffer-point buffer)) (autodoc 0) (indent 0) (total 0) (size 0)
        (t0 (get-internal-real-time)))
    (lem:move-to-line point 1021)
    (lem:line-start point)
    (lem:insert-character point #\newline)
    (lem:line-offset point -1)
    (loop for c across *text*
          do (lem:insert-character point c)
             (case c
               (#\newline
                (let ((t1 (get-internal-real-time)))
                  (lem:indent-line point)
                  (incf indent (- (get-internal-real-time) t1))))
               (#\space
                (let ((t1 (get-internal-real-time)))
                  (incf size (length (prin1-to-string (lem-lisp-syntax:parse-for-autodoc point))))
                  (incf autodoc (- (get-internal-real-time) t1)))))
             (lem:redraw-display))
    (setf total (- (get-internal-real-time) t0))
    (flet ((ms (x) (round (* 1000 x) internal-time-units-per-second)))
      (values (ms autodoc) (ms indent) (ms (- total autodoc indent)) size))))

(defun forms-forward (buffer)
  "FORM-OFFSET over every toplevel form of the file, three times."
  (loop repeat 3
        sum (let ((p (lem:copy-point (lem:buffer-start-point buffer) :temporary)) (n 0))
              (loop while (lem:form-offset p 1) do (incf n))
              n)))

(defun scan-file (buffer)
  "The highlighting of the whole file (SYNTAX-SCAN-REGION), twice."
  (loop repeat 2
        do (lem:syntax-scan-region (lem:buffer-start-point buffer) (lem:buffer-end-point buffer)))
  (lem:buffer-nlines buffer))

(defun search-file (buffer)
  "SEARCH-FORWARD of a string through the file, five times: the matches."
  (loop repeat 5
        sum (let ((p (lem:copy-point (lem:buffer-start-point buffer) :temporary)))
              (loop while (lem:search-forward p "make-") count t))))

(defun widths (buffer)
  "STRING-WIDTH of every line, ten times (CHAR-WIDTH of each character)."
  (loop repeat 10
        sum (let ((p (lem:copy-point (lem:buffer-start-point buffer) :temporary)) (w 0))
              (loop do (incf w (lem:string-width (lem:line-string p)))
                    while (lem:line-offset p 1))
              w)))

(defun run ()
  (let ((buffer (lisp-buffer "bench.lisp")))
    (timed "page-down" (page-down buffer))
    (timed "line-down" (line-down buffer))
    (timed "forms-forward" (forms-forward buffer))
    (timed "syntax-scan" (scan-file buffer))
    (timed "search" (search-file buffer))
    (timed "string-width" (widths buffer))
    (multiple-value-bind (autodoc indent rest size) (type-lisp buffer)
      (format t "~&l-type-autodoc ~D ~D~%l-type-indent ~D 0~%l-type-redisplay ~D 0~%"
              autodoc size indent rest))
    (drop-buffer buffer)))

(defun main (file &key (warm-up t))
  "Time the actions on FILE, after running them once (so that V8 has
compiled the code that runs them) if WARM-UP."
  (setf *file-text* (uiop:read-file-string file))
  ;; (what loading the Lisp mode does)
  (lem:set-syntax-parser lem-lisp-syntax:*syntax-table* (lem-lisp-mode/grammar:make-tmlanguage-lisp))
  (lem:with-implementation (make-instance 'bench-interface)
    (lem:setup-first-frame)
    (when warm-up
      (let ((*standard-output* (make-broadcast-stream))) (run)))
    (run)))
