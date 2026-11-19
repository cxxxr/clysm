;;;; tests/lem/buffer-bench.lisp — Lem's buffer code on a large Lisp file,
;;;; without a display, timed: the same file in Clysm (loaded into the
;;;; desktop application by tools/lem-scenario.sh with LEM_BUFFER_BENCH=1)
;;;; and in SBCL (with Lem's lem/core and lem-lisp-syntax loaded), so that
;;;; the time of an action of the editor can be told apart from how fast
;;;; the Lisp runs it.
;;;;
;;;; CL-USER::*BENCH-FILE* is the file (src/prims.lisp); the results are
;;;; printed as "bench NAME MS" lines.

(defpackage :lem-buffer-bench (:use :cl))
(in-package :lem-buffer-bench)

(defvar *results* '())

(defmacro timed (name (&key (repeat 1)) &body body)
  `(let ((t0 (get-internal-real-time)))
     (dotimes (i ,repeat) ,@body)
     (let ((ms (/ (* 1000.0 (- (get-internal-real-time) t0)) internal-time-units-per-second ,repeat)))
       (push (cons ,name ms) *results*)
       (format t "~&bench ~A ~,2F~%" ,name ms))))

(defun file-buffer (file)
  (let ((buffer (lem:make-buffer "buffer-bench" :temporary t :enable-undo-p nil)))
    (lem:erase-buffer buffer)
    (lem:insert-string (lem:buffer-point buffer) (uiop:read-file-string file))
    (setf (lem:buffer-syntax-table buffer) lem-lisp-syntax:*syntax-table*)
    (lem:buffer-start (lem:buffer-point buffer))
    buffer))

(defun type-at (buffer line text)
  "Type TEXT at the start of LINE (a new line) as the Lisp mode's keys do:
Return indents, a space asks for the argument list (PARSE-FOR-AUTODOC).
The time of the autodoc parses, and of the indentation."
  (let ((point (lem:buffer-point buffer)) (autodoc 0) (indent 0))
    (lem:move-to-line point line)
    (lem:line-start point)
    (lem:insert-character point #\newline)
    (lem:line-offset point -1)
    (loop for c across text
          do (case c
               (#\newline
                (lem:insert-character point #\newline)
                (let ((t0 (get-internal-real-time)))
                  (lem:indent-line point)
                  (incf indent (- (get-internal-real-time) t0))))
               (#\space
                (lem:insert-character point #\space)
                (let ((t0 (get-internal-real-time)))
                  (lem-lisp-syntax:parse-for-autodoc point)
                  (incf autodoc (- (get-internal-real-time) t0))))
               (t (lem:insert-character point c))))
    (values (/ (* 1000.0 autodoc) internal-time-units-per-second)
            (/ (* 1000.0 indent) internal-time-units-per-second))))

(defparameter *text*
  "(defun scenario-sum (list &key (key #'identity))
(let ((total 0))
(dolist (x list total)
(when (numberp (funcall key x))
(incf total (funcall key x))))))
(scenario-sum (list 1 2 3) :key #'1+)
")

(defun run (file)
  (setf *results* '())
  (let ((buffer (file-buffer file)))
    (setf (lem:variable-value 'lem:calc-indent-function :buffer buffer) 'lem-lisp-syntax:calc-indent)
    (timed "load-file" () (file-buffer file))
    ;; every toplevel form, by FORM-OFFSET
    (timed "forms-forward" (:repeat 3)
      (let ((p (lem:copy-point (lem:buffer-start-point buffer) :temporary)))
        (loop while (lem:form-offset p 1))))
    (timed "search-forward" (:repeat 3)
      (let ((p (lem:copy-point (lem:buffer-start-point buffer) :temporary)))
        (loop while (lem:search-forward p "make-"))))
    (timed "line-numbers" (:repeat 3)
      (let ((p (lem:copy-point (lem:buffer-start-point buffer) :temporary)))
        (loop while (lem:line-offset p 1) sum (lem:line-number-at-point p))))
    ;; typing inside a function in the middle of the file (where the
    ;; scenario types): the spaces' autodoc parses
    (multiple-value-bind (autodoc indent) (type-at buffer 1021 *text*)
      (format t "~&bench type-autodoc ~,2F~%bench type-indent ~,2F~%" autodoc indent)
      (push (cons "type-autodoc" autodoc) *results*)
      (push (cons "type-indent" indent) *results*))
    (lem:delete-buffer buffer)
    (reverse *results*)))
