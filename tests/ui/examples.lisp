;;;; The examples of the guide (docs/ui/guide/*.md) on the S-expression
;;;; backend.  tools/ui-doc.py --examples extracts every code block that
;;;; starts with ";;; example: NAME" into build/tests/guide-examples.lisp,
;;;; which calls BEGIN-EXAMPLE and END-EXAMPLE around each; tools/test-ui.sh
;;;; runs it after this file and compares the output with examples.expected:
;;;; what each example shows, and the errors it signalled.

(defpackage "UI-EXAMPLES" (:use "CL" "UI"))
(in-package "UI-EXAMPLES")

(setf *backend* (make-instance 'sexp-backend))

(defun shorten (text)
  (let ((text (substitute #\Space #\Newline text)))
    (if (> (length text) 160) (concatenate 'string (subseq text 0 157) "...") text)))

(defun begin-example (name)
  (format t "~&== ~A~%" name)
  (backend-log *backend*))

(defun end-example ()
  (flush)
  ;; what is on the display: the containers, and the overlay
  (dolist (entry (named-roots))
    (let ((text (node-text (cdr entry))))
      (when (plusp (length text))
        (format t "~A: ~A~%" (car entry) (shorten text)))))
  (let ((text (node-text (overlay-container *backend*))))
    (when (plusp (length text))
      (format t "overlay: ~A~%" (shorten text))))
  (dolist (operation (backend-log *backend*))
    (when (eq (car operation) :error)
      (format t "ERROR: ~A~%" (second operation))))
  ;; off the display for the next example
  (dolist (interface ui:*interfaces*) (quit-interface interface))
  (dolist (root ui::*roots*)
    ;; (not the overlay of dialogs and windows, which stays)
    (unless (rassoc root ui::*overlays*)
      (unmount root)))
  (setf ui::*dialogs* nil ui::*windows* nil ui::*popup* nil ui::*input-context* nil)
  (flush)
  (backend-log *backend*)
  nil)
