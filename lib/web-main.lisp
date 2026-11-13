;;; image for the browser: the runtime only; the host calls eval_string

;;; functions for the page, in the JavaScript global clysmRepl:
;;;   clysmRepl.complete(src)  true unless SRC ends inside a form (the page
;;;                            then lets Enter insert a newline)
;;;   clysmRepl.packageName()  the name of *PACKAGE*, for the prompt
;;;   clysmRepl.unbindTo(sp)   undo the dynamic bindings made since the
;;;                            binding stack pointer was SP (the exported
;;;                            global bsp), after a JavaScript exception such
;;;                            as a stack overflow left them in place
(defun %web-input-complete-p (src)
  (let ((*read-eval* nil))
    (with-input-from-string (s src)
      (handler-case
          (loop (when (eq (read s nil s) s) (return t)))
        (end-of-file () nil)
        ;; other reader errors are reported when the input is evaluated
        (error () t)))))

(defun %web-package-name ()
  (let ((name (package-name *package*)))
    (if (string= name "COMMON-LISP-USER") "CL-USER" name)))

(js:set (js:global) "clysmRepl"
        (js:object "complete" #'%web-input-complete-p
                   "packageName" #'%web-package-name
                   "unbindTo" (lambda (sp) (sys::%unbind-to sp))))
