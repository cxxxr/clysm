;;;; site/playground.lisp — the Lisp side of the playground (site/index.html)
;;;;
;;;; The image runs in a Web Worker (site/worker.mjs), which calls the
;;;; JavaScript global pgRun with the text of the editor or of the REPL's
;;;; input.  pgRun reads and evaluates the forms one at a time, each compiled
;;;; to WebAssembly as at the REPL (sys::%eval-toplevel), and returns an object
;;;;   { values: "printed values of the last form, one per line",
;;;;     error: "the condition's type and message" or null,
;;;;     canvas: 1 when the code drew on the canvas, else 0,
;;;;     package: the name of *PACKAGE* afterwards }
;;;; Output (*standard-output*) goes to the page as it is written.

(defpackage "PLAYGROUND"
  (:use "COMMON-LISP")
  (:export "CANVAS" "CONTEXT"))

(in-package "PLAYGROUND")

(defvar *canvas-used* nil)

(defun canvas ()
  "The page's canvas (an OffscreenCanvas of 480x320, given to the worker)."
  (setq *canvas-used* t)
  (js:get (js:global) "pgCanvas"))

(defun context ()
  "The 2D drawing context of the page's canvas."
  (let ((canvas (canvas)))
    (or (js:get (js:global) "pgContext")
        (let ((context (js:call canvas "getContext" "2d")))
          (js:set (js:global) "pgContext" context)
          context))))

(defun print-values (values)
  (with-output-to-string (out)
    (if values
        (loop for (v . more) on values
              do (prin1 v out)
                 (when more (terpri out)))
        (write-string "; No value" out))))

(defun report (condition)
  (format nil "~A: ~A" (type-of condition)
          (handler-case (princ-to-string condition)
            (error () "(the condition could not be printed)"))))

(defun run (source)
  (let ((text (js:to-lisp source))
        (values '())
        (error nil)
        (forms 0))
    (setq *canvas-used* nil)
    (with-input-from-string (in text)
      (block read-eval
        (handler-bind
            ((error (lambda (c)
                      (setq error (report c))
                      (return-from read-eval))))
          (loop
            (let ((form (read in nil in)))
              (when (eq form in) (return))
              (incf forms)
              (setq values (multiple-value-list (sys::%eval-toplevel form)))
              (setq *** ** ** * * (car values)))))))
    (finish-output)
    (js:object "values" (if (and (zerop forms) (not error)) "" (print-values values))
               "error" (if error error (js:eval "null"))
               "canvas" (if *canvas-used* 1 0)
               "package" (let ((name (package-name *package*)))
                           (if (string= name "COMMON-LISP-USER") "CL-USER" name)))))

(js:set (js:global) "pgRun" #'run)

(in-package "COMMON-LISP-USER")
