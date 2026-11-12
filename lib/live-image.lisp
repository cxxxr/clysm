;;;; live-image.lisp — connect the embedded compiler to the running image:
;;;; COMPILE produces WebAssembly and the REPL compiles every form.

(in-package :clysm)

(defun install-live-hooks ()
  (setf *live-macro-hook* (lambda (s) (macro-function s))
        *live-special-hook* (lambda (s) (sys::%special-p s))
        *live-constant-hook*
        (lambda (s)
          (if (and (symbolp s) (not (keywordp s)) (boundp s)
                   (not (zerop (logand (sys::%symbol-flags s) 2))))
              (values (symbol-value s) t)
              (values nil nil)))
        *live-symbol-macro-hook*
        (lambda (s)
          (let ((e (get s 'sys::%symbol-macro)))
            (if e (values (car e) t) (values nil nil)))))
  ;; macro expanders called by the compiler receive its environment objects
  (setq sys::*foreign-env-macroexpand-hook* #'cmacroexpand-1)
  (setf *macros-ready* t))

(defun native-eval (form)
  "Compile FORM to a new Wasm module linked with this image and run it.
If the compiler signals an error, the form is evaluated by the interpreter
instead (so the error, if any, happens at run time)."
  (multiple-value-bind (bytes consts)
      (handler-case (live-compile-forms (list form))
        (error () (return-from native-eval (eval form))))
    (dotimes (i (length bytes)) (sys::%put-char-buf (aref bytes i)))
    (sys::%compile-module (sys::%js-take-string) (sys::%js-wrap-lisp consts))))

(defun native-compile (name definition)
  (cond (definition
         (let ((fn (if (functionp definition)
                       definition
                       (native-eval `(function ,definition)))))
           (when name (setf (fdefinition name) fn))
           (values (or name fn) nil nil)))
        (t
         (let ((src (get (sys::%function-name-symbol name) 'sys::%lambda-source)))
           (when src
             (native-eval `(sys::%defun ',name (function ,src))))
           (values name nil nil)))))

(defun native-repl-eval (form)
  (handler-case (native-eval form)
    (sys::%native-compile-failed () (eval form))))

(define-condition sys::%native-compile-failed (error) ())

(install-live-hooks)
(setq sys::*native-compile-hook* #'native-compile)
(setq sys::*native-eval-hook* #'native-eval)
(setq *package* (find-package "COMMON-LISP-USER"))
