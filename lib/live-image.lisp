;;;; live-image.lisp — connect the embedded compiler to the running image:
;;;; COMPILE produces WebAssembly and the REPL compiles every form.

(in-package :clysm)

;;; The compiler macros of these functions (defined in the runtime, where
;;; they are used when a program is compiled ahead of time) are also used for
;;; code compiled in the image.
(defparameter *live-compiler-macros*
  '(format write write-to-string write-string make-string-output-stream parse-integer))

(defun install-live-hooks ()
  (setf *live-compiler-macro-hook*
        (lambda (s) (and (member s *live-compiler-macros*) (compiler-macro-function s))))
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

(defvar *native-eval-failure* nil
  "The error the compiler signaled for the last form NATIVE-EVAL interpreted instead.")

(defun native-eval (form)
  "Compile FORM to a new Wasm module linked with this image and run it.
If the compiler signals an error, the form is evaluated by the interpreter
instead (so the error, if any, happens at run time)."
  (multiple-value-bind (bytes consts)
      (handler-case (live-compile-forms (list form))
        (error (e)
          (setq *native-eval-failure* e)
          (return-from native-eval (eval form))))
    (dotimes (i (length bytes)) (sys::%put-char-buf (aref bytes i)))
    (sys::%compile-module (sys::%js-take-string) (sys::%js-wrap-lisp consts))))

(defun native-compile (name definition)
  (cond (definition
         (let* ((*native-eval-failure* nil)
                (fn (if (functionp definition)
                        definition
                        (native-eval `(function ,definition)))))
           (when name (setf (fdefinition name) fn))
           ;; CLHS 3.2.5: an error during compilation (e.g. in a macro
           ;; expander) is reported as a warning, and COMPILE reports failure;
           ;; the function signals the error when it is called
           (if *native-eval-failure*
               (progn
                 (warn "COMPILE: ~A" *native-eval-failure*)
                 (values (or name fn) t t))
               (values (or name fn) nil nil))))
        (t
         (let ((src (get (sys::%function-name-symbol name) 'sys::%lambda-source)))
           (when src
             (native-eval `(sys::%defun ',name (function ,src))))
           (values name nil nil)))))

(defun native-repl-eval (form)
  (handler-case (native-eval form)
    (sys::%native-compile-failed () (eval form))))

(define-condition sys::%native-compile-failed (error) ())

(defun native-compile-form (form)
  "Compile FORM to a module (without running it): (values octets constants)."
  (multiple-value-bind (bytes consts) (live-compile-forms (list form))
    (values (coerce bytes '(simple-array (unsigned-byte 8) (*))) consts)))

(defun native-run-module (file offset length consts)
  "Run the module stored in FILE (a native file name) at OFFSET."
  (let ((bytes (js:call (js:global) "__clysm_read_bytes" file offset length)))
    (sys::%compile-module bytes (sys::%js-wrap-lisp consts))))

(install-live-hooks)
(setq sys::*native-compile-form-hook* #'native-compile-form)
(setq sys::*native-run-module-hook* #'native-run-module)
;; compiled modules in fasls are valid only for this build of the image
(setq sys::*image-id* #.(format nil "~36R" (random (expt 2 64) (make-random-state t))))
(setq sys::*native-compile-hook* #'native-compile)
(setq sys::*native-eval-hook* #'native-eval)
(setq *package* (find-package "COMMON-LISP-USER"))
