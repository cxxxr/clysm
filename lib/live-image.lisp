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
  ;; compiler macros defined by the runtime (AREF, FORMAT, MAKE-INSTANCE ...) and by loaded code
  (setf *live-compiler-macro-hook* (lambda (s) (compiler-macro-function s)))
  ;; calls with constant keywords use the fixed slots of the function's keyed main function
  (setf *live-key-layout-hook*
        (lambda (s)
          (and (symbolp s) (fboundp s) (not (macro-function s)) (not (special-operator-p s))
               (let ((f (symbol-function s)))
                 (and (functionp f) (sys::%closure-key-layout f))))))
  ;; small functions defined in the image are copied into later callers (guarded)
  (setf *live-inline-closure-hook*
        (lambda (s)
          (let ((c (get s 'sys::%inline-closure)))
            (and c (fboundp s) (eq c (symbol-function s)) c))))
  ;; structure accessors and predicates are inline in code compiled after the DEFSTRUCT
  (setq sys::*struct-inline-hook*
        (lambda (kind function-name name index writable)
          (let ((*live-mode* t) (*live-packages* t))
            (case kind
              (:accessor (register-struct-accessor-inline function-name name index writable))
              ;; a call of the constructor is of the structure's type
              (:constructor (setf (gethash function-name *struct-constructors*) name))
              (t (register-struct-predicate-inline function-name name))))))
  ;; macro expanders called by the compiler receive its environment objects
  (setq sys::*foreign-env-macroexpand-hook* #'cmacroexpand-1)
  (setq sys::*foreign-env-macro-function-hook* #'macro-function-in)
  (setf *macros-ready* t))

(defvar *native-eval-failure* nil
  "The error the compiler signaled for the last form NATIVE-EVAL interpreted instead.")

;;; A form that only calls a global function with constant arguments (a
;;; request of micros or SWANK: (micros:autodoc '...), (swank:connection-info);
;;; a DEFSYSTEM of an .asd file, (apply 'register-system-definition '...))
;;; is called at once: compiling it took 5 to 30 ms (and the first time,
;;; V8 compiling the compiler itself, hundreds of ms), for a call of
;;; microseconds.  Calling it is what the compiled form does.

(defun simple-call-function (head)
  "The function of HEAD for a call that is a call of it, or NIL."
  (and (symbolp head) (fboundp head)
       (not (special-operator-p head)) (not (macro-function head))
       (symbol-function head)))

(defun simple-form-p (form depth)
  "Whether FORM is a constant, a bound special variable, (FUNCTION name) or
a call of a global function with such arguments (to DEPTH calls)."
  (cond ((symbolp form)
         (or (keywordp form) (member form '(t nil))
             (and (boundp form) (not (get form 'sys::%symbol-macro))
                  (or (sys::%special-p form)
                      ;; a constant
                      (not (zerop (logand (sys::%symbol-flags form) 2)))))))
        ((atom form) t)
        ((not (listp (cdr form))) nil)
        ((eq (car form) 'quote) (and (consp (cdr form)) (null (cddr form))))
        ((eq (car form) 'function)
         (and (consp (cdr form)) (null (cddr form)) (simple-call-function (second form))))
        (t (and (> depth 0)
                (simple-call-function (car form))
                (not (compiler-macro-function (car form)))
                (handler-case (list-length form) (error () nil))
                (every (lambda (arg) (simple-form-p arg (- depth 1))) (cdr form))))))

(defun simple-eval (form)
  "The value of FORM, which SIMPLE-FORM-P accepts (its values if a call)."
  (cond ((symbolp form) (symbol-value form))
        ((atom form) form)
        ((eq (car form) 'quote) (second form))
        ((eq (car form) 'function) (symbol-function (second form)))
        (t (apply (symbol-function (car form))
                  (mapcar #'simple-eval (cdr form))))))

(defun native-eval (form)
  "Compile FORM to a new Wasm module linked with this image and run it.
If the compiler signals an error, the form is evaluated by the interpreter
instead (so the error, if any, happens at run time).  A call of a global
function with constant arguments, once its macros are expanded, is called
at once (SIMPLE-EVAL)."
  (let ((expanded form))
    (loop repeat 20
          do (when (simple-form-p expanded 4)
               (return-from native-eval (simple-eval expanded)))
             (unless (and (consp expanded) (symbolp (car expanded)) (macro-function (car expanded))
                          ;; (definitions: what the compiler does with them
                          ;; at toplevel is more than their expansion does)
                          ;; (but DEFPACKAGE and IN-PACKAGE, calls of a function)
                          (or (member (car expanded) '(defpackage in-package))
                              (not (member (car expanded) sys::%compile-time-definers)))
                          (not (member (car expanded) '(eval-when progn locally macrolet symbol-macrolet
                                                        defun defgeneric defmethod defvar defparameter
                                                        defconstant))))
               (return))
             ;; (an error of the expander is the compiler's to report)
             (multiple-value-bind (expansion more)
                 (handler-case (macroexpand-1 expanded) (error () (values nil nil)))
               (unless more (return))
               (setq expanded expansion))))
  ;; (anything else is compiled as it was written)
  (multiple-value-bind (bytes consts)
      (handler-case (let ((*live-inline-ok* t))
                      ;; (the compiler's state is shared by the threads)
                      (sys::%call-with-world-lock (lambda () (live-compile-forms (list form)))))
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
  (multiple-value-bind (bytes consts) (sys::%call-with-world-lock
                                       (lambda () (live-compile-forms (list form))))
    (values (coerce bytes '(simple-array (unsigned-byte 8) (*))) consts)))

(defun native-run-octets (octets consts)
  "Run a module made by NATIVE-COMPILE-FORM."
  (dotimes (i (length octets)) (sys::%put-char-buf (aref octets i)))
  (sys::%compile-module (sys::%js-take-string) (sys::%js-wrap-lisp consts)))

(defun native-run-module (file offset length consts)
  "Run the module stored in FILE (a native file name) at OFFSET."
  (let ((bytes (js:call (js:global) "__clysm_read_bytes" file offset length)))
    (sys::%compile-module bytes (sys::%js-wrap-lisp consts))))

(install-live-hooks)
(setq sys::*native-compile-form-hook* #'native-compile-form)
(setq sys::*native-run-module-hook* #'native-run-module)
(setq sys::*native-run-octets-hook* #'native-run-octets)
;; COMPILE-FILE compiles a file into one module (src/live.lisp)
(setq sys::*native-unit-hooks*
      (list #'call-with-compilation-unit #'unit-add-form #'unit-end-segment #'unit-finish))
;; compiled modules in fasls are valid only for this build of the image
(setq sys::*image-id* #.(format nil "~36R" (random (expt 2 64) (make-random-state t))))
(setq sys::*native-compile-hook* #'native-compile)
(setq sys::*native-eval-hook* #'native-eval)
(setq *package* (find-package "COMMON-LISP-USER"))
