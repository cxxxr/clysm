;;;; live-image.lisp — connect the embedded compiler to the running image:
;;;; COMPILE produces WebAssembly and the REPL compiles every form.

(in-package :clysm)

(defun install-live-hooks ()
  (setf *live-macro-hook* (lambda (s) (macro-function s))
        *live-special-hook* (lambda (s) (sys::%special-p s))
        *live-var-type-hook* (lambda (s) (and (symbolp s) (get s 'sys::%var-type)))
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
  ;; the keyed main functions of generic functions compiled but not yet evaluated
  (setf sys::*note-key-layout-hook*
        (lambda (name layout defgeneric-p)
          (when (or defgeneric-p (not (gethash name *compiled-key-layouts*)))
            (setf (gethash name *compiled-key-layouts*) (key-layout-object layout)))))
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
        (lambda (kind function-name name index writable &optional fields kinds)
          (let ((*live-mode* t) (*live-packages* t))
            (case kind
              (:accessor (register-struct-accessor-inline function-name name index writable fields kinds))
              ;; a call of the constructor is of the structure's type
              (:constructor (setf (gethash function-name *struct-constructors*) name))
              (t (register-struct-predicate-inline function-name name fields))))))
  ;; macro expanders called by the compiler receive its environment objects
  (setq sys::*foreign-env-macroexpand-hook* #'cmacroexpand-1)
  (setq sys::*foreign-env-macro-function-hook* #'macro-function-in)
  (setf *macros-ready* t))

(defvar *native-eval-failure* nil
  "The error the compiler signaled for the last form NATIVE-EVAL interpreted instead.")

;;; Simple forms are evaluated without the compiler (as SBCL's
;;; SIMPLE-EVAL-IN-LEXENV does): a call of a global function whose arguments
;;; are simple, a variable, a constant, QUOTE, FUNCTION of a name, SETQ of a
;;; special variable, IF, PROGN, EVAL-WHEN, DECLAIM and the macros that
;;; expand to these, but not definitions (DEFMACRO, DEFSTRUCT, DEFCLASS...).
;;; Compiling a form costs a module and, the first time, V8's compilation of
;;; the compiler's functions: `clysm --eval t` took 0.2 s more than starting
;;; the REPL, and so did (load "file"), (require :asdf), (ql:quickload ...),
;;; Lem's requests of micros ((micros:autodoc '...)) and the DEFSYSTEMs of
;;; .asd files ((apply 'register-system-definition '...)), each 5-30 ms.  SIMPLE-FORM returns the form as a tree of (:CONST value),
;;; (:VAR symbol), (:FUNCTION name), (:CALL name arg...), (:SETQ symbol
;;; form), (:IF test then else), (:PROGN form...) and (:DECLAIM spec...), or NIL when it is not
;;; simple; the macros are expanded once, before anything is evaluated.

(defun simple-form (form &optional (depth 0))
  (when (> depth 40) (return-from simple-form nil))
  (flet ((sub (x) (simple-form x (1+ depth))))
    (cond ((symbolp form)
           (cond ((or (null form) (eq form t) (keywordp form)) (list :const form))
                 ((get form 'sys::%symbol-macro) (sub (macroexpand-1 form)))
                 (t (list :var form))))
          ((atom form) (list :const form))
          ((not (and (symbolp (car form)) (sys::%proper-list-p form))) nil)
          (t
           (let ((op (car form)) (args (cdr form)))
             (case op
               (quote (and (= (length args) 1) (list :const (car args))))
               (function (and (= (length args) 1)
                              (let ((name (car args)))
                                (and (or (and (symbolp name) name)
                                         (and (consp name) (eq (car name) 'setf)
                                              (consp (cdr name)) (symbolp (cadr name)) (null (cddr name))))
                                     (not (and (symbolp name) (or (macro-function name) (special-operator-p name))))
                                     (fboundp name)
                                     (list :function name)))))
               (setq (and (evenp (length args))
                          (let ((r (list :progn)))
                            (loop for (var value) on args by #'cddr
                                  do (unless (and (symbolp var) var (not (constantp var))
                                                  (sys::%special-p var) (not (get var 'sys::%symbol-macro)))
                                       (return-from simple-form nil))
                                     (let ((v (sub value)))
                                       (unless v (return-from simple-form nil))
                                       (push (list :setq var v) r)))
                            (nreverse r))))
               (if (and (<= 2 (length args) 3)
                        (let ((parts (mapcar #'sub args)))
                          (and (every #'identity parts)
                               (list :if (first parts) (second parts) (or (third parts) '(:const nil)))))))
               (progn (let ((parts (mapcar #'sub args)))
                        (and (every #'identity parts) (cons :progn parts))))
               (eval-when (and (consp args) (listp (car args))
                               (if (or (member :execute (car args)) (member 'eval (car args)))
                                   (sub `(progn ,@(cdr args)))
                                   (list :const nil))))
               ;; (the compiler also notes INLINE, NOTINLINE and SPECIAL)
               (declaim (list :declaim args))
               (t
                (cond ((special-operator-p op) nil)
                      ;; (operators the compiler has its own expansion of)
                      ((and (get op 'clysm-native)
                            (not (member op '(funcall apply values list list* vector))))
                       nil)
                      ;; (definitions: what the compiler does with them at
                      ;; toplevel is more than their expansion does; DECLAIM,
                      ;; DEFPACKAGE and IN-PACKAGE are calls of a function)
                      ((and (member op sys::%compile-time-definers)
                            (not (member op '(defpackage in-package))))
                       nil)
                      ((macro-function op) (sub (macroexpand-1 form)))
                      ((fboundp op)
                       (let ((parts (mapcar #'sub args)))
                         (and (every #'identity parts) (list* :call op parts))))))))))))

(defun simple-eval (tree)
  (ecase (car tree)
    (:const (cadr tree))
    (:var (symbol-value (cadr tree)))
    (:function (fdefinition (cadr tree)))
    (:setq (set (cadr tree) (simple-eval (caddr tree))))
    (:if (if (simple-eval (cadr tree)) (simple-eval (caddr tree)) (simple-eval (cadddr tree))))
    (:progn (let ((forms (cdr tree)))
              (if (null forms)
                  nil
                  (loop (if (cdr forms)
                            (simple-eval (pop forms))
                            (return (simple-eval (car forms))))))))
    (:declaim (let ((specs (cadr tree)))
                (sys::%call-with-world-lock
                 (lambda () (let ((*live-mode* t) (*live-packages* t) (*read-package* *package*))
                              (dolist (d specs) (compile-time-proclaim d)))))
                (sys::%proclaim-list specs)))
    (:call (let ((f (fdefinition (cadr tree))))
             (apply f (mapcar #'simple-eval (cddr tree)))))))

(defun native-eval (form)
  "Compile FORM to a new Wasm module linked with this image and run it (or,
if it is simple, evaluate it: SIMPLE-FORM).  If the compiler signals an
error, the form is evaluated by the interpreter instead (so the error, if
any, happens at run time)."
  (let ((tree (ignore-errors (simple-form form))))
    (when tree (return-from native-eval (simple-eval tree))))
  (multiple-value-bind (bytes consts)
      (handler-case (let ((*live-inline-ok* t)
                          ;; (only an image started with CLYSM_SAVE_IMAGE
                          ;; records its modules for SAVE-IMAGE: host/run.mjs)
                          (*live-snapshots* (and (sys::%getenv "CLYSM_SAVE_IMAGE") t)))
                      ;; (the compiler's state is shared by the threads)
                      (sys::%call-with-world-lock (lambda () (live-compile-forms (list form)))))
        (error (e)
          (setq *native-eval-failure* e)
          (return-from native-eval (eval form))))
    (native-run-octets bytes consts)))

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
  ;; (a Uint8Array, made at once through the memory: octet by octet, through
  ;; put_char_buf, it took a tenth of the time of compiling a small DEFUN)
  (sys::%compile-module (js:from-octets octets) (sys::%js-wrap-lisp consts)))

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
