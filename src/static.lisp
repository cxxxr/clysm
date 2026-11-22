;;;; static.lisp — a program compiled as a whole in the image that builds it
;;;;
;;;; `clysm build`: ASDF systems (and files) become ONE module together with
;;;; the runtime, compiled as a whole program is (direct calls, copies of
;;;; small functions, the types of the whole program, dispatch over all its
;;;; classes), where ASDF in the image would make a module of each file or
;;;; system, whose calls into the runtime and into each other the engine
;;;; cannot inline.
;;;;
;;;; The compiler that runs here is the image's (the REPL image: the systems
;;;; are loaded into it by ASDF as ever, so their macros, packages, reader
;;;; macros and what their files do when they are compiled and loaded are
;;;; there, exactly as for COMPILE-FILE), in a second state: that of the
;;;; program (the tables of RESET-COMPILER and the state of the code
;;;; generator), bound while a file is compiled into the program
;;;; (CALL-IN-STATIC-STATE) and unbound again around everything the image
;;;; evaluates meanwhile (CALL-IN-LIVE-STATE, src/frontend.lisp: EVAL-WHEN,
;;;; macro expanders being compiled, #.).  In the program's state the
;;;; packages of the program are the image's own (*IMAGE-PACKAGES*).
;;;;  * The runtime's files are compiled first, as for any whole program,
;;;;    but without evaluating what they define at compile time: the image is
;;;;    that runtime (*STATIC-IMAGE* is :RUNTIME).
;;;;  * Each source file of a system is compiled into the program when ASDF
;;;;    is about to load it (contrib/asdf-hooks.lisp calls
;;;;    *STATIC-FILE-HOOK*): the files before it are loaded, it is not, which
;;;;    is where COMPILE-FILE compiled it.
;;;;  * The program's own files are compiled after, and not loaded.

(in-package :clysm)

(defparameter *static-state-variables*
  '(;; (what RESET-COMPILER sets)
    *check-declared-integers* *macros* *symbol-macros* *specials* *constants* *var-types* *notinline*
    *compiler-macros* *global-defuns* *lam-counter* *warnings* *toplevel-lams* *current-toplevel*
    *toplevel-nodes* *toplevel-weight* *symbol-globals* *symbol-global-table* *const-globals*
    *const-table* *const-string-table* *data-strings* *const-counter* *closure-globals*
    *closure-global-names* *misc-globals* *pending-lams* *compiled-lams* *extra-types* *ltv-counter*
    *called-functions* *initial-symbol-values* *deferred-macros* *macros-ready* *dynamic-functions*
    *runtime-consts* *node-types* *ti-analyzed* *spec-clones* *raw-clones* *spec-cloneable*
    *inline-candidates* *inlined-into* *declared-inline* *declared-ftypes* *lvar-lams* *read-package*
    *target-packages* *host->target-name* *target-nicknames*
    ;; (the rest of the state of a module, *CODEGEN-STATE-BINDINGS*)
    *fasl-guard-globals* *fasl-guards-set* *fasl-guards-pending* *live-constants* *helpers*
    *current-lam* *catch-clones* *direct-closures*
    *static-literal-cells* *static-pending-forms* *static-literal-counter*
    *program-inline-expansions* *inline-expansion-depth*
    ;; (the mode)
    *static-state-p* *static-image* *live-mode* *live-packages* *image-packages*
    *live-macro-hook* *live-special-hook* *live-constant-hook* *live-var-type-hook*
    *live-symbol-macro-hook* *live-compiler-macro-hook* *live-inline-closure-hook*
    *live-direct-closure-hook* *live-key-layout-hook*
    *unit-env* *batch-forms* *batch-unknown-calls* *batch-no-direct* *batch-form-index*
    *host-load-defuns* *runtime-package-names* *shake* *shake-symbols* *eval-string*
    *runtime-top-count* *preinit-parts* *preinit-table-check* *m* *source-root* *current-source*
    *record-definitions* *redefinable-functions* *live-inline-ok* *live-toplevel-loop-p*
    *export-live-helpers* *runtime-copy-limit* *install-counter*
    clysm-sys::*gf-unit* clysm-sys::*proclaimed-safety*)
  "The variables of the state of a program built in the image.")

(defparameter *static-sink-variables* '(*specials* *constants* *compiler-macros* *initial-symbol-values*)
  "The tables of the program that forms evaluated in the image record in (TABLE-PUT).")

(defvar *static-file-hook* nil
  "While a program is built in the image: a function of an ASDF component,
called when ASDF is about to load its source file (contrib/asdf-hooks.lisp).")

(defun call-in-static-state (build thunk)
  "Call THUNK with the compiler in the state of the program BUILD, which
keeps what THUNK changes."
  (if *static-state-p*
      (funcall thunk)
      (let ((static (svref build 0)))
        (progv (mapcar #'car static) (mapcar #'cdr static)
          (unwind-protect (funcall thunk)
            (dolist (c static) (setf (cdr c) (symbol-value (car c)))))))))

(defun static-runtime-files (root)
  (with-open-file (s (concatenate 'string root "tools/runtime-files.lisp"))
    (mapcar (lambda (f) (concatenate 'string root "runtime/" f ".lisp")) (read s))))

(defun static-open (root)
  "Begin a program built in this image: the runtime (the files of the Clysm
installation ROOT, a directory name) is compiled.  The program, for
STATIC-ADD-FILE, STATIC-ADD-FORM and STATIC-FINISH."
  (when *static-build* (error "A program is being built in this image already."))
  (let* ((vars *static-state-variables*)
         (live (mapcar (lambda (v) (cons v (and (boundp v) (symbol-value v)))) vars))
         (build (vector (mapcar (lambda (v) (list v)) vars) live nil :runtime))
         (macro-hook *live-macro-hook*) (special-hook *live-special-hook*)
         (constant-hook *live-constant-hook*) (symbol-macro-hook *live-symbol-macro-hook*)
         (compiler-macro-hook *live-compiler-macro-hook*) (key-layout-hook *live-key-layout-hook*)
         (version (and (boundp 'clysm-sys::*clysm-version*) clysm-sys::*clysm-version*))
         (ok nil))
    (setq *static-build* build)
    (unwind-protect
         (progv vars (mapcar #'cdr live)
           (setq *static-state-p* t *static-image* :runtime
                 *live-mode* nil *live-packages* nil *image-packages* t
                 *target-packages* (make-hash-table :test 'equal)
                 *host->target-name* (make-hash-table :test 'eq)
                 *target-nicknames* (make-hash-table :test 'equal)
                 ;; (the runtime's files are compiled from what they say
                 ;; alone, as when SBCL hosts the compiler)
                 *live-macro-hook* nil *live-special-hook* nil *live-constant-hook* nil
                 *live-var-type-hook* nil *live-symbol-macro-hook* nil *live-compiler-macro-hook* nil
                 *live-inline-closure-hook* nil *live-direct-closure-hook* nil *live-key-layout-hook* nil
                 *unit-env* nil *batch-forms* nil *batch-unknown-calls* nil *batch-no-direct* nil
                 *batch-form-index* 0 *host-load-defuns* nil *shake* nil *shake-symbols* nil
                 *eval-string* nil *runtime-top-count* nil *preinit-parts* nil *preinit-table-check* nil
                 *source-root* (pathname root) *current-source* nil *record-definitions* nil
                 *redefinable-functions* nil *live-inline-ok* nil *live-toplevel-loop-p* nil
                 *export-live-helpers* nil *runtime-copy-limit* nil
                 *fasl-guard-globals* (make-hash-table :test 'equal)
                 *fasl-guards-set* (make-hash-table :test 'equal) *fasl-guards-pending* nil
                 *live-constants* nil *current-lam* nil *catch-clones* (make-hash-table :test 'equal)
                 *direct-closures* nil
                 *static-literal-cells* (make-hash-table :test 'eq) *static-pending-forms* nil
                 *static-literal-counter* 0
                 clysm-sys::*gf-unit* nil clysm-sys::*proclaimed-safety* 1)
           (reset-compiler)
           (setf (svref build 2)
                 (mapcar (lambda (v) (cons v (symbol-value v))) *static-sink-variables*))
           (let ((*read-package* (find-target-package "SYSTEM")))
             (dolist (f (static-runtime-files root)) (compile-source-file f))
             (when version
               (process-toplevel `(setq clysm-sys::*clysm-version* ,version) (make-cenv)))
             (finish-runtime))
           ;; the program's files: what the image knows of the systems loaded
           ;; into it (their macros, special variables, constants) is known
           (setq *static-image* :user
                 *live-macro-hook* macro-hook *live-special-hook* special-hook
                 *live-constant-hook* constant-hook *live-symbol-macro-hook* symbol-macro-hook
                 *live-compiler-macro-hook* compiler-macro-hook
                 ;; (a call with keywords of a function that is no DEFUN of
                 ;; the program, a generic function: by the keyed entry of
                 ;; the function as it is in the image, tested when it runs)
                 *live-key-layout-hook* key-layout-hook
                 clysm-sys::*gf-unit* (clysm-sys::%make-gf-unit))
           (setf (svref build 3) :user)
           (dolist (c (svref build 0)) (setf (cdr c) (symbol-value (car c))))
           (setq ok t))
      (unless ok (setq *static-build* nil)))
    build))

(defun static-read (stream)
  "The next form of STREAM for the program, read by the image's reader (the
readtable and the reader macros of the systems loaded), or STREAM at its end."
  (let ((*package* *read-package*)
        ;; (#+clysm-static: code for a program built so)
        (*features* (cons :clysm-static *features*)))
    (read stream nil stream)))

(defun static-add-file (build path &key (external-format :default) program loaded)
  "Compile the source file PATH into the program BUILD, as COMPILE-FILE
compiles a file: what its forms do at compile time happens in the image.
PROGRAM: the file is not loaded into the image after (a file of the program
itself, not of a system): its functions are defined there once it is
compiled, for the macros of the files after it (*HOST-LOAD-DEFUNS*).
LOADED: the file is loaded in the image already and is not to be evaluated
again (ASDF): what it does at compile time is not done."
  (let ((*package* *package*) (*readtable* *readtable*)
        (*compile-file-pathname* (pathname path))
        (*compile-file-truename* (truename path)))
    (call-in-static-state
     build
     (lambda ()
       (with-open-file (s path :external-format external-format)
         (let ((env (make-cenv)) (*read-package* *package*) (defuns nil)
               (*static-image* (if loaded :loaded *static-image*))
               (clysm-sys::*proclaimed-safety* clysm-sys::*proclaimed-safety*))
           (setf (svref build 3) *static-image*)
           (unwind-protect
                (compile-source-forms s path env
                                      (lambda (form)
                                        (when (and program (host-loadable-defun-p form))
                                          (push (cons form *read-package*) defuns)))
                                      #'static-read)
             (setf (svref build 3) :user)
             (dolist (d (reverse defuns))
               (let ((*read-package* (cdr d)))
                 (ignore-errors (host-eval (car d))))))))))
    path))

(defun static-add-form (build form)
  "Compile FORM into the program BUILD as a toplevel form read in the current package."
  (call-in-static-state
   build
   (lambda ()
     (let ((*read-package* *package*) (*current-source* nil))
       (process-toplevel form (make-cenv)))))
  nil)

(defun static-finish (build output)
  "Write the module of the program BUILD to the file OUTPUT; its size in octets."
  (unwind-protect
       (call-in-static-state
        build
        (lambda ()
          (gf-unit-flush (make-cenv))
          ;; (the functions named by *LIVE-DUMP* are printed, as when code
          ;; compiled into the image defines them)
          (let ((bytes (let ((*keep-instructions* (or *keep-instructions* (and *live-dump* t)))
                             (*built-module-hook* (and *live-dump* #'dump-live-functions)))
                         (build-module))))
            (with-open-file (o output :direction :output :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
              (write-sequence bytes o))
            (length bytes))))
    (setq *static-build* nil *static-file-hook* nil)))

;;; ------------------------------------------------------------
;;; The systems of a program (ASDF is not there when the image is made:
;;; its functions are found by name)
;;; ------------------------------------------------------------

(defun asdf-call (name &rest args)
  (apply (or (find-symbol name "ASDF") (error "ASDF has no ~A" name)) args))

(defun static-system-closure (names)
  "The names of the systems NAMES and of those they depend on to be loaded
(not those their definitions need: DEFSYSTEM-DEPENDS-ON)."
  (let ((seen nil))
    (labels ((visit (system)
               (let ((name (asdf-call "COMPONENT-NAME" system)))
                 (unless (member name seen :test #'string=)
                   (push name seen)
                   (dolist (d (asdf-call "SYSTEM-DEPENDS-ON" system))
                     (let ((dep (funcall (find-symbol "RESOLVE-DEPENDENCY-SPEC" "ASDF/FIND-COMPONENT")
                                         system d)))
                       (when dep (visit dep))))))))
      (dolist (n names) (visit (asdf-call "FIND-SYSTEM" n))))
    seen))

(defun static-registration-form (names)
  "A form for a program that has ASDF in it: the systems NAMES, which are
part of the program, are loaded as far as its ASDF is concerned (with their
versions and definition files, which programs ask for)."
  ;; (and the systems that their definition files are named after: a
  ;; program asks for the directory of "lem" having loaded "lem/core")
  (dolist (name (reverse names))
    (let ((primary (asdf-call "PRIMARY-SYSTEM-NAME" name)))
      (unless (member primary names :test #'string=) (push primary names))))
  `(progn
     ,@(loop for name in names
             for system = (asdf-call "FIND-SYSTEM" name nil)
             when system
               collect `(,(find-symbol "REGISTER-IMMUTABLE-SYSTEM" "ASDF")
                         ,name
                         :version ,(ignore-errors (asdf-call "COMPONENT-VERSION" system))
                         :source-file ,(let ((f (ignore-errors (asdf-call "SYSTEM-SOURCE-FILE" system))))
                                         (and f (namestring f)))))))

(defun static-build (&key systems files forms asds loads asdf output
                          (root (namestring (clysm-sys::%clysm-root))) (verbose t))
  "Build the program of the ASDF systems SYSTEMS (names; with the systems
they depend on), the source FILES and the toplevel FORMS (strings, read in
COMMON-LISP-USER after the files are compiled), in that order, with the
runtime, into the module OUTPUT.  The systems are loaded into this image.
LOADS: files loaded into this image first (where the systems are found);
ASDS: system definition files, loaded with those of their directories.
ASDF: ASDF and UIOP are part of the program (also when a system depends on
one of them), which knows its systems as loaded."
  (flet ((note (control &rest args)
           (when verbose (format *error-output* "~&; ~?~%" control args) (finish-output *error-output*))))
    (when systems
      (require "ASDF")
      (let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
        (when (probe-file ql) (ignore-errors (require "QUICKLISP"))))
      ;; each file is compiled into the program where a fasl of its own
      ;; would be compiled: not with its whole system loaded (a system fasl)
      (set (find-symbol "*CLYSM-SYSTEM-FASLS*" "ASDF") nil))
    (when asds (require "ASDF"))
    (dolist (f loads) (load f))
    (dolist (asd asds)
      (let ((file (truename asd)))
        (dolist (other (directory (make-pathname :name :wild :type "asd" :defaults file)))
          (unless (equal (pathname-name other) (pathname-name file)) (asdf-call "LOAD-ASD" other)))
        (asdf-call "LOAD-ASD" file)))
    (let* ((included (and systems (static-system-closure systems)))
           (count 0) (seen nil)
           (asdf (or asdf (and (intersection '("asdf" "uiop") included :test #'string=) t)))
           (build (progn (note "compiling the runtime") (static-open root))))
      (unwind-protect
           (progn
             (when asdf
               (require "ASDF")
               (note "ASDF")
               ;; (what the REPL image does when it starts, SYS::MAIN:
               ;; ASDF asks for the operating system and the directory)
               (static-add-form build '(progn (setq clysm-sys::*posix-argv* (clysm-sys::%command-line-arguments))
                                        (clysm-sys::%init-default-pathname)
                                        (clysm-sys::%init-host-features)))
               (static-add-file build (concatenate 'string root "third_party/asdf/asdf.lisp") :loaded t)
               (static-add-form build '(in-package "COMMON-LISP-USER"))
               ;; (before the systems: their files may ask ASDF for them when they are loaded)
               (let ((*package* (find-package "COMMON-LISP-USER")))
                 (static-add-form build (static-registration-form (reverse included)))))
             (setq *static-file-hook*
                   (lambda (component)
                     (let ((system (asdf-call "COMPONENT-NAME" (asdf-call "COMPONENT-SYSTEM" component))))
                       (when (member system included :test #'string=)
                         (unless (member system seen :test #'string=)
                           (push system seen)
                           (note "system ~A" system))
                         (incf count)
                         (asdf-call "CALL-WITH-AROUND-COMPILE-HOOK"
                                    component
                                    (lambda (&rest flags)
                                      (declare (ignore flags))
                                      (static-add-file
                                       build (namestring (asdf-call "COMPONENT-PATHNAME" component))
                                       :external-format (asdf-call "COMPONENT-EXTERNAL-FORMAT" component))))))))
             (dolist (s systems) (asdf-call "LOAD-SYSTEM" s))
             (dolist (s included)
               (unless (or (member s seen :test #'string=)
                           (member s '("asdf" "uiop") :test #'string=)
                           (null (asdf-call "COMPONENT-CHILDREN" (asdf-call "FIND-SYSTEM" s))))
                 (format *error-output* "~&; warning: the system ~A was loaded before the build: it is not in the program~%" s)))
             (setq *static-file-hook* nil)
             (let ((*package* (find-package "COMMON-LISP-USER")))
               (dolist (f files)
                 (note "file ~A" f)
                 (static-add-file build f :program t))
               (dolist (f forms)
                 (static-add-form build (let ((*package* (find-package "COMMON-LISP-USER")))
                                          (read-from-string f)))))
             (note "~D file~:P of ~D system~:P; writing ~A" count (length seen) output)
             (let ((n (static-finish build output)))
               (note "wrote ~A (~D bytes)" output n)
               n))
        (setq *static-build* nil *static-file-hook* nil)))))

(defun static-build-main (args)
  "The command `clysm build`: [--system NAME]... [--eval FORM]... [-o OUT.wasm] FILE..."
  (let ((systems nil) (files nil) (forms nil) (asds nil) (loads nil) (asdf nil) (output "a.wasm"))
    (loop while args
          do (let ((a (pop args)))
               (cond ((string= a "--system") (push (pop args) systems))
                     ((string= a "--asd") (push (pop args) asds))
                     ((string= a "--load") (push (pop args) loads))
                     ((string= a "--asdf") (setq asdf t))
                     ((string= a "--eval") (push (pop args) forms))
                     ((string= a "-o") (setq output (pop args)))
                     (t (push a files)))))
    (handler-bind ((error (lambda (e)
                            (format *error-output* "~&BUILD ERROR: ~A~%" e)
                            (clysm-sys::%backtrace 12)
                            (clysm-sys::%exit 1))))
      (unless (or systems files forms)
        (format *error-output* "usage: clysm build [--system NAME]... [--asd FILE.asd]... [--load FILE]... [--asdf] [--eval FORM]... [-o OUT.wasm] [FILE...]~%")
        (clysm-sys::%exit 2))
      (static-build :systems (nreverse systems) :files (nreverse files) :forms (nreverse forms)
                    :asds (nreverse asds) :loads (nreverse loads) :asdf asdf :output output))
    (clysm-sys::%exit 0)))
