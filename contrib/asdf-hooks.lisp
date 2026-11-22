;;;; contrib/asdf-hooks.lisp — loaded by (require :asdf) after ASDF itself.

(in-package :asdf)

;;; Systems that Clysm provides itself (compatibility layers for
;;; implementation-specific libraries) are found before others, e.g. before
;;; Quicklisp's releases.
;;; (this file is loaded from a fasl in the cache: the contrib directory is
;;; found from the installation)
(let ((contrib (merge-pathnames "contrib/" (sys::%clysm-root))))
  (dolist (system '("trivial-gray-streams" "closer-mop" "trivial-features" "trivial-garbage"
                    "static-vectors" "bordeaux-threads"))
    (pushnew (merge-pathnames (concatenate 'string system "/") contrib) *central-registry* :test #'equal)))

;;; When the Quicklisp client system is loaded (by Quicklisp's setup.lisp),
;;; add Clysm's implementation of Quicklisp's host interface, before the
;;; client is used.

(defmethod perform :after ((o load-op) (s system))
  (when (string= (component-name s) "quicklisp")
    (require "QL-CLYSM")
    (funcall (find-symbol "INSTALL" "QL-CLYSM-SUPPORT"))))

;;; Fixups of system definitions of libraries that select implementation-
;;; specific code which Clysm has no counterpart for (contrib/fixups.lisp).
;;; A fixup runs once, when the system is first defined.

(defvar *clysm-system-fixups* nil)      ; ((name . function) ...)
(defvar *clysm-fixed-systems* nil)

(defmacro define-clysm-fixup (name (system) &body body)
  `(progn
     (setf *clysm-system-fixups*
           (cons (cons ,name (lambda (,system) ,@body))
                 (remove ,name *clysm-system-fixups* :key #'car :test #'equal)))
     ',name))

(defun apply-clysm-fixups ()
  (dolist (e *clysm-system-fixups*)
    (let ((s (registered-system (car e))))
      ;; (an immutable system is loaded already: it has no definition to fix)
      (when (and s (not (member s *clysm-fixed-systems*))
                 (not (and *immutable-systems* (gethash (car e) *immutable-systems*))))
        (push s *clysm-fixed-systems*)
        (funcall (cdr e) s)))))

(defmethod perform :after ((o define-op) (s system))
  (apply-clysm-fixups))

(defun clysm-drop-dependencies (component &rest names)
  "Remove the dependencies on systems NAMES (also in (:feature ...) forms)."
  (labels ((dep-name (d)
           (cond ((and (consp d) (eq (car d) :feature)) (dep-name-1 (third d)))
                 (t (dep-name-1 d))))
         (dep-name-1 (d)
           (cond ((and (consp d) (member (car d) '(:version :require))) (string-downcase (string (second d))))
                 ((or (stringp d) (symbolp d)) (string-downcase (string d)))
                 (t nil))))
    (setf (component-sideway-dependencies component)
          (remove-if (lambda (d) (member (dep-name d) names :test #'equal))
                     (component-sideway-dependencies component)))))

(defun clysm-find-component (system path)
  "The component of SYSTEM at PATH, a list of names."
  (let ((c system))
    (dolist (n path c)
      (setq c (or (find-component c n) (error "No component ~S in ~S" n c))))))

(defun clysm-enable-component (system path enabled)
  (setf (component-if-feature (clysm-find-component system path)) (if enabled nil '(:or))))

(defun clysm-patch-component (system path &rest replacements)
  "Compile the component at PATH with REPLACEMENTS, pairs of strings (OLD NEW)
each of which must occur in its source, applied in order.  The patched source
is written under ~/.cache/common-lisp/clysm-patches/ (only when it changes,
so that the fasl stays valid)."
  (let* ((c (clysm-find-component system path))
         (src (component-pathname c))
         (text (uiop:read-file-string src)))
    (loop for (old new) on replacements by #'cddr
          for pos = (search old text)
          do (unless pos
               (error "Clysm fixup of ~A: ~S not found in ~A" (component-name system) old src))
             (setq text (concatenate 'string (subseq text 0 pos) new (subseq text (+ pos (length old))))))
    (let ((out (uiop:subpathname (uiop:xdg-cache-home "common-lisp" "clysm-patches")
                                 (format nil "~A/~{~A/~}~A.lisp"
                                         (substitute #\_ #\/ (component-name system))
                                         (butlast path) (car (last path))))))
      (ensure-directories-exist out)
      (unless (and (probe-file out) (string= (uiop:read-file-string out) text))
        (with-open-file (o out :direction :output :if-exists :supersede)
          (write-string text o)))
      (setf (slot-value c 'absolute-pathname) out))))

(defun clysm-add-source-file (system path name pathname &key before)
  "Add the Lisp file PATHNAME as component NAME to the module of SYSTEM at
PATH (a list of names, NIL for the system itself), before the component
named BEFORE (at the end without it)."
  (let* ((module (clysm-find-component system path))
         (c (make-instance 'cl-source-file :name name :parent module))
         (children (remove name (component-children module) :key #'component-name :test #'equal))
         (pos (and before (position before children :key #'component-name :test #'equal))))
    (setf (slot-value c 'absolute-pathname) pathname)
    (setf (component-children module)
          (if pos
              (append (subseq children 0 pos) (list c) (nthcdr pos children))
              (append children (list c))))
    (compute-children-by-name module)
    c))

;;; System fasls: the files of a system compiled into one fasl of one module
;;; (SYS::%SYSTEM-COMPILE-FILE), so that the calls between its files are
;;; direct calls (guarded by a test of the function cell: a redefinition is
;;; seen) that V8 can inline and that carry the types of arguments and values.
;;; The fasl is the output of the compile-op of each of its files: ASDF's
;;; compiling and loading of the files in turn compiles each file into the
;;; system's unit and loads it from modules made from that same code (the
;;; compile-time side effects of each file happen once, with the files before
;;; it loaded, as with a fasl for each file), and the fasl is written when the
;;; last file is compiled.  A file is up to date when the fasl is (newer than
;;; every file and the .asd, made from these files): a change to one compiles
;;; them all again.  The load of the first file to be loaded loads the fasl,
;;; which runs the files in order, and the loads of the others do nothing.
;;; Only systems whose components are plain Lisp source files (in modules,
;;; with static files) without around-compile hooks are made so, and not in
;;; an image that records its modules for a heap image.  Opt-in:
;;; (setf asdf::*clysm-system-fasls* t).

(defvar *clysm-system-fasls*
  (let ((v (uiop:getenv "CLYSM_SYSTEM_FASLS")))
    (cond ((equal v "1") t) ((equal v "auto") :auto) (t nil)))
  "Whether systems are made into system fasls (see above): T, :AUTO (but
not those being developed: see CLYSM-SYSTEM-FASL-FILES-1) or NIL.
CLYSM_SYSTEM_FASLS=1, auto or 0 in the environment.")
(defvar *clysm-system-fasl-modes* nil
  "((name . mode)...): T, :AUTO or NIL for the system NAME, instead of *CLYSM-SYSTEM-FASLS*.")
(defvar *clysm-system-fasls-loaded* nil "The names of the systems loaded from their system fasls.")
(defvar *clysm-system-fasl-state* (make-hash-table :test 'eq)
  "system -> :LOADED when it is loaded from its system fasl in this image, or
the system's unit being compiled: (unit (component . index)... ), whose CAR
is :BROKEN when a file failed to compile into it.")
(defvar *clysm-system-fasl-files* (make-hash-table :test 'eq)
  "system -> (children files . system-fasl) of CLYSM-SYSTEM-FASL-FILES")
(defvar *clysm-per-file-outputs* nil "True: the outputs of files are their own fasls.")

(defun clysm-system-fasl-files (system)
  "The source files of SYSTEM, if it is made into a system fasl, and the
pathname of the fasl; else NIL."
  (let ((mode (and (typep system 'system) (clysm-system-fasl-mode system))))
    (when (and mode (eq (type-of system) 'system) (not (sys::%getenv "CLYSM_SAVE_IMAGE")))
      (let ((e (gethash system *clysm-system-fasl-files*)))
        (unless (and e (eq (first e) (component-children system)) (eq (second e) mode))
          (setq e (list* (component-children system) mode
                         (ignore-errors (clysm-system-fasl-files-1 system mode))))
          (setf (gethash system *clysm-system-fasl-files*) e))
        (values (caddr e) (cdddr e))))))

(defun clysm-system-fasl-mode (system)
  (let ((e (assoc (component-name system) *clysm-system-fasl-modes* :test #'string=)))
    (if e (cdr e) *clysm-system-fasls*)))

(defun clysm-dev-pathname (sysfasl) (make-pathname :type "dev" :defaults sysfasl))

(defun clysm-system-fasl-reset (system)
  "Make SYSTEM (a system designator) be made into a system fasl again: forget
that it is being developed or that its system fasl failed to be made, and
the system fasl (it is made again when the system is next compiled)."
  (let* ((system (find-system system)) (*clysm-system-fasls* t) (*clysm-system-fasl-modes* nil))
    (remhash system *clysm-system-fasl-files*)
    (let ((sysfasl (cdr (clysm-system-fasl-files-1 system :any))))
      (remhash system *clysm-system-fasl-files*)
      (when sysfasl
        (dolist (p (list (clysm-dev-pathname sysfasl) (clysm-broken-pathname sysfasl)
                         (make-pathname :type "files" :defaults sysfasl) sysfasl))
          (when (probe-file p) (delete-file p)))
        t))))

(defun clysm-system-fasl-files-1 (system mode)
  "(files . system-fasl) of CLYSM-SYSTEM-FASL-FILES, or NIL.  MODE :AUTO:
not for a system being developed, one of whose files changed since its
system fasl was made (noted in NAME--system.dev, which
CLYSM-SYSTEM-FASL-RESET deletes), whose files are compiled one by one from
then on, as a change to one would compile all again."
  (let ((files nil))
    (labels ((ok (c)
               (or (let ((f (component-if-feature c))) (and f (not (uiop:featurep f))))
                   (typecase c
                     (static-file t)
                     (cl-source-file (and (member (type-of c) '(cl-source-file cl-source-file.cl cl-source-file.lsp))
                                          (null (around-compile-hook c))
                                          (push c files)))
                     (module (and (member (type-of c) '(module system))
                                  (null (around-compile-hook c))
                                  (every #'ok (component-children c))))
                     (t nil)))))
      (when (and (not (member (component-name system) '("asdf" "uiop" "quicklisp") :test #'string=))
                 (every #'ok (component-children system))
                 (cdr files))
        (setq files (nreverse files))
        (let* ((sysfasl (let* ((*clysm-per-file-outputs* t)
                               (fasl (first (output-files (make-operation 'compile-op) (first files)))))
                          (make-pathname :name (concatenate 'string (substitute #\_ #\/ (component-name system))
                                                            "--system")
                                         :type "fasl" :defaults fasl)))
               (broken (probe-file (clysm-broken-pathname sysfasl))))
          ;; (not after it failed to be made, unless a file changed since)
          (unless (and broken (not (eq mode :any))
                       (let ((date (file-write-date broken)) (asd (system-source-file system)))
                         (and (or (null asd) (not (probe-file asd)) (<= (file-write-date asd) date))
                              (every (lambda (c) (let ((src (component-pathname c)))
                                                   (or (not (probe-file src)) (<= (file-write-date src) date))))
                                     files))))
            (unless (and (eq mode :auto) (clysm-system-developed-p files sysfasl))
              (cons files sysfasl))))))))

(defun clysm-system-developed-p (files sysfasl)
  "Whether the system of FILES is being developed: noted so, or one of its
files is newer than its system fasl (which is deleted then)."
  (let ((dev (clysm-dev-pathname sysfasl)) (list-file (make-pathname :type "files" :defaults sysfasl)))
    (or (probe-file dev)
        (let ((date (and (probe-file list-file) (probe-file sysfasl) (file-write-date sysfasl))))
          (when (and date (some (lambda (c) (let ((src (component-pathname c)))
                                              (and (probe-file src) (> (file-write-date src) date))))
                                files))
            (ignore-errors
             (with-open-file (out dev :direction :output :if-exists :supersede)
               (write-line "a file of this system changed after its system fasl was made" out))
             (delete-file list-file) (delete-file sysfasl))
            t)))))

(defvar *clysm-system-fasl-valid* (make-hash-table :test 'eq)
  "system -> (ASDF session . CLYSM-SYSTEM-FASL-VALID-P), as it was found in that session.")

(defun clysm-system-fasl-valid-p (system files sysfasl)
  "The system fasl SYSFASL was made from FILES and is newer than them and
than the .asd of SYSTEM (found once in an ASDF session, which also takes the
dates of files once)."
  (let ((e (gethash system *clysm-system-fasl-valid*))
        (session (asdf/session:toplevel-asdf-session)))
    (if (and e session (eq (car e) session))
        (cdr e)
        (let ((valid (clysm-system-fasl-valid-p-1 system files sysfasl)))
          (setf (gethash system *clysm-system-fasl-valid*) (cons session valid))
          valid))))

(defun clysm-system-fasl-valid-p-1 (system files sysfasl)
  (let ((date (and (probe-file sysfasl) (file-write-date sysfasl)))
        (list-file (make-pathname :type "files" :defaults sysfasl)))
    (and date (probe-file list-file)
         (let ((made (ignore-errors (uiop:read-file-form list-file)))
               (now (mapcar (lambda (c) (namestring (component-pathname c))) files)))
           (and (= (length made) (length now)) (subsetp now made :test #'equal)))
         (let ((asd (system-source-file system)))
           (or (null asd) (not (probe-file asd)) (<= (file-write-date asd) date)))
         (every (lambda (c)
                  (let ((src (component-pathname c)))
                    (and (probe-file src) (<= (file-write-date src) date))))
                files))))

(defmethod output-files :around ((o compile-op) (c source-file))
  (multiple-value-bind (files sysfasl)
      (and (not *clysm-per-file-outputs*) (clysm-system-fasl-files (component-system c)))
    (if (member c files)
        (values (list sysfasl) t)
        (call-next-method))))

(defmethod operation-done-p :around ((o compile-op) (c source-file))
  (multiple-value-bind (files sysfasl) (clysm-system-fasl-files (component-system c))
    (and (call-next-method)
         (or (not (member c files))
             (let ((pass (gethash (component-system c) *clysm-system-fasl-state*)))
               (and (clysm-pass-p pass) (third (assoc c (clysm-pass-files pass)))))
             (clysm-system-fasl-valid-p (component-system c) files sysfasl)))))

;;; The compiling of a system in this image: the unit of its files (NIL when
;;; they are compiled to fasls of their own: no compiler, or after a file
;;; failed to compile into it), SU the unit that loads those compiled into it,
;;; and (file index status) for the files whose
;;; compile-op was performed, STATUS :COMPILED (into the unit), :LOADED, or
;;; :OWN (into a fasl of its own).
(defstruct (clysm-pass (:constructor make-clysm-pass (unit su))) unit su (files nil))

(defmethod perform :around ((o compile-op) (c source-file))
  (let ((system (component-system c)))
    (multiple-value-bind (files sysfasl) (clysm-system-fasl-files system)
      (if (not (member c files))
          (call-next-method)
          (let* ((pass (let ((p (gethash system *clysm-system-fasl-state*)))
                         (if (clysm-pass-p p) p (clysm-system-unit-start system sysfasl))))
                 (e (assoc c (clysm-pass-files pass))))
            (if (or e (null (clysm-pass-unit pass)))
                ;; (a file compiled again after an error: the unit is left,
                ;; the others are compiled to fasls of their own)
                (progn (clysm-pass-break pass sysfasl)
                       (if e (setf (third e) :own) (push (list c nil :own) (clysm-pass-files pass)))
                       (let ((*clysm-per-file-outputs* t)) (call-next-method)))
                (let ((e (list c (length (clysm-pass-files pass)) nil)))
                  (push e (clysm-pass-files pass))
                  (multiple-value-bind (warnings-p failure-p)
                      ;; (a file that did not compile into the unit to its
                      ;; end leaves it: an error, the ACCEPT restart)
                      (unwind-protect
                           (multiple-value-prog1
                               (sys::%system-compile-file (clysm-pass-unit pass) (component-pathname c)
                                                          :verbose *compile-verbose*)
                             (setf (third e) :compiled))
                        (unless (third e) (clysm-pass-break pass sysfasl)))
                    (when (sys::%su-broken (clysm-pass-su pass)) (clysm-pass-break pass sysfasl))
                    (when (and (clysm-pass-unit pass) (= (length (clysm-pass-files pass)) (length files)))
                      (handler-case
                          (progn
                            (sys::%system-unit-finish (clysm-pass-unit pass) sysfasl)
                            (remhash system *clysm-system-fasl-valid*)
                            ;; (the list last: a system fasl without it is not used)
                            (with-open-file (out (make-pathname :type "files" :defaults sysfasl)
                                                 :direction :output :if-exists :supersede)
                              (with-standard-io-syntax
                                (prin1 (mapcar (lambda (e) (namestring (component-pathname (car e))))
                                               (reverse (clysm-pass-files pass)))
                                       out))))
                        (error () (clysm-pass-break pass sysfasl))))
                    (check-lisp-compile-results sysfasl warnings-p failure-p
                                                "~/asdf-action::format-action/" (list (cons o c)))
                    (mark-operation-done o c)))))))))

(defun clysm-broken-pathname (sysfasl) (make-pathname :type "broken" :defaults sysfasl))

(defun clysm-pass-break (pass sysfasl)
  "The files of PASS left are compiled to fasls of their own, and the system
is not made into a system fasl again until it or its .asd changes (a file
noting that, next to where the system fasl goes)."
  (setf (clysm-pass-unit pass) nil)
  (ignore-errors
   (with-open-file (out (clysm-broken-pathname sysfasl) :direction :output :if-exists :supersede)
     (write-line "a file of this system did not compile into its system fasl" out))))

(defun clysm-system-unit-start (system sysfasl)
  "Begin compiling SYSTEM (its pass)."
  (remhash system *clysm-system-fasl-valid*)
  (let ((list-file (make-pathname :type "files" :defaults sysfasl)))
    (when (probe-file list-file) (delete-file list-file))
    ;; (ASDF notes the date of the output of each compile-op: a fasl that
    ;; does nothing until the system's is written)
    (ensure-directories-exist sysfasl)
    (with-open-file (out sysfasl :direction :output :if-exists :supersede)
      (write-line ";;; Clysm compiled file" out)))
  (setf (gethash system *clysm-system-fasl-state*) (let ((su (sys::%system-unit-open))) (make-clysm-pass su su))))

(defmethod perform :around ((o load-op) (c source-file))
  (let ((system (component-system c)))
    (multiple-value-bind (files sysfasl) (clysm-system-fasl-files system)
      (let ((state (gethash system *clysm-system-fasl-state*)))
        (cond ((not (member c files)) (call-next-method))
              ((eq state :loaded) (mark-operation-done o c))
              ((clysm-pass-p state)
               (let ((e (assoc c (clysm-pass-files state))))
                 (case (third e)
                   (:loaded nil)
                   (:compiled
                    ;; (with those compiled into the unit before it, in order)
                    (let ((*load-pathname* sysfasl) (*load-truename* sysfasl))
                      (dolist (x (reverse (clysm-pass-files state)))
                        (when (eq (third x) :compiled)
                          (sys::%system-load-file (clysm-pass-su state) (second x))
                          (setf (third x) :loaded))
                        (when (eq x e) (return))))
                    (mark-operation-done o c))
                   (:own (let ((*clysm-per-file-outputs* t)) (call-next-method)))
                   (t (mark-operation-done o c)))))
              (t (setf (gethash system *clysm-system-fasl-state*) :loaded)
                 (let ((sys::*system-load-readtable* *readtable*)) (load sysfasl))
                 (pushnew (component-name system) *clysm-system-fasls-loaded* :test #'string=)
                 (mark-operation-done o c)))))))

;;; (not on SYSTEM: the method above, for Quicklisp, has those specializers)
(defmethod perform :after ((o load-op) (s parent-component))
  (let ((pass (gethash s *clysm-system-fasl-state*)))
    (when (clysm-pass-p pass)
      (setf (gethash s *clysm-system-fasl-state*) :loaded)
      ;; (the host forgets the modules that loaded its files)
      (when (clysm-pass-su pass)
        (ignore-errors (js:call (js:global) "__clysm_unit_clear" (sys::%su-prefix (clysm-pass-su pass))))))))

;;; Heap images (runtime/image.lisp): UIOP's hooks of images that are
;;; restored (the user's cache, the temporary directory, the command line,
;;; the standard streams).  Its hook of images that are dumped, which clears
;;; the configuration of ASDF, is not called: an image started from a heap
;;; image finds the systems where they were when it was saved.
(pushnew 'uiop:call-image-restore-hook sys::*restore-image-hooks*)

(sys::%load-compiled-bundled "contrib/fixups.lisp" "asdf-fixups")
