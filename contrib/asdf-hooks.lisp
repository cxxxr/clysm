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
;;; (SYS::%COMPILE-FILES), so that the calls between its files are direct
;;; calls (guarded by a test of the function cell: a redefinition is seen)
;;; that V8 can inline and that carry the types of arguments and values.
;;; After a system is loaded from its per-file fasls, its system fasl is made
;;; (in this image, where all its definitions are made already: compiling
;;; them again is like recompiling a loaded file) when it is missing or older
;;; than a fasl, a source of the system or its .asd; when the system is next
;;; loaded and every file's fasl is up to date, the load of its first file
;;; loads the system fasl, which runs the files in order, and the loads of
;;; the others do nothing.  Only systems whose components are plain Lisp
;;; source files (in modules, with static files) and that have no
;;; around-compile hooks are made so.  Opt-in: (setf asdf::*clysm-system-fasls* t)
;;; (the build re-runs the system's compile-time side effects in the image,
;;; compiles it twice and keeps it twice in the cache).

(defvar *clysm-system-fasls* nil "Make and load system fasls (see above); off by default.")
(defvar *clysm-system-fasls-loaded* nil "The names of the systems loaded from their system fasls.")
(defvar *clysm-system-fasl-state* (make-hash-table :test 'eq)
  "system -> :LOADED when its system fasl was loaded in this image, :FILES when its files are loaded one by one")

(defun clysm-system-fasl-files (system)
  "The source files of SYSTEM in the order a load of it loads them, when it
qualifies for a system fasl; else NIL."
  (labels ((ok (c)
             (typecase c
               (static-file t)
               (cl-source-file (and (member (type-of c) '(cl-source-file cl-source-file.cl cl-source-file.lsp))
                                    (null (around-compile-hook c))))
               (module (and (member (type-of c) '(module system))
                            (null (around-compile-hook c))
                            (every #'ok (component-children c))))
               (t nil))))
    (when (and *clysm-system-fasls* (typep system 'system) (eq (type-of system) 'system)
               (not (member (component-name system) '("asdf" "uiop" "quicklisp") :test #'string=))
               (ignore-errors (every #'ok (component-children system))))
      (let ((files (ignore-errors
                    (remove-if-not (lambda (c) (typep c 'cl-source-file))
                                   (required-components system :other-systems nil
                                                               :goal-operation 'load-op
                                                               :keep-operation 'load-op
                                                               :keep-component 'cl-source-file)))))
        (and (cdr files) (every (lambda (c) (eq (component-system c) system)) files) files)))))

(defun clysm-system-fasl-pathname (system files)
  "Where the system fasl of SYSTEM (of the files FILES) goes: next to the fasl of the first."
  (let ((fasl (first (output-files (make-operation 'compile-op) (first files)))))
    (make-pathname :name (concatenate 'string (substitute #\_ #\/ (component-name system)) "--system")
                   :type "fasl" :defaults fasl)))

(defun clysm-system-fasl-valid-p (system files sysfasl)
  "The system fasl SYSFASL was made for FILES, in this order, and is newer
than their fasls and sources and than the .asd of SYSTEM."
  (let ((date (and (probe-file sysfasl) (file-write-date sysfasl)))
        (list-file (make-pathname :type "files" :defaults sysfasl)))
    (and date (probe-file list-file)
         (equal (uiop:read-file-forms list-file)
                (list (mapcar (lambda (c) (namestring (component-pathname c))) files)))
         (let ((asd (system-source-file system)))
           (or (null asd) (not (probe-file asd)) (<= (file-write-date asd) date)))
         (every (lambda (c)
                  (let ((src (component-pathname c))
                        (fasl (first (output-files (make-operation 'compile-op) c))))
                    (and (probe-file fasl) (probe-file src)
                         (<= (file-write-date src) (file-write-date fasl) date))))
                files))))

(defun clysm-make-system-fasl (system files sysfasl)
  (let ((list-file (make-pathname :type "files" :defaults sysfasl)))
    (handler-case
        (progn
          (when (probe-file list-file) (delete-file list-file))
          (let ((*package* (find-package :cl-user)))
            (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
              (handler-bind ((warning #'muffle-warning))
                (sys::%compile-files (mapcar #'component-pathname files) sysfasl))))
          ;; (the list last: a system fasl without it is not used)
          (with-open-file (o list-file :direction :output :if-exists :supersede)
            (with-standard-io-syntax
              (prin1 (mapcar (lambda (c) (namestring (component-pathname c))) files) o))))
      (error ()
        (ignore-errors (when (probe-file sysfasl) (delete-file sysfasl)))
        nil))))

(defmethod perform :around ((o load-op) (c cl-source-file))
  (let* ((system (component-system c))
         (state (gethash system *clysm-system-fasl-state*)))
    ;; (a file whose load is the system fasl's is marked done as a load would)
    (cond ((eq state :loaded) (mark-operation-done o c))
          ((eq state :files) (call-next-method))
          (t
           (let ((files (clysm-system-fasl-files system)))
             (if (and files (eq c (first files)))
                 (let ((sysfasl (clysm-system-fasl-pathname system files)))
                   (if (clysm-system-fasl-valid-p system files sysfasl)
                       (progn (setf (gethash system *clysm-system-fasl-state*) :loaded)
                              (load sysfasl)
                              (pushnew (component-name system) *clysm-system-fasls-loaded* :test #'string=)
                              (mark-operation-done o c))
                       (progn (setf (gethash system *clysm-system-fasl-state*) :files)
                              (call-next-method))))
                 (call-next-method)))))))

(defmethod perform :after ((o load-op) (s system))
  (let ((state (gethash s *clysm-system-fasl-state*)))
    (remhash s *clysm-system-fasl-state*)
    (unless (eq state :loaded)
      (let ((files (clysm-system-fasl-files s)))
        (when files
          (let ((sysfasl (clysm-system-fasl-pathname s files)))
            (unless (clysm-system-fasl-valid-p s files sysfasl)
              (clysm-make-system-fasl s files sysfasl))))))))

;;; Heap images (runtime/image.lisp): UIOP's hooks of images that are
;;; restored (the user's cache, the temporary directory, the command line,
;;; the standard streams).  Its hook of images that are dumped, which clears
;;; the configuration of ASDF, is not called: an image started from a heap
;;; image finds the systems where they were when it was saved.
(pushnew 'uiop:call-image-restore-hook sys::*restore-image-hooks*)

(sys::%load-compiled-bundled "contrib/fixups.lisp" "asdf-fixups")
