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

;;; Heap images (runtime/image.lisp): UIOP's hooks of images that are
;;; restored (the user's cache, the temporary directory, the command line,
;;; the standard streams).  Its hook of images that are dumped, which clears
;;; the configuration of ASDF, is not called: an image started from a heap
;;; image finds the systems where they were when it was saved.
(pushnew 'uiop:call-image-restore-hook sys::*restore-image-hooks*)

(sys::%load-compiled-bundled "contrib/fixups.lisp" "asdf-fixups")
