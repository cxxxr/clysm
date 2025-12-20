;;;; reader.lisp - S-expression reader for clysm

(in-package #:clysm/reader)

;;; Reader Interface
;;; We use the host CL's reader for now, with potential customization later.
;;; For self-hosting, we need custom reader macros for quasiquote.

;;; Quasiquote Reader Macros (for self-hosting mode)
;;;
;;; These produce portable forms that don't depend on SBCL internals:
;;;   ` => (quasiquote ...)
;;;   , => (unquote ...)
;;;   ,@ => (unquote-splicing ...)

(defun read-quasiquote (stream char)
  "Read ` (backquote) syntax. Produces (quasiquote form)."
  (declare (ignore char))
  (list 'quasiquote (read stream t nil t)))

(defun read-unquote (stream char)
  "Read , (unquote) or ,@ (unquote-splicing) syntax."
  (declare (ignore char))
  (let ((next (peek-char nil stream t nil t)))
    (if (char= next #\@)
        (progn
          (read-char stream t nil t)  ; consume @
          (list 'unquote-splicing (read stream t nil t)))
        (list 'unquote (read stream t nil t)))))

(defun make-quasiquote-readtable ()
  "Create a readtable with quasiquote reader macros installed.
   Returns a new readtable suitable for self-hosting mode."
  (let ((rt (copy-readtable nil)))
    (set-macro-character #\` #'read-quasiquote nil rt)
    (set-macro-character #\, #'read-unquote nil rt)
    rt))

(defparameter *quasiquote-readtable* nil
  "Readtable with portable quasiquote reader macros.
   Lazily initialized by install-quasiquote-reader.")

(defun install-quasiquote-reader ()
  "Install quasiquote reader macros into *readtable*.
   This replaces SBCL's default ` and , handling with portable forms."
  (unless *quasiquote-readtable*
    (setf *quasiquote-readtable* (make-quasiquote-readtable)))
  (setf *readtable* *quasiquote-readtable*))

(defun uninstall-quasiquote-reader ()
  "Restore the standard readtable."
  (setf *readtable* (copy-readtable nil)))

;;; Source Reading Functions

(defun read-source (source &key (use-quasiquote-reader nil))
  "Read Lisp source from a string. Returns a list of top-level forms.
   If USE-QUASIQUOTE-READER is true, use our portable quasiquote reader."
  (let ((*readtable* (if use-quasiquote-reader
                         (or *quasiquote-readtable*
                             (make-quasiquote-readtable))
                         *readtable*)))
    (with-input-from-string (stream source)
      (let ((forms nil))
        (do ((form (read stream nil :eof) (read stream nil :eof)))
            ((eq form :eof) (nreverse forms))
          (push form forms))))))

(defun read-file (pathname &key (use-quasiquote-reader nil))
  "Read Lisp source from a file. Returns a list of top-level forms.
   If USE-QUASIQUOTE-READER is true, use our portable quasiquote reader."
  (let ((*readtable* (if use-quasiquote-reader
                         (or *quasiquote-readtable*
                             (make-quasiquote-readtable))
                         *readtable*)))
    (with-open-file (stream pathname :direction :input)
      (let ((forms nil))
        (do ((form (read stream nil :eof) (read stream nil :eof)))
            ((eq form :eof) (nreverse forms))
          (push form forms))))))
