;;;; prepare.lisp — compile Lem and its dependencies with the image of the
;;;; application (run by build.sh on Node.js):
;;;;   node host/run.mjs IMAGE prepare.lisp LEM-DIR MAILBOX-DIR OUT-DIR
;;;; Quicklisp downloads the libraries; the fasls go to the cache of ASDF,
;;;; where the application finds them.  OUT-DIR/config.lisp gets the
;;;; directories of all the systems, so that the application loads them with
;;;; ASDF alone.

(require :asdf)
(require :quicklisp)

(destructuring-bind (lem-dir mailbox-dir out-dir) (cddr (uiop:raw-command-line-arguments))
  (push (uiop:ensure-directory-pathname lem-dir) asdf:*central-registry*)
  ;; Lem's extensions (each in a directory of its own)
  (dolist (directory (uiop:subdirectories (merge-pathnames "extensions/"
                                                           (uiop:ensure-directory-pathname lem-dir))))
    (push directory asdf:*central-registry*))
  (push (uiop:ensure-directory-pathname mailbox-dir) asdf:*central-registry*)
  (load (merge-pathnames "setup.lisp" *load-truename*))
  (ql:quickload "lem-clysm")
  (let ((directories
          (remove-duplicates
           (loop for name in (asdf:already-loaded-systems)
                 for directory = (asdf:system-source-directory name)
                 when directory collect (namestring directory))
           :test #'string=)))
    (with-open-file (out (merge-pathnames "config.lisp" (uiop:ensure-directory-pathname out-dir))
                         :direction :output :if-exists :supersede)
      (format out ";;; where the systems of Lem are (written by contrib/lem/prepare.lisp)~%")
      (let ((*print-pretty* t) (*package* (find-package :cl-user)))
        (print `(setf asdf:*central-registry*
                      (append (mapcar #'pathname ',directories) asdf:*central-registry*))
               out)
        (print `(load ,(namestring (merge-pathnames "setup.lisp" *load-truename*))) out))
      (terpri out)))
  (format t "~&lem-clysm: compiled~%"))
