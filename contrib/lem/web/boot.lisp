;;;; boot.lisp — what the page of Lem runs in a browser: Lem and its
;;;; frontend are loaded with ASDF from the fasls in the pack of files
;;;; (host/web/vfs.mjs), then Lem starts with welcome.lisp, copied into the
;;;; home directory the first time.  build-web.sh runs it on Node.js
;;;; with CL-USER::*LEM-LOAD-ONLY* true to see which files loading reads.

(defvar cl-user::*lem-load-only* nil)
(defvar cl-user::*welcome* (with-open-file (in (merge-pathnames "welcome.lisp" *load-truename*))
                             (let ((text (make-string (file-length in))))
                               (subseq text 0 (read-sequence text in)))))
(require :asdf)
(load (merge-pathnames "config.lisp" *load-truename*))
(let ((*compile-verbose* nil) (*load-verbose* nil))
  (asdf:load-system "lem-clysm"))
(unless cl-user::*lem-load-only*
  (let* ((home (user-homedir-pathname))
         (welcome (merge-pathnames "welcome.lisp" home)))
    (uiop:chdir home)
    (setf *default-pathname-defaults* home)
    ;; a file to start with, in the home directory (where it is kept)
    (unless (probe-file welcome)
      (with-open-file (out welcome :direction :output)
        (write-string cl-user::*welcome* out)))
    (lem-clysm:main (namestring welcome))))
