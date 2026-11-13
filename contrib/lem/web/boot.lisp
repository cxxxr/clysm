;;;; boot.lisp — what the page of Lem runs in a browser: Lem and its
;;;; frontend are loaded with ASDF from the fasls in the pack of files
;;;; (host/web/vfs.mjs), then Lem starts.  build-web.sh runs it on Node.js
;;;; with CL-USER::*LEM-LOAD-ONLY* true to see which files loading reads.

(defvar cl-user::*lem-load-only* nil)
(require :asdf)
(load (merge-pathnames "config.lisp" *load-truename*))
(let ((*compile-verbose* nil) (*load-verbose* nil))
  (asdf:load-system "lem-clysm"))
(unless cl-user::*lem-load-only*
  (let ((home (user-homedir-pathname)))
    (uiop:chdir home)
    (setf *default-pathname-defaults* home))
  (lem-clysm:main))
