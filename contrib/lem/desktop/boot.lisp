;;;; boot.lisp — what the page of Lem runs: Lem and its frontend are loaded
;;;; with ASDF from the fasls that build.sh compiled (config.lisp says
;;;; where their systems are), then Lem starts.

(require :asdf)
(load (merge-pathnames "config.lisp" *load-truename*))
(let ((*compile-verbose* nil) (*load-verbose* nil))
  (asdf:load-system "lem-clysm"))
;; what a world loads (M-x world-start)
(setf clysm-world:*boot-file* (namestring (merge-pathnames "world-boot.lisp" *load-truename*)))
;; files are found from the home directory
(let ((home (user-homedir-pathname)))
  (uiop:chdir home)
  (setf *default-pathname-defaults* home))
(lem-clysm:main)
