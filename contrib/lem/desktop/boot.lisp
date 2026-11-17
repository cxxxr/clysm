;;;; boot.lisp — what the page of Lem runs: Lem and its frontend are loaded
;;;; from the fasls that build.sh compiled (load-lem.lisp), while the page
;;;; shows how far it is, then Lem starts.

(load (merge-pathnames "load-lem.lisp" *load-truename*))
;; what a world loads (M-x world-start; src/world.lisp, loaded with the Lisp mode)
(defvar cl-user::*world-boot-file* (namestring (merge-pathnames "world-boot.lisp" *load-truename*)))
(cl-user::start-in-page
 (lambda ()
   (cl-user::load-lem #'cl-user::show-progress-in-page)
   ;; files are found from the home directory
   (let ((home (user-homedir-pathname)))
     (uiop:chdir home)
     (setf *default-pathname-defaults* home))
   (uiop:symbol-call :lem-clysm '#:main)))
