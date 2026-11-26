;;;; tools/system-files.lisp — the source files of an ASDF system and of the
;;;; systems it depends on, in the order they load, one per line: for
;;;; compiling a system as a whole program (bin/clysm compile --system NAME).
;;;;   bin/clysm script tools/system-files.lisp NAME [FILE.asd]
;;;;   sbcl --script tools/system-files.lisp NAME [FILE.asd]
;;;; (in Clysm's REPL image, with its own ASDF, or on SBCL).
;;;; The system is found by ASDF (its source registry, Quicklisp's
;;;; directories when ~/quicklisp/setup.lisp exists), or in FILE.asd.
;;;; Systems that Clysm has itself (in contrib/, e.g. bordeaux-threads) are
;;;; found there first.
(require :asdf)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (handler-case (load ql) (error () nil))))
(let* ((args (let ((raw (uiop:raw-command-line-arguments)))
                ;; ("clysm" SCRIPT ARG...) in Clysm's image, ("sbcl" ARG...) on SBCL
                #+clysm (cddr raw) #-clysm (cdr raw)))
       (name (first args))
       (asd (second args))
       (contrib (merge-pathnames "../contrib/" *load-truename*)))
  (unless name
    (format *error-output* "usage: bin/clysm script tools/system-files.lisp NAME [FILE.asd]~%")
    (uiop:quit 2))
  ;; Clysm's own versions of portability systems come before the registry's
  (push (lambda (system-name)
          (let ((f (merge-pathnames (format nil "~A/~A.asd" system-name system-name) contrib)))
            (when (probe-file f) f)))
        asdf:*system-definition-search-functions*)
  ;; the systems are read as for Clysm (as they are in its image)
  #-clysm (progn (pushnew :clysm *features*)
                 (setf *features* (remove :sbcl *features*)))
  ;; FILE.asd, and the other systems of its directory
  (when asd
    (let ((file (merge-pathnames asd)))
      (dolist (other (directory (merge-pathnames "*.asd" file)))
        (unless (equal (pathname-name other) (pathname-name file)) (asdf:load-asd other)))
      (asdf:load-asd file)))
  (handler-case
      (let ((seen (make-hash-table :test 'equal)) (files nil))
        (labels ((visit (system)
                   (unless (gethash (asdf:component-name system) seen)
                     (setf (gethash (asdf:component-name system) seen) t)
                     (dolist (d (asdf:system-depends-on system))
                       (let ((dep (asdf/find-component:resolve-dependency-spec system d)))
                         (when dep (visit dep))))
                     (dolist (c (asdf:required-components system :other-systems nil
                                                                 :goal-operation 'asdf:load-op
                                                                 :component-type 'asdf:cl-source-file))
                       (push (namestring (asdf:component-pathname c)) files)))))
          (visit (asdf:find-system name)))
        (dolist (f (nreverse files)) (format t "~A~%" f)))
    (error (e)
      (format *error-output* "system-files: ~A~%" e)
      (uiop:quit 1))))
