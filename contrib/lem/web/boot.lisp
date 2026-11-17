;;;; boot.lisp — what the page of Lem runs in a browser: Lem and its
;;;; frontend are loaded with ASDF from the fasls in the pack of files
;;;; (host/web/vfs.mjs), then Lem starts with welcome.lisp, copied into the
;;;; home directory the first time.
;;;;
;;;; The loading is a computation that lets the page run between files, so
;;;; that the page shows how far it is (lemBoot.progress of index.html).
;;;;
;;;; build-web.sh runs it on Node.js with CL-USER::*LEM-LOAD-ONLY* true to
;;;; see which files loading reads: then it loads the extensions as well
;;;; (src/extensions.lisp loads them in the page when they are first used),
;;;; and writes the number of files that the page loads into load-count.

(defvar cl-user::*lem-load-only* nil)
(defvar cl-user::*welcome* (with-open-file (in (merge-pathnames "welcome.lisp" *load-truename*))
                             (let ((text (make-string (file-length in))))
                               (subseq text 0 (read-sequence text in)))))
(defvar cl-user::*boot-directory* (make-pathname :name nil :type nil :defaults *load-truename*))
(require :asdf)
(load (merge-pathnames "config.lisp" *load-truename*))

(defvar cl-user::*files-loaded* 0)

(defvar cl-user::*show-progress*
  (and (not cl-user::*lem-load-only*)
       (js:eval "(n) => { const boot = globalThis.lemBoot;
                          if (boot) boot.progress(n);
                          /* the page is drawn before the next file */
                          return new Promise((resolve) => requestAnimationFrame(() => setTimeout(resolve, 0))); }")))

(defvar cl-user::*last-shown* 0)

(defmethod asdf:perform :after ((o asdf:load-op) (c asdf:cl-source-file))
  (incf cl-user::*files-loaded*)
  ;; the page is shown again at most every 50 ms: waiting for a frame
  ;; costs a few milliseconds each time
  (when (and cl-user::*show-progress*
             (js:can-await-p)
             (> (- (get-internal-real-time) cl-user::*last-shown*)
                (floor internal-time-units-per-second 20)))
    (js:await (js:funcall cl-user::*show-progress* cl-user::*files-loaded*))
    (setf cl-user::*last-shown* (get-internal-real-time))))

(defun cl-user::load-lem ()
  (let ((*compile-verbose* nil) (*load-verbose* nil))
    (asdf:load-system "lem-clysm")))

(defun cl-user::file-text (path)
  (and (probe-file path) (uiop:read-file-string path)))

(defun cl-user::welcome-unchanged-p (welcome shipped)
  "True if the file WELCOME is as a page wrote it: its text is that of the
file SHIPPED (written with it), or, from before SHIPPED was written, that
of the first version of welcome.lisp."
  (let ((text (cl-user::file-text welcome)))
    (or (null text)
        (equal text (cl-user::file-text shipped))
        (and (not (probe-file shipped))
             (uiop:string-prefix-p ";;;; Welcome to Lem, the Common Lisp editor, running in your browser." text)
             (uiop:string-suffix-p text (format nil ";;; M-x lisp-repl opens a REPL of the same image.~%"))))))

(defun cl-user::start-lem ()
  (let* ((home (user-homedir-pathname))
         (welcome (merge-pathnames "welcome.lisp" home))
         (shipped (merge-pathnames ".welcome.lisp" home)))
    (uiop:chdir home)
    (setf *default-pathname-defaults* home)
    ;; a file to start with, in the home directory (where it is kept); a
    ;; newer one replaces it if it was not changed
    (when (and (cl-user::welcome-unchanged-p welcome shipped)
               (not (equal (cl-user::file-text welcome) cl-user::*welcome*)))
      (dolist (path (list welcome shipped))
        (with-open-file (out path :direction :output :if-exists :supersede)
          (write-string cl-user::*welcome* out))))
    (uiop:symbol-call :lem-clysm '#:main (namestring welcome))))

(cond (cl-user::*lem-load-only*
       (cl-user::load-lem)
       (with-open-file (out (merge-pathnames "load-count" cl-user::*boot-directory*)
                            :direction :output :if-exists :supersede)
         (format out "~D~%" cl-user::*files-loaded*))
       (let ((*compile-verbose* nil) (*load-verbose* nil))
         (dolist (system (uiop:symbol-call :lem-clysm '#:extension-systems))
           (asdf:load-system system))))
      (t
       ;; from the event loop, as a computation that can wait
       (js:funcall (js:global "setTimeout")
                   (js:async-function
                    (lambda ()
                      (handler-bind ((error (lambda (e)
                                              (let ((boot (js:global "lemBoot")))
                                                (when (js:js-value-p boot)
                                                  (js:call boot "failed" (princ-to-string e)))))))
                        (cl-user::load-lem)
                        (cl-user::start-lem))
                      nil))
                   0)))
