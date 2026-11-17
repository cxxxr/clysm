;;;; boot.lisp — what a world loads (worker.js runs the image with this
;;;; file and the world's name as its arguments): micros and the sockets
;;;; to the page, with ASDF from the fasls that the application's build
;;;; compiled (config.lisp next to this file says where the systems are).
;;;; Then the world serves (world.lisp) and the page connects to it.

(require :asdf)
(load (merge-pathnames "config.lisp" *load-truename*))

(defun cl-user::start-world ()
  (let ((*compile-verbose* nil) (*load-verbose* nil))
    (asdf:load-system "clysm-world"))
  ;; files are found from the home directory
  (let ((home (user-homedir-pathname)))
    (uiop:chdir home)
    (setf *default-pathname-defaults* home))
  (uiop:symbol-call :clysm-world '#:serve))

;; from the event loop, as a computation that can wait (the threads of
;; micros are computations)
(js:funcall (js:global "setTimeout")
            (js:async-function
             (lambda ()
               (handler-case (cl-user::start-world)
                 (error (e)
                   (js:call (js:global "self") "postMessage"
                            (js:object "type" "failed" "message" (princ-to-string e)))))
               nil))
            0)
