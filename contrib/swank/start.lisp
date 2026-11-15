;;;; contrib/swank/start.lisp — `clysm swank`: a SWANK server for SLIME
;;;;
;;;; The port is CLYSM_SWANK_PORT (4005 by default).  SWANK is loaded with
;;;; Quicklisp; the server serves connections until the process is ended.

(require :quicklisp)
(ql:quickload :swank :silent t)
(swank:create-server :port (let ((p (sys::%getenv "CLYSM_SWANK_PORT")))
                             (if p (parse-integer p) 4005))
                     :dont-close t)
