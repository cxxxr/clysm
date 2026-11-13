;;;; tests/mcclim/hello.lisp — a small McCLIM application on the CLX backend.
;;;;   DISPLAY=:N bin/clysm script tests/mcclim/hello.lisp
;;;; Needs Quicklisp ($QUICKLISP_HOME or ~/quicklisp/) and an X server.  The
;;;; canvas shows a rectangle, a circle, a line and a text; type "Quit" and
;;;; Return in the interactor to exit.  "ready" is printed once the canvas
;;;; has been drawn (tests/mcclim/run.sh waits for it).
(require :quicklisp)
(funcall (find-symbol "QUICKLOAD" "QL") :mcclim :silent t)

(defpackage :clysm-hello-clim (:use :clim :clim-lisp))
(in-package :clysm-hello-clim)

(define-application-frame hello ()
  ()
  (:panes
   (canvas :application :display-function 'draw-canvas :width 500 :height 300
                        :scroll-bars nil)
   (interactor :interactor :height 100 :scroll-bars nil))
  (:layouts (default (vertically () canvas interactor))))

(defun draw-canvas (frame pane)
  (declare (ignore frame))
  (draw-rectangle* pane 20 20 200 120 :ink +light-blue+)
  (draw-circle* pane 300 80 50 :ink +red+)
  (draw-line* pane 20 150 480 150 :ink +dark-green+ :line-thickness 3)
  (draw-text* pane "Hello from McCLIM on Clysm (WebAssembly)!" 30 200 :ink +black+)
  (finish-output pane)
  (format *trace-output* "~&ready~%")
  (finish-output *trace-output*))

(define-hello-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(run-frame-top-level (make-application-frame 'hello :left 0 :top 0))
(format t "~&frame exited~%")
