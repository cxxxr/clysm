;;;; dev.lisp — for the development build of an application
;;;;
;;;; A development build is an image that contains the compiler, the UI
;;;; framework, this file and the application (tools/build-ui-dev.sh), shown
;;;; with a REPL docked in its window (host/web/repl.mjs).  What is evaluated
;;;; there changes the running application: a redefined component is shown at
;;;; once, a redefined function after the evaluation, and the instances of a
;;;; redefined model class get its new slots.
;;;;
;;;; Compile it after dom.lisp and before the application.

(in-package "UI")

;; the instances of redefined classes are updated
(sys::track-instances)

;; called by the page after every evaluation in the REPL
(js:set (js:global) "clysmUiRefresh"
        (lambda ()
          (refresh)
          nil))
