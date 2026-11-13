;;;; Tests of the UI framework on the S-expression backend.
;;;; tools/test-ui.sh compares the output with ui.expected.

(defpackage "UI-TEST" (:use "CL" "UI"))
(in-package "UI-TEST")

(setf *backend* (make-instance 'sexp-backend))

(defun show (title root)
  (format t "~&~%== ~A~%~S~%" title (to-sexp root)))

(defun show-log (title)
  (format t "~&-- ~A~%~{   ~S~%~}" title (backend-log *backend*)))

;;; ---- markup

(let ((root (make-root)) (items '("a" "b")))
  (mount (html (:div :class "box" :hidden nil
                 "n = " 42
                 (when (> (length items) 5) (:p "many"))
                 (:ul (mapcar (lambda (i) (:li i)) items))
                 (ecase (length items)
                   (2 (:b "two"))
                   (3 (:i "three")))))
         root)
  (show "markup" root))

;;; ---- models: a slot that is set renders the components that read it

(defmodel todo ()
  ((text :initarg :text :accessor todo-text)
   (done :initarg :done :initform nil :accessor todo-done)))

(defstate *todos* nil)
(defstate *title* "list")

(defvar *renders* nil "names of the components that rendered")

(defcomponent todo-item (todo)
  (push (todo-text todo) *renders*)
  (:li :class (if (todo-done todo) "done" nil)
    (:input :type "checkbox" :checked (todo-done todo)
            :on-change (lambda () (setf (todo-done todo) (not (todo-done todo)))))
    (todo-text todo)))

(defcomponent todo-list ()
  (push :list *renders*)
  (:div
    (:h1 *title*)
    (:ul (mapcar (lambda (todo) (keyed todo (todo-item todo))) *todos*))
    (:p (count-if-not #'todo-done *todos*) " left")))

(defun renders () (nreverse (shiftf *renders* nil)))

(let ((root (make-root))
      (a (make-instance 'todo :text "a"))
      (b (make-instance 'todo :text "b"))
      (c (make-instance 'todo :text "c")))
  (setf *todos* (list a b c))
  (mount (todo-list) root)
  (show "todo list" root)
  (format t "rendered: ~S~%" (renders))
  (backend-log *backend*)

  (setf (todo-text b) "B!")
  (flush)
  (show-log "text of b")
  (format t "rendered: ~S~%" (renders))

  (fire (find-node root "input" :index 0) "change")
  (show-log "a done (through the listener)")
  (format t "rendered: ~S~%" (renders))

  (setf *title* "my list")
  (flush)
  (show-log "title")
  (format t "rendered: ~S~%" (renders))

  (setf *todos* (list c a b))
  (flush)
  (show-log "c moved to the front")
  (format t "rendered: ~S~%" (renders))

  (setf *todos* (remove a *todos*))
  (flush)
  (show-log "a removed")
  (format t "rendered: ~S~%" (renders))

  ;; several changes, one flush
  (setf (todo-done b) t (todo-done c) t (todo-text c) "C" *title* "done")
  (push (make-instance 'todo :text "d") *todos*)
  (flush)
  (format t "rendered: ~S~%" (renders))
  (show "after all" root)

  ;; a removed component is not rendered again
  (setf (todo-text a) "gone")
  (flush)
  (format t "rendered: ~S~%" (renders)))

;;; ---- &state, replacing nodes, MOUNTED and UNMOUNTED, :ON-MOUNT

(defvar *events* nil)

(defcomponent editor (label &state (editing nil) (edits 0))
  (:div
    (if editing
        (:input :value label
                :on-mount (lambda (node) (push (list :focus (car (to-sexp node))) *events*))
                :on-blur (lambda () (setf editing nil)))
        (:span :on-dblclick (lambda () (setf editing t) (incf edits)) label))
    " edited " edits " times"))

(defmethod mounted ((self editor)) (push (list :mounted (car (ui::component-args self))) *events*))
(defmethod unmounted ((self editor)) (push (list :unmounted (car (ui::component-args self))) *events*))

(defstate *labels* '("x" "y"))

(defcomponent editors ()
  (:section (mapcar (lambda (label) (keyed label (editor label))) *labels*)))

(defun events () (nreverse (shiftf *events* nil)))

(let ((root (make-root)))
  (mount (editors) root)
  (format t "~&~%== state~%events: ~S~%" (events))
  (backend-log *backend*)
  (fire (find-node root "span" :index 1) "dblclick")
  (show-log "y is edited")
  (format t "events: ~S~%" (events))
  ;; the state of y moves with its key
  (setf *labels* '("y" "x" "z"))
  (flush)
  (show "y first" root)
  (format t "events: ~S~%" (events))
  (fire (find-node root "input") "blur")
  (fire (find-node root "span" :index 0) "dblclick")
  (setf *labels* '("y"))
  (flush)
  (show "only y" root)
  (format t "events: ~S~%" (events)))

;;; ---- AUTORUN

(defstate *a* 1)
(defstate *b* 10)

(let* ((log '())
       (reaction (autorun (lambda () (push (+ *a* *b*) log)))))
  (setf *a* 2)
  (incf *b* 5)
  (flush)
  (incf *a*)
  (flush)
  (stop reaction)
  (incf *a*)
  (flush)
  (format t "~&~%== autorun~%~S~%" (reverse log)))

;;; ---- commands

(defstate *count* 0)

(define-command (increment :label "Add one" :menu "Edit" :key "CmdOrCtrl+I") (&optional (n 1))
  (incf *count* n))

(define-command (reset :menu "Edit" :separator t :enabled (plusp *count*)) ()
  (setf *count* 0))

(define-command (fail :menu "Debug") ()
  (restart-case (error "it failed at ~D" *count*)
    (use-seven () :report "Use 7" (setf *count* 7))))

(defcomponent counter ()
  (:div (:span *count*)
        (:button :on-click (command increment 2) "+2")
        (command-button 'reset)))

(let ((root (make-root)))
  (mount (counter) root)
  (show "commands" root)
  (format t "menus: ~S~%"
          (mapcar (lambda (menu) (cons (car menu) (mapcar #'command-label (cdr menu))))
                  (command-menus)))
  (format t "keys: ~S ~S ~S~%" (parse-key "CmdOrCtrl+Shift+S")
          (command-name (command-for-key (parse-key "ctrl+i")))
          (command-for-key (parse-key "Ctrl+Shift+I")))
  (backend-log *backend*)
  (format t "reset while disabled: ~S~%" (invoke-command 'reset))
  (fire (find-node root "button" :index 0) "click" :event)
  (show-log "+2 (the reset button is enabled)")
  (increment)                            ; as from the REPL
  (flush)
  (show-log "(increment)")
  (fire (find-node root "button" :index 1) "click" :event)
  (show-log "reset")

  ;; ---- errors: the "user" chooses a restart
  (invoke-command 'fail)
  (flush)
  (show-log "fail, cancelled")
  (setf (backend-chooser *backend*) (lambda (condition restarts)
                                      (declare (ignore condition))
                                      (first restarts)))
  (invoke-command 'fail)
  (flush)
  (show-log "fail, with the restart Use 7")
  (setf (backend-chooser *backend*) nil))

;;; an error while rendering: the component keeps what it showed

(defcomponent fragile ()
  (:p "count " (if (> *count* 100) (error "too many") *count*)))

(let ((root (make-root)))
  (setf *count* 1)
  (mount (fragile) root)
  (backend-log *backend*)
  (setf *count* 101)
  (flush)
  (show-log "an error in render")
  (show "still shown" root)
  (setf *count* 2)
  (flush)
  (show "and it recovers" root))

;;; ---- a redefinition shows at once

(defcomponent greeting (name &state (clicks 0))
  (:p :on-click (lambda () (incf clicks)) "hello " name " " clicks))

(let ((root (make-root)))
  (mount (greeting "world") root)
  (fire (find-node root "p") "click")
  (backend-log *backend*)
  (eval '(defcomponent greeting (name &state (clicks 0) (extra "!"))
          (:h1 :on-click (lambda () (incf clicks)) "HELLO " name " " clicks extra)))
  (flush)
  (show-log "redefined")
  (show "redefined" root))
