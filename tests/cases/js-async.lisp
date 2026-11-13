;;; Computations of JS:ASYNC-FUNCTION: several wait at the same time, each
;;; with its own dynamic bindings; errors cross to JavaScript and back.
;;; (Run on the Node.js host; the expected output is in js-async.expected.)

(defvar *x* :global)
(defvar *log* nil)
(defun note (&rest args) (push args *log*))

(defun sleep-ms (ms)
  (js:await (js:new (js:global "Promise")
                    (lambda (resolve &rest more)
                      (declare (ignore more))
                      (js:funcall (js:global "setTimeout") resolve ms)
                      nil))))

(defun worker (name delays)
  (let ((*x* name))
    (dolist (d delays)
      (sleep-ms d)
      (note name *x*))
    (list name :done)))

(define-condition my-error (error) ((n :initarg :n :reader my-error-n)))

(defun main ()
  (let ((*x* :main))
    ;; two computations wait at the same time, a third waits for both
    (note :all (js:await-all (list (js:async #'worker :a '(30 20))
                                   (js:async #'worker :b '(10 30))
                                   (js:async (lambda ()
                                               (js:await-all (list (js:async #'worker :c '(5))
                                                                   (js:async #'worker :d '(1)))))))))
    (note :main *x*)
    ;; an error in a computation rejects its promise with a JavaScript Error
    (note :rejected
          (handler-case (js:await (js:async (lambda () (sleep-ms 1) (error "boom ~D" 42))))
            (error (c) (princ-to-string c))))
    ;; a Lisp error in a callback that JavaScript calls comes back as itself
    (note :callback
          (handler-case (js:funcall (js:eval "(f) => f(7)")
                                    (lambda (n) (error 'my-error :n n)))
            (my-error (c) (list :my-error (my-error-n c)))))
    ;; JavaScript sees the error as an Error it can catch
    (note :caught
          (js:funcall (js:eval "(f) => { try { f(); return 'no'; } catch (e) { return e.name + ': ' + e.message; } }")
                      (lambda () (error "from Lisp"))))
    ;; a JavaScript exception is a JS:JS-ERROR
    (note :js-error
          (handler-case (js:eval "null.x")
            (js:js-error (c) (list (js:js-error-name c)))))
    (note :can-await (js:can-await-p))
    (dolist (entry (reverse *log*)) (print entry))
    (terpri)
    :finished))

(note :toplevel *x* (js:can-await-p))
(js:async (lambda () (print (js:await (js:async #'main))) (terpri)))
(note :after-start *x*)
