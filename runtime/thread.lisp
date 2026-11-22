;;;; thread.lisp — green threads
;;;;
;;;; A thread is a computation of JS:ASYNC-FUNCTION: it runs on a stack of
;;;; its own that JavaScript Promise Integration suspends when it waits, and
;;;; has its own dynamic bindings (runtime/js.lisp).  Threads take turns in
;;;; the one thread of the host: one runs until it waits — for a lock, a
;;;; condition variable, another thread, SLEEP, THREAD-YIELD, or input from
;;;; a socket or the terminal (which then do not block the others) — and
;;;; JavaScript's event loop resumes the next one whose wait is over.
;;;;
;;;; The program itself (SYSTEM::MAIN) runs as a computation where the host
;;;; gives computations deep stacks (host/run.mjs), so it can wait too.
;;;; Until a second thread is made, nothing changes: blocking operations
;;;; block as before.
;;;;
;;;; Interrupts (INTERRUPT-THREAD) run when the thread waits or, in code
;;;; compiled with *INTERRUPT-CHECKS*, at its next check.  The checks also
;;;; let threads waiting for input run when input arrived, and threads whose
;;;; SLEEP is over.

(defstruct (thread (:constructor %make-thread (name function)) (:predicate threadp))
  name function
  (state :new)                          ; :new :running :waiting :finished :aborted
  (values nil)                          ; of the function, when :finished
  (error nil)                           ; the condition that ended it, when :aborted
  (wake nil)                            ; ends the current wait (a JavaScript function)
  (interrupts nil)                      ; functions to call, in order
  (joiners nil)                         ; threads waiting for it to end
  (waiting-for nil))                    ; what it waits for (for display)

;;; (a method of PRINT-OBJECT, which a shaken program keeps only with the
;;; printer, rather than a print function, which the class would keep)
(defmethod print-object ((th thread) s)
  (print-unreadable-object (th s :type t :identity t)
    (format s "~S ~(~A~)" (thread-name th) (thread-state th))))

(defconstant +ms-per-second+ 1000 "INTERNAL-TIME-UNITS-PER-SECOND (defined later, runtime/misc.lisp).")

(defvar *main-thread* nil "The thread of the program itself.")
(defvar *current-thread* nil "The current thread (bound in each thread).")
(defvar *all-threads* nil "The threads that have not ended, newest first.")
(defvar *threads-started* nil)          ; (defined in runtime/clos.lisp)
(defvar *io-waiters* 0 "The number of threads waiting for input.")
(defvar *earliest-wakeup* nil "The internal real time when a SLEEP is over first, or NIL.")
(defvar *last-io-seq* nil)
(defvar *wakeups* 0 "Incremented when a waiting thread is woken by another (%WAKE).")
(defvar *slice-start* 0 "The internal real time when the running thread last got its turn.")
(defparameter *time-slice* 10
  "Milliseconds a thread runs in code with interrupt checks before the others
get their turn (preemption).")
(defparameter *poll-interval* 10000
  "The number of interrupt checks between two calls of %INTERRUPT-POLL while
threads run.")
(defvar *preemption-inhibited* nil)     ; (defined in runtime/clos.lisp)

(defun %resumed ()
  (setq *slice-start* (get-internal-real-time)))
(defvar *last-wakeups* 0)

(defun %main-thread ()
  (or *main-thread*
      (let ((th (%make-thread "main thread" nil)))
        (setf (thread-state th) :running)
        (push th *all-threads*)
        (setq *main-thread* th))))

(defun current-thread ()
  (or *current-thread* (%main-thread)))

(defun all-threads () (copy-list *all-threads*))

(defun thread-alive-p (thread)
  (not (member (thread-state thread) '(:finished :aborted))))

(defun %threads-supported-p ()
  "True if this host can run threads: JavaScript can suspend WebAssembly."
  (%js-suspending-p))

(defun %can-wait-p ()
  "True if the current thread can wait without blocking the others."
  (and *threads-started* (js:can-await-p)))

;;; ------------------------------------------------------------
;;; waiting

(%defvar-host *js-deferred* nil)

(defun %js-deferred (ms)
  "A JavaScript array [promise, end]: END ends the wait (the promise is
resolved with 0), and after MS milliseconds (unless MS is negative) the
promise is resolved with 1."
  (js:funcall (or *js-deferred*
                  (setq *js-deferred*
                        (%js-helper "deferred")))
              ms))

(defun %wait (&optional timeout what)
  "Wait until another thread ends the wait (%WAKE) or, with TIMEOUT
(seconds), until it is over: :WOKEN or :TIMEOUT.  The thread's interrupts
run first."
  (let ((th (current-thread)))
    (%run-interrupts th)
    (unless (%can-wait-p)
      (error "~S cannot wait here~:[ (this JavaScript engine cannot suspend WebAssembly)~;~]"
             th (%threads-supported-p)))
    (let* ((pair (%js-deferred (if timeout (max 0 (round (* timeout 1000))) -1)))
           (end (js:get pair 1)))
      (setf (thread-wake th) end
            (thread-state th) :waiting
            (thread-waiting-for th) what)
      (let ((r (unwind-protect (js:await (js:get pair 0))
                 (%resumed)
                 (when (eq (thread-wake th) end) (setf (thread-wake th) nil))
                 (setf (thread-state th) :running
                       (thread-waiting-for th) nil))))
        (%run-interrupts th)
        (if (eql r 1) :timeout :woken)))))

(defun %wake (thread)
  (let ((end (thread-wake thread)))
    (when end
      (setf (thread-wake thread) nil)
      (incf *wakeups*)
      (js:funcall end 0))))

(defun %await-promise (promise &optional what)
  "Wait for the JavaScript PROMISE (input), letting the other threads run;
its value."
  (let ((th (current-thread)))
    (%run-interrupts th)
    (setf (thread-state th) :waiting (thread-waiting-for th) what)
    (incf *io-waiters*)
    (unwind-protect (js:await promise)
      (%resumed)
      (decf *io-waiters*)
      (setf (thread-state th) :running (thread-waiting-for th) nil))))

(%defvar-host *js-yield* nil)

(defun thread-yield ()
  "Let the other threads that can run, run."
  (when (%can-wait-p)
    (let ((th (current-thread)))
      (js:await (js:funcall (or *js-yield*
                                (setq *js-yield*
                                      (%js-helper "yieldTurn")))))
      (%resumed)
      (%run-interrupts th)))
  nil)

(defun %thread-sleep (seconds)
  "SLEEP in a program with threads: the others run meanwhile."
  (let ((end (+ (get-internal-real-time) (* seconds +ms-per-second+))))
    (loop
      (let ((left (- end (get-internal-real-time))))
        (when (<= left 0) (return nil))
        (when (or (null *earliest-wakeup*) (< end *earliest-wakeup*))
          (setq *earliest-wakeup* end))
        (%wait (/ left +ms-per-second+) :sleep)
        (when (and *earliest-wakeup* (<= *earliest-wakeup* (get-internal-real-time)))
          (setq *earliest-wakeup* nil))))))

;;; Interrupt checks (%INTERRUPT-POLL): an interrupt of this thread runs;
;;; the others get their turn when the thread's time slice is over
;;; (preemption), or when one waiting for input got input, one's SLEEP is
;;; over or one was woken (by a lock released, a notification...).
(defun %thread-poll ()
  (when *threads-started*
    ;; checked more often while threads run
    (%set-poll-count *poll-interval*)
    (let ((th (current-thread)))
      (when (thread-interrupts th)
        (%run-interrupts th))
      (when (and (js:can-await-p) (not *preemption-inhibited*))
        (let ((yield nil))
          ;; preemption: its time slice is over and another thread may run
          (when (and (cdr *all-threads*)
                     (>= (- (get-internal-real-time) *slice-start*) *time-slice*))
            (setq yield t))
          (when (> *io-waiters* 0)
            (let ((seq (ignore-errors (%tcp-data-seq))))
              (unless (eql seq *last-io-seq*)
                (setq *last-io-seq* seq yield t))))
          (when (and *earliest-wakeup* (<= *earliest-wakeup* (get-internal-real-time)))
            (setq *earliest-wakeup* nil yield t))
          ;; a thread was woken (by this one): it may run
          (unless (eql *wakeups* *last-wakeups*)
            (setq *last-wakeups* *wakeups* yield t))
          (when yield (thread-yield)))))))

(defun %fd-wait-input ()
  "With threads, wait for input from the terminal letting the others run."
  (when (%can-wait-p)
    (let ((stdin (ignore-errors (js:global "__clysm_stdin"))))
      (when (js:js-value-p stdin)
        (loop until (js:call stdin "ready")
              do (%await-promise (js:call stdin "wait") :input))))))

;;; ------------------------------------------------------------
;;; threads

(defun make-thread (function &key name)
  "Start a thread that calls FUNCTION; it runs when the current thread waits."
  (unless (%threads-supported-p)
    (error "Threads need JavaScript Promise Integration (Node.js 24, Chrome 137 or later)"))
  (%main-thread)
  (let ((th (%make-thread name function)))
    (push th *all-threads*)
    (setq *threads-started* t)
    (let ((promise
            (js:async
             (lambda ()
               ;; nothing of the dynamic environment of the thread that
               ;; made it: the handlers and restarts around the start of the
               ;; computation are those of the creator
               (let ((*current-thread* th)
                     (*handler-frames* (%make-handler-frames 8 nil))
                     (*handler-clusters* 0)
                     (*restart-clusters* nil)
                     (*condition-restarts* nil)
                     (*debug-frame-stack* nil))
                 ;; the thread that made it goes on first
                 (js:await (js:funcall (or *js-yield*
                                           (setq *js-yield*
                                                 (%js-helper "yieldTurn")))))
                 (%resumed)
                 (setf (thread-state th) :running)
                 (%run-thread th))
               nil))))
      ;; an exception that Lisp cannot handle (the stack exhausted) ends it
      (js:call promise "catch"
               (lambda (e)
                 (%end-thread th :aborted (make-condition 'simple-error
                                                          :format-control "~A"
                                                          :format-arguments (list e)))
                 nil)))
    th))

(defun %run-thread (th)
  (let ((ok nil))
    (unwind-protect
         (catch '%thread-exit
           ;; an error that would enter the debugger ends the thread (a
           ;; debugger of the program's own, SWANK's *DEBUGGER-HOOK*, is
           ;; called before; see %SCRIPT-MAIN)
           (let ((*debugger-fallback*
                   (lambda (c)
                     (setf (thread-error th) c)
                     (format *error-output* "~&;; Thread ~A ended by an error: ~A~%"
                             (thread-name th) c)
                     (finish-output *error-output*)
                     (throw '%thread-exit nil))))
             (setf (thread-values th) (multiple-value-list (funcall (thread-function th))))
             (setq ok t)))
      (%end-thread th (if ok :finished :aborted) (thread-error th)))))

(defun %end-thread (th state error)
  (when (thread-alive-p th)
    (setf (thread-state th) state (thread-error th) error)
    (setq *all-threads* (remove th *all-threads*))
    (dolist (j (thread-joiners th)) (%wake j))
    (setf (thread-joiners th) nil)))

(defun join-thread (thread &key timeout)
  "Wait until THREAD ends; its values, or (values NIL :TIMEOUT).  Signals an
error if it was ended by an error or by DESTROY-THREAD."
  (when (eq thread (current-thread)) (error "~S cannot join itself" thread))
  (let ((deadline (and timeout (+ (get-internal-real-time) (* timeout +ms-per-second+)))))
    (loop
      (case (thread-state thread)
        (:finished (return (values-list (thread-values thread))))
        (:aborted (error "~S ended~@[ by ~A~]" thread (thread-error thread))))
      (let ((self (current-thread)))
        (pushnew self (thread-joiners thread))
        (let ((r (unwind-protect
                      (%wait (and deadline (max 0 (/ (- deadline (get-internal-real-time))
                                                     +ms-per-second+)))
                             thread)
                   (setf (thread-joiners thread) (remove self (thread-joiners thread))))))
          (when (and (eq r :timeout) (thread-alive-p thread))
            (return (values nil :timeout))))))))

(defun %run-interrupts (th)
  (loop
    (let ((f (pop (thread-interrupts th))))
      (unless f (return))
      (funcall f))))

(defun interrupt-thread (thread function)
  "Call FUNCTION in THREAD: when it waits, or at its next interrupt check."
  (cond ((eq thread (current-thread)) (funcall function))
        ((thread-alive-p thread)
         (setf (thread-interrupts thread) (append (thread-interrupts thread) (list function)))
         (%wake thread)))
  thread)

(defun destroy-thread (thread)
  "End THREAD (its UNWIND-PROTECT cleanups run)."
  (when (eq thread (%main-thread)) (error "The main thread cannot be destroyed"))
  (when (eq (thread-state thread) :new)
    ;; it has not run yet: it ends when it starts
    (setf (thread-function thread) (lambda () (throw '%thread-exit nil))))
  (interrupt-thread thread (lambda () (throw '%thread-exit nil))))

;;; ------------------------------------------------------------
;;; locks, condition variables, semaphores

(defstruct (mutex (:constructor make-mutex (&key name)) (:predicate mutexp))
  name (owner nil) (count 0) (waiters nil))

(defun grab-mutex (mutex &key (wait t) timeout recursive)
  "Take MUTEX, waiting while another thread holds it (unless WAIT is false).
False if it was not taken (TIMEOUT seconds passed)."
  (let ((self (current-thread))
        (deadline (and timeout (+ (get-internal-real-time) (* timeout +ms-per-second+)))))
    (loop
      (let ((owner (mutex-owner mutex)))
        (cond ((null owner)
               (setf (mutex-owner mutex) self (mutex-count mutex) 1)
               (return t))
              ((eq owner self)
               (if recursive
                   (progn (incf (mutex-count mutex)) (return t))
                   (error "~S already holds ~S" self mutex)))
              ((not (thread-alive-p owner))
               ;; held by a thread that ended
               (setf (mutex-owner mutex) nil))
              ((not wait) (return nil))
              (t
               (setf (mutex-waiters mutex) (append (mutex-waiters mutex) (list self)))
               (let ((r (unwind-protect
                             (%wait (and deadline (max 0 (/ (- deadline (get-internal-real-time))
                                                            +ms-per-second+)))
                                    mutex)
                          (setf (mutex-waiters mutex) (remove self (mutex-waiters mutex))))))
                 (when (and (eq r :timeout) (not (null (mutex-owner mutex))))
                   (return nil)))))))))

(defun release-mutex (mutex)
  (unless (eq (mutex-owner mutex) (current-thread))
    (error "~S does not hold ~S" (current-thread) mutex))
  (when (<= (decf (mutex-count mutex)) 0)
    (setf (mutex-owner mutex) nil (mutex-count mutex) 0)
    (let ((next (first (mutex-waiters mutex))))
      (when next (%wake next))))
  mutex)

(defmacro with-mutex ((mutex &key (wait t) timeout recursive) &body body)
  (let ((m (gensym "MUTEX")) (got (gensym "GOT")))
    `(let* ((,m ,mutex) (,got nil))
       (unwind-protect
            (when (setq ,got (grab-mutex ,m :wait ,wait :timeout ,timeout :recursive ,recursive))
              (locally ,@body))
         (when ,got (release-mutex ,m))))))

(defstruct (waitqueue (:constructor make-waitqueue (&key name)) (:predicate waitqueuep))
  name (waiters nil))

(defun condition-wait (queue mutex &key timeout)
  "Release MUTEX, wait for CONDITION-NOTIFY (or TIMEOUT seconds), take MUTEX
again.  False if the time was over."
  (let* ((self (current-thread))
         (count (mutex-count mutex)))
    (unless (eq (mutex-owner mutex) self)
      (error "~S does not hold ~S" self mutex))
    (setf (waitqueue-waiters queue) (append (waitqueue-waiters queue) (list self)))
    (setf (mutex-count mutex) 1)
    (release-mutex mutex)
    (let ((r :woken))
      (unwind-protect
           (setq r (%wait timeout queue))
        (setf (waitqueue-waiters queue) (remove self (waitqueue-waiters queue)))
        (grab-mutex mutex)
        (setf (mutex-count mutex) count))
      (not (eq r :timeout)))))

(defun condition-notify (queue &optional (n 1))
  (dotimes (i n)
    (let ((th (pop (waitqueue-waiters queue))))
      (if th (%wake th) (return))))
  nil)

(defun condition-broadcast (queue)
  (condition-notify queue (length (waitqueue-waiters queue))))

(defstruct (semaphore (:constructor make-semaphore (&key name (count 0))) (:predicate semaphorep))
  name (count 0) (waiters nil))

(defun signal-semaphore (semaphore &optional (n 1))
  (incf (semaphore-count semaphore) n)
  (loop for th in (semaphore-waiters semaphore)
        repeat n do (%wake th))
  nil)

(defun wait-on-semaphore (semaphore &key timeout)
  "Decrement the count of SEMAPHORE, waiting while it is zero; the new
count, or NIL if TIMEOUT seconds passed."
  (let ((self (current-thread))
        (deadline (and timeout (+ (get-internal-real-time) (* timeout +ms-per-second+)))))
    (loop
      (when (> (semaphore-count semaphore) 0)
        (return (decf (semaphore-count semaphore))))
      (unless (%can-wait-p)
        (error "~S would wait for ever for ~S" self semaphore))
      (setf (semaphore-waiters semaphore) (append (semaphore-waiters semaphore) (list self)))
      (let ((r (unwind-protect
                    (%wait (and deadline (max 0 (/ (- deadline (get-internal-real-time))
                                                   +ms-per-second+)))
                           semaphore)
                 (setf (semaphore-waiters semaphore) (remove self (semaphore-waiters semaphore))))))
        (when (and (eq r :timeout) (<= (semaphore-count semaphore) 0))
          (return nil))))))

;;; ------------------------------------------------------------
;;; the program as a thread

(defun %deep-async-stacks-p ()
  "True if the host gives computations stacks as deep as its own."
  (let ((v (ignore-errors (js:global "__clysm_deep_async_stacks"))))
    (eq v t)))

(defvar *%main-calls* 0
  "The calls of %CALL-AS-MAIN-THREAD whose functions have not returned (MAIN
calls it inside the function of a heap image, which was called by it).")

(defun %main-calls-reset () (setq *%main-calls* 0))

(defun %call-as-main-thread (function)
  "Call FUNCTION as the main thread, a computation that can wait, where the
host gives computations deep stacks; else call it."
  (setq *%main-calls* (+ *%main-calls* 1))
  (if (and (%deep-async-stacks-p) (%threads-supported-p))
      (let ((promise (js:async (lambda ()
                                 (%main-thread)
                                 (funcall function)
                                 (%main-done)
                                 nil))))
        (js:call promise "catch"
                 (lambda (e)
                   (format *error-output* "~&;; the program ended by ~A~%" (or (ignore-errors (js:get e "stack")) e))
                   (finish-output *error-output*)
                   (%exit 2)))
        nil)
      (multiple-value-prog1 (funcall function) (%main-done))))

(defun %main-done ()
  "Tell the host that the program's main function has returned: a host
whose process the program owns (host/run.mjs, in the main thread) ends the
process when nothing is left to do, without waiting for the engine's
background work (see \"Start-up and loading\" in docs/INTERNALS.md)."
  (setq *%main-calls* (- *%main-calls* 1))
  (when (eql *%main-calls* 0)
    (let ((f (js:global "__clysm_main_done")))
      (when f (js:funcall f)))))
