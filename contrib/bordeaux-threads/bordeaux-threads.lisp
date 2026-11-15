;;;; bordeaux-threads.lisp — Bordeaux Threads for the one thread of Clysm
;;;; (see bordeaux-threads.asd).  Both APIs share one implementation: the
;;;; symbols of BT (version 1) are those of BT2 where the names agree.

(defpackage :bordeaux-threads-2
  (:nicknames :bt2)
  (:use :common-lisp)
  (:export #:+supports-threads-p+ #:bordeaux-threads-error #:not-implemented
           #:thread #:thread-name #:thread-native-thread #:threadp #:make-thread
           #:*default-special-bindings* #:*standard-io-bindings* #:current-thread
           #:all-threads #:start-multiprocessing #:interrupt-thread #:signal-in-thread
           #:warn-in-thread #:error-in-thread #:destroy-thread #:thread-alive-p
           #:join-thread #:abnormal-exit #:abnormal-exit-condition #:thread-yield
           #:lock #:lockp #:recursive-lock #:recursive-lock-p #:lock-name
           #:lock-native-lock #:native-lock #:native-lock-p #:native-recursive-lock
           #:native-recursive-lock-p #:make-lock #:acquire-lock #:release-lock
           #:with-lock-held #:make-recursive-lock #:acquire-recursive-lock
           #:release-recursive-lock #:with-recursive-lock-held
           #:condition-variable #:condition-variable-p #:make-condition-variable
           #:condition-wait #:condition-notify #:condition-broadcast
           #:semaphore #:semaphorep #:make-semaphore #:signal-semaphore #:wait-on-semaphore
           #:atomic-integer #:make-atomic-integer #:atomic-integer-compare-and-swap
           #:atomic-integer-decf #:atomic-integer-incf #:atomic-integer-value
           #:timeout #:with-timeout
           ;; not in Bordeaux Threads: how MAKE-THREAD starts a computation
           #:*thread-starter*))

(defpackage :bordeaux-threads
  (:nicknames :bt)
  (:use :common-lisp)
  (:import-from :bt2
                #:thread #:make-thread #:current-thread #:threadp #:thread-name
                #:start-multiprocessing #:*default-special-bindings* #:*standard-io-bindings*
                #:lock #:release-lock #:with-lock-held
                #:recursive-lock #:recursive-lock-p
                #:release-recursive-lock #:with-recursive-lock-held
                #:make-condition-variable #:condition-wait #:condition-notify
                #:make-semaphore #:signal-semaphore #:wait-on-semaphore #:semaphore
                #:with-timeout #:timeout #:all-threads #:interrupt-thread #:destroy-thread
                #:thread-alive-p #:join-thread #:thread-yield)
  (:export #:thread #:make-thread #:current-thread #:threadp #:thread-name
           #:start-multiprocessing #:*default-special-bindings* #:*standard-io-bindings*
           #:*supports-threads-p*
           #:lock #:make-lock #:lock-p #:acquire-lock #:release-lock #:with-lock-held
           #:recursive-lock #:make-recursive-lock #:recursive-lock-p
           #:acquire-recursive-lock #:release-recursive-lock #:with-recursive-lock-held
           #:make-condition-variable #:condition-wait #:condition-notify
           #:make-semaphore #:signal-semaphore #:wait-on-semaphore #:semaphore #:semaphore-p
           #:with-timeout #:timeout #:all-threads #:interrupt-thread #:destroy-thread
           #:thread-alive-p #:join-thread #:thread-yield))

(in-package :bordeaux-threads-2)

(defconstant +supports-threads-p+ nil)  ; (in the compiled code of libraries; see BT:*SUPPORTS-THREADS-P*)

(define-condition bordeaux-threads-error (error) ())

(define-condition not-implemented (bordeaux-threads-error)
  ((operation :initarg :operation :initform nil :reader not-implemented-operation))
  (:report (lambda (c s)
             (format s "~@[~A: ~]Clysm has only one thread"
                     (not-implemented-operation c)))))

(defvar *default-special-bindings* '())
(defvar *standard-io-bindings* '())

;;; Green threads (runtime/thread.lisp) are used where JavaScript can suspend
;;; WebAssembly and no *THREAD-STARTER* is set; otherwise there is one
;;; thread and MAKE-THREAD runs a function later through *THREAD-STARTER*
;;; (e.g. from the event loop of a page, as Lem does) or signals
;;; NOT-IMPLEMENTED.

;;; ---- threads

(defclass thread ()
  ((name :initarg :name :initform nil :reader thread-name)
   (function :initarg :function :initform nil :reader thread-function)
   (native :initarg :native :initform nil :accessor %thread-native)
   (alive :initform t :accessor %thread-alive)
   (values :initform nil :accessor %thread-values)
   (error :initform nil :accessor %thread-error)))

(defmethod print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (format stream "~S" (thread-name thread))))

(defun threadp (object) (typep object 'thread))
(defun thread-native-thread (thread) (or (%thread-native thread) thread))

(defvar *main-thread* (make-instance 'thread :name "main thread"))
(defvar *current-thread* *main-thread*)
(defvar *threads* (list *main-thread*))

(defvar *thread-starter* nil
  "NIL, or a function called with a function of no arguments by MAKE-THREAD:
it runs the function later in the one thread, e.g. from the event loop of a
page.  Without it, MAKE-THREAD makes a green thread where the host can, else
signals NOT-IMPLEMENTED.")

(defun green-threads-p ()
  "True if MAKE-THREAD makes green threads."
  (and (null *thread-starter*) (sys::%threads-supported-p)))

(defun green-running-p ()
  "True once green threads run: locks and waits are those of green threads."
  (and (null *thread-starter*) sys::*threads-started*))

(define-condition abnormal-exit (bordeaux-threads-error)
  ((condition :initarg :condition :initform nil :reader abnormal-exit-condition))
  (:report (lambda (c s)
             (format s "The thread exited abnormally~@[: ~A~]" (abnormal-exit-condition c)))))

(defun %run-in-thread (thread function initial-bindings)
  (let ((*current-thread* thread))
    (progv (mapcar #'car initial-bindings)
        (mapcar (lambda (b) (eval (cdr b))) initial-bindings)
      (unwind-protect
           (handler-case (setf (%thread-values thread)
                               (multiple-value-list (funcall function)))
             (error (c) (setf (%thread-error thread) c)))
        (setf (%thread-alive thread) nil)
        (setf *threads* (remove thread *threads*))))))

(defun make-thread (function &key name (initial-bindings *default-special-bindings*)
                               trap-conditions)
  (declare (ignore trap-conditions))
  (let ((thread (make-instance 'thread :name name :function function)))
    (cond ((green-threads-p)
           (push thread *threads*)
           (setf (%thread-native thread)
                 (sys::make-thread (lambda () (%run-in-thread thread function initial-bindings))
                                   :name name)))
          (*thread-starter*
           (push thread *threads*)
           (funcall *thread-starter*
                    (lambda () (%run-in-thread thread function initial-bindings))))
          (t (error 'not-implemented :operation 'make-thread)))
    thread))

(defun current-thread () *current-thread*)
(defun all-threads () (copy-list *threads*))
(defun start-multiprocessing () nil)
(defun thread-alive-p (thread) (%thread-alive thread))
(defun thread-yield () (sys::thread-yield))

(defun interrupt-thread (thread function &rest arguments)
  "FUNCTION is called in THREAD when it waits (or at an interrupt check);
with one thread, now."
  (let ((native (%thread-native thread)))
    (cond ((and native (green-running-p))
           (sys::interrupt-thread native (lambda () (apply function arguments))))
          ((%thread-alive thread)
           (apply function arguments))))
  thread)

(defun signal-in-thread (thread condition &rest args)
  (interrupt-thread thread (lambda () (apply #'signal condition args))))
(defun warn-in-thread (thread condition &rest args)
  (interrupt-thread thread (lambda () (apply #'warn condition args))))
(defun error-in-thread (thread condition &rest args)
  (interrupt-thread thread (lambda () (apply #'error condition args))))

(defun destroy-thread (thread)
  (when (eq thread *main-thread*)
    (error 'bordeaux-threads-error))
  (let ((native (%thread-native thread)))
    (if native
        (sys::destroy-thread native)
        (setf (%thread-alive thread) nil
              *threads* (remove thread *threads*))))
  thread)

(defun join-thread (thread)
  (let ((native (%thread-native thread)))
    (when (and native (%thread-alive thread))
      (ignore-errors (sys::join-thread native))
      (when (sys::thread-alive-p native)
        (error 'bordeaux-threads-error))
      ;; destroyed before it ended
      (setf (%thread-alive thread) nil))
    (cond ((%thread-alive thread)
           (error 'not-implemented :operation 'join-thread))
          ((%thread-error thread)
           (error 'abnormal-exit :condition (%thread-error thread)))
          (t (values-list (%thread-values thread))))))

;;; ---- locks
;;;
;;; With green threads, a lock is a mutex of runtime/thread.lisp.  A lock
;;; held by the current thread is entered again (as with one thread, where
;;; the holder is the code around, e.g. a handler or an interrupt function
;;; running inside the dynamic extent of WITH-LOCK-HELD).

(defclass lock ()
  ((name :initarg :name :initform nil :reader lock-name)
   (mutex :reader lock-mutex)))
(defclass recursive-lock (lock) ())

(defmethod initialize-instance :after ((lock lock) &key)
  (setf (slot-value lock 'mutex) (sys::make-mutex :name (lock-name lock))))

(defun lock-owner (lock) (sys::mutex-owner (lock-mutex lock)))

(defun lockp (object) (typep object 'lock))
(defun bt::lock-p (object) (typep object 'lock))
(defun recursive-lock-p (object) (typep object 'recursive-lock))
(defun native-lock-p (object) (lockp object))
(defun native-recursive-lock-p (object) (recursive-lock-p object))
(deftype native-lock () 'lock)
(deftype native-recursive-lock () 'recursive-lock)
(defun lock-native-lock (lock) lock)

(defun make-lock (&key name) (make-instance 'lock :name name))
(defun make-recursive-lock (&key name) (make-instance 'recursive-lock :name name))
(defun bt::make-lock (&optional name) (make-instance 'lock :name name))
(defun bt::make-recursive-lock (&optional name) (make-instance 'recursive-lock :name name))

(defun acquire-lock (lock &key (wait t) timeout)
  (let ((m (lock-mutex lock)))
    (if (and (sys::mutex-owner m) (not (green-running-p)))
        nil                             ; held: with one thread it would wait for ever
        (sys::grab-mutex m :wait wait :timeout timeout))))
(defun bt::acquire-lock (lock &optional (wait-p t))
  (acquire-lock lock :wait wait-p))

(defun release-lock (lock)
  (sys::release-mutex (lock-mutex lock))
  lock)

(defun acquire-recursive-lock (lock &key (wait t) timeout)
  (sys::grab-mutex (lock-mutex lock) :wait wait :timeout timeout :recursive t))
(defun bt::acquire-recursive-lock (lock) (acquire-recursive-lock lock))

(defun release-recursive-lock (lock)
  (sys::release-mutex (lock-mutex lock))
  lock)

(defun %call-with-lock-held (lock timeout function)
  (let ((m (lock-mutex lock)))
    (if (eq (sys::mutex-owner m) (sys::current-thread))
        (funcall function)               ; entered again
        (let ((got nil))
          (unwind-protect
               (when (setq got (if (and (sys::mutex-owner m) (not (green-running-p)))
                                   ;; one thread: the holder is the code around
                                   :again
                                   (sys::grab-mutex m :timeout timeout)))
                 (funcall function))
            (when (eq got t) (sys::release-mutex m)))))))

(defmacro with-lock-held ((place &key timeout) &body body)
  `(%call-with-lock-held ,place ,timeout (lambda () ,@body)))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  `(%call-with-lock-held ,place ,timeout (lambda () ,@body)))

;;; ---- condition variables: with one thread nothing can notify while the
;;; thread waits, so a wait times out at once

(defclass condition-variable ()
  ((name :initarg :name :initform nil)
   (queue :reader condition-variable-queue)))
(defmethod initialize-instance :after ((cv condition-variable) &key)
  (setf (slot-value cv 'queue) (sys::make-waitqueue :name (slot-value cv 'name))))
(defun condition-variable-p (object) (typep object 'condition-variable))
(defun make-condition-variable (&key name) (make-instance 'condition-variable :name name))

(defun condition-wait (condition-variable lock &key timeout)
  (cond ((green-running-p)
         (sys::condition-wait (condition-variable-queue condition-variable) (lock-mutex lock)
                              :timeout timeout))
        (timeout nil)
        (t (error 'not-implemented :operation 'condition-wait))))

(defun condition-notify (condition-variable)
  (sys::condition-notify (condition-variable-queue condition-variable))
  nil)
(defun condition-broadcast (condition-variable)
  (sys::condition-broadcast (condition-variable-queue condition-variable))
  nil)

;;; ---- semaphores

(defclass semaphore ()
  ((name :initarg :name :initform nil)
   (native :reader semaphore-native)))
(defmethod initialize-instance :after ((s semaphore) &key (count 0))
  (setf (slot-value s 'native) (sys::make-semaphore :name (slot-value s 'name) :count count)))
(defun semaphore-count (semaphore) (sys::semaphore-count (semaphore-native semaphore)))
(defun semaphorep (object) (typep object 'semaphore))
(defun bt::semaphore-p (object) (typep object 'semaphore))
(defun make-semaphore (&key name (count 0))
  (make-instance 'semaphore :name name :count count))

(defun signal-semaphore (semaphore &key (count 1))
  (sys::signal-semaphore (semaphore-native semaphore) count)
  nil)

(defun wait-on-semaphore (semaphore &key timeout)
  (let ((native (semaphore-native semaphore)))
    (cond ((plusp (sys::semaphore-count native))
           (decf (sys::semaphore-count native))
           t)
          ((green-running-p)
           (and (sys::wait-on-semaphore native :timeout timeout) t))
          (timeout nil)
          (t (error 'not-implemented :operation 'wait-on-semaphore)))))

;;; ---- atomic integers

(defstruct (atomic-integer (:constructor %make-atomic-integer (value)))
  (value 0 :type integer))
(defun make-atomic-integer (&key (value 0)) (%make-atomic-integer value))
(defun atomic-integer-compare-and-swap (atomic old new)
  (when (= (atomic-integer-value atomic) old)
    (setf (atomic-integer-value atomic) new)
    t))
(defun atomic-integer-incf (atomic &optional (delta 1))
  (incf (atomic-integer-value atomic) delta))
(defun atomic-integer-decf (atomic &optional (delta 1))
  (decf (atomic-integer-value atomic) delta))

;;; ---- timeouts: the body cannot be interrupted (with green threads, it
;;; is interrupted when it waits or at an interrupt check)

(define-condition timeout (serious-condition)
  ((length :initarg :length :initform nil :reader timeout-length))
  (:report (lambda (c s) (format s "A timeout set to ~A seconds occurred." (timeout-length c)))))

(defun %call-with-timeout (seconds function)
  (if (and seconds (green-running-p))
      (let* ((self (sys::current-thread))
             (done nil)
             (timer (sys::make-thread (lambda ()
                                        (sleep seconds)
                                        (unless done
                                          (sys::interrupt-thread
                                           self (lambda () (unless done (error 'timeout :length seconds))))))
                                      :name "timeout")))
        (unwind-protect (funcall function)
          (setq done t)
          (ignore-errors (sys::destroy-thread timer))))
      (funcall function)))

(defmacro with-timeout ((timeout) &body body)
  `(%call-with-timeout ,timeout (lambda () ,@body)))

(in-package :bordeaux-threads)
(defvar *supports-threads-p* (bt2::green-threads-p))

(pushnew :bordeaux-threads *features*)
