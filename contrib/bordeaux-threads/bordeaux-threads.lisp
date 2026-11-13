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

(defconstant +supports-threads-p+ nil)

(define-condition bordeaux-threads-error (error) ())

(define-condition not-implemented (bordeaux-threads-error)
  ((operation :initarg :operation :initform nil :reader not-implemented-operation))
  (:report (lambda (c s)
             (format s "~@[~A: ~]Clysm has only one thread"
                     (not-implemented-operation c)))))

(defvar *default-special-bindings* '())
(defvar *standard-io-bindings* '())

;;; ---- threads

(defclass thread ()
  ((name :initarg :name :initform nil :reader thread-name)
   (function :initarg :function :initform nil :reader thread-function)
   (alive :initform t :accessor %thread-alive)
   (values :initform nil :accessor %thread-values)
   (error :initform nil :accessor %thread-error)))

(defmethod print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (format stream "~S" (thread-name thread))))

(defun threadp (object) (typep object 'thread))
(defun thread-native-thread (thread) thread)

(defvar *main-thread* (make-instance 'thread :name "main thread"))
(defvar *current-thread* *main-thread*)
(defvar *threads* (list *main-thread*))

(defvar *thread-starter* nil
  "NIL, or a function called with a function of no arguments by MAKE-THREAD:
it runs the function later in the one thread, e.g. from the event loop of a
page.  Without it, MAKE-THREAD signals NOT-IMPLEMENTED.")

(define-condition abnormal-exit (bordeaux-threads-error)
  ((condition :initarg :condition :initform nil :reader abnormal-exit-condition))
  (:report (lambda (c s)
             (format s "The thread exited abnormally~@[: ~A~]" (abnormal-exit-condition c)))))

(defun make-thread (function &key name (initial-bindings *default-special-bindings*)
                               trap-conditions)
  (declare (ignore trap-conditions))
  (unless *thread-starter*
    (error 'not-implemented :operation 'make-thread))
  (let ((thread (make-instance 'thread :name name :function function)))
    (push thread *threads*)
    (funcall *thread-starter*
             (lambda ()
               (let ((*current-thread* thread))
                 (progv (mapcar #'car initial-bindings)
                     (mapcar (lambda (b) (eval (cdr b))) initial-bindings)
                   (unwind-protect
                        (handler-case (setf (%thread-values thread)
                                            (multiple-value-list (funcall function)))
                          (error (c) (setf (%thread-error thread) c)))
                     (setf (%thread-alive thread) nil)
                     (setf *threads* (remove thread *threads*)))))))
    thread))

(defun current-thread () *current-thread*)
(defun all-threads () (copy-list *threads*))
(defun start-multiprocessing () nil)
(defun thread-alive-p (thread) (%thread-alive thread))
(defun thread-yield () nil)

(defun interrupt-thread (thread function &rest arguments)
  "There is one thread: FUNCTION is called now."
  (when (%thread-alive thread)
    (apply function arguments))
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
  (setf (%thread-alive thread) nil
        *threads* (remove thread *threads*))
  thread)

(defun join-thread (thread)
  (cond ((%thread-alive thread)
         (error 'not-implemented :operation 'join-thread))
        ((%thread-error thread)
         (error 'abnormal-exit :condition (%thread-error thread)))
        (t (values-list (%thread-values thread)))))

;;; ---- locks: one thread holds them; a recursive lock counts

(defclass lock ()
  ((name :initarg :name :initform nil :reader lock-name)
   (owner :initform nil :accessor lock-owner)))
(defclass recursive-lock (lock)
  ((count :initform 0 :accessor lock-count)))

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
  (declare (ignore wait timeout))
  (cond ((lock-owner lock) nil)         ; held: it would wait for ever
        (t (setf (lock-owner lock) *current-thread*) t)))
(defun bt::acquire-lock (lock &optional (wait-p t))
  (acquire-lock lock :wait wait-p))

(defun release-lock (lock)
  (setf (lock-owner lock) nil)
  lock)

(defun acquire-recursive-lock (lock &key (wait t) timeout)
  (declare (ignore wait timeout))
  (incf (lock-count lock))
  (setf (lock-owner lock) *current-thread*)
  t)
(defun bt::acquire-recursive-lock (lock) (acquire-recursive-lock lock))

(defun release-recursive-lock (lock)
  (when (zerop (decf (lock-count lock)))
    (setf (lock-owner lock) nil))
  lock)

;;; A lock that is held is entered again: with one thread, the holder is
;;; the code around (e.g. a handler or an interrupt function running inside
;;; the dynamic extent of WITH-LOCK-HELD), which waiting would deadlock.
(defmacro with-lock-held ((place &key timeout) &body body)
  (declare (ignore timeout))
  (let ((lock (gensym "LOCK")) (owner (gensym "OWNER")))
    `(let* ((,lock ,place)
            (,owner (lock-owner ,lock)))
       (setf (lock-owner ,lock) *current-thread*)
       (unwind-protect (progn ,@body)
         (setf (lock-owner ,lock) ,owner)))))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  (declare (ignore timeout))
  (let ((lock (gensym "LOCK")))
    `(let ((,lock ,place))
       (acquire-recursive-lock ,lock)
       (unwind-protect (progn ,@body)
         (release-recursive-lock ,lock)))))

;;; ---- condition variables: nothing can notify while the one thread waits

(defclass condition-variable ()
  ((name :initarg :name :initform nil)))
(defun condition-variable-p (object) (typep object 'condition-variable))
(defun make-condition-variable (&key name) (make-instance 'condition-variable :name name))

(defun condition-wait (condition-variable lock &key timeout)
  (declare (ignore condition-variable lock))
  (if timeout
      nil
      (error 'not-implemented :operation 'condition-wait)))

(defun condition-notify (condition-variable) (declare (ignore condition-variable)) nil)
(defun condition-broadcast (condition-variable) (declare (ignore condition-variable)) nil)

;;; ---- semaphores

(defclass semaphore ()
  ((name :initarg :name :initform nil)
   (count :initarg :count :initform 0 :accessor semaphore-count)))
(defun semaphorep (object) (typep object 'semaphore))
(defun bt::semaphore-p (object) (typep object 'semaphore))
(defun make-semaphore (&key name (count 0))
  (make-instance 'semaphore :name name :count count))

(defun signal-semaphore (semaphore &key (count 1))
  (incf (semaphore-count semaphore) count)
  nil)

(defun wait-on-semaphore (semaphore &key timeout)
  (cond ((plusp (semaphore-count semaphore))
         (decf (semaphore-count semaphore))
         t)
        (timeout nil)
        (t (error 'not-implemented :operation 'wait-on-semaphore))))

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

;;; ---- timeouts: the body cannot be interrupted, so it runs to the end

(define-condition timeout (serious-condition)
  ((length :initarg :length :initform nil :reader timeout-length))
  (:report (lambda (c s) (format s "A timeout set to ~A seconds occurred." (timeout-length c)))))

(defmacro with-timeout ((timeout) &body body)
  (declare (ignore timeout))
  `(progn ,@body))

(in-package :bordeaux-threads)
(defvar *supports-threads-p* nil)

(pushnew :bordeaux-threads *features*)
