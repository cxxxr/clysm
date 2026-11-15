;;;; tests/threads/run.lisp — green threads (runtime/thread.lisp) and
;;;; Bordeaux Threads on them; the output is compared with run.expected.

(defmacro check (name form expected)
  `(let ((v (handler-case ,form (error (e) (list :error (princ-to-string e))))))
     (format t "~:[FAIL~;ok  ~] ~A~@[ => ~S~]~%" (equal v ,expected) ,name (and (not (equal v ,expected)) v))))

;;; threads take turns; join returns the values
(defvar *log* nil)
(defvar *m* (sys::make-mutex :name "log"))
(defun note (x) (sys::with-mutex (*m*) (push x *log*)))
(check "join returns the values"
       (mapcar #'sys::join-thread
               (loop for i below 3
                     collect (let ((i i))
                               (sys::make-thread (lambda () (dotimes (k 3) (note (list i k)) (sleep 0.005)) (* i 10))))))
       '(0 10 20))
(check "the threads ran interleaved"
       (let ((l (reverse *log*))) (list (length l) (not (equal (mapcar #'first l) '(0 0 0 1 1 1 2 2 2)))))
       '(9 t))
(check "a new thread starts when its creator waits"
       (let* ((order nil)
              (th (sys::make-thread (lambda () (push :thread order)))))
         (push :creator order)
         (sys::join-thread th)
         (reverse order))
       '(:creator :thread))

;;; dynamic bindings are per thread; a new thread sees the global values
(defvar *x* :global)
(check "bindings per thread"
       (let ((*x* :main))
         (let ((th (sys::make-thread (lambda () (list *x* (let ((*x* :thread)) (sleep 0.01) *x*))))))
           (sleep 0.005)
           (list *x* (sys::join-thread th))))
       '(:main (:global :thread)))
(check "handlers of the creator are not those of the thread"
       (let ((seen nil))
         (handler-bind ((error (lambda (c) (declare (ignore c)) (setq seen :creator))))
           (let ((th (sys::make-thread (lambda () (error "in thread")))))
             (list (handler-case (sys::join-thread th) (error () :aborted)) seen))))
       '(:aborted nil))

;;; a lock is held by one thread at a time
(check "mutual exclusion"
       (let ((inside 0) (max 0) (m (sys::make-mutex)))
         (mapc #'sys::join-thread
               (loop repeat 4
                     collect (sys::make-thread
                              (lambda ()
                                (dotimes (k 3)
                                  (sys::with-mutex (m)
                                    (incf inside) (setq max (max max inside))
                                    (sleep 0.002)
                                    (decf inside)))))))
         max)
       1)
(check "grab-mutex without waiting"
       (let* ((m (sys::make-mutex))
              (th (sys::make-thread (lambda () (sys::with-mutex (m) (sleep 0.05))))))
         (sleep 0.01)
         (prog1 (list (sys::grab-mutex m :wait nil) (sys::grab-mutex m :timeout 0.01))
           (sys::join-thread th)))
       '(nil nil))

;;; condition variables: a producer and a consumer
(check "condition variable"
       (let* ((q nil) (m (sys::make-mutex)) (cv (sys::make-waitqueue))
              (consumer (sys::make-thread
                         (lambda ()
                           (let ((got nil))
                             (loop (sys::with-mutex (m)
                                     (loop until q do (sys::condition-wait cv m))
                                     (let ((x (pop q)))
                                       (when (eq x :end) (return (nreverse got)))
                                       (push x got)))))))))
         (dotimes (i 5)
           (sys::with-mutex (m) (setq q (append q (list i))) (sys::condition-notify cv))
           (sys::thread-yield))
         (sys::with-mutex (m) (setq q (append q (list :end))) (sys::condition-notify cv))
         (sys::join-thread consumer))
       '(0 1 2 3 4))
(check "condition-wait times out"
       (let ((m (sys::make-mutex)) (cv (sys::make-waitqueue)))
         (sys::with-mutex (m) (sys::condition-wait cv m :timeout 0.01)))
       nil)
(check "semaphore"
       (let* ((s (sys::make-semaphore))
              (th (sys::make-thread (lambda () (list (sys::wait-on-semaphore s) (sys::wait-on-semaphore s))))))
         (sleep 0.01)
         (sys::signal-semaphore s 2)
         (list (sys::join-thread th) (sys::wait-on-semaphore s :timeout 0.01)))
       '((1 0) nil))

;;; interrupts and destroying
(check "interrupt-thread runs in the thread when it waits"
       (let* ((where nil)
              (th (sys::make-thread (lambda () (sleep 0.05) :done) :name "sleeper")))
         (sleep 0.01)
         (sys::interrupt-thread th (lambda () (setq where (sys::thread-name (sys::current-thread)))))
         (list (sys::join-thread th) where))
       '(:done "sleeper"))
(check "destroy-thread unwinds the thread"
       (let* ((cleaned nil)
              (th (sys::make-thread (lambda () (unwind-protect (loop (sleep 0.01)) (setq cleaned t))))))
         (sleep 0.02)
         (sys::destroy-thread th)
         (list (handler-case (sys::join-thread th) (error () :ended)) cleaned (sys::thread-alive-p th)))
       '(:ended t nil))
(check "join-thread with a timeout"
       (let ((th (sys::make-thread (lambda () (sleep 0.1)))))
         (prog1 (multiple-value-list (sys::join-thread th :timeout 0.01))
           (sys::destroy-thread th)))
       '(nil :timeout))

;;; a computation that does not wait runs to the end before the others
(check "sleep lets the others run"
       (let* ((n 0) (th (sys::make-thread (lambda () (loop repeat 3 do (incf n) (sleep 0.001))))))
         (sleep 0.05)
         (sys::join-thread th)
         n)
       3)
(check "deep recursion in a thread"
       (labels ((deep (n) (if (zerop n) 0 (1+ (deep (1- n))))))
         (sys::join-thread (sys::make-thread (lambda () (deep 200000)))))
       200000)
(check "only the main thread is left" (length (sys::all-threads)) 1)

;;; Bordeaux Threads
(require :quicklisp)
(funcall (read-from-string "ql:quickload") :bordeaux-threads :silent t)
(check "bt: supports threads" (symbol-value (read-from-string "bt:*supports-threads-p*")) t)
(check "bt: make-thread, join-thread, locks, condition variables"
       (eval (read-from-string "
         (let* ((lock (bt:make-lock)) (cv (bt:make-condition-variable)) (box nil)
                (th (bt:make-thread (lambda ()
                                      (bt:with-lock-held (lock)
                                        (loop until box do (bt:condition-wait cv lock))
                                        (list (bt:thread-name (bt:current-thread)) box)))
                                    :name \"waiter\")))
           (bt:thread-yield)
           (bt:with-lock-held (lock) (setq box :filled) (bt:condition-notify cv))
           (bt:join-thread th))"))
       '("waiter" :filled))
(check "bt2: with-timeout"
       (eval (read-from-string "
         (handler-case (bt2:with-timeout (0.02) (loop (sleep 0.005)))
           (bt2:timeout () :timeout))"))
       :timeout)
(check "bt2: semaphores and atomic integers"
       (eval (read-from-string "
         (let* ((s (bt2:make-semaphore)) (a (bt2:make-atomic-integer))
                (ths (loop repeat 3 collect (bt2:make-thread (lambda () (bt2:atomic-integer-incf a) (bt2:signal-semaphore s))))))
           (dotimes (i 3) (bt2:wait-on-semaphore s))
           (mapc #'bt2:join-thread ths)
           (bt2:atomic-integer-value a))"))
       3)
