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

;;; preemption: code compiled with interrupt checks gets its turn taken
(setq sys::*interrupt-checks* t)
(defvar *turns* nil)
(defun busy (name ms)
  ;; computes for MS milliseconds without waiting; notes when another ran in between
  (let ((end (+ (get-internal-real-time) ms)) (n 0))
    (loop while (< (get-internal-real-time) end)
          do (incf n)
             (unless (eq (car *turns*) name) (push name *turns*)))
    n))
(setq sys::*interrupt-checks* nil)
(check "threads that compute take turns"
       (let ((a (sys::make-thread (lambda () (busy :a 150))))
             (b (sys::make-thread (lambda () (busy :b 150)))))
         (sys::join-thread a) (sys::join-thread b)
         ;; each had several turns
         (list (> (count :a *turns*) 2) (> (count :b *turns*) 2)))
       '(t t))
(check "a computing thread does not keep a sleeping one from waking"
       (let* ((woke nil)
              (start (get-internal-real-time))
              (sleeper (sys::make-thread (lambda () (sleep 0.02) (setq woke (get-internal-real-time)))))
              (worker (sys::make-thread (lambda () (busy :w 300)))))
         (sys::join-thread sleeper)
         (prog1 (< (- woke start) 150)
           (sys::join-thread worker)))
       t)

;;; what threads wait for without blocking the others
(check "sockets: a server and a client in two threads"
       (let* ((server (sys::open-tcp-server "127.0.0.1" 0))
              (port (sys::tcp-server-port server))
              (ticks 0)
              (ticker (sys::make-thread (lambda () (loop (incf ticks) (sleep 0.005)))))
              (srv (sys::make-thread
                    (lambda ()
                      (let* ((s (sys::accept-tcp-stream server))
                             (buf (make-array 5 :element-type '(unsigned-byte 8))))
                        (read-sequence buf s)
                        (write-sequence (reverse buf) s) (finish-output s) (close s)
                        (coerce buf 'list)))))
              (cli (sys::make-thread
                    (lambda ()
                      (sleep 0.05)
                      (let ((s (sys::open-tcp-stream "127.0.0.1" port))
                            (buf (make-array 5 :element-type '(unsigned-byte 8))))
                        (write-sequence #(1 2 3 4 5) s) (finish-output s)
                        (read-sequence buf s) (close s)
                        (coerce buf 'list))))))
         (prog1 (list (sys::join-thread srv) (sys::join-thread cli) (> ticks 5))
           (sys::destroy-thread ticker)
           (sys::close-tcp-server server)))
       '((1 2 3 4 5) (5 4 3 2 1) t))
(check "a subprocess: the others run while it is waited for"
       (let* ((ticks 0)
              (ticker (sys::make-thread (lambda () (loop (incf ticks) (sleep 0.005)))))
              (out (sys::make-thread
                    (lambda ()
                      (multiple-value-bind (p in out) (sys::%launch-program "sleep 0.2; echo done" :output :stream)
                        (declare (ignore in))
                        (list (read-line out) (sys::%process-wait p)))))))
         (prog1 (list (sys::join-thread out) (> ticks 10))
           (sys::destroy-thread ticker)))
       '(("done" 0) t))
(check "run-program: the others run while it runs"
       (let* ((ticks 0)
              (ticker (sys::make-thread (lambda () (loop (incf ticks) (sleep 0.005)))))
              (out (sys::make-thread
                    (lambda () (multiple-value-list (sys::%run-program-sync "sleep 0.2; cat; echo err >&2" :input "in"))))))
         (prog1 (list (sys::join-thread out) (> ticks 10))
           (sys::destroy-thread ticker)))
       '(("in" "err
" 0) t))
(check "threads compile at the same time (the world lock)"
       (let ((ths (loop for i below 4
                        collect (let ((i i))
                                  (sys::make-thread
                                   (lambda ()
                                     (loop for k below 5
                                           collect (let ((name (intern (format nil "F-~D-~D" i k))))
                                                     (eval `(defun ,name (x) (+ x ,(* 100 i) ,k)))
                                                     (sys::%eval-toplevel `(,name 1))))))))))
         (mapcar #'sys::join-thread ths))
       '((1 2 3 4 5) (101 102 103 104 105) (201 202 203 204 205) (301 302 303 304 305)))

(defgeneric area-of (shape))
(check "threads define classes and methods at the same time"
       (let ((ths (loop for i below 4
                        collect (let ((i i))
                                  (sys::make-thread
                                   (lambda ()
                                     (let ((class (intern (format nil "SHAPE-~D" i))))
                                       (eval `(defclass ,class () ((n :initarg :n :reader shape-n))))
                                       (sleep 0.001)
                                       (eval `(defmethod area-of ((s ,class)) (* (shape-n s) ,i)))
                                       (sleep 0.001)
                                       (area-of (make-instance class :n 10)))))))))
         (mapcar #'sys::join-thread ths))
       '(0 10 20 30))

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
