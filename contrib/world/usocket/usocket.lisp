;;;; usocket for Lem on Clysm: sockets within the image
;;;;
;;;; A page has no sockets.  Lem's Lisp mode talks to micros (Lem's SWANK)
;;;; through a socket, and by default to a micros server in its own image
;;;; (self-connect).  Here a server listens on a port of the image only,
;;;; and a connection to it is a pair of pipes: what one end writes, the
;;;; other reads.  A reader waits (with Clysm's green threads) until
;;;; something is written or the other end is closed.  micros's server
;;;; makes its sockets with these too (contrib/world/sockets.lisp).
;;;;
;;;; A connection can also lead out of the image, to a Lisp in a worker
;;;; (contrib/world/): then the pipe that this end writes hands its octets
;;;; to a function instead of keeping them (MAKE-REMOTE-END), and the name
;;;; of a host says who makes such connections (REGISTER-HOST).

(defpackage :usocket
  (:use :cl)
  (:export #:socket-listen #:socket-accept #:socket-connect #:socket-close #:socket-stream
           #:socket-shutdown #:wait-for-input #:get-local-port
           #:register-host #:unregister-host #:make-remote-end #:offer-connection
           #:usocket #:stream-usocket #:stream-server-usocket #:socket
           #:socket-error #:address-in-use-error #:connection-refused-error))
(in-package :usocket)

(define-condition socket-error (error)
  ((operation :initarg :operation :initform nil :reader socket-error-operation)
   (port :initarg :port :initform nil :reader socket-error-port))
  (:report (lambda (c s)
             (format s "~@[~A: ~]~A~@[ (port ~A)~]" (socket-error-operation c)
                     (socket-error-description c) (socket-error-port c)))))
(defgeneric socket-error-description (condition)
  (:method ((c socket-error)) "socket error"))
(define-condition address-in-use-error (socket-error) ()
  (:report (lambda (c s) (format s "port ~A is in use" (socket-error-port c)))))
(define-condition connection-refused-error (socket-error) ()
  (:report (lambda (c s) (format s "no server on port ~A in this image" (socket-error-port c)))))

;;; A pipe: octets written at one end, read at the other.

(defstruct (pipe (:constructor make-pipe ()))
  (buffer (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (start 0)
  (closed nil)
  ;; of a pipe that leads out of the image: the function of OCTETS, START
  ;; and END that takes what is written, and the one called when it is closed
  (sink nil)
  (on-close nil)
  (mutex (sys::make-mutex :name "pipe"))
  (waitqueue (sys::make-waitqueue :name "pipe")))

(defun pipe-available (pipe)
  (- (fill-pointer (pipe-buffer pipe)) (pipe-start pipe)))

(defun pipe-write (pipe octets start end)
  (sys::with-mutex ((pipe-mutex pipe) :recursive t)
    (when (pipe-closed pipe)
      (error 'socket-error :operation 'write))
    (when (pipe-sink pipe)
      (funcall (pipe-sink pipe) octets start end)
      (return-from pipe-write nil))
    (let ((buffer (pipe-buffer pipe)))
      ;; the octets read are dropped when the buffer is read to its end
      (when (= (pipe-start pipe) (fill-pointer buffer))
        (setf (fill-pointer buffer) 0 (pipe-start pipe) 0))
      (loop for i from start below end
            do (vector-push-extend (aref octets i) buffer)))
    (sys::condition-broadcast (pipe-waitqueue pipe))))

(defun pipe-close (pipe)
  (sys::with-mutex ((pipe-mutex pipe) :recursive t)
    (unless (pipe-closed pipe)
      (setf (pipe-closed pipe) t)
      (when (pipe-on-close pipe)
        (funcall (pipe-on-close pipe))))
    (sys::condition-broadcast (pipe-waitqueue pipe))))

(defun pipe-wait (pipe timeout)
  "Wait until PIPE has octets to read or is closed, or TIMEOUT seconds (NIL:
no limit) have passed; true if it has or is."
  (sys::with-mutex ((pipe-mutex pipe) :recursive t)
    (loop
      (when (or (plusp (pipe-available pipe)) (pipe-closed pipe))
        (return t))
      (when (and timeout (<= timeout 0))
        (return nil))
      (unless (sys::condition-wait (pipe-waitqueue pipe) (pipe-mutex pipe) :timeout timeout)
        (return (or (plusp (pipe-available pipe)) (pipe-closed pipe)))))))

(defun pipe-read (pipe octets start end)
  "Read into OCTETS from START up to END, waiting for at least one octet;
the index after the last one read (START at end of file)."
  (pipe-wait pipe nil)
  (sys::with-mutex ((pipe-mutex pipe) :recursive t)
    (let* ((buffer (pipe-buffer pipe))
           (n (min (- end start) (pipe-available pipe))))
      (replace octets buffer :start1 start :end1 (+ start n) :start2 (pipe-start pipe))
      (incf (pipe-start pipe) n)
      (+ start n))))

;;; The stream of one end of a connection

(defclass pipe-stream (gray:fundamental-binary-input-stream gray:fundamental-binary-output-stream)
  ((in :initarg :in :reader stream-in)
   (out :initarg :out :reader stream-out)
   (open :initform t :accessor stream-open-p)))

(defmethod gray:stream-read-byte ((s pipe-stream))
  (let ((octet (make-array 1 :element-type '(unsigned-byte 8))))
    (if (= (pipe-read (stream-in s) octet 0 1) 0)
        :eof
        (aref octet 0))))

(defmethod gray:stream-read-sequence ((s pipe-stream) sequence start end &key)
  ;; as READ-SEQUENCE does: until END, or end of file
  (let ((end (or end (length sequence))))
    (loop while (< start end)
          do (let ((next (pipe-read (stream-in s) sequence start end)))
               (when (= next start) (return))
               (setf start next)))
    start))

(defmethod gray:stream-write-byte ((s pipe-stream) octet)
  (pipe-write (stream-out s) (vector octet) 0 1)
  octet)

(defmethod gray:stream-write-sequence ((s pipe-stream) sequence start end &key)
  (pipe-write (stream-out s) sequence start (or end (length sequence)))
  sequence)

(defmethod gray:stream-listen ((s pipe-stream))
  (plusp (pipe-available (stream-in s))))

(defmethod gray:stream-finish-output ((s pipe-stream)) nil)
(defmethod gray:stream-force-output ((s pipe-stream)) nil)

(defmethod close ((s pipe-stream) &key abort)
  (declare (ignore abort))
  (when (stream-open-p s)
    (setf (stream-open-p s) nil)
    (pipe-close (stream-out s))
    (pipe-close (stream-in s)))
  t)

(defmethod open-stream-p ((s pipe-stream)) (stream-open-p s))

;;; Sockets

(defclass usocket () ())

(defclass stream-usocket (usocket)
  ((stream :initarg :stream :reader socket-stream)
   (port :initarg :port :reader get-local-port)))

(defclass stream-server-usocket (usocket)
  ((port :initarg :port :reader get-local-port)
   (pending :initform '() :accessor server-pending)
   (mutex :initform (sys::make-mutex :name "server") :reader server-mutex)
   (waitqueue :initform (sys::make-waitqueue :name "server") :reader server-waitqueue)
   (closed :initform nil :accessor server-closed)))

(defun socket (usocket) usocket)

(defvar *servers* (make-hash-table) "port -> the server listening on it")

(defun socket-listen (host port &key reuse-address reuseaddress backlog element-type)
  (declare (ignore host reuse-address reuseaddress backlog element-type))
  (when (zerop port)
    (setf port (loop for p = (+ 49152 (random 16000))
                     unless (gethash p *servers*) return p)))
  (when (gethash port *servers*)
    (error 'address-in-use-error :operation 'socket-listen :port port))
  (setf (gethash port *servers*) (make-instance 'stream-server-usocket :port port)))

(defun socket-accept (server &key element-type)
  "Wait for a connection to SERVER; the socket of its end."
  (declare (ignore element-type))
  (sys::with-mutex ((server-mutex server) :recursive t)
    (loop
      (when (server-closed server)
        (error 'socket-error :operation 'socket-accept :port (get-local-port server)))
      (when (server-pending server)
        (return (pop (server-pending server))))
      (sys::condition-wait (server-waitqueue server) (server-mutex server)))))

(defvar *hosts* (make-hash-table :test 'equal)
  "name of a host outside the image -> the function of a port that connects to it")

(defun register-host (name connector)
  "SOCKET-CONNECT to the host NAME calls CONNECTOR with the port; it returns
the socket (of MAKE-REMOTE-END)."
  (setf (gethash name *hosts*) connector))

(defun unregister-host (name)
  (remhash name *hosts*))

(defun make-remote-end (port send close)
  "The socket of this end of a connection whose other end is outside the
image.  What is written to it is given to SEND (a function of OCTETS, START
and END), and CLOSE is called when it is closed here.  The second value is
a function of a vector of octets that arrived from the other end, the third
a function to call when the other end has closed."
  (let ((in (make-pipe))
        (out (make-pipe)))
    (setf (pipe-sink out) send
          (pipe-on-close out) close)
    (values (make-instance 'stream-usocket
                           :port port
                           :stream (make-instance 'pipe-stream :in in :out out))
            (lambda (octets) (pipe-write in octets 0 (length octets)))
            (lambda ()
              ;; (closed by the other end: CLOSE is not called)
              (setf (pipe-on-close out) nil)
              (pipe-close out)
              (pipe-close in)))))

(defun offer-connection (port socket)
  "Give SOCKET (of MAKE-REMOTE-END) to the server that listens on PORT, as
the connection that its SOCKET-ACCEPT returns; false if there is none."
  (let ((server (gethash port *servers*)))
    (when server
      (sys::with-mutex ((server-mutex server) :recursive t)
        (setf (server-pending server) (append (server-pending server) (list socket)))
        (sys::condition-broadcast (server-waitqueue server)))
      t)))

(defun socket-connect (host port &key element-type timeout nodelay protocol)
  (declare (ignore element-type timeout nodelay protocol))
  (let ((connector (gethash host *hosts*)))
    (when connector
      (return-from socket-connect (funcall connector port))))
  (let ((server (gethash port *servers*)))
    (unless (and server (member host '("127.0.0.1" "localhost" :localhost) :test #'equal))
      (error 'connection-refused-error :operation 'socket-connect :port port))
    (let ((to-server (make-pipe))
          (to-client (make-pipe)))
      (sys::with-mutex ((server-mutex server) :recursive t)
        (setf (server-pending server)
              (append (server-pending server)
                      (list (make-instance 'stream-usocket
                                           :port port
                                           :stream (make-instance 'pipe-stream
                                                                  :in to-server :out to-client)))))
        (sys::condition-broadcast (server-waitqueue server)))
      (make-instance 'stream-usocket
                     :port port
                     :stream (make-instance 'pipe-stream :in to-client :out to-server)))))

(defgeneric socket-close (socket))

(defmethod socket-close ((socket stream-usocket))
  (close (socket-stream socket)))

(defmethod socket-close ((server stream-server-usocket))
  (remhash (get-local-port server) *servers*)
  (sys::with-mutex ((server-mutex server) :recursive t)
    (setf (server-closed server) t)
    (sys::condition-broadcast (server-waitqueue server)))
  t)

(defun socket-shutdown (socket direction)
  (declare (ignore direction))
  (socket-close socket))

(defun wait-for-input (socket-or-sockets &key timeout ready-only)
  "Wait until one of the sockets has input (or is at end of file), or TIMEOUT
seconds have passed; the sockets that have."
  (declare (ignore ready-only))
  (let* ((sockets (if (listp socket-or-sockets) socket-or-sockets (list socket-or-sockets)))
         (ready (lambda ()
                  (remove-if-not (lambda (s)
                                   (let ((pipe (stream-in (socket-stream s))))
                                     (or (plusp (pipe-available pipe)) (pipe-closed pipe))))
                                 sockets))))
    (if (= (length sockets) 1)
        (and (pipe-wait (stream-in (socket-stream (first sockets))) timeout)
             sockets)
        (let ((end (and timeout (+ (get-internal-real-time)
                                   (* timeout internal-time-units-per-second)))))
          (loop
            (let ((r (funcall ready)))
              (when r (return r))
              (when (and end (>= (get-internal-real-time) end)) (return nil))
              (sleep 0.01)))))))
