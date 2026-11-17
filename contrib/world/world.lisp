;;;; world.lisp — worlds: Clysm images in workers of the page
;;;;
;;;; A world is an image of its own in a worker (worker.js), with its own
;;;; heap and its own thread of the computer.  It runs a micros server, and
;;;; the page connects to it as to a host of the world's name: what the
;;;; editor's Lisp mode evaluates is then compiled and run there, so a
;;;; computation that never ends, or one that breaks the image, leaves the
;;;; editor as it is, and KILL-WORLD ends it whatever it does.
;;;;
;;;; Both sides are here.  The page: START-WORLD, KILL-WORLD.  The worker:
;;;; SERVE, called by boot.lisp.  Between them go the messages of the
;;;; worker, objects with a "type":
;;;;   to the world:  boot {module, root, args} (worker.js), connect {id, port}
;;;;   to the page:   ready {port}, failed {message}, output {fd, text},
;;;;                  refused {id}
;;;;   both ways:     data {id, octets}, close {id}
;;;; A connection is the socket of usocket:make-remote-end at each end,
;;;; named by the id that the page chose.

(defpackage :clysm-world
  (:use :cl)
  (:export #:start-world #:kill-world #:find-world #:worlds
           #:world #:world-name #:world-state #:world-port #:world-seconds #:world-failure
           #:*worker-script* #:*boot-file* #:*output-function*
           #:serve))
(in-package :clysm-world)

;;; A link: one side of the messages between a page and a worker.

(defstruct link
  target                               ; what has postMessage: the Worker, or the worker's `self`
  (channels (make-hash-table))         ; id -> (deliver . closed), the functions of make-remote-end
  (handler nil))                       ; function of the type and the data of the other messages

(defun post (link type &rest properties)
  (js:call (link-target link) "postMessage" (apply #'js:object "type" type properties)))

(defun open-channel (link id port)
  "The socket of this end of connection ID."
  (multiple-value-bind (socket deliver closed)
      (usocket:make-remote-end
       port
       (lambda (octets start end)
         (post link "data" "id" id "octets" (js:from-octets octets :start start :end end)))
       (lambda ()
         (remhash id (link-channels link))
         (post link "close" "id" id)))
    (setf (gethash id (link-channels link)) (cons deliver closed))
    socket))

(defun close-channel (link id)
  "Connection ID was closed by the other end."
  (let ((channel (gethash id (link-channels link))))
    (when channel
      (remhash id (link-channels link))
      (funcall (cdr channel)))))

(defun close-channels (link)
  (loop for id being the hash-keys of (link-channels link)
        collect id into ids
        finally (mapc (lambda (id) (close-channel link id)) ids)))

(defun receive (link event)
  (let* ((data (js:get event "data"))
         (type (js:get data "type")))
    (cond ((equal type "data")
           (let ((channel (gethash (round (js:get data "id")) (link-channels link))))
             (when channel
               (funcall (car channel) (js:to-octets (js:get data "octets"))))))
          ((equal type "close")
           (close-channel link (round (js:get data "id"))))
          ((link-handler link)
           (funcall (link-handler link) type data)))))

(defun listen-to (link)
  "Messages that arrive at LINK's target are received by LINK."
  (js:call (link-target link) "addEventListener" "message"
           ;; (not a computation of its own: the octets of a connection
           ;; are delivered in the order of the messages)
           (lambda (event)
             (handler-case (receive link event)
               (error (e) (js:call (js:global "console") "error" (format nil "world: ~A" e))))
             nil)))

;;; ---- the page

(defvar *worker-script* "world-worker.js"
  "The URL of worker.js, relative to the page.")

(defvar *boot-file* nil
  "The file that a world loads (boot.lisp); the application sets it.")

(defvar *output-function*
  (lambda (world text)
    (js:call (js:global "console") "log" (format nil "[~A] ~A" (world-name world) text)))
  "Called with a world and a line that it wrote to its standard output.")

(defclass world ()
  ((name :initarg :name :reader world-name)
   (worker :initarg :worker :reader world-worker)
   (link :initarg :link :reader world-link)
   ;; :starting, :ready, :failed or :dead
   (state :initform :starting :accessor world-state)
   (port :initform nil :accessor world-port
         :documentation "the port of its micros server")
   (seconds :initform nil :accessor world-seconds
            :documentation "how long it took to start")
   (failure :initform nil :accessor world-failure)
   (next-id :initform 0 :accessor world-next-id)))

(defmethod print-object ((world world) stream)
  (print-unreadable-object (world stream :type t)
    (format stream "~A ~(~A~)" (world-name world) (world-state world))))

(defvar *worlds* '())
(defvar *world-count* 0)

(defun worlds ()
  "The worlds that were not killed."
  (copy-list *worlds*))

(defun find-world (name)
  (find name *worlds* :key #'world-name :test #'equal))

(defun connect-to-world (world port)
  (unless (eq (world-state world) :ready)
    (error 'usocket:connection-refused-error :operation 'usocket:socket-connect :port port))
  (let ((id (incf (world-next-id world))))
    (prog1 (open-channel (world-link world) id port)
      (post (world-link world) "connect" "id" id "port" port))))

(defun start-world (&key name on-ready on-failure (boot-file *boot-file*))
  "Start a world: a worker that loads the image of this page and BOOT-FILE.
It is :STARTING at first.  When its micros server listens it is :READY and
ON-READY is called with it: (usocket:socket-connect NAME (world-port world))
then connects to the server.  If it cannot start it is :FAILED, and
ON-FAILURE is called with it."
  (let* ((name (or name (format nil "world-~D" (incf *world-count*))))
         (worker (js:new (js:global "Worker") *worker-script*))
         (link (make-link :target worker))
         (world (make-instance 'world :name name :worker worker :link link))
         (started (get-internal-real-time))
         (failed (lambda (message)
                   (when (member (world-state world) '(:starting :ready))
                     (setf (world-state world) :failed
                           (world-failure world) message)
                     (usocket:unregister-host name)
                     (close-channels link)
                     (when on-failure (funcall on-failure world))))))
    (when (find-world name)
      (error "There is a world named ~A" name))
    (setf (link-handler link)
          (lambda (type data)
            (cond ((equal type "ready")
                   (setf (world-port world) (round (js:get data "port"))
                         (world-seconds world) (/ (- (get-internal-real-time) started)
                                                  (float internal-time-units-per-second))
                         (world-state world) :ready)
                   (usocket:register-host name (lambda (port) (connect-to-world world port)))
                   (when on-ready (funcall on-ready world)))
                  ((equal type "output")
                   (funcall *output-function* world (js:get data "text")))
                  ((equal type "refused")
                   (close-channel link (round (js:get data "id"))))
                  ((equal type "failed")
                   (funcall failed (js:get data "message"))))))
    (listen-to link)
    (js:call worker "addEventListener" "error"
             (lambda (event)
               (funcall failed (or (ignore-errors (js:get event "message")) "the worker failed"))
               nil))
    (push world *worlds*)
    (post link "boot"
          ;; the compiled module of this page's image, which the worker
          ;; then does not compile again
          "module" (js:global "__clysm_module")
          "root" (js:global "__clysm_root")
          "args" (vector boot-file name))
    world))

(defun kill-world (world)
  "End WORLD at once, whatever it is doing; its connections are closed."
  (unless (eq (world-state world) :dead)
    (setf (world-state world) :dead)
    (js:call (world-worker world) "terminate")
    (usocket:unregister-host (world-name world))
    (setf *worlds* (remove world *worlds*))
    (close-channels (world-link world)))
  world)

;;; ---- the worker

(defun serve (&key (port 4005))
  "In a worker: start the micros server and tell the page that the world is
ready.  Connections arrive as messages of the page."
  (let* ((self (js:global "self"))
         (link (make-link :target self)))
    (setf (link-handler link)
          (lambda (type data)
            (when (equal type "connect")
              (let* ((id (round (js:get data "id")))
                     (port (round (js:get data "port")))
                     (socket (open-channel link id port)))
                (unless (usocket:offer-connection port socket)
                  (remhash id (link-channels link))
                  (post link "refused" "id" id))))))
    (listen-to link)
    (let ((micros::*swank-debug-p* nil))
      (micros:create-server :port port :style :spawn :dont-close t))
    (post link "ready" "port" port)
    port))
