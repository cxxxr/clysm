;;;; sockets.lisp — the sockets of micros (Lem's SWANK) in a page or a worker
;;;;
;;;; An image in a page or in a worker has no sockets: micros listens and
;;;; accepts with the sockets of contrib/lem/stubs/usocket/, which connect
;;;; within the image (pipes) or to another image through the messages of a
;;;; worker (world.lisp), and its threads are Clysm's green threads.
;;;; micros's backend for Clysm is contrib/micros/clysm.lisp.

(in-package :micros/backend)

(defimplementation preferred-communication-style ()
  :spawn)

(defimplementation create-socket (host port &key backlog)
  (declare (ignore backlog))
  (usocket:socket-listen host port))

(defimplementation local-port (socket)
  (usocket:get-local-port socket))

(defimplementation close-socket (socket)
  (usocket:socket-close socket))

(defimplementation accept-connection (socket &key external-format buffering timeout)
  (declare (ignore buffering timeout))
  (when external-format
    (error "The connections of micros in Lem on Clysm are binary (external format ~S)"
           external-format))
  (usocket:socket-stream (usocket:socket-accept socket)))

(defimplementation wait-for-input (streams &optional timeout)
  (loop
    (let ((ready (remove-if-not (lambda (s)
                                  (or (ignore-errors (listen s))
                                      (not (open-stream-p s))))
                                streams)))
      (when ready (return ready)))
    (when timeout (return nil))
    (sleep 0.05)))

;;; micros's threads see the global values of the image, whose package may
;;; be that of the UI framework: the Lisp mode starts in CL-USER.
(pushnew (cons '*package* (find-package "COMMON-LISP-USER"))
         micros::*default-worker-thread-bindings*
         :key #'car)
