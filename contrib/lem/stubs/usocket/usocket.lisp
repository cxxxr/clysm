(defpackage :usocket
  (:use :cl)
  (:export #:socket-listen #:socket-connect #:socket-close #:socket-stream
           #:socket-error #:address-in-use-error #:connection-refused-error))
(in-package :usocket)

(define-condition socket-error (error)
  ((operation :initarg :operation :initform nil :reader socket-error-operation))
  (:report (lambda (c s)
             (format s "~@[~A: ~]sockets are not available" (socket-error-operation c)))))
(define-condition address-in-use-error (socket-error) ())
(define-condition connection-refused-error (socket-error) ())

(defun socket-listen (host port &rest args)
  (declare (ignore host port args))
  (error 'socket-error :operation 'socket-listen))

(defun socket-connect (host port &rest args)
  (declare (ignore host port args))
  (error 'socket-error :operation 'socket-connect))

(defun socket-close (socket) (declare (ignore socket)) nil)
(defun socket-stream (socket) (declare (ignore socket)) nil)
