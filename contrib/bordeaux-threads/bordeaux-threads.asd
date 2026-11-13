;;;; bordeaux-threads for Clysm.  Clysm has one thread, so this is the API of
;;;; Bordeaux Threads (version 1, package BT, and version 2, package BT2) for a
;;;; single thread: locks and condition variables work (a lock is held by the
;;;; one thread, a wait cannot be ended by another thread, so it times out at
;;;; once), and MAKE-THREAD signals an error unless *THREAD-STARTER* says how
;;;; to start a computation.  It is found before the Quicklisp release, which
;;;; does not load on an implementation without threads.
(defsystem "bordeaux-threads"
  :description "Bordeaux Threads for a single thread (Clysm)"
  :licence "MIT"
  :version "0.9.4"
  :depends-on ("alexandria")
  :components ((:file "bordeaux-threads")))
