;;;; The part of usocket that lem/core uses (it comes with dexador): a
;;;; Lisp in a page cannot listen on a port, so SOCKET-LISTEN signals
;;;; SOCKET-ERROR.
(defsystem "usocket"
  :description "The part of usocket used by lem/core (Clysm)"
  :components ((:file "usocket")))
