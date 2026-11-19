;;;; usocket for a Lisp in a page or a worker, which has no sockets: the
;;;; part that Lem and micros use, with sockets that connect within the
;;;; image or to a world (usocket.lisp).  Registered by an application that
;;;; runs in a page (Lem's frontends/clysm/setup.lisp), not by Clysm: on
;;;; Node.js the real usocket works.
(defsystem "usocket"
  :description "usocket within a page's image and its worlds (Clysm)"
  :components ((:file "usocket")))
