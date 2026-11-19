;;;; Worlds: Clysm images in workers of a page, each with a micros server
;;;; that the page's editor (Lem, cxxxr/lem's frontends/clysm/) connects to.  See README.md here.
(defsystem "clysm-world"
  :description "Clysm images in workers, served by micros"
  :licence "MIT"
  :depends-on ("micros" "usocket")
  :serial t
  :components ((:file "sockets")
               (:file "world")))
