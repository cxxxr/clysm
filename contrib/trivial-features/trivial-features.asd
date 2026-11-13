;;;; trivial-features for Clysm: *FEATURES* already has :UNIX and :LINUX
;;;; (the Node.js host); this system adds the byte order.  It is found before
;;;; the Quicklisp release, which has no Clysm support.
(defsystem "trivial-features"
  :description "Ensures consistent *FEATURES* across multiple CLs (Clysm)"
  :licence "MIT"
  :components ((:file "tf-clysm")))
