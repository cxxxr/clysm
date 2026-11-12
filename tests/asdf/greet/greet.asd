(defsystem "greet"
  :version "0.1.0"
  :description "A small system for testing ASDF on Clysm"
  :depends-on ("greet-util")
  :components ((:file "package")
               (:module "src"
                :depends-on ("package")
                :components ((:file "greet"))))
  :in-order-to ((test-op (test-op "greet/test"))))

(defsystem "greet/test"
  :depends-on ("greet")
  :components ((:file "test"))
  :perform (test-op (o c) (uiop:symbol-call :greet-test :run)))
