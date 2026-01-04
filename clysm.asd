;;;; clysm.asd - Clysm System Definition
;;;;
;;;; Common Lisp to WebAssembly GC Compiler
;;;; No external dependencies - designed for self-hosting

(asdf:defsystem #:clysm
  :description "Common Lisp to WebAssembly GC Compiler"
  :author "Clysm Authors"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()  ; No external dependencies - self-hosting ready
  :serial t
  :pathname "src/clysm"
  :components
  ((:file "package")
   (:file "util")
   (:file "conditions")
   (:module "backend"
    :components
    ((:file "leb128")
     (:file "wasm-types")
     (:file "sections")
     (:file "wasm-emit")
     (:file "wat-print")))
   (:module "runtime"
    :components
    ((:file "types")))))

(asdf:defsystem #:clysm/tests
  :description "Clysm Test Suite"
  :depends-on (#:clysm)
  :serial t
  :pathname "tests"
  :components
  ((:file "package")
   (:file "framework")
   (:file "runner")
   (:module "unit"
    :components
    ((:file "util-test")
     (:file "leb128-test")
     (:file "wasm-types-test")
     (:file "runtime-types-test")))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system '#:clysm))))
  (asdf:load-system '#:clysm/tests)
  (uiop:symbol-call '#:clysm/tests '#:run-all-tests))
