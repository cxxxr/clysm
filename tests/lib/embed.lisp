;;; tests/lib/package.lisp as a library whose modules hold its module (:EMBED), as a page
;;; loads it (host/web/lib.mjs, of no module of Node.js), and lazy.
(js:library-options :lazy t :embed t :output :error :extension ".js")
