;;; tests/lib/package.lisp as a lazy library (tests/lib/package.mjs): importing
;;; its modules loads nothing, and what Lisp writes to the standard output goes
;;; to the standard error output.  Its modules are .js files (with a package.json
;;; of "type": "module" beside them), which TypeScript pairs with their .d.ts.
(js:library-options :lazy t :output :error :extension ".js")
