;;;; setup.lisp — make Lem's systems loadable by ASDF on Clysm (after
;;;; (require :asdf)): the stubs of the libraries that Lem on Clysm does
;;;; without, and the system lem-clysm.  The directories of Lem itself and
;;;; of the libraries come from the caller (prepare.lisp, or the registry
;;;; that it writes).
;;;;
;;;; Lem (cxxxr/lem, branch staging) loads on Clysm as it is: what Lem on
;;;; Clysm does without — the extension manager, the DeepL commands, the
;;;; language servers, the previews of Markdown, the dashboard — is in
;;;; systems of their own that lem-clysm does not load, so its system
;;;; definitions need no fixups.  The stubs stand for libraries that need a
;;;; native library or the network (tree-sitter, subprocesses, sockets, an
;;;; HTTP client) and for deploy, which lem.asd loads.

(in-package :asdf)

(let ((here (uiop:pathname-directory-pathname *load-truename*)))
  (dolist (stub '("log4cl" "dexador" "usocket" "deploy" "lem-tree-sitter" "lem-process"))
    (pushnew (merge-pathnames (format nil "stubs/~A/" stub) here) *central-registry*
             :test #'equal))
  ;; worlds (contrib/world/)
  (pushnew (merge-pathnames "world/" (uiop:pathname-parent-directory-pathname here)) *central-registry* :test #'equal)
  (pushnew here *central-registry* :test #'equal))
