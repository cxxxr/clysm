;;;; setup.lisp — make Lem's systems loadable by ASDF on Clysm (after
;;;; (require :asdf)): the stubs of the libraries that Lem on Clysm does
;;;; without, the system lem-clysm, and the fixups of Lem's system
;;;; definitions.  The directories of Lem itself and of the libraries come
;;;; from the caller (prepare.lisp, or the registry that it writes).

(in-package :asdf)

(let ((here (uiop:pathname-directory-pathname *load-truename*)))
  (dolist (stub '("log4cl" "dexador" "usocket" "deploy" "lem-tree-sitter" "lem-process"))
    (pushnew (merge-pathnames (format nil "stubs/~A/" stub) here) *central-registry*
             :test #'equal))
  ;; worlds (contrib/world/)
  (pushnew (merge-pathnames "world/" (uiop:pathname-parent-directory-pathname here)) *central-registry* :test #'equal)
  (pushnew here *central-registry* :test #'equal))

;;; The extension manager installs packages with Quicklisp at run time; it
;;; is not part of Lem on Clysm.  micros (Lem's SWANK) is loaded with the
;;; Lisp mode, when it is first used (src/extensions.lisp): Lem's core only
;;; asks it for the name of a function (a Lem that names its timers without
;;; micros needs no patch).
(define-clysm-fixup "lem/core" (s)
  (clysm-drop-dependencies s "lem-extension-manager" "micros")
  (clysm-enable-component s '("ext" "extension-commands") nil)
  (when (search "(micros::function-name function)"
                (uiop:read-file-string
                 (component-pathname (clysm-find-component s '("common" "timer")))))
    (clysm-patch-component s '("common" "timer")
                           "(micros::function-name function)"
                           "(nth-value 2 (function-lambda-expression function))")))

;;; Lem's Lisp mode: micros, the server it talks to, runs in Lem's own
;;; image (src/micros.lisp); there are no other Lisp processes to start
;;; (stubs/lem-process/) and no language server.
(define-clysm-fixup "lem-lisp-mode" (s)
  (clysm-drop-dependencies s "lem-lsp-mode"))

;;; Lem's extensions that Lem on Clysm loads when they are first used
;;; (src/extensions.lisp): their language servers (lsp-config), tree-sitter
;;; (stubs/lem-tree-sitter/) and subprocesses are left out.
(dolist (name '("lem-js-mode" "lem-go-mode" "lem-typescript-mode" "lem-yaml-mode" "lem-python-mode"))
  (eval `(define-clysm-fixup ,name (s)
           (clysm-drop-dependencies s "lem-lsp-mode" "lem-process")
           (clysm-enable-component s '("lsp-config") nil))))

(define-clysm-fixup "lem-markdown-mode" (s)
  ;; no preview (a web server), no Emacs Lisp
  (clysm-drop-dependencies s "3bmd" "3bmd-ext-code-blocks" "lisp-preprocessor" "trivial-ws"
                           "trivial-open-browser" "lem-elisp-mode")
  (clysm-enable-component s '("preview") nil)
  (clysm-patch-component s '("languages")
                         "
    (\"emacs-lisp\" . lem-elisp-mode:elisp-mode)"
                         "")
  (clysm-patch-component s '("markdown-mode")
                         "(lem-lsp-mode:without-lsp-mode ()" "(progn"))

(define-clysm-fixup "lem-vi-mode" (s)
  ;; (the keys of the dashboard, which is not there)
  (clysm-drop-dependencies s "lem-dashboard")
  (clysm-enable-component s '("special-binds") nil))

;;; The colors of the parentheses of Lem's Lisp mode, which is loaded when
;;; it is first used, maybe after the themes
(define-clysm-fixup "lem-base16-themes" (s)
  (clysm-patch-component s '("src" "macros")
                         "(lem-lisp-mode/paren-coloring:paren-color-1 :foreground ,(color :base08))
       (lem-lisp-mode/paren-coloring:paren-color-2 :foreground ,(color :base09))
       (lem-lisp-mode/paren-coloring:paren-color-3 :foreground ,(color :base0A))
       (lem-lisp-mode/paren-coloring:paren-color-4 :foreground ,(color :base0B))
       (lem-lisp-mode/paren-coloring:paren-color-5 :foreground ,(color :base0C))
       (lem-lisp-mode/paren-coloring:paren-color-6 :foreground ,(color :base0D))"
                         ""))
