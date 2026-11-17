;;;; setup.lisp — make Lem's systems loadable by ASDF on Clysm (after
;;;; (require :asdf)): the stubs of the libraries that Lem on Clysm does
;;;; without, the system lem-clysm, and the fixups of Lem's system
;;;; definitions.  The directories of Lem itself and of the libraries come
;;;; from the caller (prepare.lisp, or the registry that it writes).

(in-package :asdf)

(let ((here (uiop:pathname-directory-pathname *load-truename*)))
  (dolist (stub '("micros" "log4cl" "dexador" "usocket" "deploy" "lem-tree-sitter"))
    (pushnew (merge-pathnames (format nil "stubs/~A/" stub) here) *central-registry*
             :test #'equal))
  (pushnew here *central-registry* :test #'equal))

;;; The extension manager installs packages with Quicklisp at run time; it
;;; is not part of Lem on Clysm.
(define-clysm-fixup "lem/core" (s)
  (clysm-drop-dependencies s "lem-extension-manager")
  (clysm-enable-component s '("ext" "extension-commands") nil))

;;; Lem's extensions that Lem on Clysm loads when they are first used
;;; (src/extensions.lisp): their language servers (lsp-config), tree-sitter
;;; (stubs/lem-tree-sitter/), subprocesses and Lem's own Lisp mode (here
;;; the Lisp mode of src/lisp-mode.lisp) are left out.
(dolist (name '("lem-js-mode" "lem-go-mode" "lem-typescript-mode" "lem-yaml-mode" "lem-python-mode"))
  (eval `(define-clysm-fixup ,name (s)
           (clysm-drop-dependencies s "lem-lsp-mode" "lem-process")
           (clysm-enable-component s '("lsp-config") nil))))

(dolist (name '("lem-c-mode" "lem-rust-mode"))
  (eval `(define-clysm-fixup ,name (s)
           (clysm-drop-dependencies s "lem-lisp-mode")
           (clysm-patch-component s '(,(if (equal name "lem-c-mode") "c-mode" "rust-mode"))
                                  "lem-lisp-mode:lisp-indent-sexp"
                                  "lem-clysm/lisp-mode::lisp-indent-sexp"))))

(define-clysm-fixup "lem-markdown-mode" (s)
  ;; no preview (a web server) and no evaluation of code blocks
  (clysm-drop-dependencies s "3bmd" "3bmd-ext-code-blocks" "lisp-preprocessor" "trivial-ws"
                           "trivial-open-browser" "lem-lisp-mode" "lem-elisp-mode")
  (clysm-enable-component s '("preview") nil)
  (clysm-enable-component s '("interactive") nil)
  (clysm-patch-component s '("languages")
                         "lem-lisp-mode:lisp-mode)
    (\"lisp\" . lem-lisp-mode:lisp-mode)
    (\"emacs-lisp\" . lem-elisp-mode:elisp-mode)"
                         "lem-clysm/lisp-mode:lisp-mode)
    (\"lisp\" . lem-clysm/lisp-mode:lisp-mode)")
  (clysm-patch-component s '("markdown-mode")
                         "(lem-lsp-mode:without-lsp-mode ()" "(progn"))

(define-clysm-fixup "lem-vi-mode" (s)
  ;; the keys of the debugger and the inspector of Lem's Lisp mode, and of
  ;; the dashboard
  (clysm-drop-dependencies s "lem-lisp-mode" "lem-dashboard")
  (clysm-enable-component s '("special-binds") nil))

;;; The colors of the parentheses of Lem's Lisp mode, which is not loaded
(define-clysm-fixup "lem-base16-themes" (s)
  (clysm-patch-component s '("src" "macros")
                         "(lem-lisp-mode/paren-coloring:paren-color-1 :foreground ,(color :base08))
       (lem-lisp-mode/paren-coloring:paren-color-2 :foreground ,(color :base09))
       (lem-lisp-mode/paren-coloring:paren-color-3 :foreground ,(color :base0A))
       (lem-lisp-mode/paren-coloring:paren-color-4 :foreground ,(color :base0B))
       (lem-lisp-mode/paren-coloring:paren-color-5 :foreground ,(color :base0C))
       (lem-lisp-mode/paren-coloring:paren-color-6 :foreground ,(color :base0D))"
                         ""))
