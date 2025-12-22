;;;; package.lisp - Package system for Clysm reader

(in-package #:clysm/reader/package)

;;; Package structure
;;; A package is represented as: (:name <name> :symbols <hash-table> :nicknames <list>)

(defvar *packages* (make-hash-table :test 'equal)
  "Table of defined packages.")

(defvar *current-package* nil
  "The current package for symbol interning.")

;;; Internal accessors

(defun package-name* (pkg)
  "Get the name of a package."
  (getf pkg :name))

(defun package-symbols* (pkg)
  "Get the symbols hash table of a package."
  (getf pkg :symbols))

(defun package-nicknames* (pkg)
  "Get the nicknames of a package."
  (getf pkg :nicknames))

;;; Package creation and lookup

(defun make-package* (name &key nicknames)
  "Create a new package with the given name."
  (let ((pkg (list :name name
                   :symbols (make-hash-table :test 'equal)
                   :nicknames nicknames)))
    ;; Register under main name
    (setf (gethash name *packages*) pkg)
    ;; Register under nicknames
    (dolist (nick nicknames)
      (setf (gethash nick *packages*) pkg))
    pkg))

(defun find-package* (name)
  "Find a package by name or nickname."
  (gethash name *packages*))

;;; Symbol interning

(defun intern-symbol (name &optional package)
  "Intern a symbol in a package.
   NAME is a string.
   PACKAGE can be a package object or package name string."
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")
                     (find-package* "CL")))
                ((stringp package)
                 (or (find-package* package)
                     (error "Package not found: ~A" package)))
                (t package)))
         (upcase-name (string-upcase name))
         (symbols (package-symbols* pkg))
         (existing (gethash upcase-name symbols)))
    (if existing
        existing
        ;; Create new symbol
        (let ((new-sym (if (string= (package-name* pkg) "KEYWORD")
                           ;; Keywords are interned in the KEYWORD package
                           (intern upcase-name "KEYWORD")
                           ;; Regular symbols - use CL:INTERN for now
                           ;; In a full implementation, we'd create our own symbol type
                           (intern upcase-name))))
          (setf (gethash upcase-name symbols) new-sym)
          new-sym))))

(defun find-symbol* (name &optional package)
  "Find a symbol in a package without interning.
   Returns NIL if not found."
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")
                     (find-package* "CL")))
                ((stringp package)
                 (find-package* package))
                (t package)))
         (upcase-name (string-upcase name)))
    (when pkg
      (gethash upcase-name (package-symbols* pkg)))))

;;; Initialize standard packages

(defun initialize-packages ()
  "Initialize the standard packages."
  ;; COMMON-LISP package
  (let ((cl-pkg (make-package* "COMMON-LISP" :nicknames '("CL"))))
    ;; Pre-intern standard symbols
    (dolist (sym '("T" "NIL" "QUOTE" "LAMBDA" "DEFUN" "DEFVAR" "DEFPARAMETER"
                   "LET" "LET*" "IF" "PROGN" "BLOCK" "RETURN-FROM"
                   "TAGBODY" "GO" "CATCH" "THROW" "UNWIND-PROTECT"
                   "SETQ" "FUNCTION" "FUNCALL" "APPLY"
                   "CAR" "CDR" "CONS" "LIST" "APPEND"
                   "+" "-" "*" "/" "=" "<" ">" "<=" ">="
                   "EQ" "EQL" "EQUAL" "EQUALP"
                   "NOT" "AND" "OR"
                   "COND" "WHEN" "UNLESS"
                   "FLET" "LABELS"
                   "QUASIQUOTE" "UNQUOTE" "UNQUOTE-SPLICING"))
      (intern-symbol sym cl-pkg)))

  ;; KEYWORD package
  (make-package* "KEYWORD")

  ;; CL-USER package
  (let ((user-pkg (make-package* "CL-USER" :nicknames '("USER"))))
    (setf *current-package* user-pkg))

  ;; CLYSM-USER package
  (make-package* "CLYSM-USER"))

;; Initialize on load
(initialize-packages)
