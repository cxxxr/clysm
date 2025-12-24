;;;; package.lisp - Package system for Clysm reader
;;;; Extended for ANSI Common Lisp compliance (013-package-system)

(in-package #:clysm/reader/package)

;;; ============================================================
;;; Package Error Condition (T009)
;;; Note: Named clysm-package-error to avoid conflict with CL:PACKAGE-ERROR
;;; ============================================================

(define-condition clysm-package-error (error)
  ((pkg-name :initarg :package :reader clysm-package-error-package :initform nil)
   (message :initarg :message :reader clysm-package-error-message))
  (:report (lambda (c s)
             (format s "Package error~@[ in ~A~]: ~A"
                     (clysm-package-error-package c)
                     (clysm-package-error-message c)))))

;;; ============================================================
;;; Package Structure (T006-T007)
;;; Extended structure with internal/external symbol tables
;;; ============================================================

;;; Package structure:
;;; (:name <string>
;;;  :nicknames <list of strings>
;;;  :internal-symbols <hash-table>  ; Symbols internal to this package
;;;  :external-symbols <hash-table>  ; Exported symbols
;;;  :use-list <list of packages>    ; Packages this package uses
;;;  :used-by-list <list of packages> ; Packages that use this package
;;;  :shadowing-symbols <list of symbols>) ; Shadowing symbols

(defvar *packages* (make-hash-table :test 'equal)
  "Table of defined packages.")

(defvar *current-package* nil
  "The current package for symbol interning (*package*).")

;;; ============================================================
;;; Internal Accessors (T006-T007)
;;; ============================================================

(defun package-name* (pkg)
  "Get the name of a package."
  (getf pkg :name))

(defun package-nicknames* (pkg)
  "Get the nicknames of a package."
  (getf pkg :nicknames))

(defun package-internal-symbols* (pkg)
  "Get the internal symbols hash table of a package."
  (getf pkg :internal-symbols))

(defun package-external-symbols* (pkg)
  "Get the external symbols hash table of a package."
  (getf pkg :external-symbols))

(defun package-use-list* (pkg)
  "Get the use-list of a package (packages this package uses)."
  (getf pkg :use-list))

(defun package-used-by-list* (pkg)
  "Get the used-by-list of a package (packages that use this package)."
  (getf pkg :used-by-list))

(defun package-shadowing-symbols* (pkg)
  "Get the shadowing symbols list of a package."
  (getf pkg :shadowing-symbols))

;;; Backward compatibility: package-symbols* returns internal-symbols
(defun package-symbols* (pkg)
  "Get the internal symbols hash table of a package.
   Deprecated: use package-internal-symbols* instead."
  (package-internal-symbols* pkg))

;;; ============================================================
;;; Setters for mutable fields
;;; ============================================================

(defun (setf package-use-list*) (value pkg)
  "Set the use-list of a package."
  (setf (getf pkg :use-list) value))

(defun (setf package-used-by-list*) (value pkg)
  "Set the used-by-list of a package."
  (setf (getf pkg :used-by-list) value))

(defun (setf package-shadowing-symbols*) (value pkg)
  "Set the shadowing-symbols of a package."
  (setf (getf pkg :shadowing-symbols) value))

(defun (setf package-nicknames*) (value pkg)
  "Set the nicknames of a package."
  (setf (getf pkg :nicknames) value))

;;; ============================================================
;;; Package Designator Conversion (T008)
;;; ============================================================

(defun packagep* (obj)
  "Check if OBJ is a package object."
  (and (listp obj)
       (getf obj :name)
       (hash-table-p (getf obj :internal-symbols))))

(defun symbol-package* (symbol)
  "Return the home package of SYMBOL (T104).
   For Clysm symbols, this searches all packages to find which one
   contains the symbol in its internal or external table.
   Returns NIL if the symbol is uninterned."
  (when (symbolp symbol)
    (let ((name (symbol-name symbol)))
      ;; Search all packages for this symbol
      (maphash (lambda (pkg-name pkg)
                 (declare (ignore pkg-name))
                 ;; Check external symbols
                 (let ((ext (gethash name (package-external-symbols* pkg))))
                   (when (eq ext symbol)
                     (return-from symbol-package* pkg)))
                 ;; Check internal symbols
                 (let ((int (gethash name (package-internal-symbols* pkg))))
                   (when (eq int symbol)
                     (return-from symbol-package* pkg))))
               *packages*)
      nil)))

(defun package-designator-to-package (designator)
  "Convert a package designator to a package object.
   DESIGNATOR can be:
   - A package object (returned as-is)
   - A string (package name)
   - A keyword (converted to uppercase string)
   - A symbol (symbol-name used as package name)
   Signals PACKAGE-ERROR if package not found."
  (cond
    ;; Already a package object
    ((packagep* designator) designator)
    ;; String name
    ((stringp designator)
     (or (find-package* designator)
         (error 'clysm-package-error
                :package designator
                :message (format nil "Package ~A does not exist" designator))))
    ;; Keyword - convert to uppercase string
    ((keywordp designator)
     (let ((name (string-upcase (symbol-name designator))))
       (or (find-package* name)
           (error 'clysm-package-error
                  :package name
                  :message (format nil "Package ~A does not exist" name)))))
    ;; Symbol - use symbol name
    ((symbolp designator)
     (let ((name (string-upcase (symbol-name designator))))
       (or (find-package* name)
           (error 'clysm-package-error
                  :package name
                  :message (format nil "Package ~A does not exist" name)))))
    (t
     (error 'clysm-package-error
            :message (format nil "Invalid package designator: ~S" designator)))))

;;; ============================================================
;;; Package Creation and Lookup
;;; ============================================================

(defun make-package* (name &key nicknames use)
  "Create a new package with the given name.
   NAME is a string.
   NICKNAMES is a list of alternate name strings.
   USE is a list of package designators to use.
   Returns the newly created package."
  (let ((existing (find-package* name)))
    (when existing
      (error 'clysm-package-error
             :package name
             :message "Package already exists")))
  (let ((pkg (list :name name
                   :nicknames nicknames
                   :internal-symbols (make-hash-table :test 'equal)
                   :external-symbols (make-hash-table :test 'equal)
                   :use-list nil
                   :used-by-list nil
                   :shadowing-symbols nil)))
    ;; Register under main name
    (setf (gethash name *packages*) pkg)
    ;; Register under nicknames
    (dolist (nick nicknames)
      (setf (gethash nick *packages*) pkg))
    ;; Process :use
    (when use
      (dolist (used-pkg-designator use)
        (let ((used-pkg (package-designator-to-package used-pkg-designator)))
          (push used-pkg (getf pkg :use-list))
          (push pkg (getf used-pkg :used-by-list)))))
    pkg))

(defun find-package* (name)
  "Find a package by name or nickname.
   NAME can be a string or symbol.
   Returns the package or NIL if not found."
  (let ((name-str (if (stringp name)
                      name
                      (string-upcase (symbol-name name)))))
    (gethash name-str *packages*)))

;;; ============================================================
;;; Package Management Functions (T020-T022)
;;; ============================================================

(defun delete-package* (designator)
  "Delete a package.
   DESIGNATOR can be a package object, string, or keyword.
   Returns T if deleted, NIL if package not found."
  (let* ((pkg (if (packagep* designator)
                  designator
                  (find-package* (if (stringp designator)
                                     designator
                                     (string-upcase (symbol-name designator))))))
         (name (when pkg (package-name* pkg)))
         (nicknames (when pkg (package-nicknames* pkg))))
    (when pkg
      ;; Remove from *packages* hash table
      (remhash name *packages*)
      ;; Remove nicknames
      (dolist (nick nicknames)
        (remhash nick *packages*))
      ;; Remove from used-by-list of used packages
      (dolist (used-pkg (package-use-list* pkg))
        (setf (getf used-pkg :used-by-list)
              (remove pkg (package-used-by-list* used-pkg))))
      t)))

(defun rename-package* (package new-name &optional new-nicknames)
  "Rename a package.
   PACKAGE is a package designator.
   NEW-NAME is the new package name (string).
   NEW-NICKNAMES is an optional list of new nicknames.
   Returns the package."
  (let* ((pkg (package-designator-to-package package))
         (old-name (package-name* pkg))
         (old-nicknames (package-nicknames* pkg)))
    ;; Remove old registrations
    (remhash old-name *packages*)
    (dolist (nick old-nicknames)
      (remhash nick *packages*))
    ;; Update package
    (setf (getf pkg :name) new-name)
    (setf (getf pkg :nicknames) new-nicknames)
    ;; Register new names
    (setf (gethash new-name *packages*) pkg)
    (dolist (nick new-nicknames)
      (setf (gethash nick *packages*) pkg))
    pkg))

(defun list-all-packages* ()
  "Return a list of all registered packages.
   Each package appears only once (duplicates from nicknames removed)."
  (let ((packages nil))
    (maphash (lambda (name pkg)
               (declare (ignore name))
               (unless (member pkg packages)
                 (push pkg packages)))
             *packages*)
    packages))

;;; ============================================================
;;; Symbol Interning (T010)
;;; Updated to use internal-symbols table
;;; ============================================================

(defun intern-symbol (name &optional package)
  "Intern a symbol in a package.
   NAME is a string.
   PACKAGE can be a package object, string, or keyword.
   Returns the symbol.
   Note: This is a simplified version that doesn't return multiple values."
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")
                     (find-package* "CL")))
                ((packagep* package) package)
                (t (package-designator-to-package package))))
         (upcase-name (string-upcase name))
         (internal-symbols (package-internal-symbols* pkg))
         (external-symbols (package-external-symbols* pkg)))
    ;; First check external symbols
    (let ((external-sym (gethash upcase-name external-symbols)))
      (when external-sym
        (return-from intern-symbol external-sym)))
    ;; Then check internal symbols
    (let ((internal-sym (gethash upcase-name internal-symbols)))
      (when internal-sym
        (return-from intern-symbol internal-sym)))
    ;; Check inherited symbols from use-list
    (dolist (used-pkg (package-use-list* pkg))
      (let ((inherited (gethash upcase-name (package-external-symbols* used-pkg))))
        (when inherited
          (return-from intern-symbol inherited))))
    ;; Create new symbol in internal table
    (let ((new-sym (if (string= (package-name* pkg) "KEYWORD")
                       ;; Keywords are special: intern in KEYWORD package and auto-export
                       (let ((kw (intern upcase-name "KEYWORD")))
                         (setf (gethash upcase-name external-symbols) kw)
                         kw)
                       ;; Regular symbols
                       (let ((sym (intern upcase-name)))
                         (setf (gethash upcase-name internal-symbols) sym)
                         sym))))
      new-sym)))

(defun find-symbol* (name &optional package)
  "Find a symbol in a package without interning.
   Returns NIL if not found."
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")
                     (find-package* "CL")))
                ((packagep* package) package)
                (t (package-designator-to-package package))))
         (upcase-name (string-upcase name)))
    (when pkg
      ;; Check external symbols first
      (let ((external-sym (gethash upcase-name (package-external-symbols* pkg))))
        (when external-sym
          (return-from find-symbol* external-sym)))
      ;; Check internal symbols
      (let ((internal-sym (gethash upcase-name (package-internal-symbols* pkg))))
        (when internal-sym
          (return-from find-symbol* internal-sym)))
      ;; Check inherited symbols from use-list
      (dolist (used-pkg (package-use-list* pkg))
        (let ((inherited (gethash upcase-name (package-external-symbols* used-pkg))))
          (when inherited
            (return-from find-symbol* inherited))))
      nil)))

;;; ============================================================
;;; Export/Import/Shadow Functions (US2 - T034-T038)
;;; ============================================================

(defun export* (symbols &optional package)
  "Export SYMBOLS from PACKAGE.
   SYMBOLS can be a symbol or list of symbols, or a string or list of strings.
   If the symbol doesn't exist in the package, it is first interned.
   Moves symbols from internal to external table."
  (let* ((pkg (if package
                  (package-designator-to-package package)
                  (or *current-package*
                      (find-package* "CL-USER"))))
         (sym-list (if (listp symbols) symbols (list symbols))))
    (dolist (sym sym-list)
      (let* ((name (if (stringp sym)
                       (string-upcase sym)
                       (symbol-name sym)))
             (internal-table (package-internal-symbols* pkg))
             (external-table (package-external-symbols* pkg))
             (existing (or (gethash name internal-table)
                           (gethash name external-table))))
        ;; If symbol doesn't exist, intern it first
        (unless existing
          (setf existing (intern-symbol name pkg)))
        ;; Move from internal to external
        (remhash name internal-table)
        (setf (gethash name external-table) existing)))
    t))

(defun unexport* (symbols &optional package)
  "Unexport SYMBOLS from PACKAGE.
   Moves symbols from external to internal table."
  (let* ((pkg (if package
                  (package-designator-to-package package)
                  (or *current-package*
                      (find-package* "CL-USER"))))
         (sym-list (if (listp symbols) symbols (list symbols))))
    (dolist (sym sym-list)
      (let* ((name (if (stringp sym)
                       (string-upcase sym)
                       (symbol-name sym)))
             (internal-table (package-internal-symbols* pkg))
             (external-table (package-external-symbols* pkg))
             (existing (gethash name external-table)))
        (when existing
          ;; Move from external to internal
          (remhash name external-table)
          (setf (gethash name internal-table) existing))))
    t))

(defun import* (symbols &optional package)
  "Import SYMBOLS into PACKAGE.
   SYMBOLS can be a symbol or list of symbols."
  (let* ((pkg (if package
                  (package-designator-to-package package)
                  (or *current-package*
                      (find-package* "CL-USER"))))
         (sym-list (if (listp symbols) symbols (list symbols))))
    (dolist (sym sym-list)
      (when sym
        (let* ((name (symbol-name sym))
               (internal-table (package-internal-symbols* pkg)))
          ;; Add to internal symbols
          (setf (gethash name internal-table) sym))))
    t))

(defun shadow* (symbols &optional package)
  "Shadow SYMBOLS in PACKAGE.
   Creates new symbols that shadow inherited ones."
  (let* ((pkg (if package
                  (package-designator-to-package package)
                  (or *current-package*
                      (find-package* "CL-USER"))))
         (sym-list (if (listp symbols) symbols (list symbols))))
    (dolist (sym sym-list)
      (let* ((name (if (stringp sym)
                       (string-upcase sym)
                       (symbol-name sym)))
             (internal-table (package-internal-symbols* pkg))
             (shadowing-list (package-shadowing-symbols* pkg)))
        ;; Create new symbol
        (let ((new-sym (intern name)))
          (setf (gethash name internal-table) new-sym)
          ;; Add to shadowing symbols list
          (unless (member new-sym shadowing-list)
            (push new-sym (getf pkg :shadowing-symbols))))))
    t))

(defun shadowing-import* (symbols &optional package)
  "Import SYMBOLS into PACKAGE, shadowing any existing symbols."
  (let* ((pkg (if package
                  (package-designator-to-package package)
                  (or *current-package*
                      (find-package* "CL-USER"))))
         (sym-list (if (listp symbols) symbols (list symbols))))
    (dolist (sym sym-list)
      (when sym
        (let* ((name (symbol-name sym))
               (internal-table (package-internal-symbols* pkg)))
          ;; Add to internal symbols (overwriting any existing)
          (setf (gethash name internal-table) sym)
          ;; Add to shadowing symbols list
          (unless (member sym (package-shadowing-symbols* pkg))
            (push sym (getf pkg :shadowing-symbols))))))
    t))

;;; ============================================================
;;; Multiple-Value Intern/Find-Symbol Functions (US5 - T076-T080)
;;; ============================================================

(defun intern* (name &optional package)
  "Intern a symbol in a package with multiple value return (T076).
   NAME is a string designator.
   PACKAGE is a package designator.
   Returns two values:
   - The symbol
   - Status: NIL (new), :INTERNAL, :EXTERNAL, or :INHERITED"
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")
                     (find-package* "CL")))
                ((packagep* package) package)
                (t (package-designator-to-package package))))
         (upcase-name (string-upcase (if (stringp name) name (symbol-name name))))
         (internal-symbols (package-internal-symbols* pkg))
         (external-symbols (package-external-symbols* pkg)))
    ;; First check external symbols
    (let ((external-sym (gethash upcase-name external-symbols)))
      (when external-sym
        (return-from intern* (values external-sym :external))))
    ;; Then check internal symbols
    (let ((internal-sym (gethash upcase-name internal-symbols)))
      (when internal-sym
        (return-from intern* (values internal-sym :internal))))
    ;; Check inherited symbols from use-list (T077)
    (dolist (used-pkg (package-use-list* pkg))
      (let ((inherited (gethash upcase-name (package-external-symbols* used-pkg))))
        (when inherited
          (return-from intern* (values inherited :inherited)))))
    ;; Create new symbol in internal table
    (let ((new-sym (if (string= (package-name* pkg) "KEYWORD")
                       ;; Keywords are special: intern in KEYWORD package and auto-export
                       (let ((kw (intern upcase-name "KEYWORD")))
                         (setf (gethash upcase-name external-symbols) kw)
                         kw)
                       ;; Regular symbols
                       (let ((sym (intern upcase-name)))
                         (setf (gethash upcase-name internal-symbols) sym)
                         sym))))
      (values new-sym nil))))  ;; NIL status for new symbol

(defun find-symbol** (name &optional package)
  "Find a symbol in a package without interning (T078).
   NAME is a string designator.
   PACKAGE is a package designator.
   Returns two values:
   - The symbol (or NIL if not found)
   - Status: NIL, :INTERNAL, :EXTERNAL, or :INHERITED"
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")
                     (find-package* "CL")))
                ((packagep* package) package)
                (t (package-designator-to-package package))))
         (upcase-name (string-upcase (if (stringp name) name (symbol-name name)))))
    (when pkg
      ;; Check external symbols first
      (let ((external-sym (gethash upcase-name (package-external-symbols* pkg))))
        (when external-sym
          (return-from find-symbol** (values external-sym :external))))
      ;; Check internal symbols
      (let ((internal-sym (gethash upcase-name (package-internal-symbols* pkg))))
        (when internal-sym
          (return-from find-symbol** (values internal-sym :internal))))
      ;; Check inherited symbols from use-list
      (dolist (used-pkg (package-use-list* pkg))
        (let ((inherited (gethash upcase-name (package-external-symbols* used-pkg))))
          (when inherited
            (return-from find-symbol** (values inherited :inherited)))))
      ;; Not found
      (values nil nil))))

(defun unintern* (symbol &optional package)
  "Unintern a symbol from a package (T079).
   SYMBOL is the symbol to unintern.
   PACKAGE is a package designator.
   Returns T if symbol was present, NIL otherwise."
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")))
                ((packagep* package) package)
                (t (package-designator-to-package package))))
         (name (symbol-name symbol))
         (internal-table (package-internal-symbols* pkg))
         (external-table (package-external-symbols* pkg))
         (removed nil))
    ;; Remove from internal symbols
    (when (gethash name internal-table)
      (remhash name internal-table)
      (setf removed t))
    ;; Remove from external symbols
    (when (gethash name external-table)
      (remhash name external-table)
      (setf removed t))
    ;; Remove from shadowing symbols
    (when (member symbol (package-shadowing-symbols* pkg))
      (setf (getf pkg :shadowing-symbols)
            (remove symbol (package-shadowing-symbols* pkg))))
    removed))

;;; ============================================================
;;; Use-Package / Unuse-Package Functions (US6 - T087-T090)
;;; ============================================================

(defun use-package* (packages-to-use &optional package)
  "Make exported symbols from PACKAGES-TO-USE accessible in PACKAGE (T087-T088).
   PACKAGES-TO-USE can be a package designator or list of designators.
   PACKAGE is a package designator (defaults to current package).
   Returns T."
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")))
                ((packagep* package) package)
                (t (package-designator-to-package package))))
         ;; Check packagep* first since packages are plists (which are lists)
         (use-list (cond
                     ((packagep* packages-to-use) (list packages-to-use))
                     ((listp packages-to-use) packages-to-use)
                     (t (list packages-to-use)))))
    (dolist (pkg-to-use-designator use-list)
      (let ((pkg-to-use (if (packagep* pkg-to-use-designator)
                            pkg-to-use-designator
                            (package-designator-to-package pkg-to-use-designator))))
        ;; Add to use-list if not already present
        (unless (member pkg-to-use (package-use-list* pkg))
          (push pkg-to-use (getf pkg :use-list))
          ;; Update used-by-list of the used package
          (push pkg (getf pkg-to-use :used-by-list)))))
    t))

(defun unuse-package* (packages-to-unuse &optional package)
  "Remove PACKAGES-TO-UNUSE from PACKAGE's use-list (T089).
   PACKAGES-TO-UNUSE can be a package designator or list of designators.
   PACKAGE is a package designator (defaults to current package).
   Returns T."
  (let* ((pkg (cond
                ((null package)
                 (or *current-package*
                     (find-package* "CL-USER")))
                ((packagep* package) package)
                (t (package-designator-to-package package))))
         ;; Check packagep* first since packages are plists (which are lists)
         (unuse-list (cond
                       ((packagep* packages-to-unuse) (list packages-to-unuse))
                       ((listp packages-to-unuse) packages-to-unuse)
                       (t (list packages-to-unuse)))))
    (dolist (pkg-to-unuse-designator unuse-list)
      (let ((pkg-to-unuse (if (packagep* pkg-to-unuse-designator)
                              pkg-to-unuse-designator
                              (package-designator-to-package pkg-to-unuse-designator))))
        ;; Remove from use-list
        (setf (getf pkg :use-list)
              (remove pkg-to-unuse (package-use-list* pkg)))
        ;; Remove from used-by-list of the unused package
        (setf (getf pkg-to-unuse :used-by-list)
              (remove pkg (package-used-by-list* pkg-to-unuse)))))
    t))

;;; ============================================================
;;; Initialize Standard Packages
;;; ============================================================

(defun initialize-packages ()
  "Initialize the standard packages."
  ;; Clear existing packages
  (clrhash *packages*)

  ;; COMMON-LISP package
  (let ((cl-pkg (make-package* "COMMON-LISP" :nicknames '("CL"))))
    ;; Pre-intern and export standard symbols
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
                   "QUASIQUOTE" "UNQUOTE" "UNQUOTE-SPLICING"
                   ;; Package functions
                   "MAKE-PACKAGE" "FIND-PACKAGE" "DELETE-PACKAGE"
                   "LIST-ALL-PACKAGES" "RENAME-PACKAGE"
                   "EXPORT" "UNEXPORT" "IMPORT" "SHADOW" "SHADOWING-IMPORT"
                   "USE-PACKAGE" "UNUSE-PACKAGE"
                   "INTERN" "FIND-SYMBOL" "UNINTERN"
                   "PACKAGE-NAME" "PACKAGE-NICKNAMES" "PACKAGE-USE-LIST"
                   "PACKAGE-USED-BY-LIST" "PACKAGE-SHADOWING-SYMBOLS"
                   "PACKAGEP" "SYMBOL-PACKAGE"
                   "IN-PACKAGE" "DEFPACKAGE" "*PACKAGE*"))
      ;; Intern and export
      (let ((interned-sym (intern sym)))
        (setf (gethash sym (package-external-symbols* cl-pkg)) interned-sym))))

  ;; KEYWORD package
  (make-package* "KEYWORD")

  ;; COMMON-LISP-USER package (uses CL)
  (let ((user-pkg (make-package* "COMMON-LISP-USER"
                                 :nicknames '("CL-USER" "USER")
                                 :use (list (find-package* "COMMON-LISP")))))
    (setf *current-package* user-pkg))

  ;; CLYSM-USER package (uses CL)
  (make-package* "CLYSM-USER" :use (list (find-package* "COMMON-LISP"))))

;; Initialize on load
(initialize-packages)
