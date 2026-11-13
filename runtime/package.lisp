;;;; package.lisp — packages
;;;;
;;;; A package is a WasmGC $package struct with 8 fields:
;;;;  0 name  1 nicknames  2 use-list  3 used-by-list  4 internal table
;;;;  5 external table  6 shadowing symbols  7 documentation

(defvar *package-table* nil)
(defvar *keyword-package* nil)
(defvar *cl-package* nil)
(defvar *system-package* nil)
(defvar *package* nil)

(defmacro %pkg (p i) `(%package-ref ,p ,i))
(defsetf %package-ref %set-package-ref)
(defmacro %pkg-set (p i v) `(%set-package-ref ,p ,i ,v))

(defun package-name (p) (%pkg (%find-package-or-die p) 0))
(defun package-nicknames (p) (%pkg (%find-package-or-die p) 1))
(defun package-use-list (p) (%pkg (%find-package-or-die p) 2))
(defun package-used-by-list (p) (%pkg (%find-package-or-die p) 3))
(defun package-shadowing-symbols (p) (%pkg (%find-package-or-die p) 6))

(defun %package-internals (p) (%pkg p 4))
(defun %package-externals (p) (%pkg p 5))

(defvar *package-local-nicknames* nil) ; package -> ((nickname . package) ...)

(defun find-package (name)
  (cond ((%packagep name) name)
        ((and (symbolp name) (null *package-local-nicknames*))
         ;; a symbol's hash is the hash of its name: no need to hash the name again
         (values (%gethash-hashed (symbol-name name) *package-table* (%symbol-hash name))))
        (t
         (let ((s (string name)))
           (or (and *package-local-nicknames* *package*
                    (cdr (assoc s (gethash *package* *package-local-nicknames*) :test #'string=)))
               (values (gethash s *package-table*)))))))

;;; package-local nicknames (as in SBCL, CCL, ECL...)
(defun package-local-nicknames (package)
  (let ((p (%find-package-or-die package)))
    (if *package-local-nicknames*
        (mapcar (lambda (e) (cons (car e) (cdr e))) (gethash p *package-local-nicknames*))
        nil)))

(defun add-package-local-nickname (local-nickname actual-package &optional (package *package*))
  (let ((nick (string local-nickname))
        (actual (%find-package-or-die actual-package))
        (p (%find-package-or-die package)))
    (unless *package-local-nicknames* (setq *package-local-nicknames* (make-hash-table :test 'eq)))
    (let* ((alist (gethash p *package-local-nicknames*))
           (old (assoc nick alist :test #'string=)))
      (when (and old (not (eq (cdr old) actual)))
        (%package-error p "~A is already a package-local nickname for ~A in ~A"
                        nick (package-name (cdr old)) (package-name p)))
      (when (member nick '("CL" "COMMON-LISP" "KEYWORD") :test #'string=)
        (%package-error p "~A cannot be a package-local nickname" nick))
      (unless old
        (setf (gethash p *package-local-nicknames*) (append alist (list (cons nick actual))))))
    p))

(defun remove-package-local-nickname (old-nickname &optional (package *package*))
  (let* ((p (%find-package-or-die package))
         (alist (and *package-local-nicknames* (gethash p *package-local-nicknames*)))
         (e (assoc (string old-nickname) alist :test #'string=)))
    (when e
      (setf (gethash p *package-local-nicknames*) (remove e alist))
      t)))

(defun package-locally-nicknamed-by-list (package)
  (let ((p (%find-package-or-die package)) (r nil))
    (when *package-local-nicknames*
      (maphash (lambda (k alist) (when (rassoc p alist) (push k r))) *package-local-nicknames*))
    r))

(defun %find-package-or-die (name)
  (or (find-package name)
      (error 'package-error :package name
                            :format-control "The package ~S does not exist."
                            :format-arguments (list name))))

(defun list-all-packages ()
  (let ((r nil))
    (maphash (lambda (k p) (declare (ignore k)) (unless (member p r) (push p r))) *package-table*)
    (nreverse r)))

(defun %new-package (name nicknames)
  (let ((p (%make-package)))
    (%pkg-set p 0 name)
    (%pkg-set p 1 nicknames)
    (%pkg-set p 2 nil)
    (%pkg-set p 3 nil)
    (%pkg-set p 4 (make-hash-table :test 'equal))
    (%pkg-set p 5 (make-hash-table :test 'equal))
    (%pkg-set p 6 nil)
    (setf (gethash name *package-table*) p)
    (dolist (n nicknames) (setf (gethash n *package-table*) p))
    p))

;;; ------------------------------------------------------------
;;; errors and name conflicts (CLHS 11.1.1.2.5)
;;; ------------------------------------------------------------

(defun %package-error (package control &rest args)
  (error 'package-error :package package :format-control control :format-arguments args))

;; %correctable-package-error: see cond.lisp

(defun %present (name p &optional (hash (%sxhash name 4)))
  "The symbol named NAME present in P, and :INTERNAL or :EXTERNAL."
  (multiple-value-bind (s found) (%gethash-hashed name (%package-externals p) hash)
    (if found (values s :external)
        (multiple-value-bind (s found) (%gethash-hashed name (%package-internals p) hash)
          (if found (values s :internal) (values nil nil))))))

(defun %inherited-candidates (name p)
  "The distinct symbols named NAME exported by the packages P uses."
  (let ((r nil))
    (dolist (u (%pkg p 2) (nreverse r))
      (multiple-value-bind (s found) (gethash name (%package-externals u))
        (when (and found (not (member s r))) (push s r))))))

(defun %shadowing-p (s p) (member s (%pkg p 6)))

(defun %remove-present (name p)
  (remhash name (%package-internals p))
  (remhash name (%package-externals p)))

;;; ------------------------------------------------------------
;;; creating packages
;;; ------------------------------------------------------------

(defun %string-designator-list (x) (mapcar #'string (if (listp x) x (list x))))

(defun make-package (name &key nicknames (use nil))
  (let ((name (string name)) (nicknames (%string-designator-list nicknames)))
    (let ((existing (find-package name)))
      (when existing
        (%correctable-package-error name "Return the existing package."
                                    "A package named ~S already exists." name)
        (return-from make-package existing)))
    (setq nicknames
          (remove-if (lambda (n)
                       (and (find-package n)
                            (%correctable-package-error n "Don't use this nickname."
                                                        "A package named ~S already exists." n)))
                     nicknames))
    (let ((uses (mapcar #'%find-package-or-die (if (listp use) use (list use)))))
      (let ((p (%new-package name nicknames)))
        (use-package uses p)
        p))))

;;; ------------------------------------------------------------
;;; symbols
;;; ------------------------------------------------------------

(defun %symbol-name-arg (name)
  (if (stringp name) name (%type-error name 'string)))

(defun find-symbol (name &optional (package *package*))
  (let* ((p (%find-package-or-die package)) (name (%symbol-name-arg name)) (hash (%sxhash name 4)))
    (multiple-value-bind (s status) (%present name p hash)
      (if status (values s status)
          (dolist (u (%pkg p 2) (values nil nil))
            (multiple-value-bind (s found) (%gethash-hashed name (%package-externals u) hash)
              (when found (return (values s :inherited)))))))))

(defun intern (name &optional (package *package*))
  (let ((p (%find-package-or-die package)) (name (%symbol-name-arg name)))
    (multiple-value-bind (s status) (find-symbol name p)
      (if status
          (values s status)
          (let ((s (make-symbol (copy-seq name))))
            (%set-symbol-package s p)
            (if (eq p *keyword-package*)
                (progn (setf (gethash (symbol-name s) (%package-externals p)) s)
                       (%set-symbol-value s s)
                       (%set-symbol-flags s 3))
                (setf (gethash (symbol-name s) (%package-internals p)) s))
            (values s nil))))))

(defun unintern (symbol &optional (package *package*))
  (let* ((p (%find-package-or-die package)) (name (symbol-name symbol)))
    (multiple-value-bind (s status) (%present name p)
      (if (and status (eq s symbol))
          (progn
            (when (%shadowing-p symbol p)
              ;; removing a shadowing symbol may expose a conflict
              (let ((cands (%inherited-candidates name p)))
                (when (and (cdr cands)
                           (%correctable-package-error
                            p (format nil "Shadowing-import ~S." (car cands))
                            "Uninterning ~S from ~A would cause a name conflict among ~S."
                            symbol (package-name p) cands))
                  (%remove-present name p)
                  (%pkg-set p 6 (remove symbol (%pkg p 6)))
                  (when (eq (%symbol-package symbol) p) (%set-symbol-package symbol nil))
                  (shadowing-import (car cands) p)
                  (return-from unintern t))))
            (%remove-present name p)
            (%pkg-set p 6 (remove symbol (%pkg p 6)))
            (when (eq (%symbol-package symbol) p) (%set-symbol-package symbol nil))
            t)
          nil))))

(defun %designator-list (x) (if (listp x) x (list x)))

(defun export (symbols &optional (package *package*))
  (let ((p (%find-package-or-die package)))
    (dolist (s (%designator-list symbols) t)
      (let ((name (symbol-name s)))
        (multiple-value-bind (found status) (find-symbol name p)
          (cond ((not (and status (eq found s)))
                 (if status
                     (%correctable-package-error
                      p "Skip it." "~S is not accessible in ~A: ~S has the same name."
                      s (package-name p) found)
                     (when (%correctable-package-error p (format nil "Import ~S." s)
                                                       "~S is not accessible in ~A." s (package-name p))
                       (import s p)
                       (setq status :internal)))))
          (when (and (eq found s) (eq status :inherited)) (import s p) (setq status :internal))
          (when (and status (eq (find-symbol name p) s))
            ;; conflicts in the packages that use P
            (let ((ok t))
              (dolist (u (%pkg p 3))
                (multiple-value-bind (other ostatus) (find-symbol name u)
                  (when (and ostatus (not (eq other s)) (not (%shadowing-p other u)))
                    (if (%correctable-package-error
                         u (format nil "Make ~S a shadowing symbol of ~A." other (package-name u))
                         "Exporting ~S from ~A conflicts with ~S in ~A."
                         s (package-name p) other (package-name u))
                        (if (eq ostatus :inherited)
                            (shadowing-import other u)
                            (pushnew other (%pkg u 6)))
                        (setq ok nil)))))
              (when ok
                (remhash name (%package-internals p))
                (setf (gethash name (%package-externals p)) s)))))))))

(defun unexport (symbols &optional (package *package*))
  (let ((p (%find-package-or-die package)))
    (dolist (s (%designator-list symbols) t)
      (let ((name (symbol-name s)))
        (multiple-value-bind (found status) (find-symbol name p)
          (unless (and status (eq found s))
            (%package-error p "~S is not accessible in ~A." s (package-name p)))
          (when (eq status :external)
            (remhash name (%package-externals p))
            (setf (gethash name (%package-internals p)) s)))))))

(defun import (symbols &optional (package *package*))
  (let ((p (%find-package-or-die package)))
    (dolist (s (%designator-list symbols) t)
      (let ((name (symbol-name s)))
        (multiple-value-bind (found status) (find-symbol name p)
          (cond ((and status (eq found s) (not (eq status :inherited))) nil)
                ((and status (not (eq found s)))
                 (when (%correctable-package-error
                        p (format nil "Shadowing-import ~S." s)
                        "Importing ~S into ~A conflicts with ~S." s (package-name p) found)
                   (shadowing-import s p)))
                (t (setf (gethash name (%package-internals p)) s)
                   (unless (%symbol-package s) (%set-symbol-package s p)))))))))

(defun shadow (names &optional (package *package*))
  (let ((p (%find-package-or-die package)))
    (dolist (n (%designator-list names) t)
      (let ((name (string n)))
        (multiple-value-bind (s status) (%present name p)
          (unless status
            (setq s (make-symbol name))
            (%set-symbol-package s p)
            (setf (gethash name (%package-internals p)) s))
          (pushnew s (%pkg p 6)))))))

(defun shadowing-import (symbols &optional (package *package*))
  (let ((p (%find-package-or-die package)))
    (dolist (s (%designator-list symbols) t)
      (let ((name (symbol-name s)))
        (multiple-value-bind (old status) (%present name p)
          (unless (and status (eq old s))
            (when status
              (%remove-present name p)
              (when (eq (%symbol-package old) p) (%set-symbol-package old nil)))
            (setf (gethash name (%package-internals p)) s)))
        (%pkg-set p 6 (cons s (remove name (%pkg p 6) :key #'symbol-name :test #'string=)))))))

(defun use-package (packages &optional (package *package*))
  (let ((p (%find-package-or-die package)))
    (when (eq p *keyword-package*) (%package-error p "The KEYWORD package cannot use other packages."))
    (dolist (u (%designator-list packages) t)
      (let ((up (%find-package-or-die u)))
        (when (eq up *keyword-package*) (%package-error up "The KEYWORD package cannot be used."))
        (unless (or (eq up p) (member up (%pkg p 2)))
          ;; name conflicts between the new external symbols and P's
          (maphash (lambda (name s)
                     (multiple-value-bind (other status) (find-symbol name p)
                       (when (and status (not (eq other s)) (not (%shadowing-p other p)))
                         (when (%correctable-package-error
                                p (format nil "Keep ~S (make it a shadowing symbol)." other)
                                "Using ~A in ~A: ~S conflicts with ~S."
                                (package-name up) (package-name p) s other)
                           (if (eq status :inherited)
                               (shadowing-import other p)
                               (pushnew other (%pkg p 6)))))))
                   (%package-externals up))
          (%pkg-set p 2 (append (%pkg p 2) (list up)))
          (%pkg-set up 3 (cons p (%pkg up 3))))))))

(defun unuse-package (packages &optional (package *package*))
  (let ((p (%find-package-or-die package)))
    (dolist (u (%designator-list packages) t)
      (let ((up (%find-package-or-die u)))
        (%pkg-set p 2 (remove up (%pkg p 2)))
        (%pkg-set up 3 (remove p (%pkg up 3)))))))

(defun delete-package (package)
  (let ((p (find-package package)))
    (cond
      ((null p)
       (%correctable-package-error package "Return NIL." "There is no package named ~S." package)
       nil)
      ((null (%pkg p 0)) nil)            ; already deleted
      (t
       (when (%pkg p 3)
         (unless (%correctable-package-error
                  p "Remove it from the use lists of the packages using it and delete it."
                  "Package ~A is used by ~S." (package-name p) (mapcar #'package-name (%pkg p 3)))
           (return-from delete-package nil))
         (dolist (u (%pkg p 3)) (unuse-package p u)))
       (maphash (lambda (k v) (declare (ignore k)) (when (eq (%symbol-package v) p) (%set-symbol-package v nil)))
                (%package-internals p))
       (maphash (lambda (k v) (declare (ignore k)) (when (eq (%symbol-package v) p) (%set-symbol-package v nil)))
                (%package-externals p))
       (dolist (u (%pkg p 2)) (%pkg-set u 3 (remove p (%pkg u 3))))
       (%pkg-set p 2 nil)
       (remhash (%pkg p 0) *package-table*)
       (dolist (n (%pkg p 1)) (remhash n *package-table*))
       (%pkg-set p 0 nil)
       (%pkg-set p 1 nil)
       t))))

(defun rename-package (package new-name &optional new-nicknames)
  (let* ((p (%find-package-or-die package))
         (new-name (if (%packagep new-name) (package-name new-name) (string new-name)))
         (new-nicknames (%string-designator-list new-nicknames)))
    (dolist (n (cons new-name new-nicknames))
      (let ((other (find-package n)))
        (when (and other (not (eq other p)))
          (%package-error n "A package named ~S already exists." n))))
    (remhash (%pkg p 0) *package-table*)
    (dolist (n (%pkg p 1)) (remhash n *package-table*))
    (%pkg-set p 0 new-name)
    (%pkg-set p 1 new-nicknames)
    (setf (gethash (%pkg p 0) *package-table*) p)
    (dolist (n (%pkg p 1)) (setf (gethash n *package-table*) p))
    p))

(defun %package-symbol-list (package kind)
  (let ((p (%find-package-or-die package)) (r nil))
    (maphash (lambda (k s) (declare (ignore k)) (push s r)) (%package-externals p))
    (unless (eq kind :external)
      (maphash (lambda (k s) (declare (ignore k)) (push s r)) (%package-internals p))
      (when (eq kind :all)
        (dolist (u (%pkg p 2))
          (maphash (lambda (k s) (declare (ignore k))
                     (unless (member s (%pkg p 6))
                       (multiple-value-bind (found st) (find-symbol (symbol-name s) p)
                         (declare (ignore found))
                         (when (eq st :inherited) (push s r)))))
                   (%package-externals u)))))
    (nreverse r)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expand-do-symbols (var list-form result body)
    ;; declarations apply to the whole iteration, including RESULT, where
    ;; VAR is bound to NIL
    (multiple-value-bind (decls forms) (%split-decls body)
      (let ((l (gensym "SYMBOLS")) (top (gensym "TOP")))
        `(block nil
           (let ((,l ,list-form))
             (let ((,var nil))
               ,@decls
               (tagbody
                  ,top
                  (when ,l
                    (setq ,var (car ,l) ,l (cdr ,l))
                    (tagbody ,@forms)
                    (go ,top)))
               (setq ,var nil)
               ,result)))))))

(defmacro do-symbols ((var &optional (package '*package*) result) &body body)
  (%expand-do-symbols var `(%package-symbol-list ,package :all) result body))
(defmacro do-external-symbols ((var &optional (package '*package*) result) &body body)
  (%expand-do-symbols var `(%package-symbol-list ,package :external) result body))
(defmacro do-all-symbols ((var &optional result) &body body)
  (%expand-do-symbols var '(%all-symbols) result body))
(defun %all-symbols ()
  (let ((r nil))
    (dolist (p (list-all-packages) (remove-duplicates r))
      (setq r (append (%package-symbol-list p :present) r)))))

(defmacro with-package-iterator ((name package-list &rest kinds) &body body)
  (when (or (null kinds)
            (dolist (k kinds nil) (unless (member k '(:internal :external :inherited)) (return t))))
    (error 'simple-program-error
           :format-control "WITH-PACKAGE-ITERATOR: bad symbol types ~S"
           :format-arguments (list kinds)))
  (let ((items (gensym)))
    `(let ((,items (%package-iterator-items ,package-list ',kinds)))
       (macrolet ((,name () '(if ,items
                                (let ((e (pop ,items))) (values t (car e) (cadr e) (caddr e)))
                                nil)))
         ,@body))))
(defun %package-iterator-items (packages kinds)
  (let ((r nil))
    (dolist (p (%designator-list packages) (nreverse r))
      (let ((p (%find-package-or-die p)))
        (when (member :external kinds)
          (maphash (lambda (k s) (declare (ignore k)) (push (list s :external p) r)) (%package-externals p)))
        (when (member :internal kinds)
          (maphash (lambda (k s) (declare (ignore k)) (push (list s :internal p) r)) (%package-internals p)))
        (when (member :inherited kinds)
          (dolist (u (%pkg p 2))
            (maphash (lambda (k s)
                       (when (eq (find-symbol k p) s) (push (list s :inherited p) r)))
                     (%package-externals u))))))))

(defun %in-package (name)
  (let ((p (find-package name)))
    (unless p (%package-error name "IN-PACKAGE: there is no package named ~S." name))
    (setq *package* p)))

(defmacro in-package (name) `(%in-package ,(string name)))

(defun %defpackage (name nicknames uses exports shadows imports &optional interns doc local-nicknames)
  (let ((p (or (find-package name) (make-package name :nicknames nicknames :use nil))))
    (when (or local-nicknames (and *package-local-nicknames* (gethash p *package-local-nicknames*)))
      (unless *package-local-nicknames* (setq *package-local-nicknames* (make-hash-table :test 'eq)))
      (setf (gethash p *package-local-nicknames*) nil)
      (dolist (ln local-nicknames)
        (add-package-local-nickname (car ln) (cadr ln) p)))
    (dolist (n nicknames)
      (unless (member n (%pkg p 1) :test #'string=)
        (when (find-package n)
          (%package-error n "A package named ~S already exists." n))
        (%pkg-set p 1 (cons n (%pkg p 1)))
        (setf (gethash n *package-table*) p)))
    (when shadows (shadow shadows p))
    (dolist (imp imports)
      (when (eq (car imp) :shadowing-import-from)
        (%defpackage-import p imp)))
    (use-package uses p)
    (dolist (imp imports)
      (unless (eq (car imp) :shadowing-import-from)
        (%defpackage-import p imp)))
    (dolist (i interns) (intern i p))
    (dolist (e exports) (export (list (intern e p)) p))
    (when doc (%set-documentation p t doc))
    p))

(defun %defpackage-import (p imp)
  (let ((from (%find-package-or-die (cadr imp))))
    (dolist (sname (cddr imp))
      (multiple-value-bind (s status) (find-symbol sname from)
        (unless status
          (if (%correctable-package-error from (format nil "Intern ~A in ~A." sname (package-name from))
                                          "DEFPACKAGE: there is no symbol named ~S in ~A."
                                          sname (package-name from))
              (setq s (intern sname from) status t)
              (setq s nil)))
        ;; S may be NIL itself
        (when status
          (if (eq (car imp) :shadowing-import-from)
              (shadowing-import (list s) p)
              (import (list s) p)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %defpackage-check (shadows interns imports exports)
    ;; CLHS DEFPACKAGE: the names in :SHADOW, :INTERN, :IMPORT-FROM and
    ;; :SHADOWING-IMPORT-FROM, and in :EXPORT and :INTERN, must be disjoint
    (flet ((bad (control &rest args)
             (error 'simple-program-error :format-control control :format-arguments args))
           (common (a b)
             (let ((r nil))
               (dolist (x a r) (when (member x b :test #'string=) (setq r (cons x r)))))))
      (let ((sets (list (cons :shadow shadows) (cons :intern interns))))
        (dolist (imp imports) (setq sets (cons (cons (car imp) (cddr imp)) sets)))
        (do ((l sets (cdr l))) ((null l))
          (dolist (other (cdr l))
            (let ((c (common (cdar l) (cdr other))))
              (when c (bad "DEFPACKAGE: ~S appear in both ~S and ~S" c (caar l) (car other)))))))
      (let ((c (common exports interns)))
        (when c (bad "DEFPACKAGE: ~S appear in both :EXPORT and :INTERN" c))))))

(defmacro defpackage (name &rest options)
  (let ((nicknames nil) (uses nil) (use-given nil) (exports nil) (shadows nil) (imports nil)
        (interns nil) (doc nil) (seen nil) (local-nicknames nil))
    (dolist (o options)
      (unless (and (consp o) (member (car o) '(:nicknames :documentation :use :shadow
                                               :shadowing-import-from :import-from :export
                                               :intern :size :local-nicknames)))
        (error 'simple-program-error :format-control "DEFPACKAGE: bad option ~S"
                                     :format-arguments (list o)))
      (when (member (car o) '(:size :documentation))
        (when (member (car o) seen)
          (error 'simple-program-error :format-control "DEFPACKAGE: option ~S given more than once"
                                       :format-arguments (list (car o))))
        (setq seen (cons (car o) seen)))
      (case (car o)
        (:documentation (setq doc (cadr o)))
        (:local-nicknames
         (setq local-nicknames (append local-nicknames
                                       (mapcar (lambda (e) (list (string (car e)) (string (cadr e)))) (cdr o)))))
        (:intern (setq interns (append interns (mapcar #'string (cdr o)))))
        (:nicknames (setq nicknames (append nicknames (mapcar #'string (cdr o)))))
        (:use (setq use-given t uses (append uses (mapcar #'string (cdr o)))))
        (:export (setq exports (append exports (mapcar #'string (cdr o)))))
        (:shadow (setq shadows (append shadows (mapcar #'string (cdr o)))))
        ((:import-from :shadowing-import-from)
         (setq imports (append imports
                               (list (list* (car o) (string (cadr o)) (mapcar #'string (cddr o)))))))))
    (%defpackage-check shadows interns imports exports)
    `(%defpackage ,(string name) ',nicknames ',(if use-given uses '("COMMON-LISP")) ',exports ',shadows
                  ',imports ',interns ,doc ',local-nicknames)))

;;; ------------------------------------------------------------
;;; initialization from the compiler-generated symbol table
;;; ------------------------------------------------------------

(defun %init-packages (tables)
  (let ((symtab (car tables)) (pkgtab (cdr tables)))
    (setq *package-table* (make-hash-table :test 'equal))
    (let ((pkgs (%make-simple-vector (%svlength pkgtab) nil)))
      (dotimes (i (%svlength pkgtab))
        (let ((spec (%svref pkgtab i)))
          (%set-svref pkgs i (%new-package (car spec) (cadr spec)))))
      (dotimes (i (%svlength pkgtab))
        (let ((spec (%svref pkgtab i)))
          (dolist (u (caddr spec))
            (use-package u (%svref pkgs i)))))
      (setq *keyword-package* (find-package "KEYWORD"))
      (setq *cl-package* (find-package "COMMON-LISP"))
      (setq *system-package* (find-package "SYSTEM"))
      (dotimes (chunk-index (%svlength symtab))
       (let ((chunk (%svref symtab chunk-index)))
        (do ((i 0 (+ i 3))) ((>= i (%svlength chunk)))
        (let ((sym (%svref chunk i)) (pidx (%svref chunk (+ i 1))) (ext (%svref chunk (+ i 2))))
          (when (>= pidx 0)
            (let ((p (%svref pkgs pidx)))
              (%set-symbol-package sym p)
              (if (or ext (eq p *keyword-package*))
                  (setf (gethash (symbol-name sym) (%package-externals p)) sym)
                  (setf (gethash (symbol-name sym) (%package-internals p)) sym))
              (when (eq p *keyword-package*)
                (%set-symbol-value sym sym)
                (%set-symbol-flags sym 3))))))))
      (%set-symbol-value t t)
      (%set-symbol-value nil nil)
      (%set-symbol-plist nil nil)
      (%set-symbol-flags t 3)
      (%set-symbol-flags nil 3)
      (setq *package* (find-package "COMMON-LISP-USER")))))

(%init-packages (%symbol-table))
