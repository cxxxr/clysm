;;;; package-macros.lisp - defpackage and in-package macros (013-package-system)
;;;; T023-T024: Package definition macros

(in-package #:clysm/lib/package-macros)

;;; ============================================================
;;; IN-PACKAGE Macro (T024)
;;; ============================================================

(defmacro in-package* (name)
  "Set *package* (actually *current-package*) to the specified package.
   NAME is a string designator (string, symbol, or keyword)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((pkg (clysm/reader/package:find-package*
                  ,(if (stringp name)
                       name
                       (string-upcase (symbol-name name))))))
       (unless pkg
         (error 'clysm/reader/package:clysm-package-error
                :package ,(if (stringp name)
                              name
                              (string-upcase (symbol-name name)))
                :message "Package does not exist"))
       (setf clysm/reader/package:*current-package* pkg)
       pkg)))

;;; ============================================================
;;; DEFPACKAGE Macro (T023)
;;; ============================================================

(defmacro defpackage* (name &body options)
  "Define a new package with the given name and options.
   Supported options:
   - (:nicknames name*)     - alternate names
   - (:use package-name*)   - packages to use
   - (:export symbol-name*) - symbols to export (requires export* from US2)
   - (:import-from package-name symbol-name*) - symbols to import (requires import* from US2)
   - (:shadow symbol-name*) - symbols to shadow (requires shadow* from US2)
   Returns the package."
  (let ((nicknames nil)
        (use nil)
        (exports nil)
        (imports nil)
        (shadows nil))
    ;; Parse options
    (dolist (option options)
      (case (first option)
        (:nicknames
         (setf nicknames (mapcar (lambda (n)
                                   (if (stringp n)
                                       n
                                       (string-upcase (symbol-name n))))
                                 (rest option))))
        (:use
         (setf use (mapcar (lambda (p)
                             (if (stringp p)
                                 p
                                 (string-upcase (symbol-name p))))
                           (rest option))))
        (:export
         (setf exports (mapcar (lambda (s)
                                 (if (stringp s)
                                     s
                                     (string-upcase (symbol-name s))))
                               (rest option))))
        (:import-from
         (push (rest option) imports))
        (:shadow
         (setf shadows (mapcar (lambda (s)
                                 (if (stringp s)
                                     s
                                     (string-upcase (symbol-name s))))
                               (rest option))))))
    (let ((pkg-name (if (stringp name)
                        name
                        (string-upcase (symbol-name name)))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; Create or find package
         (let ((pkg (or (clysm/reader/package:find-package* ,pkg-name)
                        (clysm/reader/package:make-package* ,pkg-name
                          :nicknames ',nicknames
                          :use (remove nil
                                  (mapcar #'clysm/reader/package:find-package* ',use))))))
           ;; Process :shadow (requires shadow* from US2)
           ,@(when shadows
               `((dolist (sym-name ',shadows)
                   (clysm/reader/package:shadow* sym-name pkg))))
           ;; Process :export (requires export* from US2)
           ,@(when exports
               `((dolist (sym-name ',exports)
                   (clysm/reader/package:export* sym-name pkg))))
           ;; Process :import-from (requires import* from US2)
           ,@(mapcar (lambda (import-spec)
                       (let ((from-pkg (first import-spec))
                             (syms (rest import-spec)))
                         `(dolist (sym-name ',(mapcar (lambda (s)
                                                        (if (stringp s)
                                                            s
                                                            (string-upcase (symbol-name s))))
                                                      syms))
                            (clysm/reader/package:import*
                              (clysm/reader/package:find-symbol* sym-name
                                ,(if (stringp from-pkg)
                                     from-pkg
                                     (string-upcase (symbol-name from-pkg))))
                              pkg))))
                     imports)
           pkg)))))
