;;;; interpreter-file.lisp - File loading for Tier 1 interpreter
;;;; Feature 044: Interpreter Bootstrap Strategy

(in-package #:clysm/eval/interpreter)

;;; ============================================================
;;; File Loading Infrastructure
;;; ============================================================

(defvar *current-loading-file* nil
  "Currently loading file path, for error reporting.")

(defvar *interpreted-packages* (make-hash-table :test 'equal)
  "Package registry for interpreted file loading.")

(define-condition interpreter-file-error (error)
  ((path :initarg :path :reader ife-path)
   (reason :initarg :reason :reader ife-reason))
  (:report (lambda (c s)
             (format s "Error loading ~A: ~A" (ife-path c) (ife-reason c)))))

;;; ============================================================
;;; File Reading and Parsing
;;; ============================================================

(defun read-file-forms (path)
  "Read all forms from file at PATH.
   Returns a list of S-expressions."
  (with-open-file (stream path :direction :input
                               :external-format :utf-8)
    (let ((forms nil)
          (eof (gensym "EOF")))
      (handler-case
          (loop for form = (read stream nil eof)
                until (eq form eof)
                do (push form forms))
        (error (c)
          (error 'interpreter-file-error
                 :path path
                 :reason (format nil "Read error: ~A" c))))
      (nreverse forms))))

;;; ============================================================
;;; Declaration Filtering
;;; ============================================================

(defun declaration-form-p (form)
  "Check if FORM is a declaration form to be skipped."
  (and (consp form)
       (member (first form) '(declare declaim proclaim))))

(defun filter-body-declarations (body)
  "Filter out (declare ...) forms from BODY.
   Returns (values filtered-body declarations)."
  (let ((declarations nil)
        (filtered nil))
    (dolist (form body)
      (if (and (consp form) (eq (first form) 'declare))
          (push form declarations)
          (push form filtered)))
    (values (nreverse filtered) (nreverse declarations))))

;;; ============================================================
;;; Package Handling
;;; ============================================================

(defun interpret-in-package (package-designator env)
  "Handle (in-package ...) form during file loading.
   Returns the package object."
  (declare (ignore env))
  (let ((pkg (find-package package-designator)))
    (unless pkg
      (error 'interpreter-file-error
             :path *current-loading-file*
             :reason (format nil "Package not found: ~S" package-designator)))
    (setf *package* pkg)
    pkg))

;;; ============================================================
;;; Main File Loading Function
;;; ============================================================

(defun interpret-file (path &key (verbose nil) (env nil))
  "Load and interpret file at PATH.
   Each form is evaluated in sequence.
   Returns the value of the last form."
  (let ((*current-loading-file* path)
        (*package* *package*)  ; Bind to allow in-package changes
        (env (or env (get-default-env)))
        (result nil))
    (when verbose
      (format t "~&; Loading ~A~%" path))
    (handler-case
        (let ((forms (read-file-forms path)))
          (dolist (form forms)
            (setf result (interpret-toplevel-form form env verbose))))
      (error (c)
        (error 'interpreter-file-error
               :path path
               :reason (format nil "~A" c))))
    (when verbose
      (format t "~&; Loaded ~A~%" path))
    result))

(defun interpret-toplevel-form (form env &optional verbose)
  "Interpret a top-level form during file loading.
   Handles in-package specially."
  (cond
    ;; Skip declarations
    ((declaration-form-p form)
     (when verbose
       (format t "~&;   Skipping declaration~%"))
     nil)
    ;; Handle in-package
    ((and (consp form) (eq (first form) 'in-package))
     (interpret-in-package (second form) env))
    ;; Handle eval-when
    ((and (consp form) (eq (first form) 'eval-when))
     (interpret-eval-when form env))
    ;; Normal form
    (t
     (interpret form env))))

;;; ============================================================
;;; Eval-When Handling
;;; ============================================================

(defun interpret-eval-when (form env)
  "Handle (eval-when (situations...) body...) during file loading.
   Only :execute situation is relevant for interpreter."
  (let ((situations (second form))
        (body (cddr form)))
    ;; Interpreter always runs in :execute situation
    (when (or (member :execute situations)
              (member 'eval situations)  ; Old-style
              (member :load-toplevel situations))
      (interpret-progn body env))))

;;; ============================================================
;;; Module Loading for Bootstrap
;;; ============================================================

(defun load-compiler-modules (module-paths &key (verbose nil) (env nil))
  "Load a list of compiler module files in order.
   Returns the number of modules successfully loaded."
  (let ((loaded 0)
        (env (or env (get-default-env))))
    (dolist (path module-paths)
      (handler-case
          (progn
            (interpret-file path :verbose verbose :env env)
            (incf loaded))
        (error (c)
          (format *error-output*
                  "~&; Error loading ~A: ~A~%" path c))))
    loaded))
