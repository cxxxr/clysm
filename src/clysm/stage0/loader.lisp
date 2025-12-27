;;;; loader.lisp - Module loader for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Loads source modules for compilation

(in-package #:clysm/stage0)

;;; ============================================================
;;; Module Loading
;;; ============================================================

(defstruct loaded-module
  "A loaded source module"
  (path nil :type (or null string pathname))
  (forms nil :type list)
  (form-count 0 :type integer)
  (load-time 0.0 :type single-float))

(defun load-module (path)
  "Load a single source module.
   Returns loaded-module struct or signals error."
  (let ((start-time (get-internal-real-time)))
    (handler-case
        (let* ((source (load-file-as-string path))
               (forms (read-source-string source)))
          (make-loaded-module
           :path path
           :forms forms
           :form-count (length forms)
           :load-time (/ (- (get-internal-real-time) start-time)
                         (float internal-time-units-per-second))))
      (error (e)
        (signal-file-error path (format nil "~A" e))))))

(defun load-file-as-string (path)
  "Load file contents as string.
   Uses FFI in Wasm context, CL:READ-FILE in bootstrap."
  (with-open-file (stream path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun load-all-modules ()
  "Load all compiler modules.
   Returns list of loaded-module structs."
  (let ((modules '())
        (total-forms 0))
    (dolist (path (get-module-order))
      (let ((module (load-module path)))
        (push module modules)
        (incf total-forms (loaded-module-form-count module))
        (report-progress :module-loaded
                         :path path
                         :forms (loaded-module-form-count module))))
    (report-progress :all-modules-loaded
                     :count (length modules)
                     :total-forms total-forms)
    (nreverse modules)))

;;; ============================================================
;;; Form Filtering
;;; ============================================================

(defun filter-compilable-forms (forms)
  "Filter forms to only compilable ones"
  (remove-if-not #'compilable-form-p forms))

(defun extract-defun-forms (forms)
  "Extract only defun forms"
  (remove-if-not #'defun-form-p forms))

(defun extract-defmacro-forms (forms)
  "Extract only defmacro forms"
  (remove-if-not #'defmacro-form-p forms))
