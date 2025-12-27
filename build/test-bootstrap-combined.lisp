;;;; test-bootstrap-combined.lisp - Debug the actual bootstrap combined failure
;;;; Identify which specific forms cause "Unknown instruction: NAME"

(ql:quickload :clysm :silent t)

(in-package :cl-user)

;; Re-create key bootstrap functions
(defparameter *compilation-order*
  '("src/clysm/backend/leb128.lisp"
    "src/clysm/backend/sections.lisp"
    "src/clysm/backend/wasm-emit.lisp"))

(defparameter *skip-expansion-ops*
  '(etypecase typecase ecase ctypecase ccase check-type
    handler-case handler-bind restart-case restart-bind
    with-simple-restart ignore-errors
    defclass defmethod defgeneric
    defconstant defvar defparameter
    quote function))

(defparameter *must-expand-ops*
  '(case when unless and or prog1 prog2))

(defun compilable-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (member (car form)
               '(defun defmacro defvar defparameter defconstant
                 defstruct defclass defgeneric defmethod
                 deftype define-condition define-compiler-macro
                 define-symbol-macro define-setf-expander
                 define-modify-macro define-method-combination
                 progn let let* flet labels block tagbody
                 lambda setf setq if when unless cond case typecase
                 export)
               :test #'eq)
       (not (and (eq (car form) 'defun)
                 (consp (cadr form))
                 (eq (car (cadr form)) 'setf)))))

(defun expand-defstruct (form)
  "Expand defstruct to constructor and accessor defuns."
  (let* ((name-and-options (cadr form))
         (name (if (consp name-and-options) (car name-and-options) name-and-options))
         (body (cddr form))
         (slots (if (and (consp body) (stringp (car body))) (cdr body) body))
         (constructor-name (intern (format nil "MAKE-~A" name)))
         (predicate-name (intern (format nil "~A-P" name)))
         (result '()))
    (let ((slot-specs (mapcar (lambda (slot)
                                (if (consp slot)
                                    (list (car slot) (cadr slot))
                                    (list slot nil)))
                              slots)))
      (let* ((slot-names (mapcar #'car slot-specs))
             (lambda-list `(&key ,@slot-names)))
        (push `(defun ,constructor-name ,lambda-list
                 (list ',name ,@(mapcar #'car slot-specs)))
              result))
      (loop for spec in slot-specs
            for index from 1
            for slot-name = (car spec)
            for accessor-name = (intern (format nil "~A-~A" name slot-name))
            do (push `(defun ,accessor-name (struct) (nth ,index struct)) result))
      (push `(defun ,predicate-name (obj) (and (consp obj) (eq (car obj) ',name))) result))
    (nreverse result)))

(defun expand-form-recursive (form)
  (cond
    ((atom form) form)
    ((null form) nil)
    ((and (consp form) (symbolp (car form))
          (member (car form) *skip-expansion-ops*))
     form)
    ((and (consp form) (symbolp (car form))
          (member (car form) *must-expand-ops*))
     (handler-case
         (let ((expanded (macroexpand-1 form)))
           (if (equal expanded form)
               (cons (car form) (mapcar #'expand-form-recursive (cdr form)))
               (expand-form-recursive expanded)))
       (error () form)))
    ((and (consp form) (eq (car form) 'defmacro)) form)
    ((and (consp form) (eq (car form) 'defstruct))
     (cons 'progn (mapcar #'expand-form-recursive (expand-defstruct form))))
    ((and (consp form) (eq (car form) 'defun))
     (list* 'defun (cadr form) (caddr form)
            (mapcar #'expand-form-recursive (cdddr form))))
    ((and (consp form) (member (car form) '(let let*)))
     (list* (car form)
            (mapcar (lambda (binding)
                      (if (consp binding)
                          (list (car binding) (expand-form-recursive (cadr binding)))
                          binding))
                    (cadr form))
            (mapcar #'expand-form-recursive (cddr form))))
    ((consp form)
     (handler-case
         (multiple-value-bind (expanded expanded-p) (macroexpand-1 form)
           (if expanded-p
               (expand-form-recursive expanded)
               (cons (car form) (mapcar #'expand-form-recursive (cdr form)))))
       (error () form)))
    (t form)))

(defun read-source-file (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((forms '()) (eof (gensym)))
      (loop for form = (read stream nil eof)
            until (eq form eof)
            do (push form forms))
      (nreverse forms))))

(defun test-bootstrap-combined ()
  (let* ((base-dir (asdf:system-source-directory :clysm))
         (compile-fn #'clysm/compiler:compile-to-wasm)
         (all-forms '()))

    ;; Collect from first module only for faster testing
    (let ((path (merge-pathnames "src/clysm/backend/leb128.lisp" base-dir)))
      (format t "Reading ~A...~%" path)
      (let* ((forms (read-source-file path))
             (compilable (remove-if-not #'compilable-form-p forms))
             (expanded (mapcar #'expand-form-recursive compilable)))
        (setf all-forms expanded)))

    (format t "~%Processing ~D forms~%" (length all-forms))

    ;; Test each individually
    (let ((successful '()))
      (loop for form in all-forms
            for i from 0
            do (handler-case
                   (progn
                     (funcall compile-fn form)
                     (push form successful))
                 (error (e)
                   (format t "Form #~D FAIL: ~A~%Form: ~S~%~%" i e form))))
      (setf successful (nreverse successful))
      (format t "~D forms compile individually~%" (length successful))

      ;; Now try combined
      (when (> (length successful) 0)
        (format t "~%Testing combined (~D forms)...~%" (length successful))
        (handler-case
            (let ((bytes (funcall compile-fn `(progn ,@successful))))
              (format t "Combined OK! ~D bytes~%" (length bytes)))
          (error (e)
            (format t "Combined FAIL: ~A~%~%" e)
            ;; Print the combined form for inspection
            (format t "~%First 3 forms of combined:~%")
            (loop for form in (subseq successful 0 (min 3 (length successful)))
                  do (format t "  ~S~%" form))))))))

(test-bootstrap-combined)
