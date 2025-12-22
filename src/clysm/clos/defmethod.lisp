;;;; defmethod.lisp - Method definition (Phase 10 - T233-T235)

(in-package #:clysm/clos/defmethod)

;;; ============================================================
;;; Method Result Structure
;;; ============================================================

(defstruct method-result
  "Result of parsing a defmethod form."
  (name nil :type symbol)
  (qualifier nil)
  (specializers nil :type list)
  (lambda-list nil :type list)
  (body nil :type list))

;;; ============================================================
;;; Defmethod Parsing (T233)
;;; ============================================================

(defun parse-defmethod (form)
  "Parse a defmethod form.
   Form: (defmethod name [qualifier] specialized-lambda-list body...)"
  (unless (and (consp form)
               (eq (first form) 'defmethod))
    (error "Not a defmethod form: ~S" form))
  (let* ((name (second form))
         (rest (cddr form))
         ;; Check for qualifier
         (qualifier (when (and rest (keywordp (first rest)))
                      (pop rest)))
         ;; Parse specialized lambda list
         (specialized-lambda-list (pop rest))
         (body rest))
    (multiple-value-bind (params specializers)
        (parse-specialized-lambda-list specialized-lambda-list)
      (make-method-result
       :name name
       :qualifier qualifier
       :specializers specializers
       :lambda-list params
       :body body))))

;;; ============================================================
;;; Specializer Processing (T234)
;;; ============================================================

(defun parse-specialized-lambda-list (lambda-list)
  "Parse a specialized lambda list, returning parameters and specializers."
  (let ((params nil)
        (specializers nil))
    (dolist (param lambda-list)
      (if (consp param)
          ;; (name class-name)
          (progn
            (push (first param) params)
            (push (second param) specializers))
          ;; Just a name (specializes on T)
          (progn
            (push param params)
            (push t specializers))))
    (values (nreverse params) (nreverse specializers))))

;;; ============================================================
;;; Method Creation
;;; ============================================================

;; Re-export make-method* from clysm/clos/generic
;; This is already provided by the defstruct

;;; ============================================================
;;; Method Registration (T235)
;;; ============================================================

(defun add-method* (gf method)
  "Add a method to a generic function."
  (clysm/clos/generic:gf-add-method gf method))

;;; ============================================================
;;; Define Method
;;; ============================================================

(defun define-method* (gf-name qualifier specializers lambda-list body)
  "Define and register a method."
  (let ((gf (or (clysm/clos/generic:find-gf gf-name)
                (clysm/clos/generic:defgeneric* gf-name lambda-list))))
    (let ((method (make-method*
                   :specializers specializers
                   :qualifier qualifier
                   :lambda-list lambda-list
                   :function (compile nil `(lambda ,lambda-list ,@body)))))
      (add-method* gf method)
      method)))

