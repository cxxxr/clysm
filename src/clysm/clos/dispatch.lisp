;;;; dispatch.lisp - Method dispatch (Phase 10 - T236-T238)

(in-package #:clysm/clos/dispatch)

;;; ============================================================
;;; Dispatch Cache (T237)
;;; ============================================================

(defvar *dispatch-cache* (make-hash-table :test 'equal)
  "Cache for dispatch results.")

(defun clear-dispatch-cache ()
  "Clear the dispatch cache."
  (clrhash *dispatch-cache*))

;;; ============================================================
;;; Compute Applicable Methods (T236)
;;; ============================================================

(defun compute-applicable-methods (gf args)
  "Compute applicable methods for arguments."
  (let ((classes (mapcar #'class-of-arg args))
        (applicable nil))
    ;; Find methods whose specializers match
    (dolist (method (clysm/clos/generic:gf-methods gf))
      (when (method-applicable-p method classes)
        (push method applicable)))
    ;; Sort by specificity (most specific first)
    (sort-methods applicable classes)))

(defun class-of-arg (arg)
  "Get the class name for an argument."
  (if (clysm/clos/mop:standard-instance-p arg)
      (clysm/clos/mop:class-name (clysm/clos/mop:instance-class arg))
      ;; For built-in types, return a type name
      (type-of arg)))

(defun method-applicable-p (method classes)
  "Check if a method is applicable to the given classes."
  (let ((specializers (clysm/clos/generic:method*-specializers method)))
    (and (= (length specializers) (length classes))
         (every (lambda (spec class)
                  (specializer-matches-p spec class))
                specializers classes))))

(defun specializer-matches-p (specializer class-name)
  "Check if a specializer matches a class."
  (cond
    ;; T matches everything
    ((eq specializer t) t)
    ;; Exact match
    ((eq specializer class-name) t)
    ;; Check inheritance
    (t (let ((class (clysm/clos/mop:find-class* class-name nil)))
         (when class
           (member specializer
                   (mapcar #'clysm/clos/mop:class-name
                           (clysm/clos/mop:class-precedence-list class))))))))

;;; ============================================================
;;; Method Sorting (T238)
;;; ============================================================

(defun sort-methods (methods classes)
  "Sort methods by specificity."
  (stable-sort (copy-list methods)
               (lambda (m1 m2)
                 (method-more-specific-p m1 m2 classes))))

(defun method-more-specific-p (m1 m2 classes)
  "Check if M1 is more specific than M2 for given classes."
  (let ((specs1 (clysm/clos/generic:method*-specializers m1))
        (specs2 (clysm/clos/generic:method*-specializers m2)))
    (loop for s1 in specs1
          for s2 in specs2
          for class in classes
          do (let ((cmp (compare-specializers s1 s2 class)))
               (when (not (zerop cmp))
                 (return (< cmp 0))))
          finally (return nil))))

(defun compare-specializers (s1 s2 class-name)
  "Compare two specializers. Returns -1 if s1 is more specific, 1 if s2 is, 0 if equal."
  (cond
    ((eq s1 s2) 0)
    ((eq s1 t) 1)
    ((eq s2 t) -1)
    (t (let ((class (clysm/clos/mop:find-class* class-name nil)))
         (if class
             (let* ((cpl (mapcar #'clysm/clos/mop:class-name
                                 (clysm/clos/mop:class-precedence-list class)))
                    (pos1 (position s1 cpl))
                    (pos2 (position s2 cpl)))
               (cond
                 ((and pos1 pos2) (- pos1 pos2))
                 (pos1 -1)
                 (pos2 1)
                 (t 0)))
             0)))))

;;; ============================================================
;;; Dispatch (Main Entry Point)
;;; ============================================================

(defun dispatch (gf &rest args)
  "Dispatch a generic function call."
  (let ((methods (compute-applicable-methods gf args)))
    (unless methods
      (error "No applicable method for ~S with arguments ~S"
             (clysm/clos/generic:gf-name gf) args))
    ;; Apply standard method combination
    (apply-method-combination gf methods args)))

;;; ============================================================
;;; Standard Method Combination
;;; ============================================================

(defun apply-method-combination (gf methods args)
  "Apply standard method combination."
  (declare (ignore gf))
  (let ((before-methods (filter-methods methods :before))
        (primary-methods (filter-methods methods nil))
        (after-methods (filter-methods methods :after))
        (around-methods (filter-methods methods :around)))
    (if around-methods
        ;; Around methods wrap everything
        (apply-around-methods around-methods
                              primary-methods before-methods after-methods
                              args)
        ;; Standard before/primary/after combination
        (apply-standard-combination before-methods primary-methods after-methods args))))

(defun filter-methods (methods qualifier)
  "Filter methods by qualifier."
  (remove-if-not (lambda (m)
                   (eq (clysm/clos/generic:method*-qualifier m) qualifier))
                 methods))

(defun apply-standard-combination (before-methods primary-methods after-methods args)
  "Apply standard before/primary/after combination."
  ;; Run :before methods (most specific first)
  (dolist (method before-methods)
    (apply (clysm/clos/generic:method*-function method) args))
  ;; Run primary method with call-next-method support
  (let ((result (when primary-methods
                  (let ((clysm/clos/combination:*next-methods* (rest primary-methods))
                        (clysm/clos/combination:*current-args* args))
                    (apply (clysm/clos/generic:method*-function (first primary-methods))
                           args)))))
    ;; Run :after methods (least specific first = reverse order)
    (dolist (method (reverse after-methods))
      (apply (clysm/clos/generic:method*-function method) args))
    ;; Return primary method's result
    result))

(defun apply-around-methods (around-methods primary before after args)
  "Apply :around methods."
  (if around-methods
      ;; Bind call-next-method
      (let ((clysm/clos/combination:*next-methods*
              (append (rest around-methods)
                      (list (lambda (&rest call-args)
                              (apply-standard-combination
                               before primary after
                               (or call-args args)))))))
        (apply (clysm/clos/generic:method*-function (first around-methods)) args))
      ;; No around methods, use standard
      (apply-standard-combination before primary after args)))

