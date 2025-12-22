;;;; combination.lisp - Method combination (Phase 10 - T239-T242)

(in-package #:clysm/clos/combination)

;;; ============================================================
;;; Next Method Stack
;;; ============================================================

(defvar *next-methods* nil
  "Stack of next methods for call-next-method.")

(defvar *current-args* nil
  "Current arguments for call-next-method.")

;;; ============================================================
;;; call-next-method (T242)
;;; ============================================================

(defun call-next-method* (&rest args)
  "Call the next method in the method chain.
   If ARGS is provided, use those arguments instead of the original ones."
  (unless *next-methods*
    (error "No next method"))
  (let ((next (first *next-methods*)))
    (if (functionp next)
        ;; It's a function (continuation for around methods)
        (apply next (or args *current-args*))
        ;; It's a method object
        (let ((*next-methods* (rest *next-methods*)))
          (apply (clysm/clos/generic:method*-function next)
                 (or args *current-args*))))))

;;; ============================================================
;;; next-method-p (T242)
;;; ============================================================

(defun next-method-p* ()
  "Check if there is a next method."
  (not (null *next-methods*)))

;;; ============================================================
;;; Method Combination Types
;;; ============================================================

(defvar *method-combinations* (make-hash-table :test 'eq)
  "Registry of method combination types.")

(defun register-method-combination (name combiner)
  "Register a method combination type."
  (setf (gethash name *method-combinations*) combiner))

(defun find-method-combination (name)
  "Find a method combination type."
  (gethash name *method-combinations*))

;;; ============================================================
;;; Standard Method Combination
;;; ============================================================

(defun standard-method-combination (methods args)
  "Apply standard method combination to methods."
  (let ((before (remove-if-not (lambda (m)
                                  (eq (clysm/clos/generic:method*-qualifier m) :before))
                                methods))
        (primary (remove-if-not (lambda (m)
                                   (null (clysm/clos/generic:method*-qualifier m)))
                                 methods))
        (after (remove-if-not (lambda (m)
                                 (eq (clysm/clos/generic:method*-qualifier m) :after))
                               methods))
        (around (remove-if-not (lambda (m)
                                  (eq (clysm/clos/generic:method*-qualifier m) :around))
                                methods)))
    (flet ((run-primary ()
             ;; Run before methods
             (dolist (m before)
               (apply (clysm/clos/generic:method*-function m) args))
             ;; Run primary method
             (let ((result (when primary
                             (let ((*next-methods* (rest primary))
                                   (*current-args* args))
                               (apply (clysm/clos/generic:method*-function (first primary))
                                      args)))))
               ;; Run after methods (reverse order)
               (dolist (m (reverse after))
                 (apply (clysm/clos/generic:method*-function m) args))
               result)))
      (if around
          ;; Run around methods
          (let ((*next-methods* (append (rest around)
                                        (list (lambda (&rest call-args)
                                                (let ((*current-args* (or call-args args)))
                                                  (run-primary))))))
                (*current-args* args))
            (apply (clysm/clos/generic:method*-function (first around)) args))
          ;; No around methods
          (run-primary)))))

;; Register standard method combination
(register-method-combination :standard #'standard-method-combination)

