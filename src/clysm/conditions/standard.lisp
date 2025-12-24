;;;; standard.lisp - Standard restart implementations
;;;; Implements abort, continue, muffle-warning, use-value, store-value (FR-024 to FR-028)

(in-package #:clysm/conditions)

;;; ============================================================
;;; Muffle-Warning Function (T052)
;;; ============================================================

(defun muffle-warning (&optional condition)
  "Invoke the MUFFLE-WARNING restart to suppress a warning.
   CONDITION: Optional condition to associate with the restart search.
   FR-025: muffle-warning MUST invoke the muffle-warning restart."
  (let ((restart (find-restart 'muffle-warning condition)))
    (when restart
      (invoke-restart restart))))

;;; ============================================================
;;; Continue Function (T058)
;;; ============================================================

(defun continue (&optional condition)
  "Invoke the CONTINUE restart if available.
   CONDITION: Optional condition to associate with the restart search.
   FR-026: continue function MUST invoke the continue restart if available."
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

;;; ============================================================
;;; Abort Function (T070)
;;; ============================================================

(defun abort (&optional condition)
  "Invoke the ABORT restart if available.
   CONDITION: Optional condition to associate with the restart search.
   FR-024: abort function MUST invoke the abort restart."
  (let ((restart (find-restart 'abort condition)))
    (if restart
        (invoke-restart restart)
        ;; No abort restart - signal control-error
        (error 'control-error))))

;;; ============================================================
;;; Use-Value Function (T071)
;;; ============================================================

(defun use-value (value &optional condition)
  "Invoke the USE-VALUE restart with the given value.
   VALUE: Value to use as replacement.
   CONDITION: Optional condition to associate with the restart search.
   FR-027: use-value MUST invoke the use-value restart with value."
  (let ((restart (find-restart 'use-value condition)))
    (if restart
        (invoke-restart restart value)
        ;; No use-value restart - signal control-error
        (error 'control-error))))

;;; ============================================================
;;; Store-Value Function (T072)
;;; ============================================================

(defun store-value (value &optional condition)
  "Invoke the STORE-VALUE restart with the given value.
   VALUE: Value to store.
   CONDITION: Optional condition to associate with the restart search.
   FR-028: store-value MUST invoke the store-value restart with value."
  (let ((restart (find-restart 'store-value condition)))
    (if restart
        (invoke-restart restart value)
        ;; No store-value restart - signal control-error
        (error 'control-error))))

