;;;; list-runtime.lisp - Runtime library list search functions
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;;
;;;; Implements list search functions (member, assoc, find, position)
;;;; using car/cdr/consp primitives.
;;;; These functions are compiled to Wasm and called via runtime dispatch.
;;;;
;;;; HyperSpec references:
;;;;   [member](resources/HyperSpec/Body/f_mem_m.htm)
;;;;   [assoc](resources/HyperSpec/Body/f_assocc.htm)
;;;;   [rassoc](resources/HyperSpec/Body/f_rassoc.htm)
;;;;   [find](resources/HyperSpec/Body/f_find_.htm)
;;;;   [position](resources/HyperSpec/Body/f_pos_p.htm)

(in-package #:clysm)

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun apply-test (item element test test-not key)
  "Apply the test function to compare ITEM with ELEMENT.
   Uses TEST if provided, otherwise uses TEST-NOT with negation.
   KEY is applied to ELEMENT before comparison."
  (let ((keyed-element (if key (funcall key element) element)))
    (cond
      (test (funcall test item keyed-element))
      (test-not (not (funcall test-not item keyed-element)))
      (t (eql item keyed-element)))))

(defun apply-predicate (predicate element key)
  "Apply PREDICATE to ELEMENT with optional KEY function."
  (let ((keyed-element (if key (funcall key element) element)))
    (funcall predicate keyed-element)))

;;; ============================================================
;;; T036: member-rt - List membership search (FR-005)
;;; ============================================================

(defun member-rt (item list &key (test nil) (test-not nil) (key nil))
  "Return the tail of LIST starting with the first element that matches ITEM.
   Uses car/cdr/consp primitives only."
  (declare (ignore test-not))
  ;; Simple implementation using only car/cdr/consp
  (let ((rest list)
        (test-fn (or test #'eql)))
    (loop while (consp rest)
          when (let ((element (car rest)))
                 (funcall test-fn item
                          (if key (funcall key element) element)))
            return rest
          do (setf rest (cdr rest)))
    nil))

;;; ============================================================
;;; T037: assoc-rt - Association list lookup (FR-007)
;;; ============================================================

(defun assoc-rt (item alist &key (test nil) (test-not nil) (key nil))
  "Return the first pair in ALIST whose car matches ITEM.
   Skips nil entries in ALIST."
  (declare (ignore test-not))
  (let ((rest alist)
        (test-fn (or test #'eql)))
    (loop while (consp rest)
          for entry = (car rest)
          when (and (consp entry)  ; Skip nil entries
                    (funcall test-fn item
                             (let ((entry-key (car entry)))
                               (if key (funcall key entry-key) entry-key))))
            return entry
          do (setf rest (cdr rest)))
    nil))

;;; ============================================================
;;; T038: rassoc-rt - Reverse association lookup
;;; ============================================================

(defun rassoc-rt (item alist &key (test nil) (test-not nil) (key nil))
  "Return the first pair in ALIST whose cdr matches ITEM."
  (declare (ignore test-not))
  (let ((rest alist)
        (test-fn (or test #'eql)))
    (loop while (consp rest)
          for entry = (car rest)
          when (and (consp entry)  ; Skip nil entries
                    (funcall test-fn item
                             (let ((entry-val (cdr entry)))
                               (if key (funcall key entry-val) entry-val))))
            return entry
          do (setf rest (cdr rest)))
    nil))

;;; ============================================================
;;; T039: find-rt - Sequence element search (FR-009)
;;; ============================================================

(defun find-rt (item sequence &key (test nil) (test-not nil) (key nil)
                                   (start 0) (end nil) (from-end nil))
  "Return the first element in SEQUENCE that matches ITEM."
  (declare (ignore test-not from-end))  ; Simplified implementation
  (let ((test-fn (or test #'eql)))
    (cond
      ;; List case
      ((listp sequence)
       (let ((rest sequence)
             (idx 0))
         ;; Skip to start
         (loop repeat start
               while (consp rest)
               do (setf rest (cdr rest))
                  (incf idx))
         ;; Search until end
         (loop while (and (consp rest)
                          (or (null end) (< idx end)))
               for element = (car rest)
               when (funcall test-fn item
                             (if key (funcall key element) element))
                 return element
               do (setf rest (cdr rest))
                  (incf idx))
         nil))
      ;; Vector/String case
      ((vectorp sequence)
       (let ((len (length sequence))
             (actual-end (or end len)))
         (loop for i from start below (min actual-end len)
               for element = (aref sequence i)
               when (funcall test-fn item
                             (if key (funcall key element) element))
                 return element)
         nil))
      (t nil))))

;;; ============================================================
;;; T040: position-rt - Sequence position search (FR-011)
;;; ============================================================

(defun position-rt (item sequence &key (test nil) (test-not nil) (key nil)
                                       (start 0) (end nil) (from-end nil))
  "Return the index of the first element in SEQUENCE that matches ITEM."
  (declare (ignore test-not from-end))  ; Simplified implementation
  (let ((test-fn (or test #'eql)))
    (cond
      ;; List case
      ((listp sequence)
       (let ((rest sequence)
             (idx 0))
         ;; Skip to start
         (loop repeat start
               while (consp rest)
               do (setf rest (cdr rest))
                  (incf idx))
         ;; Search until end
         (loop while (and (consp rest)
                          (or (null end) (< idx end)))
               for element = (car rest)
               when (funcall test-fn item
                             (if key (funcall key element) element))
                 return idx
               do (setf rest (cdr rest))
                  (incf idx))
         nil))
      ;; Vector/String case
      ((vectorp sequence)
       (let ((len (length sequence))
             (actual-end (or end len)))
         (loop for i from start below (min actual-end len)
               for element = (aref sequence i)
               when (funcall test-fn item
                             (if key (funcall key element) element))
                 return i)
         nil))
      (t nil))))

;;; ============================================================
;;; -if and -if-not variants (FR-006, FR-008, FR-010, FR-012)
;;; ============================================================

(defun member-if-rt (predicate list &key (key nil))
  "Return the tail of LIST starting with the first element satisfying PREDICATE."
  (let ((rest list))
    (loop while (consp rest)
          for element = (car rest)
          when (funcall predicate (if key (funcall key element) element))
            return rest
          do (setf rest (cdr rest)))
    nil))

(defun member-if-not-rt (predicate list &key (key nil))
  "Return the tail of LIST starting with first element NOT satisfying PREDICATE."
  (let ((rest list))
    (loop while (consp rest)
          for element = (car rest)
          when (not (funcall predicate (if key (funcall key element) element)))
            return rest
          do (setf rest (cdr rest)))
    nil))

(defun assoc-if-rt (predicate alist &key (key nil))
  "Return the first pair in ALIST whose car satisfies PREDICATE."
  (let ((rest alist))
    (loop while (consp rest)
          for entry = (car rest)
          when (and (consp entry)
                    (funcall predicate
                             (let ((entry-key (car entry)))
                               (if key (funcall key entry-key) entry-key))))
            return entry
          do (setf rest (cdr rest)))
    nil))

(defun rassoc-if-rt (predicate alist &key (key nil))
  "Return the first pair in ALIST whose cdr satisfies PREDICATE."
  (let ((rest alist))
    (loop while (consp rest)
          for entry = (car rest)
          when (and (consp entry)
                    (funcall predicate
                             (let ((entry-val (cdr entry)))
                               (if key (funcall key entry-val) entry-val))))
            return entry
          do (setf rest (cdr rest)))
    nil))

(defun find-if-rt (predicate sequence &key (key nil) (start 0) (end nil) (from-end nil))
  "Return the first element in SEQUENCE satisfying PREDICATE."
  (declare (ignore from-end))
  (cond
    ((listp sequence)
     (let ((rest sequence)
           (idx 0))
       (loop repeat start while (consp rest) do (setf rest (cdr rest)) (incf idx))
       (loop while (and (consp rest) (or (null end) (< idx end)))
             for element = (car rest)
             when (funcall predicate (if key (funcall key element) element))
               return element
             do (setf rest (cdr rest)) (incf idx))
       nil))
    ((vectorp sequence)
     (let ((len (length sequence))
           (actual-end (or end len)))
       (loop for i from start below (min actual-end len)
             for element = (aref sequence i)
             when (funcall predicate (if key (funcall key element) element))
               return element)
       nil))
    (t nil)))

(defun find-if-not-rt (predicate sequence &key (key nil) (start 0) (end nil) (from-end nil))
  "Return the first element in SEQUENCE NOT satisfying PREDICATE."
  (declare (ignore from-end))
  (cond
    ((listp sequence)
     (let ((rest sequence)
           (idx 0))
       (loop repeat start while (consp rest) do (setf rest (cdr rest)) (incf idx))
       (loop while (and (consp rest) (or (null end) (< idx end)))
             for element = (car rest)
             when (not (funcall predicate (if key (funcall key element) element)))
               return element
             do (setf rest (cdr rest)) (incf idx))
       nil))
    ((vectorp sequence)
     (let ((len (length sequence))
           (actual-end (or end len)))
       (loop for i from start below (min actual-end len)
             for element = (aref sequence i)
             when (not (funcall predicate (if key (funcall key element) element)))
               return element)
       nil))
    (t nil)))

(defun position-if-rt (predicate sequence &key (key nil) (start 0) (end nil) (from-end nil))
  "Return the index of first element in SEQUENCE satisfying PREDICATE."
  (declare (ignore from-end))
  (cond
    ((listp sequence)
     (let ((rest sequence)
           (idx 0))
       (loop repeat start while (consp rest) do (setf rest (cdr rest)) (incf idx))
       (loop while (and (consp rest) (or (null end) (< idx end)))
             for element = (car rest)
             when (funcall predicate (if key (funcall key element) element))
               return idx
             do (setf rest (cdr rest)) (incf idx))
       nil))
    ((vectorp sequence)
     (let ((len (length sequence))
           (actual-end (or end len)))
       (loop for i from start below (min actual-end len)
             for element = (aref sequence i)
             when (funcall predicate (if key (funcall key element) element))
               return i)
       nil))
    (t nil)))

(defun position-if-not-rt (predicate sequence &key (key nil) (start 0) (end nil) (from-end nil))
  "Return the index of first element in SEQUENCE NOT satisfying PREDICATE."
  (declare (ignore from-end))
  (cond
    ((listp sequence)
     (let ((rest sequence)
           (idx 0))
       (loop repeat start while (consp rest) do (setf rest (cdr rest)) (incf idx))
       (loop while (and (consp rest) (or (null end) (< idx end)))
             for element = (car rest)
             when (not (funcall predicate (if key (funcall key element) element)))
               return idx
             do (setf rest (cdr rest)) (incf idx))
       nil))
    ((vectorp sequence)
     (let ((len (length sequence))
           (actual-end (or end len)))
       (loop for i from start below (min actual-end len)
             for element = (aref sequence i)
             when (not (funcall predicate (if key (funcall key element) element)))
               return i)
       nil))
    (t nil)))
