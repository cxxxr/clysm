;;;; sequences-util.lisp - Utility functions for sequence operations
;;;;
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; Purpose: Provide shared utilities for sequence generic functions
;;;;
;;;; Utilities:
;;;;   validate-bounding-indices - Validate :start/:end bounds
;;;;   %sequence-length - Type-dispatching length accessor
;;;;   %sequence-ref - Type-dispatching element accessor
;;;;   %make-sequence-result - Construct result sequences
;;;;
;;;; These are internal helpers used by the public sequence functions.

(in-package #:clysm)

;;; ============================================================
;;; Bounds Validation
;;; ============================================================

(defun validate-bounding-indices (start end length)
  "Validate bounding indices for sequence operations.
Returns (cons real-start real-end) if valid, signals error otherwise.
START must be a non-negative integer.
END may be NIL (meaning LENGTH) or a non-negative integer.
Constraint: 0 <= START <= END <= LENGTH"
  (declare (type (integer 0) start length))
  (let ((real-end (or end length)))
    (unless (and (<= 0 start)
                 (<= start real-end)
                 (<= real-end length))
      (error "Invalid bounding indices: start=~A end=~A length=~A"
             start real-end length))
    (cons start real-end)))

(defun validate-two-sequence-bounds (start1 end1 length1 start2 end2 length2)
  "Validate bounding indices for two-sequence operations like mismatch and search.
Returns (values start1 end1 start2 end2) with NIL ends resolved to lengths."
  (let ((bounds1 (validate-bounding-indices start1 end1 length1))
        (bounds2 (validate-bounding-indices start2 end2 length2)))
    (values (car bounds1) (cdr bounds1)
            (car bounds2) (cdr bounds2))))

;;; ============================================================
;;; Type-Dispatching Accessors
;;; ============================================================

(defun %sequence-length (sequence)
  "Return the length of SEQUENCE. Handles lists, vectors, and strings."
  (etypecase sequence
    (list (loop for x on sequence count t))
    (vector (length sequence))
    (string (length sequence))))

(defun %sequence-ref (sequence index)
  "Return element at INDEX in SEQUENCE. Handles lists, vectors, and strings."
  (etypecase sequence
    (list (nth index sequence))
    (vector (aref sequence index))
    (string (char sequence index))))

(defun %sequence-set (sequence index value)
  "Set element at INDEX in SEQUENCE to VALUE. Destructive. Returns VALUE."
  (etypecase sequence
    (list (setf (nth index sequence) value))
    (vector (setf (aref sequence index) value))
    (string (setf (char sequence index) value)))
  value)

;;; ============================================================
;;; Sequence Construction
;;; ============================================================

(defun %make-sequence-result (type length)
  "Create a new sequence of TYPE with LENGTH elements.
TYPE should be one of: list, vector, string."
  (ecase type
    (list (make-list length))
    (vector (make-array length :initial-element nil))
    (string (make-string length :initial-element #\Space))))

(defun %sequence-type (sequence)
  "Return the type symbol for SEQUENCE: list, vector, or string."
  (etypecase sequence
    (string 'string)
    (vector 'vector)
    (list 'list)))

(defun %copy-sequence (sequence)
  "Create a shallow copy of SEQUENCE preserving its type."
  (etypecase sequence
    (list (copy-list sequence))
    (string (copy-seq sequence))
    (vector (copy-seq sequence))))

;;; ============================================================
;;; Iteration Helpers
;;; ============================================================

(defun %iterate-sequence (sequence start end from-end fn)
  "Iterate over SEQUENCE[START:END] calling FN with (index element).
If FROM-END is true, iterate in reverse order.
FN should return non-NIL to stop iteration early.
Returns the index where iteration stopped, or NIL if completed."
  (if from-end
      (loop for i from (1- end) downto start
            for elt = (%sequence-ref sequence i)
            when (funcall fn i elt)
              return i)
      (loop for i from start below end
            for elt = (%sequence-ref sequence i)
            when (funcall fn i elt)
              return i)))

;;; ============================================================
;;; Test Function Helpers
;;; ============================================================

(defun %satisfies-test-p (item element test key)
  "Return T if (funcall test item (funcall key element)) is true."
  (funcall test item (funcall key element)))

(defun %satisfies-predicate-p (element predicate key)
  "Return T if (funcall predicate (funcall key element)) is true."
  (funcall predicate (funcall key element)))

;;; ============================================================
;;; End of sequences-util.lisp
;;; ============================================================
