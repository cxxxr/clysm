;;;; sequence-functions.lisp - API Contract Definitions
;;;;
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; Purpose: Define function signatures and contracts for sequence operations
;;;;
;;;; This file serves as the API contract. Implementation must conform to these signatures.

(in-package #:clysm)

;;; ============================================================
;;; Type Declarations
;;; ============================================================

;; Sequence types
;; (deftype sequence () '(or list vector string))

;; Bounding index designator
;; (deftype bounding-index-designator () '(or null (integer 0 *)))

;;; ============================================================
;;; Group 1: Counting Functions
;;; ============================================================

;; [count](resources/HyperSpec/Body/f_countc.htm)
(declaim (ftype (function (t sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function))
                             (:test (or null function)))
                          (integer 0 *))
                count*))

;; [count-if](resources/HyperSpec/Body/f_countc.htm)
(declaim (ftype (function (function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function)))
                          (integer 0 *))
                count-if*))

;; [count-if-not](resources/HyperSpec/Body/f_countc.htm)
(declaim (ftype (function (function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function)))
                          (integer 0 *))
                count-if-not*))

;;; ============================================================
;;; Group 2: Search Functions - Find
;;; ============================================================

;; [find](resources/HyperSpec/Body/f_find_.htm)
(declaim (ftype (function (t sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function))
                             (:test (or null function)))
                          t)  ; element or NIL
                find*))

;; [find-if](resources/HyperSpec/Body/f_find_.htm)
(declaim (ftype (function (function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function)))
                          t)
                find-if*))

;; [find-if-not](resources/HyperSpec/Body/f_find_.htm)
(declaim (ftype (function (function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function)))
                          t)
                find-if-not*))

;;; ============================================================
;;; Group 3: Search Functions - Position
;;; ============================================================

;; [position](resources/HyperSpec/Body/f_pos_p.htm)
(declaim (ftype (function (t sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function))
                             (:test (or null function)))
                          (or null (integer 0 *)))
                position*))

;; [position-if](resources/HyperSpec/Body/f_pos_p.htm)
(declaim (ftype (function (function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function)))
                          (or null (integer 0 *)))
                position-if*))

;; [position-if-not](resources/HyperSpec/Body/f_pos_p.htm)
(declaim (ftype (function (function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function)))
                          (or null (integer 0 *)))
                position-if-not*))

;;; ============================================================
;;; Group 4: Comparison Functions
;;; ============================================================

;; [mismatch](resources/HyperSpec/Body/f_mismat.htm)
(declaim (ftype (function (sequence sequence &key
                             (:from-end t)
                             (:test (or null function))
                             (:key (or null function))
                             (:start1 (integer 0 *))
                             (:end1 (or null (integer 0 *)))
                             (:start2 (integer 0 *))
                             (:end2 (or null (integer 0 *))))
                          (or null (integer 0 *)))
                mismatch*))

;; [search](resources/HyperSpec/Body/f_search.htm)
(declaim (ftype (function (sequence sequence &key
                             (:from-end t)
                             (:test (or null function))
                             (:key (or null function))
                             (:start1 (integer 0 *))
                             (:end1 (or null (integer 0 *)))
                             (:start2 (integer 0 *))
                             (:end2 (or null (integer 0 *))))
                          (or null (integer 0 *)))
                search*))

;;; ============================================================
;;; Group 5: Substitution Functions (Non-Destructive)
;;; ============================================================

;; [substitute](resources/HyperSpec/Body/f_substc.htm)
(declaim (ftype (function (t t sequence &key
                             (:from-end t)
                             (:test (or null function))
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:count (or null (integer 0 *)))
                             (:key (or null function)))
                          sequence)
                substitute*))

;; [substitute-if](resources/HyperSpec/Body/f_substc.htm)
(declaim (ftype (function (t function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:count (or null (integer 0 *)))
                             (:key (or null function)))
                          sequence)
                substitute-if*))

;; [substitute-if-not](resources/HyperSpec/Body/f_substc.htm)
(declaim (ftype (function (t function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:count (or null (integer 0 *)))
                             (:key (or null function)))
                          sequence)
                substitute-if-not*))

;;; ============================================================
;;; Group 6: Substitution Functions (Destructive)
;;; ============================================================

;; [nsubstitute](resources/HyperSpec/Body/f_substc.htm)
(declaim (ftype (function (t t sequence &key
                             (:from-end t)
                             (:test (or null function))
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:count (or null (integer 0 *)))
                             (:key (or null function)))
                          sequence)
                nsubstitute*))

;; [nsubstitute-if](resources/HyperSpec/Body/f_substc.htm)
(declaim (ftype (function (t function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:count (or null (integer 0 *)))
                             (:key (or null function)))
                          sequence)
                nsubstitute-if*))

;; [nsubstitute-if-not](resources/HyperSpec/Body/f_substc.htm)
(declaim (ftype (function (t function sequence &key
                             (:from-end t)
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:count (or null (integer 0 *)))
                             (:key (or null function)))
                          sequence)
                nsubstitute-if-not*))

;;; ============================================================
;;; Group 7: Duplicate Removal Functions
;;; ============================================================

;; [remove-duplicates](resources/HyperSpec/Body/f_rm_dup.htm)
(declaim (ftype (function (sequence &key
                             (:from-end t)
                             (:test (or null function))
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function)))
                          sequence)
                remove-duplicates*))

;; [delete-duplicates](resources/HyperSpec/Body/f_rm_dup.htm)
(declaim (ftype (function (sequence &key
                             (:from-end t)
                             (:test (or null function))
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *)))
                             (:key (or null function)))
                          sequence)
                delete-duplicates*))

;;; ============================================================
;;; Group 8: Modification Functions
;;; ============================================================

;; [fill](resources/HyperSpec/Body/f_fill.htm)
(declaim (ftype (function (sequence t &key
                             (:start (integer 0 *))
                             (:end (or null (integer 0 *))))
                          sequence)
                fill*))

;; [replace](resources/HyperSpec/Body/f_replac.htm)
(declaim (ftype (function (sequence sequence &key
                             (:start1 (integer 0 *))
                             (:end1 (or null (integer 0 *)))
                             (:start2 (integer 0 *))
                             (:end2 (or null (integer 0 *))))
                          sequence)
                replace*))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;; Bounds validation (internal)
(declaim (ftype (function ((integer 0 *) (or null (integer 0 *)) (integer 0 *))
                          (cons (integer 0 *) (integer 0 *)))
                validate-bounding-indices))

;;; ============================================================
;;; End of sequence-functions.lisp
;;; ============================================================
