;;;; sequences.lisp - ANSI Common Lisp sequence generic functions
;;;;
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; Purpose: Implement ANSI CL sequence generic functions for Clysm compiler
;;;;
;;;; Functions implemented:
;;;;   Counting: count, count-if, count-if-not
;;;;   Search: find, find-if, find-if-not, position, position-if, position-if-not
;;;;   Comparison: mismatch, search
;;;;   Substitution: substitute, substitute-if, substitute-if-not,
;;;;                 nsubstitute, nsubstitute-if, nsubstitute-if-not
;;;;   Duplicates: remove-duplicates, delete-duplicates
;;;;   Modification: fill, replace
;;;;
;;;; HyperSpec references:
;;;;   [count](resources/HyperSpec/Body/f_countc.htm)
;;;;   [find](resources/HyperSpec/Body/f_find_.htm)
;;;;   [position](resources/HyperSpec/Body/f_pos_p.htm)
;;;;   [mismatch](resources/HyperSpec/Body/f_mismat.htm)
;;;;   [search](resources/HyperSpec/Body/f_search.htm)
;;;;   [substitute](resources/HyperSpec/Body/f_substc.htm)
;;;;   [remove-duplicates](resources/HyperSpec/Body/f_rm_dup.htm)
;;;;   [fill](resources/HyperSpec/Body/f_fill.htm)
;;;;   [replace](resources/HyperSpec/Body/f_replac.htm)
;;;;
;;;; All functions use *-suffix naming convention to avoid shadowing SBCL builtins.

(in-package #:clysm)

;;; ============================================================
;;; Counting Functions
;;; [count](resources/HyperSpec/Body/f_countc.htm)
;;; ============================================================

;; [count](resources/HyperSpec/Body/f_countc.htm)
;; Returns the number of elements in sequence that satisfy the test.
(defun count* (item sequence &key (test #'eql) (key #'identity)
                                  (start 0) end from-end)
  "Count occurrences of ITEM in SEQUENCE.
Returns the number of elements that satisfy (funcall test item (funcall key element)).
Keyword arguments:
  :test  - comparison function (default #'eql)
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, process from end (doesn't affect count result)"
  (declare (ignore from-end))  ; from-end doesn't affect count
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds))
         (count 0))
    (loop for i from real-start below real-end
          for elt = (%sequence-ref sequence i)
          when (funcall test item (funcall key elt))
            do (incf count))
    count))

;; [count-if](resources/HyperSpec/Body/f_countc.htm)
;; Returns the number of elements satisfying the predicate.
(defun count-if* (predicate sequence &key (key #'identity)
                                          (start 0) end from-end)
  "Count elements in SEQUENCE satisfying PREDICATE.
Returns the number of elements where (funcall predicate (funcall key element)) is true.
Keyword arguments:
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, process from end (doesn't affect count result)"
  (declare (ignore from-end))
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds))
         (count 0))
    (loop for i from real-start below real-end
          for elt = (%sequence-ref sequence i)
          when (funcall predicate (funcall key elt))
            do (incf count))
    count))

;; [count-if-not](resources/HyperSpec/Body/f_countc.htm)
;; Returns the number of elements NOT satisfying the predicate.
(defun count-if-not* (predicate sequence &key (key #'identity)
                                              (start 0) end from-end)
  "Count elements in SEQUENCE NOT satisfying PREDICATE.
Returns the number of elements where (funcall predicate (funcall key element)) is false.
Keyword arguments:
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, process from end (doesn't affect count result)"
  (declare (ignore from-end))
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds))
         (count 0))
    (loop for i from real-start below real-end
          for elt = (%sequence-ref sequence i)
          unless (funcall predicate (funcall key elt))
            do (incf count))
    count))

;;; ============================================================
;;; Search Functions
;;; [find](resources/HyperSpec/Body/f_find_.htm)
;;; [position](resources/HyperSpec/Body/f_pos_p.htm)
;;; ============================================================

;; [find](resources/HyperSpec/Body/f_find_.htm)
;; Returns the first element satisfying the test.
(defun find* (item sequence &key (test #'eql) (key #'identity)
                                 (start 0) end from-end)
  "Find ITEM in SEQUENCE.
Returns the first element where (funcall test item (funcall key element)) is true.
Returns NIL if no element matches.
Keyword arguments:
  :test  - comparison function (default #'eql)
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, search from end and return last matching element"
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds)))
    (if from-end
        (loop for i from (1- real-end) downto real-start
              for elt = (%sequence-ref sequence i)
              when (funcall test item (funcall key elt))
                return elt)
        (loop for i from real-start below real-end
              for elt = (%sequence-ref sequence i)
              when (funcall test item (funcall key elt))
                return elt))))

;; [find-if](resources/HyperSpec/Body/f_find_.htm)
;; Returns the first element satisfying the predicate.
(defun find-if* (predicate sequence &key (key #'identity)
                                         (start 0) end from-end)
  "Find first element in SEQUENCE satisfying PREDICATE.
Returns the element, or NIL if no element satisfies.
Keyword arguments:
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, return last satisfying element"
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds)))
    (if from-end
        (loop for i from (1- real-end) downto real-start
              for elt = (%sequence-ref sequence i)
              when (funcall predicate (funcall key elt))
                return elt)
        (loop for i from real-start below real-end
              for elt = (%sequence-ref sequence i)
              when (funcall predicate (funcall key elt))
                return elt))))

;; [find-if-not](resources/HyperSpec/Body/f_find_.htm)
;; Returns the first element NOT satisfying the predicate.
(defun find-if-not* (predicate sequence &key (key #'identity)
                                             (start 0) end from-end)
  "Find first element in SEQUENCE NOT satisfying PREDICATE.
Returns the element, or NIL if all elements satisfy.
Keyword arguments:
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, return last non-satisfying element"
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds)))
    (if from-end
        (loop for i from (1- real-end) downto real-start
              for elt = (%sequence-ref sequence i)
              unless (funcall predicate (funcall key elt))
                return elt)
        (loop for i from real-start below real-end
              for elt = (%sequence-ref sequence i)
              unless (funcall predicate (funcall key elt))
                return elt))))

;; [position](resources/HyperSpec/Body/f_pos_p.htm)
;; Returns the index of the first matching element.
(defun position* (item sequence &key (test #'eql) (key #'identity)
                                     (start 0) end from-end)
  "Find position of ITEM in SEQUENCE.
Returns the index of the first element satisfying the test, or NIL if not found.
Keyword arguments:
  :test  - comparison function (default #'eql)
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, return index of last matching element"
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds)))
    (if from-end
        (loop for i from (1- real-end) downto real-start
              for elt = (%sequence-ref sequence i)
              when (funcall test item (funcall key elt))
                return i)
        (loop for i from real-start below real-end
              for elt = (%sequence-ref sequence i)
              when (funcall test item (funcall key elt))
                return i))))

;; [position-if](resources/HyperSpec/Body/f_pos_p.htm)
;; Returns the index of the first element satisfying the predicate.
(defun position-if* (predicate sequence &key (key #'identity)
                                             (start 0) end from-end)
  "Find position of first element in SEQUENCE satisfying PREDICATE.
Returns the index, or NIL if no element satisfies.
Keyword arguments:
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, return index of last satisfying element"
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds)))
    (if from-end
        (loop for i from (1- real-end) downto real-start
              for elt = (%sequence-ref sequence i)
              when (funcall predicate (funcall key elt))
                return i)
        (loop for i from real-start below real-end
              for elt = (%sequence-ref sequence i)
              when (funcall predicate (funcall key elt))
                return i))))

;; [position-if-not](resources/HyperSpec/Body/f_pos_p.htm)
;; Returns the index of the first element NOT satisfying the predicate.
(defun position-if-not* (predicate sequence &key (key #'identity)
                                                 (start 0) end from-end)
  "Find position of first element in SEQUENCE NOT satisfying PREDICATE.
Returns the index, or NIL if all elements satisfy.
Keyword arguments:
  :key   - key function to extract comparison value (default #'identity)
  :start - start index (default 0)
  :end   - end index (default NIL meaning end of sequence)
  :from-end - if true, return index of last non-satisfying element"
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds)))
    (if from-end
        (loop for i from (1- real-end) downto real-start
              for elt = (%sequence-ref sequence i)
              unless (funcall predicate (funcall key elt))
                return i)
        (loop for i from real-start below real-end
              for elt = (%sequence-ref sequence i)
              unless (funcall predicate (funcall key elt))
                return i))))

;;; ============================================================
;;; Comparison Functions
;;; [mismatch](resources/HyperSpec/Body/f_mismat.htm)
;;; [search](resources/HyperSpec/Body/f_search.htm)
;;; ============================================================

;; [mismatch](resources/HyperSpec/Body/f_mismat.htm)
;; Returns the index where two sequences first differ.
(defun mismatch* (sequence1 sequence2 &key (test #'eql) (key #'identity)
                                           (start1 0) end1 (start2 0) end2
                                           from-end)
  "Compare SEQUENCE1 and SEQUENCE2, return index of first mismatch or NIL if equal.
Keyword arguments:
  :test  - comparison function (default #'eql)
  :key   - key function (default #'identity)
  :start1, :end1 - bounds for sequence1
  :start2, :end2 - bounds for sequence2
  :from-end - if true, compare from end"
  (let* ((len1 (%sequence-length sequence1))
         (len2 (%sequence-length sequence2)))
    (multiple-value-bind (s1 e1 s2 e2)
        (validate-two-sequence-bounds start1 end1 len1 start2 end2 len2)
      (let ((range1 (- e1 s1))
            (range2 (- e2 s2)))
        (if from-end
            ;; Compare from end
            (loop for offset from 1 to (min range1 range2)
                  for i1 = (- e1 offset)
                  for i2 = (- e2 offset)
                  for elt1 = (%sequence-ref sequence1 i1)
                  for elt2 = (%sequence-ref sequence2 i2)
                  unless (funcall test (funcall key elt1) (funcall key elt2))
                    return (1+ i1)
                  finally (return (if (= range1 range2) nil (+ s1 (min range1 range2)))))
            ;; Compare from start
            (loop for i from 0 below (min range1 range2)
                  for i1 = (+ s1 i)
                  for i2 = (+ s2 i)
                  for elt1 = (%sequence-ref sequence1 i1)
                  for elt2 = (%sequence-ref sequence2 i2)
                  unless (funcall test (funcall key elt1) (funcall key elt2))
                    return i1
                  finally (return (if (= range1 range2) nil i1))))))))

;; [search](resources/HyperSpec/Body/f_search.htm)
;; Searches for a subsequence within a sequence.
(defun search* (sequence1 sequence2 &key (test #'eql) (key #'identity)
                                         (start1 0) end1 (start2 0) end2
                                         from-end)
  "Search for SEQUENCE1 as a subsequence of SEQUENCE2.
Returns the index in SEQUENCE2 where SEQUENCE1 begins, or NIL if not found.
Keyword arguments:
  :test  - comparison function (default #'eql)
  :key   - key function (default #'identity)
  :start1, :end1 - bounds for sequence1 (the needle)
  :start2, :end2 - bounds for sequence2 (the haystack)
  :from-end - if true, return rightmost match"
  (let* ((len1 (%sequence-length sequence1))
         (len2 (%sequence-length sequence2)))
    (multiple-value-bind (s1 e1 s2 e2)
        (validate-two-sequence-bounds start1 end1 len1 start2 end2 len2)
      (let* ((needle-len (- e1 s1))
             (haystack-len (- e2 s2))
             (result nil))
        (when (<= needle-len haystack-len)
          (if from-end
              ;; Search from end
              (loop for i from (- e2 needle-len) downto s2
                    when (loop for j from 0 below needle-len
                               for n-elt = (%sequence-ref sequence1 (+ s1 j))
                               for h-elt = (%sequence-ref sequence2 (+ i j))
                               always (funcall test (funcall key n-elt)
                                               (funcall key h-elt)))
                      return (setf result i))
              ;; Search from start
              (loop for i from s2 to (- e2 needle-len)
                    when (loop for j from 0 below needle-len
                               for n-elt = (%sequence-ref sequence1 (+ s1 j))
                               for h-elt = (%sequence-ref sequence2 (+ i j))
                               always (funcall test (funcall key n-elt)
                                               (funcall key h-elt)))
                      return (setf result i))))
        result))))

;;; ============================================================
;;; Substitution Functions
;;; [substitute](resources/HyperSpec/Body/f_substc.htm)
;;; ============================================================

;; Internal helper for substitute/nsubstitute
(defun %substitute-impl (newitem test-fn sequence copy-p
                         &key (start 0) end from-end count (key #'identity))
  "Internal implementation for substitute operations.
TEST-FN is a function (element) -> boolean.
If COPY-P is true, returns a new sequence; otherwise modifies in place."
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds))
         (result (if copy-p (%copy-sequence sequence) sequence))
         (replaced 0)
         (max-replacements (or count most-positive-fixnum)))
    (if from-end
        (loop for i from (1- real-end) downto real-start
              for elt = (%sequence-ref result i)
              when (and (< replaced max-replacements)
                        (funcall test-fn (funcall key elt)))
                do (progn
                     (%sequence-set result i newitem)
                     (incf replaced)))
        (loop for i from real-start below real-end
              for elt = (%sequence-ref result i)
              when (and (< replaced max-replacements)
                        (funcall test-fn (funcall key elt)))
                do (progn
                     (%sequence-set result i newitem)
                     (incf replaced))))
    result))

;; [substitute](resources/HyperSpec/Body/f_substc.htm)
(defun substitute* (newitem olditem sequence &key (test #'eql) (key #'identity)
                                                  (start 0) end from-end count)
  "Return a copy of SEQUENCE with OLDITEM replaced by NEWITEM."
  (%substitute-impl newitem
                    (lambda (elt) (funcall test olditem elt))
                    sequence t
                    :start start :end end :from-end from-end
                    :count count :key key))

;; [substitute-if](resources/HyperSpec/Body/f_substc.htm)
(defun substitute-if* (newitem predicate sequence &key (key #'identity)
                                                       (start 0) end from-end count)
  "Return a copy of SEQUENCE with elements satisfying PREDICATE replaced by NEWITEM."
  (%substitute-impl newitem predicate sequence t
                    :start start :end end :from-end from-end
                    :count count :key key))

;; [substitute-if-not](resources/HyperSpec/Body/f_substc.htm)
(defun substitute-if-not* (newitem predicate sequence &key (key #'identity)
                                                           (start 0) end from-end count)
  "Return a copy of SEQUENCE with elements NOT satisfying PREDICATE replaced."
  (%substitute-impl newitem
                    (lambda (elt) (not (funcall predicate elt)))
                    sequence t
                    :start start :end end :from-end from-end
                    :count count :key key))

;; [nsubstitute](resources/HyperSpec/Body/f_substc.htm)
(defun nsubstitute* (newitem olditem sequence &key (test #'eql) (key #'identity)
                                                   (start 0) end from-end count)
  "Destructively modify SEQUENCE, replacing OLDITEM with NEWITEM."
  (%substitute-impl newitem
                    (lambda (elt) (funcall test olditem elt))
                    sequence nil
                    :start start :end end :from-end from-end
                    :count count :key key))

;; [nsubstitute-if](resources/HyperSpec/Body/f_substc.htm)
(defun nsubstitute-if* (newitem predicate sequence &key (key #'identity)
                                                        (start 0) end from-end count)
  "Destructively modify SEQUENCE, replacing elements satisfying PREDICATE."
  (%substitute-impl newitem predicate sequence nil
                    :start start :end end :from-end from-end
                    :count count :key key))

;; [nsubstitute-if-not](resources/HyperSpec/Body/f_substc.htm)
(defun nsubstitute-if-not* (newitem predicate sequence &key (key #'identity)
                                                            (start 0) end from-end count)
  "Destructively modify SEQUENCE, replacing elements NOT satisfying PREDICATE."
  (%substitute-impl newitem
                    (lambda (elt) (not (funcall predicate elt)))
                    sequence nil
                    :start start :end end :from-end from-end
                    :count count :key key))

;;; ============================================================
;;; Duplicate Removal Functions
;;; [remove-duplicates](resources/HyperSpec/Body/f_rm_dup.htm)
;;; ============================================================

;; [remove-duplicates](resources/HyperSpec/Body/f_rm_dup.htm)
(defun remove-duplicates* (sequence &key (test #'eql) (key #'identity)
                                         (start 0) end from-end)
  "Return a copy of SEQUENCE with duplicate elements removed.
By default, later occurrences are kept (earlier ones removed).
With :from-end t, earlier occurrences are kept."
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds))
         (seq-type (%sequence-type sequence)))
    ;; Collect non-duplicate elements based on from-end
    (let ((result-elements nil)
          (seen nil))
      (if from-end
          ;; Keep first occurrences - scan forward, collect if not seen
          (progn
            (loop for i from real-start below real-end
                  for elt = (%sequence-ref sequence i)
                  for key-val = (funcall key elt)
                  unless (cl:member key-val seen :test test)
                    do (push elt result-elements)
                       (push key-val seen))
            (setf result-elements (nreverse result-elements)))
          ;; Keep last occurrences - scan backward, collect if not seen, then reverse
          (progn
            (loop for i from (1- real-end) downto real-start
                  for elt = (%sequence-ref sequence i)
                  for key-val = (funcall key elt)
                  unless (cl:member key-val seen :test test)
                    do (push elt result-elements)
                       (push key-val seen))))
      ;; Prepend elements before start, append elements after end
      (let ((before-elements (loop for i from 0 below real-start
                                   collect (%sequence-ref sequence i)))
            (after-elements (loop for i from real-end below len
                                  collect (%sequence-ref sequence i))))
        (setf result-elements (append before-elements result-elements after-elements)))
      ;; Convert to appropriate type
      (ecase seq-type
        (list result-elements)
        (vector (coerce result-elements 'vector))
        (string (coerce result-elements 'string))))))

;; [delete-duplicates](resources/HyperSpec/Body/f_rm_dup.htm)
;; Note: For simplicity, this implementation copies like remove-duplicates*
;; A truly destructive version would modify the sequence in place
(defun delete-duplicates* (sequence &key (test #'eql) (key #'identity)
                                         (start 0) end from-end)
  "Destructively remove duplicate elements from SEQUENCE.
May modify the original sequence."
  ;; For vectors and strings, we return the result of remove-duplicates*
  ;; For lists, we could do in-place, but for simplicity we'll just copy
  (remove-duplicates* sequence :test test :key key :start start :end end :from-end from-end))

;;; ============================================================
;;; Modification Functions
;;; [fill](resources/HyperSpec/Body/f_fill.htm)
;;; [replace](resources/HyperSpec/Body/f_replac.htm)
;;; ============================================================

;; [fill](resources/HyperSpec/Body/f_fill.htm)
(defun fill* (sequence item &key (start 0) end)
  "Destructively fill SEQUENCE with ITEM.
Returns the modified sequence."
  (let* ((len (%sequence-length sequence))
         (bounds (validate-bounding-indices start end len))
         (real-start (car bounds))
         (real-end (cdr bounds)))
    (loop for i from real-start below real-end
          do (%sequence-set sequence i item))
    sequence))

;; [replace](resources/HyperSpec/Body/f_replac.htm)
(defun replace* (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
  "Destructively copy elements from SEQUENCE2 into SEQUENCE1.
Returns the modified SEQUENCE1."
  (let* ((len1 (%sequence-length sequence1))
         (len2 (%sequence-length sequence2)))
    (multiple-value-bind (s1 e1 s2 e2)
        (validate-two-sequence-bounds start1 end1 len1 start2 end2 len2)
      (let ((copy-count (min (- e1 s1) (- e2 s2))))
        ;; Handle overlapping sequences: if s1 < s2, copy forward; else backward
        (if (and (eq sequence1 sequence2) (< s2 s1) (< s1 e2))
            ;; Overlap: copy backward
            (loop for i from (1- copy-count) downto 0
                  do (%sequence-set sequence1 (+ s1 i)
                                    (%sequence-ref sequence2 (+ s2 i))))
            ;; No overlap or safe: copy forward
            (loop for i from 0 below copy-count
                  do (%sequence-set sequence1 (+ s1 i)
                                    (%sequence-ref sequence2 (+ s2 i)))))))
    sequence1))

;;; ============================================================
;;; End of sequences.lisp
;;; ============================================================
