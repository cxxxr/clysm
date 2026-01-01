;;;; list-ops.lisp - Runtime Library List Operations
;;;; Feature 001-runtime-library-system: Layer 2 Runtime Library
;;;;
;;;; This file contains Layer 2 runtime library functions implemented in Lisp.
;;;; These functions are compiled to WasmGC using the Clysm compiler.
;;;; They build on Layer 1 primitives (cons, car, cdr, etc.).
;;;;
;;;; Functions in this file:
;;;;   - assoc (T039)
;;;;   - member (T040)
;;;;   - find (T041)
;;;;   - position (T042)
;;;;   - nth, nthcdr (T043)
;;;;
;;;; Note: This file is loaded by the runtime library loader and compiled
;;;; as part of the runtime library module.

;;; ============================================================================
;;; Association List Operations (T039)
;;; ============================================================================

(defun assoc (item alist &key key test test-not)
  "Return the first cons in ALIST whose car is equal to ITEM.

If KEY is supplied, it is applied to the car of each pair before comparison.
TEST specifies the comparison function (default is EQL).
TEST-NOT specifies a function that should return NIL for a match.

See: resources/HyperSpec/Body/f_assocc.htm"
  (cond
    ;; Handle test-not
    (test-not
     (dolist (pair alist nil)
       (when (and (consp pair)
                  (not (funcall test-not item
                               (if key (funcall key (car pair)) (car pair)))))
         (return pair))))
    ;; Handle explicit test
    (test
     (dolist (pair alist nil)
       (when (and (consp pair)
                  (funcall test item
                           (if key (funcall key (car pair)) (car pair))))
         (return pair))))
    ;; Handle key only (default eql test)
    (key
     (dolist (pair alist nil)
       (when (and (consp pair)
                  (eql item (funcall key (car pair))))
         (return pair))))
    ;; Default: eql comparison on car
    (t
     (dolist (pair alist nil)
       (when (and (consp pair) (eql item (car pair)))
         (return pair))))))

(defun assoc-if (predicate alist &key key)
  "Return the first cons in ALIST whose car satisfies PREDICATE.

If KEY is supplied, it is applied to the car of each pair before testing.

See: resources/HyperSpec/Body/f_assocc.htm"
  (if key
      (dolist (pair alist nil)
        (when (and (consp pair)
                   (funcall predicate (funcall key (car pair))))
          (return pair)))
      (dolist (pair alist nil)
        (when (and (consp pair)
                   (funcall predicate (car pair)))
          (return pair)))))

(defun assoc-if-not (predicate alist &key key)
  "Return the first cons in ALIST whose car does not satisfy PREDICATE.

If KEY is supplied, it is applied to the car of each pair before testing.

See: resources/HyperSpec/Body/f_assocc.htm"
  (if key
      (dolist (pair alist nil)
        (when (and (consp pair)
                   (not (funcall predicate (funcall key (car pair)))))
          (return pair)))
      (dolist (pair alist nil)
        (when (and (consp pair)
                   (not (funcall predicate (car pair))))
          (return pair)))))

;;; ============================================================================
;;; Member Operations (T040)
;;; ============================================================================

(defun member (item list &key key test test-not)
  "Return the tail of LIST beginning with an element equal to ITEM.

If KEY is supplied, it is applied to each element before comparison.
TEST specifies the comparison function (default is EQL).
TEST-NOT specifies a function that should return NIL for a match.

See: resources/HyperSpec/Body/f_mem_m.htm"
  (cond
    ;; Handle test-not
    (test-not
     (do ((l list (cdr l)))
         ((null l) nil)
       (when (not (funcall test-not item
                           (if key (funcall key (car l)) (car l))))
         (return l))))
    ;; Handle explicit test
    (test
     (do ((l list (cdr l)))
         ((null l) nil)
       (when (funcall test item
                      (if key (funcall key (car l)) (car l)))
         (return l))))
    ;; Handle key only (default eql test)
    (key
     (do ((l list (cdr l)))
         ((null l) nil)
       (when (eql item (funcall key (car l)))
         (return l))))
    ;; Default: eql comparison
    (t
     (do ((l list (cdr l)))
         ((null l) nil)
       (when (eql item (car l))
         (return l))))))

(defun member-if (predicate list &key key)
  "Return the tail of LIST beginning with an element satisfying PREDICATE.

If KEY is supplied, it is applied to each element before testing.

See: resources/HyperSpec/Body/f_mem_m.htm"
  (if key
      (do ((l list (cdr l)))
          ((null l) nil)
        (when (funcall predicate (funcall key (car l)))
          (return l)))
      (do ((l list (cdr l)))
          ((null l) nil)
        (when (funcall predicate (car l))
          (return l)))))

(defun member-if-not (predicate list &key key)
  "Return the tail of LIST beginning with an element not satisfying PREDICATE.

If KEY is supplied, it is applied to each element before testing.

See: resources/HyperSpec/Body/f_mem_m.htm"
  (if key
      (do ((l list (cdr l)))
          ((null l) nil)
        (when (not (funcall predicate (funcall key (car l))))
          (return l)))
      (do ((l list (cdr l)))
          ((null l) nil)
        (when (not (funcall predicate (car l)))
          (return l)))))

;;; ============================================================================
;;; Find Operations (T041)
;;; ============================================================================

(defun find (item sequence &key from-end test test-not start end key)
  "Return the first element of SEQUENCE that is equal to ITEM.

START and END specify the subsequence to search (default: entire sequence).
FROM-END if true searches from the end.
KEY is applied to each element before comparison.
TEST specifies the comparison function (default is EQL).
TEST-NOT specifies a function that should return NIL for a match.

See: resources/HyperSpec/Body/f_find_.htm"
  (let* ((len (length sequence))
         (start (or start 0))
         (end (or end len)))
    (if from-end
        ;; Search from end
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (cond (test-not (not (funcall test-not item key-val)))
                        (test (funcall test item key-val))
                        (t (eql item key-val)))
              (return elt))))
        ;; Search from start
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (cond (test-not (not (funcall test-not item key-val)))
                        (test (funcall test item key-val))
                        (t (eql item key-val)))
              (return elt)))))))

(defun find-if (predicate sequence &key from-end start end key)
  "Return the first element of SEQUENCE that satisfies PREDICATE.

See: resources/HyperSpec/Body/f_find_.htm"
  (let* ((len (length sequence))
         (start (or start 0))
         (end (or end len)))
    (if from-end
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (funcall predicate key-val)
              (return elt))))
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (funcall predicate key-val)
              (return elt)))))))

(defun find-if-not (predicate sequence &key from-end start end key)
  "Return the first element of SEQUENCE that does not satisfy PREDICATE.

See: resources/HyperSpec/Body/f_find_.htm"
  (let* ((len (length sequence))
         (start (or start 0))
         (end (or end len)))
    (if from-end
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (not (funcall predicate key-val))
              (return elt))))
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (not (funcall predicate key-val))
              (return elt)))))))

;;; ============================================================================
;;; Position Operations (T042)
;;; ============================================================================

(defun position (item sequence &key from-end test test-not start end key)
  "Return the index of the first element in SEQUENCE equal to ITEM.

Returns NIL if no such element is found.

See: resources/HyperSpec/Body/f_pos_p.htm"
  (let* ((len (length sequence))
         (start (or start 0))
         (end (or end len)))
    (if from-end
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (cond (test-not (not (funcall test-not item key-val)))
                        (test (funcall test item key-val))
                        (t (eql item key-val)))
              (return i))))
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (cond (test-not (not (funcall test-not item key-val)))
                        (test (funcall test item key-val))
                        (t (eql item key-val)))
              (return i)))))))

(defun position-if (predicate sequence &key from-end start end key)
  "Return the index of the first element in SEQUENCE satisfying PREDICATE.

See: resources/HyperSpec/Body/f_pos_p.htm"
  (let* ((len (length sequence))
         (start (or start 0))
         (end (or end len)))
    (if from-end
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (funcall predicate key-val)
              (return i))))
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (funcall predicate key-val)
              (return i)))))))

(defun position-if-not (predicate sequence &key from-end start end key)
  "Return the index of the first element not satisfying PREDICATE.

See: resources/HyperSpec/Body/f_pos_p.htm"
  (let* ((len (length sequence))
         (start (or start 0))
         (end (or end len)))
    (if from-end
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (not (funcall predicate key-val))
              (return i))))
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (let* ((elt (elt sequence i))
                 (key-val (if key (funcall key elt) elt)))
            (when (not (funcall predicate key-val))
              (return i)))))))

;;; ============================================================================
;;; List Access Operations (T043)
;;; ============================================================================

(defun nth (n list)
  "Return the Nth element of LIST (0-indexed).

See: resources/HyperSpec/Body/f_nth.htm"
  (car (nthcdr n list)))

(defun nthcdr (n list)
  "Return the Nth cdr of LIST.

See: resources/HyperSpec/Body/f_nthcdr.htm"
  (do ((i 0 (1+ i))
       (result list (cdr result)))
      ((or (>= i n) (null result)) result)))
