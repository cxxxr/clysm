;;;; util.lisp - Self-contained Utilities
;;;;
;;;; No external dependencies - designed for self-hosting
;;;; These utilities replace what would typically come from alexandria

(in-package #:clysm)

;;; ============================================================
;;; Hash Table Utilities
;;; ============================================================

(defun hash-table-keys (hash-table)
  "Return a list of all keys in HASH-TABLE."
  (loop for key being the hash-keys of hash-table
        collect key))

(defun hash-table-values (hash-table)
  "Return a list of all values in HASH-TABLE."
  (loop for value being the hash-values of hash-table
        collect value))

;;; ============================================================
;;; List Utilities
;;; ============================================================

(defun flatten (tree)
  "Flatten a nested list structure into a single list."
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (flatten (car tree))
                   (flatten (cdr tree))))))

(defun mappend (function list)
  "Map FUNCTION over LIST and append the results."
  (apply #'append (mapcar function list)))

(defun lastcar (list)
  "Return the last element of LIST."
  (car (last list)))

(defun ensure-list (thing)
  "If THING is a list, return it. Otherwise wrap it in a list."
  (if (listp thing)
      thing
      (list thing)))

(defun alist-get (key alist &key (test #'eql) default)
  "Get the value associated with KEY in ALIST."
  (let ((pair (assoc key alist :test test)))
    (if pair
        (cdr pair)
        default)))

(defun plist-get (plist key &key default)
  "Get the value associated with KEY in PLIST."
  (let ((tail (member key plist)))
    (if tail
        (cadr tail)
        default)))

;;; ============================================================
;;; Sequence Utilities
;;; ============================================================

(defun split-sequence (delimiter sequence &key (test #'eql))
  "Split SEQUENCE at each occurrence of DELIMITER."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length sequence)
          when (funcall test delimiter (elt sequence i))
            do (push (subseq sequence start i) result)
               (setf start (1+ i)))
    (push (subseq sequence start) result)
    (nreverse result)))

;;; ============================================================
;;; Control Flow Macros
;;; ============================================================

(defmacro if-let ((var form) then &optional else)
  "Bind VAR to the result of FORM; if non-nil execute THEN, else ELSE."
  `(let ((,var ,form))
     (if ,var ,then ,else)))

(defmacro when-let ((var form) &body body)
  "Bind VAR to the result of FORM; if non-nil execute BODY."
  `(let ((,var ,form))
     (when ,var ,@body)))

(defmacro if-let* (bindings then &optional else)
  "Like IF-LET but with multiple sequential bindings.
All bindings must be non-nil for THEN to execute."
  (if (null bindings)
      then
      (let ((binding (first bindings)))
        `(if-let ,binding
             (if-let* ,(rest bindings) ,then ,else)
             ,else))))

;;; ============================================================
;;; Symbol/Gensym Utilities
;;; ============================================================

(defmacro with-gensyms (names &body body)
  "Bind each name in NAMES to a fresh gensym."
  `(let ,(mapcar (lambda (name)
                   `(,name (gensym ,(string name))))
                 names)
     ,@body))

(defmacro once-only (names &body body)
  "Evaluate each form in NAMES only once, binding to gensyms."
  (let ((gensyms (mapcar (lambda (n) (gensym (string n))) names)))
    `(let ,(mapcar #'list gensyms names)
       `(let ,(list ,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let ,(mapcar #'list names gensyms)
             ,@body)))))

;;; ============================================================
;;; Byte/Binary Utilities
;;; ============================================================

(defun bytes-to-vector (bytes)
  "Convert a list of bytes to a vector of (unsigned-byte 8)."
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(defun vector-to-bytes (vector)
  "Convert a vector to a list of bytes."
  (coerce vector 'list))

(defun concat-bytes (&rest byte-sequences)
  "Concatenate multiple byte sequences (lists or vectors) into a single vector."
  (let* ((lists (mapcar (lambda (seq)
                          (if (listp seq) seq (coerce seq 'list)))
                        byte-sequences))
         (all-bytes (apply #'append lists)))
    (bytes-to-vector all-bytes)))

;;; ============================================================
;;; String Utilities
;;; ============================================================

(defun string-join (strings &optional (separator ""))
  "Join STRINGS with SEPARATOR between each."
  (with-output-to-string (out)
    (loop for (string . rest) on strings
          do (write-string string out)
          when rest do (write-string separator out))))

(defun string-prefix-p (prefix string)
  "Return T if STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun string-suffix-p (suffix string)
  "Return T if STRING ends with SUFFIX."
  (and (<= (length suffix) (length string))
       (string= suffix string :start2 (- (length string) (length suffix)))))

;;; ============================================================
;;; File Utilities
;;; ============================================================

(defun read-file-bytes (pathname)
  "Read entire file as a vector of bytes."
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun write-file-bytes (pathname bytes)
  "Write BYTES (vector or list) to file at PATHNAME."
  (let ((vec (if (vectorp bytes) bytes (bytes-to-vector bytes))))
    (with-open-file (stream pathname
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-sequence vec stream))))
