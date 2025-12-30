;;;; list-ops.lisp - ANSI Common Lisp list operations
;;;;
;;;; Feature: 001-ansi-list-ops (Phase 15A)
;;;; Purpose: Implement ANSI CL list operations for Clysm compiler
;;;;
;;;; Functions implemented:
;;;;   List Tail: last, butlast, nbutlast, nth, nthcdr
;;;;   Membership: member, member-if, member-if-not
;;;;   Alist Lookup: assoc, assoc-if, rassoc, rassoc-if
;;;;   Alist Construction: pairlis, acons, copy-alist
;;;;   Set Operations: intersection, union, set-difference, subsetp, adjoin
;;;;
;;;; HyperSpec references included per Constitution IX.

(in-package #:clysm)

;;; ============================================================
;;; List Tail Operations
;;; ============================================================

;; [nthcdr](resources/HyperSpec/Body/f_nthcdr.htm)
;; Returns the result of applying cdr n times to list
(defun nthcdr* (n list)
  "Return the result of applying CDR n times to LIST."
  (declare (type (integer 0) n))
  (loop repeat n
        while list
        do (setf list (cdr list)))
  list)

;; [nth](resources/HyperSpec/Body/f_nth.htm)
;; Returns the nth element (zero-indexed) of list
(defun nth* (n list)
  "Return the Nth element of LIST (zero-indexed)."
  (declare (type (integer 0) n))
  (car (nthcdr* n list)))

;; [last](resources/HyperSpec/Body/f_last.htm)
;; Returns the last n cons cells of list
(defun last* (list &optional (n 1))
  "Return the last N cons cells of LIST."
  (declare (type (integer 0) n))
  (if (zerop n)
      nil
      ;; Two-pointer technique: advance lead pointer n steps ahead,
      ;; then move both until lead hits end
      (let ((lead list)
            (lag list))
        ;; Advance lead n steps
        (loop repeat n
              while lead
              do (setf lead (cdr lead)))
        ;; If lead is nil, list has <= n elements, return list
        (if (null lead)
            list
            ;; Move both until lead reaches end
            (loop while (cdr lead)
                  do (setf lead (cdr lead))
                     (setf lag (cdr lag))
                  finally (return (cdr lag)))))))

;; [butlast](resources/HyperSpec/Body/f_butlas.htm)
;; Returns a fresh list of all but the last n elements
(defun butlast* (list &optional (n 1))
  "Return a fresh list of all but the last N elements of LIST."
  (declare (type (integer 0) n))
  (if (or (zerop n) (null list))
      (if (zerop n)
          (copy-list list)
          nil)
      ;; Find the position to stop copying
      (let ((len 0)
            (result nil)
            (tail nil))
        ;; First, calculate length
        (loop for rest on list
              do (incf len))
        ;; Copy up to (- len n) elements
        (let ((copy-count (max 0 (- len n))))
          (loop for rest on list
                for i from 0 below copy-count
                for new-cell = (cons (car rest) nil)
                do (if (null result)
                       (setf result new-cell
                             tail new-cell)
                       (setf (cdr tail) new-cell
                             tail new-cell))))
        result)))

;; [nbutlast](resources/HyperSpec/Body/f_butlas.htm)
;; Destructively modifies list to remove the last n elements
(defun nbutlast* (list &optional (n 1))
  "Destructively remove the last N elements from LIST."
  (declare (type (integer 0) n))
  (if (or (zerop n) (null list))
      (if (zerop n)
          list
          nil)
      ;; Find length, then modify (length - n - 1)th cdr to nil
      (let ((len 0))
        (loop for rest on list
              do (incf len))
        (let ((keep-count (- len n)))
          (if (<= keep-count 0)
              nil
              (let ((tail list))
                ;; Move to position (keep-count - 1)
                (loop repeat (1- keep-count)
                      do (setf tail (cdr tail)))
                ;; Set its cdr to nil
                (setf (cdr tail) nil)
                list))))))

;;; ============================================================
;;; Membership Operations
;;; ============================================================

;; [member](resources/HyperSpec/Body/f_mem_m.htm)
;; Searches for item in list using :test and :key
(defun member* (item list &key (test #'eql) (key #'identity))
  "Search for ITEM in LIST. Return tail starting from match, or NIL."
  (loop for tail on list
        when (funcall test item (funcall key (car tail)))
          return tail
        finally (return nil)))

;; [member-if](resources/HyperSpec/Body/f_mem_m.htm)
;; Searches for element satisfying predicate
(defun member-if* (predicate list &key (key #'identity))
  "Search for element satisfying PREDICATE. Return tail starting from match."
  (loop for tail on list
        when (funcall predicate (funcall key (car tail)))
          return tail
        finally (return nil)))

;; [member-if-not](resources/HyperSpec/Body/f_mem_m.htm)
;; Searches for element NOT satisfying predicate
(defun member-if-not* (predicate list &key (key #'identity))
  "Search for element NOT satisfying PREDICATE. Return tail starting from match."
  (loop for tail on list
        unless (funcall predicate (funcall key (car tail)))
          return tail
        finally (return nil)))

;;; ============================================================
;;; Association List Lookup Operations
;;; ============================================================

;; [assoc](resources/HyperSpec/Body/f_assocc.htm)
;; Searches alist by key (CAR of each entry)
(defun assoc* (item alist &key (test #'eql) (key #'identity))
  "Search ALIST for entry whose key matches ITEM."
  (loop for entry in alist
        ;; Per ANSI CL, skip non-cons elements
        when (and (consp entry)
                  (funcall test item (funcall key (car entry))))
          return entry
        finally (return nil)))

;; [assoc-if](resources/HyperSpec/Body/f_assocc.htm)
;; Searches alist where key satisfies predicate
(defun assoc-if* (predicate alist &key (key #'identity))
  "Search ALIST for entry whose key satisfies PREDICATE."
  (loop for entry in alist
        when (and (consp entry)
                  (funcall predicate (funcall key (car entry))))
          return entry
        finally (return nil)))

;; [rassoc](resources/HyperSpec/Body/f_rassoc.htm)
;; Searches alist by value (CDR of each entry)
(defun rassoc* (item alist &key (test #'eql) (key #'identity))
  "Search ALIST for entry whose value (CDR) matches ITEM."
  (loop for entry in alist
        when (and (consp entry)
                  (funcall test item (funcall key (cdr entry))))
          return entry
        finally (return nil)))

;; [rassoc-if](resources/HyperSpec/Body/f_rassoc.htm)
;; Searches alist where value satisfies predicate
(defun rassoc-if* (predicate alist &key (key #'identity))
  "Search ALIST for entry whose value (CDR) satisfies PREDICATE."
  (loop for entry in alist
        when (and (consp entry)
                  (funcall predicate (funcall key (cdr entry))))
          return entry
        finally (return nil)))

;;; ============================================================
;;; Association List Construction
;;; ============================================================

;; [acons](resources/HyperSpec/Body/f_acons.htm)
;; Prepends a new key-value pair to alist
(defun acons* (key value alist)
  "Prepend (KEY . VALUE) to ALIST."
  (cons (cons key value) alist))

;; [pairlis](resources/HyperSpec/Body/f_pairli.htm)
;; Constructs alist from parallel key and value lists
(defun pairlis* (keys values &optional alist)
  "Construct an alist by pairing KEYS with VALUES, prepended to ALIST."
  (loop for k in keys
        for v in values
        collect (cons k v) into pairs
        finally (return (nconc pairs alist))))

;; [copy-alist](resources/HyperSpec/Body/f_cp_ali.htm)
;; Creates a fresh copy of alist (top-level cons cells copied)
(defun copy-alist* (alist)
  "Return a fresh copy of ALIST with new cons cells for spine and entries."
  (loop for entry in alist
        collect (if (consp entry)
                    (cons (car entry) (cdr entry))
                    entry)))

;;; ============================================================
;;; Set Operations
;;; ============================================================

;; [adjoin](resources/HyperSpec/Body/f_adjoin.htm)
;; Adds item to list if not already present
(defun adjoin* (item list &key (test #'eql) (key #'identity))
  "Add ITEM to LIST if not already a member."
  (if (member* item list :test test :key key)
      list
      (cons item list)))

;; [intersection](resources/HyperSpec/Body/f_intera.htm)
;; Returns elements common to both lists
(defun intersection* (list1 list2 &key (test #'eql) (key #'identity))
  "Return a list of elements present in both LIST1 and LIST2."
  (loop for item in list1
        when (member* (funcall key item) list2 :test test :key key)
          collect item))

;; [union](resources/HyperSpec/Body/f_unionc.htm)
;; Returns elements in either list without duplicates
(defun union* (list1 list2 &key (test #'eql) (key #'identity))
  "Return a list of elements from either LIST1 or LIST2, no duplicates."
  (let ((result (copy-list list1)))
    (loop for item in list2
          unless (member* (funcall key item) result :test test :key key)
            do (push item result))
    result))

;; [set-difference](resources/HyperSpec/Body/f_set_di.htm)
;; Returns elements in first list but not in second
(defun set-difference* (list1 list2 &key (test #'eql) (key #'identity))
  "Return elements in LIST1 but not in LIST2."
  (loop for item in list1
        unless (member* (funcall key item) list2 :test test :key key)
          collect item))

;; [subsetp](resources/HyperSpec/Body/f_subset.htm)
;; Tests if first list is a subset of second
(defun subsetp* (list1 list2 &key (test #'eql) (key #'identity))
  "Return T if every element of LIST1 is in LIST2."
  (loop for item in list1
        always (member* (funcall key item) list2 :test test :key key)))

;;; ============================================================
;;; End of list-ops.lisp
;;; ============================================================
