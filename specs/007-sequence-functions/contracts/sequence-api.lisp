;;;; sequence-api.lisp - API Contract for Sequence Functions
;;;;
;;;; This file defines the public interface for sequence functions.
;;;; Each function follows ANSI Common Lisp semantics unless otherwise noted.
;;;;
;;;; Feature: 007-sequence-functions
;;;; Date: 2025-12-23

(in-package #:clysm/contracts)

;;; ============================================================
;;; Tier 1: Basic Sequence Functions
;;; ============================================================

(defgeneric length (sequence)
  (:documentation
   "Return the number of elements in SEQUENCE.
    For lists, counts cons cells until NIL.

    Examples:
      (length nil)        => 0
      (length '(1 2 3))   => 3
      (length '((a) (b))) => 2"))

(defgeneric append (&rest lists)
  (:documentation
   "Return a new list that is the concatenation of LISTS.
    Does not modify input lists. Last argument is shared.

    Examples:
      (append nil nil)       => NIL
      (append '(1 2) '(3 4)) => (1 2 3 4)
      (append '(1) '(2) '(3)) => (1 2 3)"))

(defgeneric reverse (list)
  (:documentation
   "Return a new list with elements in reverse order.
    Does not modify the input list.

    Examples:
      (reverse nil)      => NIL
      (reverse '(1 2 3)) => (3 2 1)"))

(defgeneric nreverse (list)
  (:documentation
   "Destructively reverse LIST by modifying cdr pointers.
    May modify the input list. Returns the reversed list.

    Examples:
      (nreverse nil)      => NIL
      (nreverse '(1 2 3)) => (3 2 1)  ; input list is modified"))

(defgeneric last (list &optional n)
  (:documentation
   "Return the last N cons cells of LIST.
    N defaults to 1.

    Examples:
      (last '(1 2 3))   => (3)
      (last '(1 2 3) 2) => (2 3)
      (last nil)        => NIL"))

(defgeneric butlast (list &optional n)
  (:documentation
   "Return a list with the last N elements removed.
    N defaults to 1. Does not modify input list.

    Examples:
      (butlast '(1 2 3))   => (1 2)
      (butlast '(1 2 3) 2) => (1)
      (butlast '(1))       => NIL"))

(defgeneric copy-list (list)
  (:documentation
   "Return a shallow copy of LIST.
    Creates new cons cells with same car values.

    Examples:
      (copy-list nil)      => NIL
      (copy-list '(1 2 3)) => (1 2 3)  ; new cons cells"))

;;; ============================================================
;;; Tier 2: Higher-Order Sequence Functions
;;; ============================================================

(defgeneric mapcar (function list)
  (:documentation
   "Apply FUNCTION to each element of LIST.
    Return a new list of the results.

    Examples:
      (mapcar #'1+ '(1 2 3))           => (2 3 4)
      (mapcar #'car '((a b) (c d)))    => (A C)
      (mapcar #'identity nil)          => NIL"))

(defgeneric mapc (function list)
  (:documentation
   "Apply FUNCTION to each element of LIST for side effects.
    Return the original LIST.

    Examples:
      (mapc #'print '(1 2 3)) ; prints 1, 2, 3
                              => (1 2 3)"))

(defgeneric maplist (function list)
  (:documentation
   "Apply FUNCTION to successive tails of LIST.
    Return a new list of the results.

    Examples:
      (maplist #'identity '(1 2 3)) => ((1 2 3) (2 3) (3))
      (maplist #'length '(a b c))   => (3 2 1)"))

(defgeneric reduce (function list &key initial-value)
  (:documentation
   "Combine elements of LIST using FUNCTION.
    If INITIAL-VALUE provided, use it as first accumulator.

    Examples:
      (reduce #'+ '(1 2 3 4))                => 10
      (reduce #'+ '(1 2 3) :initial-value 10) => 16
      (reduce #'cons '(1 2 3) :initial-value nil) => ((NIL . 1) . 2) . 3)"))

;;; ============================================================
;;; Tier 3: Search and Filter Functions
;;; ============================================================

(defgeneric find (item list &key test key)
  (:documentation
   "Return the first element of LIST matching ITEM.
    TEST defaults to #'eql. KEY defaults to #'identity.

    Examples:
      (find 2 '(1 2 3))     => 2
      (find 5 '(1 2 3))     => NIL
      (find 'a '((a 1) (b 2)) :key #'car) => (A 1)"))

(defgeneric find-if (predicate list &key key)
  (:documentation
   "Return the first element of LIST satisfying PREDICATE.

    Examples:
      (find-if #'evenp '(1 2 3 4)) => 2
      (find-if #'evenp '(1 3 5))   => NIL"))

(defgeneric position (item list &key test key)
  (:documentation
   "Return the index of first element matching ITEM, or NIL.
    Index is 0-based.

    Examples:
      (position 'b '(a b c)) => 1
      (position 'z '(a b c)) => NIL"))

(defgeneric position-if (predicate list &key key)
  (:documentation
   "Return the index of first element satisfying PREDICATE, or NIL.

    Examples:
      (position-if #'evenp '(1 2 3)) => 1
      (position-if #'evenp '(1 3 5)) => NIL"))

(defgeneric remove (item list &key test key)
  (:documentation
   "Return a new list with all elements matching ITEM removed.
    Does not modify input list.

    Examples:
      (remove 2 '(1 2 3 2 4)) => (1 3 4)
      (remove 'a '(a b a c))  => (B C)"))

(defgeneric remove-if (predicate list &key key)
  (:documentation
   "Return a new list with elements satisfying PREDICATE removed.

    Examples:
      (remove-if #'evenp '(1 2 3 4)) => (1 3)
      (remove-if #'null '(1 nil 2))  => (1 2)"))

(defgeneric remove-if-not (predicate list &key key)
  (:documentation
   "Return a new list keeping only elements satisfying PREDICATE.
    Equivalent to (remove-if (complement predicate) list).

    Examples:
      (remove-if-not #'evenp '(1 2 3 4)) => (2 4)"))

(defgeneric count (item list &key test key)
  (:documentation
   "Return the number of elements matching ITEM.

    Examples:
      (count 'a '(a b a c a)) => 3
      (count 2 '(1 2 3 2))    => 2"))

(defgeneric count-if (predicate list &key key)
  (:documentation
   "Return the number of elements satisfying PREDICATE.

    Examples:
      (count-if #'evenp '(1 2 3 4)) => 2
      (count-if #'null '(1 nil 2))  => 1"))

;;; ============================================================
;;; Tier 4: Optional Functions
;;; ============================================================

(defgeneric member (item list &key test key)
  (:documentation
   "Return the tail of LIST starting with first element matching ITEM.

    Examples:
      (member 2 '(1 2 3))   => (2 3)
      (member 'b '(a b c))  => (B C)
      (member 5 '(1 2 3))   => NIL"))

(defgeneric assoc (key alist &key test)
  (:documentation
   "Return the first pair in ALIST whose car matches KEY.

    Examples:
      (assoc 'a '((a . 1) (b . 2))) => (A . 1)
      (assoc 'c '((a . 1) (b . 2))) => NIL"))

(defgeneric rassoc (value alist &key test)
  (:documentation
   "Return the first pair in ALIST whose cdr matches VALUE.

    Examples:
      (rassoc 1 '((a . 1) (b . 2))) => (A . 1)
      (rassoc 3 '((a . 1) (b . 2))) => NIL"))

(defgeneric subst (new old tree &key test key)
  (:documentation
   "Substitute NEW for subtrees matching OLD in TREE.
    Returns a copy of TREE with substitutions.

    Examples:
      (subst 'x 'a '(a b (a c))) => (X B (X C))"))

(defgeneric every (predicate list)
  (:documentation
   "Return T if PREDICATE is true for all elements, NIL otherwise.

    Examples:
      (every #'numberp '(1 2 3))  => T
      (every #'evenp '(2 3 4))    => NIL"))

(defgeneric some (predicate list)
  (:documentation
   "Return the first non-NIL result of applying PREDICATE.

    Examples:
      (some #'evenp '(1 2 3)) => T  ; actually returns 2 for some implementations
      (some #'evenp '(1 3 5)) => NIL"))

(defgeneric notany (predicate list)
  (:documentation
   "Return T if PREDICATE is false for all elements.
    Equivalent to (not (some predicate list)).

    Examples:
      (notany #'evenp '(1 3 5)) => T
      (notany #'evenp '(1 2 3)) => NIL"))

(defgeneric notevery (predicate list)
  (:documentation
   "Return T if PREDICATE is false for at least one element.
    Equivalent to (not (every predicate list)).

    Examples:
      (notevery #'evenp '(2 3 4)) => T
      (notevery #'evenp '(2 4 6)) => NIL"))

;;; ============================================================
;;; Contract Invariants
;;; ============================================================

#|
INVARIANTS:

1. Non-destructive functions never modify their input:
   - append, reverse, copy-list, remove, remove-if, remove-if-not
   - mapcar, maplist, reduce
   - find, find-if, position, position-if
   - count, count-if, member, assoc, rassoc
   - every, some, notany, notevery, subst

2. Destructive functions may modify their input:
   - nreverse (modifies cdr pointers)

3. NIL handling:
   - (length nil) => 0
   - (mapcar fn nil) => nil
   - (find x nil) => nil
   - (reduce fn nil) => error without :initial-value (Phase 8c)

4. Return type consistency:
   - Functions returning lists always return proper lists (nil-terminated)
   - find/find-if return the element itself, not a cons cell
   - member returns the tail (a cons cell or nil)

5. Default arguments (Phase 8a):
   - :test defaults to #'eql
   - :key defaults to nil (identity)
   - These are fixed in Phase 8a; full keyword support in later phases
|#

;;; End of sequence-api.lisp
