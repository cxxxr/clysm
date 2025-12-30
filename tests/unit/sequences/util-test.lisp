;;;; util-test.lisp - Unit tests for sequence utility functions
;;;; Feature: 001-ansi-sequence-functions (Phase 15B)
;;;; Task: T008 - Unit tests for sequence utility functions

(in-package #:clysm/tests/unit/sequences/util)

;;; ============================================================
;;; validate-bounding-indices Tests
;;; ============================================================

(deftest validate-bounding-indices-basic
  "Test basic valid bounds"
  (let ((result (clysm::validate-bounding-indices 0 5 10)))
    (ok (equal result '(0 . 5))
        "Should return (start . end) cons")))

(deftest validate-bounding-indices-nil-end
  "Test NIL end defaults to length"
  (let ((result (clysm::validate-bounding-indices 0 nil 10)))
    (ok (equal result '(0 . 10))
        "NIL end should default to length")))

(deftest validate-bounding-indices-start-equals-end
  "Test start equals end is valid"
  (let ((result (clysm::validate-bounding-indices 5 5 10)))
    (ok (equal result '(5 . 5))
        "start=end should be valid (empty range)")))

(deftest validate-bounding-indices-full-range
  "Test full range (0 to length)"
  (let ((result (clysm::validate-bounding-indices 0 10 10)))
    (ok (equal result '(0 . 10))
        "Full range should be valid")))

(deftest validate-bounding-indices-invalid-start-negative
  "Test negative start signals error"
  ;; SBCL type check will catch this, but implementation should also check
  (ok (handler-case
          (progn
            (clysm::validate-bounding-indices -1 5 10)
            nil)  ; Should not reach here
        (error () t))
      "Negative start should signal error"))

(deftest validate-bounding-indices-invalid-start-exceeds-end
  "Test start > end signals error"
  (ok (handler-case
          (progn
            (clysm::validate-bounding-indices 7 5 10)
            nil)
        (error () t))
      "start > end should signal error"))

(deftest validate-bounding-indices-invalid-end-exceeds-length
  "Test end > length signals error"
  (ok (handler-case
          (progn
            (clysm::validate-bounding-indices 0 15 10)
            nil)
        (error () t))
      "end > length should signal error"))

;;; ============================================================
;;; %sequence-length Tests
;;; ============================================================

(deftest sequence-length-list
  "Test %sequence-length on lists"
  (ok (= (clysm::%sequence-length '(a b c)) 3)
      "List length should be 3")
  (ok (= (clysm::%sequence-length '()) 0)
      "Empty list length should be 0")
  (ok (= (clysm::%sequence-length '(1)) 1)
      "Single element list length should be 1"))

(deftest sequence-length-vector
  "Test %sequence-length on vectors"
  (ok (= (clysm::%sequence-length #(1 2 3 4 5)) 5)
      "Vector length should be 5")
  (ok (= (clysm::%sequence-length #()) 0)
      "Empty vector length should be 0"))

(deftest sequence-length-string
  "Test %sequence-length on strings"
  (ok (= (clysm::%sequence-length "hello") 5)
      "String length should be 5")
  (ok (= (clysm::%sequence-length "") 0)
      "Empty string length should be 0"))

;;; ============================================================
;;; %sequence-ref Tests
;;; ============================================================

(deftest sequence-ref-list
  "Test %sequence-ref on lists"
  (let ((lst '(a b c d)))
    (ok (eq (clysm::%sequence-ref lst 0) 'a)
        "First element should be a")
    (ok (eq (clysm::%sequence-ref lst 2) 'c)
        "Third element should be c")
    (ok (eq (clysm::%sequence-ref lst 3) 'd)
        "Fourth element should be d")))

(deftest sequence-ref-vector
  "Test %sequence-ref on vectors"
  (let ((vec #(10 20 30 40)))
    (ok (= (clysm::%sequence-ref vec 0) 10)
        "First element should be 10")
    (ok (= (clysm::%sequence-ref vec 2) 30)
        "Third element should be 30")))

(deftest sequence-ref-string
  "Test %sequence-ref on strings"
  (let ((str "hello"))
    (ok (char= (clysm::%sequence-ref str 0) #\h)
        "First char should be h")
    (ok (char= (clysm::%sequence-ref str 4) #\o)
        "Fifth char should be o")))

;;; ============================================================
;;; %sequence-set Tests
;;; ============================================================

(deftest sequence-set-list
  "Test %sequence-set on lists"
  (let ((lst (list 'a 'b 'c)))
    (clysm::%sequence-set lst 1 'x)
    (ok (eq (nth 1 lst) 'x)
        "Second element should be x after set")))

(deftest sequence-set-vector
  "Test %sequence-set on vectors"
  (let ((vec (vector 1 2 3)))
    (clysm::%sequence-set vec 0 99)
    (ok (= (aref vec 0) 99)
        "First element should be 99 after set")))

(deftest sequence-set-string
  "Test %sequence-set on strings"
  (let ((str (copy-seq "hello")))
    (clysm::%sequence-set str 0 #\H)
    (ok (char= (char str 0) #\H)
        "First char should be H after set")))

;;; ============================================================
;;; %sequence-type Tests
;;; ============================================================

(deftest sequence-type-list
  "Test %sequence-type returns 'list for lists"
  (ok (eq (clysm::%sequence-type '(a b c)) 'list)
      "Type of list should be list"))

(deftest sequence-type-vector
  "Test %sequence-type returns 'vector for vectors"
  (ok (eq (clysm::%sequence-type #(1 2 3)) 'vector)
      "Type of vector should be vector"))

(deftest sequence-type-string
  "Test %sequence-type returns 'string for strings"
  (ok (eq (clysm::%sequence-type "hello") 'string)
      "Type of string should be string"))

;;; ============================================================
;;; %copy-sequence Tests
;;; ============================================================

(deftest copy-sequence-list
  "Test %copy-sequence on lists"
  (let* ((original '(a b c))
         (copy (clysm::%copy-sequence original)))
    (ok (equal copy '(a b c))
        "Copy should equal original")
    (ok (not (eq copy original))
        "Copy should not be eq to original")))

(deftest copy-sequence-vector
  "Test %copy-sequence on vectors"
  (let* ((original #(1 2 3))
         (copy (clysm::%copy-sequence original)))
    (ok (equalp copy #(1 2 3))
        "Copy should equalp original")
    (ok (not (eq copy original))
        "Copy should not be eq to original")))

(deftest copy-sequence-string
  "Test %copy-sequence on strings"
  (let* ((original "hello")
         (copy (clysm::%copy-sequence original)))
    (ok (equal copy "hello")
        "Copy should equal original")
    (ok (not (eq copy original))
        "Copy should not be eq to original")))

;;; ============================================================
;;; %iterate-sequence Tests
;;; ============================================================

(deftest iterate-sequence-forward
  "Test %iterate-sequence in forward direction"
  (let ((visited nil))
    (clysm::%iterate-sequence '(a b c d) 0 4 nil
                               (lambda (i elt)
                                 (push (cons i elt) visited)
                                 nil))
    (ok (equal (nreverse visited)
               '((0 . a) (1 . b) (2 . c) (3 . d)))
        "Should visit all elements in order")))

(deftest iterate-sequence-reverse
  "Test %iterate-sequence in reverse direction"
  (let ((visited nil))
    (clysm::%iterate-sequence '(a b c d) 0 4 t
                               (lambda (i elt)
                                 (push (cons i elt) visited)
                                 nil))
    (ok (equal (nreverse visited)
               '((3 . d) (2 . c) (1 . b) (0 . a)))
        "Should visit all elements in reverse order")))

(deftest iterate-sequence-early-termination
  "Test %iterate-sequence stops when function returns non-nil"
  (let ((result (clysm::%iterate-sequence '(1 2 3 4 5) 0 5 nil
                                           (lambda (i elt)
                                             (when (= elt 3) i)))))
    (ok (= result 2)
        "Should return index 2 where element 3 was found")))

(deftest iterate-sequence-bounded
  "Test %iterate-sequence respects :start and :end"
  (let ((visited nil))
    (clysm::%iterate-sequence '(a b c d e) 1 4 nil
                               (lambda (i elt)
                                 (push elt visited)
                                 nil))
    (ok (equal (nreverse visited) '(b c d))
        "Should only visit elements 1-3")))

;;; ============================================================
;;; %satisfies-test-p Tests
;;; ============================================================

(deftest satisfies-test-p-eql
  "Test %satisfies-test-p with eql"
  (ok (clysm::%satisfies-test-p 42 42 #'eql #'identity)
      "42 eql 42 should be true")
  (ok (not (clysm::%satisfies-test-p 42 43 #'eql #'identity))
      "42 eql 43 should be false"))

(deftest satisfies-test-p-with-key
  "Test %satisfies-test-p with :key function"
  (ok (clysm::%satisfies-test-p 'a '(a 1) #'eql #'car)
      "a eql (car (a 1)) should be true")
  (ok (not (clysm::%satisfies-test-p 'b '(a 1) #'eql #'car))
      "b eql (car (a 1)) should be false"))

(deftest satisfies-test-p-string-equal
  "Test %satisfies-test-p with string-equal"
  (ok (clysm::%satisfies-test-p "hello" "HELLO" #'string-equal #'identity)
      "hello string-equal HELLO should be true"))

;;; ============================================================
;;; %satisfies-predicate-p Tests
;;; ============================================================

(deftest satisfies-predicate-p-evenp
  "Test %satisfies-predicate-p with evenp"
  (ok (clysm::%satisfies-predicate-p 4 #'evenp #'identity)
      "4 should satisfy evenp")
  (ok (not (clysm::%satisfies-predicate-p 3 #'evenp #'identity))
      "3 should not satisfy evenp"))

(deftest satisfies-predicate-p-with-key
  "Test %satisfies-predicate-p with :key function"
  (ok (clysm::%satisfies-predicate-p '(a 2) #'evenp #'cadr)
      "(cadr (a 2)) = 2 should satisfy evenp"))

;;; ============================================================
;;; validate-two-sequence-bounds Tests
;;; ============================================================

(deftest validate-two-sequence-bounds-basic
  "Test validate-two-sequence-bounds with valid bounds"
  (multiple-value-bind (s1 e1 s2 e2)
      (clysm::validate-two-sequence-bounds 0 5 10 0 3 5)
    (ok (and (= s1 0) (= e1 5) (= s2 0) (= e2 3))
        "Should return correct bounds for both sequences")))

(deftest validate-two-sequence-bounds-nil-ends
  "Test validate-two-sequence-bounds with NIL ends"
  (multiple-value-bind (s1 e1 s2 e2)
      (clysm::validate-two-sequence-bounds 0 nil 10 2 nil 8)
    (ok (and (= s1 0) (= e1 10) (= s2 2) (= e2 8))
        "NIL ends should default to lengths")))
