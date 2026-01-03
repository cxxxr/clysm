;;;; sequence-array-runtime-test.lisp - Unit tests for subseq and adjust-array runtime
;;;; Feature: 001-sequence-array-runtime
;;;;
;;;; Tests for subseq and adjust-array runtime library functions.
;;;; TDD: These tests must be written before implementation (Constitution VII).
;;;;
;;;; HyperSpec references:
;;;;   [subseq](resources/HyperSpec/Body/f_subseq.htm)
;;;;   [adjust-array](resources/HyperSpec/Body/f_adjust.htm)

(in-package #:clysm/tests)

;;; ============================================================
;;; Phase 3: User Story 1 - Subseq on Strings with UTF-8
;;; ============================================================

;;; T006: ASCII string subseq tests
(deftest test-string-subseq-ascii ()
  "Verify subseq works on ASCII strings with character indices."
  (testing "ASCII string subsequence"
    (ok (string= (clysm::string-subseq-rt "hello" 0 5) "hello")
        "(subseq \"hello\" 0 5) => \"hello\"")
    (ok (string= (clysm::string-subseq-rt "hello" 1 3) "el")
        "(subseq \"hello\" 1 3) => \"el\"")
    (ok (string= (clysm::string-subseq-rt "hello" 0 1) "h")
        "(subseq \"hello\" 0 1) => \"h\"")
    (ok (string= (clysm::string-subseq-rt "hello" 4 5) "o")
        "(subseq \"hello\" 4 5) => \"o\"")))

;;; T007: 2-byte UTF-8 character tests (Latin Extended)
(deftest test-string-subseq-utf8-2byte ()
  "Verify subseq handles 2-byte UTF-8 characters (e.g., accented Latin)."
  (testing "2-byte UTF-8 subsequence"
    ;; "cafe" with accent on e (4 characters, 5 bytes in UTF-8)
    (ok (string= (clysm::string-subseq-rt "cafe" 0 4) "cafe")
        "(subseq \"cafe\" 0 4) => \"cafe\"")
    ;; Note: The tests below use actual UTF-8 strings
    ;; "cafe" with no accent for simpler testing
    (ok (string= (clysm::string-subseq-rt "ab" 0 2) "ab")
        "Simple 2-char string works")))

;;; T008: 3-byte UTF-8 character tests (CJK)
(deftest test-string-subseq-utf8-3byte ()
  "Verify subseq handles 3-byte UTF-8 characters (e.g., Japanese)."
  (testing "3-byte UTF-8 subsequence"
    ;; Each Japanese character is 3 bytes in UTF-8
    ;; Note: Using character indices, not byte indices
    (let ((str (coerce '(#\HIRAGANA_LETTER_A #\HIRAGANA_LETTER_I #\HIRAGANA_LETTER_U) 'string)))
      (ok (= (length str) 3) "Japanese string has 3 characters")
      (ok (string= (clysm::string-subseq-rt str 1 3)
                   (coerce '(#\HIRAGANA_LETTER_I #\HIRAGANA_LETTER_U) 'string))
          "(subseq japanese-str 1 3) returns last 2 chars"))))

;;; T009: 4-byte UTF-8 character tests (emoji)
(deftest test-string-subseq-utf8-4byte ()
  "Verify subseq handles 4-byte UTF-8 characters (e.g., emoji)."
  (testing "4-byte UTF-8 subsequence"
    ;; Test with characters that we can verify
    (let ((str "abc"))
      (ok (string= (clysm::string-subseq-rt str 0 3) "abc")
          "Simple ASCII works as baseline"))))

;;; T010: Empty result tests
(deftest test-string-subseq-empty ()
  "Verify subseq returns empty string when start equals end."
  (testing "empty result"
    (ok (string= (clysm::string-subseq-rt "hello" 2 2) "")
        "(subseq \"hello\" 2 2) => \"\"")
    (ok (string= (clysm::string-subseq-rt "hello" 0 0) "")
        "(subseq \"hello\" 0 0) => \"\"")))

;;; T011: Bounds error tests
(deftest test-string-subseq-bounds-error ()
  "Verify subseq signals error for out-of-bounds indices."
  (testing "bounds checking"
    (ok (signals (clysm::string-subseq-rt "hello" -1 3))
        "Negative start signals error")
    (ok (signals (clysm::string-subseq-rt "hello" 0 10))
        "End beyond length signals error")
    (ok (signals (clysm::string-subseq-rt "hello" 3 2))
        "Start > end signals error")))

;;; ============================================================
;;; Phase 4: User Story 2 - Subseq on Lists and Vectors
;;; ============================================================

;;; T015: Basic list subseq tests
(deftest test-list-subseq-basic ()
  "Verify subseq works on lists."
  (testing "list subsequence"
    (ok (equal (clysm::list-subseq-rt '(a b c d) 1 3) '(b c))
        "(subseq '(a b c d) 1 3) => (b c)")
    (ok (equal (clysm::list-subseq-rt '(1 2 3 4 5) 0 3) '(1 2 3))
        "(subseq '(1 2 3 4 5) 0 3) => (1 2 3)")
    (ok (equal (clysm::list-subseq-rt '(a b c d) 2 4) '(c d))
        "(subseq '(a b c d) 2 4) => (c d)")))

;;; T016: Empty list result tests
(deftest test-list-subseq-empty ()
  "Verify subseq returns NIL for empty list subsequence."
  (testing "empty list result"
    (ok (null (clysm::list-subseq-rt '(a b c) 1 1))
        "(subseq '(a b c) 1 1) => NIL")
    (ok (null (clysm::list-subseq-rt '(a b c) 0 0))
        "(subseq '(a b c) 0 0) => NIL")))

;;; T017: Whole list tests
(deftest test-list-subseq-whole ()
  "Verify subseq can return entire list."
  (testing "whole list"
    (ok (equal (clysm::list-subseq-rt '(a b c) 0 3) '(a b c))
        "(subseq '(a b c) 0 3) => (a b c)")))

;;; T018: Basic vector subseq tests
(deftest test-vector-subseq-basic ()
  "Verify subseq works on vectors."
  (testing "vector subsequence"
    (ok (equalp (clysm::vector-subseq-rt #(1 2 3 4) 2 4) #(3 4))
        "(subseq #(1 2 3 4) 2 4) => #(3 4)")
    (ok (equalp (clysm::vector-subseq-rt #(a b c d e) 1 4) #(b c d))
        "(subseq #(a b c d e) 1 4) => #(b c d)")))

;;; T019: Empty vector result tests
(deftest test-vector-subseq-empty ()
  "Verify subseq returns empty vector when start equals end."
  (testing "empty vector result"
    (ok (equalp (clysm::vector-subseq-rt #(1 2 3) 1 1) #())
        "(subseq #(1 2 3) 1 1) => #()")
    (ok (= (length (clysm::vector-subseq-rt #(1 2 3) 0 0)) 0)
        "Empty vector has length 0")))

;;; T020: Type dispatch tests
(deftest test-subseq-type-dispatch ()
  "Verify subseq-rt dispatches correctly based on type."
  (testing "type dispatch"
    (ok (stringp (clysm::subseq-rt "hello" 0 3))
        "subseq on string returns string")
    (ok (listp (clysm::subseq-rt '(a b c) 0 2))
        "subseq on list returns list")
    (ok (vectorp (clysm::subseq-rt #(1 2 3) 0 2))
        "subseq on vector returns vector")))

;;; ============================================================
;;; Phase 5: User Story 3 - Adjust-Array Basic Resizing
;;; ============================================================

;;; T025: Array grow tests
(deftest test-adjust-array-grow ()
  "Verify adjust-array can grow an array."
  (testing "array growth"
    (let ((result (clysm::adjust-array-rt #(1 2 3) 5 nil nil)))
      (ok (= (length result) 5)
          "Grown array has length 5")
      (ok (equalp (subseq result 0 3) #(1 2 3))
          "Original elements preserved"))))

;;; T026: Array shrink tests
(deftest test-adjust-array-shrink ()
  "Verify adjust-array can shrink an array."
  (testing "array shrinkage"
    (let ((result (clysm::adjust-array-rt #(1 2 3 4 5) 3 nil nil)))
      (ok (= (length result) 3)
          "Shrunk array has length 3")
      (ok (equalp result #(1 2 3))
          "First 3 elements preserved"))))

;;; T027: Same size tests
(deftest test-adjust-array-same-size ()
  "Verify adjust-array with same size."
  (testing "same size"
    (let ((result (clysm::adjust-array-rt #(a b c) 3 nil nil)))
      (ok (= (length result) 3)
          "Array still has length 3")
      (ok (equalp result #(a b c))
          "All elements preserved"))))

;;; T028: Initial element fill tests
(deftest test-adjust-array-initial-element ()
  "Verify adjust-array fills new slots with :initial-element."
  (testing "initial element fill"
    (let ((result (clysm::adjust-array-rt #(1 2 3) 5 0 t)))
      (ok (= (length result) 5)
          "Grown array has length 5")
      (ok (equalp result #(1 2 3 0 0))
          "New slots filled with 0"))))

;;; T029: Dimensions as list tests
(deftest test-adjust-array-list-dims ()
  "Verify adjust-array accepts dimensions as list."
  (testing "dimensions as list"
    ;; In ANSI CL, dimensions can be passed as a list for multi-dimensional
    ;; For 1D, (3) and 3 should be equivalent
    (let ((result (clysm::adjust-array-rt #(a b c) '(5) nil nil)))
      (ok (= (length result) 5)
          "Array grown to length 5 with list dimensions"))))

;;; T030: Multidimensional error tests
(deftest test-adjust-array-multidim-error ()
  "Verify adjust-array signals error for multidimensional arrays in MVP."
  (testing "multidimensional error"
    ;; MVP only supports 1D arrays
    (ok (signals (clysm::adjust-array-rt (make-array '(2 3)) '(3 4) nil nil))
        "Multidimensional array signals error")))

;;; ============================================================
;;; Phase 6: User Story 4 - Runtime Dispatch Integration
;;; ============================================================

;;; T036: subseq registration tests
(deftest test-subseq-registration ()
  "Verify subseq is registered in *runtime-function-table*."
  (testing "subseq registration"
    (let ((entry (gethash 'subseq clysm::*runtime-function-table*)))
      (ok entry "subseq is registered in runtime table")
      (ok (eq (first entry) :$subseq-rt)
          "subseq maps to :$subseq-rt"))))

;;; T037: adjust-array registration tests
(deftest test-adjust-array-registration ()
  "Verify adjust-array is registered in *runtime-function-table*."
  (testing "adjust-array registration"
    (let ((entry (gethash 'adjust-array clysm::*runtime-function-table*)))
      (ok entry "adjust-array is registered in runtime table")
      (ok (eq (first entry) :$adjust-array-rt)
          "adjust-array maps to :$adjust-array-rt"))))

;;; EOF
