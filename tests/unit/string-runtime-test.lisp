;;;; string-runtime-test.lisp - Unit tests for string runtime functions
;;;; Feature: 001-string-runtime-migration
;;;;
;;;; Tests for string manipulation runtime functions.
;;;; TDD: These tests must be written before implementation (Constitution VII).

(in-package #:clysm/tests)

;;; ============================================================
;;; Phase 2: Helper Function Tests (T004-T007)
;;; ============================================================

;;; T004: Tests for utf8-continuation-byte-p
(deftest utf8-continuation-byte-p-returns-t-for-continuation-bytes ()
  "Verify utf8-continuation-byte-p returns T for valid continuation bytes (10xxxxxx)"
  (testing "continuation byte detection"
    ;; Continuation bytes have pattern 10xxxxxx (0x80-0xBF)
    (ok (clysm::utf8-continuation-byte-p #x80) "0x80 is a continuation byte")
    (ok (clysm::utf8-continuation-byte-p #x8F) "0x8F is a continuation byte")
    (ok (clysm::utf8-continuation-byte-p #xBF) "0xBF is a continuation byte")))

(deftest utf8-continuation-byte-p-returns-nil-for-non-continuation ()
  "Verify utf8-continuation-byte-p returns NIL for non-continuation bytes"
  (testing "non-continuation byte detection"
    ;; ASCII bytes (0x00-0x7F)
    (ok (not (clysm::utf8-continuation-byte-p #x00)) "0x00 is not a continuation byte")
    (ok (not (clysm::utf8-continuation-byte-p #x7F)) "0x7F is not a continuation byte")
    ;; Start bytes (11xxxxxx, 0xC0-0xFF)
    (ok (not (clysm::utf8-continuation-byte-p #xC0)) "0xC0 is not a continuation byte")
    (ok (not (clysm::utf8-continuation-byte-p #xE0)) "0xE0 is not a continuation byte")
    (ok (not (clysm::utf8-continuation-byte-p #xF0)) "0xF0 is not a continuation byte")))

;;; T005: Tests for decode-utf8-char
(deftest decode-utf8-char-decodes-ascii ()
  "Verify decode-utf8-char handles ASCII correctly"
  (testing "ASCII decoding"
    (multiple-value-bind (char bytes-consumed)
        (clysm::decode-utf8-char "Hello" 0)
      (ok (char= char #\H) "First char is H")
      (ok (= bytes-consumed 1) "ASCII consumes 1 byte"))
    (multiple-value-bind (char bytes-consumed)
        (clysm::decode-utf8-char "Hello" 4)
      (ok (char= char #\o) "Fifth char is o")
      (ok (= bytes-consumed 1) "ASCII consumes 1 byte"))))

(deftest decode-utf8-char-decodes-multibyte ()
  "Verify decode-utf8-char handles multibyte UTF-8 correctly"
  (testing "multibyte decoding"
    ;; 2-byte sequence (U+00E9 = e with acute = "é")
    (let ((str (coerce '(#\LATIN_SMALL_LETTER_E_WITH_ACUTE) 'string)))
      (multiple-value-bind (char bytes-consumed)
          (clysm::decode-utf8-char str 0)
        (ok (char= char #\LATIN_SMALL_LETTER_E_WITH_ACUTE) "Decodes é correctly")
        (ok (= bytes-consumed 2) "2-byte sequence consumes 2 bytes")))))

;;; T006: Tests for char-in-bag-p
(deftest char-in-bag-p-finds-character-in-bag ()
  "Verify char-in-bag-p returns T when character is in bag"
  (testing "character membership"
    (ok (clysm::char-in-bag-p #\a "abc") "a is in \"abc\"")
    (ok (clysm::char-in-bag-p #\b "abc") "b is in \"abc\"")
    (ok (clysm::char-in-bag-p #\c "abc") "c is in \"abc\"")
    (ok (clysm::char-in-bag-p #\Space " \t\n") "Space is in whitespace bag")))

(deftest char-in-bag-p-returns-nil-for-missing-character ()
  "Verify char-in-bag-p returns NIL when character is not in bag"
  (testing "character not in bag"
    (ok (not (clysm::char-in-bag-p #\d "abc")) "d is not in \"abc\"")
    (ok (not (clysm::char-in-bag-p #\A "abc")) "A is not in \"abc\" (case sensitive)")
    (ok (not (clysm::char-in-bag-p #\x "")) "x is not in empty bag")))

(deftest char-in-bag-p-handles-empty-bag ()
  "Verify char-in-bag-p handles empty character bag"
  (testing "empty bag"
    (ok (not (clysm::char-in-bag-p #\a "")) "no character is in empty bag")))

;;; T007: Tests for alpha-char-p-ascii
(deftest alpha-char-p-ascii-returns-t-for-letters ()
  "Verify alpha-char-p-ascii returns T for ASCII letters"
  (testing "ASCII letter detection"
    ;; Lowercase
    (ok (clysm::alpha-char-p-ascii #\a) "a is alphabetic")
    (ok (clysm::alpha-char-p-ascii #\m) "m is alphabetic")
    (ok (clysm::alpha-char-p-ascii #\z) "z is alphabetic")
    ;; Uppercase
    (ok (clysm::alpha-char-p-ascii #\A) "A is alphabetic")
    (ok (clysm::alpha-char-p-ascii #\M) "M is alphabetic")
    (ok (clysm::alpha-char-p-ascii #\Z) "Z is alphabetic")))

(deftest alpha-char-p-ascii-returns-nil-for-non-letters ()
  "Verify alpha-char-p-ascii returns NIL for non-letters"
  (testing "non-letter detection"
    (ok (not (clysm::alpha-char-p-ascii #\0)) "0 is not alphabetic")
    (ok (not (clysm::alpha-char-p-ascii #\9)) "9 is not alphabetic")
    (ok (not (clysm::alpha-char-p-ascii #\Space)) "space is not alphabetic")
    (ok (not (clysm::alpha-char-p-ascii #\-)) "dash is not alphabetic")
    (ok (not (clysm::alpha-char-p-ascii #\_)) "underscore is not alphabetic")))

;;; ============================================================
;;; Phase 3: User Story 1 - Runtime Library Tests (T012-T018)
;;; ============================================================

;;; T012: Tests for string-char-rt
(deftest string-char-rt-returns-character-at-index ()
  "Verify string-char-rt returns the correct character at given index"
  (testing "basic character access"
    (ok (char= (clysm::string-char-rt "Hello" 0) #\H) "index 0 returns H")
    (ok (char= (clysm::string-char-rt "Hello" 1) #\e) "index 1 returns e")
    (ok (char= (clysm::string-char-rt "Hello" 4) #\o) "index 4 returns o")))

(deftest string-char-rt-handles-empty-string ()
  "Verify string-char-rt signals error for empty string"
  (testing "empty string bounds"
    (ok (signals (clysm::string-char-rt "" 0)) "empty string signals error")))

(deftest string-char-rt-signals-error-for-out-of-bounds ()
  "Verify string-char-rt signals error for index out of bounds"
  (testing "bounds checking"
    (ok (signals (clysm::string-char-rt "Hello" 5)) "index 5 is out of bounds")
    (ok (signals (clysm::string-char-rt "Hello" 10)) "index 10 is out of bounds")))

;;; T013: Tests for string-trim-rt
(deftest string-trim-rt-trims-both-ends ()
  "Verify string-trim-rt removes characters from both ends"
  (testing "basic trimming"
    (ok (string= (clysm::string-trim-rt " " "  hello  " nil nil) "hello")
        "trims spaces from both ends")
    (ok (string= (clysm::string-trim-rt " \t" "\t hello \t" nil nil) "hello")
        "trims mixed whitespace")))

(deftest string-trim-rt-handles-no-trim-needed ()
  "Verify string-trim-rt returns string unchanged when no trim needed"
  (testing "no trimming needed"
    (ok (string= (clysm::string-trim-rt " " "hello" nil nil) "hello")
        "no spaces to trim")
    (ok (string= (clysm::string-trim-rt "xyz" "hello" nil nil) "hello")
        "no matching characters")))

(deftest string-trim-rt-handles-empty-bag ()
  "Verify string-trim-rt with empty bag returns string unchanged"
  (testing "empty character bag"
    (ok (string= (clysm::string-trim-rt "" "  hello  " nil nil) "  hello  ")
        "empty bag means no trimming")))

(deftest string-trim-rt-handles-empty-string ()
  "Verify string-trim-rt handles empty input string"
  (testing "empty input string"
    (ok (string= (clysm::string-trim-rt " " "" nil nil) "")
        "empty string returns empty string")))

;;; T014: Tests for string-left-trim-rt
(deftest string-left-trim-rt-trims-left-only ()
  "Verify string-left-trim-rt removes characters from left end only"
  (testing "left trimming"
    (ok (string= (clysm::string-left-trim-rt " " "  hello  " nil nil) "hello  ")
        "trims left spaces only")
    (ok (string= (clysm::string-left-trim-rt "xy" "xyxhelloyx" nil nil) "helloyx")
        "trims matching chars from left")))

;;; T015: Tests for string-right-trim-rt
(deftest string-right-trim-rt-trims-right-only ()
  "Verify string-right-trim-rt removes characters from right end only"
  (testing "right trimming"
    (ok (string= (clysm::string-right-trim-rt " " "  hello  " nil nil) "  hello")
        "trims right spaces only")
    (ok (string= (clysm::string-right-trim-rt "xy" "xyxhelloyx" nil nil) "xyxhello")
        "trims matching chars from right")))

;;; T016: Tests for string-capitalize-rt
(deftest string-capitalize-rt-capitalizes-words ()
  "Verify string-capitalize-rt capitalizes first letter of each word"
  (testing "word capitalization"
    (ok (string= (clysm::string-capitalize-rt "hello world" nil nil) "Hello World")
        "capitalizes simple words")
    (ok (string= (clysm::string-capitalize-rt "HELLO WORLD" nil nil) "Hello World")
        "lowercases remaining letters")
    (ok (string= (clysm::string-capitalize-rt "heLLo wORLd" nil nil) "Hello World")
        "mixed case normalized")))

(deftest string-capitalize-rt-handles-word-boundaries ()
  "Verify string-capitalize-rt treats non-alpha as word boundaries"
  (testing "word boundary handling"
    (ok (string= (clysm::string-capitalize-rt "hello-world" nil nil) "Hello-World")
        "dash is word boundary")
    (ok (string= (clysm::string-capitalize-rt "hello123world" nil nil) "Hello123World")
        "digits are word boundaries")
    (ok (string= (clysm::string-capitalize-rt "  multiple   spaces  " nil nil) "  Multiple   Spaces  ")
        "preserves spaces")))

(deftest string-capitalize-rt-handles-empty-string ()
  "Verify string-capitalize-rt handles empty string"
  (testing "empty string"
    (ok (string= (clysm::string-capitalize-rt "" nil nil) "")
        "empty string returns empty string")))

;;; T017: Tests for nstring-capitalize-rt
(deftest nstring-capitalize-rt-modifies-in-place ()
  "Verify nstring-capitalize-rt modifies string destructively"
  (testing "destructive modification"
    (let ((str (copy-seq "hello world")))
      (let ((result (clysm::nstring-capitalize-rt str nil nil)))
        (ok (eq result str) "returns the same object")
        (ok (string= str "Hello World") "original string modified")))))

;;; T018: Tests for string-compare-ci-rt
(deftest string-compare-ci-rt-equal-comparison ()
  "Verify string-compare-ci-rt handles :equal comparison"
  (testing "case-insensitive equality"
    (ok (clysm::string-compare-ci-rt "ABC" "abc" nil nil nil nil :equal)
        "ABC equals abc case-insensitively")
    (ok (clysm::string-compare-ci-rt "Hello" "HELLO" nil nil nil nil :equal)
        "Hello equals HELLO case-insensitively")
    (ok (not (clysm::string-compare-ci-rt "Hello" "World" nil nil nil nil :equal))
        "Hello does not equal World")))

(deftest string-compare-ci-rt-not-equal-comparison ()
  "Verify string-compare-ci-rt handles :not-equal comparison"
  (testing "case-insensitive inequality"
    ;; Returns mismatch index for true, NIL for false
    (ok (clysm::string-compare-ci-rt "Hello" "World" nil nil nil nil :not-equal)
        "Hello not-equal World returns mismatch index")
    (ok (not (clysm::string-compare-ci-rt "ABC" "abc" nil nil nil nil :not-equal))
        "ABC not-equal abc returns NIL (they are equal)")))

(deftest string-compare-ci-rt-ordering-comparisons ()
  "Verify string-compare-ci-rt handles ordering comparisons"
  (testing "case-insensitive ordering"
    (ok (clysm::string-compare-ci-rt "abc" "def" nil nil nil nil :lt)
        "abc < def")
    (ok (clysm::string-compare-ci-rt "DEF" "abc" nil nil nil nil :gt)
        "DEF > abc")
    (ok (clysm::string-compare-ci-rt "abc" "abc" nil nil nil nil :le)
        "abc <= abc")
    (ok (clysm::string-compare-ci-rt "ABC" "abc" nil nil nil nil :ge)
        "ABC >= abc")))

;;; ============================================================
;;; Phase 4: User Story 4 - Keyword Argument Tests (T030-T033)
;;; ============================================================

;;; T030: Tests for string-trim-rt with :start/:end
(deftest string-trim-rt-respects-start-end ()
  "Verify string-trim-rt respects :start and :end bounds"
  (testing ":start/:end handling"
    ;; Only consider characters in [start, end) for trimming
    (ok (string= (clysm::string-trim-rt " " "  hello  " 2 7) "hello")
        ":start 2 :end 7 trims within range")
    (ok (string= (clysm::string-trim-rt " " "  hello  " 0 5) "hel")
        ":end 5 limits trimming range")))

;;; T031: Tests for string-capitalize-rt with :start/:end
(deftest string-capitalize-rt-respects-start-end ()
  "Verify string-capitalize-rt respects :start and :end bounds"
  (testing ":start/:end handling"
    (ok (string= (clysm::string-capitalize-rt "hello world" 6 nil) "hello World")
        ":start 6 capitalizes from 'world'")
    (ok (string= (clysm::string-capitalize-rt "hello world" nil 5) "Hello world")
        ":end 5 capitalizes only 'hello'")))

;;; T032: Tests for string-compare-ci-rt with :start1/:end1/:start2/:end2
(deftest string-compare-ci-rt-respects-all-bounds ()
  "Verify string-compare-ci-rt respects all start/end bounds"
  (testing "all bounds handling"
    (ok (clysm::string-compare-ci-rt "xxHELLOxx" "yyHELLOyy" 2 7 2 7 :equal)
        "comparing 'HELLO' substrings")
    (ok (not (clysm::string-compare-ci-rt "xxHELLOxx" "yyWORLDyy" 2 7 2 7 :equal))
        "comparing different substrings")))

;;; T033: Tests for bounds validation
(deftest bounds-validation-signals-error ()
  "Verify functions signal error for invalid bounds"
  (testing "bounds validation"
    (ok (signals (clysm::string-trim-rt " " "hello" 3 2))
        "start > end signals error")
    (ok (signals (clysm::string-capitalize-rt "hello" 10 nil))
        "start beyond length signals error")))

;;; ============================================================
;;; Phase 5: User Story 2 - Dispatch Table Tests (T038-T041)
;;; ============================================================

;;; T038: Tests for char registration
(deftest char-registered-in-runtime-table ()
  "Verify char and schar are registered in *runtime-function-table*"
  (testing "char registration"
    (ok (gethash 'char clysm/compiler/codegen/func-section::*runtime-function-table*)
        "char is registered")
    (ok (gethash 'schar clysm/compiler/codegen/func-section::*runtime-function-table*)
        "schar is registered")))

;;; T039: Tests for string-trim family registration
(deftest string-trim-family-registered ()
  "Verify string-trim family is registered in *runtime-function-table*"
  (testing "string-trim registration"
    (ok (gethash 'string-trim clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-trim is registered")
    (ok (gethash 'string-left-trim clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-left-trim is registered")
    (ok (gethash 'string-right-trim clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-right-trim is registered")))

;;; T040: Tests for string-capitalize family registration
(deftest string-capitalize-family-registered ()
  "Verify string-capitalize family is registered in *runtime-function-table*"
  (testing "string-capitalize registration"
    (ok (gethash 'string-capitalize clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-capitalize is registered")
    (ok (gethash 'nstring-capitalize clysm/compiler/codegen/func-section::*runtime-function-table*)
        "nstring-capitalize is registered")))

;;; T041: Tests for string comparison function registrations
(deftest string-comparison-functions-registered ()
  "Verify string comparison functions are registered in *runtime-function-table*"
  (testing "string comparison registration"
    (ok (gethash 'string-equal clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-equal is registered")
    (ok (gethash 'string-not-equal clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-not-equal is registered")
    (ok (gethash 'string-lessp clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-lessp is registered")
    (ok (gethash 'string-greaterp clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-greaterp is registered")
    (ok (gethash 'string-not-lessp clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-not-lessp is registered")
    (ok (gethash 'string-not-greaterp clysm/compiler/codegen/func-section::*runtime-function-table*)
        "string-not-greaterp is registered")))
