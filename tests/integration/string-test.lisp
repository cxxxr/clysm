;;;; string-test.lisp - Integration tests for string functions (008-character-string)
(in-package #:clysm/tests/integration/string)

;;; ============================================================
;;; Phase 4: User Story 2 - String Literals and Basic Access
;;; ============================================================

;;; --- T037: String length tests (ASCII/Unicode) ---

(deftest test-string-length-ascii
  "Verify (length \"hello\") returns 5"
  (ok (= 5 (clysm/tests:compile-and-run '(length "hello")))
      "(length \"hello\") should return 5"))

(deftest test-string-length-empty
  "Verify (length \"\") returns 0"
  (ok (= 0 (clysm/tests:compile-and-run '(length "")))
      "(length \"\") should return 0"))

;;; --- T038: char/schar access tests ---

(deftest test-string-char-first
  "Verify (char \"hello\" 0) returns #\\h"
  (ok (= 104 (clysm/tests:compile-and-run '(char-code (char "hello" 0))))
      "(char \"hello\" 0) should return #\\h"))

(deftest test-string-char-last
  "Verify (char \"hello\" 4) returns #\\o"
  (ok (= 111 (clysm/tests:compile-and-run '(char-code (char "hello" 4))))
      "(char \"hello\" 4) should return #\\o"))

(deftest test-string-char-middle
  "Verify (char \"hello\" 2) returns #\\l"
  (ok (= 108 (clysm/tests:compile-and-run '(char-code (char "hello" 2))))
      "(char \"hello\" 2) should return #\\l"))

(deftest test-string-schar
  "Verify schar works like char for simple strings"
  (ok (= 101 (clysm/tests:compile-and-run '(char-code (schar "hello" 1))))
      "(schar \"hello\" 1) should return #\\e"))

;;; --- T039: stringp tests ---

(deftest test-stringp-true
  "Verify (stringp \"hello\") returns T"
  (ok (clysm/tests:compile-and-run '(stringp "hello"))
      "(stringp \"hello\") should return T"))

(deftest test-stringp-empty
  "Verify (stringp \"\") returns T"
  (ok (clysm/tests:compile-and-run '(stringp ""))
      "(stringp \"\") should return T"))

(deftest test-stringp-char
  "Verify (stringp #\\a) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(stringp #\a)))
      "(stringp #\\a) should return NIL"))

(deftest test-stringp-number
  "Verify (stringp 123) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(stringp 123)))
      "(stringp 123) should return NIL"))

;;; ============================================================
;;; Phase 5: User Story 3 - String Comparison
;;; ============================================================

;;; --- T048: string=/string/= tests ---

(deftest test-string=-equal
  "Verify (string= \"abc\" \"abc\") returns T"
  (ok (clysm/tests:compile-and-run '(string= "abc" "abc"))
      "(string= \"abc\" \"abc\") should return T"))

(deftest test-string=-not-equal
  "Verify (string= \"abc\" \"def\") returns NIL"
  (ok (null (clysm/tests:compile-and-run '(string= "abc" "def")))
      "(string= \"abc\" \"def\") should return NIL"))

(deftest test-string=-case-sensitive
  "Verify (string= \"abc\" \"ABC\") returns NIL (case-sensitive)"
  (ok (null (clysm/tests:compile-and-run '(string= "abc" "ABC")))
      "(string= \"abc\" \"ABC\") should return NIL"))

(deftest test-string/=-different
  "Verify (string/= \"abc\" \"def\") returns T"
  (ok (clysm/tests:compile-and-run '(string/= "abc" "def"))
      "(string/= \"abc\" \"def\") should return non-NIL"))

(deftest test-string/=-same
  "Verify (string/= \"abc\" \"abc\") returns NIL"
  (ok (null (clysm/tests:compile-and-run '(string/= "abc" "abc")))
      "(string/= \"abc\" \"abc\") should return NIL"))

;;; --- T049: string</string>/string<=/string>= tests ---

(deftest test-string<-true
  "Verify (string< \"apple\" \"banana\") returns T"
  (ok (clysm/tests:compile-and-run '(string< "apple" "banana"))
      "(string< \"apple\" \"banana\") should return non-NIL"))

(deftest test-string<-false
  "Verify (string< \"banana\" \"apple\") returns NIL"
  (ok (null (clysm/tests:compile-and-run '(string< "banana" "apple")))
      "(string< \"banana\" \"apple\") should return NIL"))

(deftest test-string<-prefix
  "Verify (string< \"ab\" \"abc\") returns T (prefix is less)"
  (ok (clysm/tests:compile-and-run '(string< "ab" "abc"))
      "(string< \"ab\" \"abc\") should return non-NIL"))

(deftest test-string>-true
  "Verify (string> \"banana\" \"apple\") returns T"
  (ok (clysm/tests:compile-and-run '(string> "banana" "apple"))
      "(string> \"banana\" \"apple\") should return non-NIL"))

(deftest test-string<=-equal
  "Verify (string<= \"abc\" \"abc\") returns T"
  (ok (clysm/tests:compile-and-run '(string<= "abc" "abc"))
      "(string<= \"abc\" \"abc\") should return non-NIL"))

(deftest test-string>=-equal
  "Verify (string>= \"abc\" \"abc\") returns T"
  (ok (clysm/tests:compile-and-run '(string>= "abc" "abc"))
      "(string>= \"abc\" \"abc\") should return non-NIL"))

;;; --- T050: Case-insensitive string comparison tests ---

(deftest test-string-equal-case-insensitive
  "Verify (string-equal \"abc\" \"ABC\") returns T"
  (ok (clysm/tests:compile-and-run '(string-equal "abc" "ABC"))
      "(string-equal \"abc\" \"ABC\") should return T"))

(deftest test-string-lessp-case-insensitive
  "Verify (string-lessp \"ABC\" \"def\") returns T"
  (ok (clysm/tests:compile-and-run '(string-lessp "ABC" "def"))
      "(string-lessp \"ABC\" \"def\") should return non-NIL"))

(deftest test-string-greaterp-case-insensitive
  "Verify (string-greaterp \"DEF\" \"abc\") returns T"
  (ok (clysm/tests:compile-and-run '(string-greaterp "DEF" "abc"))
      "(string-greaterp \"DEF\" \"abc\") should return non-NIL"))

;;; ============================================================
;;; Phase 6: User Story 4 - String Generation and Conversion
;;; ============================================================

;;; --- T064: make-string tests ---

(deftest test-make-string-default
  "Verify (make-string 5) creates a 5-character string"
  (ok (= 5 (clysm/tests:compile-and-run '(length (make-string 5))))
      "(length (make-string 5)) should return 5"))

(deftest test-make-string-initial-element
  "Verify (make-string 3 :initial-element #\\x) creates \"xxx\""
  (ok (= 3 (clysm/tests:compile-and-run
            '(length (make-string 3 :initial-element #\x))))
      "(length (make-string 3 :initial-element #\\x)) should return 3")
  (ok (= 120 (clysm/tests:compile-and-run
              '(char-code (char (make-string 3 :initial-element #\x) 0))))
      "(char (make-string 3 :initial-element #\\x) 0) should return #\\x"))

(deftest test-make-string-zero
  "Verify (make-string 0) creates an empty string"
  (ok (= 0 (clysm/tests:compile-and-run '(length (make-string 0))))
      "(length (make-string 0)) should return 0"))

;;; --- T065: string conversion tests ---

(deftest test-string-from-char
  "Verify (string #\\a) returns \"a\""
  (ok (= 1 (clysm/tests:compile-and-run '(length (string #\a))))
      "(length (string #\\a)) should return 1")
  (ok (= 97 (clysm/tests:compile-and-run
             '(char-code (char (string #\a) 0))))
      "(char (string #\\a) 0) should return #\\a"))

;;; --- T066: string-upcase/downcase/capitalize tests ---

(deftest test-string-upcase
  "Verify (string-upcase \"hello\") returns \"HELLO\""
  (ok (= 72 (clysm/tests:compile-and-run
             '(char-code (char (string-upcase "hello") 0))))
      "(char (string-upcase \"hello\") 0) should return #\\H")
  (ok (= 5 (clysm/tests:compile-and-run
            '(length (string-upcase "hello"))))
      "(length (string-upcase \"hello\")) should return 5"))

(deftest test-string-downcase
  "Verify (string-downcase \"HELLO\") returns \"hello\""
  (ok (= 104 (clysm/tests:compile-and-run
              '(char-code (char (string-downcase "HELLO") 0))))
      "(char (string-downcase \"HELLO\") 0) should return #\\h"))

(deftest test-string-capitalize
  "Verify (string-capitalize \"hello world\") returns \"Hello World\""
  (ok (= 72 (clysm/tests:compile-and-run
             '(char-code (char (string-capitalize "hello world") 0))))
      "(char (string-capitalize \"hello world\") 0) should return #\\H"))

;;; ============================================================
;;; Phase 7: User Story 5 - Substrings and Concatenation
;;; ============================================================

;;; --- T073: subseq string tests ---

(deftest test-subseq-string-middle
  "Verify (subseq \"hello\" 1 4) returns \"ell\""
  (ok (= 3 (clysm/tests:compile-and-run '(length (subseq "hello" 1 4))))
      "(length (subseq \"hello\" 1 4)) should return 3")
  (ok (= 101 (clysm/tests:compile-and-run
              '(char-code (char (subseq "hello" 1 4) 0))))
      "(char (subseq \"hello\" 1 4) 0) should return #\\e"))

(deftest test-subseq-string-from-start
  "Verify (subseq \"hello\" 0 3) returns \"hel\""
  (ok (= 3 (clysm/tests:compile-and-run '(length (subseq "hello" 0 3))))
      "(length (subseq \"hello\" 0 3)) should return 3"))

(deftest test-subseq-string-to-end
  "Verify (subseq \"hello\" 2) returns \"llo\""
  (ok (= 3 (clysm/tests:compile-and-run '(length (subseq "hello" 2))))
      "(length (subseq \"hello\" 2)) should return 3"))

(deftest test-subseq-string-empty
  "Verify (subseq \"hello\" 2 2) returns \"\""
  (ok (= 0 (clysm/tests:compile-and-run '(length (subseq "hello" 2 2))))
      "(length (subseq \"hello\" 2 2)) should return 0"))

;;; --- T074: concatenate 'string tests ---

(deftest test-concatenate-two-strings
  "Verify (concatenate 'string \"hello\" \" \" \"world\") works"
  (ok (= 11 (clysm/tests:compile-and-run
             '(length (concatenate (quote string) "hello" " " "world"))))
      "(length (concatenate 'string \"hello\" \" \" \"world\")) should return 11"))

(deftest test-concatenate-empty
  "Verify concatenate with empty strings"
  (ok (= 5 (clysm/tests:compile-and-run
            '(length (concatenate (quote string) "" "hello" ""))))
      "(length (concatenate 'string \"\" \"hello\" \"\")) should return 5"))

(deftest test-concatenate-single
  "Verify concatenate with single string"
  (ok (= 5 (clysm/tests:compile-and-run
            '(length (concatenate (quote string) "hello"))))
      "(length (concatenate 'string \"hello\")) should return 5"))

;;; --- T075: Edge case tests ---

(deftest test-string-empty-char-error
  "Verify accessing empty string throws error"
  ;; Note: This test may need adjustment based on error handling implementation
  (ok (signals (clysm/tests:compile-and-run '(char "" 0)))
      "(char \"\" 0) should signal an error"))

;;; ============================================================
;;; Phase 9: Unicode and Edge Cases
;;; ============================================================

;;; --- T091: Unicode string tests ---
;;; Note: These tests will be enabled when Unicode support is complete

;;; --- T092: Edge case tests ---

(deftest test-string-single-char
  "Verify single character string operations"
  (ok (= 1 (clysm/tests:compile-and-run '(length "a")))
      "(length \"a\") should return 1")
  (ok (= 97 (clysm/tests:compile-and-run '(char-code (char "a" 0))))
      "(char \"a\" 0) should return #\\a"))
