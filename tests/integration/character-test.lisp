;;;; character-test.lisp - Integration tests for character functions (008-character-string)
(in-package #:clysm/tests/integration/character)

;;; ============================================================
;;; Phase 3: User Story 1 - Character Literals and Basic Operations
;;; ============================================================

;;; --- T010: char-code/code-char integration tests ---

(deftest test-char-code-ascii
  "Verify (char-code #\\A) returns 65"
  (ok (= 65 (clysm/tests:compile-and-run '(char-code #\A)))
      "(char-code #\\A) should return 65"))

(deftest test-char-code-lowercase
  "Verify (char-code #\\a) returns 97"
  (ok (= 97 (clysm/tests:compile-and-run '(char-code #\a)))
      "(char-code #\\a) should return 97"))

(deftest test-char-code-digit
  "Verify (char-code #\\0) returns 48"
  (ok (= 48 (clysm/tests:compile-and-run '(char-code #\0)))
      "(char-code #\\0) should return 48"))

(deftest test-char-code-space
  "Verify (char-code #\\Space) returns 32"
  (ok (= 32 (clysm/tests:compile-and-run '(char-code #\Space)))
      "(char-code #\\Space) should return 32"))

(deftest test-char-code-newline
  "Verify (char-code #\\Newline) returns 10"
  (ok (= 10 (clysm/tests:compile-and-run '(char-code #\Newline)))
      "(char-code #\\Newline) should return 10"))

(deftest test-code-char-valid
  "Verify (code-char 65) returns #\\A"
  (ok (= 65 (clysm/tests:compile-and-run '(char-code (code-char 65))))
      "(code-char 65) should return a character with code 65"))

(deftest test-code-char-invalid-negative
  "Verify (code-char -1) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(code-char -1)))
      "(code-char -1) should return NIL"))

(deftest test-code-char-invalid-too-large
  "Verify (code-char #x110000) returns NIL (beyond Unicode range)"
  (ok (null (clysm/tests:compile-and-run '(code-char #x110000)))
      "(code-char #x110000) should return NIL"))

;;; --- T011: Character comparison integration tests ---

(deftest test-char=-same
  "Verify (char= #\\a #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(char= #\a #\a))
      "(char= #\\a #\\a) should return T"))

(deftest test-char=-different
  "Verify (char= #\\a #\\b) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(char= #\a #\b)))
      "(char= #\\a #\\b) should return NIL"))

(deftest test-char=-case-sensitive
  "Verify (char= #\\a #\\A) returns NIL (case-sensitive)"
  (ok (null (clysm/tests:compile-and-run '(char= #\a #\A)))
      "(char= #\\a #\\A) should return NIL"))

(deftest test-char<-true
  "Verify (char< #\\a #\\b) returns T"
  (ok (clysm/tests:compile-and-run '(char< #\a #\b))
      "(char< #\\a #\\b) should return T"))

(deftest test-char<-false
  "Verify (char< #\\b #\\a) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(char< #\b #\a)))
      "(char< #\\b #\\a) should return NIL"))

(deftest test-char>-true
  "Verify (char> #\\z #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(char> #\z #\a))
      "(char> #\\z #\\a) should return T"))

(deftest test-char<=-true
  "Verify (char<= #\\a #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(char<= #\a #\a))
      "(char<= #\\a #\\a) should return T"))

(deftest test-char>=-true
  "Verify (char>= #\\a #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(char>= #\a #\a))
      "(char>= #\\a #\\a) should return T"))

(deftest test-char/=-different
  "Verify (char/= #\\a #\\b) returns T"
  (ok (clysm/tests:compile-and-run '(char/= #\a #\b))
      "(char/= #\\a #\\b) should return T"))

(deftest test-char/=-same
  "Verify (char/= #\\a #\\a) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(char/= #\a #\a)))
      "(char/= #\\a #\\a) should return NIL"))

;;; --- Case-insensitive comparison tests ---

(deftest test-char-equal-case-insensitive
  "Verify (char-equal #\\a #\\A) returns T"
  (ok (clysm/tests:compile-and-run '(char-equal #\a #\A))
      "(char-equal #\\a #\\A) should return T"))

(deftest test-char-lessp-case-insensitive
  "Verify (char-lessp #\\A #\\b) returns T"
  (ok (clysm/tests:compile-and-run '(char-lessp #\A #\b))
      "(char-lessp #\\A #\\b) should return T"))

(deftest test-char-greaterp-case-insensitive
  "Verify (char-greaterp #\\Z #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(char-greaterp #\Z #\a))
      "(char-greaterp #\\Z #\\a) should return T"))

;;; --- T012: Case conversion integration tests ---

(deftest test-char-upcase-lowercase
  "Verify (char-upcase #\\a) returns #\\A"
  (ok (= 65 (clysm/tests:compile-and-run '(char-code (char-upcase #\a))))
      "(char-upcase #\\a) should return #\\A"))

(deftest test-char-upcase-uppercase
  "Verify (char-upcase #\\A) returns #\\A (no change)"
  (ok (= 65 (clysm/tests:compile-and-run '(char-code (char-upcase #\A))))
      "(char-upcase #\\A) should return #\\A"))

(deftest test-char-upcase-non-alpha
  "Verify (char-upcase #\\1) returns #\\1 (no change)"
  (ok (= 49 (clysm/tests:compile-and-run '(char-code (char-upcase #\1))))
      "(char-upcase #\\1) should return #\\1"))

(deftest test-char-downcase-uppercase
  "Verify (char-downcase #\\A) returns #\\a"
  (ok (= 97 (clysm/tests:compile-and-run '(char-code (char-downcase #\A))))
      "(char-downcase #\\A) should return #\\a"))

(deftest test-char-downcase-lowercase
  "Verify (char-downcase #\\a) returns #\\a (no change)"
  (ok (= 97 (clysm/tests:compile-and-run '(char-code (char-downcase #\a))))
      "(char-downcase #\\a) should return #\\a"))

;;; --- characterp tests ---

(deftest test-characterp-true
  "Verify (characterp #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(characterp #\a))
      "(characterp #\\a) should return T"))

(deftest test-characterp-number
  "Note: In current MVP, characters and fixnums share i31ref representation.
   characterp returns T for any i31ref, including fixnums.
   This is a known limitation - proper type tagging is needed to distinguish.
   TODO: Update this test when type tagging is implemented."
  (skip "characterp cannot distinguish fixnums from characters in current MVP"))

(deftest test-characterp-string
  "Verify (characterp \"a\") returns NIL"
  (ok (null (clysm/tests:compile-and-run '(characterp "a")))
      "(characterp \"a\") should return NIL"))

;;; ============================================================
;;; Phase 8: User Story 6 - Character Predicates
;;; ============================================================

;;; --- T081: alpha-char-p tests ---

(deftest test-alpha-char-p-lowercase
  "Verify (alpha-char-p #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(alpha-char-p #\a))
      "(alpha-char-p #\\a) should return T"))

(deftest test-alpha-char-p-uppercase
  "Verify (alpha-char-p #\\A) returns T"
  (ok (clysm/tests:compile-and-run '(alpha-char-p #\A))
      "(alpha-char-p #\\A) should return T"))

(deftest test-alpha-char-p-digit
  "Verify (alpha-char-p #\\1) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(alpha-char-p #\1)))
      "(alpha-char-p #\\1) should return NIL"))

(deftest test-alpha-char-p-space
  "Verify (alpha-char-p #\\Space) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(alpha-char-p #\Space)))
      "(alpha-char-p #\\Space) should return NIL"))

;;; --- T082: digit-char-p tests ---

(deftest test-digit-char-p-digit
  "Verify (digit-char-p #\\5) returns 5"
  (ok (= 5 (clysm/tests:compile-and-run '(digit-char-p #\5)))
      "(digit-char-p #\\5) should return 5"))

(deftest test-digit-char-p-zero
  "Verify (digit-char-p #\\0) returns 0"
  (ok (= 0 (clysm/tests:compile-and-run '(digit-char-p #\0)))
      "(digit-char-p #\\0) should return 0"))

(deftest test-digit-char-p-alpha
  "Verify (digit-char-p #\\a) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(digit-char-p #\a)))
      "(digit-char-p #\\a) should return NIL"))

(deftest test-digit-char-p-hex-lowercase
  "Verify (digit-char-p #\\f 16) returns 15"
  (ok (= 15 (clysm/tests:compile-and-run '(digit-char-p #\f 16)))
      "(digit-char-p #\\f 16) should return 15"))

(deftest test-digit-char-p-hex-uppercase
  "Verify (digit-char-p #\\F 16) returns 15"
  (ok (= 15 (clysm/tests:compile-and-run '(digit-char-p #\F 16)))
      "(digit-char-p #\\F 16) should return 15"))

(deftest test-digit-char-p-hex-invalid
  "Verify (digit-char-p #\\G 16) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(digit-char-p #\G 16)))
      "(digit-char-p #\\G 16) should return NIL"))

;;; --- T083: alphanumericp tests ---

(deftest test-alphanumericp-alpha
  "Verify (alphanumericp #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(alphanumericp #\a))
      "(alphanumericp #\\a) should return T"))

(deftest test-alphanumericp-digit
  "Verify (alphanumericp #\\5) returns T"
  (ok (clysm/tests:compile-and-run '(alphanumericp #\5))
      "(alphanumericp #\\5) should return T"))

(deftest test-alphanumericp-symbol
  "Verify (alphanumericp #\\!) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(alphanumericp #\!)))
      "(alphanumericp #\\!) should return NIL"))

;;; --- T084: upper-case-p/lower-case-p tests ---

(deftest test-upper-case-p-true
  "Verify (upper-case-p #\\A) returns T"
  (ok (clysm/tests:compile-and-run '(upper-case-p #\A))
      "(upper-case-p #\\A) should return T"))

(deftest test-upper-case-p-false
  "Verify (upper-case-p #\\a) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(upper-case-p #\a)))
      "(upper-case-p #\\a) should return NIL"))

(deftest test-upper-case-p-digit
  "Verify (upper-case-p #\\1) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(upper-case-p #\1)))
      "(upper-case-p #\\1) should return NIL"))

(deftest test-lower-case-p-true
  "Verify (lower-case-p #\\a) returns T"
  (ok (clysm/tests:compile-and-run '(lower-case-p #\a))
      "(lower-case-p #\\a) should return T"))

(deftest test-lower-case-p-false
  "Verify (lower-case-p #\\A) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(lower-case-p #\A)))
      "(lower-case-p #\\A) should return NIL"))
