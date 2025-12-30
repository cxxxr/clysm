;;;; tests/unit/character-functions.lisp
;;;; Unit tests for ANSI CL character functions (001-ansi-char-functions)
;;;;
;;;; Phase 16A: Character Functions
;;;; Tests for graphic-char-p, standard-char-p, both-case-p,
;;;; char-name, name-char, digit-char, char-int
;;;;
;;;; ANSI CL References (Constitution Principle IX):
;;;; - graphic-char-p: resources/HyperSpec/Body/f_graphi.htm
;;;; - standard-char-p: resources/HyperSpec/Body/f_std_ch.htm
;;;; - both-case-p: resources/HyperSpec/Body/f_upper_.htm
;;;; - char-name: resources/HyperSpec/Body/f_char_n.htm
;;;; - name-char: resources/HyperSpec/Body/f_name_c.htm
;;;; - digit-char: resources/HyperSpec/Body/f_digit_.htm
;;;; - char-int: resources/HyperSpec/Body/f_char_i.htm

(in-package #:clysm/tests/unit/character-functions)

;;; ============================================================
;;; User Story 1: Character Type Classification (Priority: P1)
;;; ============================================================

;;; --- T010: graphic-char-p tests ---
;;; Printable characters (32-126) return T, control chars return NIL

(deftest test-graphic-char-p-printable-letter
  "graphic-char-p returns T for printable letter #\\A"
  (ok (compile-and-run '(graphic-char-p #\A))
      "(graphic-char-p #\\A) should return T"))

(deftest test-graphic-char-p-space
  "graphic-char-p returns T for #\\Space (code 32)"
  (ok (compile-and-run '(graphic-char-p #\Space))
      "(graphic-char-p #\\Space) should return T"))

(deftest test-graphic-char-p-tilde
  "graphic-char-p returns T for #\\~ (code 126, highest printable)"
  (ok (compile-and-run '(graphic-char-p #\~))
      "(graphic-char-p #\\~) should return T"))

(deftest test-graphic-char-p-control-null
  "graphic-char-p returns NIL for control character #\\Null (U+0000)"
  (ok (not (compile-and-run '(graphic-char-p #\Null)))
      "(graphic-char-p #\\Null) should return NIL"))

(deftest test-graphic-char-p-control-tab
  "graphic-char-p returns NIL for control character #\\Tab (U+0009)"
  (ok (not (compile-and-run '(graphic-char-p #\Tab)))
      "(graphic-char-p #\\Tab) should return NIL"))

(deftest test-graphic-char-p-control-newline
  "graphic-char-p returns NIL for control character #\\Newline (U+000A)"
  (ok (not (compile-and-run '(graphic-char-p #\Newline)))
      "(graphic-char-p #\\Newline) should return NIL"))

;;; --- T011: standard-char-p tests ---
;;; 96 standard characters: A-Z, a-z, 0-9, space, newline, punctuation

(deftest test-standard-char-p-uppercase
  "standard-char-p returns T for uppercase letter #\\A"
  (ok (compile-and-run '(standard-char-p #\A))
      "(standard-char-p #\\A) should return T"))

(deftest test-standard-char-p-lowercase
  "standard-char-p returns T for lowercase letter #\\a"
  (ok (compile-and-run '(standard-char-p #\a))
      "(standard-char-p #\\a) should return T"))

(deftest test-standard-char-p-digit
  "standard-char-p returns T for digit #\\5"
  (ok (compile-and-run '(standard-char-p #\5))
      "(standard-char-p #\\5) should return T"))

(deftest test-standard-char-p-space
  "standard-char-p returns T for #\\Space"
  (ok (compile-and-run '(standard-char-p #\Space))
      "(standard-char-p #\\Space) should return T"))

(deftest test-standard-char-p-newline
  "standard-char-p returns T for #\\Newline"
  (ok (compile-and-run '(standard-char-p #\Newline))
      "(standard-char-p #\\Newline) should return T"))

(deftest test-standard-char-p-unicode-hiragana
  "standard-char-p returns NIL for Unicode hiragana (U+3042)"
  ;; Hiragana 'a' is beyond ASCII
  (ok (not (compile-and-run '(standard-char-p (code-char #x3042))))
      "(standard-char-p hiragana-a) should return NIL"))

(deftest test-standard-char-p-tab
  "standard-char-p returns NIL for #\\Tab (not in 96 standard chars)"
  (ok (not (compile-and-run '(standard-char-p #\Tab)))
      "(standard-char-p #\\Tab) should return NIL"))

;;; --- T012: both-case-p tests ---
;;; Characters with both uppercase and lowercase variants

(deftest test-both-case-p-uppercase
  "both-case-p returns T for uppercase #\\A"
  (ok (compile-and-run '(both-case-p #\A))
      "(both-case-p #\\A) should return T"))

(deftest test-both-case-p-lowercase
  "both-case-p returns T for lowercase #\\z"
  (ok (compile-and-run '(both-case-p #\z))
      "(both-case-p #\\z) should return T"))

(deftest test-both-case-p-digit
  "both-case-p returns NIL for digit #\\5"
  (ok (not (compile-and-run '(both-case-p #\5)))
      "(both-case-p #\\5) should return NIL"))

(deftest test-both-case-p-punctuation
  "both-case-p returns NIL for punctuation #\\!"
  (ok (not (compile-and-run '(both-case-p #\!)))
      "(both-case-p #\\!) should return NIL"))

(deftest test-both-case-p-space
  "both-case-p returns NIL for #\\Space"
  (ok (not (compile-and-run '(both-case-p #\Space)))
      "(both-case-p #\\Space) should return NIL"))

;;; ============================================================
;;; User Story 2: Character Name Conversion (Priority: P2)
;;; ============================================================

;;; --- T018: char-name tests ---
;;; Returns name string for named characters, NIL for unnamed

(deftest test-char-name-space
  "char-name returns \"Space\" for #\\Space"
  (ok (string= "Space" (compile-and-run '(char-name #\Space)))
      "(char-name #\\Space) should return \"Space\""))

(deftest test-char-name-newline
  "char-name returns \"Newline\" for #\\Newline"
  (ok (string= "Newline" (compile-and-run '(char-name #\Newline)))
      "(char-name #\\Newline) should return \"Newline\""))

(deftest test-char-name-tab
  "char-name returns \"Tab\" for #\\Tab"
  (ok (string= "Tab" (compile-and-run '(char-name #\Tab)))
      "(char-name #\\Tab) should return \"Tab\""))

(deftest test-char-name-return
  "char-name returns \"Return\" for #\\Return"
  (ok (string= "Return" (compile-and-run '(char-name #\Return)))
      "(char-name #\\Return) should return \"Return\""))

(deftest test-char-name-null
  "char-name returns \"Null\" for #\\Null"
  (ok (string= "Null" (compile-and-run '(char-name #\Null)))
      "(char-name #\\Null) should return \"Null\""))

(deftest test-char-name-printable-nil
  "char-name returns NIL for regular printable character #\\A"
  (ok (null (compile-and-run '(char-name #\A)))
      "(char-name #\\A) should return NIL"))

;;; --- T019: name-char tests (case-insensitive) ---
;;; Returns character for valid name, NIL for invalid

(deftest test-name-char-space
  "name-char returns #\\Space for \"Space\""
  (ok (char= #\Space (compile-and-run '(name-char "Space")))
      "(name-char \"Space\") should return #\\Space"))

(deftest test-name-char-uppercase
  "name-char is case-insensitive: \"NEWLINE\" returns #\\Newline"
  (ok (char= #\Newline (compile-and-run '(name-char "NEWLINE")))
      "(name-char \"NEWLINE\") should return #\\Newline"))

(deftest test-name-char-lowercase
  "name-char is case-insensitive: \"newline\" returns #\\Newline"
  (ok (char= #\Newline (compile-and-run '(name-char "newline")))
      "(name-char \"newline\") should return #\\Newline"))

(deftest test-name-char-mixed-case
  "name-char is case-insensitive: \"NeWlInE\" returns #\\Newline"
  (ok (char= #\Newline (compile-and-run '(name-char "NeWlInE")))
      "(name-char \"NeWlInE\") should return #\\Newline"))

(deftest test-name-char-invalid
  "name-char returns NIL for invalid name \"InvalidName\""
  (ok (null (compile-and-run '(name-char "InvalidName")))
      "(name-char \"InvalidName\") should return NIL"))

(deftest test-name-char-tab
  "name-char returns #\\Tab for \"Tab\""
  (ok (char= #\Tab (compile-and-run '(name-char "Tab")))
      "(name-char \"Tab\") should return #\\Tab"))

;;; ============================================================
;;; User Story 3: Digit-Character Conversion (Priority: P2)
;;; ============================================================

;;; --- T025: digit-char with radix 10 ---

(deftest test-digit-char-zero
  "digit-char returns #\\0 for weight 0 in radix 10"
  (ok (char= #\0 (compile-and-run '(digit-char 0)))
      "(digit-char 0) should return #\\0"))

(deftest test-digit-char-five
  "digit-char returns #\\5 for weight 5 in radix 10"
  (ok (char= #\5 (compile-and-run '(digit-char 5)))
      "(digit-char 5) should return #\\5"))

(deftest test-digit-char-nine
  "digit-char returns #\\9 for weight 9 in radix 10"
  (ok (char= #\9 (compile-and-run '(digit-char 9)))
      "(digit-char 9) should return #\\9"))

;;; --- T026: digit-char with radix 16 (hex) ---

(deftest test-digit-char-hex-ten
  "digit-char returns #\\A for weight 10 in radix 16"
  (ok (char= #\A (compile-and-run '(digit-char 10 16)))
      "(digit-char 10 16) should return #\\A"))

(deftest test-digit-char-hex-fifteen
  "digit-char returns #\\F for weight 15 in radix 16"
  (ok (char= #\F (compile-and-run '(digit-char 15 16)))
      "(digit-char 15 16) should return #\\F"))

(deftest test-digit-char-base36
  "digit-char returns #\\Z for weight 35 in radix 36"
  (ok (char= #\Z (compile-and-run '(digit-char 35 36)))
      "(digit-char 35 36) should return #\\Z"))

;;; --- T027: digit-char edge cases ---

(deftest test-digit-char-weight-ge-radix
  "digit-char returns NIL when weight >= radix"
  (ok (null (compile-and-run '(digit-char 10 10)))
      "(digit-char 10 10) should return NIL"))

(deftest test-digit-char-negative-weight
  "digit-char returns NIL for negative weight"
  (ok (null (compile-and-run '(digit-char -1)))
      "(digit-char -1) should return NIL"))

(deftest test-digit-char-weight-exceeds-radix
  "digit-char returns NIL when weight 5 with radix 5"
  (ok (null (compile-and-run '(digit-char 5 5)))
      "(digit-char 5 5) should return NIL"))

;;; ============================================================
;;; User Story 4: Character Integer Conversion (Priority: P3)
;;; ============================================================

;;; --- T032: char-int tests ---
;;; Returns integer encoding (equivalent to char-code in this impl)

(deftest test-char-int-uppercase-a
  "char-int returns 65 for #\\A (ASCII/UTF-8)"
  (ok (= 65 (compile-and-run '(char-int #\A)))
      "(char-int #\\A) should return 65"))

(deftest test-char-int-space
  "char-int returns 32 for #\\Space"
  (ok (= 32 (compile-and-run '(char-int #\Space)))
      "(char-int #\\Space) should return 32"))

(deftest test-char-int-null
  "char-int returns 0 for #\\Null"
  (ok (= 0 (compile-and-run '(char-int #\Null)))
      "(char-int #\\Null) should return 0"))

(deftest test-char-int-equal-chars
  "char-int returns same value for equal characters"
  (ok (= (compile-and-run '(char-int #\X))
         (compile-and-run '(char-int #\X)))
      "Two equal characters should have same char-int"))

(deftest test-char-int-different-chars
  "char-int returns different values for different characters"
  (ok (/= (compile-and-run '(char-int #\A))
          (compile-and-run '(char-int #\B)))
      "Different characters should have different char-int values"))

;;; ============================================================
;;; Round-trip Verification Tests
;;; ============================================================

;;; --- T024: char-name/name-char round-trip ---

(deftest test-round-trip-space
  "Round-trip: (name-char (char-name #\\Space)) = #\\Space"
  (ok (char= #\Space (compile-and-run '(name-char (char-name #\Space))))
      "name-char of char-name for Space should return Space"))

(deftest test-round-trip-newline
  "Round-trip: (name-char (char-name #\\Newline)) = #\\Newline"
  (ok (char= #\Newline (compile-and-run '(name-char (char-name #\Newline))))
      "name-char of char-name for Newline should return Newline"))

;;; --- T031: digit-char/digit-char-p round-trip ---

(deftest test-round-trip-digit-5
  "Round-trip: (digit-char (digit-char-p #\\5)) = #\\5"
  (ok (char= #\5 (compile-and-run '(digit-char (digit-char-p #\5))))
      "digit-char of digit-char-p for #\\5 should return #\\5"))

(deftest test-round-trip-hex-a
  "Round-trip: (digit-char (digit-char-p #\\A 16) 16) = #\\A"
  (ok (char= #\A (compile-and-run '(digit-char (digit-char-p #\A 16) 16)))
      "digit-char of digit-char-p for #\\A in hex should return #\\A"))
