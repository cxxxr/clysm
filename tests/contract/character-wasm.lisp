;;;; tests/contract/character-wasm.lisp
;;;; Contract tests for ANSI CL character functions Wasm output (001-ansi-char-functions)
;;;;
;;;; Phase 16A: Character Functions
;;;; Validates that generated Wasm bytecode is structurally correct
;;;; and passes wasm-tools validate
;;;;
;;;; ANSI CL References (Constitution Principle IX):
;;;; - graphic-char-p: resources/HyperSpec/Body/f_graphi.htm
;;;; - standard-char-p: resources/HyperSpec/Body/f_std_ch.htm
;;;; - both-case-p: resources/HyperSpec/Body/f_upper_.htm
;;;; - char-name: resources/HyperSpec/Body/f_char_n.htm
;;;; - name-char: resources/HyperSpec/Body/f_name_c.htm
;;;; - digit-char: resources/HyperSpec/Body/f_digit_.htm
;;;; - char-int: resources/HyperSpec/Body/f_char_i.htm

(in-package #:clysm/tests/contract/character-wasm)

;;; Helper to compile expression and validate the resulting Wasm bytes
(defun compile-and-validate (expr)
  "Compile expression to Wasm and validate the output.
   Returns T if valid, NIL otherwise."
  (validate-wasm-silent (clysm/compiler:compile-to-wasm expr)))

;;; ============================================================
;;; T036: Wasm Output Validation Tests
;;; ============================================================

;;; --- Character Classification Predicates (US1) ---

(deftest test-graphic-char-p-wasm-valid
  "graphic-char-p compiles to valid Wasm"
  (ok (compile-and-validate '(graphic-char-p #\A))
      "graphic-char-p should produce valid Wasm"))

(deftest test-standard-char-p-wasm-valid
  "standard-char-p compiles to valid Wasm"
  (ok (compile-and-validate '(standard-char-p #\A))
      "standard-char-p should produce valid Wasm"))

(deftest test-both-case-p-wasm-valid
  "both-case-p compiles to valid Wasm"
  (ok (compile-and-validate '(both-case-p #\A))
      "both-case-p should produce valid Wasm"))

;;; --- Character Name Conversion (US2) ---

(deftest test-char-name-wasm-valid
  "char-name compiles to valid Wasm"
  (ok (compile-and-validate '(char-name #\Space))
      "char-name should produce valid Wasm"))

(deftest test-name-char-wasm-valid
  "name-char compiles to valid Wasm"
  (ok (compile-and-validate '(name-char "Space"))
      "name-char should produce valid Wasm"))

;;; --- Digit-Character Conversion (US3) ---

(deftest test-digit-char-wasm-valid
  "digit-char compiles to valid Wasm with default radix"
  (ok (compile-and-validate '(digit-char 5))
      "digit-char should produce valid Wasm"))

(deftest test-digit-char-with-radix-wasm-valid
  "digit-char compiles to valid Wasm with explicit radix"
  (ok (compile-and-validate '(digit-char 10 16))
      "digit-char with radix should produce valid Wasm"))

;;; --- Character Integer Conversion (US4) ---

(deftest test-char-int-wasm-valid
  "char-int compiles to valid Wasm"
  (ok (compile-and-validate '(char-int #\A))
      "char-int should produce valid Wasm"))

;;; ============================================================
;;; Combined Expression Tests
;;; ============================================================

(deftest test-char-name-name-char-roundtrip-wasm-valid
  "char-name/name-char roundtrip compiles to valid Wasm"
  (ok (compile-and-validate '(name-char (char-name #\Space)))
      "char-name/name-char roundtrip should produce valid Wasm"))

(deftest test-digit-char-roundtrip-wasm-valid
  "digit-char/digit-char-p roundtrip compiles to valid Wasm"
  (ok (compile-and-validate '(digit-char (digit-char-p #\5)))
      "digit-char/digit-char-p roundtrip should produce valid Wasm"))

(deftest test-nested-char-predicates-wasm-valid
  "Nested character predicates compile to valid Wasm"
  (ok (compile-and-validate '(if (graphic-char-p #\A)
                                 (both-case-p #\A)
                                 nil))
      "Nested character predicates should produce valid Wasm"))
