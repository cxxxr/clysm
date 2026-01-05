;;;; tests/unit/repl-test.lisp - REPL Tests
;;;;
;;;; Tests for the REPL printer, host evaluation, and JS loader.

(in-package #:clysm/tests)

;;; ============================================================
;;; Printer Tests
;;; ============================================================

(defsuite printer-suite "Printer tests")

(deftest test-print-nil ()
  "Test printing NIL"
  (is-equal "NIL" (clysm:clysm-prin1-to-string nil)))

(deftest test-print-t ()
  "Test printing T"
  (is-equal "T" (clysm:clysm-prin1-to-string t)))

(deftest test-print-integers ()
  "Test printing integers"
  (is-equal "42" (clysm:clysm-prin1-to-string 42))
  (is-equal "-123" (clysm:clysm-prin1-to-string -123))
  (is-equal "0" (clysm:clysm-prin1-to-string 0)))

(deftest test-print-floats ()
  "Test printing floats"
  (is (string= "3.14" (subseq (clysm:clysm-prin1-to-string 3.14) 0 4))))

(deftest test-print-strings ()
  "Test printing strings"
  ;; With readability
  (let ((clysm:*clysm-print-readably* t))
    (is-equal "\"hello\"" (clysm:clysm-prin1-to-string "hello")))
  ;; Without readability
  (let ((clysm:*clysm-print-readably* nil))
    (is-equal "hello" (clysm:clysm-prin1-to-string "hello"))))

(deftest test-print-string-escapes ()
  "Test printing strings with escapes"
  (let ((clysm:*clysm-print-readably* t))
    (is-equal "\"line1\\nline2\"" (clysm:clysm-prin1-to-string "line1
line2"))))

(deftest test-print-characters ()
  "Test printing characters"
  (is-equal "#\\a" (clysm:clysm-prin1-to-string #\a))
  (is-equal "#\\Space" (clysm:clysm-prin1-to-string #\Space))
  (is-equal "#\\Newline" (clysm:clysm-prin1-to-string #\Newline)))

(deftest test-print-symbols ()
  "Test printing symbols"
  (is-equal "FOO" (clysm:clysm-prin1-to-string 'foo))
  (is-equal "BAR" (clysm:clysm-prin1-to-string 'bar)))

(deftest test-print-simple-list ()
  "Test printing simple lists"
  (is-equal "(1 2 3)" (clysm:clysm-prin1-to-string '(1 2 3))))

(deftest test-print-nested-list ()
  "Test printing nested lists"
  (is-equal "((1 2) (3 4))" (clysm:clysm-prin1-to-string '((1 2) (3 4)))))

(deftest test-print-dotted-pair ()
  "Test printing dotted pairs"
  (is-equal "(A . B)" (clysm:clysm-prin1-to-string '(a . b))))

(deftest test-print-dotted-list ()
  "Test printing dotted lists"
  (is-equal "(1 2 . 3)" (clysm:clysm-prin1-to-string '(1 2 . 3))))

(deftest test-print-empty-list ()
  "Test printing empty list"
  (is-equal "NIL" (clysm:clysm-prin1-to-string '())))

(deftest test-print-quoted ()
  "Test printing quoted forms"
  (is-equal "(QUOTE FOO)" (clysm:clysm-prin1-to-string '(quote foo))))

(deftest test-print-length ()
  "Test print-length limit"
  (let ((clysm:*clysm-print-length* 3))
    (is-equal "(1 2 3 ...)" (clysm:clysm-prin1-to-string '(1 2 3 4 5)))))

(deftest test-print-level ()
  "Test print-level limit"
  (let ((clysm:*clysm-print-level* 1))
    (is-equal "(#)" (clysm:clysm-prin1-to-string '((nested))))))

;;; ============================================================
;;; Host Evaluation Tests
;;; ============================================================

(defsuite host-eval-suite "Host evaluation tests")

(deftest test-eval-host-nil ()
  "Test evaluating nil in host"
  (is-false (clysm:clysm-eval-host nil)))

(deftest test-eval-host-t ()
  "Test evaluating t in host"
  (is-eq t (clysm:clysm-eval-host t)))

(deftest test-eval-host-integer ()
  "Test evaluating integers in host"
  (is-eq 42 (clysm:clysm-eval-host 42))
  (is-eq -10 (clysm:clysm-eval-host -10)))

(deftest test-eval-host-string ()
  "Test evaluating strings in host"
  (is-equal "hello" (clysm:clysm-eval-host "hello")))

(deftest test-eval-host-character ()
  "Test evaluating characters in host"
  (is-eq #\a (clysm:clysm-eval-host #\a)))

(deftest test-eval-host-quote ()
  "Test evaluating quoted forms in host"
  (is-eq 'foo (clysm:clysm-eval-host '(quote foo)))
  (is (equal '(1 2 3) (clysm:clysm-eval-host '(quote (1 2 3))))))

(deftest test-eval-host-arithmetic ()
  "Test evaluating arithmetic in host"
  (is-eq 5 (clysm:clysm-eval-host '(+ 2 3)))
  (is-eq 6 (clysm:clysm-eval-host '(* 2 3)))
  (is-eq -1 (clysm:clysm-eval-host '(- 2 3)))
  (is-eq 2 (clysm:clysm-eval-host '(/ 6 3))))

(deftest test-eval-host-list ()
  "Test evaluating list construction in host"
  (is (equal '(1 2 3) (clysm:clysm-eval-host '(list 1 2 3)))))

;;; ============================================================
;;; Wasmtime Result Parsing Tests
;;; ============================================================

(defsuite wasmtime-result-suite "Wasmtime result parsing tests")

(deftest test-parse-integer-result ()
  "Test parsing integer results"
  (is-eq 42 (clysm:parse-wasmtime-result "42"))
  (is-eq -10 (clysm:parse-wasmtime-result "-10"))
  (is-eq 0 (clysm:parse-wasmtime-result "0")))

(deftest test-parse-empty-result ()
  "Test parsing empty result"
  (is-false (clysm:parse-wasmtime-result ""))
  (is-false (clysm:parse-wasmtime-result "  ")))

(deftest test-parse-whitespace-result ()
  "Test parsing result with whitespace"
  (is-eq 42 (clysm:parse-wasmtime-result "  42  "))
  (is-eq 42 (clysm:parse-wasmtime-result (format nil "42~%"))))

(deftest test-parse-ref-null ()
  "Test parsing ref.null result"
  (is-false (clysm:parse-wasmtime-result "ref.null")))

;;; ============================================================
;;; JavaScript Loader Tests
;;; ============================================================

(defsuite js-loader-suite "JavaScript loader tests")

(deftest test-generate-js-loader ()
  "Test generating JavaScript loader"
  (let ((js (clysm:generate-js-loader)))
    (is (stringp js))
    (is (> (length js) 100))
    (is (search "Clysm" js))))

(deftest test-bundle-wasm-esm ()
  "Test bundling Wasm as ES module"
  (let ((bundle (clysm:bundle-wasm-module '(0 97 115 109) :format :esm)))
    (is (stringp bundle))
    (is (search "export default" bundle))))

(deftest test-bundle-wasm-cjs ()
  "Test bundling Wasm as CommonJS module"
  (let ((bundle (clysm:bundle-wasm-module '(0 97 115 109) :format :cjs)))
    (is (stringp bundle))
    (is (search "module.exports" bundle))))

;;; ============================================================
;;; Wasm Evaluation Tests (End-to-End)
;;; ============================================================

;;; NOTE: The Wasm runtime now returns the length of the prin1-to-string
;;; representation of the result. This allows the host to know the string
;;; size, though the actual string content is not yet readable from CLI.
;;; Future versions will support reading the string from Wasm memory.

(defsuite wasm-eval-suite "Wasm evaluation end-to-end tests")

(defun eval-via-wasm (expr)
  "Helper: read EXPR, compile to Wasm, run, and return result.
Returns the string length of the printed result."
  (let* ((form (clysm:clysm-read-from-string expr))
         (wasm (clysm:compile-to-wasm (list form))))
    (clysm:run-wasm-module wasm)))

(deftest test-wasm-eval-addition ()
  "Test addition via Wasm - returns length of \"3\""
  (is-eq 1 (eval-via-wasm "(+ 1 2)")))

(deftest test-wasm-eval-multiplication ()
  "Test multiplication via Wasm - returns length of \"12\""
  (is-eq 2 (eval-via-wasm "(* 3 4)")))

(deftest test-wasm-eval-subtraction ()
  "Test subtraction via Wasm - returns length of \"7\""
  (is-eq 1 (eval-via-wasm "(- 10 3)")))

(deftest test-wasm-eval-nested-arithmetic ()
  "Test nested arithmetic via Wasm - returns length of \"26\""
  (is-eq 2 (eval-via-wasm "(+ (* 2 3) (* 4 5))")))

(deftest test-wasm-eval-less-than ()
  "Test less-than comparison via Wasm - returns length of \"T\" (1) if true"
  (is (eval-via-wasm "(< 1 2)")))

(deftest test-wasm-eval-greater-than ()
  "Test greater-than comparison via Wasm - returns length of \"T\" (1) if true"
  (is (eval-via-wasm "(> 5 3)")))

(deftest test-wasm-eval-equal ()
  "Test numeric equality via Wasm - returns length of \"T\" (1) if true"
  (is (eval-via-wasm "(= 42 42)")))

;;; ============================================================
;;; Wasm Print Tests (String Length)
;;; ============================================================

(defsuite wasm-print-suite "Wasm print length tests")

(deftest test-wasm-print-fixnum ()
  "Test printing fixnum - \"42\" has length 2"
  (is-eq 2 (eval-via-wasm "42")))

(deftest test-wasm-print-nil ()
  "Test printing NIL - \"NIL\" has length 3"
  (is-eq 3 (eval-via-wasm "nil")))

(deftest test-wasm-print-cons ()
  "Test printing cons cell - \"(1 . 2)\" has length 7"
  (is-eq 7 (eval-via-wasm "(cons 1 2)")))

(deftest test-wasm-print-negative ()
  "Test printing negative number - \"-42\" has length 3"
  (is-eq 3 (eval-via-wasm "(- 0 42)")))

(deftest test-wasm-print-symbol ()
  "Test printing symbol - \"FOO\" has length 3"
  (is-eq 3 (eval-via-wasm "'foo")))

(deftest test-wasm-print-symbol-long ()
  "Test printing longer symbol - \"HELLO-WORLD\" has length 11"
  (is-eq 11 (eval-via-wasm "'hello-world")))

(deftest test-wasm-print-symbol-in-list ()
  "Test printing list with symbol - \"(A B C)\" has length 7"
  (is-eq 7 (eval-via-wasm "'(a b c)")))
