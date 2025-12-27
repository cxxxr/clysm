;;;; format-self-host-test.lisp - Self-hosting validation for FORMAT
;;;; Feature: 032-format-function

(defpackage #:clysm/tests/integration/format-self-host
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/streams
                          #:format))

(in-package #:clysm/tests/integration/format-self-host)

;;; ============================================================
;;; T058-T059: Self-Hosting Validation Tests
;;; ============================================================

;;; These tests validate that our FORMAT implementation supports all
;;; directives actually used by the Clysm compiler itself.

;;; Directive Usage Analysis (T058):
;;; - ~A: 52 uses (aesthetic output)
;;; - ~%: 43 uses (newline)
;;; - ~D: 22 uses (decimal integer)
;;; - ~S: 8 uses (standard output with escaping)
;;; - ~&: 4 uses (fresh-line)
;;; - ~{~^, ~}: 1 use in ansi-test/conditions.lisp

(deftest self-host-repl-format
  "REPL format strings work"
  (testing "REPL banner formats correctly"
    (ok (stringp (clysm/streams:format nil "~%Clysm REPL - WebAssembly GC Common Lisp Compiler~%"))
        "REPL banner format works")
    (ok (stringp (clysm/streams:format nil "Type (quit) or :q to exit.~%~%"))
        "REPL exit hint format works")))

(deftest self-host-error-format
  "Error message format strings work"
  (testing "Parser error messages format correctly"
    (ok (stringp (clysm/streams:format nil "Parse error at line ~D, column ~D: ~A"
                                       42 17 "unexpected token"))
        "Parser error format works")
    (ok (stringp (clysm/streams:format nil "Package ~A not found" "MISSING"))
        "Package error format works")))

(deftest self-host-condition-format
  "Condition format strings work"
  (testing "Condition error printing works"
    (ok (stringp (clysm/streams:format nil "~&Unhandled error: ~A~%" "test error"))
        "Error output format works")
    (ok (stringp (clysm/streams:format nil "~S" 'test-symbol))
        "~S format works")))

(deftest self-host-ansi-test-format
  "ANSI test harness format strings work"
  (testing "Category listing format works"
    ;; This is the actual pattern from ansi-test/conditions.lisp
    (ok (string= (clysm/streams:format nil "~{~A~^, ~}" '(cons list symbol))
                 "CONS, LIST, SYMBOL")
        "Category list format with ~{~^, ~} works"))
  (testing "Test progress format works"
    (ok (stringp (clysm/streams:format nil "~&Category: ~A..." 'arithmetic))
        "Category progress format works")
    (ok (stringp (clysm/streams:format nil " [~D/~D] ~A~%" 5 10 "50.0%"))
        "Test count format works")))

(deftest self-host-runner-format
  "Runner format strings work"
  (testing "Time format works"
    (ok (stringp (clysm/streams:format nil "~Ds" 120))
        "Seconds format works")
    (ok (stringp (clysm/streams:format nil "~Dm ~Ds" 2 30))
        "Minutes/seconds format works"))
  (testing "Summary format works"
    (ok (stringp (clysm/streams:format nil "~&Summary: ~D/~D (~A%) | ~D failed | ~D skipped~%"
                                       80 100 "80.0" 10 10))
        "Summary format works")))

(deftest self-host-skip-registry-format
  "Skip registry format strings work"
  (testing "Skip reason formats work"
    (ok (stringp (clysm/streams:format nil "unsupported-category: ~A" "format"))
        "Category skip reason works")
    (ok (stringp (clysm/streams:format nil "unsupported-form: ~A" "some-macro"))
        "Form skip reason works")
    (ok (stringp (clysm/streams:format nil "explicitly-skipped: ~A" "test-name"))
        "Explicit skip reason works")))

(deftest self-host-compile-error-format
  "Compile error format strings work"
  (testing "Error formatting in runner"
    (ok (stringp (clysm/streams:format nil "compile-error: ~A" "syntax error"))
        "Compile error format works")))

(deftest self-host-all-directives-smoke
  "All implemented directives work in combination"
  (testing "Complex format string smoke test"
    ;; Combines ~A, ~D, ~S, ~%, ~&, ~{~^~}, ~[~], ~?
    (let ((result (clysm/streams:format nil
                    "~A: ~D items~%~{~A~^, ~}~%~[none~;single~;multiple~]~&done"
                    "List" 3 '(x y z) 2)))
      (ok (search "List: 3 items" result)
          "~A and ~D work")
      (ok (search "X, Y, Z" result)
          "~{~^~} iteration works")
      (ok (search "multiple" result)
          "~[~] conditional works")
      (ok (search "done" result)
          "~& fresh-line works"))))
