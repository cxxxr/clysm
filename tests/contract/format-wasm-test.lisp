;;;; format-wasm-test.lisp - Contract tests for format directive parsing
;;;; Feature: 032-format-function

(defpackage #:clysm/tests/contract/format-wasm
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/streams
                          #:format))

(in-package #:clysm/tests/contract/format-wasm)

;;; ============================================================
;;; T054: Format Parsing Contract Tests
;;; ============================================================

(deftest format-parse-simple-directives
  "Test that simple directives parse to valid structures"
  (let ((info (clysm/streams::parse-format-string "~A ~S ~D ~% ~~")))
    (ok (clysm/streams::format-string-info-p info)
        "Returns format-string-info struct")
    (ok (= 5 (length (clysm/streams::format-string-info-directives info)))
        "Parses all 5 directives")))

(deftest format-parse-fresh-line-directive
  "Test ~& fresh-line directive parsing"
  (let* ((info (clysm/streams::parse-format-string "x~&y"))
         (dirs (clysm/streams::format-string-info-directives info)))
    (ok (= 1 (length dirs))
        "Single directive parsed")
    (ok (eq :fresh-line (clysm/streams::format-directive-type (first dirs)))
        "Directive type is :fresh-line")))

(deftest format-parse-iteration-directive
  "Test ~{~} iteration directive parsing"
  (let* ((info (clysm/streams::parse-format-string "~{~A~^, ~}"))
         (dirs (clysm/streams::format-string-info-directives info)))
    (ok (= 1 (length dirs))
        "Single top-level directive")
    (let ((iter-dir (first dirs)))
      (ok (clysm/streams::iteration-directive-p iter-dir)
          "Is iteration-directive struct")
      (ok (eq :iteration (clysm/streams::format-directive-type iter-dir))
          "Directive type is :iteration")
      (ok (= 2 (length (clysm/streams::iteration-directive-body-directives iter-dir)))
          "Body has 2 directives: ~A and ~^"))))

(deftest format-parse-conditional-directive
  "Test ~[~] conditional directive parsing"
  (let* ((info (clysm/streams::parse-format-string "~[zero~;one~;two~]"))
         (dirs (clysm/streams::format-string-info-directives info)))
    (ok (= 1 (length dirs))
        "Single top-level directive")
    (let ((cond-dir (first dirs)))
      (ok (clysm/streams::conditional-directive-p cond-dir)
          "Is conditional-directive struct")
      (ok (eq :conditional (clysm/streams::format-directive-type cond-dir))
          "Directive type is :conditional")
      (ok (= 3 (length (clysm/streams::conditional-directive-clauses cond-dir)))
          "Has 3 clauses"))))

(deftest format-parse-boolean-conditional
  "Test ~:[~] boolean conditional parsing"
  (let* ((info (clysm/streams::parse-format-string "~:[no~;yes~]"))
         (dirs (clysm/streams::format-string-info-directives info)))
    (ok (= 1 (length dirs))
        "Single top-level directive")
    (let ((cond-dir (first dirs)))
      (ok (clysm/streams::conditional-directive-p cond-dir)
          "Is conditional-directive struct")
      (ok (clysm/streams::conditional-directive-boolean-p cond-dir)
          "Boolean-p is true"))))

(deftest format-parse-recursive-directive
  "Test ~? recursive directive parsing"
  (let* ((info (clysm/streams::parse-format-string "prefix: ~?"))
         (dirs (clysm/streams::format-string-info-directives info)))
    (ok (= 1 (length dirs))
        "Single directive")
    (let ((rec-dir (first dirs)))
      (ok (clysm/streams::recursive-directive-p rec-dir)
          "Is recursive-directive struct")
      (ok (eq :recursive (clysm/streams::format-directive-type rec-dir))
          "Directive type is :recursive"))))

(deftest format-parse-nested-structures
  "Test nested iteration/conditional parsing"
  ;; ~{~[A~;B~]~}
  (let* ((info (clysm/streams::parse-format-string "~{~[A~;B~]~}"))
         (dirs (clysm/streams::format-string-info-directives info)))
    (ok (= 1 (length dirs))
        "Single top-level directive")
    (let* ((iter-dir (first dirs))
           (body-dirs (clysm/streams::iteration-directive-body-directives iter-dir)))
      (ok (= 1 (length body-dirs))
          "Iteration body has 1 directive")
      (ok (clysm/streams::conditional-directive-p (first body-dirs))
          "Nested directive is conditional"))))

(deftest format-directive-positions
  "Test that directive start/end positions are correct"
  (let* ((info (clysm/streams::parse-format-string "abc~Axyz"))
         (dirs (clysm/streams::format-string-info-directives info)))
    (ok (= 1 (length dirs))
        "Single directive")
    (let ((dir (first dirs)))
      (ok (= 3 (clysm/streams::format-directive-start dir))
          "Directive starts at position 3")
      (ok (= 5 (clysm/streams::format-directive-end dir))
          "Directive ends at position 5"))))

;;; ============================================================
;;; Format Execution Contract Tests
;;; ============================================================

(deftest format-output-contract
  "Test format function return value contract"
  ;; destination=nil returns string
  (ok (stringp (clysm/streams:format nil "test"))
      "NIL destination returns string")
  ;; destination=t returns nil (output to *standard-output*)
  (ok (null (with-output-to-string (*standard-output*)
              (clysm/streams:format t "test")))
      "T destination returns NIL"))

(deftest format-column-tracking-contract
  "Test column tracking for fresh-line"
  ;; Reset and verify
  (setf clysm/streams::*format-column* 0)
  (clysm/streams:format nil "abc")
  (ok (= 3 clysm/streams::*format-column*)
      "Column advances for output")
  (clysm/streams:format nil "~%")
  (ok (= 0 clysm/streams::*format-column*)
      "Column resets after newline"))
