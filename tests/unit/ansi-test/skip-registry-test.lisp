;;;; skip-registry-test.lisp - Unit tests for skip detection
;;;;
;;;; T049: contains-unsupported-form-p tests
;;;; T050: detect-skip-reason tests

(in-package #:clysm/tests/unit/ansi-test)

;;; ==========================================================================
;;; T049: contains-unsupported-form-p tests
;;; ==========================================================================

(deftest contains-unsupported-form-detects-format
  "Test that contains-unsupported-form-p detects FORMAT"
  (let ((form '(format nil "~A" 42)))
    (ok (contains-unsupported-form-p form *default-skip-registry*)
        "Detects FORMAT as unsupported")))

(deftest contains-unsupported-form-detects-nested
  "Test that contains-unsupported-form-p detects nested unsupported forms"
  (let ((form '(let ((x 1)) (format nil "~A" x))))
    (ok (contains-unsupported-form-p form *default-skip-registry*)
        "Detects FORMAT nested in LET")))

(deftest contains-unsupported-form-detects-defgeneric
  "Test that contains-unsupported-form-p detects DEFGENERIC"
  (let ((form '(defgeneric foo (x))))
    (ok (contains-unsupported-form-p form *default-skip-registry*)
        "Detects DEFGENERIC as unsupported")))

(deftest contains-unsupported-form-allows-supported
  "Test that contains-unsupported-form-p allows supported forms"
  (let ((form '(+ 1 2)))
    (ok (null (contains-unsupported-form-p form *default-skip-registry*))
        "Allows simple arithmetic")))

(deftest contains-unsupported-form-allows-complex-supported
  "Test that contains-unsupported-form-p allows complex supported forms"
  ;; NOTE: LET is now in unsupported-forms (021-ansi-test-execution)
  ;; Test with simple nested forms instead
  (let ((form '(car (cons 1 2))))
    (ok (null (contains-unsupported-form-p form *default-skip-registry*))
        "Allows CONS and CAR")))

;;; ==========================================================================
;;; T050: detect-skip-reason tests
;;; ==========================================================================

(deftest detect-skip-reason-unsupported-form
  "Test that detect-skip-reason returns reason for unsupported form"
  (let* ((tc (make-test-case
              :name 'format.test
              :category "printer"
              :form '(format nil "~A" 42)
              :expected-values '("42")))
         (reason (detect-skip-reason tc)))
    (ok reason "Returns a skip reason")
    (ok (search "unsupported" reason) "Reason mentions unsupported")))

(deftest detect-skip-reason-unsupported-category
  "Test that detect-skip-reason returns reason for unsupported category"
  (let* ((tc (make-test-case
              :name 'streams.test
              :category "streams"  ;; streams is in unsupported categories
              :form '(+ 1 2)
              :expected-values '(3)))
         (reason (detect-skip-reason tc)))
    (ok reason "Returns a skip reason for unsupported category")
    (ok (search "unsupported-category" reason) "Reason mentions unsupported-category")))

(deftest detect-skip-reason-no-skip
  "Test that detect-skip-reason returns NIL for supported tests"
  (let* ((tc (make-test-case
              :name 'cons.test
              :category "cons"
              :form '(cons 1 2)
              :expected-values '((1 . 2))))
         (reason (detect-skip-reason tc)))
    (ok (null reason) "Returns NIL for supported test")))

(deftest detect-skip-reason-explicit-skip
  "Test that detect-skip-reason handles explicitly skipped tests"
  (let* ((registry (make-skip-registry
                    :unsupported-forms nil
                    :unsupported-categories nil
                    :skipped-tests '(explicit.skip.test)))
         (tc (make-test-case
              :name 'explicit.skip.test
              :category "test"
              :form '(+ 1 2)
              :expected-values '(3)))
         (reason (detect-skip-reason tc registry)))
    (ok reason "Returns skip reason for explicitly skipped test")
    (ok (search "explicitly-skipped" reason) "Reason mentions explicitly-skipped")))

;;; ==========================================================================
;;; T005-T008: 021-ansi-test-execution foundational tests
;;; ==========================================================================

(deftest contains-unsupported-form-detects-loop
  "T005: Test that contains-unsupported-form-p detects LOOP"
  (let ((form '(loop for x in '(1 2 3) collect x)))
    (ok (contains-unsupported-form-p form *default-skip-registry*)
        "Detects LOOP as unsupported")))

(deftest contains-unsupported-form-detects-let
  "T006: Test that contains-unsupported-form-p detects LET"
  (let ((form '(let ((x 1)) x)))
    (ok (contains-unsupported-form-p form *default-skip-registry*)
        "Detects LET as unsupported")))

(deftest contains-unsupported-form-detects-ansi-test-macros
  "T007: Test that contains-unsupported-form-p detects ANSI test macros"
  (ok (contains-unsupported-form-p '(eqt x y) *default-skip-registry*)
      "Detects EQT as unsupported")
  (ok (contains-unsupported-form-p '(equalt x y) *default-skip-registry*)
      "Detects EQUALT as unsupported")
  (ok (contains-unsupported-form-p '(notnot x) *default-skip-registry*)
      "Detects NOTNOT as unsupported")
  (ok (contains-unsupported-form-p '(signals-error (foo) type-error) *default-skip-registry*)
      "Detects SIGNALS-ERROR as unsupported"))

(deftest contains-unsupported-form-detects-ansi-test-globals
  "T008: Test that contains-unsupported-form-p detects ANSI test globals"
  (ok (contains-unsupported-form-p '*universe* *default-skip-registry*)
      "Detects *UNIVERSE* as unsupported")
  (ok (contains-unsupported-form-p '*numbers* *default-skip-registry*)
      "Detects *NUMBERS* as unsupported")
  (ok (contains-unsupported-form-p '(member x *symbols*) *default-skip-registry*)
      "Detects *SYMBOLS* as unsupported")
  (ok (contains-unsupported-form-p '(loop for x in *conses* collect x) *default-skip-registry*)
      "Detects *CONSES* in expression"))

(deftest detect-skip-reason-format-string
  "T044: Test that skip reason for unsupported-form includes form name"
  (let* ((tc (make-test-case
              :name 'format.test
              :category "cons"
              :form '(format nil "~A" 42)
              :expected-values '("42")))
         (reason (detect-skip-reason tc)))
    (ok reason "Returns a skip reason")
    (ok (search "unsupported-form: FORMAT" reason)
        "Reason follows format 'unsupported-form: FORMAT'")))
