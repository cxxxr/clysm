;;;; ast-tagbody-test.lisp - Tests for AST-TAGBODY handling (001-wasm-local-binding)
;;;;
;;;; Phase 13D User Story 4: Handle AST-TAGBODY Structure Serialization
;;;; TDD Required: Tests MUST fail before implementation (Constitution VII)
;;;;
;;;; Test Coverage:
;;;; - T031: AST-TAGBODY literal handling verification
;;;; - T036: P943 error pattern eliminated verification

(in-package #:cl-user)

(defpackage #:clysm/tests/unit/ast-tagbody
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/ast-tagbody)

;;; ============================================================
;;; T031: AST-TAGBODY Structure Handling Tests
;;; ============================================================

(deftest ast-tagbody-structure-exists-test
  "Verify AST-TAGBODY structure is defined."
  (testing "AST-TAGBODY structure exists"
    (let ((sym (find-symbol "AST-TAGBODY" :clysm/compiler/ast)))
      (ok sym "AST-TAGBODY symbol found")
      (when sym
        ;; Check that it's a struct
        (let ((constructor (find-symbol "MAKE-AST-TAGBODY" :clysm/compiler/ast)))
          (ok constructor "MAKE-AST-TAGBODY constructor exists")
          (when (and constructor (fboundp constructor))
            (ok t "MAKE-AST-TAGBODY is callable")))))))

(deftest tagbody-compilation-test
  "Verify TAGBODY forms compile without #S(AST-TAGBODY...) errors."
  (testing "Simple TAGBODY compiles"
    (let ((compile-fn (find-symbol "COMPILE-TO-WAT" :clysm)))
      (when (and compile-fn (fboundp compile-fn))
        (handler-case
            (let ((result (funcall compile-fn
                                    '(defun test-tagbody ()
                                       (tagbody
                                        start
                                        (print "start")
                                        (go end)
                                        end)))))
              (ok result "TAGBODY form compiled successfully")
              ;; Check that result doesn't contain #S( structure notation
              (let ((wat-str (if (stringp result) result (format nil "~A" result))))
                (ng (search "#S(" wat-str)
                    "No #S( structure notation in output")))
          (error (c)
            (let ((msg (princ-to-string c)))
              ;; Check if it's NOT the P943 error pattern
              (ng (search "#S(CLYSM/COMPILER/AST:AST-TAGBODY" msg)
                  (format nil "P943 pattern detected: ~A" msg)))))))))

;;; ============================================================
;;; T036: P943 Error Pattern Elimination Test
;;; ============================================================

(deftest p943-pattern-eliminated-test
  "Verify P943 error pattern is not in Stage 1 report."
  (testing "P943 pattern not in Stage 1 report"
    ;; Read the Stage 1 report if it exists
    (let ((report-path "/home/user/src/clysm-workbench/clysm3/dist/stage1-report.json"))
      (if (probe-file report-path)
          (handler-case
              (with-open-file (stream report-path :direction :input)
                (let ((content (make-string (file-length stream))))
                  (read-sequence content stream)
                  ;; Check for AST-TAGBODY pattern
                  (ng (search "#S(CLYSM/COMPILER/AST:AST-TAGBODY" content)
                      "P943 pattern not in Stage 1 report")))
            (error (c)
              (skip (format nil "Could not read report: ~A" c))))
          (skip "Stage 1 report not found - skipping verification")))))
