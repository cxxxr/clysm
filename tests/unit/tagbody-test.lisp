;;;; tagbody-test.lisp - Unit tests for tagbody/go strategy analysis
;;;; TDD: These tests are written first and must fail before implementation.

(in-package #:clysm/tests/unit/tagbody)

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(defun make-test-segments (specs)
  "Create test segment structures from specs.
   Each spec is (tag form1 form2 ...) or (nil form1 form2 ...)."
  (loop for spec in specs
        collect (cons (first spec) (rest spec))))

;;; ============================================================
;;; T005: Tests for collect-go-targets
;;; ============================================================

(deftest test-collect-go-targets-no-go
  "collect-go-targets returns empty list when no go forms exist."
  (let ((segments (make-test-segments
                   '((nil (setq x 1))
                     (A (setq x 2))
                     (B (setq x 3))))))
    (ok (null (clysm/compiler/codegen/func-section::collect-go-targets segments))
        "No go forms should return empty list")))

(deftest test-collect-go-targets-single-go
  "collect-go-targets finds single go target."
  (let ((segments (make-test-segments
                   `((LOOP ,(clysm/compiler/ast:make-ast-go :tag 'LOOP))))))
    (ok (equal '(LOOP)
               (clysm/compiler/codegen/func-section::collect-go-targets segments))
        "Should find single go target")))

(deftest test-collect-go-targets-multiple-goes
  "collect-go-targets finds all go targets."
  (let ((segments (make-test-segments
                   `((A ,(clysm/compiler/ast:make-ast-go :tag 'B))
                     (B ,(clysm/compiler/ast:make-ast-go :tag 'A)
                        ,(clysm/compiler/ast:make-ast-go :tag 'C))
                     (C (setq x 1))))))
    (let ((targets (clysm/compiler/codegen/func-section::collect-go-targets segments)))
      (ok (= 3 (length targets))
          "Should find 3 go targets")
      (ok (member 'A targets)
          "Should find target A")
      (ok (member 'B targets)
          "Should find target B")
      (ok (member 'C targets)
          "Should find target C"))))

(deftest test-collect-go-targets-nested-if
  "collect-go-targets finds go inside if form."
  (let ((segments (make-test-segments
                   `((LOOP ,(clysm/compiler/ast:make-ast-if
                             :test (clysm/compiler/ast:make-ast-literal :value t :literal-type :t)
                             :then (clysm/compiler/ast:make-ast-go :tag 'LOOP)
                             :else (clysm/compiler/ast:make-ast-literal :value nil :literal-type :nil)))))))
    (ok (equal '(LOOP)
               (clysm/compiler/codegen/func-section::collect-go-targets segments))
        "Should find go inside if")))

;;; ============================================================
;;; T006: Tests for all-goes-are-backward-p
;;; ============================================================

(deftest test-all-goes-backward-simple-loop
  "all-goes-are-backward-p returns T for backward-only jumps."
  (let ((segments (make-test-segments
                   `((LOOP (setq x 1)
                           ,(clysm/compiler/ast:make-ast-go :tag 'LOOP))))))
    (ok (clysm/compiler/codegen/func-section::all-goes-are-backward-p segments 'LOOP)
        "Go at end jumping to LOOP at start should be backward")))

(deftest test-all-goes-backward-with-forward
  "all-goes-are-backward-p returns NIL when forward jump exists."
  (let ((segments (make-test-segments
                   `((nil ,(clysm/compiler/ast:make-ast-go :tag 'END))
                     (END (setq x 1))))))
    (ok (not (clysm/compiler/codegen/func-section::all-goes-are-backward-p segments 'END))
        "Go to later tag should be forward")))

(deftest test-all-goes-backward-multiple-backward
  "all-goes-are-backward-p returns T for multiple backward jumps to same tag."
  (let ((segments (make-test-segments
                   `((LOOP (setq x 1)
                           ,(clysm/compiler/ast:make-ast-if
                             :test (clysm/compiler/ast:make-ast-literal :value t :literal-type :t)
                             :then (clysm/compiler/ast:make-ast-go :tag 'LOOP)
                             :else (clysm/compiler/ast:make-ast-literal :value nil :literal-type :nil))
                           ,(clysm/compiler/ast:make-ast-go :tag 'LOOP))))))
    (ok (clysm/compiler/codegen/func-section::all-goes-are-backward-p segments 'LOOP)
        "Multiple backward jumps should still be backward")))

;;; ============================================================
;;; T007: Tests for analyze-tagbody-strategy
;;; ============================================================

(deftest test-analyze-strategy-sequential
  "analyze-tagbody-strategy returns :sequential when no go exists."
  (let ((segments (make-test-segments
                   '((nil (setq x 1))
                     (A (setq x 2))
                     (B (setq x 3))))))
    (ok (eq :sequential
            (clysm/compiler/codegen/func-section::analyze-tagbody-strategy segments))
        "Tagbody without go should be :sequential")))

(deftest test-analyze-strategy-simple-loop
  "analyze-tagbody-strategy returns :simple-loop for single tag with backward go."
  (let ((segments (make-test-segments
                   `((LOOP (setq x 1)
                           ,(clysm/compiler/ast:make-ast-go :tag 'LOOP))))))
    (ok (eq :simple-loop
            (clysm/compiler/codegen/func-section::analyze-tagbody-strategy segments))
        "Single tag with backward go should be :simple-loop")))

(deftest test-analyze-strategy-simple-loop-with-preamble
  "analyze-tagbody-strategy returns :simple-loop for loop with preamble segment."
  (let ((segments (make-test-segments
                   `((nil (setq x 0))
                     (LOOP (setq x (+ x 1))
                           ,(clysm/compiler/ast:make-ast-go :tag 'LOOP))))))
    (ok (eq :simple-loop
            (clysm/compiler/codegen/func-section::analyze-tagbody-strategy segments))
        "Preamble + single tag loop should be :simple-loop")))

(deftest test-analyze-strategy-dispatch-forward-jump
  "analyze-tagbody-strategy returns :dispatch for forward jump."
  (let ((segments (make-test-segments
                   `((nil ,(clysm/compiler/ast:make-ast-go :tag 'END))
                     (END (setq x 1))))))
    (ok (eq :dispatch
            (clysm/compiler/codegen/func-section::analyze-tagbody-strategy segments))
        "Forward jump should require :dispatch")))

(deftest test-analyze-strategy-dispatch-multiple-tags
  "analyze-tagbody-strategy returns :dispatch for multiple tags with jumps."
  (let ((segments (make-test-segments
                   `((A ,(clysm/compiler/ast:make-ast-go :tag 'B))
                     (B ,(clysm/compiler/ast:make-ast-go :tag 'A))))))
    (ok (eq :dispatch
            (clysm/compiler/codegen/func-section::analyze-tagbody-strategy segments))
        "Multiple tags with jumps should require :dispatch")))

(deftest test-analyze-strategy-dispatch-mixed-jumps
  "analyze-tagbody-strategy returns :dispatch for mixed forward/backward jumps."
  (let ((segments (make-test-segments
                   `((A ,(clysm/compiler/ast:make-ast-go :tag 'B))
                     (B ,(clysm/compiler/ast:make-ast-go :tag 'A)
                        ,(clysm/compiler/ast:make-ast-go :tag 'C))
                     (C (setq x 1))))))
    (ok (eq :dispatch
            (clysm/compiler/codegen/func-section::analyze-tagbody-strategy segments))
        "Mixed forward/backward jumps should require :dispatch")))
