;;;; sbcl-free-test.lisp - Integration tests for SBCL-free workflow
;;;;
;;;; Part of Feature 044: Interpreter Bootstrap Strategy
;;;; Phase 7: T109 - SBCL-free workflow tests

(defpackage #:clysm/tests/sbcl-free-test
  (:use #:cl #:rove))

(in-package #:clysm/tests/sbcl-free-test)

;;; ============================================================
;;; Test: Wasmtime Availability
;;; ============================================================

(deftest test-wasmtime-available
  "Verify wasmtime is available for SBCL-free workflow"
  (ok (clysm/stage1:wasmtime-available-p)
      "wasmtime is available"))

;;; ============================================================
;;; Test: Interpreter Can Generate Stage 0
;;; ============================================================

(deftest test-interpreter-generates-stage0
  "Verify interpreter can generate Stage 0 without SBCL compilation"
  (let ((result (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
                 :module-limit 3 :verbose nil)))
    (ok (clysm/interpreter-bootstrap:bootstrap-result-success result)
        "Stage 0 generation succeeds")
    (ok (> (clysm/interpreter-bootstrap:bootstrap-result-modules-loaded result) 0)
        "At least one module loaded")
    (ok (> (clysm/interpreter-bootstrap:bootstrap-result-forms-compiled result) 0)
        "At least one form compiled")))

;;; ============================================================
;;; Test: Stage 0 Binary Validity
;;; ============================================================

(deftest test-stage0-wasm-validity
  "Verify Stage 0 binary passes wasm-tools validation"
  (let* ((result (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
                  :module-limit 3 :verbose nil))
         (bytes (clysm/interpreter-bootstrap:bootstrap-result-wasm-bytes result)))
    (when bytes
      (multiple-value-bind (valid-p err)
          (clysm/interpreter-bootstrap:validate-stage0-binary bytes)
        (ok valid-p (format nil "wasm-tools validates~@[ (error: ~A)~]" err))))))

;;; ============================================================
;;; Test: Interpreter Environment Creation
;;; ============================================================

(deftest test-interpreter-env-creation
  "Verify interpreter environment can be created without errors"
  (let ((env (clysm/eval/interpreter:make-interpreter-env)))
    (ok env "Environment created")
    (ok (hash-table-p (clysm/eval/interpreter::interpreter-env-bindings env))
        "Has bindings hash-table")))

;;; ============================================================
;;; Test: Interpreter Basic Evaluation
;;; ============================================================

(deftest test-interpreter-basic-evaluation
  "Verify interpreter can evaluate basic expressions"
  (let ((env (clysm/eval/interpreter:make-interpreter-env)))
    ;; Simple arithmetic
    (ok (= 3 (clysm/eval/interpreter:interpret '(+ 1 2) :env env))
        "Arithmetic works")
    ;; Function definition and call
    (clysm/eval/interpreter:interpret '(defun test-add (a b) (+ a b)) :env env)
    (ok (= 5 (clysm/eval/interpreter:interpret '(test-add 2 3) :env env))
        "Function definition and call works")
    ;; Let binding
    (ok (= 10 (clysm/eval/interpreter:interpret '(let ((x 5)) (* x 2)) :env env))
        "Let binding works")))

;;; ============================================================
;;; Test: Interpreter Built-in Functions
;;; ============================================================

(deftest test-interpreter-builtins
  "Verify interpreter has essential built-in functions"
  (let ((env (clysm/eval/interpreter:make-interpreter-env)))
    ;; List operations
    (ok (equal '(1 2 3)
               (clysm/eval/interpreter:interpret '(list 1 2 3) :env env))
        "list works")
    (ok (= 1 (clysm/eval/interpreter:interpret '(car '(1 2 3)) :env env))
        "car works")
    (ok (equal '(2 3)
               (clysm/eval/interpreter:interpret '(cdr '(1 2 3)) :env env))
        "cdr works")
    ;; Predicates
    (ok (clysm/eval/interpreter:interpret '(listp '(1 2)) :env env)
        "listp works")
    (ok (clysm/eval/interpreter:interpret '(numberp 42) :env env)
        "numberp works")))

;;; ============================================================
;;; Test: Interpreter Macro Expansion
;;; ============================================================

(deftest test-interpreter-macros
  "Verify interpreter supports macro definition and expansion"
  (let ((env (clysm/eval/interpreter:make-interpreter-env)))
    ;; Define a simple macro
    (clysm/eval/interpreter:interpret
     '(defmacro my-when (test &body body)
        `(if ,test (progn ,@body)))
     :env env)
    ;; Use the macro
    (ok (= 42
           (clysm/eval/interpreter:interpret
            '(let ((x nil))
               (my-when t (setq x 42))
               x)
            :env env))
        "Custom macro works")))

;;; ============================================================
;;; Test: Progress Callback Support
;;; ============================================================

(deftest test-progress-callback-support
  "Verify progress callbacks work for SBCL-free monitoring"
  (let ((phases nil))
    (let ((clysm/interpreter-bootstrap:*bootstrap-progress-callback*
            (lambda (phase module-count form-count)
              (declare (ignore module-count form-count))
              (push phase phases))))
      (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
       :module-limit 1 :verbose nil)
      (ok (member :start phases) ":start phase recorded")
      (ok (member :complete phases) ":complete phase recorded"))))

;;; ============================================================
;;; Test: Error Handling in SBCL-Free Mode
;;; ============================================================

(deftest test-sbcl-free-error-handling
  "Verify error handling works in SBCL-free workflow"
  (let ((result (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
                 :module-limit 5 :verbose nil)))
    ;; Even with some compilation errors, should complete gracefully
    (ok (clysm/interpreter-bootstrap:bootstrap-result-p result)
        "Returns valid result even with errors")
    (ok (listp (clysm/interpreter-bootstrap:bootstrap-result-errors result))
        "Errors are collected as list")))

;;; ============================================================
;;; Test: Fixpoint Status Conversion
;;; ============================================================

(deftest test-fixpoint-exit-codes
  "Verify fixpoint exit codes work for SBCL-free scripts"
  (let ((result (clysm/interpreter-bootstrap:make-fixpoint-result
                 :status :achieved)))
    (ok (= 0 (clysm/interpreter-bootstrap:fixpoint-exit-code result))
        "achieved -> 0"))
  (let ((result (clysm/interpreter-bootstrap:make-fixpoint-result
                 :status :not-achieved)))
    (ok (= 1 (clysm/interpreter-bootstrap:fixpoint-exit-code result))
        "not-achieved -> 1"))
  (let ((result (clysm/interpreter-bootstrap:make-fixpoint-result
                 :status :compilation-error)))
    (ok (= 2 (clysm/interpreter-bootstrap:fixpoint-exit-code result))
        "compilation-error -> 2"))
  (let ((result (clysm/interpreter-bootstrap:make-fixpoint-result
                 :status :missing-dependency)))
    (ok (= 3 (clysm/interpreter-bootstrap:fixpoint-exit-code result))
        "missing-dependency -> 3")))
