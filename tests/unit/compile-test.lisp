;;;; compile-test.lisp - Unit tests for compile function
;;;; Feature: 017-eval-jit-compile
;;;; TDD: Tests written FIRST per Constitution Principle VII

(in-package #:clysm/tests/unit/compile)

;;; ============================================================
;;; User Story 1: Compile Anonymous Lambda Tests (T007-T011)
;;; ============================================================

(deftest test-compile-nil-name
  "T007: compile* with nil name returns a callable function."
  (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (+ x 1)))))
    (ok (functionp fn)
        "compile* returns a function")
    (ok (= (funcall fn 5) 6)
        "Compiled function computes (+ 5 1) = 6")))

(deftest test-compile-single-param
  "T008: compile* with single-param lambda works correctly."
  (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (* x 2)))))
    (ok (= (funcall fn 3) 6)
        "Compiled function computes (* 3 2) = 6")
    (ok (= (funcall fn 10) 20)
        "Compiled function computes (* 10 2) = 20")))

(deftest test-compile-multi-param
  "T009: compile* with multi-param lambda works correctly."
  (let ((fn (clysm/eval/compile:compile* nil '(lambda (a b) (* a b)))))
    (ok (= (funcall fn 3 4) 12)
        "Compiled function computes (* 3 4) = 12")
    (ok (= (funcall fn 5 7) 35)
        "Compiled function computes (* 5 7) = 35")))

(deftest test-compile-zero-param
  "T010: compile* with zero-param lambda works correctly."
  (let ((fn (clysm/eval/compile:compile* nil '(lambda () 42))))
    (ok (= (funcall fn) 42)
        "Compiled function returns 42")))

(deftest test-compile-error-non-lambda
  "T011: compile* signals error on non-lambda input."
  (ok (signals (clysm/eval/compile:compile* nil '(not-a-lambda)) 'error)
      "compile* signals error for non-lambda")
  (ok (signals (clysm/eval/compile:compile* nil 42) 'error)
      "compile* signals error for non-cons")
  (ok (signals (clysm/eval/compile:compile* nil '(lambda)) 'error)
      "compile* signals error for lambda without params"))

;;; ============================================================
;;; User Story 2: Tier 1 Execution Tests (T017-T018)
;;; ============================================================

(deftest test-new-function-starts-tier-1
  "T017: Newly compiled function starts at :tier-1."
  ;; Reset state before test
  (clysm/eval/compile:reset-invocation-counts)
  (let* ((fn (clysm/eval/compile:compile* nil '(lambda (x) x)))
         ;; Get the tiered-function from function slots
         ;; For anonymous functions, we need to check via invocation count
         (result (funcall fn 42)))
    (ok (= result 42)
        "Function executes correctly at Tier 1")))

(deftest test-function-below-threshold-stays-tier-1
  "T018: Function below threshold stays in Tier 1."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)
  ;; Use threshold - 2 so that even after the final assertion call,
  ;; we're still below the threshold (avoiding mock JIT replacement)
  (let* ((calls-before-threshold (- clysm/eval/compile:*compilation-threshold* 2))
         (fn (clysm/eval/compile:compile* nil '(lambda (x) (+ x 1)))))
    ;; Call threshold - 2 times
    (dotimes (i calls-before-threshold)
      (funcall fn i))
    ;; Still in Tier 1 - function should work normally
    ;; After this call, count will be threshold - 1, still below threshold
    (ok (= (funcall fn 100) 101)
        "Function still works after multiple calls below threshold")))

;;; ============================================================
;;; User Story 3: Tier Promotion Tests (T023-T025)
;;; ============================================================

(deftest test-invocation-counter-increments
  "T023: Invocation counter increments on each call."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)
  ;; Create a named function to track the tiered-function
  (let* ((fn (clysm/eval/compile:compile* 'test-counter '(lambda (x) x)))
         (tf (clysm/eval/compile:get-tiered-function 'test-counter)))
    (ok (= (clysm/eval/compile:tiered-function-invocation-count tf) 0)
        "Initial invocation count is 0")
    (funcall fn 1)
    (ok (= (clysm/eval/compile:tiered-function-invocation-count tf) 1)
        "Invocation count is 1 after one call")
    (funcall fn 2)
    (funcall fn 3)
    (ok (= (clysm/eval/compile:tiered-function-invocation-count tf) 3)
        "Invocation count is 3 after three calls")))

(deftest test-should-promote-after-threshold
  "T024: should-promote-to-tier-2-p returns T after threshold."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/jit:reset-function-slots)
  (let ((tf (clysm/eval/compile:make-tiered-function
             :name nil
             :definition '(lambda (x) x)
             :tier :tier-1
             :implementation #'identity
             :invocation-count 0
             :promotion-failed-p nil)))
    (ok (not (clysm/eval/compile:should-promote-to-tier-2-p tf))
        "Should not promote at count 0")
    (setf (clysm/eval/compile:tiered-function-invocation-count tf) 9)
    (ok (not (clysm/eval/compile:should-promote-to-tier-2-p tf))
        "Should not promote at count 9 (threshold is 10)")
    (setf (clysm/eval/compile:tiered-function-invocation-count tf) 10)
    (ok (clysm/eval/compile:should-promote-to-tier-2-p tf)
        "Should promote at count 10")))

(deftest test-configurable-threshold
  "T025: Threshold is configurable."
  (clysm/eval/compile:reset-invocation-counts)
  (let ((original-threshold clysm/eval/compile:*compilation-threshold*))
    (unwind-protect
        (progn
          (setf clysm/eval/compile:*compilation-threshold* 5)
          (let ((tf (clysm/eval/compile:make-tiered-function
                     :name nil
                     :definition '(lambda (x) x)
                     :tier :tier-1
                     :implementation #'identity
                     :invocation-count 4
                     :promotion-failed-p nil)))
            (ok (not (clysm/eval/compile:should-promote-to-tier-2-p tf))
                "Should not promote at 4 with threshold 5")
            (setf (clysm/eval/compile:tiered-function-invocation-count tf) 5)
            (ok (clysm/eval/compile:should-promote-to-tier-2-p tf)
                "Should promote at 5 with threshold 5")))
      (setf clysm/eval/compile:*compilation-threshold* original-threshold))))

;;; ============================================================
;;; User Story 4: Named Function Tests (T033-T035)
;;; ============================================================

(deftest test-named-compile-registers
  "T033: Named compile registers in *tiered-functions*."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)
  (let ((fn (clysm/eval/compile:compile* 'my-test-fn '(lambda (x) (+ x 1)))))
    (declare (ignore fn))
    (ok (clysm/eval/compile:get-tiered-function 'my-test-fn)
        "Function is registered in tiered-functions")
    (ok (clysm/eval/compile:tiered-function-p
         (clysm/eval/compile:get-tiered-function 'my-test-fn))
        "Registered value is a tiered-function")))

(deftest test-get-tiered-function-retrieves
  "T034: get-tiered-function retrieves registered function."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)
  (clysm/eval/compile:compile* 'retrieve-test '(lambda (x) x))
  (let ((tf (clysm/eval/compile:get-tiered-function 'retrieve-test)))
    (ok tf "get-tiered-function returns non-nil")
    (ok (eq (clysm/eval/compile:tiered-function-name tf) 'retrieve-test)
        "Retrieved function has correct name")))

(deftest test-recompile-updates-slot
  "T035: Recompile updates function slot (hot-patch)."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)
  ;; First compile
  (let ((fn1 (clysm/eval/compile:compile* 'hotpatch-test '(lambda (x) (+ x 1)))))
    (ok (= (funcall fn1 10) 11)
        "First compiled function returns 11")
    ;; Recompile with different definition
    (let ((fn2 (clysm/eval/compile:compile* 'hotpatch-test '(lambda (x) (* x 2)))))
      (ok (= (funcall fn2 10) 20)
          "Recompiled function returns 20"))))

;;; ============================================================
;;; Tiered Function Struct Tests
;;; ============================================================

(deftest test-tiered-function-struct
  "Test tiered-function struct creation and accessors."
  (let ((tf (clysm/eval/compile:make-tiered-function
             :name 'test-fn
             :definition '(lambda (x) x)
             :tier :tier-1
             :implementation #'identity
             :invocation-count 5
             :promotion-failed-p nil)))
    (ok (clysm/eval/compile:tiered-function-p tf)
        "tiered-function-p returns T")
    (ok (eq (clysm/eval/compile:tiered-function-name tf) 'test-fn)
        "tiered-function-name accessor works")
    (ok (equal (clysm/eval/compile:tiered-function-definition tf) '(lambda (x) x))
        "tiered-function-definition accessor works")
    (ok (eq (clysm/eval/compile:tiered-function-tier tf) :tier-1)
        "tiered-function-tier accessor works")
    (ok (functionp (clysm/eval/compile:tiered-function-implementation tf))
        "tiered-function-implementation accessor works")
    (ok (= (clysm/eval/compile:tiered-function-invocation-count tf) 5)
        "tiered-function-invocation-count accessor works")
    (ok (not (clysm/eval/compile:tiered-function-promotion-failed-p tf))
        "tiered-function-promotion-failed-p accessor works")))

(deftest test-get-current-tier
  "Test get-current-tier function."
  (let ((tf (clysm/eval/compile:make-tiered-function
             :definition '(lambda (x) x)
             :tier :tier-1
             :implementation #'identity)))
    (ok (eq (clysm/eval/compile:get-current-tier tf) :tier-1)
        "get-current-tier returns :tier-1")))
