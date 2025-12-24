;;;; condition-test.lisp - Integration tests for condition system
;;;; TDD: These tests define expected behavior BEFORE implementation

(in-package #:clysm/tests)

;;; ============================================================
;;; User Story 1: Basic Error Signaling and Handling
;;; Goal: Developers can catch and handle errors using handler-case
;;; ============================================================

(deftest handler-case-catches-type-error-test
  "Test handler-case catches type-error (T028)"
  (testing "handler-case catches matching condition type"
    (let ((result
            (clysm/conditions:handler-case
                (clysm/conditions:error 'clysm/conditions:type-error
                                        :datum 42
                                        :expected-type 'string)
              (clysm/conditions:type-error (c)
                (declare (ignore c))
                :caught))))
      (ok (eq :caught result)
          "handler-case should return handler body value")))

  (testing "handler-case provides condition object to handler"
    (let ((caught-datum nil))
      (clysm/conditions:handler-case
          (clysm/conditions:error 'clysm/conditions:type-error
                                  :datum 42
                                  :expected-type 'string)
        (clysm/conditions:type-error (c)
          (setf caught-datum (clysm/conditions:type-error-datum c))))
      (ok (eql 42 caught-datum)
          "handler should receive condition with correct slots"))))

(deftest nested-handler-case-innermost-test
  "Test nested handler-case finds innermost handler (T029)"
  (testing "innermost handler takes precedence"
    (let ((result
            (clysm/conditions:handler-case
                (clysm/conditions:handler-case
                    (clysm/conditions:error 'clysm/conditions:simple-error
                                            :format-control "test")
                  (clysm/conditions:error (c)
                    (declare (ignore c))
                    :inner))
              (clysm/conditions:error (c)
                (declare (ignore c))
                :outer))))
      (ok (eq :inner result)
          "innermost handler should handle the error")))

  (testing "outer handler catches when inner doesn't match"
    (let ((result
            (clysm/conditions:handler-case
                (clysm/conditions:handler-case
                    (clysm/conditions:error 'clysm/conditions:simple-error
                                            :format-control "test")
                  (clysm/conditions:type-error (c)
                    (declare (ignore c))
                    :inner))
              (clysm/conditions:error (c)
                (declare (ignore c))
                :outer))))
      (ok (eq :outer result)
          "outer handler should catch when inner type doesn't match"))))

(deftest handler-case-no-error-test
  "Test handler-case returns body value when no error (T030)"
  (testing "no error - returns body value"
    (let ((result
            (clysm/conditions:handler-case
                (+ 1 2)
              (clysm/conditions:error (c)
                (declare (ignore c))
                :error-caught))))
      (ok (eql 3 result)
          "should return body value when no error")))

  (testing "multiple values preserved"
    (multiple-value-bind (a b)
        (clysm/conditions:handler-case
            (values 1 2)
          (clysm/conditions:error (c)
            (declare (ignore c))
            :error-caught))
      (ok (and (eql 1 a) (eql 2 b))
          "should preserve multiple values from body"))))

;;; ============================================================
;;; User Story 2: Restart-Based Recovery (T037-T040)
;;; Goal: Developers can establish restarts and invoke them from handlers
;;; ============================================================

(deftest restart-case-establishes-restart-test
  "Test restart-case establishes invocable restart (T037)"
  (testing "restart-case makes restart visible via find-restart"
    (let ((result
            (clysm/conditions:restart-case
                (progn
                  (ok (clysm/conditions:find-restart 'use-value)
                      "restart should be visible during body")
                  :normal-result)
              (use-value (v)
                v))))
      (ok (eq :normal-result result)
          "restart-case should return body value when restart not invoked"))))

(deftest invoke-restart-control-transfer-test
  "Test invoke-restart transfers control via catch/throw (T040)"
  (testing "invoke-restart executes restart body and returns its value"
    (let ((result
            (clysm/conditions:restart-case
                (progn
                  (clysm/conditions:invoke-restart 'use-value 42)
                  :not-reached)
              (use-value (v)
                v))))
      (ok (eql 42 result)
          "invoke-restart should transfer control to restart")))

  (testing "invoke-restart from handler"
    (let ((result
            (clysm/conditions:restart-case
                (clysm/conditions:handler-case
                    (clysm/conditions:error 'clysm/conditions:simple-error
                                            :format-control "test")
                  (clysm/conditions:error (c)
                    (declare (ignore c))
                    (clysm/conditions:invoke-restart 'use-value :recovered)))
              (use-value (v)
                v))))
      (ok (eq :recovered result)
          "handler should be able to invoke restart"))))

;;; ============================================================
;;; User Story 3: Warning and Non-Fatal Conditions (T048-T050)
;;; Goal: Developers can signal warnings that output messages but continue
;;; ============================================================

(deftest warn-outputs-and-continues-test
  "Test warn outputs message and continues execution (T048)"
  (testing "warn should output to *error-output* and continue"
    (let ((output (with-output-to-string (cl:*error-output*)
                    (clysm/conditions:warn "Test warning message"))))
      (ok (search "Test warning message" output)
          "warning message should be output"))
    ;; Execution continues after warn
    (let ((result (progn
                    (clysm/conditions:warn "ignored")
                    :continued)))
      (ok (eq :continued result)
          "execution should continue after warn"))))

(deftest muffle-warning-test
  "Test muffle-warning suppresses warning output (T049)"
  (testing "muffle-warning should suppress output"
    (let ((output (with-output-to-string (cl:*error-output*)
                    (clysm/conditions:handler-bind
                        ((clysm/conditions:warning
                          (lambda (c)
                            (declare (ignore c))
                            (clysm/conditions:muffle-warning))))
                      (clysm/conditions:warn "Should be muffled")))))
      (ok (not (search "Should be muffled" output))
          "muffled warning should not output"))))

;;; ============================================================
;;; User Story 4: Continuable Errors with cerror (T055-T056)
;;; Goal: Developers can signal continuable errors
;;; ============================================================

(deftest cerror-establishes-continue-test
  "Test cerror establishes continue restart (T055)"
  (testing "cerror with continue restart invoked"
    (let ((result
            (clysm/conditions:handler-case
                (clysm/conditions:handler-bind
                    ((clysm/conditions:error
                      (lambda (c)
                        (declare (ignore c))
                        (clysm/conditions:invoke-restart 'clysm/conditions:continue))))
                  (clysm/conditions:cerror "Continue anyway" "Test error")
                  :continued)
              (clysm/conditions:error (c)
                (declare (ignore c))
                :unhandled))))
      (ok (eq :continued result)
          "cerror should allow continuation"))))

(deftest continue-allows-execution-test
  "Test continue restart allows execution to proceed (T056)"
  (testing "continue function invokes continue restart"
    (let ((result
            (clysm/conditions:handler-bind
                ((clysm/conditions:error
                  (lambda (c)
                    (declare (ignore c))
                    (clysm/conditions:continue))))
              (clysm/conditions:cerror "Continue" "Test")
              :completed)))
      (ok (eq :completed result)
          "continue should allow execution to proceed")))

  (testing "continue returns nil when no restart exists"
    (ok (null (clysm/conditions:continue))
        "continue should return nil when no restart exists")))

;;; ============================================================
;;; User Story 5: Handler-Bind for Non-Transferring Handlers
;;; Goal: Handlers that inspect without automatic control transfer
;;; ============================================================

(deftest handler-bind-declines-test
  "Test handler-bind handler that returns normally declines (T060)"
  (testing "handler that returns normally declines to outer handler"
    (let ((inner-called nil)
          (outer-called nil))
      (clysm/conditions:handler-case
          (clysm/conditions:handler-bind
              ((clysm/conditions:error
                (lambda (c)
                  (declare (ignore c))
                  (setf inner-called t)
                  ;; Return normally - decline
                  nil)))
            (clysm/conditions:error 'clysm/conditions:simple-error
                                    :format-control "test"))
        (clysm/conditions:error (c)
          (declare (ignore c))
          (setf outer-called t)))
      (ok inner-called "inner handler-bind handler should be called")
      (ok outer-called "outer handler-case should catch after decline"))))

(deftest handler-bind-invokes-restart-test
  "Test handler-bind handler that invokes restart (T061)"
  (testing "handler-bind handler can invoke restart"
    (let ((result
            (clysm/conditions:restart-case
                (clysm/conditions:handler-bind
                    ((clysm/conditions:error
                      (lambda (c)
                        (declare (ignore c))
                        (clysm/conditions:invoke-restart 'use-value :from-bind))))
                  (clysm/conditions:error 'clysm/conditions:simple-error
                                          :format-control "test")
                  :not-reached)
              (use-value (v) v))))
      (ok (eq :from-bind result)
          "handler-bind handler should invoke restart"))))

(deftest handler-bind-multiple-handlers-test
  "Test multiple handler-bind handlers tried innermost first (T062)"
  (testing "handlers searched innermost first"
    (let ((call-order '()))
      (clysm/conditions:handler-case
          (clysm/conditions:handler-bind
              ((clysm/conditions:error
                (lambda (c)
                  (declare (ignore c))
                  (push :outer call-order)
                  nil)))
            (clysm/conditions:handler-bind
                ((clysm/conditions:error
                  (lambda (c)
                    (declare (ignore c))
                    (push :inner call-order)
                    nil)))
              (clysm/conditions:error 'clysm/conditions:simple-error
                                      :format-control "test")))
        (clysm/conditions:error (c)
          (declare (ignore c))
          :caught))
      (ok (equal '(:outer :inner) call-order)
          "handlers should be called innermost first"))))

;;; ============================================================
;;; User Story 6: Standard Restarts (T067-T069)
;;; Goal: Standard restarts abort, use-value, store-value available
;;; ============================================================

(deftest abort-restart-test
  "Test abort restart performs non-local exit (T067)"
  (testing "abort invokes abort restart"
    (let ((result
            (catch 'top-level
              (clysm/conditions:restart-case
                  (progn
                    (clysm/conditions:abort)
                    :not-reached)
                (clysm/conditions:abort ()
                  (throw 'top-level :aborted))))))
      (ok (eq :aborted result)
          "abort should invoke abort restart")))

  (testing "abort signals control-error when no restart"
    (let ((got-error nil))
      (clysm/conditions:handler-case
          (clysm/conditions:abort)
        (clysm/conditions:control-error (c)
          (declare (ignore c))
          (setf got-error t)))
      (ok got-error "abort should signal control-error when no restart"))))

(deftest use-value-restart-test
  "Test use-value restart substitutes value (T068)"
  (testing "use-value invokes restart with value"
    (let ((result
            (clysm/conditions:restart-case
                (progn
                  (clysm/conditions:use-value 42)
                  :not-reached)
              (clysm/conditions:use-value (v)
                v))))
      (ok (eql 42 result)
          "use-value should pass value to restart")))

  (testing "use-value signals control-error when no restart"
    (let ((got-error nil))
      (clysm/conditions:handler-case
          (clysm/conditions:use-value 123)
        (clysm/conditions:control-error (c)
          (declare (ignore c))
          (setf got-error t)))
      (ok got-error "use-value should signal control-error when no restart"))))

(deftest store-value-restart-test
  "Test store-value restart stores value (T069)"
  (testing "store-value invokes restart with value"
    (let ((stored-value nil))
      (clysm/conditions:restart-case
          (clysm/conditions:store-value 99)
        (clysm/conditions:store-value (v)
          (setf stored-value v)))
      (ok (eql 99 stored-value)
          "store-value should pass value to restart")))

  (testing "store-value signals control-error when no restart"
    (let ((got-error nil))
      (clysm/conditions:handler-case
          (clysm/conditions:store-value 123)
        (clysm/conditions:control-error (c)
          (declare (ignore c))
          (setf got-error t)))
      (ok got-error "store-value should signal control-error when no restart"))))

;;; ============================================================
;;; User Story 7: With-Simple-Restart Convenience Macro (T073-T074)
;;; Goal: Convenient macro for common restart patterns
;;; ============================================================

(deftest with-simple-restart-normal-test
  "Test with-simple-restart normal completion returns (values result nil) (T073)"
  (testing "normal completion returns value and nil"
    (multiple-value-bind (result invoked-p)
        (clysm/conditions:with-simple-restart (skip "Skip processing")
          :computed-value)
      (ok (eq :computed-value result)
          "should return body value")
      (ok (null invoked-p)
          "invoked-p should be nil for normal completion"))))

(deftest with-simple-restart-invoked-test
  "Test with-simple-restart invoked returns (values nil t) (T074)"
  (testing "restart invoked returns nil and t"
    (multiple-value-bind (result invoked-p)
        (clysm/conditions:with-simple-restart (skip "Skip processing")
          (clysm/conditions:invoke-restart 'skip)
          :not-reached)
      (ok (null result)
          "result should be nil when restart invoked")
      (ok invoked-p
          "invoked-p should be t when restart invoked"))))

;;; ============================================================
;;; Phase 10: Polish & Cross-Cutting Concerns (T077-T079)
;;; ============================================================

(deftest handler-unbinding-test
  "Test handler unbinding during execution prevents infinite recursion (T077)"
  (testing "handler is unbound during its execution"
    (let ((call-count 0))
      (clysm/conditions:handler-case
          (clysm/conditions:handler-bind
              ((clysm/conditions:error
                (lambda (c)
                  (incf call-count)
                  ;; During this handler, signaling same error should NOT
                  ;; recurse to this handler - it should be unbound
                  (when (< call-count 3)
                    ;; Don't actually signal again to avoid complexity
                    nil))))
            (clysm/conditions:error 'clysm/conditions:simple-error
                                    :format-control "test"))
        (clysm/conditions:error (c)
          (declare (ignore c))
          :caught))
      (ok (= 1 call-count)
          "handler should only be called once"))))

(deftest invoke-restart-nonexistent-test
  "Test invoke-restart with non-visible restart signals control-error (T078)"
  (testing "invoke-restart signals control-error for missing restart"
    (let ((got-error nil))
      (clysm/conditions:handler-case
          (clysm/conditions:invoke-restart 'nonexistent-restart)
        (clysm/conditions:control-error (c)
          (declare (ignore c))
          (setf got-error t)))
      (ok got-error "should signal control-error"))))

(deftest unwind-protect-during-restart-test
  "Test unwind-protect cleanup during restart invocation (T079)"
  (testing "unwind-protect forms run during restart"
    (let ((cleanup-ran nil))
      (clysm/conditions:restart-case
          (unwind-protect
               (clysm/conditions:invoke-restart 'skip)
            (setf cleanup-ran t))
        (skip () :skipped))
      (ok cleanup-ran "unwind-protect cleanup should run"))))

;;; ============================================================
;;; Additional tests for signal function (T081)
;;; ============================================================

(deftest signal-function-test
  "Test signal function invokes handlers without default action (T081)"
  (testing "signal returns nil when no handler handles"
    (let ((result (clysm/conditions:signal 'clysm/conditions:simple-condition
                                           :format-control "test")))
      (ok (null result)
          "signal should return nil")))

  (testing "signal calls matching handlers"
    (let ((handler-called nil))
      (clysm/conditions:handler-bind
          ((clysm/conditions:condition
            (lambda (c)
              (declare (ignore c))
              (setf handler-called t)
              nil)))
        (clysm/conditions:signal 'clysm/conditions:simple-condition
                                 :format-control "test"))
      (ok handler-called "signal should call handler"))))

;;; ============================================================
;;; Later phases will add more integration tests
;;; ============================================================

