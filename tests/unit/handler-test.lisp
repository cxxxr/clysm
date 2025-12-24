;;;; handler-test.lisp - Tests for handler stack operations
;;;; TDD: These tests define expected behavior BEFORE implementation

(in-package #:clysm/tests)

(deftest handler-struct-test
  "Test handler struct creation (T020)"
  (let ((h (clysm/runtime/condition:make-handler
            :type 'clysm/conditions:error
            :function (lambda (c) (declare (ignore c)) :handled))))
    (ok (eq 'clysm/conditions:error (clysm/runtime/condition:handler-type h))
        "handler-type accessor should work")
    (ok (functionp (clysm/runtime/condition:handler-function h))
        "handler-function accessor should work")))

(deftest handler-cluster-struct-test
  "Test handler-cluster struct creation (T020)"
  (let* ((h1 (clysm/runtime/condition:make-handler
              :type 'clysm/conditions:error
              :function (lambda (c) (declare (ignore c)) :h1)))
         (h2 (clysm/runtime/condition:make-handler
              :type 'clysm/conditions:warning
              :function (lambda (c) (declare (ignore c)) :h2)))
         (cluster (clysm/runtime/condition:make-handler-cluster
                   :handlers (list h1 h2))))
    (ok (= 2 (length (clysm/runtime/condition:handler-cluster-handlers cluster)))
        "handler-cluster should hold multiple handlers")))

(deftest handler-clusters-dynamic-variable-test
  "Test *handler-clusters* dynamic variable (T022)"
  ;; Should initially be nil or empty
  (ok (null clysm/runtime/condition:*handler-clusters*)
      "*handler-clusters* should initially be nil"))

(deftest push-pop-handler-cluster-test
  "Test handler stack push/pop operations (T024)"
  (let* ((h1 (clysm/runtime/condition:make-handler
              :type 'clysm/conditions:error
              :function (lambda (c) (declare (ignore c)) :h1)))
         (cluster1 (clysm/runtime/condition:make-handler-cluster
                    :handlers (list h1)))
         (cluster2 (clysm/runtime/condition:make-handler-cluster
                    :handlers nil)))
    ;; Save original state
    (let ((original clysm/runtime/condition:*handler-clusters*))
      ;; Push first cluster
      (let ((prev1 (clysm/runtime/condition:push-handler-cluster cluster1)))
        (ok (eq prev1 original)
            "push-handler-cluster should return previous state")
        (ok (eq cluster1 (car clysm/runtime/condition:*handler-clusters*))
            "pushed cluster should be at front")

        ;; Push second cluster
        (let ((prev2 (clysm/runtime/condition:push-handler-cluster cluster2)))
          (ok (eq cluster2 (car clysm/runtime/condition:*handler-clusters*))
              "second pushed cluster should be at front")
          (ok (eq cluster1 (cadr clysm/runtime/condition:*handler-clusters*))
              "first cluster should be second")

          ;; Pop second cluster
          (clysm/runtime/condition:pop-handler-cluster prev2)
          (ok (eq cluster1 (car clysm/runtime/condition:*handler-clusters*))
              "after pop, first cluster should be at front"))

        ;; Pop first cluster
        (clysm/runtime/condition:pop-handler-cluster prev1)
        (ok (eq original clysm/runtime/condition:*handler-clusters*)
            "after all pops, should return to original state")))))

(deftest with-handler-cluster-test
  "Test with-handler-cluster macro for proper cleanup (T024)"
  (let* ((original clysm/runtime/condition:*handler-clusters*)
         (cluster (clysm/runtime/condition:make-handler-cluster :handlers nil))
         (inside-value nil))
    ;; Normal exit
    (clysm/runtime/condition:with-handler-cluster cluster
      (setf inside-value clysm/runtime/condition:*handler-clusters*))
    (ok (eq cluster (car inside-value))
        "cluster should be visible inside with-handler-cluster")
    (ok (eq original clysm/runtime/condition:*handler-clusters*)
        "after normal exit, should restore original state")

    ;; Abnormal exit (catch/throw)
    (catch 'test-exit
      (clysm/runtime/condition:with-handler-cluster cluster
        (throw 'test-exit nil)))
    (ok (eq original clysm/runtime/condition:*handler-clusters*)
        "after abnormal exit, should restore original state")))

(deftest find-handler-test
  "Test find-handler function for condition type matching (T026)"
  (let* ((original clysm/runtime/condition:*handler-clusters*)
         (error-handler (clysm/runtime/condition:make-handler
                         :type 'clysm/conditions:error
                         :function (lambda (c) (declare (ignore c)) :error)))
         (warning-handler (clysm/runtime/condition:make-handler
                           :type 'clysm/conditions:warning
                           :function (lambda (c) (declare (ignore c)) :warning)))
         (cluster (clysm/runtime/condition:make-handler-cluster
                   :handlers (list error-handler warning-handler))))
    (unwind-protect
        (progn
          (clysm/runtime/condition:push-handler-cluster cluster)

          ;; Find handler for error
          (let ((error-condition (make-instance 'clysm/conditions:simple-error)))
            (multiple-value-bind (handler remaining)
                (clysm/runtime/condition:find-handler error-condition)
              (ok handler "should find handler for error")
              (ok (eq 'clysm/conditions:error
                      (clysm/runtime/condition:handler-type handler))
                  "should find error handler")
              (ok (null remaining)
                  "remaining should be rest of clusters")))

          ;; Find handler for warning
          (let ((warning-condition (make-instance 'clysm/conditions:simple-warning)))
            (multiple-value-bind (handler remaining)
                (clysm/runtime/condition:find-handler warning-condition)
              (ok handler "should find handler for warning")
              (ok (eq 'clysm/conditions:warning
                      (clysm/runtime/condition:handler-type handler))
                  "should find warning handler")))

          ;; control-error is a subtype of error, so should be caught by error handler
          ;; This tests that type inheritance works correctly
          (let ((control-condition (make-instance 'clysm/conditions:control-error)))
            (multiple-value-bind (handler remaining)
                (clysm/runtime/condition:find-handler control-condition)
              (declare (ignore remaining))
              (ok handler "should find handler for control-error (subtype of error)")
              (ok (eq 'clysm/conditions:error
                      (clysm/runtime/condition:handler-type handler))
                  "should find error handler for control-error"))))
      ;; Cleanup
      (setf clysm/runtime/condition:*handler-clusters* original))))

(deftest find-handler-innermost-first-test
  "Test that find-handler searches innermost to outermost (FR-013)"
  (let* ((original clysm/runtime/condition:*handler-clusters*)
         (outer-handler (clysm/runtime/condition:make-handler
                         :type 'clysm/conditions:error
                         :function (lambda (c) (declare (ignore c)) :outer)))
         (inner-handler (clysm/runtime/condition:make-handler
                         :type 'clysm/conditions:error
                         :function (lambda (c) (declare (ignore c)) :inner)))
         (outer-cluster (clysm/runtime/condition:make-handler-cluster
                         :handlers (list outer-handler)))
         (inner-cluster (clysm/runtime/condition:make-handler-cluster
                         :handlers (list inner-handler))))
    (unwind-protect
        (progn
          ;; Push outer then inner (inner is at front)
          (clysm/runtime/condition:push-handler-cluster outer-cluster)
          (clysm/runtime/condition:push-handler-cluster inner-cluster)

          (let ((condition (make-instance 'clysm/conditions:simple-error)))
            (multiple-value-bind (handler remaining)
                (clysm/runtime/condition:find-handler condition)
              (declare (ignore remaining))
              ;; Should find inner handler first
              (ok (eq :inner (funcall (clysm/runtime/condition:handler-function handler)
                                      condition))
                  "should find innermost handler first"))))
      ;; Cleanup
      (setf clysm/runtime/condition:*handler-clusters* original))))

(deftest call-handler-unbinding-test
  "Test that call-handler temporarily unbinds the matching cluster (edge case)"
  ;; This prevents infinite recursion when a handler signals the same condition
  (let* ((original clysm/runtime/condition:*handler-clusters*)
         (handler-called nil)
         (clusters-during-call nil)
         (handler (clysm/runtime/condition:make-handler
                   :type 'clysm/conditions:error
                   :function (lambda (c)
                               (declare (ignore c))
                               (setf handler-called t)
                               (setf clusters-during-call
                                     clysm/runtime/condition:*handler-clusters*)
                               :result)))
         (cluster (clysm/runtime/condition:make-handler-cluster
                   :handlers (list handler))))
    (unwind-protect
        (progn
          (clysm/runtime/condition:push-handler-cluster cluster)

          (let ((condition (make-instance 'clysm/conditions:simple-error)))
            (multiple-value-bind (found-handler remaining)
                (clysm/runtime/condition:find-handler condition)
              ;; Call the handler
              (let ((result (clysm/runtime/condition:call-handler
                             found-handler condition remaining)))
                (ok handler-called "handler should be called")
                (ok (eq :result result) "should return handler result")
                ;; During handler execution, the cluster should be unbound
                (ok (null clusters-during-call)
                    "cluster should be unbound during handler execution")))))
      ;; Cleanup
      (setf clysm/runtime/condition:*handler-clusters* original))))

;;; ============================================================
;;; signal-internal tests (T027)
;;; ============================================================

(deftest signal-internal-no-handlers-test
  "Test signal-internal returns nil when no handlers (T027)"
  (let ((original clysm/runtime/condition:*handler-clusters*))
    (unwind-protect
        (progn
          ;; Ensure no handlers
          (setf clysm/runtime/condition:*handler-clusters* nil)
          (let ((condition (make-instance 'clysm/conditions:simple-error)))
            (ok (null (clysm/conditions::signal-internal condition))
                "signal-internal should return nil when no handlers")))
      (setf clysm/runtime/condition:*handler-clusters* original))))

(deftest signal-internal-handler-declines-test
  "Test signal-internal when handler declines (returns normally) (T027)"
  (let* ((original clysm/runtime/condition:*handler-clusters*)
         (decline-count 0)
         (handler (clysm/runtime/condition:make-handler
                   :type 'clysm/conditions:error
                   :function (lambda (c)
                               (declare (ignore c))
                               ;; Return normally = decline
                               (incf decline-count)
                               nil)))
         (cluster (clysm/runtime/condition:make-handler-cluster
                   :handlers (list handler))))
    (unwind-protect
        (progn
          (clysm/runtime/condition:push-handler-cluster cluster)
          (let ((condition (make-instance 'clysm/conditions:simple-error)))
            ;; Handler declines, so signal-internal should return nil
            (ok (null (clysm/conditions::signal-internal condition))
                "signal-internal should return nil when handler declines")
            (ok (= 1 decline-count)
                "handler should be called once")))
      (setf clysm/runtime/condition:*handler-clusters* original))))

(deftest signal-internal-searches-multiple-handlers-test
  "Test signal-internal tries handlers until one handles or all decline (T027)"
  (let* ((original clysm/runtime/condition:*handler-clusters*)
         (call-log nil)
         (outer-handler (clysm/runtime/condition:make-handler
                         :type 'clysm/conditions:error
                         :function (lambda (c)
                                     (declare (ignore c))
                                     (push :outer call-log)
                                     nil))) ; decline
         (inner-handler (clysm/runtime/condition:make-handler
                         :type 'clysm/conditions:error
                         :function (lambda (c)
                                     (declare (ignore c))
                                     (push :inner call-log)
                                     nil))) ; decline
         (outer-cluster (clysm/runtime/condition:make-handler-cluster
                         :handlers (list outer-handler)))
         (inner-cluster (clysm/runtime/condition:make-handler-cluster
                         :handlers (list inner-handler))))
    (unwind-protect
        (progn
          (clysm/runtime/condition:push-handler-cluster outer-cluster)
          (clysm/runtime/condition:push-handler-cluster inner-cluster)
          (let ((condition (make-instance 'clysm/conditions:simple-error)))
            (clysm/conditions::signal-internal condition)
            ;; Both handlers should be called in order (inner first, then outer)
            (ok (= 2 (length call-log))
                "both handlers should be called")
            (ok (equal '(:outer :inner) call-log)
                "handlers should be called inner-to-outer")))
      (setf clysm/runtime/condition:*handler-clusters* original))))

(deftest signal-internal-type-matching-test
  "Test signal-internal only calls matching type handlers (T027)"
  (let* ((original clysm/runtime/condition:*handler-clusters*)
         (warning-called nil)
         (error-called nil)
         (warning-handler (clysm/runtime/condition:make-handler
                           :type 'clysm/conditions:warning
                           :function (lambda (c)
                                       (declare (ignore c))
                                       (setf warning-called t)
                                       nil)))
         (error-handler (clysm/runtime/condition:make-handler
                         :type 'clysm/conditions:error
                         :function (lambda (c)
                                     (declare (ignore c))
                                     (setf error-called t)
                                     nil)))
         (cluster (clysm/runtime/condition:make-handler-cluster
                   :handlers (list warning-handler error-handler))))
    (unwind-protect
        (progn
          (clysm/runtime/condition:push-handler-cluster cluster)
          ;; Signal an error - should only call error handler
          (let ((condition (make-instance 'clysm/conditions:simple-error)))
            (clysm/conditions::signal-internal condition)
            (ok (not warning-called)
                "warning handler should not be called for error")
            (ok error-called
                "error handler should be called for error")))
      (setf clysm/runtime/condition:*handler-clusters* original))))
