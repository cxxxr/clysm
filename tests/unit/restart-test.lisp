;;;; restart-test.lisp - Tests for restart stack operations
;;;; TDD: These tests define expected behavior BEFORE implementation

(in-package #:clysm/tests)

(deftest restart-struct-test
  "Test restart struct creation (T021)"
  (let ((r (clysm/runtime/condition:make-%restart
            :name 'use-value
            :function (lambda (v) v)
            :report-function (lambda (s) (format s "Use a value"))
            :interactive-function (lambda () (list (read)))
            :test-function (lambda (c) (declare (ignore c)) t)
            :associated-conditions nil)))
    (ok (eq 'use-value (clysm/runtime/condition:%restart-name r))
        "%restart-name accessor should work")
    (ok (functionp (clysm/runtime/condition:%restart-function r))
        "%restart-function accessor should work")
    (ok (functionp (clysm/runtime/condition:%restart-report-function r))
        "%restart-report-function accessor should work")
    (ok (functionp (clysm/runtime/condition:%restart-interactive-function r))
        "%restart-interactive-function accessor should work")
    (ok (functionp (clysm/runtime/condition:%restart-test-function r))
        "%restart-test-function accessor should work")
    (ok (null (clysm/runtime/condition:%restart-associated-conditions r))
        "%restart-associated-conditions accessor should work")))

(deftest restart-cluster-struct-test
  "Test restart-cluster struct creation (T021)"
  (let* ((r1 (clysm/runtime/condition:make-%restart
              :name 'abort
              :function (lambda () (throw 'abort nil))))
         (r2 (clysm/runtime/condition:make-%restart
              :name 'continue
              :function (lambda () nil)))
         (catch-tag (gensym "RESTART"))
         (cluster (clysm/runtime/condition:make-restart-cluster
                   :restarts (list r1 r2)
                   :catch-tag catch-tag)))
    (ok (= 2 (length (clysm/runtime/condition:restart-cluster-restarts cluster)))
        "restart-cluster should hold multiple restarts")
    (ok (eq catch-tag (clysm/runtime/condition:restart-cluster-catch-tag cluster))
        "restart-cluster should store catch tag")))

(deftest restart-clusters-dynamic-variable-test
  "Test *restart-clusters* dynamic variable (T023)"
  ;; Should initially be nil or empty
  (ok (null clysm/runtime/condition:*restart-clusters*)
      "*restart-clusters* should initially be nil"))

(deftest push-pop-restart-cluster-test
  "Test restart stack push/pop operations (T025)"
  (let* ((r1 (clysm/runtime/condition:make-%restart
              :name 'abort
              :function (lambda () nil)))
         (cluster1 (clysm/runtime/condition:make-restart-cluster
                    :restarts (list r1)
                    :catch-tag (gensym)))
         (cluster2 (clysm/runtime/condition:make-restart-cluster
                    :restarts nil
                    :catch-tag (gensym))))
    ;; Save original state
    (let ((original clysm/runtime/condition:*restart-clusters*))
      ;; Push first cluster
      (let ((prev1 (clysm/runtime/condition:push-restart-cluster cluster1)))
        (ok (eq prev1 original)
            "push-restart-cluster should return previous state")
        (ok (eq cluster1 (car clysm/runtime/condition:*restart-clusters*))
            "pushed cluster should be at front")

        ;; Push second cluster
        (let ((prev2 (clysm/runtime/condition:push-restart-cluster cluster2)))
          (ok (eq cluster2 (car clysm/runtime/condition:*restart-clusters*))
              "second pushed cluster should be at front")
          (ok (eq cluster1 (cadr clysm/runtime/condition:*restart-clusters*))
              "first cluster should be second")

          ;; Pop second cluster
          (clysm/runtime/condition:pop-restart-cluster prev2)
          (ok (eq cluster1 (car clysm/runtime/condition:*restart-clusters*))
              "after pop, first cluster should be at front"))

        ;; Pop first cluster
        (clysm/runtime/condition:pop-restart-cluster prev1)
        (ok (eq original clysm/runtime/condition:*restart-clusters*)
            "after all pops, should return to original state")))))

(deftest with-restart-cluster-test
  "Test with-restart-cluster macro for proper cleanup (T025)"
  (let* ((original clysm/runtime/condition:*restart-clusters*)
         (cluster (clysm/runtime/condition:make-restart-cluster
                   :restarts nil
                   :catch-tag (gensym)))
         (inside-value nil))
    ;; Normal exit
    (clysm/runtime/condition:with-restart-cluster cluster
      (setf inside-value clysm/runtime/condition:*restart-clusters*))
    (ok (eq cluster (car inside-value))
        "cluster should be visible inside with-restart-cluster")
    (ok (eq original clysm/runtime/condition:*restart-clusters*)
        "after normal exit, should restore original state")

    ;; Abnormal exit (catch/throw)
    (catch 'test-exit
      (clysm/runtime/condition:with-restart-cluster cluster
        (throw 'test-exit nil)))
    (ok (eq original clysm/runtime/condition:*restart-clusters*)
        "after abnormal exit, should restore original state")))

(deftest restart-visibility-during-handler-test
  "Test that restarts remain visible during handler execution"
  ;; Unlike handlers, restarts should NOT be unbound during handler execution
  ;; This allows handlers to invoke restarts established by restart-case
  (let* ((original-handlers clysm/runtime/condition:*handler-clusters*)
         (original-restarts clysm/runtime/condition:*restart-clusters*)
         (restart-found nil)
         (restart (clysm/runtime/condition:make-%restart
                   :name 'test-restart
                   :function (lambda () :restarted)))
         (restart-cluster (clysm/runtime/condition:make-restart-cluster
                           :restarts (list restart)
                           :catch-tag (gensym)))
         (handler (clysm/runtime/condition:make-handler
                   :type 'clysm/conditions:error
                   :function (lambda (c)
                               (declare (ignore c))
                               ;; Check if restart is visible during handler
                               (setf restart-found
                                     clysm/runtime/condition:*restart-clusters*)
                               :handled)))
         (handler-cluster (clysm/runtime/condition:make-handler-cluster
                           :handlers (list handler))))
    (unwind-protect
        (progn
          ;; Push restart cluster first, then handler cluster
          (clysm/runtime/condition:push-restart-cluster restart-cluster)
          (clysm/runtime/condition:push-handler-cluster handler-cluster)

          (let ((condition (make-instance 'clysm/conditions:simple-error)))
            (multiple-value-bind (found-handler remaining)
                (clysm/runtime/condition:find-handler condition)
              ;; Call handler (this unbinds handler cluster but NOT restart cluster)
              (clysm/runtime/condition:call-handler found-handler condition remaining)
              ;; Restart cluster should have been visible during handler
              (ok (eq restart-cluster (car restart-found))
                  "restart cluster should be visible during handler execution"))))
      ;; Cleanup
      (setf clysm/runtime/condition:*handler-clusters* original-handlers)
      (setf clysm/runtime/condition:*restart-clusters* original-restarts))))

;;; ============================================================
;;; Tests for compute-restarts and find-restart (T038, T039)
;;; ============================================================

(deftest compute-restarts-test
  "Test compute-restarts returns all visible restarts (T038)"
  (let* ((original clysm/runtime/condition:*restart-clusters*)
         (r1 (clysm/runtime/condition:make-%restart
              :name 'abort
              :function (lambda () nil)))
         (r2 (clysm/runtime/condition:make-%restart
              :name 'continue
              :function (lambda () nil)))
         (cluster (clysm/runtime/condition:make-restart-cluster
                   :restarts (list r1 r2)
                   :catch-tag (gensym))))
    (unwind-protect
        (progn
          (clysm/runtime/condition:push-restart-cluster cluster)
          (let ((restarts (clysm/conditions:compute-restarts)))
            (ok (= 2 (length restarts))
                "compute-restarts should return all restarts")
            (ok (member r1 restarts)
                "abort restart should be in list")
            (ok (member r2 restarts)
                "continue restart should be in list")))
      (setf clysm/runtime/condition:*restart-clusters* original))))

(deftest find-restart-by-name-test
  "Test find-restart locates restart by name (T039)"
  (let* ((original clysm/runtime/condition:*restart-clusters*)
         (r1 (clysm/runtime/condition:make-%restart
              :name 'use-value
              :function (lambda (v) v)))
         (cluster (clysm/runtime/condition:make-restart-cluster
                   :restarts (list r1)
                   :catch-tag (gensym))))
    (unwind-protect
        (progn
          (clysm/runtime/condition:push-restart-cluster cluster)
          (ok (eq r1 (clysm/conditions:find-restart 'use-value))
              "find-restart should find restart by name")
          (ok (null (clysm/conditions:find-restart 'nonexistent))
              "find-restart should return nil for unknown name"))
      (setf clysm/runtime/condition:*restart-clusters* original))))
