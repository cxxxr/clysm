;;;; types-test.lisp - Unit tests for workflow data types
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Tests struct definitions and their accessors

(defpackage #:clysm/tests/unit/workflow/types-test
  (:use #:cl #:rove #:clysm/workflow))

(in-package #:clysm/tests/unit/workflow/types-test)

;;; ============================================================
;;; T013: Unit tests for all struct definitions
;;; ============================================================

(deftest source-module-creation-test
  "Test source-module struct creation and accessors."
  (let ((module (make-source-module
                 :path "/home/user/project/src/foo.lisp"
                 :relative-path "src/foo.lisp"
                 :package 'foo
                 :mtime 1735300800
                 :hash "abc123def456"
                 :forms '((defun foo () 42))
                 :form-count 1
                 :dependencies '("src/bar.lisp")
                 :status :pending)))
    (testing "path accessor"
      (ok (equal (source-module-path module) "/home/user/project/src/foo.lisp")))
    (testing "relative-path accessor"
      (ok (equal (source-module-relative-path module) "src/foo.lisp")))
    (testing "package accessor"
      (ok (eq (source-module-package module) 'foo)))
    (testing "mtime accessor"
      (ok (= (source-module-mtime module) 1735300800)))
    (testing "hash accessor"
      (ok (equal (source-module-hash module) "abc123def456")))
    (testing "forms accessor"
      (ok (equal (source-module-forms module) '((defun foo () 42)))))
    (testing "form-count accessor"
      (ok (= (source-module-form-count module) 1)))
    (testing "dependencies accessor"
      (ok (equal (source-module-dependencies module) '("src/bar.lisp"))))
    (testing "status accessor"
      (ok (eq (source-module-status module) :pending)))))

(deftest compilation-error-creation-test
  "Test compilation-error struct creation and accessors."
  (let ((err (make-compilation-error
              :severity :error
              :message "Undefined function FOO"
              :path "src/bar.lisp"
              :line 42
              :column 10
              :form '(foo 1 2)
              :operator 'foo
              :suggestion "Did you mean BAR?")))
    (testing "severity accessor"
      (ok (eq (compilation-error-severity err) :error)))
    (testing "message accessor"
      (ok (equal (compilation-error-message err) "Undefined function FOO")))
    (testing "path accessor"
      (ok (equal (compilation-error-path err) "src/bar.lisp")))
    (testing "line accessor"
      (ok (= (compilation-error-line err) 42)))
    (testing "column accessor"
      (ok (= (compilation-error-column err) 10)))
    (testing "form accessor"
      (ok (equal (compilation-error-form err) '(foo 1 2))))
    (testing "operator accessor"
      (ok (eq (compilation-error-operator err) 'foo)))
    (testing "suggestion accessor"
      (ok (equal (compilation-error-suggestion err) "Did you mean BAR?")))))

(deftest compilation-result-creation-test
  "Test compilation-result struct creation and accessors."
  (let ((result (make-compilation-result
                 :success-p t
                 :wasm-bytes #(0 97 115 109)
                 :byte-count 4
                 :form-count 10
                 :forms-compiled 8
                 :forms-failed 1
                 :forms-skipped 1
                 :compile-time-ms 150)))
    (testing "success-p accessor"
      (ok (compilation-result-success-p result)))
    (testing "wasm-bytes accessor"
      (ok (equalp (compilation-result-wasm-bytes result) #(0 97 115 109))))
    (testing "byte-count accessor"
      (ok (= (compilation-result-byte-count result) 4)))
    (testing "form-count accessor"
      (ok (= (compilation-result-form-count result) 10)))
    (testing "forms-compiled accessor"
      (ok (= (compilation-result-forms-compiled result) 8)))
    (testing "forms-failed accessor"
      (ok (= (compilation-result-forms-failed result) 1)))
    (testing "forms-skipped accessor"
      (ok (= (compilation-result-forms-skipped result) 1)))
    (testing "compile-time-ms accessor"
      (ok (= (compilation-result-compile-time-ms result) 150)))))

(deftest compilation-options-creation-test
  "Test compilation-options struct creation and accessors."
  (let ((opts (make-compilation-options
               :output "output.wasm"
               :verbose t
               :force nil
               :continue-on-error t
               :cache-path ".clysm-cache")))
    (testing "output accessor"
      (ok (equal (compilation-options-output opts) "output.wasm")))
    (testing "verbose accessor"
      (ok (compilation-options-verbose opts)))
    (testing "force accessor"
      (ok (not (compilation-options-force opts))))
    (testing "continue-on-error accessor"
      (ok (compilation-options-continue-on-error opts)))
    (testing "cache-path accessor"
      (ok (equal (compilation-options-cache-path opts) ".clysm-cache")))))

(deftest progress-info-creation-test
  "Test progress-info struct creation and accessors."
  (let ((progress (make-progress-info
                   :phase :compiling
                   :total-modules 45
                   :current-module 12
                   :current-path "src/compiler/ast.lisp"
                   :percentage 26.7
                   :message "Compiling ast.lisp...")))
    (testing "phase accessor"
      (ok (eq (progress-info-phase progress) :compiling)))
    (testing "total-modules accessor"
      (ok (= (progress-info-total-modules progress) 45)))
    (testing "current-module accessor"
      (ok (= (progress-info-current-module progress) 12)))
    (testing "current-path accessor"
      (ok (equal (progress-info-current-path progress) "src/compiler/ast.lisp")))
    (testing "percentage accessor"
      (ok (< (abs (- (progress-info-percentage progress) 26.7)) 0.01)))
    (testing "message accessor"
      (ok (equal (progress-info-message progress) "Compiling ast.lisp...")))))

(deftest compilation-session-creation-test
  "Test compilation-session struct creation and accessors."
  (let ((session (make-compilation-session
                  :start-time 1735300800000
                  :project-root "/home/user/project"
                  :output-path "/home/user/project/output.wasm"
                  :input-patterns '("src/**/*.lisp"))))
    (testing "start-time accessor"
      (ok (= (compilation-session-start-time session) 1735300800000)))
    (testing "project-root accessor"
      (ok (equal (compilation-session-project-root session) "/home/user/project")))
    (testing "output-path accessor"
      (ok (equal (compilation-session-output-path session) "/home/user/project/output.wasm")))
    (testing "input-patterns accessor"
      (ok (equal (compilation-session-input-patterns session) '("src/**/*.lisp"))))))

(deftest dependency-graph-creation-test
  "Test dependency-graph struct creation and accessors."
  (let* ((path-index (make-hash-table :test 'equal))
         (dependents (make-hash-table :test 'equal))
         (dependencies (make-hash-table :test 'equal))
         (graph (make-dependency-graph
                 :modules nil
                 :path-index path-index
                 :order '("a.lisp" "b.lisp")
                 :dependents dependents
                 :dependencies dependencies)))
    (testing "path-index accessor"
      (ok (hash-table-p (dependency-graph-path-index graph))))
    (testing "order accessor"
      (ok (equal (dependency-graph-order graph) '("a.lisp" "b.lisp"))))
    (testing "dependents accessor"
      (ok (hash-table-p (dependency-graph-dependents graph))))
    (testing "dependencies accessor"
      (ok (hash-table-p (dependency-graph-dependencies graph))))))

(deftest cached-module-creation-test
  "Test cached-module struct creation and accessors."
  (let ((cached (make-cached-module
                 :path "src/foo.lisp"
                 :mtime 1735300800
                 :hash "abc123"
                 :compiled-size 1024
                 :compile-time-ms 50)))
    (testing "path accessor"
      (ok (equal (cached-module-path cached) "src/foo.lisp")))
    (testing "mtime accessor"
      (ok (= (cached-module-mtime cached) 1735300800)))
    (testing "hash accessor"
      (ok (equal (cached-module-hash cached) "abc123")))
    (testing "compiled-size accessor"
      (ok (= (cached-module-compiled-size cached) 1024)))
    (testing "compile-time-ms accessor"
      (ok (= (cached-module-compile-time-ms cached) 50)))))

(deftest compilation-cache-creation-test
  "Test compilation-cache struct creation and accessors."
  (let* ((modules (make-hash-table :test 'equal))
         (cache (make-compilation-cache
                 :version 1
                 :timestamp "2025-12-27T12:00:00Z"
                 :project-root "/home/user/project"
                 :modules modules)))
    (testing "version accessor"
      (ok (= (compilation-cache-version cache) 1)))
    (testing "timestamp accessor"
      (ok (equal (compilation-cache-timestamp cache) "2025-12-27T12:00:00Z")))
    (testing "project-root accessor"
      (ok (equal (compilation-cache-project-root cache) "/home/user/project")))
    (testing "modules accessor"
      (ok (hash-table-p (compilation-cache-modules cache))))))

(deftest source-module-status-transitions-test
  "Test that source-module status can be updated through all valid states."
  (let ((module (make-source-module :status :pending)))
    (testing "initial status is :pending"
      (ok (eq (source-module-status module) :pending)))
    (testing "can transition to :compiling"
      (setf (source-module-status module) :compiling)
      (ok (eq (source-module-status module) :compiling)))
    (testing "can transition to :compiled"
      (setf (source-module-status module) :compiled)
      (ok (eq (source-module-status module) :compiled)))
    (testing "can transition to :failed"
      (setf (source-module-status module) :failed)
      (ok (eq (source-module-status module) :failed)))
    (testing "can transition to :skipped"
      (setf (source-module-status module) :skipped)
      (ok (eq (source-module-status module) :skipped)))))
