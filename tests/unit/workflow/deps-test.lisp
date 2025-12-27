;;;; deps-test.lisp - Unit tests for dependency analysis
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; T034: Unit test for dependency graph construction

(defpackage #:clysm/tests/unit/workflow/deps-test
  (:use #:cl #:rove #:clysm/workflow))

(in-package #:clysm/tests/unit/workflow/deps-test)

;;; ============================================================
;;; T034: Unit tests for dependency graph
;;; ============================================================

(deftest extract-package-test
  "Test extracting package from forms."
  (testing "extracts quoted symbol"
    (let ((forms '((in-package :foo) (defun bar () 42))))
      (ok (eq (extract-package forms) :foo))))
  (testing "extracts unquoted symbol"
    (let ((forms '((in-package foo))))
      (ok (eq (extract-package forms) 'foo))))
  (testing "returns NIL for empty forms"
    (ok (null (extract-package nil))))
  (testing "returns NIL when no in-package"
    (ok (null (extract-package '((defun foo () 42)))))))

(deftest build-dependency-graph-test
  "Test building dependency graph."
  (let* ((mod1 (make-source-module :path "/a.lisp" :relative-path "a.lisp"))
         (mod2 (make-source-module :path "/b.lisp" :relative-path "b.lisp"))
         (graph (build-dependency-graph (list mod1 mod2))))
    (testing "graph has correct order"
      (ok (equal (dependency-graph-order graph) '("a.lisp" "b.lisp"))))
    (testing "path-index contains modules"
      (ok (eq (gethash "a.lisp" (dependency-graph-path-index graph)) mod1)))
    (testing "b depends on a"
      (ok (member "a.lisp" (gethash "b.lisp" (dependency-graph-dependencies graph))
                  :test #'equal)))))

(deftest get-dependents-test
  "Test getting dependents of a module."
  (let* ((mod1 (make-source-module :path "/a.lisp" :relative-path "a.lisp"))
         (mod2 (make-source-module :path "/b.lisp" :relative-path "b.lisp"))
         (mod3 (make-source-module :path "/c.lisp" :relative-path "c.lisp"))
         (graph (build-dependency-graph (list mod1 mod2 mod3))))
    (testing "a has b and c as dependents"
      (let ((deps (get-dependents graph "a.lisp")))
        (ok (= (length deps) 2))
        (ok (member "b.lisp" deps :test #'equal))
        (ok (member "c.lisp" deps :test #'equal))))
    (testing "c has no dependents"
      (ok (null (get-dependents graph "c.lisp"))))))

(deftest detect-cycles-test
  "Test cycle detection."
  (let* ((mod1 (make-source-module :path "/a.lisp" :relative-path "a.lisp"))
         (mod2 (make-source-module :path "/b.lisp" :relative-path "b.lisp"))
         (graph (build-dependency-graph (list mod1 mod2))))
    (testing "linear dependencies have no cycles"
      (ok (null (detect-cycles graph))))))
