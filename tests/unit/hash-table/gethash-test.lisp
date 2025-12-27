;;;; gethash-test.lisp - Unit tests for gethash compilation
;;;;
;;;; Feature: 043-self-hosting-blockers
;;;; User Story: US4 - Hash Tables

(in-package #:clysm/tests)

(deftest gethash-ast-test
  "Test parsing of gethash forms."
  (testing "gethash with 2 args parses correctly"
    (let ((ast (clysm/compiler::parse-form '(gethash 'key ht))))
      (ok (clysm/compiler::ast-call-p ast))
      (ok (eq 'gethash (clysm/compiler::ast-call-name ast)))))

  (testing "gethash with 3 args (default) parses correctly"
    (let ((ast (clysm/compiler::parse-form '(gethash 'key ht nil))))
      (ok (clysm/compiler::ast-call-p ast)))))

(deftest setf-gethash-test
  "Test (setf gethash) compilation."
  (testing "(setf gethash) parses correctly"
    (let ((ast (clysm/compiler::parse-form '(setf (gethash 'key ht) 'value))))
      (ok ast "Should parse (setf gethash)"))))
