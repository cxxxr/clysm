;;;; tests/package.lisp - Test Package Definition

(defpackage #:clysm/tests
  (:use #:cl #:clysm)
  (:export
   ;; Test framework
   #:deftest
   #:defsuite
   #:is
   #:is-true
   #:is-false
   #:is-equal
   #:is-eql
   #:is-eq
   #:signals
   #:finishes

   ;; Test execution
   #:run-test
   #:run-suite
   #:run-all-tests

   ;; Test registry
   #:*test-suites*
   #:*current-suite*))
