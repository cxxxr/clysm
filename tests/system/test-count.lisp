;;; Tests of tests/system (bin/clysm test --system wordcount-tests ...).
(defpackage :wordcount-tests (:use :cl :clysm-test :wordcount))
(in-package :wordcount-tests)
(deftest words () (is-equal 3 (count-words "a b  c")))
(deftest empty () (is-equal 0 (count-words "")))
(deftest no-number () (signals error (count-words 42)))
(run-tests-and-exit)
