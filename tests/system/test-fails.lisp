;;; A test that fails, to check that bin/clysm test reports it (tests/system/run.sh).
(defpackage :failing-tests (:use :cl :clysm-test))
(in-package :failing-tests)
(deftest arithmetic () (is (= (+ 1 1) 3)))
(run-tests-and-exit)
