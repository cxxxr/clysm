;;; A small test library without dependencies (bin/clysm test).
(defsystem "clysm-test"
  :description "Tests for programs compiled by Clysm: DEFTEST, IS, SIGNALS, RUN-TESTS"
  :components ((:file "clysm-test")))
