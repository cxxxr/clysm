;;;; tests/mcclim/test-suite.lisp — McCLIM's own test suite (mcclim/test, FiveAM).
;;;;   DISPLAY=:N bin/clysm script tests/mcclim/test-suite.lisp
;;;; Tests/dynamic-vars.lisp is left out: it needs threads.  Clysm has no
;;;; threads, so McCLIM runs in its single-process mode, in which
;;;; STREAM-READ-GESTURE.SMOKE also fails on SBCL (with :MCCLIM-NO-THREADS).
(require :quicklisp)
(funcall (find-symbol "QUICKLOAD" "QL") '(:mcclim :fiveam) :silent t)
(asdf::clysm-enable-component (asdf:find-system "mcclim/test") '("Tests" "dynamic-vars") nil)
(funcall (find-symbol "QUICKLOAD" "QL") :mcclim/test :silent t)
(let* ((results (funcall (find-symbol "RUN" "FIVEAM") :mcclim))
       (failed (remove-if-not (lambda (r) (typep r (find-symbol "TEST-FAILURE" "IT.BESE.FIVEAM")))
                              results))
       (names (remove-duplicates
               (mapcar (lambda (r)
                         (funcall (find-symbol "NAME" "IT.BESE.FIVEAM")
                                  (funcall (find-symbol "TEST-CASE" "IT.BESE.FIVEAM") r)))
                       failed))))
  (format t "~&checks: ~D  failed: ~D~%" (length results) (length failed))
  (format t "failed tests: ~S~%" names)
  (format t "~:[unexpected failures~;mcclim tests: ok~]~%"
          (subsetp names '(clim-tests::stream-read-gesture.smoke))))
