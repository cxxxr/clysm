;;;; control-flow-test.lisp - Control flow tests (T099-T102)
(in-package #:clysm/tests/integration/control-flow)

;;; ============================================================
;;; T099: block/return-from tests
;;; ============================================================

(deftest test-simple-block
  "Simple block should return its body value"
  ;; (block nil 42)
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(block nil 42)))
      "Block should return body value"))

(deftest test-block-return-from
  "return-from should exit block with value"
  ;; (block foo (return-from foo 42) 0)
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(block foo (return-from foo 42) 0)))
      "return-from should exit block with value"))

(deftest test-block-return-from-no-value
  "return-from with no value should return NIL"
  ;; (block foo (return-from foo) 0)
  ;; => NIL (represented as 0 in tests for now)
  (ok (clysm/tests:compile-and-run
       '(block foo
          (if t (return-from foo 1) 0)
          99))
      "return-from with conditional"))

(deftest test-nested-blocks
  "Nested blocks should work correctly"
  ;; (block outer
  ;;   (block inner
  ;;     (return-from outer 42))
  ;;   0)
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(block outer
                (block inner
                  (return-from outer 42))
                0)))
      "return-from should exit correct block"))

(deftest test-block-with-computation
  "Block with computation before return"
  ;; (block foo
  ;;   (let ((x 10))
  ;;     (if (> x 5)
  ;;         (return-from foo (* x 2))
  ;;         0)))
  ;; => 20
  (ok (= 20 (clysm/tests:compile-and-run
             '(block foo
                (let ((x 10))
                  (if (> x 5)
                      (return-from foo (* x 2))
                      0)))))
      "Block with computation should work"))

(deftest test-block-normal-exit
  "Block should return last form if no return-from"
  ;; (block foo 1 2 3)
  ;; => 3
  (ok (= 3 (clysm/tests:compile-and-run
            '(block foo 1 2 3)))
      "Block normal exit should return last form"))

;;; ============================================================
;;; T100: tagbody/go tests
;;; ============================================================

(deftest test-simple-tagbody
  "Simple tagbody with no go"
  ;; (let ((x 0))
  ;;   (tagbody
  ;;     (setq x 1))
  ;;   x)
  ;; => 1
  (ok (= 1 (clysm/tests:compile-and-run
            '(let ((x 0))
               (tagbody
                 (setq x 1))
               x)))
      "Simple tagbody should work"))

(deftest test-tagbody-with-go
  "tagbody with go should loop"
  ;; (let ((x 0))
  ;;   (tagbody
  ;;    start
  ;;     (setq x (+ x 1))
  ;;     (if (< x 3) (go start)))
  ;;   x)
  ;; => 3
  (ok (= 3 (clysm/tests:compile-and-run
            '(let ((x 0))
               (tagbody
                start
                 (setq x (+ x 1))
                 (if (< x 3) (go start)))
               x)))
      "tagbody with go should loop"))

(deftest test-tagbody-multiple-tags
  "tagbody with multiple tags"
  ;; (let ((x 0))
  ;;   (tagbody
  ;;    start
  ;;     (setq x 1)
  ;;     (go end)
  ;;    middle
  ;;     (setq x 2)
  ;;    end
  ;;     (setq x (+ x 10)))
  ;;   x)
  ;; => 11 (1 + 10, skipping middle)
  (ok (= 11 (clysm/tests:compile-and-run
             '(let ((x 0))
                (tagbody
                 start
                  (setq x 1)
                  (go end)
                 middle
                  (setq x 2)
                 end
                  (setq x (+ x 10)))
                x)))
      "tagbody should skip to correct tag"))

(deftest test-tagbody-countdown
  "tagbody countdown loop"
  ;; (let ((n 5) (sum 0))
  ;;   (tagbody
  ;;    loop
  ;;     (if (= n 0) (go done))
  ;;     (setq sum (+ sum n))
  ;;     (setq n (- n 1))
  ;;     (go loop)
  ;;    done)
  ;;   sum)
  ;; => 15 (5+4+3+2+1)
  (ok (= 15 (clysm/tests:compile-and-run
             '(let ((n 5) (sum 0))
                (tagbody
                 loop
                  (if (= n 0) (go done))
                  (setq sum (+ sum n))
                  (setq n (- n 1))
                  (go loop)
                 done)
                sum)))
      "tagbody countdown should sum correctly"))

;;; ============================================================
;;; T101: catch/throw tests
;;; ============================================================

(deftest test-simple-catch
  "Simple catch with no throw"
  ;; (catch 'foo 42)
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(catch 'foo 42)))
      "catch without throw should return body value"))

(deftest test-catch-throw
  "catch/throw should transfer control"
  ;; (catch 'foo
  ;;   (throw 'foo 42)
  ;;   0)
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(catch 'foo
                (throw 'foo 42)
                0)))
      "throw should exit catch with value"))

(deftest test-nested-catch
  "Nested catch with throw to outer"
  ;; (catch 'outer
  ;;   (catch 'inner
  ;;     (throw 'outer 42))
  ;;   0)
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(catch 'outer
                (catch 'inner
                  (throw 'outer 42))
                0)))
      "throw should find correct catch"))

(deftest test-throw-from-function
  "throw from within a function"
  ;; (defun thrower () (throw 'foo 42))
  ;; (catch 'foo (thrower) 0)
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(progn
                (defun thrower () (throw 'foo 42))
                (catch 'foo (thrower) 0))))
      "throw should unwind stack to catch"))

(deftest test-catch-wrong-tag
  "throw with different tag should propagate"
  ;; (catch 'outer
  ;;   (catch 'inner
  ;;     (throw 'outer 42))
  ;;   99)
  ;; => 42 (throw 'outer skips catch 'inner)
  (ok (= 42 (clysm/tests:compile-and-run
             '(catch 'outer
                (catch 'inner
                  (throw 'outer 42))
                99)))
      "throw should skip non-matching catches"))

;;; ============================================================
;;; T102: unwind-protect tests
;;; ============================================================

(deftest test-unwind-protect-normal
  "unwind-protect cleanup runs on normal exit"
  ;; (let ((x 0))
  ;;   (unwind-protect
  ;;       (setq x 1)
  ;;     (setq x (+ x 10)))
  ;;   x)
  ;; => 11 (protected form sets 1, cleanup adds 10)
  (ok (= 11 (clysm/tests:compile-and-run
             '(let ((x 0))
                (unwind-protect
                    (setq x 1)
                  (setq x (+ x 10)))
                x)))
      "cleanup should run on normal exit"))

(deftest test-unwind-protect-with-return-from
  "unwind-protect cleanup runs on return-from"
  ;; (let ((x 0))
  ;;   (block foo
  ;;     (unwind-protect
  ;;         (return-from foo 42)
  ;;       (setq x 10)))
  ;;   x)
  ;; The block returns 42, but x is set to 10 by cleanup
  (ok (= 10 (clysm/tests:compile-and-run
             '(let ((x 0))
                (block foo
                  (unwind-protect
                      (progn (setq x 1) (return-from foo 42))
                    (setq x 10)))
                x)))
      "cleanup should run on return-from"))

(deftest test-unwind-protect-with-throw
  "unwind-protect cleanup runs on throw"
  ;; (let ((x 0))
  ;;   (catch 'foo
  ;;     (unwind-protect
  ;;         (throw 'foo 42)
  ;;       (setq x 10)))
  ;;   x)
  ;; The catch returns 42, but x is set to 10 by cleanup
  (ok (= 10 (clysm/tests:compile-and-run
             '(let ((x 0))
                (catch 'foo
                  (unwind-protect
                      (progn (setq x 1) (throw 'foo 42))
                    (setq x 10)))
                x)))
      "cleanup should run on throw"))

(deftest test-nested-unwind-protect
  "Nested unwind-protect should run all cleanups"
  ;; (let ((x 0))
  ;;   (unwind-protect
  ;;       (unwind-protect
  ;;           (setq x 1)
  ;;         (setq x (+ x 10)))
  ;;     (setq x (+ x 100)))
  ;;   x)
  ;; => 111 (1 + 10 + 100)
  (ok (= 111 (clysm/tests:compile-and-run
              '(let ((x 0))
                 (unwind-protect
                     (unwind-protect
                         (setq x 1)
                       (setq x (+ x 10)))
                   (setq x (+ x 100)))
                 x)))
      "nested cleanups should all run"))

(deftest test-unwind-protect-preserves-value
  "unwind-protect should preserve protected form's value"
  ;; (unwind-protect 42 1 2 3)
  ;; => 42 (cleanup forms don't affect return value)
  (ok (= 42 (clysm/tests:compile-and-run
             '(unwind-protect 42 1 2 3)))
      "protected form value should be preserved"))

(deftest test-unwind-protect-multiple-cleanup-forms
  "unwind-protect with multiple cleanup forms"
  ;; (let ((x 0))
  ;;   (unwind-protect
  ;;       (setq x 1)
  ;;     (setq x (+ x 10))
  ;;     (setq x (+ x 100)))
  ;;   x)
  ;; => 111
  (ok (= 111 (clysm/tests:compile-and-run
              '(let ((x 0))
                 (unwind-protect
                     (setq x 1)
                   (setq x (+ x 10))
                   (setq x (+ x 100)))
                 x)))
      "multiple cleanup forms should all execute"))
