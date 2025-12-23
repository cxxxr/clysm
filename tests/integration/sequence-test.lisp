;;;; sequence-test.lisp - Integration tests for sequence functions (007-sequence-functions)
(in-package #:clysm/tests/integration/sequence)

;;; ============================================================
;;; Phase 3: User Story 1 - Basic List Operations
;;; ============================================================

;;; --- T006: length tests ---
(deftest test-length-empty-list
  "Verify (length nil) returns 0"
  (ok (= 0 (clysm/tests:compile-and-run '(length nil)))
      "(length nil) should return 0"))

(deftest test-length-basic
  "Verify (length '(1 2 3)) returns 3"
  (ok (= 3 (clysm/tests:compile-and-run '(length (list 1 2 3))))
      "(length (list 1 2 3)) should return 3"))

(deftest test-length-single
  "Verify (length '(1)) returns 1"
  (ok (= 1 (clysm/tests:compile-and-run '(length (list 1))))
      "(length (list 1)) should return 1"))

;;; --- T007: append tests ---
(deftest test-append-two-lists
  "Verify (append '(1 2) '(3 4)) returns (1 2 3 4)"
  (ok (= 1 (clysm/tests:compile-and-run '(car (append (list 1 2) (list 3 4)))))
      "(car (append '(1 2) '(3 4))) should return 1")
  (ok (= 3 (clysm/tests:compile-and-run '(third (append (list 1 2) (list 3 4)))))
      "(third (append '(1 2) '(3 4))) should return 3")
  (ok (= 4 (clysm/tests:compile-and-run '(length (append (list 1 2) (list 3 4)))))
      "(length (append '(1 2) '(3 4))) should return 4"))

(deftest test-append-empty-lists
  "Verify append with empty lists"
  (ok (null (clysm/tests:compile-and-run '(append nil nil)))
      "(append nil nil) should return NIL")
  (ok (= 1 (clysm/tests:compile-and-run '(car (append nil (list 1 2)))))
      "(car (append nil '(1 2))) should return 1")
  (ok (= 1 (clysm/tests:compile-and-run '(car (append (list 1 2) nil))))
      "(car (append '(1 2) nil)) should return 1"))

;;; --- T008: reverse tests ---
(deftest test-reverse-basic
  "Verify (reverse '(1 2 3)) returns (3 2 1)"
  (ok (= 3 (clysm/tests:compile-and-run '(car (reverse (list 1 2 3)))))
      "(car (reverse '(1 2 3))) should return 3")
  (ok (= 2 (clysm/tests:compile-and-run '(second (reverse (list 1 2 3)))))
      "(second (reverse '(1 2 3))) should return 2")
  (ok (= 1 (clysm/tests:compile-and-run '(third (reverse (list 1 2 3)))))
      "(third (reverse '(1 2 3))) should return 1"))

(deftest test-reverse-empty
  "Verify (reverse nil) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(reverse nil)))
      "(reverse nil) should return NIL"))

(deftest test-reverse-single
  "Verify (reverse '(1)) returns (1)"
  (ok (= 1 (clysm/tests:compile-and-run '(car (reverse (list 1)))))
      "(car (reverse '(1))) should return 1"))

;;; --- T009: nreverse tests ---
(deftest test-nreverse-basic
  "Verify (nreverse '(1 2 3)) returns (3 2 1)"
  (ok (= 3 (clysm/tests:compile-and-run '(car (nreverse (list 1 2 3)))))
      "(car (nreverse '(1 2 3))) should return 3")
  (ok (= 1 (clysm/tests:compile-and-run '(third (nreverse (list 1 2 3)))))
      "(third (nreverse '(1 2 3))) should return 1"))

(deftest test-nreverse-empty
  "Verify (nreverse nil) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(nreverse nil)))
      "(nreverse nil) should return NIL"))

;;; --- T010: last tests ---
(deftest test-last-basic
  "Verify (last '(1 2 3)) returns (3)"
  (ok (= 3 (clysm/tests:compile-and-run '(car (last (list 1 2 3)))))
      "(car (last '(1 2 3))) should return 3")
  (ok (null (clysm/tests:compile-and-run '(cdr (last (list 1 2 3)))))
      "(cdr (last '(1 2 3))) should return NIL"))

(deftest test-last-empty
  "Verify (last nil) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(last nil)))
      "(last nil) should return NIL"))

(deftest test-last-single
  "Verify (last '(1)) returns (1)"
  (ok (= 1 (clysm/tests:compile-and-run '(car (last (list 1)))))
      "(car (last '(1))) should return 1"))

;;; --- T011: butlast tests ---
(deftest test-butlast-basic
  "Verify (butlast '(1 2 3)) returns (1 2)"
  (ok (= 1 (clysm/tests:compile-and-run '(car (butlast (list 1 2 3)))))
      "(car (butlast '(1 2 3))) should return 1")
  (ok (= 2 (clysm/tests:compile-and-run '(second (butlast (list 1 2 3)))))
      "(second (butlast '(1 2 3))) should return 2")
  (ok (= 2 (clysm/tests:compile-and-run '(length (butlast (list 1 2 3)))))
      "(length (butlast '(1 2 3))) should return 2"))

(deftest test-butlast-empty
  "Verify (butlast nil) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(butlast nil)))
      "(butlast nil) should return NIL"))

(deftest test-butlast-single
  "Verify (butlast '(1)) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(butlast (list 1))))
      "(butlast '(1)) should return NIL"))

;;; --- T012: copy-list tests ---
(deftest test-copy-list-basic
  "Verify copy-list creates a copy"
  (ok (= 1 (clysm/tests:compile-and-run '(car (copy-list (list 1 2 3)))))
      "(car (copy-list '(1 2 3))) should return 1")
  (ok (= 3 (clysm/tests:compile-and-run '(third (copy-list (list 1 2 3)))))
      "(third (copy-list '(1 2 3))) should return 3")
  (ok (= 3 (clysm/tests:compile-and-run '(length (copy-list (list 1 2 3)))))
      "(length (copy-list '(1 2 3))) should return 3"))

(deftest test-copy-list-empty
  "Verify (copy-list nil) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(copy-list nil)))
      "(copy-list nil) should return NIL"))

;;; ============================================================
;;; Phase 4: User Story 2 - Higher-Order Functions
;;; ============================================================

;;; --- T021: mapcar tests ---
(deftest test-mapcar-basic
  "Verify (mapcar #'1+ '(1 2 3)) returns (2 3 4)"
  (ok (= 2 (clysm/tests:compile-and-run
            '(car (mapcar (lambda (x) (+ x 1)) (list 1 2 3)))))
      "(car (mapcar #'1+ '(1 2 3))) should return 2")
  (ok (= 4 (clysm/tests:compile-and-run
            '(third (mapcar (lambda (x) (+ x 1)) (list 1 2 3)))))
      "(third (mapcar #'1+ '(1 2 3))) should return 4"))

(deftest test-mapcar-empty
  "Verify (mapcar fn nil) returns NIL"
  (ok (null (clysm/tests:compile-and-run
             '(mapcar (lambda (x) (+ x 1)) nil)))
      "(mapcar #'1+ nil) should return NIL"))

;;; --- T022: mapc tests ---
(deftest test-mapc-return-value
  "Verify mapc returns the original list"
  ;; mapc returns the original list, not the mapped values
  (ok (= 1 (clysm/tests:compile-and-run
            '(car (mapc (lambda (x) (+ x 1)) (list 1 2 3)))))
      "(car (mapc #'1+ '(1 2 3))) should return 1 (original list)"))

;;; --- T023: maplist tests ---
(deftest test-maplist-basic
  "Verify maplist applies function to successive cdrs"
  (ok (= 3 (clysm/tests:compile-and-run
            '(car (maplist (lambda (x) (length x)) (list 1 2 3)))))
      "(car (maplist #'length '(1 2 3))) should return 3")
  (ok (= 1 (clysm/tests:compile-and-run
            '(third (maplist (lambda (x) (length x)) (list 1 2 3)))))
      "(third (maplist #'length '(1 2 3))) should return 1"))

;;; --- T024: reduce tests ---
(deftest test-reduce-basic
  "Verify (reduce #'+ '(1 2 3 4)) returns 10"
  (ok (= 10 (clysm/tests:compile-and-run
             '(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))))
      "(reduce #'+ '(1 2 3 4)) should return 10"))

(deftest test-reduce-single
  "Verify (reduce #'+ '(5)) returns 5"
  (ok (= 5 (clysm/tests:compile-and-run
            '(reduce (lambda (a b) (+ a b)) (list 5))))
      "(reduce #'+ '(5)) should return 5"))

;;; ============================================================
;;; Phase 5: User Story 3 - Search and Filter Functions
;;; ============================================================

;;; --- T030: find tests ---
(deftest test-find-found
  "Verify (find 2 '(1 2 3)) returns 2"
  (ok (= 2 (clysm/tests:compile-and-run '(find 2 (list 1 2 3))))
      "(find 2 '(1 2 3)) should return 2"))

(deftest test-find-not-found
  "Verify (find 5 '(1 2 3)) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(find 5 (list 1 2 3))))
      "(find 5 '(1 2 3)) should return NIL"))

;;; --- T031: find-if tests ---
(deftest test-find-if-basic
  "Verify (find-if #'evenp '(1 2 3 4)) returns 2"
  (ok (= 2 (clysm/tests:compile-and-run
            '(find-if (lambda (x) (= 0 (- x (* 2 (truncate x 2))))) (list 1 2 3 4))))
      "(find-if #'evenp '(1 2 3 4)) should return 2"))

(deftest test-find-if-not-found
  "Verify (find-if #'evenp '(1 3 5)) returns NIL"
  (ok (null (clysm/tests:compile-and-run
             '(find-if (lambda (x) (= 0 (- x (* 2 (truncate x 2))))) (list 1 3 5))))
      "(find-if #'evenp '(1 3 5)) should return NIL"))

;;; --- T032: position tests ---
(deftest test-position-found
  "Verify (position 2 '(1 2 3)) returns 1"
  (ok (= 1 (clysm/tests:compile-and-run '(position 2 (list 1 2 3))))
      "(position 2 '(1 2 3)) should return 1 (0-indexed)"))

(deftest test-position-not-found
  "Verify (position 5 '(1 2 3)) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(position 5 (list 1 2 3))))
      "(position 5 '(1 2 3)) should return NIL"))

;;; --- T033: position-if tests ---
(deftest test-position-if-basic
  "Verify (position-if #'evenp '(1 2 3)) returns 1"
  (ok (= 1 (clysm/tests:compile-and-run
            '(position-if (lambda (x) (= 0 (- x (* 2 (truncate x 2))))) (list 1 2 3 4))))
      "(position-if #'evenp '(1 2 3 4)) should return 1"))

;;; --- T034: remove tests ---
(deftest test-remove-single
  "Verify (remove 2 '(1 2 3)) returns (1 3)"
  (ok (= 2 (clysm/tests:compile-and-run '(length (remove 2 (list 1 2 3)))))
      "(length (remove 2 '(1 2 3))) should return 2")
  (ok (= 1 (clysm/tests:compile-and-run '(car (remove 2 (list 1 2 3)))))
      "(car (remove 2 '(1 2 3))) should return 1")
  (ok (= 3 (clysm/tests:compile-and-run '(second (remove 2 (list 1 2 3)))))
      "(second (remove 2 '(1 2 3))) should return 3"))

(deftest test-remove-multiple
  "Verify (remove 2 '(1 2 3 2 4)) removes all 2s"
  (ok (= 3 (clysm/tests:compile-and-run '(length (remove 2 (list 1 2 3 2 4)))))
      "(length (remove 2 '(1 2 3 2 4))) should return 3"))

;;; --- T035: remove-if tests ---
(deftest test-remove-if-basic
  "Verify (remove-if #'evenp '(1 2 3 4)) returns (1 3)"
  (ok (= 2 (clysm/tests:compile-and-run
            '(length (remove-if (lambda (x) (= 0 (- x (* 2 (truncate x 2))))) (list 1 2 3 4)))))
      "(length (remove-if #'evenp '(1 2 3 4))) should return 2"))

;;; --- T036: remove-if-not tests ---
(deftest test-remove-if-not-basic
  "Verify (remove-if-not #'evenp '(1 2 3 4)) returns (2 4)"
  (ok (= 2 (clysm/tests:compile-and-run
            '(length (remove-if-not (lambda (x) (= 0 (- x (* 2 (truncate x 2))))) (list 1 2 3 4)))))
      "(length (remove-if-not #'evenp '(1 2 3 4))) should return 2")
  (ok (= 2 (clysm/tests:compile-and-run
            '(car (remove-if-not (lambda (x) (= 0 (- x (* 2 (truncate x 2))))) (list 1 2 3 4)))))
      "(car (remove-if-not #'evenp '(1 2 3 4))) should return 2"))

;;; --- T037: count tests ---
(deftest test-count-none
  "Verify (count 5 '(1 2 3)) returns 0"
  (ok (= 0 (clysm/tests:compile-and-run '(count 5 (list 1 2 3))))
      "(count 5 '(1 2 3)) should return 0"))

(deftest test-count-single
  "Verify (count 2 '(1 2 3)) returns 1"
  (ok (= 1 (clysm/tests:compile-and-run '(count 2 (list 1 2 3))))
      "(count 2 '(1 2 3)) should return 1"))

(deftest test-count-multiple
  "Verify (count 2 '(2 1 2 3 2)) returns 3"
  (ok (= 3 (clysm/tests:compile-and-run '(count 2 (list 2 1 2 3 2))))
      "(count 2 '(2 1 2 3 2)) should return 3"))

;;; --- T038: count-if tests ---
(deftest test-count-if-basic
  "Verify (count-if #'evenp '(1 2 3 4)) returns 2"
  (ok (= 2 (clysm/tests:compile-and-run
            '(count-if (lambda (x) (= 0 (- x (* 2 (truncate x 2))))) (list 1 2 3 4))))
      "(count-if #'evenp '(1 2 3 4)) should return 2"))

;;; ============================================================
;;; Phase 6: User Story 4 - List Membership and Association
;;; ============================================================

;;; --- T049: member tests ---
(deftest test-member-found
  "Verify (member 2 '(1 2 3)) returns (2 3)"
  (ok (= 2 (clysm/tests:compile-and-run '(car (member 2 (list 1 2 3)))))
      "(car (member 2 '(1 2 3))) should return 2")
  (ok (= 3 (clysm/tests:compile-and-run '(second (member 2 (list 1 2 3)))))
      "(second (member 2 '(1 2 3))) should return 3"))

(deftest test-member-not-found
  "Verify (member 5 '(1 2 3)) returns NIL"
  (ok (null (clysm/tests:compile-and-run '(member 5 (list 1 2 3))))
      "(member 5 '(1 2 3)) should return NIL"))

;;; --- T050: assoc tests ---
;; Note: assoc uses cons pairs with car as key
(deftest test-assoc-found
  "Verify assoc finds key in alist"
  (ok (= 1 (clysm/tests:compile-and-run
            '(cdr (assoc 10 (list (cons 10 1) (cons 20 2))))))
      "(cdr (assoc 10 '((10 . 1) (20 . 2)))) should return 1"))

(deftest test-assoc-not-found
  "Verify (assoc key alist) returns NIL when not found"
  (ok (null (clysm/tests:compile-and-run
             '(assoc 30 (list (cons 10 1) (cons 20 2)))))
      "(assoc 30 '((10 . 1) (20 . 2))) should return NIL"))

;;; --- T051: rassoc tests ---
(deftest test-rassoc-found
  "Verify rassoc finds value in alist"
  (ok (= 10 (clysm/tests:compile-and-run
             '(car (rassoc 1 (list (cons 10 1) (cons 20 2))))))
      "(car (rassoc 1 '((10 . 1) (20 . 2)))) should return 10"))

(deftest test-rassoc-not-found
  "Verify (rassoc value alist) returns NIL when not found"
  (ok (null (clysm/tests:compile-and-run
             '(rassoc 3 (list (cons 10 1) (cons 20 2)))))
      "(rassoc 3 '((10 . 1) (20 . 2))) should return NIL"))

;;; ============================================================
;;; Phase 7: User Story 5 - Quantifier Predicates
;;; ============================================================

;;; --- T056: every tests ---
(deftest test-every-all-true
  "Verify (every #'numberp '(1 2 3)) returns T"
  (ok (clysm/tests:compile-and-run
       '(every (lambda (x) (> x 0)) (list 1 2 3)))
      "(every (> x 0) '(1 2 3)) should return T"))

(deftest test-every-one-false
  "Verify every returns NIL when one element fails"
  (ok (null (clysm/tests:compile-and-run
             '(every (lambda (x) (> x 0)) (list 1 -1 3))))
      "(every (> x 0) '(1 -1 3)) should return NIL"))

(deftest test-every-empty
  "Verify (every pred nil) returns T"
  (ok (clysm/tests:compile-and-run
       '(every (lambda (x) (> x 0)) nil))
      "(every pred nil) should return T"))

;;; --- T057: some tests ---
(deftest test-some-found
  "Verify some returns first satisfying value"
  (ok (clysm/tests:compile-and-run
       '(some (lambda (x) (> x 2)) (list 1 2 3 4)))
      "(some (> x 2) '(1 2 3 4)) should return non-NIL"))

(deftest test-some-not-found
  "Verify (some pred list) returns NIL when none match"
  (ok (null (clysm/tests:compile-and-run
             '(some (lambda (x) (> x 10)) (list 1 2 3))))
      "(some (> x 10) '(1 2 3)) should return NIL"))

;;; --- T058: notany tests ---
(deftest test-notany-none-match
  "Verify (notany pred list) returns T when none match"
  (ok (clysm/tests:compile-and-run
       '(notany (lambda (x) (> x 10)) (list 1 2 3)))
      "(notany (> x 10) '(1 2 3)) should return T"))

(deftest test-notany-one-matches
  "Verify notany returns NIL when one matches"
  (ok (null (clysm/tests:compile-and-run
             '(notany (lambda (x) (> x 2)) (list 1 2 3))))
      "(notany (> x 2) '(1 2 3)) should return NIL"))

;;; --- T059: notevery tests ---
(deftest test-notevery-one-fails
  "Verify (notevery pred list) returns T when one fails"
  (ok (clysm/tests:compile-and-run
       '(notevery (lambda (x) (> x 0)) (list 1 -1 3)))
      "(notevery (> x 0) '(1 -1 3)) should return T"))

(deftest test-notevery-all-pass
  "Verify notevery returns NIL when all pass"
  (ok (null (clysm/tests:compile-and-run
             '(notevery (lambda (x) (> x 0)) (list 1 2 3))))
      "(notevery (> x 0) '(1 2 3)) should return NIL"))
