;;;; test-list-ops.lisp - Quick verification of list operations
(asdf:load-system :clysm)

(format t "~%=== Testing list operation compilation ===~%")

;; Test last
(handler-case
    (progn
      (clysm:compile-to-wasm '(last '(a b c)))
      (format t "[PASS] (last '(a b c))~%"))
  (error (e) (format t "[FAIL] (last '(a b c)): ~a~%" e)))

;; Test butlast
(handler-case
    (progn
      (clysm:compile-to-wasm '(butlast '(1 2 3 4 5)))
      (format t "[PASS] (butlast '(1 2 3 4 5))~%"))
  (error (e) (format t "[FAIL] (butlast '(1 2 3 4 5)): ~a~%" e)))

;; Test nth
(handler-case
    (progn
      (clysm:compile-to-wasm '(nth 2 '(a b c d)))
      (format t "[PASS] (nth 2 '(a b c d))~%"))
  (error (e) (format t "[FAIL] (nth 2 '(a b c d)): ~a~%" e)))

;; Test nthcdr
(handler-case
    (progn
      (clysm:compile-to-wasm '(nthcdr 2 '(a b c d)))
      (format t "[PASS] (nthcdr 2 '(a b c d))~%"))
  (error (e) (format t "[FAIL] (nthcdr 2 '(a b c d)): ~a~%" e)))

;; Test member (from spec SC-004)
(handler-case
    (progn
      (clysm:compile-to-wasm '(member 2 '(1 2 3)))
      (format t "[PASS] (member 2 '(1 2 3))~%"))
  (error (e) (format t "[FAIL] (member 2 '(1 2 3)): ~a~%" e)))

;; Test intersection (from spec SC-004)
(handler-case
    (progn
      (clysm:compile-to-wasm '(intersection '(1 2 3) '(2 3 4)))
      (format t "[PASS] (intersection '(1 2 3) '(2 3 4))~%"))
  (error (e) (format t "[FAIL] (intersection '(1 2 3) '(2 3 4)): ~a~%" e)))

;; Test union
(handler-case
    (progn
      (clysm:compile-to-wasm '(union '(1 2 3) '(2 3 4)))
      (format t "[PASS] (union '(1 2 3) '(2 3 4))~%"))
  (error (e) (format t "[FAIL] (union '(1 2 3) '(2 3 4)): ~a~%" e)))

;; Test set-difference
(handler-case
    (progn
      (clysm:compile-to-wasm '(set-difference '(1 2 3) '(2 3 4)))
      (format t "[PASS] (set-difference '(1 2 3) '(2 3 4))~%"))
  (error (e) (format t "[FAIL] (set-difference '(1 2 3) '(2 3 4)): ~a~%" e)))

;; Test subsetp
(handler-case
    (progn
      (clysm:compile-to-wasm '(subsetp '(1 2) '(1 2 3)))
      (format t "[PASS] (subsetp '(1 2) '(1 2 3))~%"))
  (error (e) (format t "[FAIL] (subsetp '(1 2) '(1 2 3)): ~a~%" e)))

;; Test adjoin
(handler-case
    (progn
      (clysm:compile-to-wasm '(adjoin 4 '(1 2 3)))
      (format t "[PASS] (adjoin 4 '(1 2 3))~%"))
  (error (e) (format t "[FAIL] (adjoin 4 '(1 2 3)): ~a~%" e)))

;; Test acons (with variable binding to avoid dotted pair literal issue)
(handler-case
    (progn
      (clysm:compile-to-wasm '(acons 'a 1 nil))
      (format t "[PASS] (acons 'a 1 nil)~%"))
  (error (e) (format t "[FAIL] (acons 'a 1 nil): ~a~%" e)))

;; Test pairlis
(handler-case
    (progn
      (clysm:compile-to-wasm '(pairlis '(a b c) '(1 2 3)))
      (format t "[PASS] (pairlis '(a b c) '(1 2 3))~%"))
  (error (e) (format t "[FAIL] (pairlis '(a b c) '(1 2 3)): ~a~%" e)))

;; Test copy-alist with nil (simplest case)
(handler-case
    (progn
      (clysm:compile-to-wasm '(copy-alist nil))
      (format t "[PASS] (copy-alist nil)~%"))
  (error (e) (format t "[FAIL] (copy-alist nil): ~a~%" e)))

;; Test member-if
(handler-case
    (progn
      (clysm:compile-to-wasm '(member-if #'evenp '(1 2 3 4 5)))
      (format t "[PASS] (member-if #'evenp '(1 2 3 4 5))~%"))
  (error (e) (format t "[FAIL] (member-if #'evenp '(1 2 3 4 5)): ~a~%" e)))

;; Test member-if-not
(handler-case
    (progn
      (clysm:compile-to-wasm '(member-if-not #'evenp '(1 2 3 4 5)))
      (format t "[PASS] (member-if-not #'evenp '(1 2 3 4 5))~%"))
  (error (e) (format t "[FAIL] (member-if-not #'evenp '(1 2 3 4 5)): ~a~%" e)))

;; Simple tests for spec verification examples
(format t "~%=== Spec SC-004 Verification Examples ===~%")
(handler-case
    (progn
      (clysm:compile-to-wasm '(intersection '(1 2 3) '(2 3 4)))
      (format t "[PASS] SC-004a: (intersection '(1 2 3) '(2 3 4))~%"))
  (error (e) (format t "[FAIL] SC-004a: ~a~%" e)))

(handler-case
    (progn
      (clysm:compile-to-wasm '(member 2 '(1 2 3)))
      (format t "[PASS] SC-004b: (member 2 '(1 2 3))~%"))
  (error (e) (format t "[FAIL] SC-004b: ~a~%" e)))

(format t "~%=== All tests completed ===~%")
