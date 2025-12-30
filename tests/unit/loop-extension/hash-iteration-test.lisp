;;;; hash-iteration-test.lisp - LOOP hash-table iteration tests (001-loop-extension)
;;;; Reference: resources/HyperSpec/Body/m_loop.htm section 6.1.2.1.7
(in-package #:clysm/tests/unit/loop-extension/hash-iteration)

;;; ============================================================
;;; T009: Hash-keys basic iteration test
;;; ============================================================

(deftest hash-keys-basic-iteration
  (testing "FOR var BEING THE HASH-KEYS OF hash-table collects all keys"
    (let ((expander (clysm/lib/macros:make-loop-expander)))
      ;; Test that parsing succeeds
      (let ((expansion (funcall expander
                                '(loop for k being the hash-keys of ht collect k))))
        (ok expansion "Expansion should not be nil")
        (ok (listp expansion) "Expansion should be a list")))))

;;; ============================================================
;;; T010: Hash-values basic iteration test
;;; ============================================================

(deftest hash-values-basic-iteration
  (testing "FOR var BEING THE HASH-VALUES OF hash-table collects all values"
    (let ((expander (clysm/lib/macros:make-loop-expander)))
      (let ((expansion (funcall expander
                                '(loop for v being the hash-values of ht collect v))))
        (ok expansion "Expansion should not be nil")
        (ok (listp expansion) "Expansion should be a list")))))

;;; ============================================================
;;; T011: Hash-keys with USING (hash-value) test
;;; ============================================================

(deftest hash-keys-with-using-hash-value
  (testing "FOR k BEING THE HASH-KEYS OF ht USING (HASH-VALUE v) provides both"
    (let ((expander (clysm/lib/macros:make-loop-expander)))
      (let ((expansion (funcall expander
                                '(loop for k being the hash-keys of ht
                                       using (hash-value v)
                                       collect (cons k v)))))
        (ok expansion "Expansion should not be nil")
        (ok (listp expansion) "Expansion should be a list")
        ;; The expansion should contain reference to both k and v
        (ok (search "K" (format nil "~S" expansion))
            "Expansion should reference key variable K")
        (ok (search "V" (format nil "~S" expansion))
            "Expansion should reference value variable V")))))

;;; ============================================================
;;; T012: Hash-values with USING (hash-key) test
;;; ============================================================

(deftest hash-values-with-using-hash-key
  (testing "FOR v BEING THE HASH-VALUES OF ht USING (HASH-KEY k) provides both"
    (let ((expander (clysm/lib/macros:make-loop-expander)))
      (let ((expansion (funcall expander
                                '(loop for v being the hash-values of ht
                                       using (hash-key k)
                                       collect (cons k v)))))
        (ok expansion "Expansion should not be nil")
        (ok (listp expansion) "Expansion should be a list")))))

;;; ============================================================
;;; T013: Empty hash-table iteration test
;;; ============================================================

(deftest empty-hash-table-iteration
  (testing "Iteration over empty hash-table yields no iterations"
    (let ((expander (clysm/lib/macros:make-loop-expander)))
      ;; Just verify the expansion is valid - runtime will test actual behavior
      (let ((expansion (funcall expander
                                '(loop for k being the hash-keys of ht collect k))))
        (ok expansion "Expansion for empty hash-table form should succeed")))))

;;; ============================================================
;;; Runtime verification tests (require compiled expansion)
;;; ============================================================

(deftest hash-keys-runtime-collect
  (testing "Runtime: hash-keys iteration collects all keys"
    (let* ((ht (make-hash-table))
           (expander (clysm/lib/macros:make-loop-expander))
           (expansion (funcall expander
                               '(loop for k being the hash-keys of ht collect k))))
      ;; Set up test data
      (setf (gethash :a ht) 1)
      (setf (gethash :b ht) 2)
      (setf (gethash :c ht) 3)
      ;; Evaluate the expansion
      (let ((result (eval `(let ((ht ,ht)) ,expansion))))
        (ok (= 3 (length result)) "Should collect 3 keys")
        (ok (member :a result) "Should include :a")
        (ok (member :b result) "Should include :b")
        (ok (member :c result) "Should include :c")))))

(deftest hash-values-runtime-collect
  (testing "Runtime: hash-values iteration collects all values"
    (let* ((ht (make-hash-table))
           (expander (clysm/lib/macros:make-loop-expander))
           (expansion (funcall expander
                               '(loop for v being the hash-values of ht collect v))))
      (setf (gethash :a ht) 1)
      (setf (gethash :b ht) 2)
      (setf (gethash :c ht) 3)
      (let ((result (eval `(let ((ht ,ht)) ,expansion))))
        (ok (= 3 (length result)) "Should collect 3 values")
        (ok (member 1 result) "Should include 1")
        (ok (member 2 result) "Should include 2")
        (ok (member 3 result) "Should include 3")))))

(deftest hash-keys-using-runtime
  (testing "Runtime: hash-keys with USING collects key-value pairs"
    (let* ((ht (make-hash-table))
           (expander (clysm/lib/macros:make-loop-expander))
           (expansion (funcall expander
                               '(loop for k being the hash-keys of ht
                                      using (hash-value v)
                                      collect (cons k v)))))
      (setf (gethash :a ht) 1)
      (setf (gethash :b ht) 2)
      (let ((result (eval `(let ((ht ,ht)) ,expansion))))
        (ok (= 2 (length result)) "Should collect 2 pairs")
        (ok (member '(:a . 1) result :test #'equal) "Should include (:a . 1)")
        (ok (member '(:b . 2) result :test #'equal) "Should include (:b . 2)")))))

(deftest empty-hash-table-runtime
  (testing "Runtime: empty hash-table iteration returns nil"
    (let* ((ht (make-hash-table))
           (expander (clysm/lib/macros:make-loop-expander))
           (expansion (funcall expander
                               '(loop for k being the hash-keys of ht collect k))))
      (let ((result (eval `(let ((ht ,ht)) ,expansion))))
        (ok (null result) "Empty hash-table should return nil")))))
