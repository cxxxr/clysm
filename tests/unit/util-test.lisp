;;;; tests/unit/util-test.lisp - Tests for util.lisp

(in-package #:clysm/tests)

(defsuite :util)

;;; ============================================================
;;; Hash Table Utilities
;;; ============================================================

(deftest hash-table-keys-empty
  (let ((ht (make-hash-table)))
    (is-equal nil (hash-table-keys ht))))

(deftest hash-table-keys-basic
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    (let ((keys (hash-table-keys ht)))
      (is-eql 3 (length keys))
      (is (member :a keys))
      (is (member :b keys))
      (is (member :c keys)))))

(deftest hash-table-values-basic
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (let ((vals (hash-table-values ht)))
      (is-eql 2 (length vals))
      (is (member 1 vals))
      (is (member 2 vals)))))

;;; ============================================================
;;; List Utilities
;;; ============================================================

(deftest flatten-empty
  (is-equal nil (flatten nil)))

(deftest flatten-atom
  (is-equal '(1) (flatten 1)))

(deftest flatten-simple-list
  (is-equal '(1 2 3) (flatten '(1 2 3))))

(deftest flatten-nested
  (is-equal '(1 2 3 4 5) (flatten '((1 2) (3 (4 5))))))

(deftest flatten-deeply-nested
  (is-equal '(1 2 3) (flatten '(((1)) ((2) (3))))))

(deftest mappend-basic
  (is-equal '(1 2 3 4) (mappend #'list '(1 2 3 4))))

(deftest mappend-multiple
  (is-equal '(1 1 2 2 3 3)
            (mappend (lambda (x) (list x x)) '(1 2 3))))

(deftest lastcar-single
  (is-eql 1 (lastcar '(1))))

(deftest lastcar-multiple
  (is-eql 3 (lastcar '(1 2 3))))

(deftest ensure-list-nil
  (is-equal nil (ensure-list nil)))

(deftest ensure-list-atom
  (is-equal '(1) (ensure-list 1)))

(deftest ensure-list-already-list
  (is-equal '(1 2 3) (ensure-list '(1 2 3))))

(deftest alist-get-found
  (is-eql 2 (alist-get :b '((:a . 1) (:b . 2) (:c . 3)))))

(deftest alist-get-not-found
  (is-eq nil (alist-get :z '((:a . 1) (:b . 2)))))

(deftest alist-get-default
  (is-eql 99 (alist-get :z '((:a . 1)) :default 99)))

(deftest plist-get-found
  (is-eql 2 (plist-get '(:a 1 :b 2 :c 3) :b)))

(deftest plist-get-not-found
  (is-eq nil (plist-get '(:a 1 :b 2) :z)))

;;; ============================================================
;;; Sequence Utilities
;;; ============================================================

(deftest split-sequence-basic
  (is-equal '("a" "b" "c") (split-sequence #\, "a,b,c")))

(deftest split-sequence-no-delimiter
  (is-equal '("abc") (split-sequence #\, "abc")))

(deftest split-sequence-empty
  (is-equal '("") (split-sequence #\, "")))

;;; ============================================================
;;; Control Flow Macros
;;; ============================================================

(deftest if-let-true
  (is-eql 42
          (if-let (x (+ 1 1))
            (* x 21)
            0)))

(deftest if-let-false
  (is-eql 0
          (if-let (x nil)
            42
            0)))

(deftest when-let-true
  (let ((result nil))
    (when-let (x (list 1 2 3))
      (setf result (length x)))
    (is-eql 3 result)))

(deftest when-let-false
  (let ((result :unchanged))
    (when-let (x nil)
      (setf result :changed))
    (is-eq :unchanged result)))

(deftest with-gensyms-binding
  (let ((form (macroexpand-1 '(with-gensyms (a b) (list a b)))))
    (is (eq 'let (car form)))
    (is-eql 2 (length (cadr form)))))

;;; ============================================================
;;; Byte Utilities
;;; ============================================================

(deftest bytes-to-vector-basic
  (let ((vec (bytes-to-vector '(1 2 3))))
    (is (vectorp vec))
    (is-eql 3 (length vec))
    (is-eql 1 (aref vec 0))
    (is-eql 2 (aref vec 1))
    (is-eql 3 (aref vec 2))))

(deftest vector-to-bytes-basic
  (let ((bytes (vector-to-bytes #(1 2 3))))
    (is-equal '(1 2 3) bytes)))

(deftest concat-bytes-lists
  (let ((result (concat-bytes '(1 2) '(3 4))))
    (is (vectorp result))
    (is-eql 4 (length result))
    (is-equal '(1 2 3 4) (vector-to-bytes result))))

(deftest concat-bytes-mixed
  (let ((result (concat-bytes '(1 2) #(3 4) '(5))))
    (is-equal '(1 2 3 4 5) (vector-to-bytes result))))

;;; ============================================================
;;; String Utilities
;;; ============================================================

(deftest string-join-basic
  (is-equal "a,b,c" (string-join '("a" "b" "c") ",")))

(deftest string-join-empty-sep
  (is-equal "abc" (string-join '("a" "b" "c"))))

(deftest string-join-single
  (is-equal "hello" (string-join '("hello") ",")))

(deftest string-prefix-p-true
  (is (string-prefix-p "hello" "hello world")))

(deftest string-prefix-p-false
  (is-false (string-prefix-p "world" "hello world")))

(deftest string-prefix-p-exact
  (is (string-prefix-p "hello" "hello")))

(deftest string-suffix-p-true
  (is (string-suffix-p "world" "hello world")))

(deftest string-suffix-p-false
  (is-false (string-suffix-p "hello" "hello world")))
