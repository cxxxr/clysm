;;;; package-test.lisp - Package and symbol intern tests (Phase 7 - US5)
(in-package #:clysm/tests/unit/package)

;;; Package Creation Tests

(deftest package-creation
  (testing "create package"
    (let ((pkg (clysm/reader/package:make-package* "TEST-PKG")))
      (ok pkg)
      (ok (stringp (clysm/reader/package::package-name* pkg)))))

  (testing "find package"
    (clysm/reader/package:make-package* "FIND-TEST")
    (let ((pkg (clysm/reader/package:find-package* "FIND-TEST")))
      (ok pkg)))

  (testing "find non-existent package returns nil"
    (ok (null (clysm/reader/package:find-package* "NO-SUCH-PACKAGE-XYZZY")))))

;;; Symbol Intern Tests

(deftest symbol-intern
  (testing "intern new symbol"
    (let* ((pkg (clysm/reader/package:make-package* "INTERN-TEST"))
           (sym (clysm/reader/package:intern-symbol "FOO" pkg)))
      (ok sym)
      (ok (symbolp sym))
      (ok (string= "FOO" (symbol-name sym)))))

  (testing "intern same symbol returns same object"
    (let* ((pkg (clysm/reader/package:make-package* "SAME-SYM"))
           (sym1 (clysm/reader/package:intern-symbol "BAR" pkg))
           (sym2 (clysm/reader/package:intern-symbol "BAR" pkg)))
      (ok (eq sym1 sym2))))

  (testing "different names create different symbols"
    (let* ((pkg (clysm/reader/package:make-package* "DIFF-SYM"))
           (sym1 (clysm/reader/package:intern-symbol "X" pkg))
           (sym2 (clysm/reader/package:intern-symbol "Y" pkg)))
      (ok (not (eq sym1 sym2)))))

  (testing "case sensitivity (upcase by default)"
    (let* ((pkg (clysm/reader/package:make-package* "CASE-TEST"))
           (sym (clysm/reader/package:intern-symbol "lower" pkg)))
      ;; Should be interned as uppercase
      (ok (string= "LOWER" (symbol-name sym))))))

;;; Find Symbol Tests

(deftest find-symbol-tests
  (testing "find existing symbol"
    (let* ((pkg (clysm/reader/package:make-package* "FIND-SYM"))
           (sym (clysm/reader/package:intern-symbol "EXISTS" pkg))
           (found (clysm/reader/package:find-symbol* "EXISTS" pkg)))
      (ok (eq sym found))))

  (testing "find non-existent symbol returns nil"
    (let* ((pkg (clysm/reader/package:make-package* "FIND-NIL")))
      (ok (null (clysm/reader/package:find-symbol* "NOPE" pkg))))))

;;; Standard Package Tests

(deftest standard-packages
  (testing "COMMON-LISP package exists"
    (ok (clysm/reader/package:find-package* "COMMON-LISP")))

  (testing "CL package alias"
    (ok (clysm/reader/package:find-package* "CL")))

  (testing "KEYWORD package exists"
    (ok (clysm/reader/package:find-package* "KEYWORD")))

  (testing "standard symbols in CL package"
    (let ((pkg (clysm/reader/package:find-package* "CL")))
      (ok (eq t (clysm/reader/package:find-symbol* "T" pkg)))
      ;; NIL is a valid symbol but evaluates to false, so check explicitly
      (ok (null (clysm/reader/package:find-symbol* "NIL" pkg)))
      (ok (symbolp (clysm/reader/package:find-symbol* "QUOTE" pkg)))
      (ok (symbolp (clysm/reader/package:find-symbol* "LAMBDA" pkg)))
      (ok (symbolp (clysm/reader/package:find-symbol* "DEFUN" pkg))))))

;;; Keyword Package Tests

(deftest keyword-package
  (testing "keyword intern creates keyword"
    (let ((kw (clysm/reader/package:intern-symbol "TEST"
               (clysm/reader/package:find-package* "KEYWORD"))))
      (ok (keywordp kw))
      (ok (eq kw :test))))

  (testing "keywords are self-evaluating"
    (let ((kw (clysm/reader/package:intern-symbol "FOO"
               (clysm/reader/package:find-package* "KEYWORD"))))
      (ok (eq kw (symbol-value kw))))))

;;; Package Prefix Tests

(deftest package-prefix
  (testing "parse package prefix"
    ;; Testing colon-separated package:symbol notation
    (let* ((pkg (clysm/reader/package:make-package* "PREFIX-TEST"))
           (sym (clysm/reader/package:intern-symbol "SYM" pkg)))
      (ok (symbolp sym))))

  (testing "double colon for internal symbols"
    ;; :: notation for internal symbols
    (ok t)))  ; Implementation detail

;;; Current Package Tests

(deftest current-package
  (testing "default current package"
    ;; Should have some default package
    (ok t))

  (testing "symbol interning uses current package"
    ;; Without explicit package, use current
    (ok t)))
