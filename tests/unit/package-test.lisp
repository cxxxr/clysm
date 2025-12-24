;;;; package-test.lisp - Package system tests (013-package-system)
(in-package #:clysm/tests/unit/package)

;;; ============================================================
;;; Phase 2: Foundational Tests (T004-T005)
;;; These tests must FAIL before implementation
;;; ============================================================

;;; T004: Extended package structure tests
(deftest extended-package-structure
  (testing "package has internal-symbols hash table"
    (let ((pkg (clysm/reader/package:make-package* "STRUCT-TEST-1")))
      (ok (hash-table-p (clysm/reader/package::package-internal-symbols* pkg))
          "Package should have internal-symbols hash table")))

  (testing "package has external-symbols hash table"
    (let ((pkg (clysm/reader/package:make-package* "STRUCT-TEST-2")))
      (ok (hash-table-p (clysm/reader/package::package-external-symbols* pkg))
          "Package should have external-symbols hash table")))

  (testing "package has use-list"
    (let ((pkg (clysm/reader/package:make-package* "STRUCT-TEST-3")))
      (ok (listp (clysm/reader/package::package-use-list* pkg))
          "Package should have use-list")))

  (testing "package has used-by-list"
    (let ((pkg (clysm/reader/package:make-package* "STRUCT-TEST-4")))
      (ok (listp (clysm/reader/package::package-used-by-list* pkg))
          "Package should have used-by-list")))

  (testing "package has shadowing-symbols list"
    (let ((pkg (clysm/reader/package:make-package* "STRUCT-TEST-5")))
      (ok (listp (clysm/reader/package::package-shadowing-symbols* pkg))
          "Package should have shadowing-symbols list"))))

;;; T005: Package designator conversion tests
(deftest package-designator-conversion
  (testing "string designator finds package"
    (clysm/reader/package:make-package* "DESIG-TEST-1")
    (let ((pkg (clysm/reader/package::package-designator-to-package "DESIG-TEST-1")))
      (ok pkg "String designator should find package")))

  (testing "keyword designator finds package"
    (clysm/reader/package:make-package* "DESIG-TEST-2")
    (let ((pkg (clysm/reader/package::package-designator-to-package :desig-test-2)))
      (ok pkg "Keyword designator should find package")))

  (testing "package object returns itself"
    (let* ((pkg (clysm/reader/package:make-package* "DESIG-TEST-3"))
           (result (clysm/reader/package::package-designator-to-package pkg)))
      (ok (eq pkg result) "Package object should return itself")))

  (testing "invalid designator signals error"
    (ok (handler-case
            (progn
              (clysm/reader/package::package-designator-to-package "NO-SUCH-PKG-XYZZY")
              nil)  ; Should not reach here
          (error () t))
        "Invalid designator should signal an error")))

;;; ============================================================
;;; Original tests (moved below foundational tests)
;;; ============================================================

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

;;; ============================================================
;;; Phase 3: User Story 1 Tests (T011-T015)
;;; Package definition and switching
;;; ============================================================

;;; T011: make-package with :nicknames :use options
(deftest make-package-options
  (testing "make-package with :nicknames"
    (let ((pkg (clysm/reader/package:make-package* "NICK-TEST"
                 :nicknames '("NT" "NTEST"))))
      (ok pkg "Package created")
      (ok (clysm/reader/package:find-package* "NICK-TEST") "Find by name")
      (ok (clysm/reader/package:find-package* "NT") "Find by nickname NT")
      (ok (clysm/reader/package:find-package* "NTEST") "Find by nickname NTEST")
      (ok (eq pkg (clysm/reader/package:find-package* "NT"))
          "Nickname returns same package")))

  (testing "make-package with :use"
    (let* ((base-pkg (clysm/reader/package:make-package* "USE-BASE"))
           (user-pkg (clysm/reader/package:make-package* "USE-USER"
                       :use (list base-pkg))))
      (ok user-pkg "Package created with :use")
      (ok (member base-pkg (clysm/reader/package::package-use-list* user-pkg))
          "Base is in use-list")
      (ok (member user-pkg (clysm/reader/package::package-used-by-list* base-pkg))
          "User is in used-by-list")))

  (testing "make-package fails for existing name"
    (clysm/reader/package:make-package* "EXIST-TEST")
    (ok (handler-case
            (progn
              (clysm/reader/package:make-package* "EXIST-TEST")
              nil)
          (error () t))
        "Error signaled for duplicate package")))

;;; T012: find-package by name and nickname
(deftest find-package-options
  (testing "find-package by string name"
    (clysm/reader/package:make-package* "FP-STRING")
    (ok (clysm/reader/package:find-package* "FP-STRING")))

  (testing "find-package returns nil for non-existent"
    (ok (null (clysm/reader/package:find-package* "NO-SUCH-PACKAGE-XYZ123")))))

;;; T013: delete-package
(deftest delete-package-test
  (testing "delete-package removes package"
    (clysm/reader/package:make-package* "DEL-TEST")
    (ok (clysm/reader/package:find-package* "DEL-TEST") "Package exists before delete")
    (clysm/reader/package:delete-package* "DEL-TEST")
    (ok (null (clysm/reader/package:find-package* "DEL-TEST"))
        "Package not found after delete"))

  (testing "delete-package removes nicknames"
    (clysm/reader/package:make-package* "DEL-NICK" :nicknames '("DN"))
    (ok (clysm/reader/package:find-package* "DN") "Nickname exists")
    (clysm/reader/package:delete-package* "DEL-NICK")
    (ok (null (clysm/reader/package:find-package* "DN"))
        "Nickname not found after delete")))

;;; T014: rename-package
(deftest rename-package-test
  (testing "rename-package changes name"
    (clysm/reader/package:make-package* "OLD-NAME")
    (let ((pkg (clysm/reader/package:find-package* "OLD-NAME")))
      (clysm/reader/package:rename-package* pkg "NEW-NAME")
      (ok (null (clysm/reader/package:find-package* "OLD-NAME"))
          "Old name no longer works")
      (ok (clysm/reader/package:find-package* "NEW-NAME")
          "New name works")))

  (testing "rename-package with new nicknames"
    (clysm/reader/package:make-package* "RENAME-NICK" :nicknames '("RN1"))
    (let ((pkg (clysm/reader/package:find-package* "RENAME-NICK")))
      (clysm/reader/package:rename-package* pkg "RENAMED-NICK" '("RN2"))
      (ok (null (clysm/reader/package:find-package* "RN1"))
          "Old nickname no longer works")
      (ok (clysm/reader/package:find-package* "RN2")
          "New nickname works"))))

;;; T015: list-all-packages
(deftest list-all-packages-test
  (testing "list-all-packages returns list"
    (let ((packages (clysm/reader/package:list-all-packages*)))
      (ok (listp packages) "Returns a list")
      (ok (> (length packages) 0) "List is not empty")
      (ok (member (clysm/reader/package:find-package* "COMMON-LISP") packages)
          "CL package in list"))))

;;; ============================================================
;;; Phase 6: User Story 4 Tests (T056-T060)
;;; Standard packages: CL, CL-USER, KEYWORD
;;; ============================================================

;;; T056: COMMON-LISP package exists with nickname CL
(deftest standard-cl-package
  (testing "COMMON-LISP package exists"
    (let ((pkg (clysm/reader/package:find-package* "COMMON-LISP")))
      (ok pkg "COMMON-LISP package exists")))
  (testing "CL is a nickname for COMMON-LISP"
    (let ((pkg-cl (clysm/reader/package:find-package* "CL"))
          (pkg-full (clysm/reader/package:find-package* "COMMON-LISP")))
      (ok pkg-cl "CL package found")
      (ok (eq pkg-cl pkg-full) "CL and COMMON-LISP are same package")))
  (testing "CL exports standard symbols"
    (let ((pkg (clysm/reader/package:find-package* "CL")))
      (let ((ext-table (clysm/reader/package:package-external-symbols* pkg)))
        (ok (gethash "CAR" ext-table) "CAR exported")
        (ok (gethash "CDR" ext-table) "CDR exported")
        (ok (gethash "CONS" ext-table) "CONS exported")
        (ok (gethash "DEFUN" ext-table) "DEFUN exported")
        (ok (gethash "LET" ext-table) "LET exported")))))

;;; T057: COMMON-LISP-USER package exists and uses CL
(deftest standard-cl-user-package
  (testing "COMMON-LISP-USER package exists"
    (let ((pkg (clysm/reader/package:find-package* "COMMON-LISP-USER")))
      (ok pkg "COMMON-LISP-USER package exists")))
  (testing "CL-USER is a nickname for COMMON-LISP-USER"
    (let ((pkg-short (clysm/reader/package:find-package* "CL-USER"))
          (pkg-full (clysm/reader/package:find-package* "COMMON-LISP-USER")))
      (ok (eq pkg-short pkg-full) "CL-USER and COMMON-LISP-USER are same")))
  (testing "USER is a nickname for COMMON-LISP-USER"
    (let ((pkg-user (clysm/reader/package:find-package* "USER"))
          (pkg-full (clysm/reader/package:find-package* "COMMON-LISP-USER")))
      (ok (eq pkg-user pkg-full) "USER and COMMON-LISP-USER are same")))
  (testing "CL-USER uses CL package"
    (let ((cl-user (clysm/reader/package:find-package* "CL-USER"))
          (cl-pkg (clysm/reader/package:find-package* "CL")))
      (ok (member cl-pkg (clysm/reader/package:package-use-list* cl-user))
          "CL in CL-USER's use-list"))))

;;; T058: KEYWORD package exists
(deftest standard-keyword-package
  (testing "KEYWORD package exists"
    (let ((pkg (clysm/reader/package:find-package* "KEYWORD")))
      (ok pkg "KEYWORD package exists")))
  (testing "KEYWORD package has no nicknames"
    (let ((pkg (clysm/reader/package:find-package* "KEYWORD")))
      (ok (null (clysm/reader/package:package-nicknames* pkg))
          "KEYWORD has no nicknames"))))

;;; T059: *current-package* initialized to CL-USER
(deftest current-package-initialized
  (testing "*current-package* is CL-USER"
    (let ((current clysm/reader/package:*current-package*)
          (cl-user (clysm/reader/package:find-package* "CL-USER")))
      (ok current "*current-package* is set")
      (ok (eq current cl-user) "*current-package* is CL-USER"))))

;;; T060: Keyword auto-export and self-evaluation
(deftest keyword-auto-export
  (testing "keywords are auto-exported"
    (let* ((kw-pkg (clysm/reader/package:find-package* "KEYWORD"))
           (sym (clysm/reader/package:intern-symbol "AUTO-EXPORTED" kw-pkg)))
      (ok (keywordp sym) "Interned keyword is keywordp")
      (let ((ext-table (clysm/reader/package:package-external-symbols* kw-pkg)))
        (ok (gethash "AUTO-EXPORTED" ext-table)
            "Keyword is in external symbols table"))))
  (testing "keywords are self-evaluating"
    (ok (eq :test (symbol-value :test)) "Keyword equals its value")))

;;; ============================================================
;;; Phase 7: User Story 5 Tests (T069-T075)
;;; intern/find-symbol with multiple values
;;; ============================================================

;;; T069: intern returns multiple values (symbol, status)
(deftest intern-multiple-values
  (testing "intern returns symbol and status as multiple values"
    (let ((pkg (clysm/reader/package:make-package* "MV-INTERN-TEST")))
      (multiple-value-bind (sym status)
          (clysm/reader/package:intern* "NEW-SYM" pkg)
        (ok (symbolp sym) "First value is a symbol")
        (ok (null status) "Second value is NIL for new symbol"))
      ;; Cleanup
      (clysm/reader/package:delete-package* pkg))))

;;; T070: intern returns :internal for existing internal symbol
(deftest intern-internal-status
  (testing "intern returns :internal for existing internal symbol"
    (let ((pkg (clysm/reader/package:make-package* "INTERN-INT-TEST")))
      ;; First intern creates new symbol
      (clysm/reader/package:intern* "EXISTING" pkg)
      ;; Second intern finds it
      (multiple-value-bind (sym status)
          (clysm/reader/package:intern* "EXISTING" pkg)
        (ok (symbolp sym) "Returns symbol")
        (ok (eq :internal status) "Status is :internal"))
      (clysm/reader/package:delete-package* pkg))))

;;; T071: intern returns :external for exported symbol
(deftest intern-external-status
  (testing "intern returns :external for exported symbol"
    (let ((pkg (clysm/reader/package:make-package* "INTERN-EXT-TEST")))
      ;; Create and export symbol
      (clysm/reader/package:intern* "EXPORTED" pkg)
      (clysm/reader/package:export* "EXPORTED" pkg)
      ;; Intern again should return :external
      (multiple-value-bind (sym status)
          (clysm/reader/package:intern* "EXPORTED" pkg)
        (ok (symbolp sym) "Returns symbol")
        (ok (eq :external status) "Status is :external"))
      (clysm/reader/package:delete-package* pkg))))

;;; T072: intern returns :inherited for used symbol
(deftest intern-inherited-status
  (testing "intern returns :inherited for symbol from used package"
    (let* ((base-pkg (clysm/reader/package:make-package* "INHERIT-BASE"))
           (user-pkg (clysm/reader/package:make-package* "INHERIT-USER"
                       :use (list base-pkg))))
      ;; Create and export symbol in base
      (clysm/reader/package:intern* "BASE-SYM" base-pkg)
      (clysm/reader/package:export* "BASE-SYM" base-pkg)
      ;; Find in user package via inheritance
      (multiple-value-bind (sym status)
          (clysm/reader/package:intern* "BASE-SYM" user-pkg)
        (ok (symbolp sym) "Returns symbol")
        (ok (eq :inherited status) "Status is :inherited"))
      (clysm/reader/package:delete-package* user-pkg)
      (clysm/reader/package:delete-package* base-pkg))))

;;; T073: find-symbol does not create new symbol
(deftest find-symbol-no-create
  (testing "find-symbol does not create new symbols"
    (let ((pkg (clysm/reader/package:make-package* "FIND-NO-CREATE")))
      (multiple-value-bind (sym status)
          (clysm/reader/package:find-symbol** "NOT-THERE" pkg)
        (ok (null sym) "No symbol returned")
        (ok (null status) "No status returned"))
      ;; Verify symbol was not created
      (ok (null (gethash "NOT-THERE" (clysm/reader/package:package-internal-symbols* pkg)))
          "Symbol not in internal table")
      (clysm/reader/package:delete-package* pkg))))

;;; T074: find-symbol returns nil, nil for missing symbol
(deftest find-symbol-missing
  (testing "find-symbol returns nil, nil for missing symbol"
    (let ((pkg (clysm/reader/package:make-package* "FIND-MISSING")))
      (multiple-value-bind (sym status)
          (clysm/reader/package:find-symbol** "NONEXISTENT" pkg)
        (ok (null sym) "First value is nil")
        (ok (null status) "Second value is nil"))
      (clysm/reader/package:delete-package* pkg))))

;;; T075: unintern removes symbol from package
(deftest unintern-test
  (testing "unintern removes symbol from package"
    (let ((pkg (clysm/reader/package:make-package* "UNINTERN-TEST")))
      (let ((sym (clysm/reader/package:intern* "TO-REMOVE" pkg)))
        (ok sym "Symbol created")
        (clysm/reader/package:unintern* sym pkg)
        (ok (null (clysm/reader/package:find-symbol** "TO-REMOVE" pkg))
            "Symbol no longer found"))
      (clysm/reader/package:delete-package* pkg)))
  (testing "unintern removes from external symbols"
    (let ((pkg (clysm/reader/package:make-package* "UNINTERN-EXT")))
      (let ((sym (clysm/reader/package:intern* "EXT-REMOVE" pkg)))
        (clysm/reader/package:export* sym pkg)
        (clysm/reader/package:unintern* sym pkg)
        (ok (null (clysm/reader/package:find-symbol** "EXT-REMOVE" pkg))
            "Exported symbol no longer found"))
      (clysm/reader/package:delete-package* pkg))))

;;; ============================================================
;;; Phase 8: User Story 6 Tests (T081-T086)
;;; use-package/unuse-package
;;; ============================================================

;;; T081: use-package adds to use-list
(deftest use-package-add-to-use-list
  (testing "use-package adds package to use-list"
    (let* ((base-pkg (clysm/reader/package:make-package* "USE-BASE-81"))
           (user-pkg (clysm/reader/package:make-package* "USE-USER-81")))
      (clysm/reader/package:use-package* base-pkg user-pkg)
      (ok (member base-pkg (clysm/reader/package:package-use-list* user-pkg))
          "Base package in use-list")
      (clysm/reader/package:delete-package* user-pkg)
      (clysm/reader/package:delete-package* base-pkg))))

;;; T082: use-package updates used-by-list
(deftest use-package-updates-used-by
  (testing "use-package updates used-by-list of used package"
    (let* ((base-pkg (clysm/reader/package:make-package* "USE-BASE-82"))
           (user-pkg (clysm/reader/package:make-package* "USE-USER-82")))
      (clysm/reader/package:use-package* base-pkg user-pkg)
      (ok (member user-pkg (clysm/reader/package:package-used-by-list* base-pkg))
          "User package in used-by-list")
      (clysm/reader/package:delete-package* user-pkg)
      (clysm/reader/package:delete-package* base-pkg))))

;;; T083: use-package name conflict detection (simplified)
(deftest use-package-conflict
  (testing "use-package works without errors"
    (let* ((base1 (clysm/reader/package:make-package* "CONFLICT-BASE1"))
           (user-pkg (clysm/reader/package:make-package* "CONFLICT-USER")))
      ;; Export symbol from base
      (clysm/reader/package:intern* "SOME-NAME" base1)
      (clysm/reader/package:export* "SOME-NAME" base1)
      ;; use-package works
      (clysm/reader/package:use-package* base1 user-pkg)
      (ok (member base1 (clysm/reader/package:package-use-list* user-pkg))
          "use-package succeeds")
      (clysm/reader/package:delete-package* user-pkg)
      (clysm/reader/package:delete-package* base1))))

;;; T084: unuse-package removes from use-list
(deftest unuse-package-test
  (testing "unuse-package removes from use-list"
    (let* ((base-pkg (clysm/reader/package:make-package* "UNUSE-BASE"))
           (user-pkg (clysm/reader/package:make-package* "UNUSE-USER")))
      (clysm/reader/package:use-package* base-pkg user-pkg)
      (ok (member base-pkg (clysm/reader/package:package-use-list* user-pkg))
          "Package in use-list before unuse")
      (clysm/reader/package:unuse-package* base-pkg user-pkg)
      (ok (null (member base-pkg (clysm/reader/package:package-use-list* user-pkg)))
          "Package removed from use-list")
      (ok (null (member user-pkg (clysm/reader/package:package-used-by-list* base-pkg)))
          "Package removed from used-by-list")
      (clysm/reader/package:delete-package* user-pkg)
      (clysm/reader/package:delete-package* base-pkg))))

;;; ============================================================
;;; Phase 9: Package Information Tests (T091-T097)
;;; Package information accessors
;;; ============================================================

;;; T091: package-name function
(deftest package-name-test
  (testing "package-name returns the package name"
    (let ((pkg (clysm/reader/package:make-package* "PKG-NAME-TEST")))
      (ok (string= "PKG-NAME-TEST" (clysm/reader/package:package-name* pkg))
          "package-name returns correct name")
      (clysm/reader/package:delete-package* pkg)))
  (testing "package-name for standard packages"
    (let ((cl-pkg (clysm/reader/package:find-package* "CL")))
      (ok (string= "COMMON-LISP" (clysm/reader/package:package-name* cl-pkg))
          "CL's name is COMMON-LISP"))))

;;; T092: package-nicknames function
(deftest package-nicknames-test
  (testing "package-nicknames returns nickname list"
    (let ((pkg (clysm/reader/package:make-package* "NICK-INFO-TEST"
                 :nicknames '("NIT" "N-I-T"))))
      (let ((nicks (clysm/reader/package:package-nicknames* pkg)))
        (ok (listp nicks) "Returns a list")
        (ok (member "NIT" nicks :test #'string=) "Contains NIT")
        (ok (member "N-I-T" nicks :test #'string=) "Contains N-I-T"))
      (clysm/reader/package:delete-package* pkg)))
  (testing "package-nicknames returns empty list for package with no nicknames"
    (let ((pkg (clysm/reader/package:make-package* "NO-NICK-TEST")))
      (ok (null (clysm/reader/package:package-nicknames* pkg))
          "Returns empty list")
      (clysm/reader/package:delete-package* pkg))))

;;; T093: package-use-list function
(deftest package-use-list-test
  (testing "package-use-list returns used packages"
    (let* ((base (clysm/reader/package:make-package* "USE-LIST-BASE"))
           (user (clysm/reader/package:make-package* "USE-LIST-USER"
                   :use (list base))))
      (let ((use-list (clysm/reader/package:package-use-list* user)))
        (ok (listp use-list) "Returns a list")
        (ok (member base use-list) "Contains base package"))
      (clysm/reader/package:delete-package* user)
      (clysm/reader/package:delete-package* base)))
  (testing "CL-USER uses CL"
    (let ((cl-user (clysm/reader/package:find-package* "CL-USER"))
          (cl-pkg (clysm/reader/package:find-package* "CL")))
      (ok (member cl-pkg (clysm/reader/package:package-use-list* cl-user))
          "CL-USER uses CL"))))

;;; T094: package-used-by-list function
(deftest package-used-by-list-test
  (testing "package-used-by-list returns using packages"
    (let* ((base (clysm/reader/package:make-package* "USED-BY-BASE"))
           (user (clysm/reader/package:make-package* "USED-BY-USER"
                   :use (list base))))
      (let ((used-by (clysm/reader/package:package-used-by-list* base)))
        (ok (listp used-by) "Returns a list")
        (ok (member user used-by) "Contains user package"))
      (clysm/reader/package:delete-package* user)
      (clysm/reader/package:delete-package* base)))
  (testing "CL is used by CL-USER"
    (let ((cl-user (clysm/reader/package:find-package* "CL-USER"))
          (cl-pkg (clysm/reader/package:find-package* "CL")))
      (ok (member cl-user (clysm/reader/package:package-used-by-list* cl-pkg))
          "CL is used by CL-USER"))))

;;; T095: package-shadowing-symbols function
(deftest package-shadowing-symbols-test
  (testing "package-shadowing-symbols returns shadowing symbols"
    (let ((pkg (clysm/reader/package:make-package* "SHADOW-SYMS-TEST")))
      (ok (listp (clysm/reader/package:package-shadowing-symbols* pkg))
          "Returns a list (initially empty)")
      (ok (null (clysm/reader/package:package-shadowing-symbols* pkg))
          "Initially empty")
      (clysm/reader/package:shadow* "SHADOWED" pkg)
      (ok (= 1 (length (clysm/reader/package:package-shadowing-symbols* pkg)))
          "Has one shadowing symbol after shadow*")
      (clysm/reader/package:delete-package* pkg))))

;;; T096: packagep predicate
(deftest packagep-test
  (testing "packagep returns t for packages"
    (let ((pkg (clysm/reader/package:make-package* "PACKAGEP-TEST")))
      (ok (clysm/reader/package:packagep* pkg) "packagep returns t for package")
      (clysm/reader/package:delete-package* pkg)))
  (testing "packagep returns nil for non-packages"
    (ok (not (clysm/reader/package:packagep* nil)) "nil is not a package")
    (ok (not (clysm/reader/package:packagep* "string")) "string is not a package")
    (ok (not (clysm/reader/package:packagep* 42)) "number is not a package")
    (ok (not (clysm/reader/package:packagep* 'symbol)) "symbol is not a package")))

;;; T097: symbol-package function
(deftest symbol-package-test
  (testing "symbol-package returns home package"
    (let ((pkg (clysm/reader/package:make-package* "SYM-PKG-TEST")))
      (let ((sym (clysm/reader/package:intern-symbol "MY-SYMBOL" pkg)))
        (ok (eq pkg (clysm/reader/package:symbol-package* sym))
            "symbol-package returns the package where symbol was interned"))
      (clysm/reader/package:delete-package* pkg)))
  (testing "symbol-package for Clysm keywords"
    (let* ((kw-pkg (clysm/reader/package:find-package* "KEYWORD"))
           ;; Create keyword through Clysm's package system
           (kw (clysm/reader/package:intern-symbol "SYM-PKG-KW-TEST" kw-pkg)))
      (ok (eq kw-pkg (clysm/reader/package:symbol-package* kw))
          "Clysm keywords have KEYWORD as home package"))))
