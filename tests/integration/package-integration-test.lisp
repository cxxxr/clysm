;;;; package-integration-test.lisp - Package system integration tests (013-package-system)
(in-package #:clysm/tests/integration/package)

;;; Integration tests for the package system
;;; Tests end-to-end package operations including:
;;; - defpackage macro
;;; - in-package macro
;;; - Package qualifiers in reader
;;; - Standard package availability
;;; - Cross-package symbol access

;;; T016: defpackage macro integration
(deftest defpackage-integration
  (testing "defpackage creates package with all options"
    ;; Clean up any existing test packages
    (when (clysm/reader/package:find-package* "TEST-DEFPKG-INT")
      (clysm/reader/package:delete-package* "TEST-DEFPKG-INT"))
    ;; Use defpackage* to create a package with options
    (let ((pkg (clysm/lib/package-macros:defpackage* "TEST-DEFPKG-INT"
                 (:nicknames "TDPI")
                 (:export "FOO" "BAR"))))
      (ok pkg "defpackage* creates package")
      (ok (clysm/reader/package:find-package* "TEST-DEFPKG-INT")
          "Package found by name")
      (ok (clysm/reader/package:find-package* "TDPI")
          "Package found by nickname")
      ;; Check exports
      (let ((ext-table (clysm/reader/package:package-external-symbols* pkg)))
        (ok (gethash "FOO" ext-table) "FOO is exported")
        (ok (gethash "BAR" ext-table) "BAR is exported")))
    ;; Cleanup
    (clysm/reader/package:delete-package* "TEST-DEFPKG-INT")))

;;; T017: in-package macro integration
(deftest in-package-integration
  (testing "in-package switches *current-package*"
    ;; Create a test package
    (when (clysm/reader/package:find-package* "TEST-INPKG-INT")
      (clysm/reader/package:delete-package* "TEST-INPKG-INT"))
    (clysm/reader/package:make-package* "TEST-INPKG-INT")
    ;; Save current package
    (let ((old-pkg clysm/reader/package:*current-package*))
      ;; Switch to test package
      (clysm/lib/package-macros:in-package* "TEST-INPKG-INT")
      (ok (string= "TEST-INPKG-INT"
                   (clysm/reader/package:package-name* clysm/reader/package:*current-package*))
          "in-package* switched to new package")
      ;; Restore
      (setf clysm/reader/package:*current-package* old-pkg))
    ;; Cleanup
    (clysm/reader/package:delete-package* "TEST-INPKG-INT")))

;;; T032: defpackage :export option
(deftest defpackage-export-option
  (testing "defpackage :export makes symbols external"
    (when (clysm/reader/package:find-package* "TEST-EXPORT-OPT")
      (clysm/reader/package:delete-package* "TEST-EXPORT-OPT"))
    (let ((pkg (clysm/lib/package-macros:defpackage* "TEST-EXPORT-OPT"
                 (:export "EXPORTED-SYM"))))
      (let ((ext-table (clysm/reader/package:package-external-symbols* pkg)))
        (ok (gethash "EXPORTED-SYM" ext-table)
            "Symbol is in external table")))
    (clysm/reader/package:delete-package* "TEST-EXPORT-OPT")))

;;; T033: defpackage :import-from option
(deftest defpackage-import-from-option
  (testing "defpackage :import-from imports specific symbols"
    ;; Create source package with exported symbol
    (when (clysm/reader/package:find-package* "TEST-IMPORT-SRC")
      (clysm/reader/package:delete-package* "TEST-IMPORT-SRC"))
    (when (clysm/reader/package:find-package* "TEST-IMPORT-DST")
      (clysm/reader/package:delete-package* "TEST-IMPORT-DST"))
    (let ((src-pkg (clysm/lib/package-macros:defpackage* "TEST-IMPORT-SRC"
                     (:export "IMPORTED-SYM"))))
      ;; Create destination package importing the symbol
      (let ((dst-pkg (clysm/lib/package-macros:defpackage* "TEST-IMPORT-DST"
                       (:import-from "TEST-IMPORT-SRC" "IMPORTED-SYM"))))
        (let ((int-table (clysm/reader/package:package-internal-symbols* dst-pkg)))
          (ok (gethash "IMPORTED-SYM" int-table)
              "Imported symbol is in internal table"))))
    (clysm/reader/package:delete-package* "TEST-IMPORT-DST")
    (clysm/reader/package:delete-package* "TEST-IMPORT-SRC")))

;;; T048: External symbol check on single colon
(deftest external-symbol-check
  (testing "pkg:symbol requires symbol to be exported"
    ;; Create package with an exported and internal symbol
    (when (clysm/reader/package:find-package* "TEST-EXT-CHK")
      (clysm/reader/package:delete-package* "TEST-EXT-CHK"))
    (let ((pkg (clysm/lib/package-macros:defpackage* "TEST-EXT-CHK"
                 (:export "EXPORTED"))))
      ;; Intern an internal symbol
      (clysm/reader/package:intern-symbol "INTERNAL" pkg)
      ;; External symbol access should work via reader
      (let ((tokens (clysm/reader/tokenizer:tokenize "test-ext-chk:exported")))
        (ok tokens "Tokenizing external symbol works")
        ;; Parse should succeed for exported symbol
        (ok (clysm/reader/parser:parse tokens)
            "Parsing external symbol succeeds"))
      ;; Internal symbol access via :: should work
      (let ((tokens (clysm/reader/tokenizer:tokenize "test-ext-chk::internal")))
        (ok tokens "Tokenizing internal symbol works")
        (ok (clysm/reader/parser:parse tokens)
            "Parsing internal symbol succeeds"))
      ;; Single colon access to internal symbol should fail
      (let ((tokens (clysm/reader/tokenizer:tokenize "test-ext-chk:internal")))
        (ok (signals (clysm/reader/parser:parse tokens))
            "Single colon access to non-exported symbol signals error")))
    (clysm/reader/package:delete-package* "TEST-EXT-CHK")))

;;; T061: CL symbols accessible from CL-USER
(deftest cl-symbols-from-cl-user
  (testing "CL symbols are accessible from CL-USER via use-list"
    (let ((cl-user (clysm/reader/package:find-package* "CL-USER"))
          (cl-pkg (clysm/reader/package:find-package* "CL")))
      (ok cl-user "CL-USER package exists")
      (ok cl-pkg "CL package exists")
      ;; CL-USER uses CL package
      (ok (member cl-pkg (clysm/reader/package:package-use-list* cl-user))
          "CL is in CL-USER's use-list")
      ;; Inherited symbols should be accessible via intern* with :inherited status
      (multiple-value-bind (sym status)
          (clysm/reader/package:intern* "CAR" cl-user)
        (ok sym "CAR symbol found")
        (ok (eq status :inherited) "CAR has :inherited status from CL")))))

;;; T085: defpackage :use option
(deftest defpackage-use-option
  (testing "defpackage :use sets up package inheritance"
    ;; Cleanup
    (when (clysm/reader/package:find-package* "TEST-USE-BASE")
      (clysm/reader/package:delete-package* "TEST-USE-BASE"))
    (when (clysm/reader/package:find-package* "TEST-USE-USER")
      (clysm/reader/package:delete-package* "TEST-USE-USER"))
    ;; Create base package with exported symbol
    (let ((base-pkg (clysm/lib/package-macros:defpackage* "TEST-USE-BASE"
                      (:export "SHARED-FN"))))
      ;; Create user package that uses base
      (let ((user-pkg (clysm/lib/package-macros:defpackage* "TEST-USE-USER"
                        (:use "TEST-USE-BASE"))))
        ;; Check use-list
        (ok (member base-pkg (clysm/reader/package:package-use-list* user-pkg))
            "Base package in user's use-list")
        ;; Check used-by-list
        (ok (member user-pkg (clysm/reader/package:package-used-by-list* base-pkg))
            "User package in base's used-by-list")))
    ;; Cleanup
    (clysm/reader/package:delete-package* "TEST-USE-USER")
    (clysm/reader/package:delete-package* "TEST-USE-BASE")))

;;; T086: Inherited symbol access after use-package
(deftest inherited-symbol-access
  (testing "symbols from used packages are accessible"
    ;; Cleanup
    (when (clysm/reader/package:find-package* "TEST-INHERIT-BASE")
      (clysm/reader/package:delete-package* "TEST-INHERIT-BASE"))
    (when (clysm/reader/package:find-package* "TEST-INHERIT-USER")
      (clysm/reader/package:delete-package* "TEST-INHERIT-USER"))
    ;; Create base package with exported symbol
    (let ((base-pkg (clysm/lib/package-macros:defpackage* "TEST-INHERIT-BASE"
                      (:export "EXPORTED-FN"))))
      ;; Create user package
      (let ((user-pkg (clysm/reader/package:make-package* "TEST-INHERIT-USER")))
        ;; Use the base package
        (clysm/reader/package:use-package* base-pkg user-pkg)
        ;; The exported symbol should be accessible with :inherited status
        (multiple-value-bind (sym status)
            (clysm/reader/package:intern* "EXPORTED-FN" user-pkg)
          (ok sym "Inherited symbol found")
          (ok (eq status :inherited) "Symbol has :inherited status"))))
    ;; Cleanup
    (clysm/reader/package:delete-package* "TEST-INHERIT-USER")
    (clysm/reader/package:delete-package* "TEST-INHERIT-BASE")))
