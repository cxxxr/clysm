(defpackage :clysm-tests/unit/validation/analyzer
  (:use :cl :rove)
  (:local-nicknames (:v :clysm-validation)))

(in-package :clysm-tests/unit/validation/analyzer)

;;; T007: Unit tests for Module struct

(deftest module-info-struct-creation
  "Test that module-info struct can be created with required fields"
  (let ((module (v:make-module-info :path #p"/src/clysm/backend/leb128.lisp"
                                    :directory #p"/src/clysm/backend/"
                                    :dependencies nil
                                    :symbols-used nil)))
    (ok (v:module-info-p module) "Should create a module-info struct")
    (ok (pathnamep (v:module-info-path module)) "Path should be a pathname")
    (ok (pathnamep (v:module-info-directory module)) "Directory should be a pathname")
    (ok (null (v:module-info-dependencies module)) "Dependencies should be nil initially")
    (ok (null (v:module-info-symbols-used module)) "Symbols-used should be nil initially")))

(deftest module-info-with-dependencies
  "Test module-info struct with dependencies"
  (let ((module (v:make-module-info :path #p"/src/clysm/backend/sections.lisp"
                                    :directory #p"/src/clysm/backend/"
                                    :dependencies '("leb128.lisp")
                                    :symbols-used nil)))
    (ok (equal (v:module-info-dependencies module) '("leb128.lisp"))
        "Should store dependencies list")))

(deftest module-info-with-symbols
  "Test module-info struct with symbols-used"
  (let ((module (v:make-module-info :path #p"/src/clysm/backend/leb128.lisp"
                                    :directory #p"/src/clysm/backend/"
                                    :dependencies nil
                                    :symbols-used '(defun let if + ash logand))))
    (ok (= (length (v:module-info-symbols-used module)) 6)
        "Should store all symbols")))

;;; T011: Unit tests for extract-symbols function

(deftest extract-symbols-from-atom
  "Test extracting symbols from atomic forms"
  (let ((syms (v:extract-symbols 'foo)))
    (ok (member 'foo syms) "Should extract single symbol"))
  (let ((syms (v:extract-symbols 42)))
    (ok (null syms) "Should return nil for numbers"))
  (let ((syms (v:extract-symbols "string")))
    (ok (null syms) "Should return nil for strings")))

(deftest extract-symbols-from-list
  "Test extracting symbols from list forms"
  (let ((syms (v:extract-symbols '(defun foo (x) (+ x 1)))))
    (ok (member 'defun syms) "Should extract defun")
    (ok (member 'foo syms) "Should extract function name")
    (ok (member 'x syms) "Should extract parameter")
    (ok (member '+ syms) "Should extract operator")))

(deftest extract-symbols-from-nested-list
  "Test extracting symbols from deeply nested forms"
  (let ((syms (v:extract-symbols '(let ((a 1)) (if (> a 0) (+ a 1) (- a 1))))))
    (ok (member 'let syms) "Should extract let")
    (ok (member 'a syms) "Should extract variable")
    (ok (member 'if syms) "Should extract if")
    (ok (member '> syms) "Should extract >")
    (ok (member '+ syms) "Should extract +")
    (ok (member '- syms) "Should extract -")))

(deftest extract-symbols-unique
  "Test that extract-symbols returns unique symbols"
  (let ((syms (v:extract-symbols '(+ 1 (+ 2 (+ 3 4))))))
    ;; + appears 3 times but should only appear once in result
    (ok (= (count '+ syms) 1) "Should return unique symbols")))

(deftest extract-symbols-filters-cl-package
  "Test that extract-symbols can filter to only CL package symbols"
  (let ((syms (v:extract-symbols '(defun my-func (x) (+ x 1)) :cl-only t)))
    (ok (member 'defun syms) "Should include CL symbols")
    (ok (member '+ syms) "Should include CL symbols")
    ;; my-func and x are not in CL package
    (ok (not (member 'my-func syms)) "Should exclude non-CL symbols")
    (ok (not (member 'x syms)) "Should exclude non-CL symbols")))

;;; T012: Unit tests for analyze-file function

(deftest analyze-file-returns-feature-usage-list
  "Test that analyze-file returns a list of feature-usage structs"
  (let* ((test-file (merge-pathnames
                     #p"tests/unit/validation/test-data/simple.lisp"
                     (asdf:system-source-directory :clysm)))
         (usages (v:analyze-file test-file)))
    (ok (listp usages) "Should return a list")
    (when usages
      (ok (v:feature-usage-p (first usages)) "Elements should be feature-usage structs"))))

;;; T013: Unit tests for analyze-directory function

(deftest analyze-directory-scans-lisp-files
  "Test that analyze-directory finds all .lisp files"
  (let* ((test-dir (merge-pathnames
                    #p"src/clysm/backend/"
                    (asdf:system-source-directory :clysm)))
         (result (v:analyze-directory test-dir)))
    (ok (hash-table-p result) "Should return a hash-table")
    ;; backend/ should have leb128.lisp, sections.lisp, wasm-emit.lisp, wat-print.lisp
    (ok (>= (hash-table-count result) 4) "Should find multiple files")))
