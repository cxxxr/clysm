;;;; test-intern.lisp - Unit tests for intern* FFI stub
;;;; Task: T026 [US3]
;;;;
;;;; TDD: Tests written FIRST before implementation.
;;;; These tests must FAIL initially (T028).

(in-package #:clysm/tests)

(deftest test-intern*-basic ()
  "Test that intern* creates/finds a symbol in a package."
  (let ((result (clysm::intern* "TEST-INTERN-SYMBOL" "CL-USER")))
    (ok (not (null result))
        "intern* should return a symbol")
    (ok (symbolp result)
        "intern* should return a symbol type")))

(deftest test-intern*-existing-symbol ()
  "Test that intern* returns existing symbol if already interned."
  ;; First intern creates/finds
  (let ((sym1 (clysm::intern* "TEST-EXISTING-SYM" "CL-USER"))
        (sym2 (clysm::intern* "TEST-EXISTING-SYM" "CL-USER")))
    (ok (eq sym1 sym2)
        "intern* should return same symbol for same name/package")))

(deftest test-intern*-default-package ()
  "Test that intern* uses *package* when package not specified."
  (let* ((pkg-name (package-name *package*))
         (result (clysm::intern* "TEST-DEFAULT-PKG-SYM")))
    (ok (symbolp result)
        "intern* with default package should return symbol")
    (ok (eq (symbol-package result) *package*)
        "Symbol should be in current *package*")))

(deftest test-intern*-keyword-package ()
  "Test that intern* in KEYWORD package creates keyword symbol."
  (let ((result (clysm::intern* "TEST-KEYWORD" "KEYWORD")))
    (ok (keywordp result)
        "Symbol in KEYWORD package should be a keyword")))

(deftest test-intern*-compilation ()
  "Test that a defun using intern* compiles to Wasm."
  (let ((form '(defun test-intern () (clysm::intern* "FOO" "CL-USER"))))
    (let ((result (compile-form-to-wasm form)))
      (ok result
          "Form using intern* should compile")
      (ok (search "host" (wasm-output-imports result))
          "Compiled output should include host FFI import"))))
