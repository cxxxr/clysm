;;; module-wasm-test.lisp - Contract tests for Wasm validation of compiler modules
;;;
;;; Tests compile each module, validate Wasm output with wasm-tools.
;;; T040-T046: Contract tests for different module categories
;;; T047-T054: Implementation tests

(defpackage :clysm-tests/contract/validation/module-wasm
  (:use :cl :rove)
  (:local-nicknames (:v :clysm-validation)))

(in-package :clysm-tests/contract/validation/module-wasm)

;;; Helper functions

(defun compile-and-validate-module (relative-path)
  "Compile a module and validate its Wasm output.
   Returns (values success-p wasm-valid-p error-message)."
  (let* ((base-dir (asdf:system-source-directory :clysm))
         (full-path (merge-pathnames relative-path base-dir))
         (module (v:make-module-info :path full-path
                                     :directory (make-pathname :directory (pathname-directory full-path))
                                     :dependencies nil
                                     :symbols-used nil)))
    (let ((result (v:compile-module module)))
      (if (v:compilation-result-success result)
          (let ((wasm-bytes (v:compilation-result-wasm-bytes result)))
            (if (and wasm-bytes (> (length wasm-bytes) 0))
                (multiple-value-bind (valid-p error)
                    (v:validate-wasm wasm-bytes)
                  (values t valid-p error))
                (values t nil "Empty Wasm bytes")))
          (values nil nil (v:compilation-result-error-message result))))))

(defun test-module-compilation (relative-path description)
  "Test that a module compiles and produces valid Wasm."
  (multiple-value-bind (compiled valid error)
      (compile-and-validate-module relative-path)
    (ok compiled (format nil "~A should compile" description))
    (when compiled
      (ok valid (format nil "~A should produce valid Wasm: ~A" description (or error ""))))))

;;; T040: Contract tests for leb128.lisp compilation

(deftest leb128-compiles-to-valid-wasm
  "Test that leb128.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/backend/leb128.lisp" "leb128.lisp"))

;;; T041: Contract tests for sections.lisp compilation

(deftest sections-compiles-to-valid-wasm
  "Test that sections.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/backend/sections.lisp" "sections.lisp"))

;;; T042: Contract tests for tokenizer.lisp compilation

(deftest tokenizer-compiles-to-valid-wasm
  "Test that tokenizer.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/reader/tokenizer.lisp" "tokenizer.lisp"))

;;; T043: Contract tests for parser.lisp compilation

(deftest parser-compiles-to-valid-wasm
  "Test that parser.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/reader/parser.lisp" "parser.lisp"))

;;; T044: Contract tests for ast.lisp compilation

(deftest ast-compiles-to-valid-wasm
  "Test that ast.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/compiler/ast.lisp" "ast.lisp"))

;;; T045: Contract tests for codegen/*.lisp compilation

(deftest codegen-gc-types-compiles-to-valid-wasm
  "Test that gc-types.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/compiler/codegen/gc-types.lisp" "gc-types.lisp"))

(deftest codegen-type-section-compiles-to-valid-wasm
  "Test that type-section.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/compiler/codegen/type-section.lisp" "type-section.lisp"))

(deftest codegen-func-section-compiles-to-valid-wasm
  "Test that func-section.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/compiler/codegen/func-section.lisp" "func-section.lisp"))

;;; T046: Contract tests for compiler.lisp compilation

(deftest compiler-compiles-to-valid-wasm
  "Test that compiler.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/compiler/compiler.lisp" "compiler.lisp"))

;;; T048: Contract tests for backend/ modules

(deftest wasm-emit-compiles-to-valid-wasm
  "Test that wasm-emit.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/backend/wasm-emit.lisp" "wasm-emit.lisp"))

(deftest wat-print-compiles-to-valid-wasm
  "Test that wat-print.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/backend/wat-print.lisp" "wat-print.lisp"))

;;; T049: Contract tests for reader/ modules

(deftest reader-package-compiles-to-valid-wasm
  "Test that reader/package.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/reader/package.lisp" "reader/package.lisp"))

(deftest reader-compiles-to-valid-wasm
  "Test that reader.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/reader/reader.lisp" "reader.lisp"))

;;; T050: Contract tests for compiler/ modules

(deftest env-compiles-to-valid-wasm
  "Test that env.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/compiler/env.lisp" "env.lisp"))

(deftest free-vars-compiles-to-valid-wasm
  "Test that free-vars.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/compiler/analyzer/free-vars.lisp" "free-vars.lisp"))

;;; T051: Contract tests for runtime/ modules

(deftest objects-compiles-to-valid-wasm
  "Test that objects.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/runtime/objects.lisp" "objects.lisp"))

(deftest special-vars-compiles-to-valid-wasm
  "Test that special-vars.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/runtime/special-vars.lisp" "special-vars.lisp"))

;;; T052: Contract tests for clos/ modules

(deftest mop-compiles-to-valid-wasm
  "Test that mop.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/clos/mop.lisp" "mop.lisp"))

(deftest defclass-compiles-to-valid-wasm
  "Test that defclass.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/clos/defclass.lisp" "defclass.lisp"))

;;; T053: Contract tests for conditions/ modules

(deftest conditions-types-compiles-to-valid-wasm
  "Test that conditions/types.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/conditions/types.lisp" "conditions/types.lisp"))

(deftest conditions-handlers-compiles-to-valid-wasm
  "Test that handlers.lisp compiles to valid Wasm"
  (test-module-compilation "src/clysm/conditions/handlers.lisp" "handlers.lisp"))

;;; T054: Test summary reporting

(deftest compilation-report-summary
  "Test that compilation report can be generated"
  (let ((modules (v:get-dependency-order)))
    (ok (> (length modules) 30) "Should have many modules to compile")
    ;; Try compiling first 3 modules as a sample
    (dolist (module (subseq modules 0 (min 3 (length modules))))
      (let ((result (v:compile-module module)))
        (ok (v:compilation-result-p result)
            (format nil "~A should return compilation-result"
                    (pathname-name (v:module-info-path module))))))))
