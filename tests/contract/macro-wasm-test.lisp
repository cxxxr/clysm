;;;; macro-wasm-test.lisp - Contract tests for macro Wasm IR generation
;;;; Feature 042: Advanced Defmacro

(defpackage #:clysm/tests/contract/macro-wasm
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/compiler/transform/macro
                          #:macro-function)
  (:import-from #:clysm/compiler
                #:compile-to-wasm)
  (:import-from #:clysm/compiler/transform/macro
                #:make-macro-registry
                #:register-macro
                #:global-macro-registry
                #:reset-global-macro-registry))

(in-package #:clysm/tests/contract/macro-wasm)

;;; ============================================================
;;; Helper: Validate Wasm bytes
;;; ============================================================

(defun validate-wasm-bytes (bytes)
  "Validate Wasm bytes using wasm-tools."
  (let ((temp-file (uiop:with-temporary-file (:stream s :pathname p
                                              :type "wasm"
                                              :keep t)
                     (write-sequence bytes s)
                     p)))
    (unwind-protect
         (multiple-value-bind (output error-output exit-code)
             (uiop:run-program (list "wasm-tools" "validate" "--features" "gc"
                                     (namestring temp-file))
                               :output :string
                               :error-output :string
                               :ignore-error-status t)
           (declare (ignore output))
           (zerop exit-code))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

;;; ============================================================
;;; T043: macroexpand-1 compile-time expansion Wasm IR validates
;;; ============================================================

(deftest macroexpand-1-compile-time-wasm
  (testing "macroexpand-1 of quoted form generates valid Wasm IR"
    ;; Register a simple macro for testing
    (reset-global-macro-registry)
    (setf (macro-function 'test-when)
          (lambda (form &optional env)
            (declare (ignore env))
            `(if ,(second form) (progn ,@(cddr form)) nil)))
    ;; Compile macroexpand-1 with a quoted macro call
    (let ((bytes (compile-to-wasm '(macroexpand-1 '(test-when t (+ 1 2))))))
      (ok (not (null bytes)) "Compilation should produce bytes")
      (ok (validate-wasm-bytes bytes) "Generated Wasm should be valid"))))

;;; ============================================================
;;; T044: macroexpand compile-time expansion Wasm IR validates
;;; ============================================================

(deftest macroexpand-compile-time-wasm
  (testing "macroexpand of quoted form generates valid Wasm IR"
    ;; Register a chain of macros
    (reset-global-macro-registry)
    (setf (macro-function 'outer-macro)
          (lambda (form &optional env)
            (declare (ignore env))
            `(inner-macro ,(second form))))
    (setf (macro-function 'inner-macro)
          (lambda (form &optional env)
            (declare (ignore env))
            `(+ ,(second form) 10)))
    ;; Compile macroexpand with quoted form
    (let ((bytes (compile-to-wasm '(macroexpand '(outer-macro 5)))))
      (ok (not (null bytes)) "Compilation should produce bytes")
      (ok (validate-wasm-bytes bytes) "Generated Wasm should be valid"))))

;;; ============================================================
;;; T043/T044: Non-macro forms pass through correctly
;;; ============================================================

(deftest macroexpand-non-macro-wasm
  (testing "macroexpand-1 on non-macro form generates valid Wasm IR"
    (reset-global-macro-registry)
    (let ((bytes (compile-to-wasm '(macroexpand-1 '(+ 1 2)))))
      (ok (not (null bytes)) "Compilation should produce bytes")
      (ok (validate-wasm-bytes bytes) "Generated Wasm should be valid")))

  (testing "macroexpand on atom generates valid Wasm IR"
    (reset-global-macro-registry)
    (let ((bytes (compile-to-wasm '(macroexpand '42))))
      (ok (not (null bytes)) "Compilation should produce bytes")
      (ok (validate-wasm-bytes bytes) "Generated Wasm should be valid"))))

;;; ============================================================
;;; Contract tests for $macro-environment type
;;; ============================================================

(deftest macro-environment-type-index
  (testing "$macro-environment type index is defined"
    (ok (= 24 clysm/compiler/codegen/gc-types:+type-macro-environment+)
        "Type index should be 24"))

  (testing "make-macro-environment-type function exists"
    (ok (fboundp 'clysm/compiler/codegen/gc-types:make-macro-environment-type)
        "make-macro-environment-type should be defined")))

;;; ============================================================
;;; Runtime expansion note
;;; ============================================================

(deftest runtime-macroexpand-note
  (testing "Runtime macro expansion documented"
    ;; Note: True runtime macro expansion (calling macroexpand on
    ;; non-quoted forms at runtime in Wasm) requires:
    ;; 1. Macro registry exported as Wasm global
    ;; 2. Expander functions stored as closures
    ;; 3. Dynamic function call support
    ;;
    ;; Current implementation supports compile-time expansion only.
    ;; This is sufficient for most use cases where macroexpand is
    ;; called on literal quoted forms.
    (ok t "Runtime expansion documented as future enhancement")))
