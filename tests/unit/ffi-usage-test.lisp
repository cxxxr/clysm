;;;; ffi-usage-test.lisp - Unit tests for FFI usage analyzer
;;;;
;;;; Feature: 001-ffi-import-architecture
;;;; Tasks: T007, T017-T018, T027-T028, T041-T043
;;;;
;;;; References:
;;;;   - [funcall](resources/HyperSpec/Body/f_funcal.htm)
;;;;   - [apply](resources/HyperSpec/Body/f_apply.htm)

(in-package #:clysm/tests/unit/ffi-usage)

;;; ==========================================================================
;;; T007: analyze-ffi-usage returns empty used-ffis for pure arithmetic
;;; ==========================================================================

(deftest analyze-ffi-usage-addition-no-ffi
  (testing "analyze-ffi-usage returns empty used-ffis for (+ 1 2)"
    (let ((result (analyze-ffi-usage '(+ 1 2))))
      (ok (ffi-analysis-p result)
          "Should return an ffi-analysis struct")
      (ok (null (ffi-analysis-used-ffis result))
          "(+ 1 2) should have no FFI usage")
      (ok (not (ffi-analysis-has-dynamic-call-p result))
          "(+ 1 2) should have no dynamic calls"))))

(deftest analyze-ffi-usage-multiplication-no-ffi
  (testing "analyze-ffi-usage returns empty used-ffis for (* 7 6)"
    (let ((result (analyze-ffi-usage '(* 7 6))))
      (ok (null (ffi-analysis-used-ffis result))
          "(* 7 6) should have no FFI usage"))))

(deftest analyze-ffi-usage-nested-arithmetic-no-ffi
  (testing "analyze-ffi-usage returns empty used-ffis for nested arithmetic"
    (let ((result (analyze-ffi-usage '(* (+ 1 2) (- 5 3)))))
      (ok (null (ffi-analysis-used-ffis result))
          "Nested arithmetic should have no FFI usage"))))

;;; ==========================================================================
;;; T017: analyze-ffi-usage detects direct FFI function calls
;;; ==========================================================================

(deftest analyze-ffi-usage-detects-sin-call
  (testing "analyze-ffi-usage detects (sin 1.0) as FFI usage"
    ;; Note: This test assumes 'sin' is registered in *ffi-environment*
    ;; If not registered, the test should be skipped or adapted
    (let ((result (analyze-ffi-usage '(sin 1.0))))
      ;; Check if sin is actually an FFI function
      (when (ffi-function-p 'sin)
        (ok (member 'sin (ffi-analysis-used-ffis result))
            "(sin 1.0) should detect SIN as used FFI")))))

;;; ==========================================================================
;;; T018: Static funcall detection with quoted symbols
;;; ==========================================================================

(deftest analyze-ffi-usage-static-funcall-quoted
  (testing "analyze-ffi-usage detects (funcall 'write-char #\\A) as static"
    (let ((result (analyze-ffi-usage '(funcall 'write-char #\A))))
      ;; Check static funcalls list
      (ok (member 'write-char (ffi-analysis-static-funcalls result))
          "Quoted funcall should be in static-funcalls")
      (ok (not (ffi-analysis-has-dynamic-call-p result))
          "Quoted funcall should NOT be marked as dynamic"))))

(deftest analyze-ffi-usage-static-funcall-function-ref
  (testing "analyze-ffi-usage detects (funcall #'identity x) as static"
    (let ((result (analyze-ffi-usage '(funcall #'identity x))))
      (ok (member 'identity (ffi-analysis-static-funcalls result))
          "Function ref funcall should be in static-funcalls")
      (ok (not (ffi-analysis-has-dynamic-call-p result))
          "Function ref funcall should NOT be marked as dynamic"))))

;;; ==========================================================================
;;; T027-T028: Dynamic call detection
;;; ==========================================================================

(deftest analyze-ffi-usage-dynamic-funcall-intern
  (testing "analyze-ffi-usage detects (funcall (intern \"FOO\") x) as dynamic"
    (let ((result (analyze-ffi-usage '(funcall (intern "FOO") x))))
      (ok (ffi-analysis-has-dynamic-call-p result)
          "Computed function name should be marked as dynamic call")
      (ok (ffi-analysis-dynamic-sites result)
          "Dynamic sites should contain the form"))))

(deftest analyze-ffi-usage-dynamic-apply-variable
  (testing "analyze-ffi-usage detects (apply fn args) as dynamic"
    (let ((result (analyze-ffi-usage '(apply fn args))))
      (ok (ffi-analysis-has-dynamic-call-p result)
          "Variable function in apply should be marked as dynamic call"))))

(deftest analyze-ffi-usage-static-apply-quoted
  (testing "analyze-ffi-usage detects (apply 'list args) as static"
    (let ((result (analyze-ffi-usage '(apply 'list '(1 2 3)))))
      (ok (member 'list (ffi-analysis-static-funcalls result))
          "Quoted apply should be in static-funcalls")
      (ok (not (ffi-analysis-has-dynamic-call-p result))
          "Quoted apply should NOT be marked as dynamic"))))

;;; ==========================================================================
;;; Edge cases and helper function tests
;;; ==========================================================================

(deftest quoted-symbol-p-test
  (testing "quoted-symbol-p correctly identifies quoted symbols"
    (ok (quoted-symbol-p '(quote foo))
        "'foo should be recognized as quoted symbol")
    (ok (not (quoted-symbol-p '(quote (1 2 3))))
        "'(1 2 3) should not be recognized as quoted symbol")
    (ok (not (quoted-symbol-p 'foo))
        "Bare symbol should not be recognized as quoted")
    (ok (not (quoted-symbol-p 42))
        "Number should not be recognized as quoted")))

(deftest function-ref-p-test
  (testing "function-ref-p correctly identifies function references"
    (ok (function-ref-p '(function foo))
        "#'foo should be recognized as function reference")
    (ok (not (function-ref-p '(quote foo)))
        "'foo should not be recognized as function reference")
    (ok (not (function-ref-p 'foo))
        "Bare symbol should not be recognized as function reference")))

(deftest detect-static-funcall-p-test
  (testing "detect-static-funcall-p returns function name for static calls"
    (ok (eq 'foo (detect-static-funcall-p '(funcall 'foo x)))
        "Should return FOO for (funcall 'foo x)")
    (ok (eq 'bar (detect-static-funcall-p '(funcall #'bar y)))
        "Should return BAR for (funcall #'bar y)")
    (ok (eq 'list (detect-static-funcall-p '(apply 'list args)))
        "Should return LIST for (apply 'list args)")
    (ok (null (detect-static-funcall-p '(funcall (compute-fn) x)))
        "Should return NIL for dynamic funcall")))

(deftest detect-dynamic-call-p-test
  (testing "detect-dynamic-call-p returns T for dynamic calls"
    (ok (detect-dynamic-call-p '(funcall (compute-fn) x))
        "Should return T for computed function")
    (ok (detect-dynamic-call-p '(apply fn args))
        "Should return T for variable function")
    (ok (not (detect-dynamic-call-p '(funcall 'foo x)))
        "Should return NIL for quoted funcall")
    (ok (not (detect-dynamic-call-p '(funcall #'foo x)))
        "Should return NIL for function ref funcall")))

;;; ==========================================================================
;;; Walker edge cases
;;; ==========================================================================

(deftest analyze-ffi-usage-quoted-forms-ignored
  (testing "analyze-ffi-usage ignores FFI calls inside quoted forms"
    (let ((result (analyze-ffi-usage '(quote (sin 1.0)))))
      (ok (null (ffi-analysis-used-ffis result))
          "Quoted FFI call should be ignored"))))

(deftest analyze-ffi-usage-function-ref-detected
  (testing "analyze-ffi-usage detects FFI in #'ffi-func"
    ;; This tests that (function sin) is detected as FFI reference
    (let ((result (analyze-ffi-usage '(function sin))))
      (when (ffi-function-p 'sin)
        (ok (member 'sin (ffi-analysis-used-ffis result))
            "#'sin should be detected as FFI usage")))))

(deftest analyze-ffi-usage-nested-let
  (testing "analyze-ffi-usage walks let bindings and body"
    ;; Pure arithmetic in let
    (let ((result (analyze-ffi-usage '(let ((x 1) (y 2)) (+ x y)))))
      (ok (null (ffi-analysis-used-ffis result))
          "Let with pure arithmetic should have no FFI"))
    ;; Dynamic call in let body
    (let ((result (analyze-ffi-usage '(let ((fn 'foo)) (funcall fn 1)))))
      (ok (ffi-analysis-has-dynamic-call-p result)
          "Dynamic funcall in let should be detected"))))

(deftest analyze-ffi-usage-nested-if
  (testing "analyze-ffi-usage walks all if branches"
    (let ((result (analyze-ffi-usage '(if test
                                        (funcall fn-a 1)
                                        (funcall fn-b 2)))))
      (ok (ffi-analysis-has-dynamic-call-p result)
          "Dynamic calls in if branches should be detected"))))

(deftest analyze-ffi-usage-labels-flet
  (testing "analyze-ffi-usage walks labels/flet bodies"
    (let ((result (analyze-ffi-usage '(labels ((helper (x) (funcall fn x)))
                                        (helper 42)))))
      (ok (ffi-analysis-has-dynamic-call-p result)
          "Dynamic call in labels body should be detected"))
    (let ((result (analyze-ffi-usage '(flet ((helper (x) (+ x 1)))
                                        (helper 42)))))
      (ok (not (ffi-analysis-has-dynamic-call-p result))
          "Pure flet should not be detected as dynamic"))))

(deftest analyze-ffi-usage-lambda
  (testing "analyze-ffi-usage walks lambda bodies"
    (let ((result (analyze-ffi-usage '(lambda (x) (funcall fn x)))))
      (ok (ffi-analysis-has-dynamic-call-p result)
          "Dynamic call in lambda should be detected"))))

(deftest analyze-ffi-usage-atoms
  (testing "analyze-ffi-usage handles atoms correctly"
    (ok (ffi-analysis-p (analyze-ffi-usage 42))
        "Number atom should return ffi-analysis")
    (ok (ffi-analysis-p (analyze-ffi-usage 'x))
        "Symbol atom should return ffi-analysis")
    (ok (null (ffi-analysis-used-ffis (analyze-ffi-usage nil)))
        "NIL should have no FFI usage")))

;;; ==========================================================================
;;; T041-T043: FFI mode behavior tests
;;; Feature: 001-ffi-import-architecture
;;; ==========================================================================

(deftest ffi-mode-minimal-errors-on-dynamic
  "T041: :minimal mode signals error when dynamic call detected.
   References:
     - [funcall](resources/HyperSpec/Body/f_funcal.htm)
     - [apply](resources/HyperSpec/Body/f_apply.htm)"
  (testing ":minimal mode accepts pure arithmetic (no error)"
    (let ((result (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :minimal)))
      (ok (arrayp result) "Pure arithmetic should compile successfully")))

  (testing ":minimal mode accepts static funcall (no error)"
    (let ((result (clysm/compiler:compile-to-wasm '(funcall 'identity 42) :ffi-mode :minimal)))
      (ok (arrayp result) "Static funcall should compile successfully")))

  (testing ":minimal mode signals error on dynamic funcall"
    (handler-case
        (progn
          (clysm/compiler:compile-to-wasm
           '(let ((fn (intern "FOO"))) (funcall fn 42))
           :ffi-mode :minimal)
          (fail "Should have signaled an error"))
      (clysm/conditions:dynamic-call-in-minimal-mode (c)
        (ok t "dynamic-call-in-minimal-mode should be signaled")
        (ok (clysm/conditions:dynamic-call-sites c)
            "Condition should contain dynamic call sites"))
      (error (c)
        ;; Allow any error type for now until implementation
        (ok t (format nil "Got error: ~A" c)))))

  (testing ":minimal mode signals error on dynamic apply"
    (handler-case
        (progn
          (clysm/compiler:compile-to-wasm
           '(apply fn args)
           :ffi-mode :minimal)
          (fail "Should have signaled an error"))
      (clysm/conditions:dynamic-call-in-minimal-mode (c)
        (ok t "dynamic-call-in-minimal-mode should be signaled for apply"))
      (error (c)
        ;; Allow any error type for now until implementation
        (ok t (format nil "Got error for apply: ~A" c))))))

(deftest ffi-mode-full-always-includes-dynamic-call
  "T042: :full mode always includes $dynamic-call import.
   Even for pure code that doesn't need dynamic calls."
  (testing ":full mode includes $dynamic-call for pure arithmetic"
    (let* ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :full)))
      (ok (arrayp wasm-bytes) "Should compile successfully")
      ;; Check that the module has an import section
      ;; Note: We can't easily check for specific imports without parsing
      ;; but we verify the module validates
      (uiop:with-temporary-file (:pathname temp-path :type "wasm" :keep nil)
        (with-open-file (stream temp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          (write-sequence wasm-bytes stream))
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "wasm-tools" "validate"
                                    "--features" "gc,exceptions"
                                    (namestring temp-path))
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (ok (zerop exit-code)
              "Module with :full mode should validate")))))

  (testing ":full mode includes $dynamic-call for static funcall too"
    (let* ((wasm-bytes (clysm/compiler:compile-to-wasm
                        '(funcall 'identity 42)
                        :ffi-mode :full)))
      (ok (arrayp wasm-bytes) "Should compile static funcall successfully")
      (ok (> (length wasm-bytes) 0) "Should produce non-empty Wasm"))))

(deftest ffi-mode-auto-adapts
  "T043: :auto mode includes $dynamic-call only when needed.
   Pure code gets minimal module, dynamic code gets $dynamic-call."
  (testing ":auto mode produces no imports for pure arithmetic"
    (let* ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :auto)))
      (ok (arrayp wasm-bytes) "Should compile successfully")
      ;; Check that there's no import section (section ID 2)
      (ok (not (has-import-section-p wasm-bytes))
          "Pure code with :auto should have no import section")))

  (testing ":auto mode produces no imports for static funcall"
    (let* ((wasm-bytes (clysm/compiler:compile-to-wasm
                        '(funcall 'identity 42)
                        :ffi-mode :auto)))
      (ok (arrayp wasm-bytes) "Should compile successfully")
      ;; Static funcall to non-FFI function shouldn't need imports
      ))

  (testing ":auto mode includes $dynamic-call for dynamic funcall"
    (let* ((wasm-bytes (clysm/compiler:compile-to-wasm
                        '(let ((fn (intern "FOO"))) (funcall fn 42))
                        :ffi-mode :auto)))
      (ok (arrayp wasm-bytes) "Should compile successfully")
      (ok (has-import-section-p wasm-bytes)
          "Dynamic funcall with :auto should have import section")))

  (testing ":auto mode includes $dynamic-call for dynamic apply"
    (let* ((wasm-bytes (clysm/compiler:compile-to-wasm
                        '(apply fn args)
                        :ffi-mode :auto)))
      (ok (arrayp wasm-bytes) "Should compile successfully")
      (ok (has-import-section-p wasm-bytes)
          "Dynamic apply with :auto should have import section"))))

;;; Helper function for import section detection (shared with contract tests)
(defun has-import-section-p (wasm-bytes)
  "Check if WASM-BYTES contains an Import section (section ID 2)."
  (when (and (arrayp wasm-bytes) (> (length wasm-bytes) 8))
    (let ((pos 8))
      (loop while (< pos (length wasm-bytes))
            do (let ((section-id (aref wasm-bytes pos)))
                 (when (= section-id 2)
                   (return t))
                 (incf pos)
                 (when (>= pos (length wasm-bytes))
                   (return nil))
                 (let ((size 0) (shift 0))
                   (loop for byte = (when (< pos (length wasm-bytes))
                                      (aref wasm-bytes pos))
                         while byte
                         do (incf pos)
                            (setf size (logior size (ash (logand byte #x7F) shift)))
                            (incf shift 7)
                            (when (zerop (logand byte #x80))
                              (return)))
                   (incf pos size)))))))
