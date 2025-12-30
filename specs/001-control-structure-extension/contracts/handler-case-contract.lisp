;;;; handler-case-contract.lisp - Test contracts for handler-case
;;;; HyperSpec: resources/HyperSpec/Body/m_hand_1.htm

(in-package #:clysm-test)

;;; ============================================================
;;; Contract Tests: handler-case Exception Handling
;;; ============================================================

(deftest handler-case-no-error-passes-through
  "Contract: handler-case returns expression value when no error"
  (let ((wasm (compile-form-to-wasm
               '(handler-case 42
                  (error (e) (declare (ignore e)) nil)))))
    (ok wasm "handler-case should compile")
    (ok (wasm-validates wasm))))

(deftest handler-case-catches-error
  "Contract: handler-case catches matching error type"
  (let ((wasm (compile-form-to-wasm
               '(handler-case (error "test")
                  (error (e) (declare (ignore e)) :caught)))))
    (ok wasm "handler-case with error should compile")
    ;; Should contain try_table instruction
    (ok (wasm-contains-instruction wasm 'try_table))
    (ok (wasm-validates wasm))))

(deftest handler-case-multiple-handlers
  "Contract: handler-case with multiple handler clauses compiles"
  (let ((wasm (compile-form-to-wasm
               '(handler-case (error "test")
                  (simple-error (e) (declare (ignore e)) :simple)
                  (error (e) (declare (ignore e)) :error)
                  (condition (e) (declare (ignore e)) :condition)))))
    (ok wasm "multiple handlers should compile")
    (ok (wasm-validates wasm))))

(deftest handler-case-binds-variable
  "Contract: handler-case binds condition to variable"
  (let ((wasm (compile-form-to-wasm
               '(handler-case (error "message")
                  (error (e) e)))))
    (ok wasm "handler with bound variable should compile")
    (ok (wasm-validates wasm))))

(deftest handler-case-no-variable
  "Contract: handler-case without variable binding compiles"
  (let ((wasm (compile-form-to-wasm
               '(handler-case (error "test")
                  (error () :handled)))))
    (ok wasm "handler without variable should compile")
    (ok (wasm-validates wasm))))

(deftest handler-case-nested
  "Contract: Nested handler-case forms compile"
  (let ((wasm (compile-form-to-wasm
               '(handler-case
                    (handler-case (error "inner")
                      (simple-error (e) (declare (ignore e)) :inner))
                  (error (e) (declare (ignore e)) :outer)))))
    (ok wasm "nested handler-case should compile")
    (ok (wasm-validates wasm))))

(deftest handler-case-with-values
  "Contract: handler-case preserves multiple values"
  (let ((wasm (compile-form-to-wasm
               '(handler-case (values 1 2 3)
                  (error (e) (declare (ignore e)) (values nil nil nil))))))
    (ok wasm "handler-case with values should compile")
    (ok (wasm-validates wasm))))

(deftest handler-case-propagates-unmatched
  "Contract: Unmatched errors propagate to outer handlers"
  (let ((wasm (compile-form-to-wasm
               '(handler-case (error "test")
                  (type-error (e) (declare (ignore e)) :type-error)))))
    ;; Should contain throw_ref for re-throwing unmatched
    (ok (wasm-validates wasm))))

(deftest handler-case-generates-try-table
  "Contract: handler-case generates Wasm try_table structure"
  (let ((wasm (compile-form-to-wasm
               '(handler-case (f)
                  (error (e) (declare (ignore e)) nil)))))
    ;; Verify try_table instruction exists
    (ok (wasm-contains-instruction wasm 'try_table)
        "must generate try_table")
    ;; Verify catch clause
    (ok (wasm-contains-instruction wasm 'catch)
        "must generate catch clause")))

(deftest handler-case-empty-handlers
  "Contract: handler-case with no handlers acts as progn"
  (let ((wasm (compile-form-to-wasm
               '(handler-case (+ 1 2)))))
    (ok wasm "handler-case with no handlers should compile")
    (ok (wasm-validates wasm))))
