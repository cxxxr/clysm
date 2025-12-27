;;;; whole-test.lisp - Tests for &whole macro lambda-list parameter
;;;; Feature 042: Advanced Defmacro

(defpackage #:clysm/tests/unit/macro/whole
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler/transform/macro
                #:parse-macro-lambda-list
                #:macro-lambda-list-info
                #:macro-lambda-list-info-whole-var
                #:macro-lambda-list-info-required
                #:macro-lambda-list-info-env-var
                #:macro-lambda-list-malformed
                #:compile-defmacro
                #:parse-defmacro
                #:defmacro-result-lambda-info
                #:make-macro-registry
                #:register-macro
                #:macroexpand-1*))

(in-package #:clysm/tests/unit/macro/whole)

;;; ============================================================
;;; T014: &whole as first element extracts variable
;;; ============================================================

(deftest whole-first-element
  (testing "&whole as first element extracts variable name"
    (let ((info (parse-macro-lambda-list '(&whole form x y))))
      (ok (eq 'form (macro-lambda-list-info-whole-var info)))
      (ok (equal '(x y) (macro-lambda-list-info-required info)))))

  (testing "&whole with no other params"
    (let ((info (parse-macro-lambda-list '(&whole form))))
      (ok (eq 'form (macro-lambda-list-info-whole-var info)))
      (ok (null (macro-lambda-list-info-required info))))))

;;; ============================================================
;;; T015: &whole followed by required params works
;;; ============================================================

(deftest whole-with-required-params
  (testing "&whole followed by required params works correctly"
    (let ((info (parse-macro-lambda-list '(&whole form a b c))))
      (ok (eq 'form (macro-lambda-list-info-whole-var info)))
      (ok (equal '(a b c) (macro-lambda-list-info-required info)))))

  (testing "defmacro with &whole binds correctly"
    (let* ((result (parse-defmacro
                    '(defmacro test-whole (&whole form x)
                      (list 'quote form))))
           (expander (compile-defmacro result)))
      ;; The expander receives the complete form
      (let ((expansion (funcall expander '(test-whole hello))))
        (ok (equal ''(test-whole hello) expansion))))))

;;; ============================================================
;;; T016: &whole with destructuring params works
;;; ============================================================

(deftest whole-with-destructuring
  (testing "&whole with &body works correctly"
    (let ((info (parse-macro-lambda-list '(&whole form name &body body))))
      (ok (eq 'form (macro-lambda-list-info-whole-var info)))
      (ok (equal '(name) (macro-lambda-list-info-required info)))))

  (testing "&whole with &optional works"
    (let ((info (parse-macro-lambda-list '(&whole form x &optional y))))
      (ok (eq 'form (macro-lambda-list-info-whole-var info)))
      (ok (equal '(x) (macro-lambda-list-info-required info))))))

;;; ============================================================
;;; T017: &whole not first signals error
;;; ============================================================

(deftest whole-not-first-error
  (testing "&whole not as first element signals macro-lambda-list-malformed"
    (ok (signals (parse-macro-lambda-list '(x &whole form y))
                 'macro-lambda-list-malformed)))

  (testing "&whole after &optional signals error"
    (ok (signals (parse-macro-lambda-list '(x &optional y &whole form))
                 'macro-lambda-list-malformed))))

;;; ============================================================
;;; T018: &whole without following variable signals error
;;; ============================================================

(deftest whole-without-variable-error
  (testing "&whole at end with no variable signals error"
    (ok (signals (parse-macro-lambda-list '(&whole))
                 'macro-lambda-list-malformed)))

  (testing "&whole followed by &optional signals error"
    (ok (signals (parse-macro-lambda-list '(&whole &optional x))
                 'macro-lambda-list-malformed))))

;;; ============================================================
;;; T069-T071: Error reporting tests (US5 in whole-test.lisp per tasks.md)
;;; ============================================================

(deftest expansion-error-includes-form
  (testing "argument count mismatch includes form in error"
    ;; Define a macro that requires exactly 2 arguments
    (let* ((result (parse-defmacro
                    '(defmacro needs-two (a b)
                      (list 'list a b))))
           (expander (compile-defmacro result)))
      ;; Call with wrong number of arguments
      (handler-case
          (progn
            (funcall expander '(needs-two only-one))
            (fail "Should have signaled an error"))
        (error (e)
          ;; Error should mention the form (case-insensitive search)
          (ok (search "NEEDS-TWO" (princ-to-string e))))))))

(deftest argument-count-mismatch-error
  (testing "argument count mismatch reports expected vs actual"
    (let* ((result (parse-defmacro
                    '(defmacro needs-three (a b c)
                      (list a b c))))
           (expander (compile-defmacro result)))
      (handler-case
          (progn
            (funcall expander '(needs-three one))
            (fail "Should have signaled an error"))
        (error (e)
          (let ((msg (princ-to-string e)))
            ;; Should indicate the macro name (uppercase in CL)
            (ok (search "NEEDS-THREE" msg))))))))

(deftest invalid-argument-type-error
  (testing "macro expansion preserves &whole form for error reporting"
    (let* ((result (parse-defmacro
                    '(defmacro check-type-macro (&whole form x)
                      (unless (symbolp x)
                        (error "Expected symbol in ~S, got ~S" form x))
                      (list 'quote x))))
           (expander (compile-defmacro result)))
      ;; Valid call
      (ok (equal ''hello (funcall expander '(check-type-macro hello))))
      ;; Error should include whole form (uppercase in CL)
      (handler-case
          (progn
            (funcall expander '(check-type-macro 123))
            (fail "Should have signaled an error"))
        (error (e)
          (let ((msg (princ-to-string e)))
            (ok (search "CHECK-TYPE-MACRO" msg))))))))
