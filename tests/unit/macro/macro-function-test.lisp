;;;; macro-function-test.lisp - Tests for macro-function and runtime macroexpand
;;;; Feature 042: Advanced Defmacro

(defpackage #:clysm/tests/unit/macro/macro-function
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/compiler/transform/macro
                          #:macro-function
                          #:macroexpand-1
                          #:macroexpand)
  (:import-from #:clysm/compiler/transform/macro
                #:macroexpand-1*
                #:macroexpand*
                #:macro-expansion-depth-exceeded
                #:*macro-expansion-limit*
                #:make-macro-registry
                #:register-macro
                #:global-macro-registry
                #:reset-global-macro-registry
                #:make-macro-environment
                #:env-macro-function))

(in-package #:clysm/tests/unit/macro/macro-function)

;;; ============================================================
;;; T038: macro-function returns expander or nil
;;; ============================================================

(deftest macro-function-returns-expander
  (testing "macro-function returns expander function for defined macro"
    (reset-global-macro-registry)
    (setf (macro-function 'test-macro)
          (lambda (form &optional env)
            (declare (ignore env))
            (list 'expanded (second form))))
    (let ((fn (macro-function 'test-macro)))
      (ok (functionp fn))
      (ok (equal '(expanded hello) (funcall fn '(test-macro hello))))))

  (testing "macro-function returns nil for non-macro"
    (reset-global-macro-registry)
    (ok (null (macro-function 'undefined-macro)))))

;;; ============================================================
;;; T039: macroexpand-1 returns (form nil) for non-macro
;;; ============================================================

(deftest macroexpand-1-non-macro
  (testing "macroexpand-1 returns form unchanged with nil for non-macro"
    (reset-global-macro-registry)
    (multiple-value-bind (form expanded-p)
        (macroexpand-1 '(not-a-macro 1 2 3))
      (ok (equal '(not-a-macro 1 2 3) form))
      (ok (null expanded-p))))

  (testing "macroexpand-1 returns atom unchanged with nil"
    (reset-global-macro-registry)
    (multiple-value-bind (form expanded-p)
        (macroexpand-1 42)
      (ok (= 42 form))
      (ok (null expanded-p))))

  (testing "macroexpand-1 returns nil unchanged with nil"
    (reset-global-macro-registry)
    (multiple-value-bind (form expanded-p)
        (macroexpand-1 nil)
      (ok (null form))
      (ok (null expanded-p)))))

;;; ============================================================
;;; T040: macroexpand-1 returns (expanded t) for macro
;;; ============================================================

(deftest macroexpand-1-macro-call
  (testing "macroexpand-1 returns expanded form with t for macro call"
    (reset-global-macro-registry)
    (setf (macro-function 'my-wrap)
          (lambda (form &optional env)
            (declare (ignore env))
            (list 'wrapped (second form))))
    (multiple-value-bind (form expanded-p)
        (macroexpand-1 '(my-wrap value))
      (ok (equal '(wrapped value) form))
      (ok expanded-p))))

;;; ============================================================
;;; T041: macroexpand loops until non-macro
;;; ============================================================

(deftest macroexpand-full-expansion
  (testing "macroexpand repeatedly expands until result is not a macro call"
    (reset-global-macro-registry)
    ;; Define a chain of macros
    (setf (macro-function 'level-1)
          (lambda (form &optional env)
            (declare (ignore form env))
            '(level-2 x)))
    (setf (macro-function 'level-2)
          (lambda (form &optional env)
            (declare (ignore form env))
            '(level-3 y)))
    (setf (macro-function 'level-3)
          (lambda (form &optional env)
            (declare (ignore form env))
            '(done z)))
    (multiple-value-bind (form expanded-p)
        (macroexpand '(level-1))
      (ok (equal '(done z) form))
      (ok expanded-p)))

  (testing "macroexpand returns nil for ever-expanded when no expansion"
    (reset-global-macro-registry)
    (multiple-value-bind (form expanded-p)
        (macroexpand '(not-a-macro 1 2))
      (ok (equal '(not-a-macro 1 2) form))
      (ok (null expanded-p)))))

;;; ============================================================
;;; T042: macroexpand signals error after 1000 steps
;;; ============================================================

(deftest macroexpand-depth-limit
  (testing "macroexpand signals macro-expansion-depth-exceeded after limit"
    (let ((registry (make-macro-registry)))
      ;; Create an infinite expansion macro
      (register-macro registry 'infinite-loop
                      (lambda (form &optional env)
                        (declare (ignore form env))
                        (list 'infinite-loop)))
      (ok (signals (macroexpand* registry '(infinite-loop))
                   'macro-expansion-depth-exceeded)))))

;;; ============================================================
;;; Additional tests: macro-function with environment
;;; ============================================================

(deftest macro-function-with-environment
  (testing "macro-function with environment finds local macros"
    (reset-global-macro-registry)
    (let* ((local-registry (make-macro-registry))
           (env (make-macro-environment :local-macros local-registry)))
      ;; Register macro only in local environment
      (register-macro local-registry 'local-only
                      (lambda (form &optional e)
                        (declare (ignore e))
                        (list 'local-result (second form))))
      ;; Should find it through env
      (ok (not (null (macro-function 'local-only env))))
      ;; Should not find it without env
      (ok (null (macro-function 'local-only)))))

  (testing "macroexpand-1 uses environment for local macros"
    (reset-global-macro-registry)
    (let* ((local-registry (make-macro-registry))
           (env (make-macro-environment :local-macros local-registry)))
      (register-macro local-registry 'local-when
                      (lambda (form &optional e)
                        (declare (ignore e))
                        (list 'if (second form) (cons 'progn (cddr form)) nil)))
      ;; Expand with environment
      (multiple-value-bind (form expanded-p)
          (macroexpand-1 '(local-when t (do-something)) env)
        (ok expanded-p)
        (ok (eq 'if (first form)))))))
