;;;; environment-test.lisp - Tests for &environment macro lambda-list parameter
;;;; Feature 042: Advanced Defmacro

(defpackage #:clysm/tests/unit/macro/environment
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler/transform/macro
                #:parse-macro-lambda-list
                #:macro-lambda-list-info
                #:macro-lambda-list-info-env-var
                #:macro-lambda-list-info-required
                #:macro-lambda-list-malformed
                #:make-macro-environment
                #:macro-environment-p
                #:macro-environment-local-macros
                #:macro-environment-parent
                #:env-macro-function
                #:extend-environment
                #:make-macro-registry
                #:register-macro
                #:parse-defmacro
                #:compile-defmacro))

(in-package #:clysm/tests/unit/macro/environment)

;;; ============================================================
;;; T024: &environment extracts env variable
;;; ============================================================

(deftest environment-extracts-variable
  (testing "&environment extracts environment variable name"
    (let ((info (parse-macro-lambda-list '(x &environment env y))))
      (ok (eq 'env (macro-lambda-list-info-env-var info)))
      (ok (equal '(x y) (macro-lambda-list-info-required info)))))

  (testing "&environment only parameter"
    (let ((info (parse-macro-lambda-list '(&environment env))))
      (ok (eq 'env (macro-lambda-list-info-env-var info)))
      (ok (null (macro-lambda-list-info-required info))))))

;;; ============================================================
;;; T025: &environment can appear anywhere in lambda-list
;;; ============================================================

(deftest environment-position-flexible
  (testing "&environment at beginning"
    (let ((info (parse-macro-lambda-list '(&environment env x y))))
      (ok (eq 'env (macro-lambda-list-info-env-var info)))
      (ok (equal '(x y) (macro-lambda-list-info-required info)))))

  (testing "&environment in middle"
    (let ((info (parse-macro-lambda-list '(x &environment env y))))
      (ok (eq 'env (macro-lambda-list-info-env-var info)))
      (ok (equal '(x y) (macro-lambda-list-info-required info)))))

  (testing "&environment at end"
    (let ((info (parse-macro-lambda-list '(x y &environment env))))
      (ok (eq 'env (macro-lambda-list-info-env-var info)))
      (ok (equal '(x y) (macro-lambda-list-info-required info)))))

  (testing "&environment after &optional"
    (let ((info (parse-macro-lambda-list '(x &optional y &environment env))))
      (ok (eq 'env (macro-lambda-list-info-env-var info))))))

;;; ============================================================
;;; T026: env-macro-function finds local macros
;;; ============================================================

(deftest env-macro-function-local-lookup
  (testing "env-macro-function finds local macros"
    (let* ((registry (make-macro-registry))
           (env (make-macro-environment :local-macros registry)))
      (register-macro registry 'test-local-macro
                      (lambda (form &optional e)
                        (declare (ignore e))
                        (list 'found (second form))))
      (ok (not (null (env-macro-function env 'test-local-macro))))))

  (testing "env-macro-function returns nil for undefined macro"
    (let ((env (make-macro-environment)))
      (ok (null (env-macro-function env 'no-such-macro))))))

;;; ============================================================
;;; T027: env-macro-function searches parent chain
;;; ============================================================

(deftest env-macro-function-parent-chain
  (testing "env-macro-function searches parent environment chain"
    (let* ((parent-registry (make-macro-registry))
           (parent-env (make-macro-environment :local-macros parent-registry))
           (child-env (make-macro-environment :parent parent-env)))
      ;; Register macro in parent
      (register-macro parent-registry 'parent-macro
                      (lambda (form &optional e)
                        (declare (ignore e form))
                        'from-parent))
      ;; Child should find it through parent chain
      (ok (not (null (env-macro-function child-env 'parent-macro))))))

  (testing "local macros shadow parent macros"
    (let* ((parent-registry (make-macro-registry))
           (parent-env (make-macro-environment :local-macros parent-registry))
           (child-registry (make-macro-registry))
           (child-env (make-macro-environment :local-macros child-registry
                                               :parent parent-env)))
      ;; Register same-named macro in both
      (register-macro parent-registry 'shadowed-macro
                      (lambda (form &optional e)
                        (declare (ignore e form))
                        'from-parent))
      (register-macro child-registry 'shadowed-macro
                      (lambda (form &optional e)
                        (declare (ignore e form))
                        'from-child))
      ;; Child should find its own
      (let ((expander (env-macro-function child-env 'shadowed-macro)))
        (ok (eq 'from-child (funcall expander nil)))))))

;;; ============================================================
;;; T028: make-macro-environment creates valid struct
;;; ============================================================

(deftest make-environment-struct
  (testing "make-macro-environment creates valid struct"
    (let ((env (make-macro-environment)))
      (ok (macro-environment-p env))
      (ok (null (macro-environment-local-macros env)))
      (ok (null (macro-environment-parent env)))))

  (testing "make-macro-environment with local-macros"
    (let* ((registry (make-macro-registry))
           (env (make-macro-environment :local-macros registry)))
      (ok (macro-environment-p env))
      (ok (eq registry (macro-environment-local-macros env))))))

;;; ============================================================
;;; T029: extend-environment creates child with parent link
;;; ============================================================

(deftest extend-environment-parent-link
  (testing "extend-environment creates child with parent link"
    (let* ((parent-env (make-macro-environment))
           (child-env (extend-environment parent-env)))
      (ok (macro-environment-p child-env))
      (ok (eq parent-env (macro-environment-parent child-env)))))

  (testing "extend-environment with local macros"
    (let* ((parent-env (make-macro-environment))
           (registry (make-macro-registry))
           (child-env (extend-environment parent-env registry)))
      (ok (eq registry (macro-environment-local-macros child-env)))
      (ok (eq parent-env (macro-environment-parent child-env))))))

;;; ============================================================
;;; Additional tests: &environment binding in macro expansion
;;; ============================================================

(deftest environment-binding-in-macro
  (testing "defmacro with &environment receives environment"
    (let* ((result (parse-defmacro
                    '(defmacro env-aware-macro (&environment env form)
                      ;; Just check env is bound (non-nil or nil is fine)
                      (if env
                          (list 'quote (list 'with-env form))
                          (list 'quote (list 'without-env form))))))
           (expander (compile-defmacro result)))
      ;; Call with nil environment
      (let ((expansion (funcall expander '(env-aware-macro some-form) nil)))
        (ok (consp expansion)))
      ;; Call with an environment
      (let* ((env (make-macro-environment))
             (expansion (funcall expander '(env-aware-macro some-form) env)))
        (ok (consp expansion))))))
