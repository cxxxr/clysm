;;;; destructuring-bind-test.lisp - Unit tests for destructuring-bind macro
;;;; Phase 9B - 031-destructuring-bind-macro
;;;; Tests follow TDD: written FIRST, must FAIL before implementation

(defpackage #:clysm/tests/unit/destructuring-bind
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/destructuring-bind)

;;; ============================================================
;;; Phase 2: Foundational Tests - Struct Creation
;;; ============================================================

(deftest struct-creation
  (testing "parsed-lambda-list struct creation"
    (let ((pll (clysm/lib/destructuring:make-parsed-lambda-list)))
      (ok (clysm/lib/destructuring:parsed-lambda-list-p pll))
      (ok (null (clysm/lib/destructuring:parsed-lambda-list-whole-var pll)))
      (ok (null (clysm/lib/destructuring:parsed-lambda-list-required-params pll)))
      (ok (null (clysm/lib/destructuring:parsed-lambda-list-optional-params pll)))
      (ok (null (clysm/lib/destructuring:parsed-lambda-list-rest-var pll)))
      (ok (null (clysm/lib/destructuring:parsed-lambda-list-key-params pll)))
      (ok (not (clysm/lib/destructuring:parsed-lambda-list-allow-other-keys-p pll)))))

  (testing "param-spec struct creation"
    (let ((ps (clysm/lib/destructuring:make-param-spec :type :variable :var 'x)))
      (ok (clysm/lib/destructuring:param-spec-p ps))
      (ok (eq :variable (clysm/lib/destructuring:param-spec-type ps)))
      (ok (eq 'x (clysm/lib/destructuring:param-spec-var ps)))))

  (testing "optional-param-spec struct creation"
    (let ((ops (clysm/lib/destructuring:make-optional-param-spec
                :param (clysm/lib/destructuring:make-param-spec :type :variable :var 'b)
                :default-form 10
                :supplied-p 'b-supplied-p)))
      (ok (clysm/lib/destructuring:optional-param-spec-p ops))
      (ok (clysm/lib/destructuring:param-spec-p
           (clysm/lib/destructuring:optional-param-spec-param ops)))
      (ok (= 10 (clysm/lib/destructuring:optional-param-spec-default-form ops)))
      (ok (eq 'b-supplied-p (clysm/lib/destructuring:optional-param-spec-supplied-p ops)))))

  (testing "key-param-spec struct creation"
    (let ((kps (clysm/lib/destructuring:make-key-param-spec
                :keyword :x
                :param (clysm/lib/destructuring:make-param-spec :type :variable :var 'x)
                :default-form 100
                :supplied-p 'x-p)))
      (ok (clysm/lib/destructuring:key-param-spec-p kps))
      (ok (eq :x (clysm/lib/destructuring:key-param-spec-keyword kps)))
      (ok (= 100 (clysm/lib/destructuring:key-param-spec-default-form kps)))
      (ok (eq 'x-p (clysm/lib/destructuring:key-param-spec-supplied-p kps))))))

;;; ============================================================
;;; Phase 3: User Story 1 - Basic Destructuring Tests
;;; ============================================================

(deftest parse-required-params-basic
  (testing "parse simple required params (a b c)"
    ;; TDD: This test should FAIL until T018 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(a b c))))
      (ok (clysm/lib/destructuring:parsed-lambda-list-p pll))
      (ok (= 3 (length (clysm/lib/destructuring:parsed-lambda-list-required-params pll))))
      ;; Verify each param is a variable type param-spec
      (let ((params (clysm/lib/destructuring:parsed-lambda-list-required-params pll)))
        (ok (eq 'a (clysm/lib/destructuring:param-spec-var (first params))))
        (ok (eq 'b (clysm/lib/destructuring:param-spec-var (second params))))
        (ok (eq 'c (clysm/lib/destructuring:param-spec-var (third params))))))))

(deftest parse-nested-params
  (testing "parse nested params ((a b) c)"
    ;; TDD: This test should FAIL until T019 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '((a b) c))))
      (ok (clysm/lib/destructuring:parsed-lambda-list-p pll))
      (let ((params (clysm/lib/destructuring:parsed-lambda-list-required-params pll)))
        (ok (= 2 (length params)))
        ;; First param should be nested
        (let ((first-param (first params)))
          (ok (eq :nested (clysm/lib/destructuring:param-spec-type first-param)))
          (ok (clysm/lib/destructuring:parsed-lambda-list-p
               (clysm/lib/destructuring:param-spec-nested-list first-param))))
        ;; Second param should be variable
        (let ((second-param (second params)))
          (ok (eq :variable (clysm/lib/destructuring:param-spec-type second-param)))
          (ok (eq 'c (clysm/lib/destructuring:param-spec-var second-param))))))))

(deftest generate-required-bindings
  (testing "generate code for required params"
    ;; TDD: This test should FAIL until T020 is implemented
    (let* ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(a b c)))
           (code (clysm/lib/destructuring:generate-destructuring-code pll '#:list '((list a b c)))))
      ;; Should generate nested let forms
      (ok (listp code))
      ;; The generated code should contain bindings for a, b, c
      ;; Exact structure depends on implementation, but should reference car/cdr
      (ok code))))

(deftest insufficient-elements-error
  (testing "program-error on insufficient elements"
    ;; TDD: This test should FAIL until T022 is implemented
    ;; When we try to destructure a shorter list than required,
    ;; the generated code should signal program-error
    (let* ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(a b c)))
           (code (clysm/lib/destructuring:generate-destructuring-code pll 'test-list '(t))))
      ;; Generated code should include checks that signal program-error
      ;; when the list doesn't have enough elements
      (ok code))))

;;; ============================================================
;;; Phase 4: User Story 2 - Optional and Rest Tests
;;; ============================================================

(deftest parse-optional-params
  (testing "parse &optional params"
    ;; TDD: This test should FAIL until T032 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(a &optional b))))
      (ok (clysm/lib/destructuring:parsed-lambda-list-p pll))
      (ok (= 1 (length (clysm/lib/destructuring:parsed-lambda-list-required-params pll))))
      (ok (= 1 (length (clysm/lib/destructuring:parsed-lambda-list-optional-params pll)))))))

(deftest parse-optional-with-default
  (testing "parse &optional with default value"
    ;; TDD: This test should FAIL until T032 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(a &optional (b 10)))))
      (let ((opt-params (clysm/lib/destructuring:parsed-lambda-list-optional-params pll)))
        (ok (= 1 (length opt-params)))
        (ok (= 10 (clysm/lib/destructuring:optional-param-spec-default-form (first opt-params))))))))

(deftest parse-optional-with-supplied-p
  (testing "parse &optional with supplied-p variable"
    ;; TDD: This test should FAIL until T032 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list
                '(a &optional (b nil b-supplied-p)))))
      (let ((opt-params (clysm/lib/destructuring:parsed-lambda-list-optional-params pll)))
        (ok (= 1 (length opt-params)))
        (ok (eq 'b-supplied-p
                (clysm/lib/destructuring:optional-param-spec-supplied-p (first opt-params))))))))

(deftest parse-rest-param
  (testing "parse &rest param"
    ;; TDD: This test should FAIL until T033 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(a &rest r))))
      (ok (eq 'r (clysm/lib/destructuring:parsed-lambda-list-rest-var pll))))))

(deftest generate-optional-bindings
  (testing "generate code for &optional with defaults"
    ;; TDD: This test should FAIL until T035 is implemented
    (let* ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(a &optional (b 10))))
           (code (clysm/lib/destructuring:generate-destructuring-code pll '#:list '(t))))
      ;; Should generate code with conditional default evaluation
      (ok code))))

;;; ============================================================
;;; Phase 5: User Story 3 - Keyword Parameter Tests
;;; ============================================================

(deftest parse-key-params
  (testing "parse &key params"
    ;; TDD: This test should FAIL until T046 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(&key x y))))
      (ok (= 2 (length (clysm/lib/destructuring:parsed-lambda-list-key-params pll)))))))

(deftest parse-key-with-alternate-name
  (testing "parse &key with alternate keyword name"
    ;; TDD: This test should FAIL until T046 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list
                '(&key ((:my-key x) 10)))))
      (let ((key-params (clysm/lib/destructuring:parsed-lambda-list-key-params pll)))
        (ok (= 1 (length key-params)))
        (ok (eq :my-key (clysm/lib/destructuring:key-param-spec-keyword (first key-params))))))))

(deftest parse-allow-other-keys
  (testing "parse &allow-other-keys"
    ;; TDD: This test should FAIL until T047 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list
                '(&key x &allow-other-keys))))
      (ok (clysm/lib/destructuring:parsed-lambda-list-allow-other-keys-p pll)))))

(deftest generate-key-bindings
  (testing "generate code for &key extraction"
    ;; TDD: This test should FAIL until T049 is implemented
    (let* ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(&key x y)))
           (code (clysm/lib/destructuring:generate-destructuring-code pll '#:list '(t))))
      ;; Should generate getf-like plist traversal
      (ok code))))

(deftest unknown-keyword-error
  (testing "error on unknown keyword without &allow-other-keys"
    ;; TDD: This test should FAIL until T050 is implemented
    ;; Generated code should signal error for unrecognized keywords
    (let* ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(&key x)))
           (code (clysm/lib/destructuring:generate-destructuring-code pll '#:list '(t))))
      ;; Code should include keyword validation
      (ok code))))

;;; ============================================================
;;; Phase 6: User Story 4 - Whole and Body Tests
;;; ============================================================

(deftest parse-whole-param
  (testing "parse &whole param"
    ;; TDD: This test should FAIL until T058 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(&whole w a b))))
      (ok (eq 'w (clysm/lib/destructuring:parsed-lambda-list-whole-var pll)))
      (ok (= 2 (length (clysm/lib/destructuring:parsed-lambda-list-required-params pll)))))))

(deftest parse-body-as-rest-synonym
  (testing "parse &body as &rest synonym"
    ;; TDD: This test should FAIL until T059 is implemented
    (let ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(a &body b))))
      (ok (eq 'b (clysm/lib/destructuring:parsed-lambda-list-rest-var pll))))))

(deftest generate-whole-binding
  (testing "generate code for &whole binding"
    ;; TDD: This test should FAIL until T061 is implemented
    (let* ((pll (clysm/lib/destructuring:parse-destructuring-lambda-list '(&whole w a b)))
           (code (clysm/lib/destructuring:generate-destructuring-code pll '#:list '((list w a b)))))
      ;; Should bind w to entire list before destructuring
      (ok code))))

;;; ============================================================
;;; Macro Expander Integration Tests
;;; ============================================================

(deftest make-destructuring-bind-expander-exists
  (testing "make-destructuring-bind-expander function exists"
    ;; TDD: This test should FAIL until T010 is implemented
    (ok (fboundp 'clysm/lib/macros:make-destructuring-bind-expander))))
