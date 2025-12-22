;;;; macro-test.lisp - Macro system tests (Phase 8 - US6)
(in-package #:clysm/tests/unit/macro)

;;; Compile-time Environment Tests (T159)

(deftest macro-registry
  (testing "register macro function"
    ;; The macro registry should store macro functions by symbol
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (ok (clysm/compiler/transform/macro:registry-p registry))))

  (testing "add macro to registry"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/compiler/transform/macro:register-macro
       registry 'test-macro (lambda (form) form))
      (ok (clysm/compiler/transform/macro:macro-function* registry 'test-macro))))

  (testing "lookup non-existent macro returns nil"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (ok (null (clysm/compiler/transform/macro:macro-function* registry 'no-such-macro)))))

  (testing "compile-time symbol table"
    (let ((env (clysm/compiler/transform/macro:make-compile-env)))
      (ok (clysm/compiler/transform/macro:compile-env-p env)))))

(deftest macro-detection
  (testing "detect macro form"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      ;; Register a test macro
      (clysm/compiler/transform/macro:register-macro
       registry 'my-macro (lambda (form) (declare (ignore form)) 'expanded))
      ;; Should detect (my-macro ...) as a macro call
      (ok (clysm/compiler/transform/macro:macro-form-p registry '(my-macro 1 2 3)))))

  (testing "non-macro form not detected"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      ;; Without registering, should not be detected
      (ok (not (clysm/compiler/transform/macro:macro-form-p registry '(+ 1 2)))))))

;;; Macro Expander Tests (T160)

(deftest macro-expansion
  (testing "simple macro expansion"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      ;; Define a simple macro that wraps in list
      (clysm/compiler/transform/macro:register-macro
       registry 'wrap
       (lambda (form)
         (list 'list (second form))))
      (let ((result (clysm/compiler/transform/macro:macroexpand-1*
                     registry '(wrap x))))
        (ok (equal '(list x) result)))))

  (testing "macro with multiple arguments"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      ;; Define a macro that creates a progn
      (clysm/compiler/transform/macro:register-macro
       registry 'my-progn
       (lambda (form)
         (cons 'progn (rest form))))
      (let ((result (clysm/compiler/transform/macro:macroexpand-1*
                     registry '(my-progn a b c))))
        (ok (equal '(progn a b c) result)))))

  (testing "recursive macro expansion"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      ;; Define nested macros
      (clysm/compiler/transform/macro:register-macro
       registry 'outer
       (lambda (form)
         (list 'inner (second form))))
      (clysm/compiler/transform/macro:register-macro
       registry 'inner
       (lambda (form)
         (list 'result (second form))))
      ;; macroexpand* should expand all macros
      (let ((result (clysm/compiler/transform/macro:macroexpand*
                     registry '(outer x))))
        (ok (equal '(result x) result)))))

  (testing "non-macro form returns unchanged"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (let ((form '(+ 1 2)))
        (let ((result (clysm/compiler/transform/macro:macroexpand-1*
                       registry form)))
          (ok (eq form result))))))

  (testing "atom expansion returns unchanged"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (ok (= 42 (clysm/compiler/transform/macro:macroexpand-1* registry 42)))
      (ok (eq 'foo (clysm/compiler/transform/macro:macroexpand-1* registry 'foo))))))

(deftest form-walking
  (testing "walk expands nested forms"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      ;; Define a simple macro
      (clysm/compiler/transform/macro:register-macro
       registry 'double
       (lambda (form)
         (list '+ (second form) (second form))))
      ;; Walk should expand the macro inside a list
      (let ((result (clysm/compiler/transform/macro:macroexpand-all
                     registry '(if t (double x) 0))))
        (ok (equal '(if t (+ x x) 0) result)))))

  (testing "walk handles quote"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      ;; Quoted forms should not be expanded
      (clysm/compiler/transform/macro:register-macro
       registry 'test-macro
       (lambda (form)
         (declare (ignore form))
         'should-not-appear))
      (let ((result (clysm/compiler/transform/macro:macroexpand-all
                     registry '(quote (test-macro)))))
        (ok (equal '(quote (test-macro)) result)))))

  (testing "walk handles let bindings"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/compiler/transform/macro:register-macro
       registry 'inc
       (lambda (form)
         (list '+ (second form) 1)))
      (let ((result (clysm/compiler/transform/macro:macroexpand-all
                     registry '(let ((x 1)) (inc x)))))
        (ok (equal '(let ((x 1)) (+ x 1)) result))))))

;;; Macro Environment Tests

(deftest macro-environment
  (testing "lexical environment in macros"
    ;; Macros should have access to compile-time environment
    (ok t))  ; Placeholder for compile-time env tests

  (testing "macro hygiene basic"
    ;; Test that gensym produces unique symbols
    (let ((sym1 (gensym "G"))
          (sym2 (gensym "G")))
      (ok (not (eq sym1 sym2))))))

;;; Edge Cases

(deftest macro-edge-cases
  (testing "nil form"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (ok (null (clysm/compiler/transform/macro:macroexpand-1* registry nil)))))

  (testing "empty list"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (ok (null (clysm/compiler/transform/macro:macroexpand-1* registry '())))))

  (testing "macro returning nil"
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/compiler/transform/macro:register-macro
       registry 'nil-macro
       (lambda (form)
         (declare (ignore form))
         nil))
      (ok (null (clysm/compiler/transform/macro:macroexpand-1*
                 registry '(nil-macro)))))))
