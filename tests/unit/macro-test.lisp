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

;;; Expansion Depth Limit Tests (T007)

(deftest expansion-depth-limit
  (testing "infinite macro expansion triggers error"
    ;; Create a macro that expands to itself, causing infinite expansion
    ;; NOTE: Use LIST to create a fresh list each time - quoted constants
    ;; are EQ which would cause macroexpand* to stop early
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/compiler/transform/macro:register-macro
       registry 'infinite-loop
       (lambda (form)
         (declare (ignore form))
         (list 'infinite-loop)))  ; Creates fresh list each time
      ;; Should signal macro-expansion-depth-exceeded
      (ok (signals clysm/compiler/transform/macro:macro-expansion-depth-exceeded
            (clysm/compiler/transform/macro:macroexpand*
             registry '(infinite-loop))))))

  (testing "deep but finite expansion succeeds"
    ;; Create macros that expand a fixed number of times
    (let ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/compiler/transform/macro:register-macro
       registry 'level-1
       (lambda (form)
         (declare (ignore form))
         '(level-2)))
      (clysm/compiler/transform/macro:register-macro
       registry 'level-2
       (lambda (form)
         (declare (ignore form))
         '(level-3)))
      (clysm/compiler/transform/macro:register-macro
       registry 'level-3
       (lambda (form)
         (declare (ignore form))
         '(done)))
      ;; Should succeed (only 3 expansions)
      (ok (equal '(done)
                 (clysm/compiler/transform/macro:macroexpand*
                  registry '(level-1)))))))

;;; &key Lambda List Tests (T009-T015)

(deftest defmacro-key-support
  (testing "parse-lambda-list extracts &key parameters"
    (multiple-value-bind (req opt rest kind keys allow)
        (clysm/compiler/transform/macro::parse-lambda-list
         '(x &key (timeout 30) verbose))
      (ok (equal req '(x)))
      (ok (null opt))
      (ok (null rest))
      (ok (equal keys '((:timeout timeout 30) (:verbose verbose nil))))
      (ok (null allow))))

  (testing "defmacro with &key binds keyword arguments"
    (let* ((form '(defmacro with-options (name &key (timeout 30) verbose)
                    (list 'let (list (list '*timeout* timeout)
                                     (list '*verbose* verbose))
                          name)))
           (result (clysm/compiler/transform/macro:parse-defmacro form))
           (expander (clysm/compiler/transform/macro::compile-defmacro result)))
      ;; Test with keyword args provided
      (ok (equal (funcall expander '(with-options (do-work) :timeout 60 :verbose t))
                 '(let ((*timeout* 60) (*verbose* t)) (do-work))))
      ;; Test with default values
      (ok (equal (funcall expander '(with-options (do-work)))
                 '(let ((*timeout* 30) (*verbose* nil)) (do-work)))))))

;;; ============================================================
;;; T023: case Macro Tests
;;; ============================================================

(deftest case-macro-expansion
  (testing "case expands to cond-like structure"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(case x (a 1) (b 2) (c 3)))))
        (ok expanded)
        ;; Should produce a let binding the keyform then conditionals
        (ok (eq (first expanded) 'let)))))

  (testing "case with otherwise clause"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(case x (a 1) (otherwise 99)))))
        (ok expanded))))

  (testing "case with multiple keys"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(case x ((a b c) 1) ((d e) 2)))))
        (ok expanded)))))

;;; ============================================================
;;; T030-T031: prog1 and prog2 Macro Tests
;;; ============================================================

(deftest prog1-macro-expansion
  (testing "prog1 returns first form value"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(prog1 (foo) (bar) (baz)))))
        (ok expanded)
        ;; Should expand to a let that saves first form's result
        (ok (eq (first expanded) 'let)))))

  (testing "prog1 with single form"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(prog1 x))))
        (ok expanded)))))

(deftest prog2-macro-expansion
  (testing "prog2 returns second form value"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(prog2 (foo) (bar) (baz)))))
        (ok expanded)
        ;; Should expand to progn with let saving second form's result
        (ok (eq (first expanded) 'progn)))))

  (testing "prog2 with just two forms"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(prog2 x y))))
        (ok expanded)))))

;;; ============================================================
;;; T032-T035: do and do* Macro Tests
;;; ============================================================

(deftest do-macro-expansion
  (testing "do expands to block with tagbody"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(do ((i 0 (+ i 1)))
                                     ((>= i 10) result)
                                   (print i)))))
        (ok expanded)
        ;; Should expand to a block
        (ok (eq (first expanded) 'block)))))

  (testing "do with multiple variables"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(do ((i 0 (+ i 1))
                                      (j 10 (- j 1)))
                                     ((= i j) i)))))
        (ok expanded)))))

(deftest do*-macro-expansion
  (testing "do* expands with let* for sequential binding"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(do* ((i 0 (+ i 1))
                                       (j i (* j 2)))
                                      ((>= i 5) j)))))
        (ok expanded)
        (ok (eq (first expanded) 'block))))))

;;; ============================================================
;;; T041-T045: User-Facing macroexpand Tests
;;; ============================================================

(deftest global-macroexpand
  (testing "global-macro-registry returns a registry"
    (ok (clysm/compiler/transform/macro:registry-p
         (clysm/compiler/transform/macro:global-macro-registry))))

  (testing "macroexpand-1 works with global registry"
    ;; First install standard macros in global registry
    (clysm/lib/macros:install-standard-macros
     (clysm/compiler/transform/macro:global-macro-registry))
    (let ((expanded (clysm/compiler/transform/macro:macroexpand-1
                     '(when t (print 'hello)))))
      (ok expanded)
      ;; when expands to if
      (ok (eq (first expanded) 'if))))

  (testing "macroexpand fully expands nested macros"
    (clysm/lib/macros:install-standard-macros
     (clysm/compiler/transform/macro:global-macro-registry))
    (let ((expanded (clysm/compiler/transform/macro:macroexpand
                     '(and a b c))))
      (ok expanded)))

  (testing "reset-global-macro-registry clears macros"
    (clysm/compiler/transform/macro:reset-global-macro-registry)
    (let ((registry (clysm/compiler/transform/macro:global-macro-registry)))
      (ok (null (clysm/compiler/transform/macro:macro-function* registry 'when))))))
