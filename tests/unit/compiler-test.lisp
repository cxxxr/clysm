;;;; tests/unit/compiler-test.lisp - Compiler Tests

(in-package #:clysm/tests)

;;; ============================================================
;;; AST Parser Tests
;;; ============================================================

(defsuite compiler-suite
    "Tests for the Clysm compiler")

(deftest test-parse-integer ()
  "Test parsing integer literals"
  (let ((ast (clysm:parse-sexp 42)))
    (is (typep ast 'clysm:ast-literal))
    (is-eql 42 (clysm:ast-literal-value ast))
    (is-eq :fixnum (clysm:ast-literal-type ast))))

(deftest test-parse-negative-integer ()
  "Test parsing negative integer literals"
  (let ((ast (clysm:parse-sexp -123)))
    (is (typep ast 'clysm:ast-literal))
    (is-eql -123 (clysm:ast-literal-value ast))))

(deftest test-parse-character ()
  "Test parsing character literals"
  (let ((ast (clysm:parse-sexp #\A)))
    (is (typep ast 'clysm:ast-literal))
    (is-eql #\A (clysm:ast-literal-value ast))
    (is-eq :character (clysm:ast-literal-type ast))))

(deftest test-parse-symbol ()
  "Test parsing symbol references"
  (let ((ast (clysm:parse-sexp 'foo)))
    (is (typep ast 'clysm:ast-var))
    (is-eq 'foo (clysm:ast-var-name ast))))

(deftest test-parse-nil ()
  "Test parsing NIL"
  (let ((ast (clysm:parse-sexp nil)))
    (is (typep ast 'clysm:ast-quote))
    (is-eq nil (clysm:ast-quote-value ast))))

(deftest test-parse-t ()
  "Test parsing T"
  (let ((ast (clysm:parse-sexp t)))
    (is (typep ast 'clysm:ast-quote))
    (is-eq t (clysm:ast-quote-value ast))))

;;; ============================================================
;;; Special Form Parser Tests
;;; ============================================================

(deftest test-parse-quote ()
  "Test parsing quote"
  (let ((ast (clysm:parse-sexp '(quote foo))))
    (is (typep ast 'clysm:ast-quote))
    (is-eq 'foo (clysm:ast-quote-value ast))))

(deftest test-parse-if-two-branch ()
  "Test parsing if with two branches"
  (let ((ast (clysm:parse-sexp '(if x y z))))
    (is (typep ast 'clysm:ast-if))
    (is (typep (clysm:ast-if-test ast) 'clysm:ast-var))
    (is (typep (clysm:ast-if-then ast) 'clysm:ast-var))
    (is (typep (clysm:ast-if-else ast) 'clysm:ast-var))))

(deftest test-parse-if-one-branch ()
  "Test parsing if with one branch"
  (let ((ast (clysm:parse-sexp '(if x y))))
    (is (typep ast 'clysm:ast-if))
    (is (typep (clysm:ast-if-test ast) 'clysm:ast-var))
    (is (typep (clysm:ast-if-then ast) 'clysm:ast-var))
    (is-eq nil (clysm:ast-if-else ast))))

(deftest test-parse-progn ()
  "Test parsing progn"
  (let ((ast (clysm:parse-sexp '(progn a b c))))
    (is (typep ast 'clysm:ast-progn))
    (is-eql 3 (length (clysm:ast-progn-forms ast)))))

(deftest test-parse-progn-empty ()
  "Test parsing empty progn"
  (let ((ast (clysm:parse-sexp '(progn))))
    (is (typep ast 'clysm:ast-progn))
    (is-eql 0 (length (clysm:ast-progn-forms ast)))))

(deftest test-parse-let ()
  "Test parsing let"
  (let ((ast (clysm:parse-sexp '(let ((x 1) (y 2)) (+ x y)))))
    (is (typep ast 'clysm:ast-let))
    (is-eql 2 (length (clysm:ast-let-bindings ast)))
    (is-false (clysm:ast-let-sequential-p ast))))

(deftest test-parse-let* ()
  "Test parsing let*"
  (let ((ast (clysm:parse-sexp '(let* ((x 1) (y x)) y))))
    (is (typep ast 'clysm:ast-let))
    (is-eql 2 (length (clysm:ast-let-bindings ast)))
    (is (clysm:ast-let-sequential-p ast))))

(deftest test-parse-let-simple-binding ()
  "Test parsing let with simple bindings"
  (let ((ast (clysm:parse-sexp '(let (x y) x))))
    (is (typep ast 'clysm:ast-let))
    (is-eql 2 (length (clysm:ast-let-bindings ast)))))

(deftest test-parse-lambda ()
  "Test parsing lambda"
  (let ((ast (clysm:parse-sexp '(lambda (x y) (+ x y)))))
    (is (typep ast 'clysm:ast-lambda))
    (is-equal '(x y) (clysm:ast-lambda-params ast))
    (is (typep (clysm:ast-lambda-body ast) 'clysm:ast-progn))))

(deftest test-parse-lambda-no-params ()
  "Test parsing lambda with no parameters"
  (let ((ast (clysm:parse-sexp '(lambda () 42))))
    (is (typep ast 'clysm:ast-lambda))
    (is-equal '() (clysm:ast-lambda-params ast))))

(deftest test-parse-setq ()
  "Test parsing setq"
  (let ((ast (clysm:parse-sexp '(setq x 42))))
    (is (typep ast 'clysm:ast-setq))
    (is-eq 'x (clysm:ast-setq-name ast))
    (is (typep (clysm:ast-setq-value ast) 'clysm:ast-literal))))

(deftest test-parse-setq-multiple ()
  "Test parsing setq with multiple pairs"
  (let ((ast (clysm:parse-sexp '(setq x 1 y 2))))
    (is (typep ast 'clysm:ast-progn))
    (is-eql 2 (length (clysm:ast-progn-forms ast)))))

(deftest test-parse-block ()
  "Test parsing block"
  (let ((ast (clysm:parse-sexp '(block foo (return-from foo 42)))))
    (is (typep ast 'clysm:ast-block))
    (is-eq 'foo (clysm:ast-block-name ast))))

(deftest test-parse-return-from ()
  "Test parsing return-from"
  (let ((ast (clysm:parse-sexp '(return-from foo 42))))
    (is (typep ast 'clysm:ast-return-from))
    (is-eq 'foo (clysm:ast-return-from-name ast))))

(deftest test-parse-defun ()
  "Test parsing defun"
  (let ((ast (clysm:parse-sexp '(defun add (x y) (+ x y)))))
    (is (typep ast 'clysm:ast-defun))
    (is-eq 'add (clysm:ast-defun-name ast))
    (is-equal '(x y) (clysm:ast-defun-params ast))))

(deftest test-parse-defvar ()
  "Test parsing defvar"
  (let ((ast (clysm:parse-sexp '(defvar *counter* 0))))
    (is (typep ast 'clysm:ast-defvar))
    (is-eq '*counter* (clysm:ast-defvar-name ast))))

;;; ============================================================
;;; Function Call Parser Tests
;;; ============================================================

(deftest test-parse-primitive-call ()
  "Test parsing primitive function call"
  (let ((ast (clysm:parse-sexp '(+ 1 2))))
    (is (typep ast 'clysm:ast-primitive-call))
    (is-eq '+ (clysm:ast-primitive-call-name ast))
    (is-eql 2 (length (clysm:ast-primitive-call-args ast)))))

(deftest test-parse-cons-call ()
  "Test parsing cons call"
  (let ((ast (clysm:parse-sexp '(cons 1 2))))
    (is (typep ast 'clysm:ast-primitive-call))
    (is-eq 'cons (clysm:ast-primitive-call-name ast))))

(deftest test-parse-general-call ()
  "Test parsing general function call"
  (let ((ast (clysm:parse-sexp '(my-func 1 2 3))))
    (is (typep ast 'clysm:ast-call))
    (is (typep (clysm:ast-call-func ast) 'clysm:ast-var))
    (is-eql 3 (length (clysm:ast-call-args ast)))))

;;; ============================================================
;;; Special Form Registry Tests
;;; ============================================================

(deftest test-special-form-p ()
  "Test special-form-p predicate"
  (is (clysm:special-form-p 'quote))
  (is (clysm:special-form-p 'if))
  (is (clysm:special-form-p 'progn))
  (is (clysm:special-form-p 'let))
  (is (clysm:special-form-p 'lambda))
  (is-false (clysm:special-form-p 'car))
  (is-false (clysm:special-form-p 'not-a-special-form)))

;;; ============================================================
;;; Compile Environment Tests
;;; ============================================================

(deftest test-make-compile-env ()
  "Test creating a compile environment"
  (let ((env (clysm:make-compile-env)))
    (is (clysm:compile-env-p env))
    (is-eq nil (clysm:compile-env-locals env))
    (is-eql 0 (clysm:compile-env-local-count env))))

(deftest test-env-bind-local ()
  "Test binding local variables"
  (let ((env (clysm:make-compile-env)))
    (let ((binding (clysm:env-bind-local env 'x)))
      (is-eq 'x (clysm:binding-name binding))
      (is-eq :local (clysm:binding-kind binding))
      (is-eql 0 (clysm:binding-index binding)))
    (let ((binding (clysm:env-bind-local env 'y)))
      (is-eql 1 (clysm:binding-index binding)))
    (is-eql 2 (clysm:compile-env-local-count env))))

(deftest test-env-lookup ()
  "Test looking up variables"
  (let ((env (clysm:make-compile-env)))
    (clysm:env-bind-local env 'x)
    (clysm:env-bind-local env 'y)
    (multiple-value-bind (binding found-p)
        (clysm:env-lookup env 'x)
      (is found-p)
      (is-eq 'x (clysm:binding-name binding)))
    (multiple-value-bind (binding found-p)
        (clysm:env-lookup env 'z)
      (declare (ignore binding))
      (is-false found-p))))

(deftest test-env-bind-param ()
  "Test binding parameters"
  (let ((env (clysm:make-compile-env)))
    (let ((binding (clysm:env-bind-param env 'x 0)))
      (is-eq :param (clysm:binding-kind binding))
      (is-eql 0 (clysm:binding-index binding)))
    (let ((binding (clysm:env-bind-param env 'y 1)))
      (is-eql 1 (clysm:binding-index binding)))))

(deftest test-env-blocks ()
  "Test block management"
  (let ((env (clysm:make-compile-env)))
    (let ((label1 (clysm:env-push-block env 'outer)))
      (is (integerp label1))
      (let ((label2 (clysm:env-push-block env 'inner)))
        (is (integerp label2))
        (multiple-value-bind (label depth)
            (clysm:env-find-block env 'inner)
          (is-eql label2 label)
          (is-eql 0 depth))
        (multiple-value-bind (label depth)
            (clysm:env-find-block env 'outer)
          (is-eql label1 label)
          (is-eql 1 depth))
        (clysm:env-pop-block env))
      (clysm:env-pop-block env))))

(deftest test-env-tail-position ()
  "Test tail position tracking"
  (let ((env (clysm:make-compile-env)))
    (is-false (clysm:compile-env-tail-position-p env))
    (let ((tail-env (clysm:env-in-tail-position env)))
      (is (clysm:compile-env-tail-position-p tail-env)))
    (let ((non-tail-env (clysm:env-not-in-tail-position env)))
      (is-false (clysm:compile-env-tail-position-p non-tail-env)))))

;;; ============================================================
;;; Code Generation Tests
;;; ============================================================

(deftest test-compile-integer-literal ()
  "Test compiling integer literals"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry))
         (ast (clysm:parse-sexp 42))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    (is (> (length code) 0))
    ;; Should contain i32.const and ref.i31
    (is (member #x41 code))   ; i32.const
    (is (member #xFB code)))) ; GC prefix for ref.i31

(deftest test-compile-progn ()
  "Test compiling progn"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry))
         (ast (clysm:parse-sexp '(progn 1 2 3)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain multiple i32.const and drops
    (is (> (length code) 3))))

(deftest test-compile-if ()
  "Test compiling if expression"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry))
         (ast (clysm:parse-sexp '(if 1 2 3)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain if, else, end opcodes
    (is (member #x04 code))   ; if
    (is (member #x05 code))   ; else
    (is (member #x0B code)))) ; end

(deftest test-compile-let ()
  "Test compiling let expression"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry))
         (ast (clysm:parse-sexp '(let ((x 42)) x)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain local.set and local.get
    (is (member #x21 code))   ; local.set
    (is (member #x20 code)))) ; local.get

(deftest test-compile-primitive-call ()
  "Test compiling primitive call"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry))
         (ast (clysm:parse-sexp '(+ 1 2)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain i32.add
    (is (member #x6A code)))) ; i32.add

(deftest test-compile-cons ()
  "Test compiling cons"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry))
         (ast (clysm:parse-sexp '(cons 1 2)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain struct.new (GC prefix)
    (is (member #xFB code))
    (is (member #x00 code)))) ; struct.new

(deftest test-compile-comparison ()
  "Test compiling comparison"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry))
         (ast (clysm:parse-sexp '(< 1 2)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain i32.lt_s
    (is (member #x48 code)))) ; i32.lt_s

;;; ============================================================
;;; Defun Compilation Tests
;;; ============================================================

(deftest test-compile-simple-defun ()
  "Test compiling a simple defun"
  (let* ((context (clysm:compile-toplevel '(defun identity (x) x))))
    (is (clysm:codegen-context-p context))
    (let ((module (clysm:codegen-context-module context)))
      ;; Should have the function registered
      (clysm:module-finalize module)
      (is (>= (length (clysm:wasm-module-funcs module)) 1))
      (is (>= (length (clysm:wasm-module-exports module)) 1)))))

(deftest test-compile-arithmetic-defun ()
  "Test compiling a defun with arithmetic"
  (let* ((context (clysm:compile-toplevel '(defun add (x y) (+ x y)))))
    (is (clysm:codegen-context-p context))
    (let ((module (clysm:codegen-context-module context)))
      (clysm:module-finalize module)
      ;; Check that we can emit the module
      (let ((bytes (clysm:emit-wasm-binary module)))
        (is (listp bytes))
        (is (> (length bytes) 8))))))

(deftest test-compile-forms ()
  "Test compiling multiple forms"
  (let ((module (clysm:compile-forms
                 '((defun foo (x) x)
                   (defun bar (x) (+ x 1))))))
    (clysm:module-finalize module)
    (is (>= (length (clysm:wasm-module-funcs module)) 2))
    (is (>= (length (clysm:wasm-module-exports module)) 2))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(deftest test-compile-to-valid-wasm ()
  "Test that compiled code produces valid Wasm"
  (let ((module (clysm:compile-forms
                 '((defun double (x) (+ x x))))))
    (let ((bytes (clysm:emit-wasm-binary module)))
      ;; Check magic number
      (is-eql #x00 (first bytes))
      (is-eql #x61 (second bytes))
      (is-eql #x73 (third bytes))
      (is-eql #x6D (fourth bytes))
      ;; Check version
      (is-eql #x01 (fifth bytes)))))
