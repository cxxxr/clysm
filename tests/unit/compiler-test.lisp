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

;;; ============================================================
;;; Closure Tests (Phase 5)
;;; ============================================================

(deftest test-emit-closure-operations ()
  "Test closure instruction emitters"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    ;; Test emit-make-closure
    (let ((code (clysm:emit-make-closure registry)))
      (is (listp code))
      (is (member #xFB code)))  ; GC prefix
    ;; Test emit-closure-get-env
    (let ((code (clysm:emit-closure-get-env registry)))
      (is (listp code))
      (is (member #xFB code)))
    ;; Test emit-closure-get-code for each arity
    (dolist (arity '(0 1 2 :n))
      (let ((code (clysm:emit-closure-get-code registry arity)))
        (is (listp code))
        (is (member #xFB code))))))

(deftest test-emit-create-env ()
  "Test environment array creation"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    ;; Test creating empty env
    (let ((code (clysm:emit-create-env registry 0)))
      (is (listp code))
      (is (member #xFB code)))
    ;; Test creating env with 3 slots
    (let ((code (clysm:emit-create-env registry 3)))
      (is (listp code))
      (is (member #xFB code)))))

(deftest test-func-type-accessors ()
  "Test function type index accessors"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    (is (integerp (clysm:func-0-type-index registry)))
    (is (integerp (clysm:func-1-type-index registry)))
    (is (integerp (clysm:func-2-type-index registry)))
    (is (integerp (clysm:func-n-type-index registry)))
    ;; All should be distinct
    (let ((indices (list (clysm:func-0-type-index registry)
                         (clysm:func-1-type-index registry)
                         (clysm:func-2-type-index registry)
                         (clysm:func-n-type-index registry))))
      (is-eql 4 (length (remove-duplicates indices))))))

(deftest test-analyze-free-variables ()
  "Test free variable analysis"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry)))
    ;; Simple case: lambda with no free variables
    (let* ((ast (clysm:parse-sexp '(lambda (x) x)))
           (free-vars (clysm:analyze-free-variables ast env)))
      ;; x is a param, not free
      (is-false (member 'x free-vars)))
    ;; Lambda that captures y (free)
    (let* ((ast (clysm:parse-sexp '(lambda (x) (+ x y))))
           (free-vars (clysm:analyze-free-variables ast env)))
      ;; y should be free (not bound in env or lambda params)
      (is (member 'y free-vars))
      ;; x should NOT be free (it's a lambda param)
      (is-false (member 'x free-vars)))))

(deftest test-compile-simple-lambda ()
  "Test compiling a simple lambda without captures"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context))
         (ast (clysm:parse-sexp '(lambda (x) x)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    (is (> (length code) 0))
    ;; Should contain struct.new for closure
    (is (member #xFB code))
    ;; Lambda counter should have incremented
    (is (>= (clysm:codegen-context-lambda-counter context) 1))))

(deftest test-compile-lambda-with-capture ()
  "Test compiling a lambda that captures a variable"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context)))
    ;; Bind y as a local (will be captured by lambda)
    (clysm:env-bind-local env 'y)
    ;; Compile lambda that captures y
    (let* ((ast (clysm:parse-sexp '(lambda (x) (+ x y))))
           (code (clysm:compile-expression ast env)))
      (is (listp code))
      (is (> (length code) 0))
      ;; Should have created an environment array
      (is (member #x08 code)))))  ; array.new_fixed

(deftest test-compile-defun-with-multiple-args ()
  "Test compiling defun with 0, 1, 2, and 3+ args"
  ;; 0-arg function
  (let* ((context (clysm:compile-toplevel '(defun zero () 42))))
    (is (clysm:codegen-context-p context)))
  ;; 1-arg function
  (let* ((context (clysm:compile-toplevel '(defun one (x) x))))
    (is (clysm:codegen-context-p context)))
  ;; 2-arg function
  (let* ((context (clysm:compile-toplevel '(defun two (x y) (+ x y)))))
    (is (clysm:codegen-context-p context)))
  ;; 3-arg function (uses array calling convention)
  (let* ((context (clysm:compile-toplevel '(defun three (x y z) (+ x (+ y z))))))
    (is (clysm:codegen-context-p context))))

(deftest test-emit-call-ref ()
  "Test call_ref instruction emission"
  (let ((code (clysm:emit-call-ref 5)))
    (is (listp code))
    (is-eql #x14 (first code))))  ; call_ref opcode

(deftest test-emit-return-call-ref ()
  "Test return_call_ref instruction emission"
  (let ((code (clysm:emit-return-call-ref 5)))
    (is (listp code))
    (is-eql #x15 (first code))))  ; return_call_ref opcode

(deftest test-emit-ref.func ()
  "Test ref.func instruction emission"
  (let ((code (clysm:emit-ref.func 3)))
    (is (listp code))
    (is-eql #xD2 (first code))))  ; ref.func opcode

;;; ============================================================
;;; Control Flow Tests (Phase 6)
;;; ============================================================

;;; --- AST Parsing Tests ---

(deftest test-parse-tagbody ()
  "Test parsing tagbody form"
  (let ((ast (clysm:parse-sexp '(tagbody
                                  start
                                    (print "start")
                                  loop
                                    (print "loop")))))
    (is (clysm:ast-tagbody-p ast))
    (is (listp (clysm:ast-tagbody-segments ast)))
    ;; Should have segments for each tag
    (is (>= (length (clysm:ast-tagbody-segments ast)) 2))))

(deftest test-parse-go ()
  "Test parsing go form"
  (let ((ast (clysm:parse-sexp '(go loop))))
    (is (clysm:ast-go-p ast))
    (is-eql 'loop (clysm:ast-go-tag ast))))

(deftest test-parse-go-integer-tag ()
  "Test parsing go with integer tag"
  (let ((ast (clysm:parse-sexp '(go 42))))
    (is (clysm:ast-go-p ast))
    (is-eql 42 (clysm:ast-go-tag ast))))

(deftest test-parse-catch ()
  "Test parsing catch form"
  (let ((ast (clysm:parse-sexp '(catch 'error (+ 1 2)))))
    (is (clysm:ast-catch-p ast))
    (is (clysm:ast-quote-p (clysm:ast-catch-tag ast)))
    ;; Body is wrapped in progn
    (is (clysm:ast-progn-p (clysm:ast-catch-body ast)))))

(deftest test-parse-throw ()
  "Test parsing throw form"
  (let ((ast (clysm:parse-sexp '(throw 'error 42))))
    (is (clysm:ast-throw-p ast))
    (is (clysm:ast-quote-p (clysm:ast-throw-tag ast)))
    (is (clysm:ast-literal-p (clysm:ast-throw-value ast)))))

(deftest test-parse-unwind-protect ()
  "Test parsing unwind-protect form"
  (let ((ast (clysm:parse-sexp '(unwind-protect
                                    (do-something)
                                  (cleanup1)
                                  (cleanup2)))))
    (is (clysm:ast-unwind-protect-p ast))
    (is (clysm:ast-call-p (clysm:ast-unwind-protect-protected ast)))
    (is (clysm:ast-progn-p (clysm:ast-unwind-protect-cleanup ast)))))

;;; --- Environment Management Tests ---

(deftest test-env-tagbody-tags ()
  "Test tagbody tag management"
  (let ((env (clysm:make-compile-env)))
    ;; Push some tags
    (let ((entries (clysm:env-push-tagbody env '(start loop end))))
      (is (listp entries))
      (is-eql 3 (length entries)))
    ;; Find tags
    (multiple-value-bind (idx depth)
        (clysm:env-find-tag env 'loop)
      (is (integerp idx))
      (is (integerp depth)))
    ;; Pop tags
    (clysm:env-pop-tagbody env 3)
    ;; Should not find anymore
    (multiple-value-bind (idx depth)
        (clysm:env-find-tag env 'loop)
      (declare (ignore depth))
      (is-false idx))))

(deftest test-env-catch-management ()
  "Test catch/throw environment management"
  (let ((env (clysm:make-compile-env)))
    ;; Push catch handler
    (let ((depth (clysm:env-push-catch env 0)))
      (is (integerp depth))
      (is-eql 0 depth))
    ;; Catch depth should be 1
    (is-eql 1 (clysm:env-catch-depth env))
    ;; Pop and check
    (clysm:env-pop-catch env)
    (is-eql 0 (clysm:env-catch-depth env))))

;;; --- Code Generation Tests ---

(deftest test-compile-tagbody-simple ()
  "Test compiling simple tagbody"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context))
         (ast (clysm:parse-sexp '(tagbody
                                   start
                                     1
                                   end
                                     2)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain block and loop opcodes
    (is (member #x02 code))   ; block
    (is (member #x03 code))   ; loop
    ;; Should contain br_table
    (is (member #x0E code)))) ; br_table

(deftest test-compile-catch-simple ()
  "Test compiling simple catch"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context))
         ;; Use integer tag instead of symbol (symbols not yet implemented)
         (ast (clysm:parse-sexp '(catch 1 42)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain block opcodes
    (is (member #x02 code))   ; block
    (is (member #x0B code)))) ; end

(deftest test-compile-throw-simple ()
  "Test compiling simple throw"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context))
         ;; Use integer tag instead of symbol (symbols not yet implemented)
         (ast (clysm:parse-sexp '(throw 1 42)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should produce code that evaluates both tag and value
    (is (> (length code) 0))))

(deftest test-compile-unwind-protect-simple ()
  "Test compiling simple unwind-protect"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context))
         (ast (clysm:parse-sexp '(unwind-protect 42 1)))
         (code (clysm:compile-expression ast env)))
    (is (listp code))
    ;; Should contain local.set and local.get for result handling
    (is (member #x21 code))   ; local.set
    (is (member #x20 code))   ; local.get
    ;; Should contain drop for cleanup result
    (is (member #x1A code)))) ; drop

;;; --- Exception Handling Instruction Tests ---

(deftest test-emit-throw ()
  "Test throw instruction emission"
  (let ((code (clysm:emit-throw 0)))
    (is (listp code))
    (is-eql #x08 (first code))))  ; throw opcode

(deftest test-emit-throw-ref ()
  "Test throw_ref instruction emission"
  (let ((code (clysm:emit-throw-ref)))
    (is (listp code))
    (is-eql #x0A (first code))))  ; throw_ref opcode

(deftest test-emit-try-table ()
  "Test try_table instruction emission"
  (let ((code (clysm:emit-try-table nil '((:catch 0 1)))))
    (is (listp code))
    (is-eql #x1F (first code))))  ; try_table opcode

(deftest test-encode-catch-clause ()
  "Test catch clause encoding"
  ;; :catch tag label
  (let ((code (clysm:encode-catch-clause '(:catch 0 1))))
    (is (listp code))
    (is-eql #x00 (first code)))  ; catch kind
  ;; :catch-ref tag label
  (let ((code (clysm:encode-catch-clause '(:catch-ref 0 1))))
    (is (listp code))
    (is-eql #x01 (first code)))  ; catch-ref kind
  ;; :catch-all label
  (let ((code (clysm:encode-catch-clause '(:catch-all 0))))
    (is (listp code))
    (is-eql #x02 (first code)))  ; catch-all kind
  ;; :catch-all-ref label
  (let ((code (clysm:encode-catch-clause '(:catch-all-ref 0))))
    (is (listp code))
    (is-eql #x03 (first code)))) ; catch-all-ref kind

;;; --- Free Variable Analysis Tests ---

(deftest test-analyze-free-variables-tagbody ()
  "Test free variable analysis for tagbody"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry)))
    (let* ((ast (clysm:parse-sexp '(tagbody loop (+ x 1))))
           (free-vars (clysm:analyze-free-variables ast env)))
      ;; x should be free
      (is (member 'x free-vars)))))

(deftest test-analyze-free-variables-catch ()
  "Test free variable analysis for catch"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry)))
    (let* ((ast (clysm:parse-sexp '(catch 'error (+ x 1))))
           (free-vars (clysm:analyze-free-variables ast env)))
      ;; x should be free
      (is (member 'x free-vars)))))

(deftest test-analyze-free-variables-unwind-protect ()
  "Test free variable analysis for unwind-protect"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (env (clysm:make-compile-env :type-registry registry)))
    (let* ((ast (clysm:parse-sexp '(unwind-protect x (cleanup y))))
           (free-vars (clysm:analyze-free-variables ast env)))
      ;; Both x and y should be free
      (is (member 'x free-vars))
      (is (member 'y free-vars)))))

;;; ============================================================
;;; Special Variables Tests (Phase 7)
;;; ============================================================

;;; --- Context Special Variables Tests ---

(deftest test-context-declare-special ()
  "Test declaring special variables in codegen context"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry)))
    ;; Initially empty
    (is-false (clysm:context-special-p context '*foo*))

    ;; Declare special
    (clysm:context-declare-special context '*foo*)

    ;; Now it's special
    (is (clysm:context-special-p context '*foo*))
    (is (member '*foo* (clysm:codegen-context-specials context)))))

(deftest test-context-special-globals ()
  "Test registering special variable globals"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry)))
    ;; Initially no global
    (is-false (clysm:context-special-global-index context '*bar*))

    ;; Register a global
    (clysm:context-register-special-global context '*bar* 42)

    ;; Now has a global index
    (is-eql 42 (clysm:context-special-global-index context '*bar*))))

;;; --- Environment Special Tests ---

(deftest test-env-special-p ()
  "Test env-special-p predicate"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context)))
    ;; Initially not special
    (is-false (clysm:env-special-p env '*counter*))

    ;; Declare in context
    (clysm:context-declare-special context '*counter*)

    ;; Now visible through env
    (is (clysm:env-special-p env '*counter*))))

(deftest test-env-declare-special ()
  "Test declaring specials locally in environment"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context)))
    ;; Declare locally
    (clysm:env-declare-special env '*local-special*)

    ;; Check local specials
    (is (member '*local-special* (clysm:compile-env-specials env)))
    (is (clysm:env-special-p env '*local-special*))))

;;; --- Module Add Global Tests ---

(deftest test-module-add-global ()
  "Test adding globals to module"
  (let* ((module (clysm:make-wasm-module))
         (init-expr (list (clysm:opcode :ref.null) #x6E))
         (global (clysm:make-global :anyref init-expr :mutable t :name "test")))
    (let ((idx (clysm:module-add-global module global)))
      ;; First global gets index 0
      (is-eql 0 idx)
      ;; Verify it was added
      (is-eql 1 (length (clysm:wasm-module-globals module))))))

;;; --- Emit Global Instructions Tests ---

(deftest test-emit-global-get ()
  "Test global.get instruction emission"
  (let ((code (clysm:emit-global.get 5)))
    (is (listp code))
    (is-eql #x23 (first code))))  ; global.get opcode

(deftest test-emit-global-set ()
  "Test global.set instruction emission"
  (let ((code (clysm:emit-global.set 5)))
    (is (listp code))
    (is-eql #x24 (first code))))  ; global.set opcode

;;; --- Compile Defvar Tests ---

(deftest test-compile-defvar ()
  "Test compiling defvar creates a global"
  (let* ((context (clysm:compile-toplevel '(defvar *my-var* 10))))
    (is (clysm:codegen-context-p context))
    ;; Should be declared special
    (is (clysm:context-special-p context '*my-var*))
    ;; Should have a global index
    (is (clysm:context-special-global-index context '*my-var*))
    ;; Should have created a global in the module
    (is (>= (length (clysm:wasm-module-globals
                     (clysm:codegen-context-module context))) 1))))

(deftest test-compile-defvar-no-init ()
  "Test compiling defvar without initial value"
  (let* ((context (clysm:compile-toplevel '(defvar *uninit*))))
    (is (clysm:codegen-context-p context))
    (is (clysm:context-special-p context '*uninit*))
    (is (clysm:context-special-global-index context '*uninit*))))

(deftest test-compile-defvar-twice ()
  "Test that defvar doesn't create duplicate globals"
  (let* ((context (clysm:compile-toplevel '(defvar *dup* 1))))
    ;; First defvar
    (let ((first-idx (clysm:context-special-global-index context '*dup*)))
      ;; Compile same defvar again
      (clysm:compile-toplevel '(defvar *dup* 2) context)
      ;; Should have same global index
      (is-eql first-idx (clysm:context-special-global-index context '*dup*)))))

;;; --- Special Variable Access Tests ---

(deftest test-compile-special-var-access ()
  "Test accessing a special variable"
  (let* ((context (clysm:compile-toplevel '(defvar *acc-test* 0)))
         (module (clysm:codegen-context-module context))
         (registry (clysm:codegen-context-type-registry context))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context)))
    ;; Compile access to *acc-test*
    (let* ((ast (clysm:parse-sexp '*acc-test*))
           (code (clysm:compile-expression ast env)))
      (is (listp code))
      (is (> (length code) 0))
      ;; Should contain global.get (#x23)
      (is (member #x23 code)))))

(deftest test-compile-special-var-setq ()
  "Test assigning to a special variable"
  (let* ((context (clysm:compile-toplevel '(defvar *setq-test* 0)))
         (module (clysm:codegen-context-module context))
         (registry (clysm:codegen-context-type-registry context))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context)))
    ;; Compile setq of *setq-test*
    (let* ((ast (clysm:parse-sexp '(setq *setq-test* 42)))
           (code (clysm:compile-expression ast env)))
      (is (listp code))
      (is (> (length code) 0))
      ;; Should contain global.set (#x24)
      (is (member #x24 code)))))

;;; --- Special Let Binding Tests ---

(deftest test-compile-let-with-special ()
  "Test let binding with a special variable"
  (let* ((context (clysm:compile-toplevel '(defvar *let-test* 0)))
         (registry (clysm:codegen-context-type-registry context))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context)))
    ;; Compile let binding the special variable
    (let* ((ast (clysm:parse-sexp '(let ((*let-test* 100)) *let-test*)))
           (code (clysm:compile-expression ast env)))
      (is (listp code))
      (is (> (length code) 0))
      ;; Should contain global.get (save old) and global.set (set new)
      (is (member #x23 code))  ; global.get
      (is (member #x24 code)))))  ; global.set

(deftest test-env-special-p-not-by-name ()
  "Test that *foo* naming convention does not make a variable special"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (context (clysm:make-codegen-context :module module
                                               :type-registry registry))
         (env (clysm:make-compile-env :type-registry registry
                                       :codegen-context context)))
    ;; *foo* is NOT special unless explicitly declared
    (is-false (clysm:env-special-p env '*foo*))
    (is-false (clysm:env-special-p env '*bar*))

    ;; Even with earmuffs, need explicit declaration
    (clysm:context-declare-special context '*foo*)
    (is (clysm:env-special-p env '*foo*))
    (is-false (clysm:env-special-p env '*bar*))))

;;; ============================================================
;;; Macro System Tests (Phase 8)
;;; ============================================================

;;; --- Macro Registry Tests ---

(deftest test-register-macro ()
  "Test registering a macro"
  ;; Clean up first
  (clysm:unregister-macro 'test-macro-1)

  ;; Register a simple macro
  (clysm:register-macro 'test-macro-1
                        (lambda (form)
                          (declare (ignore form))
                          42))

  ;; Check it's registered
  (is (clysm:macrop 'test-macro-1))
  (is (functionp (clysm:get-macro-expander 'test-macro-1)))

  ;; Clean up
  (clysm:unregister-macro 'test-macro-1))

(deftest test-unregister-macro ()
  "Test unregistering a macro"
  (clysm:register-macro 'test-macro-2
                        (lambda (form) form))
  (is (clysm:macrop 'test-macro-2))

  (clysm:unregister-macro 'test-macro-2)
  (is-false (clysm:macrop 'test-macro-2)))

(deftest test-macrop ()
  "Test macrop predicate"
  (clysm:unregister-macro 'not-a-macro)

  (is-false (clysm:macrop 'not-a-macro))
  (is-false (clysm:macrop '+))
  (is-false (clysm:macrop 'if)))

;;; --- Macro Expansion Tests ---

(deftest test-clysm-macroexpand-1 ()
  "Test single macro expansion"
  ;; Register a test macro
  (clysm:register-macro 'double
                        (lambda (form)
                          `(+ ,(second form) ,(second form))))

  ;; Expand once
  (multiple-value-bind (expanded did-expand)
      (clysm:clysm-macroexpand-1 '(double 5))
    (is did-expand)
    (is (equal '(+ 5 5) expanded)))

  ;; Non-macro form doesn't expand
  (multiple-value-bind (expanded did-expand)
      (clysm:clysm-macroexpand-1 '(+ 1 2))
    (is-false did-expand)
    (is (equal '(+ 1 2) expanded)))

  (clysm:unregister-macro 'double))

(deftest test-clysm-macroexpand ()
  "Test full macro expansion"
  ;; Register nested macros
  (clysm:register-macro 'inc
                        (lambda (form)
                          `(+ 1 ,(second form))))
  (clysm:register-macro 'inc2
                        (lambda (form)
                          `(inc (inc ,(second form)))))

  ;; Full expansion (top-level only, like CL's macroexpand)
  (multiple-value-bind (expanded did-expand)
      (clysm:clysm-macroexpand '(inc2 x))
    (is did-expand)
    ;; inc2 -> (inc (inc x)) -> (+ 1 (inc x))
    ;; Stops because + is not a macro (inner inc is NOT expanded)
    (is (equal '(+ 1 (inc x)) expanded)))

  (clysm:unregister-macro 'inc)
  (clysm:unregister-macro 'inc2))

;;; --- Defmacro Parsing Tests ---

(deftest test-parse-defmacro ()
  "Test parsing defmacro"
  (clysm:unregister-macro 'my-when)

  (let ((ast (clysm:parse-sexp '(defmacro my-when (test &body body)
                                  `(if ,test (progn ,@body))))))
    (is (typep ast 'clysm:ast-defmacro))
    (is-eq 'my-when (clysm:ast-defmacro-name ast))
    (is (equal '(test &body body) (clysm:ast-defmacro-lambda-list ast)))
    ;; Macro should be registered
    (is (clysm:macrop 'my-when)))

  (clysm:unregister-macro 'my-when))

(deftest test-macro-expansion-in-parse ()
  "Test that macros are expanded during parsing"
  ;; Define a simple macro
  (clysm:register-macro 'always-42
                        (lambda (form)
                          (declare (ignore form))
                          42))

  ;; Parse a form using the macro
  (let ((ast (clysm:parse-sexp '(always-42))))
    ;; Should be expanded to 42 (a literal)
    (is (typep ast 'clysm:ast-literal))
    (is-eql 42 (clysm:ast-literal-value ast)))

  (clysm:unregister-macro 'always-42))

(deftest test-when-macro ()
  "Test a realistic when macro"
  (clysm:unregister-macro 'my-when)

  ;; Define when macro
  (clysm:parse-sexp '(defmacro my-when (test &body body)
                       `(if ,test (progn ,@body))))

  ;; Use the macro
  (let ((ast (clysm:parse-sexp '(my-when (< x 10) (+ x 1) (+ x 2)))))
    ;; Should expand to if form
    (is (typep ast 'clysm:ast-if))
    ;; test should be (< x 10) -> primitive-call
    (is (typep (clysm:ast-if-test ast) 'clysm:ast-primitive-call))
    ;; then should be progn with two forms
    (is (typep (clysm:ast-if-then ast) 'clysm:ast-progn))
    (is-eql 2 (length (clysm:ast-progn-forms (clysm:ast-if-then ast)))))

  (clysm:unregister-macro 'my-when))

(deftest test-unless-macro ()
  "Test an unless macro"
  (clysm:unregister-macro 'my-unless)

  ;; Define unless macro
  (clysm:parse-sexp '(defmacro my-unless (test &body body)
                       `(if ,test nil (progn ,@body))))

  ;; Use the macro
  (let ((ast (clysm:parse-sexp '(my-unless flag (do-something)))))
    (is (typep ast 'clysm:ast-if))
    ;; else should be progn
    (is (typep (clysm:ast-if-else ast) 'clysm:ast-progn)))

  (clysm:unregister-macro 'my-unless))

(deftest test-compile-defmacro ()
  "Test that defmacro compiles without generating code"
  (clysm:unregister-macro 'compile-test-macro)

  (let* ((context (clysm:compile-toplevel '(defmacro compile-test-macro (x)
                                              `(+ ,x 1)))))
    ;; Should return context
    (is (clysm:codegen-context-p context))
    ;; Macro should be registered
    (is (clysm:macrop 'compile-test-macro)))

  (clysm:unregister-macro 'compile-test-macro))

(deftest test-macro-with-rest ()
  "Test macro with &rest parameter"
  (clysm:unregister-macro 'my-list)

  (clysm:parse-sexp '(defmacro my-list (&rest items)
                       `(cons ,(first items)
                              ,(if (rest items)
                                   `(my-list ,@(rest items))
                                   nil))))

  ;; Should be able to use the macro
  (is (clysm:macrop 'my-list))

  (clysm:unregister-macro 'my-list))
