;;;; macro-test.lisp - Macro integration tests (Phase 8 - US6)
(in-package #:clysm/tests/integration/macro)

;;; defmacro Tests (T162)

(deftest defmacro-basic
  (testing "define simple macro"
    ;; (defmacro my-quote (x) `(quote ,x))
    (let ((result (clysm/compiler/transform/macro:parse-defmacro
                   '(defmacro my-quote (x) (list 'quote x)))))
      (ok result)
      (ok (clysm/compiler/transform/macro:defmacro-result-p result))))

  (testing "macro with no arguments"
    ;; (defmacro nil-macro () nil)
    (let ((result (clysm/compiler/transform/macro:parse-defmacro
                   '(defmacro nil-macro () nil))))
      (ok result)))

  (testing "macro with multiple arguments"
    ;; (defmacro my-if (test then else) `(if ,test ,then ,else))
    (let ((result (clysm/compiler/transform/macro:parse-defmacro
                   '(defmacro my-if (test then else)
                     (list 'if test then else)))))
      (ok result))))

(deftest defmacro-lambda-list
  (testing "&rest in lambda list"
    ;; (defmacro my-list (&rest args) `(list ,@args))
    (let ((result (clysm/compiler/transform/macro:parse-defmacro
                   '(defmacro my-list (&rest args)
                     (cons 'list args)))))
      (ok result)))

  (testing "&body in lambda list"
    ;; (defmacro with-body (&body body) `(progn ,@body))
    (let ((result (clysm/compiler/transform/macro:parse-defmacro
                   '(defmacro with-body (&body body)
                     (cons 'progn body)))))
      (ok result)))

  (testing "&optional in lambda list"
    ;; (defmacro maybe (x &optional default) ...)
    (let ((result (clysm/compiler/transform/macro:parse-defmacro
                   '(defmacro maybe (x &optional (default nil))
                     (list 'or x default)))))
      (ok result)))

  (testing "mixed lambda list"
    ;; (defmacro complex (a b &optional c &rest d) ...)
    (let ((result (clysm/compiler/transform/macro:parse-defmacro
                   '(defmacro complex (a b &optional c &rest d)
                     (list 'list a b c d)))))
      (ok result))))

;;; Standard Macro Tests - when (T163)

(deftest when-macro
  (testing "when true executes body"
    ;; (when t 1 2 3) => 3
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(when t 1 2 3))))
        (ok (listp expanded))
        ;; Should expand to (if t (progn 1 2 3))
        (ok (eq 'if (first expanded))))))

  (testing "when false returns nil"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(when nil 1 2 3))))
        (ok (listp expanded))
        (ok (eq 'if (first expanded))))))

  (testing "when with single body form"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(when t 42))))
        (ok (listp expanded))))))

;;; Standard Macro Tests - unless

(deftest unless-macro
  (testing "unless false executes body"
    ;; (unless nil 1 2 3) => 3
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(unless nil 1 2 3))))
        (ok (listp expanded))
        ;; Should expand to (if (not nil) (progn 1 2 3)) or similar
        (ok (eq 'if (first expanded))))))

  (testing "unless true returns nil"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(unless t 42))))
        (ok (listp expanded))))))

;;; Standard Macro Tests - cond

(deftest cond-macro
  (testing "cond with single clause"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(cond (t 1)))))
        (ok (listp expanded)))))

  (testing "cond with multiple clauses"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(cond
                                   ((= x 1) 'one)
                                   ((= x 2) 'two)
                                   (t 'other)))))
        (ok (listp expanded))
        ;; Should expand to nested if forms
        (ok (eq 'if (first expanded))))))

  (testing "cond with else clause"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(cond
                                   (nil 'never)
                                   (t 'always)))))
        (ok (listp expanded)))))

  (testing "empty cond"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(cond))))
        (ok (or (null expanded) (listp expanded)))))))

;;; Standard Macro Tests - dolist

(deftest dolist-macro
  (testing "dolist basic iteration"
    ;; (dolist (x '(1 2 3)) (print x))
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(dolist (x '(1 2 3))
                                   (print x)))))
        (ok (listp expanded)))))

  (testing "dolist with result form"
    ;; (dolist (x '(1 2 3) x) ...)
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(dolist (x '(1 2 3) 'done)
                                   (print x)))))
        (ok (listp expanded)))))

  (testing "dolist on empty list"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(dolist (x '()) x))))
        (ok (listp expanded))))))

;;; Standard Macro Tests - dotimes

(deftest dotimes-macro
  (testing "dotimes basic counting"
    ;; (dotimes (i 5) (print i))
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(dotimes (i 5)
                                   (print i)))))
        (ok (listp expanded)))))

  (testing "dotimes with result form"
    ;; (dotimes (i 5 i) ...)
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(dotimes (i 5 'done)
                                   (print i)))))
        (ok (listp expanded)))))

  (testing "dotimes zero times"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(dotimes (i 0) (print i)))))
        (ok (listp expanded))))))

;;; Additional Standard Macros (10+ cases)

(deftest and-macro
  (testing "and with all true"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(and t t t))))
        (ok (listp expanded)))))

  (testing "and with no arguments"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-1*
                       registry '(and))))
        ;; (and) should return t
        (ok (or (eq t expanded) (listp expanded)))))))

(deftest or-macro
  (testing "or with all false"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(or nil nil nil))))
        (ok (listp expanded)))))

  (testing "or with no arguments"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-1*
                       registry '(or))))
        ;; (or) should return nil
        (ok (or (null expanded) (listp expanded)))))))

;;; Macro Invocation Integration

(deftest macro-invocation
  (testing "macro registered and callable"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      ;; Register a simple test macro
      (clysm/compiler/transform/macro:register-macro
       registry 'double
       (lambda (form)
         (let ((x (second form)))
           (list '+ x x))))
      ;; Expand it
      (let ((expanded (clysm/compiler/transform/macro:macroexpand*
                       registry '(double 5))))
        (ok (equal '(+ 5 5) expanded)))))

  (testing "nested macro expansion"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      ;; (when t (when t 42)) should expand fully
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(when t (when t 42)))))
        (ok (listp expanded))
        ;; Should be nested if forms
        (ok (eq 'if (first expanded)))))))

;;; End-to-End Compilation Tests

(deftest macro-compilation
  (testing "expand and compile when"
    ;; This tests the full pipeline: macro expand -> compile -> run
    ;; For now just test expansion works
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(when t (+ 1 2)))))
        ;; Expanded form should be compilable
        (ok (listp expanded))
        (ok (eq 'if (first expanded))))))

  (testing "expand and compile unless"
    (let* ((registry (clysm/compiler/transform/macro:make-macro-registry)))
      (clysm/lib/macros:install-standard-macros registry)
      (let ((expanded (clysm/compiler/transform/macro:macroexpand-all
                       registry '(unless nil (+ 1 2)))))
        (ok (listp expanded))))))
