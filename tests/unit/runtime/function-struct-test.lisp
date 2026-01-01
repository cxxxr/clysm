;;;; function-struct-test.lisp - Unit tests for runtime-function struct
;;;; Feature 001-runtime-library-system
;;;; Task T025: Unit test for runtime-function struct

(in-package #:clysm/tests)

(deftest runtime-function-has-required-slots ()
  "Verify runtime-function struct has all required slots"
  (testing "runtime-function struct slots"
    (let ((fn (clysm/runtime-library:make-runtime-function
               :name 'assoc
               :lambda-list '(item alist &key key test test-not)
               :body '((cond))
               :source-file #p"/tmp/test.lisp"
               :compiled-index nil
               :dependencies '(funcall))))
      (ok (clysm/runtime-library:runtime-function-p fn)
          "make-runtime-function creates a runtime-function")
      (ok (eq 'assoc (clysm/runtime-library:runtime-function-name fn))
          "name slot accessible")
      (ok (equal '(item alist &key key test test-not)
                 (clysm/runtime-library:runtime-function-lambda-list fn))
          "lambda-list slot accessible")
      (ok (equal '((cond)) (clysm/runtime-library:runtime-function-body fn))
          "body slot accessible")
      (ok (pathnamep (clysm/runtime-library:runtime-function-source-file fn))
          "source-file slot accessible")
      (ok (null (clysm/runtime-library:runtime-function-compiled-index fn))
          "compiled-index initially nil")
      (ok (equal '(funcall) (clysm/runtime-library:runtime-function-dependencies fn))
          "dependencies slot accessible"))))

(deftest runtime-function-name-is-symbol ()
  "Verify name slot stores symbol"
  (testing "name slot type"
    (let ((fn (clysm/runtime-library:make-runtime-function
               :name 'member
               :lambda-list '(item list)
               :body '(list))))
      (ok (symbolp (clysm/runtime-library:runtime-function-name fn))
          "name is a symbol"))))

(deftest runtime-function-tracks-dependencies ()
  "Verify dependencies slot stores list of function names"
  (testing "dependencies tracking"
    (let ((fn (clysm/runtime-library:make-runtime-function
               :name 'assoc-if
               :lambda-list '(predicate alist &key key)
               :body '(nil)
               :dependencies '(assoc funcall))))
      (ok (listp (clysm/runtime-library:runtime-function-dependencies fn))
          "dependencies is a list")
      (ok (every #'symbolp (clysm/runtime-library:runtime-function-dependencies fn))
          "all dependencies are symbols")
      (ok (= 2 (length (clysm/runtime-library:runtime-function-dependencies fn)))
          "correct number of dependencies"))))

(deftest runtime-module-has-required-slots ()
  "Verify runtime-module struct has all required slots"
  (testing "runtime-module struct slots"
    (let* ((fn1 (clysm/runtime-library:make-runtime-function
                 :name 'assoc :lambda-list '(item alist) :body '(nil)))
           (fn2 (clysm/runtime-library:make-runtime-function
                 :name 'member :lambda-list '(item list) :body '(nil)))
           (mod (clysm/runtime-library:make-runtime-module
                 :name :list-ops
                 :source-file #p"/tmp/list-ops.lisp"
                 :functions (list fn1 fn2)
                 :exports '(assoc member))))
      (ok (clysm/runtime-library:runtime-module-p mod)
          "make-runtime-module creates a runtime-module")
      (ok (eq :list-ops (clysm/runtime-library:runtime-module-name mod))
          "name slot accessible")
      (ok (pathnamep (clysm/runtime-library:runtime-module-source-file mod))
          "source-file slot accessible")
      (ok (= 2 (length (clysm/runtime-library:runtime-module-functions mod)))
          "functions slot holds list of functions")
      (ok (equal '(assoc member) (clysm/runtime-library:runtime-module-exports mod))
          "exports slot accessible"))))
