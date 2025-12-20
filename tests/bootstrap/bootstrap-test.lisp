;;;; bootstrap-test.lisp - Bootstrap compilation test
;;;;
;;;; Tests that the compiler can compile non-trivial programs
;;;; using the features needed for self-compilation.

(in-package #:clysm/tests)

(def-suite bootstrap
  :description "Bootstrap compilation tests"
  :in :clysm)

(in-suite bootstrap)

;;; A mini-compiler that uses most of the features needed for bootstrap
(defparameter *mini-compiler-source*
  '(;; Define a symbol table structure
    (defstruct symbol-entry name value)

    ;; Create and use a hash table for symbol lookup
    (defun make-symbol-table ()
      (make-hash-table))

    (defun add-symbol (table name value)
      (let ((entry (make-symbol-entry)))
        (setf (symbol-entry-name entry) name)
        (setf (symbol-entry-value entry) value)
        (sethash name entry table)
        entry))

    (defun lookup-symbol (table name)
      (gethash name table))

    ;; A simple AST node structure
    (defstruct ast-node kind value children)

    ;; Build an AST from a simple expression
    (defun build-ast (expr)
      (cond
        ((numberp expr)
         (let ((node (make-ast-node)))
           (setf (ast-node-kind node) 'number)
           (setf (ast-node-value node) expr)
           node))
        ((symbolp expr)
         (let ((node (make-ast-node)))
           (setf (ast-node-kind node) 'symbol)
           (setf (ast-node-value node) expr)
           node))
        ((consp expr)
         (let ((node (make-ast-node)))
           (setf (ast-node-kind node) 'call)
           (setf (ast-node-value node) (car expr))
           ;; Build children recursively
           (let ((children nil))
             (dolist (child (cdr expr))
               (setf children (cons (build-ast child) children)))
             (setf (ast-node-children node) (reverse children)))
           node))
        (t
         (error "Unknown expression type"))))

    ;; Evaluate a simple expression (numbers and +/-)
    (defun eval-ast (node env)
      (case (ast-node-kind node)
        (number (ast-node-value node))
        (symbol (lookup-symbol env (ast-node-value node)))
        (call
         (let ((op (ast-node-value node))
               (args (ast-node-children node)))
           (cond
             ((eq op '+)
              (+ (eval-ast (first args) env)
                 (eval-ast (second args) env)))
             ((eq op '-)
              (- (eval-ast (first args) env)
                 (eval-ast (second args) env)))
             ((eq op '*)
              (* (eval-ast (first args) env)
                 (eval-ast (second args) env)))
             (t 0))))
        (t 0)))

    ;; Main test function
    (defun test-mini-compiler ()
      (let ((env (make-symbol-table)))
        ;; Add some symbols
        (add-symbol env 'x 10)
        (add-symbol env 'y 20)
        ;; Build and evaluate an expression: (+ (* x 2) y)
        (let ((ast (build-ast '(+ (* x 2) y))))
          (eval-ast ast env))))))

(test compile-mini-compiler
  "Test compiling a mini-compiler program."
  (let* ((module (clysm/compiler:compile-module *mini-compiler-source*))
         (bytes (clysm/wasm:encode-module module)))
    ;; Should produce a valid WASM module
    (is (> (length bytes) 100))
    ;; WASM magic number
    (is (= (aref bytes 0) #x00))
    (is (= (aref bytes 1) #x61))
    (is (= (aref bytes 2) #x73))
    (is (= (aref bytes 3) #x6d))))

;;; Test using multiple values
(test compile-multiple-values-usage
  "Test compiling code that uses multiple values."
  (let* ((module (clysm/compiler:compile-module
                  '((defun get-pair ()
                      (values 10 20))
                    (defun sum-pair ()
                      (multiple-value-bind (a b) (get-pair)
                        (+ a b))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 50))))

;;; Test using destructuring-bind
(test compile-destructuring-usage
  "Test compiling code that uses destructuring-bind."
  (let* ((module (clysm/compiler:compile-module
                  '((defun process-list (lst)
                      (destructuring-bind (first second rest) lst
                        (+ first second))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 50))))

;;; Test complex control flow
(test compile-complex-control-flow
  "Test compiling code with complex control flow."
  (let* ((module (clysm/compiler:compile-module
                  '((defun classify (x)
                      (cond
                        ((< x 0) -1)
                        ((= x 0) 0)
                        ((< x 10) 1)
                        ((< x 100) 2)
                        (t 3)))
                    (defun count-classified (lst)
                      (let ((neg 0) (zero 0) (small 0) (med 0) (large 0))
                        (dolist (x lst)
                          (case (classify x)
                            (-1 (setf neg (+ neg 1)))
                            (0 (setf zero (+ zero 1)))
                            (1 (setf small (+ small 1)))
                            (2 (setf med (+ med 1)))
                            (t (setf large (+ large 1)))))
                        (+ neg zero small med large))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 100))))

;;; Test labels (local recursive functions)
(test compile-labels-usage
  "Test compiling code that uses labels."
  (let* ((module (clysm/compiler:compile-module
                  '((defun tree-sum (tree)
                      (labels ((sum-node (node)
                                 (if (consp node)
                                     (+ (sum-node (car node))
                                        (sum-node (cdr node)))
                                     (if (numberp node) node 0))))
                        (sum-node tree))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 50))))
