;;;; transformer.lisp - Transform ANSI tests for cl-wasm execution

(in-package #:cl-wasm/ansi-tests)

;;; Test specification structure

(defstruct test-spec
  "Specification of an ANSI test."
  (name nil :type symbol)
  (form nil)
  (expected nil)
  (notes nil :type list)
  (category nil :type (or keyword null))
  (source-file nil :type (or pathname null))
  (skip-reason nil :type (or string null)))

;;; Parsing deftest forms

(defun deftest-form-p (form)
  "Check if FORM is a deftest form."
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) "DEFTEST")))

(defun parse-deftest (form)
  "Parse a (deftest name form expected) into a test-spec.
   Returns NIL if the form is not a valid deftest."
  (when (and (deftest-form-p form)
             (>= (length form) 3))
    (let* ((name (second form))
           (rest (cddr form))
           ;; Handle properties like :notes
           (props nil)
           (test-form nil)
           (expected nil))
      ;; Parse properties (keywords)
      (loop while (and (consp rest) (keywordp (car rest)))
            do (push (car rest) props)
               (push (cadr rest) props)
               (setf rest (cddr rest)))
      ;; Remaining should be form and expected
      (when (>= (length rest) 2)
        (setf test-form (car rest))
        (setf expected (cdr rest)))  ; All remaining values are expected
      (when (and name test-form)
        (make-test-spec
         :name name
         :form test-form
         :expected (if (= (length expected) 1)
                       (first expected)
                       expected)
         :notes (getf (nreverse props) :notes))))))

;;; Reading test files

(defun read-test-file (filepath)
  "Read all deftest forms from a file.
   Returns a list of test-spec objects."
  (let ((tests nil))
    (with-open-file (stream filepath :direction :input
                                     :external-format :utf-8
                                     :if-does-not-exist nil)
      (when stream
        (let ((*package* (find-package :cl-user))
              (*read-eval* nil))
          (handler-case
              (loop for form = (read stream nil :eof)
                    until (eq form :eof)
                    do (when (deftest-form-p form)
                         (let ((spec (parse-deftest form)))
                           (when spec
                             (setf (test-spec-source-file spec) filepath)
                             (push spec tests)))))
            (error (e)
              (warn "Error reading ~A: ~A" filepath e))))))
    (nreverse tests)))

(defun find-test-files (directory)
  "Find all .lsp test files in a directory."
  (when (uiop:directory-exists-p directory)
    (uiop:directory-files directory "*.lsp")))

(defun load-category-tests (category)
  "Load all tests for a category."
  (let ((dir (category-directory category)))
    (when dir
      (let ((tests nil))
        (dolist (file (find-test-files dir))
          ;; Skip load.lsp and auxiliary files
          (unless (member (pathname-name file) '("load" "aux") :test #'string-equal)
            (setf tests (append tests (read-test-file file)))))
        ;; Set category on all tests
        (dolist (test tests)
          (setf (test-spec-category test) category))
        tests))))

;;; Filtering tests

(defun filter-runnable-tests (tests)
  "Filter tests to those that can potentially run on cl-wasm."
  (remove-if-not (lambda (test)
                   (runnable-test-p (test-spec-form test)
                                    (test-spec-expected test)))
                 tests))

(defun filter-simple-tests (tests)
  "Filter to only simple, self-contained tests."
  (remove-if (lambda (test)
               (let ((form (test-spec-form test)))
                 ;; Skip tests that use global variables like *numbers*
                 (or (references-global-p form)
                     ;; Skip tests with loop
                     (uses-loop-p form)
                     ;; Skip tests with flet/labels (closures work but complex)
                     (uses-local-functions-p form))))
             tests))

(defun references-global-p (form)
  "Check if form references special variables (starts with *)."
  (cond
    ((null form) nil)
    ((symbolp form)
     (let ((name (symbol-name form)))
       (and (> (length name) 2)
            (char= (char name 0) #\*)
            (char= (char name (1- (length name))) #\*))))
    ((atom form) nil)
    (t (or (references-global-p (car form))
           (references-global-p (cdr form))))))

(defun uses-loop-p (form)
  "Check if form uses LOOP macro."
  (cond
    ((null form) nil)
    ((eq form 'loop) t)
    ((atom form) nil)
    (t (or (uses-loop-p (car form))
           (uses-loop-p (cdr form))))))

(defun uses-local-functions-p (form)
  "Check if form uses flet or labels."
  (cond
    ((null form) nil)
    ((member form '(flet labels)) t)
    ((atom form) nil)
    (t (or (uses-local-functions-p (car form))
           (uses-local-functions-p (cdr form))))))

;;; Transform for WASM execution

(defun wrap-test-for-wasm (test-spec)
  "Wrap a test form for WASM compilation.
   Returns a list of forms to compile."
  (let ((form (test-spec-form test-spec)))
    ;; Wrap in a function that returns the result
    `((defun test-main ()
        ,form))))

(defun compile-test-to-wasm (test-spec)
  "Compile a test-spec to WASM bytes.
   Returns WASM bytes or signals an error."
  (let ((forms (wrap-test-for-wasm test-spec)))
    (let ((module (cl-wasm/compiler:compile-module forms)))
      (cl-wasm/wasm:encode-module module))))

;;; Test categorization helpers

(defun categorize-test (test-spec)
  "Categorize a test as :simple, :moderate, or :complex."
  (let ((form (test-spec-form test-spec))
        (expected (test-spec-expected test-spec)))
    (cond
      ;; Simple: constant expression
      ((constantp form) :simple)
      ;; Simple: single function call with constants
      ((and (consp form)
            (every #'constantp (cdr form)))
       :simple)
      ;; Moderate: nested expressions, no control flow
      ((not (uses-control-flow-p form)) :moderate)
      ;; Complex: everything else
      (t :complex))))

(defun uses-control-flow-p (form)
  "Check if form uses control flow constructs."
  (cond
    ((null form) nil)
    ((member form '(if when unless cond and or block return-from
                    dotimes dolist loop tagbody go)) t)
    ((atom form) nil)
    ((member (car form) '(if when unless cond and or block return-from
                          dotimes dolist loop tagbody go)) t)
    (t (or (uses-control-flow-p (car form))
           (uses-control-flow-p (cdr form))))))

;;; Summary functions

(defun summarize-category-tests (category)
  "Get a summary of tests in a category."
  (let* ((all-tests (load-category-tests category))
         (runnable (filter-runnable-tests all-tests))
         (simple (filter-simple-tests runnable)))
    (list :category category
          :total (length all-tests)
          :runnable (length runnable)
          :simple (length simple))))
