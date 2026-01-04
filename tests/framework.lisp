;;;; tests/framework.lisp - Minimal Test Framework
;;;;
;;;; Self-contained test framework with no external dependencies.
;;;; Inspired by prove/rove but simplified for self-hosting.

(in-package #:clysm/tests)

;;; ============================================================
;;; Test Registry
;;; ============================================================

(defvar *test-suites* (make-hash-table :test 'equal)
  "Hash table mapping suite names to test lists.")

(defvar *current-suite* nil
  "The currently active test suite name.")

(defstruct test-case
  "A single test case."
  name
  suite
  function
  description)

(defstruct test-result
  "Result of running a test."
  test-case
  passed-p
  error-info
  duration)

;;; ============================================================
;;; Test Definition Macros
;;; ============================================================

(defmacro defsuite (name &optional description)
  "Define a test suite."
  `(progn
     (setf (gethash ',name *test-suites*) nil)
     (setf *current-suite* ',name)
     ',name))

(defmacro deftest (name &body body)
  "Define a test case in the current suite."
  (let ((suite (or *current-suite* :default)))
    `(progn
       (let ((test (make-test-case
                    :name ',name
                    :suite ',suite
                    :function (lambda () ,@body)
                    :description nil)))
         (push test (gethash ',suite *test-suites* nil))
         ',name))))

;;; ============================================================
;;; Assertion State
;;; ============================================================

(defvar *current-test* nil
  "The currently running test.")

(defvar *assertion-count* 0
  "Number of assertions in current test.")

(defvar *failure-count* 0
  "Number of failed assertions in current test.")

(defvar *failures* nil
  "List of failure descriptions in current test.")

(define-condition assertion-failure (error)
  ((description :initarg :description :reader failure-description)
   (expected :initarg :expected :reader failure-expected)
   (actual :initarg :actual :reader failure-actual))
  (:report (lambda (c s)
             (format s "Assertion failed: ~A~%  Expected: ~S~%  Actual: ~S"
                     (failure-description c)
                     (failure-expected c)
                     (failure-actual c)))))

;;; ============================================================
;;; Assertion Macros
;;; ============================================================

(defun record-assertion (passed-p description expected actual)
  "Record an assertion result."
  (incf *assertion-count*)
  (unless passed-p
    (incf *failure-count*)
    (push (list description expected actual) *failures*))
  passed-p)

(defmacro is (form &optional description)
  "Assert that FORM evaluates to a true value."
  (let ((desc (or description (format nil "~S" form))))
    `(record-assertion ,form ,desc t ,form)))

(defmacro is-true (form &optional description)
  "Assert that FORM evaluates to exactly T."
  (let ((result (gensym "RESULT"))
        (desc (or description (format nil "~S is true" form))))
    `(let ((,result ,form))
       (record-assertion (eq ,result t) ,desc t ,result))))

(defmacro is-false (form &optional description)
  "Assert that FORM evaluates to NIL."
  (let ((result (gensym "RESULT"))
        (desc (or description (format nil "~S is false" form))))
    `(let ((,result ,form))
       (record-assertion (null ,result) ,desc nil ,result))))

(defmacro is-equal (expected actual &optional description)
  "Assert that EXPECTED and ACTUAL are EQUAL."
  (let ((exp (gensym "EXPECTED"))
        (act (gensym "ACTUAL"))
        (desc (or description
                  (format nil "~S equals ~S" actual expected))))
    `(let ((,exp ,expected)
           (,act ,actual))
       (record-assertion (equal ,exp ,act) ,desc ,exp ,act))))

(defmacro is-eql (expected actual &optional description)
  "Assert that EXPECTED and ACTUAL are EQL."
  (let ((exp (gensym "EXPECTED"))
        (act (gensym "ACTUAL"))
        (desc (or description
                  (format nil "~S eql ~S" actual expected))))
    `(let ((,exp ,expected)
           (,act ,actual))
       (record-assertion (eql ,exp ,act) ,desc ,exp ,act))))

(defmacro is-eq (expected actual &optional description)
  "Assert that EXPECTED and ACTUAL are EQ."
  (let ((exp (gensym "EXPECTED"))
        (act (gensym "ACTUAL"))
        (desc (or description
                  (format nil "~S eq ~S" actual expected))))
    `(let ((,exp ,expected)
           (,act ,actual))
       (record-assertion (eq ,exp ,act) ,desc ,exp ,act))))

(defmacro signals (condition-type &body body)
  "Assert that BODY signals a condition of CONDITION-TYPE."
  (let ((signaled (gensym "SIGNALED")))
    `(let ((,signaled nil))
       (handler-case
           (progn ,@body)
         (,condition-type ()
           (setf ,signaled t)))
       (record-assertion ,signaled
                         ,(format nil "signals ~S" condition-type)
                         ',condition-type
                         (if ,signaled ',condition-type 'no-signal)))))

(defmacro finishes (&body body)
  "Assert that BODY completes without signaling an error."
  (let ((finished (gensym "FINISHED"))
        (err (gensym "ERROR")))
    `(let ((,finished nil)
           (,err nil))
       (handler-case
           (progn ,@body (setf ,finished t))
         (error (e)
           (setf ,err e)))
       (record-assertion ,finished
                         "finishes without error"
                         'no-error
                         (or ,err 'completed)))))

;;; ============================================================
;;; Test Execution
;;; ============================================================

(defun run-test-case (test-case)
  "Run a single test case and return a test-result."
  (let ((*current-test* test-case)
        (*assertion-count* 0)
        (*failure-count* 0)
        (*failures* nil)
        (start-time (get-internal-real-time))
        (error-info nil)
        (passed-p t))
    (handler-case
        (funcall (test-case-function test-case))
      (error (e)
        (setf error-info e)
        (setf passed-p nil)))
    (when (> *failure-count* 0)
      (setf passed-p nil))
    (make-test-result
     :test-case test-case
     :passed-p passed-p
     :error-info (or error-info (nreverse *failures*))
     :duration (/ (- (get-internal-real-time) start-time)
                  internal-time-units-per-second))))

(defun run-test (name &optional (suite *current-suite*))
  "Run a single test by name."
  (let ((tests (gethash suite *test-suites*)))
    (if-let (test (find name tests :key #'test-case-name))
      (run-test-case test)
      (error "Test ~S not found in suite ~S" name suite))))

(defun run-suite (suite-name)
  "Run all tests in a suite."
  (let ((tests (gethash suite-name *test-suites*)))
    (unless tests
      (error "Suite ~S not found" suite-name))
    (mapcar #'run-test-case (reverse tests))))
