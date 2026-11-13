;;;; clysm-test.lisp — a small test library (portable Common Lisp)
;;;;
;;;;   (deftest parse-empty () (is (equal (parse "") nil)))
;;;;   (deftest bad-input () (signals parse-error (parse "(")))
;;;;   (run-tests-and-exit)    ; exit status 0 when every test passed
;;;;
;;;; A test is a function; IS records a failure (with the form and its
;;;; values) and goes on, an error signalled in a test fails it.  RUN-TESTS
;;;; runs the tests (all, or those of a package, or those whose name
;;;; contains a string) and returns true when all passed.

(defpackage :clysm-test
  (:use :cl)
  (:export #:deftest #:is #:is-equal #:signals #:skip #:run-tests #:run-tests-and-exit
           #:*tests* #:*verbose*))
(in-package :clysm-test)

(defvar *tests* nil "The tests, in the order they were defined: (name . function).")
(defvar *verbose* nil "When true, each test that passes is printed too.")
(defvar *failures* nil)
(defvar *checks* 0)

(defmacro deftest (name () &body body)
  "Define the test NAME (a symbol) with BODY."
  `(progn
     (let ((entry (assoc ',name *tests*)))
       (if entry
           (setf (cdr entry) (lambda () ,@body))
           (setf *tests* (append *tests* (list (cons ',name (lambda () ,@body)))))))
     ',name))

(defun %fail (format-control &rest args)
  (push (apply #'format nil format-control args) *failures*))

(defmacro is (form &optional description)
  "Check that FORM is true."
  (let ((v (gensym)))
    `(let ((,v (multiple-value-list ,form)))
       (incf *checks*)
       (unless (car ,v)
         (%fail "~@[~A: ~]~S was false~@[ (~{~S~^ ~})~]" ,description ',form (cdr ,v)))
       (car ,v))))

(defmacro is-equal (expected form &key (test '#'equal))
  "Check that FORM's value is EXPECTED (by TEST, EQUAL by default)."
  (let ((e (gensym)) (v (gensym)))
    `(let ((,e ,expected) (,v ,form))
       (incf *checks*)
       (unless (funcall ,test ,e ,v)
         (%fail "~S is ~S, expected ~S" ',form ,v ,e))
       ,v)))

(defmacro signals (type &body body)
  "Check that BODY signals a condition of TYPE."
  `(progn
     (incf *checks*)
     (block signals
       (handler-case (progn ,@body)
         (,type () (return-from signals t)))
       (%fail "~S did not signal ~S" '(progn ,@body) ',type)
       nil)))

(define-condition skip-test (condition) ((reason :initarg :reason :reader skip-reason)))
(defun skip (&optional (reason "skipped")) (signal 'skip-test :reason reason) (throw 'skip reason))

(defun %selected (filter)
  (remove-if-not (lambda (entry)
                   (let ((name (car entry)))
                     (cond ((null filter) t)
                           ((packagep filter) (eq (symbol-package name) filter))
                           ((stringp filter) (search (string-upcase filter) (symbol-name name)))
                           (t (member name (if (listp filter) filter (list filter)))))))
                 *tests*))

(defun run-tests (&key only (stream *standard-output*))
  "Run the tests (ONLY: a package, a substring of the names, or a list of
names) and report on STREAM; true when every test passed."
  (let ((passed 0) (failed 0) (skipped 0) (*checks* 0))
    (dolist (entry (%selected only))
      (let ((*failures* nil) (skip nil))
        (setf skip
              (catch 'skip
                (handler-case (progn (funcall (cdr entry)) nil)
                  (error (c) (%fail "error: ~A" c) nil))))
        (cond (skip (incf skipped) (format stream "~&skip ~(~S~): ~A~%" (car entry) skip))
              (*failures*
               (incf failed)
               (format stream "~&FAIL ~(~S~)~%~{  ~A~%~}" (car entry) (reverse *failures*)))
              (t (incf passed) (when *verbose* (format stream "~&ok   ~(~S~)~%" (car entry)))))))
    (format stream "~&~D passed, ~D failed~@[, ~D skipped~] (~D checks)~%"
            passed failed (and (plusp skipped) skipped) *checks*)
    (finish-output stream)
    (zerop failed)))

(defun %exit (code)
  #+clysm (sys::%exit code)
  #+sbcl (sb-ext:exit :code code :abort t)
  #-(or clysm sbcl) (error "exit with ~D" code))

(defun run-tests-and-exit (&rest args)
  "RUN-TESTS, then exit with status 0 if every test passed, 1 otherwise."
  (%exit (if (apply #'run-tests args) 0 1)))
