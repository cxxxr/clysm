;;;; loader.lisp - Test file parsing and loading
;;;;
;;;; Parses DEFTEST forms from the pfdietz/ansi-test suite.

(in-package #:clysm/ansi-test)

;;; ==========================================================================
;;; Configuration
;;; ==========================================================================

(defparameter *ansi-test-root*
  (merge-pathnames #p"ansi-test/" (asdf:system-source-directory :clysm/ansi-test))
  "Root directory of the pfdietz/ansi-test suite.")

;;; ==========================================================================
;;; T023: parse-deftest function
;;; ==========================================================================

(defun parse-deftest (form category &optional source-file)
  "Parse a DEFTEST form into a test-case structure.
FORM should be (deftest name body expected-values...).
Returns a test-case or NIL if form is not a valid deftest."
  (when (and (consp form)
             (eq (car form) 'deftest)
             (>= (length form) 3))
    (let* ((name (second form))
           (body (third form))
           (expected-values (cdddr form)))
      (make-test-case
       :name name
       :category category
       :source-file source-file
       :form body
       :expected-values expected-values))))

;;; ==========================================================================
;;; T024: load-file-tests function
;;; ==========================================================================

(defun load-file-tests (filepath category)
  "Load all DEFTEST forms from FILEPATH and return list of test-cases."
  (let ((tests '()))
    (handler-case
        (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
          (when stream
            (let ((*package* (find-package :clysm/ansi-test))
                  (*read-eval* nil))
              (loop for form = (read stream nil :eof)
                    until (eq form :eof)
                    when (and (consp form) (eq (car form) 'deftest))
                      do (let ((tc (parse-deftest form category filepath)))
                           (when tc (push tc tests)))))))
      (error (e)
        (warn "Error loading ~A: ~A" filepath e)))
    (nreverse tests)))

;;; ==========================================================================
;;; T025: load-category-tests function
;;; ==========================================================================

(defun load-category-tests (category-name &optional (root *ansi-test-root*))
  "Load all tests from a category directory.
Returns list of test-case structures."
  (let* ((category-dir (merge-pathnames (make-pathname :directory (list :relative category-name))
                                        root))
         (lsp-files (directory (merge-pathnames "*.lsp" category-dir)))
         (all-tests '()))
    (dolist (file lsp-files)
      (let ((tests (load-file-tests file category-name)))
        (setf all-tests (nconc all-tests tests))))
    all-tests))

;;; ==========================================================================
;;; T026: list-categories function
;;; ==========================================================================

(defun list-categories (&key (root *ansi-test-root*) include-skipped)
  "List all available test categories from the ansi-test directory.
Returns list of (name :count N :status :active/:skipped).
If INCLUDE-SKIPPED is nil, omits categories in *default-skip-registry*."
  (let* ((subdirs (directory (merge-pathnames "*/" root)))
         (skipped-categories (skip-registry-unsupported-categories *default-skip-registry*))
         (categories '()))
    (dolist (dir subdirs)
      (let* ((name (car (last (pathname-directory dir))))
             (is-skipped (member name skipped-categories :test #'string-equal)))
        ;; Skip non-test directories
        (when (and name
                   (not (member name '("auxiliary" "beyond-ansi" "bugs" "doc"
                                       "misc" "random" "rctest" "sandbox")
                                :test #'string-equal))
                   (or include-skipped (not is-skipped)))
          (let* ((lsp-files (directory (merge-pathnames "*.lsp" dir)))
                 (test-count (loop for file in lsp-files
                                   sum (count-tests-in-file file))))
            (push (list name
                        :count test-count
                        :status (if is-skipped :skipped :active))
                  categories)))))
    (sort categories #'string< :key #'car)))

(defun count-tests-in-file (filepath)
  "Count the number of DEFTEST forms in a file."
  (let ((count 0))
    (handler-case
        (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
          (when stream
            (let ((*package* (find-package :clysm/ansi-test))
                  (*read-eval* nil))
              (loop for form = (read stream nil :eof)
                    until (eq form :eof)
                    when (and (consp form) (eq (car form) 'deftest))
                      do (incf count)))))
      (error () nil))
    count))
