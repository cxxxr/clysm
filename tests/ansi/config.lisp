;;;; config.lisp - Configuration for ANSI test integration

(in-package #:cl-wasm/ansi-tests)

;;; Paths
(defvar *project-root*
  (asdf:system-source-directory :cl-wasm)
  "Root directory of the cl-wasm project.")

(defvar *ansi-test-directory*
  (merge-pathnames "vendor/ansi-test/" *project-root*)
  "Directory containing the ANSI test suite.")

(defvar *scripts-directory*
  (merge-pathnames "scripts/" *project-root*)
  "Directory containing helper scripts.")

(defvar *wasm-runner*
  (merge-pathnames "wasm-runner.js" *scripts-directory*)
  "Path to the WASM runner script.")

(defvar *temp-directory*
  (uiop:temporary-directory)
  "Directory for temporary WASM files.")

;;; Runtime settings
(defvar *wasm-runtime* :node
  "WASM runtime to use. Currently only :node is supported.")

(defvar *node-executable* "node"
  "Path to the Node.js executable.")

(defvar *timeout-seconds* 10
  "Timeout for individual test execution in seconds.")

;;; Test categories available in ansi-test
(defparameter *ansi-test-categories*
  '(:numbers
    :cons
    :data-and-control-flow
    :iteration
    :eval-and-compile
    :types-and-classes
    :conditions
    :symbols
    :packages
    :arrays
    :strings
    :sequences
    :hash-tables
    :filenames
    :files
    :streams
    :printer
    :reader
    :system-construction
    :environment)
  "All categories in the ANSI test suite.")

;;; Category to directory mapping
(defparameter *category-directories*
  '((:numbers . "numbers")
    (:cons . "cons")
    (:data-and-control-flow . "data-and-control-flow")
    (:iteration . "iteration")
    (:eval-and-compile . "eval-and-compile")
    (:types-and-classes . "types-and-classes")
    (:conditions . "conditions")
    (:symbols . "symbols")
    (:packages . "packages")
    (:arrays . "arrays")
    (:strings . "strings")
    (:sequences . "sequences")
    (:hash-tables . "hash-tables")
    (:filenames . "filenames")
    (:files . "files")
    (:streams . "streams")
    (:printer . "printer")
    (:reader . "reader")
    (:system-construction . "system-construction")
    (:environment . "environment"))
  "Mapping from category keyword to directory name.")

;;; Expected failures (tests known to fail)
(defvar *expected-failures* '()
  "List of test names that are expected to fail.")

;;; Tests to skip entirely (e.g., require unimplemented features)
(defvar *skip-tests* '()
  "List of test names to skip entirely.")

;;; Helper functions
(defun category-directory (category)
  "Get the directory path for a category."
  (let ((dir-name (cdr (assoc category *category-directories*))))
    (when dir-name
      (merge-pathnames (format nil "~A/" dir-name) *ansi-test-directory*))))

(defun list-categories ()
  "List all available test categories."
  *ansi-test-categories*)
