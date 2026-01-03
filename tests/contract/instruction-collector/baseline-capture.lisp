;;;; Baseline Capture Utility for Instruction Collector Contract Tests
;;;; Captures Wasm bytecode before migration for byte-identical comparison

(defpackage #:clysm/tests/contract/instruction-collector/baseline-capture
  (:use #:cl)
  (:import-from #:clysm
                #:compile-to-wasm
                #:compile-to-instructions)
  (:export #:capture-baseline
           #:save-baseline
           #:load-baseline
           #:baseline-exists-p
           #:*baseline-directory*))

(in-package #:clysm/tests/contract/instruction-collector/baseline-capture)

(defparameter *baseline-directory*
  (merge-pathnames "baselines/"
                   (asdf:system-relative-pathname :clysm "tests/contract/instruction-collector/"))
  "Directory for storing baseline Wasm captures")

(defun ensure-baseline-directory ()
  "Ensure the baseline directory exists."
  (ensure-directories-exist *baseline-directory*))

(defun baseline-path (name)
  "Return the full path for a baseline file with the given NAME."
  (merge-pathnames (format nil "~A.wasm" name) *baseline-directory*))

(defun capture-baseline (form &key (env nil))
  "Compile FORM to Wasm bytecode and return as a byte vector.
   This captures the current state before migration."
  (let ((wasm-bytes (compile-to-wasm form :env env)))
    (if (typep wasm-bytes '(vector (unsigned-byte 8)))
        wasm-bytes
        (error "compile-to-wasm did not return a byte vector"))))

(defun save-baseline (name form &key (env nil) (overwrite nil))
  "Capture FORM as Wasm and save to baseline file NAME.
   If OVERWRITE is nil (default), error if file exists."
  (ensure-baseline-directory)
  (let ((path (baseline-path name)))
    (when (and (probe-file path) (not overwrite))
      (error "Baseline ~A already exists. Use :overwrite t to replace." name))
    (let ((bytes (capture-baseline form :env env)))
      (with-open-file (out path
                           :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (write-sequence bytes out))
      (format t "~&Saved baseline ~A (~D bytes)~%" name (length bytes))
      path)))

(defun load-baseline (name)
  "Load a previously saved baseline and return as byte vector.
   Signals error if baseline does not exist."
  (let ((path (baseline-path name)))
    (unless (probe-file path)
      (error "Baseline ~A does not exist at ~A" name path))
    (with-open-file (in path
                        :direction :input
                        :element-type '(unsigned-byte 8))
      (let* ((length (file-length in))
             (bytes (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence bytes in)
        bytes))))

(defun baseline-exists-p (name)
  "Return T if baseline NAME exists."
  (probe-file (baseline-path name)))

;;; High-level capture functions for specific migration targets

(defun capture-equalp-baseline ()
  "Capture baseline for (equalp x y) compilation."
  (save-baseline "equalp-baseline"
                 '(defun test-equalp (x y) (equalp x y))
                 :overwrite t))

(defun capture-equal-baseline ()
  "Capture baseline for (equal x y) compilation."
  (save-baseline "equal-baseline"
                 '(defun test-equal (x y) (equal x y))
                 :overwrite t))

(defun capture-primitive-arithmetic-baseline ()
  "Capture baseline for arithmetic primitives."
  (save-baseline "primitive-arithmetic-baseline"
                 '(defun test-arithmetic (a b)
                    (+ a b (* a b) (- a) (/ a b)))
                 :overwrite t))

(defun capture-primitive-comparison-baseline ()
  "Capture baseline for comparison primitives."
  (save-baseline "primitive-comparison-baseline"
                 '(defun test-comparison (a b)
                    (and (< a b) (> a b) (<= a b) (>= a b) (= a b) (/= a b)))
                 :overwrite t))

(defun capture-primitive-typep-baseline ()
  "Capture baseline for type predicates."
  (save-baseline "primitive-typep-baseline"
                 '(defun test-typep (x)
                    (list (consp x) (numberp x) (stringp x) (symbolp x)))
                 :overwrite t))
