;;;; phase13d-features.lisp - T023: Integration test for Phase 13D feature utilization
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Verifies that Phase 13D-1~6 features are properly utilized in Stage 1 generation

(defpackage #:clysm/tests/integration/stage1-phase13d
  (:use #:cl #:rove))

(in-package #:clysm/tests/integration/stage1-phase13d)

;;; ==========================================================================
;;; T023: Phase 13D Feature Utilization Integration Test
;;; ==========================================================================
;;;
;;; Phase 13D features to verify:
;;; - Phase 13D-1: ANSI CL array primitives (aref, svref)
;;; - Phase 13D-2: ANSI CL sequence operations (subseq, concatenate)
;;; - Phase 13D-3: Compile-time directives (eval-when)
;;; - Phase 13D-4: Global variable definitions (defvar, defparameter, defconstant)
;;; - Phase 13D-5: LOOP extension
;;; - Phase 13D-6: Control structure extension (handler-case, values, the, labels)

(defun phase13d-feature-present-p (feature-pattern source-dir)
  "Check if a Phase 13D feature pattern is present in the Stage 1 source."
  (let ((found nil))
    (dolist (file (uiop:directory-files source-dir "*.lisp"))
      (with-open-file (s file :direction :input)
        (let ((content (make-string (file-length s))))
          (read-sequence content s)
          (when (search feature-pattern content)
            (setf found t)
            (return)))))
    found))

;;; --------------------------------------------------------------------------
;;; Phase 13D-1: Array Primitives
;;; --------------------------------------------------------------------------

(deftest stage1-uses-aref ()
  "Verify Stage 1 source uses AREF (Phase 13D-1)."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm))))
    (ok (phase13d-feature-present-p "(aref " stage1-dir)
        "Stage 1 source uses AREF array access")))

;;; --------------------------------------------------------------------------
;;; Phase 13D-2: Sequence Operations
;;; --------------------------------------------------------------------------

(deftest stage1-uses-subseq ()
  "Verify Stage 1 source uses SUBSEQ (Phase 13D-2)."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm))))
    (ok (phase13d-feature-present-p "(subseq " stage1-dir)
        "Stage 1 source uses SUBSEQ sequence operation")))

;;; --------------------------------------------------------------------------
;;; Phase 13D-4: Global Variable Definitions
;;; --------------------------------------------------------------------------

(deftest stage1-uses-defvar ()
  "Verify Stage 1 source uses DEFVAR (Phase 13D-4)."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm))))
    (ok (phase13d-feature-present-p "(defvar " stage1-dir)
        "Stage 1 source uses DEFVAR global definitions")))

(deftest stage1-uses-defparameter ()
  "Verify Stage 1 source uses DEFPARAMETER (Phase 13D-4)."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm))))
    (ok (phase13d-feature-present-p "(defparameter " stage1-dir)
        "Stage 1 source uses DEFPARAMETER global definitions")))

;;; --------------------------------------------------------------------------
;;; Phase 13D-5: LOOP Extension
;;; --------------------------------------------------------------------------

(deftest stage1-uses-loop ()
  "Verify Stage 1 source uses LOOP (Phase 13D-5)."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm))))
    (ok (phase13d-feature-present-p "(loop " stage1-dir)
        "Stage 1 source uses LOOP macro")))

(deftest stage1-uses-loop-for ()
  "Verify Stage 1 source uses LOOP FOR iteration (Phase 13D-5)."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm))))
    (ok (phase13d-feature-present-p "loop for " stage1-dir)
        "Stage 1 source uses LOOP FOR iteration")))

;;; --------------------------------------------------------------------------
;;; Phase 13D-6: Control Structure Extension
;;; --------------------------------------------------------------------------

(deftest stage1-uses-handler-case ()
  "Verify Stage 1 source uses HANDLER-CASE (Phase 13D-6)."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm))))
    (ok (phase13d-feature-present-p "(handler-case" stage1-dir)
        "Stage 1 source uses HANDLER-CASE error handling")))

(deftest stage1-uses-values ()
  "Verify Stage 1 source uses VALUES (Phase 13D-6)."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm))))
    (ok (phase13d-feature-present-p "(values " stage1-dir)
        "Stage 1 source uses VALUES multiple values")))

;;; --------------------------------------------------------------------------
;;; Combined Feature Verification
;;; --------------------------------------------------------------------------

(deftest stage1-phase13d-features-comprehensive ()
  "Verify all critical Phase 13D features are utilized in Stage 1."
  (let ((stage1-dir (merge-pathnames "src/clysm/stage1/"
                                      (asdf:system-source-directory :clysm)))
        (features '(("(loop " . "LOOP (13D-5)")
                    ("(handler-case" . "HANDLER-CASE (13D-6)")
                    ("(values " . "VALUES (13D-6)")
                    ("(subseq " . "SUBSEQ (13D-2)")
                    ("(aref " . "AREF (13D-1)")))
        (all-present t))
    (dolist (feature features)
      (let ((pattern (car feature))
            (name (cdr feature)))
        (unless (phase13d-feature-present-p pattern stage1-dir)
          (setf all-present nil)
          (format t "~&Missing feature: ~A~%" name))))
    (ok all-present "All critical Phase 13D features are present in Stage 1 source")))

;;; --------------------------------------------------------------------------
;;; Compilation Verification
;;; --------------------------------------------------------------------------

(deftest stage1-phase13d-forms-compile ()
  "Verify Phase 13D feature forms compile individually."
  (let ((test-forms
          ;; Representative forms using Phase 13D features
          '((defun test-aref (arr) (aref arr 0))
            (defun test-subseq (s) (subseq s 0 1))
            (defun test-values () (values 1 2))
            (defvar *test-var* nil)
            (defconstant +test-const+ 42))))
    (dolist (form test-forms)
      (handler-case
          (let ((bytes (clysm:compile-to-wasm form)))
            (ok (and bytes (> (length bytes) 0))
                (format nil "Phase 13D form ~A compiles" (car form))))
        (error (e)
          (fail (format nil "Phase 13D form ~A failed: ~A" (car form) e)))))))
