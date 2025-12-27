(defpackage :clysm-tests/unit/validation/feature-registry
  (:use :cl :rove)
  (:local-nicknames (:v :clysm-validation)))

(in-package :clysm-tests/unit/validation/feature-registry)

;;; T005: Unit tests for CL-Feature struct

(deftest cl-feature-struct-creation
  "Test that CL-Feature struct can be created with required fields"
  (let ((feature (v:make-cl-feature :symbol 'defun
                                    :category :special-form
                                    :status :supported
                                    :notes nil)))
    (ok (v:cl-feature-p feature) "Should create a cl-feature struct")
    (ok (eq (v:cl-feature-symbol feature) 'defun) "Should store symbol")
    (ok (eq (v:cl-feature-category feature) :special-form) "Should store category")
    (ok (eq (v:cl-feature-status feature) :supported) "Should store status")
    (ok (null (v:cl-feature-notes feature)) "Should store notes")))

(deftest cl-feature-struct-with-notes
  "Test CL-Feature struct with notes for partial support"
  (let ((feature (v:make-cl-feature :symbol 'loop
                                    :category :macro
                                    :status :partial
                                    :notes "Only :for :collect :do supported")))
    (ok (eq (v:cl-feature-status feature) :partial) "Should have partial status")
    (ok (stringp (v:cl-feature-notes feature)) "Should have string notes")))

(deftest cl-feature-status-categories
  "Test all valid status categories"
  (dolist (status '(:supported :partial :unsupported :internal))
    (let ((feature (v:make-cl-feature :symbol 'test-sym
                                      :category :function
                                      :status status
                                      :notes nil)))
      (ok (eq (v:cl-feature-status feature) status)
          (format nil "Should accept ~A status" status)))))

(deftest cl-feature-category-types
  "Test all valid category types"
  (dolist (category '(:special-form :macro :function :type :declaration))
    (let ((feature (v:make-cl-feature :symbol 'test-sym
                                      :category category
                                      :status :supported
                                      :notes nil)))
      (ok (eq (v:cl-feature-category feature) category)
          (format nil "Should accept ~A category" category)))))

;;; T009: Unit tests for *clysm-features* hash-table lookup

(deftest clysm-features-hash-table-exists
  "Test that *clysm-features* is a hash-table"
  (ok (hash-table-p v:*clysm-features*) "Should be a hash-table"))

(deftest clysm-features-contains-basic-forms
  "Test that registry contains essential CL forms"
  (dolist (sym '(defun defmacro let let* lambda if progn))
    (let ((feature (gethash sym v:*clysm-features*)))
      (ok feature (format nil "Should contain ~A" sym))
      (when feature
        (ok (v:cl-feature-p feature) "Should be a cl-feature struct")))))

;;; T014: Unit tests for feature-status lookup function

(deftest feature-status-supported
  "Test feature-status returns :supported for known supported symbols"
  (ok (eq (v:feature-status 'defun) :supported)
      "defun should be supported"))

(deftest feature-status-partial
  "Test feature-status returns :partial for partially supported symbols"
  (ok (eq (v:feature-status 'loop) :partial)
      "loop should have partial support"))

(deftest feature-status-unknown
  "Test feature-status returns :unknown for unknown symbols"
  (ok (eq (v:feature-status 'unknown-nonexistent-symbol-xyz) :unknown)
      "Unknown symbol should return :unknown"))

(deftest feature-status-with-notes
  "Test feature-status-with-notes returns both status and notes"
  (multiple-value-bind (status notes)
      (v:feature-status-with-notes 'loop)
    (ok (eq status :partial) "loop should have partial status")
    (ok (or (null notes) (stringp notes)) "Notes should be nil or string")))
