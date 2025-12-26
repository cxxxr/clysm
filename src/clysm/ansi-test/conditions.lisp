;;;; conditions.lisp - Error conditions for ANSI test harness
;;;;
;;;; Defines condition types for error handling in the test runner.

(in-package #:clysm/ansi-test)

;;; ==========================================================================
;;; T038: category-not-found-error condition
;;; ==========================================================================

(define-condition category-not-found-error (error)
  ((category :initarg :category
             :reader error-category
             :documentation "The category that was not found")
   (available :initarg :available
              :reader available-categories
              :documentation "List of available category names"))
  (:report (lambda (condition stream)
             (format stream "Category ~S not found. Available categories: ~{~A~^, ~}"
                     (error-category condition)
                     (available-categories condition))))
  (:documentation "Signaled when a specified test category does not exist."))
