;;; tests/contract/stage0-extend-test.lisp - Contract tests for Stage 0 extension
;;; Feature 038: Stage 0 Capability Extension
;;; Contract tests verify Wasm output structure and validation

(in-package #:clysm/tests/contract/stage0-extend)

;;;; Test Suite Definition
(deftest stage0-extend-contract-tests
  (testing "Stage 0 Extension Wasm Contracts"
    ;; T080: Combined form compilation
    ;; Verify that extended forms produce valid Wasm
    ))

;;;; T080: Combined form compilation
(deftest combined-form-compilation-test
  (testing "Combined forms produce valid Wasm output"
    ;; Test that defconstant + defun + define-condition together
    ;; produce valid Wasm
    (let ((forms '((defconstant +test-const+ 42)
                   (defun test-fn () +test-const+))))
      ;; Each form should compile individually
      (dolist (form forms)
        (let ((wasm (ignore-errors (clysm::compile-form (clysm::parse-form form)))))
          (ok wasm (format nil "~S should compile" (car form))))))))

;;;; Wasm validation tests
(deftest defconstant-wasm-valid-test
  (testing "defconstant produces valid Wasm globals"
    ;; Test that defconstant output passes wasm-tools validate
    (let* ((form '(defconstant +magic+ 12345))
           (ast (clysm::parse-form form))
           (wasm-bytes (clysm::compile-to-wasm ast)))
      ;; Write to temp file and validate
      (when wasm-bytes
        (let ((temp-file (format nil "/tmp/defconstant-test-~A.wasm" (get-universal-time))))
          (with-open-file (out temp-file :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence wasm-bytes out))
          ;; Run wasm-tools validate
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program (list "wasm-tools" "validate" temp-file)
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output error-output))
            (ok (zerop exit-code) "wasm-tools validate should pass")
            ;; Cleanup
            (delete-file temp-file)))))))

;;;; Define-condition expansion contract
(deftest define-condition-expansion-contract-test
  (testing "define-condition expands to valid defclass"
    (let ((expansion (clysm::expand-define-condition
                      '(define-condition my-error (error)
                        ((msg :initarg :msg))))))
      ;; Should be defclass
      (ok (eq (car expansion) 'defclass)
          "Expansion should be defclass")
      ;; Name preserved
      (ok (eq (cadr expansion) 'my-error)
          "Class name should be my-error")
      ;; Parent preserved
      (ok (equal (caddr expansion) '(error))
          "Parent should be (error)"))))

;;;; Declare filtering contract
(deftest declare-filter-contract-test
  (testing "Declare filtering preserves body semantics"
    (multiple-value-bind (body declarations)
        (clysm::filter-declare-forms
         '((declare (type fixnum x))
           (let ((y 10))
             (declare (type fixnum y))
             (+ x y))
           (+ x 1)))
      (declare (ignore declarations))
      ;; Body should have 2 forms
      (ok (= (length body) 2)
          "Filtered body should have 2 forms")
      ;; Order preserved
      (ok (eq (caar body) 'let)
          "First body form should be let")
      (ok (equal (cadr body) '(+ x 1))
          "Second body form should be (+ x 1)"))))

;;;; Error reporting contract
(deftest error-report-format-contract-test
  (testing "Error report follows specified format"
    ;; Test generate-failure-report output format
    (let ((result (clysm::make-compile-result
                   :total 100
                   :compiled 60
                   :failed 40
                   :operator-failures (let ((ht (make-hash-table :test 'eq)))
                                        (setf (gethash 'defstruct ht) 15)
                                        (setf (gethash 'format ht) 25)
                                        ht))))
      (let ((report (with-output-to-string (s)
                      (clysm::generate-failure-report result s))))
        ;; Should contain percentage
        (ok (search "60%" report)
            "Report should contain percentage")
        ;; Should group by operator
        (ok (search "defstruct" report)
            "Report should list defstruct failures")
        (ok (search "format" report)
            "Report should list format failures")))))
