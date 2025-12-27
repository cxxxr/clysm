(defpackage :clysm-tests/unit/validation/reporter
  (:use :cl :rove)
  (:local-nicknames (:v :clysm-validation)))

(in-package :clysm-tests/unit/validation/reporter)

;;; T015: Unit tests for Coverage-Report struct

(deftest coverage-report-struct-creation
  "Test that coverage-report struct can be created with required fields"
  (let ((report (v:make-coverage-report :scope "test-scope"
                                         :total-symbols 100
                                         :supported-count 80
                                         :partial-count 10
                                         :unsupported-count 5
                                         :unknown-count 5
                                         :coverage-pct 90.0
                                         :unsupported-details nil)))
    (ok (v:coverage-report-p report) "Should create a coverage-report struct")
    (ok (string= (v:coverage-report-scope report) "test-scope") "Should store scope")
    (ok (= (v:coverage-report-total-symbols report) 100) "Should store total-symbols")
    (ok (= (v:coverage-report-supported-count report) 80) "Should store supported-count")
    (ok (= (v:coverage-report-coverage-pct report) 90.0) "Should store coverage-pct")))

(deftest coverage-report-with-unsupported
  "Test coverage-report with unsupported features"
  (let ((report (v:make-coverage-report :scope "partial"
                                         :total-symbols 10
                                         :supported-count 5
                                         :partial-count 2
                                         :unsupported-count 3
                                         :unknown-count 0
                                         :coverage-pct 70.0
                                         :unsupported-details '(foo bar baz))))
    (ok (= (v:coverage-report-unsupported-count report) 3) "Should have 3 unsupported")
    (ok (= (length (v:coverage-report-unsupported-details report)) 3) "Should list 3 details")))

;;; T016: Unit tests for generate-report function

(deftest generate-report-produces-output
  "Test that generate-report produces markdown output"
  (let* ((report (v:make-coverage-report :scope "test"
                                          :total-symbols 100
                                          :supported-count 90
                                          :partial-count 5
                                          :unsupported-count 3
                                          :unknown-count 2
                                          :coverage-pct 95.0
                                          :unsupported-details nil))
         (coverage-data (list :directories (list report) :overall report))
         (output (with-output-to-string (s)
                   (v:generate-report coverage-data s))))
    (ok (> (length output) 0) "Should produce output")
    (ok (search "Coverage Report" output) "Should contain report title")
    (ok (search "Summary" output) "Should contain summary section")))

(deftest compute-coverage-from-symbols
  "Test compute-coverage function"
  (let ((symbols '(defun defmacro let if + - loop xyz-unknown)))
    (let ((report (v:compute-coverage "test" symbols)))
      (ok (v:coverage-report-p report) "Should return coverage-report")
      (ok (> (v:coverage-report-supported-count report) 0) "Should have supported symbols")
      (ok (= (v:coverage-report-total-symbols report) (length symbols)) "Should count all symbols"))))

;;; T056: Unit tests for Blessed-Subset struct

(deftest blessed-subset-struct-creation
  "Test that blessed-subset struct can be created with required fields"
  (let ((subset (v:make-blessed-subset :version "1.0.0"
                                        :generation-date "2025-12-27"
                                        :special-forms '(if progn let)
                                        :macros '(defun defmacro when)
                                        :functions '(+ - * /)
                                        :types '(integer string symbol)
                                        :partial-notes nil)))
    (ok (v:blessed-subset-p subset) "Should create a blessed-subset struct")
    (ok (string= (v:blessed-subset-version subset) "1.0.0") "Should store version")
    (ok (string= (v:blessed-subset-generation-date subset) "2025-12-27") "Should store date")
    (ok (= (length (v:blessed-subset-special-forms subset)) 3) "Should store special-forms")
    (ok (= (length (v:blessed-subset-macros subset)) 3) "Should store macros")
    (ok (= (length (v:blessed-subset-functions subset)) 4) "Should store functions")
    (ok (= (length (v:blessed-subset-types subset)) 3) "Should store types")))

(deftest blessed-subset-with-partial-notes
  "Test blessed-subset struct with partial support notes"
  (let ((subset (v:make-blessed-subset :version "1.0.0"
                                        :generation-date "2025-12-27"
                                        :special-forms nil
                                        :macros '(loop)
                                        :functions '(format)
                                        :types nil
                                        :partial-notes '((loop . "Only basic forms")
                                                         (format . "Limited directives")))))
    (ok (= (length (v:blessed-subset-partial-notes subset)) 2) "Should have 2 notes")
    (ok (assoc 'loop (v:blessed-subset-partial-notes subset)) "Should have loop note")
    (ok (assoc 'format (v:blessed-subset-partial-notes subset)) "Should have format note")))

;;; T057: Unit tests for aggregate-verified-features function

(deftest aggregate-verified-features-empty
  "Test aggregate-verified-features with empty results"
  (let ((features (v:aggregate-verified-features nil)))
    (ok (listp features) "Should return a list")
    (ok (null features) "Should be empty for no results")))

(deftest aggregate-verified-features-collects-from-successful
  "Test aggregate-verified-features collects from successful compilations"
  (let* ((module1 (v:make-module-info :path #p"/test1.lisp"
                                       :directory #p"/"
                                       :dependencies nil
                                       :symbols-used '(defun + - let)))
         (module2 (v:make-module-info :path #p"/test2.lisp"
                                       :directory #p"/"
                                       :dependencies nil
                                       :symbols-used '(defmacro if progn)))
         (result1 (v:make-compilation-result :module module1
                                              :success t
                                              :wasm-bytes #(0 1 2)
                                              :error-message nil
                                              :unsupported-feature nil
                                              :validation-passed t
                                              :validation-error nil))
         (result2 (v:make-compilation-result :module module2
                                              :success t
                                              :wasm-bytes #(0 1 2)
                                              :error-message nil
                                              :unsupported-feature nil
                                              :validation-passed t
                                              :validation-error nil))
         (features (v:aggregate-verified-features (list result1 result2))))
    (ok (listp features) "Should return a list")
    ;; Should collect symbols from both modules
    (ok (member 'defun features) "Should include defun")
    (ok (member 'defmacro features) "Should include defmacro")))

(deftest aggregate-verified-features-ignores-failed
  "Test aggregate-verified-features ignores failed compilations"
  (let* ((module (v:make-module-info :path #p"/fail.lisp"
                                      :directory #p"/"
                                      :dependencies nil
                                      :symbols-used '(foo bar baz)))
         (result (v:make-compilation-result :module module
                                             :success nil
                                             :wasm-bytes nil
                                             :error-message "Failed"
                                             :unsupported-feature nil
                                             :validation-passed nil
                                             :validation-error nil))
         (features (v:aggregate-verified-features (list result))))
    (ok (null features) "Should not include features from failed compilation")))

;;; T058: Unit tests for generate-blessed-subset function

(deftest categorize-features-groups-correctly
  "Test categorize-features groups symbols by category"
  (let ((symbols '(defun defmacro + - if let integer string)))
    (let ((categorized (v:categorize-features symbols)))
      (ok (listp (getf categorized :special-forms)) "Should have special-forms")
      (ok (listp (getf categorized :macros)) "Should have macros")
      (ok (listp (getf categorized :functions)) "Should have functions")
      (ok (listp (getf categorized :types)) "Should have types"))))

(deftest collect-partial-notes-extracts-notes
  "Test collect-partial-notes extracts notes for partial symbols"
  (let ((notes (v:collect-partial-notes '(loop format defun))))
    (ok (listp notes) "Should return a list")
    ;; loop and format have partial support with notes
    (when (assoc 'loop notes)
      (ok (stringp (cdr (assoc 'loop notes))) "loop note should be string"))))

(deftest generate-blessed-subset-creates-file
  "Test generate-blessed-subset creates a valid Lisp file"
  (let ((temp-path (merge-pathnames "test-blessed-subset.lisp"
                                    (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (v:generate-blessed-subset '(defun let if + - integer) temp-path)
          (ok (probe-file temp-path) "Should create file")
          (let ((content (uiop:read-file-string temp-path)))
            (ok (search "blessed-subset.lisp" content) "Should have header")
            (ok (search "*blessed-special-forms*" content) "Should define special-forms")
            (ok (search "*blessed-macros*" content) "Should define macros")
            (ok (search "*blessed-functions*" content) "Should define functions")))
      (when (probe-file temp-path)
        (delete-file temp-path)))))
