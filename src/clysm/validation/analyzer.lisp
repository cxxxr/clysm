;;; analyzer.lisp - Static Analyzer for Clysm Validation
;;;
;;; Provides S-expression walking and symbol extraction for
;;; analyzing which CL features are used in compiler modules.

(in-package :clysm-validation)

;;; T008: Module struct definition

(defstruct module-info
  "Represents a Clysm compiler module with its dependencies and symbol usage.

   Slots:
   - path: Pathname to the source file
   - directory: Pathname to the containing directory
   - dependencies: List of file names this module depends on
   - symbols-used: List of CL symbols used in this module"
  (path nil :type (or null pathname))
  (directory nil :type (or null pathname))
  (dependencies nil :type list)
  (symbols-used nil :type list))

;;; Feature-Usage struct (for tracking symbol usage in files)

(defstruct feature-usage
  "Tracks usage of a CL symbol within a file.

   Slots:
   - symbol: The CL symbol
   - file: Pathname of the file where symbol is used
   - line-numbers: List of line numbers (if available)
   - occurrence-count: Number of times symbol appears"
  (symbol nil :type symbol)
  (file nil :type (or null pathname))
  (line-numbers nil :type list)
  (occurrence-count 0 :type fixnum))

;;; T017: extract-symbols function (S-expression walker)

(defun extract-symbols (form &key (cl-only nil))
  "Recursively extract all unique symbols from a Lisp form.

   If CL-ONLY is T, only returns symbols from the CL package.
   Returns a list of unique symbols."
  (let ((symbols (make-hash-table :test 'eq)))
    (labels ((walk (form)
               (cond
                 ((symbolp form)
                  (when (and form  ; exclude NIL
                             (or (not cl-only)
                                 (eq (symbol-package form)
                                     (find-package :cl))))
                    (setf (gethash form symbols) t)))
                 ((consp form)
                  (walk (car form))
                  (walk (cdr form)))
                 ;; Ignore numbers, strings, etc.
                 (t nil))))
      (walk form))
    (alexandria:hash-table-keys symbols)))

;;; T018: read-source-file function

(defun read-source-file (path)
  "Read all forms from a Lisp source file.
   Returns a list of forms read from the file."
  (with-open-file (stream path :direction :input
                               :external-format :utf-8
                               :if-does-not-exist nil)
    (when stream
      (let ((forms nil)
            (eof (gensym "EOF")))
        (loop
          (let ((form (read stream nil eof)))
            (when (eq form eof)
              (return (nreverse forms)))
            (push form forms)))))))

;;; T019: analyze-file function

(defun analyze-file (path)
  "Analyze a single Lisp source file and return a list of feature-usage structs.

   Each struct represents a CL symbol used in the file with its occurrence count."
  (let ((forms (read-source-file path))
        (symbol-counts (make-hash-table :test 'eq)))
    (when forms
      ;; Count all CL symbol occurrences
      (dolist (form forms)
        (let ((symbols (extract-symbols form :cl-only t)))
          (dolist (sym symbols)
            (incf (gethash sym symbol-counts 0)))))
      ;; Convert to feature-usage structs
      (let ((result nil))
        (maphash (lambda (sym count)
                   (push (make-feature-usage :symbol sym
                                             :file path
                                             :line-numbers nil  ; Line tracking not implemented
                                             :occurrence-count count)
                         result))
                 symbol-counts)
        result))))

;;; T020: analyze-directory function

(defun analyze-directory (directory)
  "Scan all .lisp files in DIRECTORY and return analysis results.

   Returns a hash-table mapping pathnames to lists of feature-usage structs."
  (let ((result (make-hash-table :test 'equal))
        (pattern (merge-pathnames "*.lisp" directory)))
    (dolist (path (directory pattern))
      (let ((usages (analyze-file path)))
        (when usages
          (setf (gethash path result) usages))))
    result))

;;; T025: analyze-all function

(defparameter *target-directories*
  '("src/clysm/backend/"
    "src/clysm/reader/"
    "src/clysm/compiler/"
    "src/clysm/runtime/"
    "src/clysm/clos/"
    "src/clysm/conditions/")
  "The 6 target directories for self-compilation validation.")

(defun analyze-all (&optional (base-dir (asdf:system-source-directory :clysm)))
  "Analyze all 6 target directories and return combined results.

   Returns a hash-table mapping directory names to their analysis results."
  (let ((result (make-hash-table :test 'equal)))
    (dolist (dir-name *target-directories*)
      (let* ((dir-path (merge-pathnames dir-name base-dir))
             (dir-result (analyze-directory dir-path)))
        (setf (gethash dir-name result) dir-result)))
    result))

;;; Aggregate statistics

(defun count-unique-symbols (directory-result)
  "Count unique symbols across all files in a directory analysis result."
  (let ((all-symbols (make-hash-table :test 'eq)))
    (maphash (lambda (path usages)
               (declare (ignore path))
               (dolist (usage usages)
                 (setf (gethash (feature-usage-symbol usage) all-symbols) t)))
             directory-result)
    (hash-table-count all-symbols)))

(defun get-all-unique-symbols (analysis-result)
  "Get list of all unique CL symbols found across all directories."
  (let ((all-symbols (make-hash-table :test 'eq)))
    (maphash (lambda (dir-name dir-result)
               (declare (ignore dir-name))
               (maphash (lambda (path usages)
                          (declare (ignore path))
                          (dolist (usage usages)
                            (setf (gethash (feature-usage-symbol usage) all-symbols) t)))
                        dir-result))
             analysis-result)
    (alexandria:hash-table-keys all-symbols)))

(defun classify-symbols (symbols)
  "Classify a list of symbols by their support status.
   Returns a plist (:supported list :partial list :unsupported list :unknown list)"
  (let ((supported nil)
        (partial nil)
        (unsupported nil)
        (unknown nil))
    (dolist (sym symbols)
      (case (feature-status sym)
        (:supported (push sym supported))
        (:partial (push sym partial))
        (:unsupported (push sym unsupported))
        (:internal (push sym supported))  ; internal counts as supported
        (:unknown (push sym unknown))))
    (list :supported (nreverse supported)
          :partial (nreverse partial)
          :unsupported (nreverse unsupported)
          :unknown (nreverse unknown))))
