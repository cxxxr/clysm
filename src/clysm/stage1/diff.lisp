;;;; diff.lisp - Binary diff analysis for Stage comparison
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Compares Stage 0 and Stage 1 Wasm binaries

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Binary Information Extraction
;;; ==========================================================================

(defun extract-binary-info (path)
  "Extract metadata from a Wasm binary.
Returns a binary-info struct."
  (unless (probe-file path)
    (return-from extract-binary-info
      (make-binary-info
       :path (namestring path)
       :size-bytes 0
       :valid-p nil)))
  (let ((size (with-open-file (s path :element-type '(unsigned-byte 8))
                (file-length s)))
        (exports nil)
        (types 0)
        (functions 0)
        (valid nil))
    ;; Use wasm-tools to extract information
    (handler-case
        (let ((output (uiop:run-program
                       (list "wasm-tools" "print" (namestring path))
                       :output :string
                       :error-output nil
                       :ignore-error-status t)))
          (when output
            ;; Count exports
            (setf exports (extract-exports output))
            ;; Count types
            (setf types (count-pattern "(type" output))
            ;; Count functions
            (setf functions (count-pattern "(func" output)))
          ;; Validate
          (setf valid (validate-binary path)))
      (error () nil))
    (make-binary-info
     :path (namestring path)
     :size-bytes size
     :exports exports
     :types types
     :functions functions
     :valid-p valid)))

(defun extract-exports (wat-output)
  "Extract export names from WAT output."
  (let ((exports nil)
        (pos 0))
    (loop
      (let ((start (search "(export" wat-output :start2 pos)))
        (unless start (return))
        (let* ((quote-start (position #\" wat-output :start (+ start 7)))
               (quote-end (when quote-start
                            (position #\" wat-output :start (1+ quote-start)))))
          (when (and quote-start quote-end)
            (push (subseq wat-output (1+ quote-start) quote-end) exports))
          (setf pos (1+ (or quote-end start))))))
    (nreverse exports)))

(defun count-pattern (pattern text)
  "Count occurrences of pattern in text."
  (let ((count 0)
        (pos 0))
    (loop
      (let ((found (search pattern text :start2 pos)))
        (unless found (return count))
        (incf count)
        (setf pos (1+ found))))))

(defun validate-binary (path)
  "Check if binary passes wasm-tools validation."
  (zerop (nth-value 2
           (uiop:run-program
            (list "wasm-tools" "validate" (namestring path))
            :output nil
            :error-output nil
            :ignore-error-status t))))

;;; ==========================================================================
;;; Byte-Level Binary Comparison (Feature 040)
;;; ==========================================================================

(defun binaries-identical-p (path1 path2)
  "Compare two binary files byte-by-byte.
Returns (values identical-p first-diff-offset).
If identical, returns (values T nil).
If different, returns (values NIL offset-of-first-difference)."
  (unless (probe-file path1)
    (error 'fixpoint-comparison-error
           :stage1-path path1
           :stage2-path path2
           :context "First file not found"))
  (unless (probe-file path2)
    (error 'fixpoint-comparison-error
           :stage1-path path1
           :stage2-path path2
           :context "Second file not found"))
  (with-open-file (s1 path1 :element-type '(unsigned-byte 8))
    (with-open-file (s2 path2 :element-type '(unsigned-byte 8))
      (let ((len1 (file-length s1))
            (len2 (file-length s2)))
        ;; Different sizes means not identical
        (when (/= len1 len2)
          (return-from binaries-identical-p
            (values nil (min len1 len2))))
        ;; Compare byte-by-byte
        (let ((buffer1 (make-array 4096 :element-type '(unsigned-byte 8)))
              (buffer2 (make-array 4096 :element-type '(unsigned-byte 8)))
              (offset 0))
          (loop
            (let ((n1 (read-sequence buffer1 s1))
                  (n2 (read-sequence buffer2 s2)))
              (when (zerop n1)
                (return-from binaries-identical-p (values t nil)))
              (dotimes (i (min n1 n2))
                (when (/= (aref buffer1 i) (aref buffer2 i))
                  (return-from binaries-identical-p
                    (values nil (+ offset i)))))
              (incf offset n1))))))))

(defun compute-byte-diff (path1 path2)
  "Compute detailed byte-level diff between two binaries.
Returns a byte-diff-info struct."
  (unless (and (probe-file path1) (probe-file path2))
    (return-from compute-byte-diff
      (make-byte-diff-info
       :first-offset nil
       :total-diff-bytes 0
       :size-mismatch-p t
       :size1 (if (probe-file path1)
                  (with-open-file (s path1) (file-length s))
                  0)
       :size2 (if (probe-file path2)
                  (with-open-file (s path2) (file-length s))
                  0))))
  (with-open-file (s1 path1 :element-type '(unsigned-byte 8))
    (with-open-file (s2 path2 :element-type '(unsigned-byte 8))
      (let* ((len1 (file-length s1))
             (len2 (file-length s2))
             (size-mismatch (/= len1 len2))
             (first-offset nil)
             (diff-count 0)
             (diff-regions nil)
             (in-diff-region nil)
             (region-start 0))
        ;; Compare up to shorter length
        (let ((buffer1 (make-array 4096 :element-type '(unsigned-byte 8)))
              (buffer2 (make-array 4096 :element-type '(unsigned-byte 8)))
              (offset 0))
          (loop
            (let ((n1 (read-sequence buffer1 s1))
                  (n2 (read-sequence buffer2 s2)))
              (when (and (zerop n1) (zerop n2))
                (return))
              (dotimes (i (min n1 n2))
                (let ((pos (+ offset i)))
                  (if (/= (aref buffer1 i) (aref buffer2 i))
                      (progn
                        (incf diff-count)
                        (unless first-offset
                          (setf first-offset pos))
                        (unless in-diff-region
                          (setf in-diff-region t
                                region-start pos)))
                      (when in-diff-region
                        (push (cons region-start (- pos region-start))
                              diff-regions)
                        (setf in-diff-region nil)))))
              (incf offset (max n1 n2)))))
        ;; Close any open diff region
        (when in-diff-region
          (push (cons region-start 1) diff-regions))
        (make-byte-diff-info
         :first-offset first-offset
         :total-diff-bytes diff-count
         :size-mismatch-p size-mismatch
         :size1 len1
         :size2 len2
         :diff-regions (nreverse diff-regions))))))

;;; ==========================================================================
;;; Binary Comparison
;;; ==========================================================================

(defun compare-binaries (stage0-path stage1-path)
  "Compare two Wasm binaries and return a diff-report."
  (let ((stage0-info (extract-binary-info stage0-path))
        (stage1-info (extract-binary-info stage1-path)))
    (make-diff-report
     :stage0 stage0-info
     :stage1 stage1-info
     :differences (compute-differences stage0-info stage1-info))))

(defun compute-differences (stage0-info stage1-info)
  "Compute differences between two binary-info structs."
  (let ((size0 (binary-info-size-bytes stage0-info))
        (size1 (binary-info-size-bytes stage1-info))
        (exports0 (binary-info-exports stage0-info))
        (exports1 (binary-info-exports stage1-info)))
    (make-diff-details
     :size-delta (format-size-delta size0 size1)
     :missing-exports (set-difference exports0 exports1 :test #'equal)
     :new-exports (set-difference exports1 exports0 :test #'equal)
     :type-changes nil))) ; TODO: Implement type comparison

(defun format-size-delta (size0 size1)
  "Format size difference as a string."
  (let ((delta (- size1 size0)))
    (cond
      ((zerop delta) "0 bytes (identical)")
      ((plusp delta) (format nil "+~D bytes (+~,1F%)"
                             delta (* 100.0 (/ delta size0))))
      (t (format nil "~D bytes (~,1F%)"
                 delta (* 100.0 (/ delta size0)))))))

;;; ==========================================================================
;;; Export Comparison
;;; ==========================================================================

(defun compare-exports (stage0-path stage1-path)
  "Compare exports between two binaries.
Returns three values: common, missing-from-stage1, new-in-stage1."
  (let* ((stage0-info (extract-binary-info stage0-path))
         (stage1-info (extract-binary-info stage1-path))
         (exports0 (binary-info-exports stage0-info))
         (exports1 (binary-info-exports stage1-info)))
    (values
     (intersection exports0 exports1 :test #'equal)
     (set-difference exports0 exports1 :test #'equal)
     (set-difference exports1 exports0 :test #'equal))))

(defun compare-types (stage0-path stage1-path)
  "Compare type counts between binaries.
Returns (stage0-types stage1-types difference)."
  (let* ((stage0-info (extract-binary-info stage0-path))
         (stage1-info (extract-binary-info stage1-path)))
    (values
     (binary-info-types stage0-info)
     (binary-info-types stage1-info)
     (- (binary-info-types stage1-info)
        (binary-info-types stage0-info)))))

;;; ==========================================================================
;;; Diff Report Generation
;;; ==========================================================================

(defun generate-diff-report (stage0-path stage1-path
                             &key (stream *standard-output*)
                                  (format :json))
  "Generate a diff report comparing two binaries."
  (let ((report (compare-binaries stage0-path stage1-path)))
    (ecase format
      (:json (write-diff-json report stream))
      (:text (write-diff-text report stream)))))

(defun write-diff-json (report stream)
  "Write diff report as JSON."
  (let ((s0 (diff-report-stage0 report))
        (s1 (diff-report-stage1 report))
        (diff (diff-report-differences report)))
    (format stream "{~%")
    (format stream "  \"stage0\": {~%")
    (format stream "    \"path\": ~S,~%" (binary-info-path s0))
    (format stream "    \"size_bytes\": ~D,~%" (binary-info-size-bytes s0))
    (format stream "    \"exports\": ~D,~%" (length (binary-info-exports s0)))
    (format stream "    \"types\": ~D,~%" (binary-info-types s0))
    (format stream "    \"functions\": ~D,~%" (binary-info-functions s0))
    (format stream "    \"valid\": ~A~%" (if (binary-info-valid-p s0) "true" "false"))
    (format stream "  },~%")
    (format stream "  \"stage1\": {~%")
    (format stream "    \"path\": ~S,~%" (binary-info-path s1))
    (format stream "    \"size_bytes\": ~D,~%" (binary-info-size-bytes s1))
    (format stream "    \"exports\": ~D,~%" (length (binary-info-exports s1)))
    (format stream "    \"types\": ~D,~%" (binary-info-types s1))
    (format stream "    \"functions\": ~D,~%" (binary-info-functions s1))
    (format stream "    \"valid\": ~A~%" (if (binary-info-valid-p s1) "true" "false"))
    (format stream "  },~%")
    (format stream "  \"differences\": {~%")
    (format stream "    \"size_delta\": ~S,~%" (diff-details-size-delta diff))
    (format stream "    \"missing_exports\": ~D,~%"
            (length (diff-details-missing-exports diff)))
    (format stream "    \"new_exports\": ~D~%"
            (length (diff-details-new-exports diff)))
    (format stream "  }~%")
    (format stream "}~%")))

(defun write-diff-text (report stream)
  "Write diff report as human-readable text."
  (let ((s0 (diff-report-stage0 report))
        (s1 (diff-report-stage1 report))
        (diff (diff-report-differences report)))
    (format stream "=== Stage Comparison Report ===~%~%")
    (format stream "Stage 0: ~A~%" (binary-info-path s0))
    (format stream "  Size:      ~D bytes~%" (binary-info-size-bytes s0))
    (format stream "  Exports:   ~D~%" (length (binary-info-exports s0)))
    (format stream "  Types:     ~D~%" (binary-info-types s0))
    (format stream "  Functions: ~D~%" (binary-info-functions s0))
    (format stream "  Valid:     ~A~%~%" (if (binary-info-valid-p s0) "Yes" "No"))
    (format stream "Stage 1: ~A~%" (binary-info-path s1))
    (format stream "  Size:      ~D bytes~%" (binary-info-size-bytes s1))
    (format stream "  Exports:   ~D~%" (length (binary-info-exports s1)))
    (format stream "  Types:     ~D~%" (binary-info-types s1))
    (format stream "  Functions: ~D~%" (binary-info-functions s1))
    (format stream "  Valid:     ~A~%~%" (if (binary-info-valid-p s1) "Yes" "No"))
    (format stream "Differences:~%")
    (format stream "  Size: ~A~%" (diff-details-size-delta diff))
    (when (diff-details-missing-exports diff)
      (format stream "  Missing from Stage 1:~%")
      (dolist (e (diff-details-missing-exports diff))
        (format stream "    - ~A~%" e)))
    (when (diff-details-new-exports diff)
      (format stream "  New in Stage 1:~%")
      (dolist (e (diff-details-new-exports diff))
        (format stream "    - ~A~%" e)))))
