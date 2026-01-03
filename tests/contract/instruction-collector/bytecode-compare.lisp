;;;; Bytecode Comparison Utility for Instruction Collector Contract Tests
;;;; Compares Wasm bytecode to verify byte-identical output after migration

(defpackage #:clysm/tests/contract/instruction-collector/bytecode-compare
  (:use #:cl)
  (:import-from #:clysm/tests/contract/instruction-collector/baseline-capture
                #:load-baseline
                #:capture-baseline)
  (:export #:compare-bytes
           #:bytecode-identical-p
           #:verify-against-baseline
           #:format-diff-report))

(in-package #:clysm/tests/contract/instruction-collector/bytecode-compare)

(defun compare-bytes (baseline-bytes current-bytes)
  "Compare two byte vectors and return a diff report.
   Returns (VALUES identical-p diff-list) where diff-list contains
   (offset baseline-byte current-byte) for each difference."
  (let ((len1 (length baseline-bytes))
        (len2 (length current-bytes))
        (diffs '()))
    ;; Compare byte by byte up to minimum length
    (dotimes (i (min len1 len2))
      (unless (= (aref baseline-bytes i) (aref current-bytes i))
        (push (list i (aref baseline-bytes i) (aref current-bytes i)) diffs)))
    ;; Handle length difference
    (cond
      ((> len1 len2)
       (loop for i from len2 below len1
             do (push (list i (aref baseline-bytes i) :missing) diffs)))
      ((< len1 len2)
       (loop for i from len1 below len2
             do (push (list i :missing (aref current-bytes i)) diffs))))
    (values (null diffs) (nreverse diffs))))

(defun bytecode-identical-p (baseline-bytes current-bytes)
  "Return T if two byte vectors are identical."
  (and (= (length baseline-bytes) (length current-bytes))
       (every #'= baseline-bytes current-bytes)))

(defun format-diff-report (diffs &key (max-diffs 10))
  "Format a list of diffs into a human-readable report.
   Shows at most MAX-DIFFS differences."
  (if (null diffs)
      "No differences found - bytecode is identical."
      (with-output-to-string (s)
        (format s "Found ~D difference~:P:~%" (length diffs))
        (loop for (offset base curr) in diffs
              for i from 1 to max-diffs
              do (format s "  Offset 0x~4,'0X: baseline=~A current=~A~%"
                         offset
                         (if (eq base :missing) "N/A" (format nil "0x~2,'0X" base))
                         (if (eq curr :missing) "N/A" (format nil "0x~2,'0X" curr))))
        (when (> (length diffs) max-diffs)
          (format s "  ... and ~D more differences~%" (- (length diffs) max-diffs))))))

(defun verify-against-baseline (baseline-name form &key (env nil) (report-on-failure t))
  "Verify that compiling FORM produces byte-identical output to saved baseline.
   Returns (VALUES success-p diff-report).
   If REPORT-ON-FAILURE is T, prints the diff report on mismatch."
  (let ((baseline-bytes (load-baseline baseline-name))
        (current-bytes (capture-baseline form :env env)))
    (multiple-value-bind (identical-p diffs)
        (compare-bytes baseline-bytes current-bytes)
      (if identical-p
          (values t "Bytecode identical - contract verified.")
          (let ((report (format-diff-report diffs)))
            (when report-on-failure
              (format *error-output* "~&Contract verification FAILED for ~A:~%~A~%"
                      baseline-name report))
            (values nil report))))))

;;; Contract verification functions for specific migration targets

(defun verify-equalp-contract ()
  "Verify that equalp compilation produces byte-identical output."
  (verify-against-baseline "equalp-baseline"
                           '(defun test-equalp (x y) (equalp x y))))

(defun verify-equal-contract ()
  "Verify that equal compilation produces byte-identical output."
  (verify-against-baseline "equal-baseline"
                           '(defun test-equal (x y) (equal x y))))

(defun verify-primitive-arithmetic-contract ()
  "Verify that arithmetic primitive compilation produces byte-identical output."
  (verify-against-baseline "primitive-arithmetic-baseline"
                           '(defun test-arithmetic (a b)
                              (+ a b (* a b) (- a) (/ a b)))))

(defun verify-primitive-comparison-contract ()
  "Verify that comparison primitive compilation produces byte-identical output."
  (verify-against-baseline "primitive-comparison-baseline"
                           '(defun test-comparison (a b)
                              (and (< a b) (> a b) (<= a b) (>= a b) (= a b) (/= a b)))))

(defun verify-primitive-typep-contract ()
  "Verify that type predicate compilation produces byte-identical output."
  (verify-against-baseline "primitive-typep-baseline"
                           '(defun test-typep (x)
                              (list (consp x) (numberp x) (stringp x) (symbolp x)))))
