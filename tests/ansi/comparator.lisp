;;;; comparator.lisp - Compare WASM execution results with expected values

(in-package #:clysm/ansi-tests)

;;; WASM representation constants
;;; In clysm:
;;; - NIL is represented as 0
;;; - T is represented as 1 (non-zero)
;;; - Integers are i32 values
;;; - Cons cells are heap pointers

(defparameter +wasm-nil+ 0
  "WASM representation of NIL.")

(defparameter +wasm-t+ 1
  "WASM representation of T (or any true value).")

;;; Result parsing

(defstruct wasm-result
  "Result from WASM execution."
  (status nil :type (member :success :error :timeout))
  (value nil)
  (type nil :type (or string null))
  (error nil :type (or string null))
  (memory-size 0 :type integer))

(defun parse-wasm-output (json-string)
  "Parse JSON output from wasm-runner.js into a wasm-result."
  (handler-case
      (let ((data (uiop:split-string json-string :separator '(#\Newline))))
        ;; Take first non-empty line (JSON output)
        (let ((json-line (find-if (lambda (s) (and (> (length s) 0)
                                                    (char= (char s 0) #\{)))
                                  data)))
          (if json-line
              (parse-json-result json-line)
              (make-wasm-result :status :error
                                :error "No JSON output from WASM runner"))))
    (error (e)
      (make-wasm-result :status :error
                        :error (format nil "Parse error: ~A" e)))))

(defun parse-json-result (json-string)
  "Parse a JSON string into wasm-result.
   Simple JSON parser for our specific format."
  (let ((status nil)
        (value nil)
        (type nil)
        (error-msg nil)
        (memory-size 0))
    ;; Very simple JSON parsing for our known format
    ;; Format: {\"status\":\"...\",\"value\":...,\"type\":\"...\"}
    (when (search "\"status\":\"success\"" json-string)
      (setf status :success))
    (when (search "\"status\":\"error\"" json-string)
      (setf status :error))
    ;; Extract value
    (let ((value-pos (search "\"value\":" json-string)))
      (when value-pos
        (let* ((start (+ value-pos 8))
               (end (or (position #\, json-string :start start)
                        (position #\} json-string :start start))))
          (when end
            (let ((value-str (string-trim " " (subseq json-string start end))))
              (setf value (parse-json-value value-str)))))))
    ;; Extract type
    (let ((type-pos (search "\"type\":\"" json-string)))
      (when type-pos
        (let* ((start (+ type-pos 8))
               (end (position #\" json-string :start start)))
          (when end
            (setf type (subseq json-string start end))))))
    ;; Extract error
    (let ((error-pos (search "\"error\":\"" json-string)))
      (when error-pos
        (let* ((start (+ error-pos 9))
               (end (position #\" json-string :start start)))
          (when end
            (setf error-msg (subseq json-string start end))))))
    (make-wasm-result :status (or status :error)
                      :value value
                      :type type
                      :error error-msg
                      :memory-size memory-size)))

(defun parse-json-value (str)
  "Parse a JSON value string."
  (cond
    ((string= str "null") nil)
    ((string= str "true") t)
    ((string= str "false") nil)
    ((char= (char str 0) #\")
     ;; String value
     (subseq str 1 (1- (length str))))
    (t
     ;; Try to parse as number
     (handler-case
         (parse-integer str)
       (error ()
         (handler-case
             (read-from-string str)
           (error ()
             str)))))))

;;; Value comparison

(defun wasm-value-equal-p (wasm-value expected)
  "Compare a WASM value with an expected Lisp value."
  (cond
    ;; NIL comparison
    ((null expected)
     (eql wasm-value +wasm-nil+))
    ;; T comparison (any non-zero is true)
    ((eq expected t)
     (and (integerp wasm-value)
          (not (zerop wasm-value))))
    ;; Integer comparison
    ((integerp expected)
     (eql wasm-value expected))
    ;; Single-value list
    ((and (listp expected) (= (length expected) 1))
     (wasm-value-equal-p wasm-value (first expected)))
    ;; Multiple values (not fully supported yet)
    ((listp expected)
     ;; For now, just compare with first value
     (wasm-value-equal-p wasm-value (first expected)))
    ;; Other types
    (t
     (equalp wasm-value expected))))

(defun compare-result (wasm-result expected)
  "Compare WASM execution result with expected value.
   Returns (values match-p actual expected)."
  (if (eq (wasm-result-status wasm-result) :success)
      (let ((actual (wasm-result-value wasm-result)))
        (values (wasm-value-equal-p actual expected)
                actual
                expected))
      (values nil
              (wasm-result-error wasm-result)
              expected)))

;;; Result classification

(defun classify-result (wasm-result expected)
  "Classify the test result.
   Returns :pass, :fail, :error, or :skip."
  (case (wasm-result-status wasm-result)
    (:success
     (if (wasm-value-equal-p (wasm-result-value wasm-result) expected)
         :pass
         :fail))
    (:error :error)
    (:timeout :timeout)
    (t :error)))

;;; Expected failure handling

(defun expected-failure-p (test-name)
  "Check if TEST-NAME is in the expected failures list."
  (member test-name *expected-failures* :test #'equal))

(defun classify-with-expectations (test-name wasm-result expected)
  "Classify result considering expected failures.
   Returns :pass, :fail, :expected-failure, :unexpected-success, :error."
  (let ((basic-result (classify-result wasm-result expected)))
    (cond
      ((and (eq basic-result :pass) (expected-failure-p test-name))
       :unexpected-success)
      ((and (eq basic-result :fail) (expected-failure-p test-name))
       :expected-failure)
      (t basic-result))))
