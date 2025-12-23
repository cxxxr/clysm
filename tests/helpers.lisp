;;;; helpers.lisp - Test helper functions (T065)

(in-package #:clysm/tests/helpers)

;;; ============================================================
;;; Temporary File Management (must be defined first - used by other functions)
;;; ============================================================

(defmacro with-temp-wasm-file ((var bytes) &body body)
  "Execute body with a temporary Wasm file."
  (let ((temp-file (gensym "TEMP-FILE-"))
        (bytes-var (gensym "BYTES-")))
    `(let* ((,bytes-var ,bytes)
            (,temp-file (format nil "/tmp/clysm-test-~A.wasm"
                                (random 1000000)))
            (,var ,temp-file))
       (unwind-protect
            (progn
              (with-open-file (stream ,temp-file
                                      :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
                (write-sequence ,bytes-var stream))
              ,@body)
         (when (probe-file ,temp-file)
           (delete-file ,temp-file))))))

;;; ============================================================
;;; Wasm Execution Helpers
;;; ============================================================

;; Sentinel value for NIL (MIN_INT32)
(defparameter +nil-sentinel+ -2147483648)

(defun parse-wasm-output (output)
  "Parse wasmtime output to Lisp value.
   MIN_INT32 (-2147483648) is used as a sentinel for NIL."
  (let ((trimmed (string-trim '(#\Space #\Newline #\Return #\Tab) output)))
    (cond
      ((string= trimmed "") nil)
      ((every #'digit-char-p trimmed)
       (let ((value (parse-integer trimmed)))
         (if (= value +nil-sentinel+)
             nil
             value)))
      ((and (> (length trimmed) 0)
            (char= (char trimmed 0) #\-)
            (every #'digit-char-p (subseq trimmed 1)))
       (let ((value (parse-integer trimmed)))
         (if (= value +nil-sentinel+)
             nil
             value)))
      ((string-equal trimmed "true") t)
      ((string-equal trimmed "false") nil)
      (t trimmed))))

(define-condition wasm-runtime-error (error)
  ((exit-code :initarg :exit-code :reader wasm-runtime-error-exit-code)
   (message :initarg :message :reader wasm-runtime-error-message))
  (:report (lambda (c stream)
             (format stream "Wasm runtime error (exit ~A): ~A"
                     (wasm-runtime-error-exit-code c)
                     (wasm-runtime-error-message c)))))

(defun run-wasm-bytes (bytes)
  "Run Wasm bytes with wasmtime and return the result.
   Uses --wasm gc and --wasm function-references flags for WasmGC support.
   Uses --invoke to capture the return value.
   Signals WASM-RUNTIME-ERROR on non-zero exit code."
  (with-temp-wasm-file (wasm-file bytes)
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list "wasmtime" "--wasm" "gc"
                                "--wasm" "function-references"
                                "--wasm" "exceptions"
                                "--invoke" "_start" wasm-file)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (if (zerop exit-code)
          (parse-wasm-output output)
          ;; Signal error for non-zero exit code (e.g., uncaught exceptions)
          (error 'wasm-runtime-error
                 :exit-code exit-code
                 :message (or (and (> (length error-output) 0) error-output)
                              (and (> (length output) 0) output)
                              "Unknown error"))))))

;;; ============================================================
;;; Compile and Run (Main Test Helper)
;;; ============================================================

(defun compile-and-run (expr)
  "Compile an expression to Wasm and run it with wasmtime.
   Returns the result of execution.

   For fixnum results, returns an integer.
   For boolean results, returns T or NIL.

   Example:
     (compile-and-run '(+ 1 2))  ; => 3"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm expr)))
    (run-wasm-bytes wasm-bytes)))

;;; ============================================================
;;; Wasm Validation
;;; ============================================================

(defun validate-wasm (bytes)
  "Validate Wasm bytes using wasm-tools.
   Returns T if valid, signals error otherwise."
  (with-temp-wasm-file (temp-file bytes)
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list "wasm-tools" "validate" "--features" "gc" temp-file)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore output))
      (if (zerop exit-code)
          t
          (error "Wasm validation failed: ~A" error-output)))))

(defun validate-wasm-silent (bytes)
  "Validate Wasm bytes silently. Returns T if valid, NIL otherwise."
  (handler-case
      (validate-wasm bytes)
    (error () nil)))

;;; ============================================================
;;; Debugging Helpers
;;; ============================================================

(defun compile-to-wat-debug (expr)
  "Compile expression to WAT text for debugging."
  (clysm/compiler:compile-to-wat expr))

(defun compile-to-instructions-debug (expr)
  "Compile expression to Wasm instructions for debugging."
  (clysm/compiler:compile-expression expr))

(defun disassemble-wasm (bytes)
  "Disassemble Wasm bytes to WAT using wasm-tools."
  (with-temp-wasm-file (wasm-file bytes)
    (uiop:run-program (list "wasm-tools" "print" wasm-file)
                      :output :string)))

;;; ============================================================
;;; Assertion Helpers
;;; ============================================================

(defun assert-compiles (expr)
  "Assert that an expression compiles without error."
  (handler-case
      (progn
        (clysm/compiler:compile-to-wasm expr)
        t)
    (error (e)
      (error "Compilation failed: ~A" e))))

(defun assert-validates (expr)
  "Assert that an expression compiles to valid Wasm."
  (let ((bytes (clysm/compiler:compile-to-wasm expr)))
    (validate-wasm bytes)))

(defun assert-equals (expected expr)
  "Assert that expression evaluates to expected value."
  (let ((result (compile-and-run expr)))
    (unless (equal expected result)
      (error "Expected ~A but got ~A for ~S" expected result expr))
    t))
