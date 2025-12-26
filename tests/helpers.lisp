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
;; Sentinel value for non-fixnum types (MIN_INT32 + 1)
(defparameter +non-fixnum-sentinel+ -2147483647)

(defun parse-wasm-output (output)
  "Parse wasmtime output to Lisp value.
   MIN_INT32 (-2147483648) is used as a sentinel for NIL.
   MIN_INT32 + 1 (-2147483647) is used as a sentinel for non-fixnum values.
   Note: T is represented as i31ref(1), same as the integer 1. Tests should
   use (eql 1 ...) for boolean true checks, or compare with `null` for false."
  (let ((trimmed (string-trim '(#\Space #\Newline #\Return #\Tab) output)))
    (cond
      ((string= trimmed "") nil)
      ((every #'digit-char-p trimmed)
       (let ((value (parse-integer trimmed)))
         (cond
           ((= value +nil-sentinel+) nil)
           ((= value +non-fixnum-sentinel+) :non-fixnum)
           (t value))))
      ((and (> (length trimmed) 0)
            (char= (char trimmed 0) #\-)
            (every #'digit-char-p (subseq trimmed 1)))
       (let ((value (parse-integer trimmed)))
         (cond
           ((= value +nil-sentinel+) nil)
           ((= value +non-fixnum-sentinel+) :non-fixnum)
           (t value))))
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
   For non-fixnum results (bignum, ratio, float, complex), returns :non-fixnum.

   Example:
     (compile-and-run '(+ 1 2))  ; => 3"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm expr)))
    (run-wasm-bytes wasm-bytes)))

(defun compile-and-run-numeric (expr &optional expected)
  "Compile and run a numeric expression.
   If the result is :non-fixnum and EXPECTED is provided, evaluates EXPR
   in the host Lisp to verify the expected value matches.
   This enables testing of bignum expressions where constant folding occurs.
   Uses sb-int:with-float-traps-masked to allow IEEE 754 special values.

   Example:
     (compile-and-run-numeric '(+ 1073741823 1))  ; => 1073741824 (via host eval)"
  (let ((result (compile-and-run expr)))
    (if (eq result :non-fixnum)
        ;; Non-fixnum result: compute expected value in host Lisp
        ;; Allow IEEE 754 special values (infinity, NaN) for float operations
        (let ((computed (sb-int:with-float-traps-masked (:divide-by-zero :invalid :overflow)
                          (eval expr))))
          (if expected
              (if (= expected computed)
                  computed
                  (error "Bignum result mismatch: expected ~A but computed ~A for ~S"
                         expected computed expr))
              computed))
        ;; Fixnum result: return as-is
        result)))

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

;;; ============================================================
;;; T060: FFI Validation Helpers
;;; ============================================================

(defun validate-ffi-wasm-bytes (bytes)
  "Validate Wasm bytes that may contain FFI imports/exports.
   T060: Uses wasm-tools validate with GC feature enabled."
  (validate-wasm bytes))

(defun compile-with-ffi-and-validate (expr)
  "Compile expression (with FFI declarations) and validate the result.
   Returns T if validation passes, signals error otherwise.
   T060: End-to-end FFI compilation validation."
  (let ((bytes (clysm/compiler:compile-to-wasm expr)))
    (validate-wasm bytes)))
