;;;; generator.lisp - Stage 1 binary generation
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Generates Stage 1 Wasm binary from Clysm compiler source

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Phase 13D-7: Validated Bundled Compilation Strategy
;;; ==========================================================================
;;;
;;; Strategy:
;;; 1. Test each form individually to identify which compile successfully
;;; 2. Bundle successful defuns and validate the output
;;; 3. Use binary search to find maximum valid bundle size if full bundle fails
;;;
;;; This produces a valid Wasm module containing the maximum number of
;;; successfully compiled and validated functions.

(defun test-form-compilation (sexp &key (validate t))
  "Test if a form compiles successfully AND produces valid Wasm.
Returns (values result bytes error-condition) where:
  - result is T if compilation succeeds and validation passes
  - result is :SKIPPED if compile-to-wasm returns nil (directive form)
  - result is NIL if compilation fails or validation fails
  - error-condition is the captured error (or NIL)
Phase 13D-3: Added :skipped handling for compile-time directives.
Phase 13D M4: Returns error condition for detailed error logging."
  (handler-case
      (let ((bytes (clysm:compile-to-wasm sexp)))
        (cond
          ;; T009: Directive forms return nil bytes - mark as skipped
          ((null bytes) (values :skipped nil nil))
          ;; Validation failed
          ((and validate (not (validate-wasm-bytes bytes)))
           (values nil nil nil))
          ;; Success
          (t (values t bytes nil))))
    (error (e) (values nil nil e))))

(defun classify-forms (forms &key (progress-callback nil) (validate t))
  "Classify forms into successful, failed, and skipped compilations.
FORMS is a list of source-form structs.
When VALIDATE is true (default), only forms that produce valid Wasm are counted as successful.
Phase 13D-3: Added :skipped tracking for compile-time directives.
Phase 13D M4: Captures detailed error info for DEFUN failures.
Returns (values successful-sexps results stats)."
  ;; Phase 13D M4: Clear error analysis state at start
  ;; Use runtime symbol lookup since stage0 loads after stage1
  (let ((clear-fn (find-symbol "CLEAR-ERROR-ANALYSIS" :clysm/stage0)))
    (when clear-fn (funcall clear-fn)))
  (let ((total (length forms))
        (compiled 0)
        (failed 0)
        (skipped 0)  ; T010: Add skipped counter
        (results nil)
        (successful-sexps nil))
    (loop for form in forms
          for index from 1
          do (let* ((sexp (if (source-form-p form)
                              (source-form-sexp form)
                              form))
                    (form-id (if (source-form-p form)
                                 (source-form-id form)
                                 "0:0"))
                    ;; Phase 13D M4: Extract module path from form's module
                    (module-path (if (and (source-form-p form)
                                          (source-form-module form))
                                     (source-module-relative-path (source-form-module form))
                                     "unknown")))
               (multiple-value-bind (result bytes err-condition)
                   (test-form-compilation sexp :validate validate)
                 (declare (ignore bytes))
                 ;; T011: Add case branch for :skipped
                 (cond
                   ;; Skipped (directive form)
                   ((eq result :skipped)
                    (incf skipped)
                    (push (make-compilation-result
                           :form form
                           :form-id form-id
                           :success-p :skipped)
                          results))
                   ;; Compiled successfully
                   (result
                    (incf compiled)
                    (push sexp successful-sexps)
                    (push (make-compilation-result
                           :form form
                           :form-id form-id
                           :success-p t)
                          results))
                   ;; Failed
                   (t
                    (incf failed)
                    ;; Phase 13D M4: Capture detailed error for DEFUN forms
                    (when (and (consp sexp) (eq (car sexp) 'defun) err-condition)
                      (let ((collect-fn (find-symbol "COLLECT-DEFUN-ERROR" :clysm/stage0)))
                        (when collect-fn
                          (let ((fn-name (if (and (cdr sexp) (symbolp (cadr sexp)))
                                             (symbol-name (cadr sexp))
                                             "UNKNOWN"))
                                (lambda-list (when (and (cddr sexp) (listp (caddr sexp)))
                                               (format nil "~S" (caddr sexp)))))
                            (funcall collect-fn
                                     fn-name module-path err-condition
                                     :lambda-list lambda-list
                                     :form sexp)))))
                    (push (make-compilation-result
                           :form form
                           :form-id form-id
                           :success-p nil
                           :error-message (if err-condition
                                              (format nil "~A" err-condition)
                                              "Compilation or validation failed"))
                          results))))
               (when progress-callback
                 (funcall progress-callback index total t))))
    (values (nreverse successful-sexps)
            (nreverse results)
            ;; T012: Include :skipped in stats
            (list :compiled compiled :failed failed :skipped skipped :total total))))

(defun validate-wasm-bytes (bytes)
  "Validate Wasm bytes by writing to temp file and running wasm-tools.
Returns T if valid, NIL otherwise."
  (let ((temp-path (merge-pathnames
                    (format nil "clysm-test-~D.wasm" (get-universal-time))
                    (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (out temp-path
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence bytes out))
          (multiple-value-bind (output error-output status)
              (uiop:run-program (list "wasm-tools" "validate" (namestring temp-path))
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output error-output))
            (zerop status)))
      (when (probe-file temp-path)
        (delete-file temp-path)))))

(defun compile-defuns-bundle (defuns)
  "Compile a list of defuns into a bundled module for validation.
Returns Wasm bytes or NIL on failure.
Note: This is used during binary search, so does not add extra exports."
  (handler-case
      (clysm:compile-to-wasm `(progn ,@defuns))
    (error () nil)))

(defun actual-defun-p (form)
  "Check if a form is an actual defun that creates a Wasm function.
Only (defun ...) forms create separate functions in the Wasm module.
Other bundleable forms (defconstant, defvar, defparameter, defclass)
go into the _start function and don't create separate functions."
  (and (consp form) (eq (car form) 'defun)))

(defun compile-defuns-bundle-with-exports (defuns)
  "Compile a list of defuns into a bundled module with Stage 1 exports.
Returns Wasm bytes or NIL on failure.
Phase 13D-9: Includes stub functions for compile_form, compile_all, _initialize
and exports them."
  ;; Count only actual defuns that create separate Wasm functions
  (let ((actual-defun-count (count-if #'actual-defun-p defuns)))
    (handler-case
        (let* (;; Add stub functions after the regular defuns
               ;; These are the compiler export functions for Stage 1
               (stub-defuns (list
                             ;; compile_form: takes an S-expression, returns Wasm bytes or nil
                             '(defun clysm-stage1-compile-form (form)
                               ;; Stub: return nil (not yet implemented)
                               (if form nil nil))
                             ;; compile_all: compile all forms from source
                             '(defun clysm-stage1-compile-all (source-list)
                               ;; Stub: return nil (not yet implemented)
                               (if source-list nil nil))
                             ;; _initialize: runtime initialization
                             '(defun clysm-stage1-initialize ()
                               ;; Stub: return 0 (success)
                               0)))
               ;; Combine all defuns
               (all-defuns (append defuns stub-defuns))
               ;; Calculate export indices based on ACTUAL defuns, not all forms
               ;; Index 0 is _start (main)
               ;; Indices 1 to N are the actual defuns (not defvar/defconstant/etc)
               ;; Indices N+1, N+2, N+3 are the stub functions
               (compile-form-idx (1+ actual-defun-count))
               (compile-all-idx (+ 2 actual-defun-count))
               (initialize-idx (+ 3 actual-defun-count))
               ;; Define extra exports
               (extra-exports (list
                               (list "compile_form" :func compile-form-idx)
                               (list "compile_all" :func compile-all-idx)
                               (list "_initialize" :func initialize-idx))))
          (clysm:compile-to-wasm `(progn ,@all-defuns)
                                  :extra-exports extra-exports))
      (error () nil))))

(defun find-valid-bundle (defuns)
  "Find the largest prefix of defuns that produces valid Wasm.
Uses binary search for efficiency.
Returns (values wasm-bytes num-defuns)."
  (let ((n (length defuns)))
    (when (zerop n)
      (return-from find-valid-bundle (values nil 0)))
    ;; Try full bundle first
    (let ((bytes (compile-defuns-bundle defuns)))
      (when (and bytes (validate-wasm-bytes bytes))
        (return-from find-valid-bundle (values bytes n))))
    ;; Binary search for largest valid prefix
    (let ((lo 1)
          (hi (1- n))
          (best-bytes nil)
          (best-count 0))
      (loop while (<= lo hi)
            do (let* ((mid (floor (+ lo hi) 2))
                      (subset (subseq defuns 0 mid))
                      (bytes (compile-defuns-bundle subset)))
                 (if (and bytes (validate-wasm-bytes bytes))
                     (progn
                       (setf best-bytes bytes
                             best-count mid
                             lo (1+ mid)))
                     (setf hi (1- mid)))))
      (values best-bytes best-count))))

(defun bundleable-form-p (sexp)
  "Check if a form can be bundled into a progn for compilation.
Returns T for forms that produce meaningful Wasm output."
  (and (consp sexp)
       (member (car sexp) '(defun defconstant defvar defparameter defclass))))

(defun bundle-and-compile (successful-sexps)
  "Bundle successful forms and compile to a valid module.
Returns Wasm bytes or NIL on failure.
Phase 13D-9: After finding valid bundle, recompile with Stage 1 exports."
  (when successful-sexps
    ;; Include all bundleable forms, not just defuns
    (let* ((bundleable (remove-if-not #'bundleable-form-p successful-sexps))
           ;; Limit to reasonable size
           (limited (if (> (length bundleable) 500)
                        (subseq bundleable 0 500)
                        bundleable)))
      (when limited
        (format t "~&Finding largest valid bundle from ~D forms...~%" (length limited))
        (multiple-value-bind (bytes count)
            (find-valid-bundle limited)
          (declare (ignore bytes))  ; We'll recompile with exports
          (format t "~&Valid bundle contains ~D forms~%" count)
          ;; Phase 13D-9: Recompile the valid subset with Stage 1 exports
          (when (> count 0)
            (let* ((valid-forms (subseq limited 0 count))
                   (final-bytes (compile-defuns-bundle-with-exports valid-forms)))
              (if (and final-bytes (validate-wasm-bytes final-bytes))
                  final-bytes
                  ;; Fallback: try without exports if export compilation fails
                  (progn
                    (format t "~&Warning: compilation with exports failed, using base bundle~%")
                    (compile-defuns-bundle valid-forms))))))))))

(defun create-stage1-stub-module ()
  "Create a minimal valid Stage 1 Wasm module stub.
Phase 13D-9: Now exports compile_form, compile_all, _initialize in addition to _start."
  ;; Module header: \0asm + version 1
  (let ((header #(#x00 #x61 #x73 #x6D #x01 #x00 #x00 #x00))
        ;; Type section: 2 function types
        ;; Type 0: () -> i32 (for _start)
        ;; Type 1: (anyref) -> anyref (for compile_form, compile_all)
        (type-section #(#x01 #x0B #x02
                        #x60 #x00 #x01 #x7F           ; Type 0: () -> i32
                        #x60 #x01 #x6E #x01 #x6E))    ; Type 1: (anyref) -> anyref
        ;; Function section: 4 functions
        ;; Func 0: type 0 (_start)
        ;; Func 1: type 1 (compile_form)
        ;; Func 2: type 1 (compile_all)
        ;; Func 3: type 0 (_initialize)
        (func-section #(#x03 #x05 #x04 #x00 #x01 #x01 #x00))
        ;; Export section: 4 exports
        ;; _start (func 0), compile_form (func 1), compile_all (func 2), _initialize (func 3)
        (export-section (create-stage1-export-section))
        ;; Code section: 4 function bodies
        (code-section (create-stage1-code-section)))
    (let* ((total-size (+ (length header)
                          (length type-section)
                          (length func-section)
                          (length export-section)
                          (length code-section)))
           (module (make-array total-size :element-type '(unsigned-byte 8)))
           (offset 0))
      (dolist (section (list header type-section func-section export-section code-section))
        (replace module section :start1 offset)
        (incf offset (length section)))
      module)))

(defun create-stage1-export-section ()
  "Create export section with _start, compile_form, compile_all, _initialize.
Phase 13D-9: Exports all required compiler functions."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Section ID = 7 (export)
    (vector-push-extend #x07 content)
    ;; Section size placeholder - will be filled after building content
    (let ((exports-content (make-array 0 :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer 0)))
      ;; Number of exports: 4
      (vector-push-extend #x04 exports-content)
      ;; Export 0: _start (func 0)
      (emit-export-entry exports-content "_start" #x00 0)
      ;; Export 1: compile_form (func 1)
      (emit-export-entry exports-content "compile_form" #x00 1)
      ;; Export 2: compile_all (func 2)
      (emit-export-entry exports-content "compile_all" #x00 2)
      ;; Export 3: _initialize (func 3)
      (emit-export-entry exports-content "_initialize" #x00 3)
      ;; Now build the final section with proper size
      (emit-leb128-to-vector (length exports-content) content)
      (loop for b across exports-content do (vector-push-extend b content)))
    (coerce content '(simple-array (unsigned-byte 8) (*)))))

(defun emit-export-entry (buffer name kind index)
  "Emit a single export entry to buffer.
NAME is the export name string, KIND is export kind (0=func), INDEX is the index."
  ;; Name length
  (emit-leb128-to-vector (length name) buffer)
  ;; Name bytes (ASCII)
  (loop for c across name do (vector-push-extend (char-code c) buffer))
  ;; Export kind
  (vector-push-extend kind buffer)
  ;; Export index
  (emit-leb128-to-vector index buffer))

(defun emit-leb128-to-vector (value vector)
  "Emit unsigned LEB128 to adjustable vector."
  (loop
    (let ((byte (logand value #x7F)))
      (setf value (ash value -7))
      (if (zerop value)
          (progn
            (vector-push-extend byte vector)
            (return))
          (vector-push-extend (logior byte #x80) vector)))))

(defun create-stage1-code-section ()
  "Create code section with 4 function bodies.
Phase 13D-9: Stub implementations for all exports."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Section ID = 10 (code)
    (vector-push-extend #x0A content)
    ;; Build function bodies
    (let ((bodies-content (make-array 0 :element-type '(unsigned-byte 8)
                                        :adjustable t :fill-pointer 0)))
      ;; Number of function bodies: 4
      (vector-push-extend #x04 bodies-content)
      ;; Function 0: _start - returns 0 (success)
      ;; Body: 0 locals, i32.const 0, end
      (emit-func-body bodies-content '(#x00 #x41 #x00 #x0B))
      ;; Function 1: compile_form - stub returning ref.null
      ;; Body: 0 locals, ref.null any, end
      (emit-func-body bodies-content '(#x00 #xD0 #x6E #x0B))
      ;; Function 2: compile_all - stub returning ref.null
      ;; Body: 0 locals, ref.null any, end
      (emit-func-body bodies-content '(#x00 #xD0 #x6E #x0B))
      ;; Function 3: _initialize - returns 0
      ;; Body: 0 locals, i32.const 0, end
      (emit-func-body bodies-content '(#x00 #x41 #x00 #x0B))
      ;; Now build the final section with proper size
      (emit-leb128-to-vector (length bodies-content) content)
      (loop for b across bodies-content do (vector-push-extend b content)))
    (coerce content '(simple-array (unsigned-byte 8) (*)))))

(defun emit-func-body (buffer instructions)
  "Emit a function body with given instruction bytes."
  ;; Body size
  (emit-leb128-to-vector (length instructions) buffer)
  ;; Instructions
  (loop for b in instructions do (vector-push-extend b buffer)))

;;; ==========================================================================
;;; Binary Output
;;; ==========================================================================

(defun write-stage1-binary (bytes output-path)
  "Write Stage 1 binary to file."
  (with-open-file (out output-path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-sequence bytes out)
    (length bytes)))

(defun validate-stage1 (path)
  "Validate Stage 1 binary using wasm-tools."
  (multiple-value-bind (output error-output status)
      (uiop:run-program (list "wasm-tools" "validate" (namestring path))
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore output))
    (if (zerop status)
        t
        (error 'stage1-stage0-invalid
               :stage0-path (namestring path)
               :context error-output))))

;;; ==========================================================================
;;; Main Generation Entry Point (Phase 13D-7)
;;; ==========================================================================

(defun generate-stage1 (&key (output-path nil)
                              (report-path nil)
                              (validate t)
                              (progress-callback nil))
  "Generate Stage 1 binary from Clysm source.

Phase 13D-7: Classifies forms, finds the largest valid bundle of defuns,
and produces a valid Wasm module."
  (let* ((root (asdf:system-source-directory :clysm))
         (output (or output-path (merge-pathnames "dist/clysm-stage1.wasm" root)))
         (report-out (or report-path (merge-pathnames "dist/stage1-report.json" root)))
         (modules (read-all-modules)))
    (let ((all-forms (all-compilable-forms modules)))
      ;; Phase 1: Classify forms
      (format t "~&Classifying ~D forms...~%" (length all-forms))
      (multiple-value-bind (successful-sexps results stats)
          (classify-forms all-forms :progress-callback progress-callback)
        (format t "~&Classification: ~D succeeded, ~D failed~%"
                (getf stats :compiled) (getf stats :failed))
        ;; Phase 2: Bundle and compile with validation
        (let* ((bundled-bytes (bundle-and-compile successful-sexps))
               (bytes (or bundled-bytes (create-stage1-stub-module)))
               (report (generate-progress-report modules results)))
          (format t "~&Output size: ~D bytes~%" (length bytes))
          ;; Write binary
          (ensure-directories-exist output)
          (write-stage1-binary bytes output)
          ;; Validate if requested
          (when validate
            (format t "~&Final validation...~%")
            (validate-stage1 output)
            (format t "~&Validation PASSED~%"))
          ;; Write report
          (ensure-directories-exist report-out)
          (with-open-file (s report-out
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
            (write-progress-report report :stream s :format :json))
          ;; Phase 13D M4: Write detailed DEFUN error log
          ;; Use runtime symbol lookup since stage0 loads after stage1
          (let ((write-fn (find-symbol "WRITE-DEFUN-ERRORS-JSON" :clysm/stage0))
                (errors-var (find-symbol "*DEFUN-ERRORS*" :clysm/stage0))
                (defun-errors-path (merge-pathnames "dist/defun-errors.json" root)))
            (when write-fn
              (format t "~&Writing DEFUN error log to ~A~%" defun-errors-path)
              (funcall write-fn (namestring defun-errors-path))
              (format t "~&DEFUN errors logged: ~D entries~%"
                      (if errors-var (length (symbol-value errors-var)) 0))))
          report)))))
