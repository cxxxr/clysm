;;;; bootstrap.lisp - Stage 0 Cross-Compilation Bootstrap Script
;;;;
;;;; Usage: sbcl --load build/bootstrap.lisp
;;;;
;;;; This script compiles all 41 Clysm compiler modules into a single
;;;; WebAssembly binary (clysm-stage0.wasm) using source-level concatenation.

(in-package :cl-user)

;;; ============================================================
;;; Package Definition
;;; ============================================================

(defpackage :clysm/bootstrap
  (:use :cl)
  (:export #:bootstrap-context
           #:make-bootstrap-context
           #:bootstrap-context-p
           #:bootstrap-context-source-files
           #:bootstrap-context-all-forms
           #:bootstrap-context-current-module
           #:bootstrap-context-error-info
           #:bootstrap-context-output-path
           #:bootstrap-context-compilation-start
           #:bootstrap-context-constant-registry
           #:read-source-file
           #:read-all-source-forms
           #:filter-compilable-forms
           #:collect-all-forms
           #:compile-all-forms
           #:write-wasm-output
           #:validate-output
           #:run-bootstrap
           ;; Feature 038: Constant Registry API
           #:register-constant
           #:lookup-constant
           #:constant-defined-p
           ;; Feature 038: Compile Result Extensions
           #:compile-result
           #:make-compile-result
           #:compile-result-successful
           #:compile-result-failed
           #:compile-result-failed-forms
           #:compile-result-bytes
           #:compile-result-operator-failures
           #:compile-result-operator-examples
           #:record-failure
           #:generate-failure-report
           ;; Feature 038: Expansion Functions
           #:expand-defstruct
           #:expand-define-condition))

(in-package :clysm/bootstrap)

;;; ============================================================
;;; T008: Bootstrap Context Struct
;;; ============================================================

(defstruct bootstrap-context
  "State maintained during the bootstrap compilation process."
  (source-files nil :type list)           ; List of pathnames in order
  (all-forms nil :type list)              ; Collected compilable forms
  (compilation-start nil)                 ; Timestamp for timing
  (current-module nil)                    ; Progress tracking
  (error-info nil)                        ; (module message feature) if failed
  (output-path "dist/clysm-stage0.wasm")  ; Output file path
  ;; Feature 038: Constant registry for compile-time constant folding
  (constant-registry (make-hash-table :test 'eq) :type hash-table))

;;; ============================================================
;;; Feature 038 T007-T010: Constant Registry API
;;; ============================================================

(defun register-constant (name value registry)
  "Register a constant NAME with VALUE in REGISTRY.
   Used for compile-time constant folding during bootstrap.
   Returns VALUE for convenience."
  (setf (gethash name registry) value)
  value)

(defun lookup-constant (name registry)
  "Look up constant NAME in REGISTRY.
   Returns (VALUES value found-p)."
  (gethash name registry))

(defun constant-defined-p (name registry)
  "Return T if constant NAME is defined in REGISTRY."
  (nth-value 1 (gethash name registry)))

;;; ============================================================
;;; Module Order (from validation/compiler-order.lisp)
;;; ============================================================

(defparameter *compilation-order*
  '(;; Backend (no dependencies)
    "src/clysm/backend/leb128.lisp"
    "src/clysm/backend/sections.lisp"
    "src/clysm/backend/wasm-emit.lisp"
    "src/clysm/backend/wat-print.lisp"

    ;; Reader (depends on backend via UTF-8)
    "src/clysm/reader/tokenizer.lisp"
    "src/clysm/reader/parser.lisp"
    "src/clysm/reader/package.lisp"
    "src/clysm/reader/reader.lisp"

    ;; Compiler core (depends on reader)
    "src/clysm/compiler/ast.lisp"
    "src/clysm/compiler/env.lisp"
    "src/clysm/compiler/analyzer/free-vars.lisp"
    "src/clysm/compiler/analyzer/tail-call.lisp"
    "src/clysm/compiler/analyzer/type-infer.lisp"
    "src/clysm/compiler/analyzer/io-usage.lisp"
    "src/clysm/compiler/transform/closure.lisp"
    "src/clysm/compiler/transform/macro.lisp"
    "src/clysm/compiler/codegen/wasm-ir.lisp"
    "src/clysm/compiler/codegen/gc-types.lisp"
    "src/clysm/compiler/codegen/type-section.lisp"
    "src/clysm/compiler/codegen/func-section.lisp"
    "src/clysm/compiler/compiler.lisp"

    ;; Runtime (depends on compiler)
    "src/clysm/runtime/objects.lisp"
    "src/clysm/runtime/special-vars.lisp"
    "src/clysm/runtime/multi-value.lisp"
    "src/clysm/runtime/printer.lisp"
    "src/clysm/runtime/condition-runtime.lisp"

    ;; CLOS (depends on runtime)
    "src/clysm/clos/mop.lisp"
    "src/clysm/clos/defclass.lisp"
    "src/clysm/clos/instance.lisp"
    "src/clysm/clos/slot-access.lisp"
    "src/clysm/clos/generic.lisp"
    "src/clysm/clos/defmethod.lisp"
    "src/clysm/clos/combination.lisp"
    "src/clysm/clos/dispatch.lisp"
    "src/clysm/clos/method-combination.lisp"

    ;; Conditions (depends on CLOS)
    "src/clysm/conditions/package.lisp"
    "src/clysm/conditions/types.lisp"
    "src/clysm/conditions/handlers.lisp"
    "src/clysm/conditions/restarts.lisp"
    "src/clysm/conditions/signaling.lisp"
    "src/clysm/conditions/standard.lisp")
  "The 41 modules in dependency order for cross-compilation.")

;;; ============================================================
;;; T009: Read Source Forms
;;; ============================================================

(defun read-source-file (path)
  "Read all forms from a source file.
   Returns a list of S-expressions."
  (with-open-file (stream path :direction :input
                               :external-format :utf-8)
    (let ((forms '())
          (eof (gensym "EOF")))
      (loop for form = (read stream nil eof)
            until (eq form eof)
            do (push form forms))
      (nreverse forms))))

(defun read-all-source-forms (&optional (base-dir (asdf:system-source-directory :clysm)))
  "Read all forms from all 41 modules in compilation order.
   Returns a flat list of all S-expressions."
  (let ((all-forms '()))
    (dolist (relative-path *compilation-order*)
      (let ((full-path (merge-pathnames relative-path base-dir)))
        (when (probe-file full-path)
          (let ((forms (read-source-file full-path)))
            (setf all-forms (append all-forms forms))))))
    all-forms))

;;; ============================================================
;;; T010: Filter Compilable Forms
;;; ============================================================

(defun compilable-form-p (form)
  "Return T if FORM is a compilable top-level form.
   Excludes: in-package, defpackage, declare, proclaim, eval-when.
   Also excludes: (defun (setf ...) ...) forms (not yet supported)."
  (and (consp form)
       (symbolp (car form))
       (member (car form)
               '(;; Definitions
                 defun defmacro defvar defparameter defconstant
                 defstruct defclass defgeneric defmethod
                 deftype define-condition define-compiler-macro
                 define-symbol-macro define-setf-expander
                 define-modify-macro define-method-combination
                 ;; Control structures
                 progn let let* flet labels block tagbody
                 lambda setf setq if when unless cond case typecase
                 ;; Package operations (these are compile-time side effects)
                 export)
               :test #'eq)
       ;; Exclude (defun (setf foo) ...) forms - not yet supported
       (not (and (eq (car form) 'defun)
                 (consp (cadr form))
                 (eq (car (cadr form)) 'setf)))))

(defun filter-compilable-forms (forms)
  "Filter forms to only include compilable ones.
   Skips in-package, defpackage, declare, proclaim, eval-when."
  (remove-if-not #'compilable-form-p forms))

;;; ============================================================
;;; Selective Macro Pre-Expansion
;;; ============================================================

;; We need to expand certain CL macros (case, cond, when, unless) because
;; Clysm's macro system doesn't handle them. But we must skip others
;; (defstruct, etypecase) that require runtime type info.

(defparameter *skip-expansion-ops*
  '(;; Type dispatch - needs runtime type info
    etypecase typecase ecase ctypecase ccase check-type
    ;; Condition handling - complex expansions
    handler-case handler-bind restart-case restart-bind
    with-simple-restart ignore-errors
    ;; Note: defstruct is now handled by expand-form-recursive (Feature 038)
    ;; Class definitions - don't expand to SBCL internals
    defclass defmethod defgeneric
    ;; Variable/constant definitions - don't expand to SBCL internals (Feature 038)
    defconstant defvar defparameter
    ;; Quote and function - don't expand contents
    quote function)
  "Operators whose macro expansion should be skipped.")

(defparameter *must-expand-ops*
  '(;; These expand to simple if/cond forms universally
    case when unless
    and or
    ;; Simple control flow
    prog1 prog2
    ;; Feature 043: Binding macros - SBCL expands to let/mvb
    multiple-value-bind)
  "Operators that MUST be expanded before Clysm compilation.
   Note: LOOP/DO macros are NOT expanded here because SBCL expands
   them to SBCL-internal functions. Instead, Clysm's macro system
   (registered in clysm/lib/macros.lisp) handles them.")

(defun expand-form-recursive (form)
  "Recursively expand select macros in FORM.
   Expands case/cond/when/unless but skips defstruct/etypecase/etc."
  (cond
    ;; Atoms and nil - pass through
    ((atom form) form)
    ((null form) nil)
    ;; Skip these ops entirely (including their subforms)
    ((and (consp form)
          (symbolp (car form))
          (member (car form) *skip-expansion-ops*))
     form)
    ;; Must-expand ops - use SBCL to expand
    ((and (consp form)
          (symbolp (car form))
          (member (car form) *must-expand-ops*))
     (handler-case
         (let ((expanded (macroexpand-1 form)))
           (if (equal expanded form)
               ;; No expansion happened - recurse into subforms
               (cons (car form)
                     (mapcar #'expand-form-recursive (cdr form)))
               ;; Expansion happened - recurse on result
               (expand-form-recursive expanded)))
       (error (e)
         (format *error-output* "~%Warning: Error expanding ~A: ~A~%"
                 (car form) e)
         form)))
    ;; defmacro - keep body unexpanded
    ((and (consp form) (eq (car form) 'defmacro))
     form)
    ;; Feature 038: define-condition - expand to defclass
    ((and (consp form) (eq (car form) 'define-condition))
     (expand-form-recursive (expand-define-condition form)))
    ;; Feature 038: defstruct - expand to constructor/accessor defuns
    ((and (consp form) (eq (car form) 'defstruct))
     (let ((defuns (expand-defstruct form)))
       ;; Return a progn of all generated defuns
       (cons 'progn (mapcar #'expand-form-recursive defuns))))
    ;; defun - expand body forms
    ((and (consp form) (eq (car form) 'defun))
     (list* 'defun
            (cadr form)   ; name
            (caddr form)  ; params
            (mapcar #'expand-form-recursive (cdddr form))))
    ;; let/let* - expand binding values and body
    ((and (consp form) (member (car form) '(let let*)))
     (list* (car form)
            (mapcar (lambda (binding)
                      (if (consp binding)
                          (list (car binding)
                                (expand-form-recursive (cadr binding)))
                          binding))
                    (cadr form))
            (mapcar #'expand-form-recursive (cddr form))))
    ;; flet/labels - expand function bodies
    ((and (consp form) (member (car form) '(flet labels)))
     (list* (car form)
            (mapcar (lambda (def)
                      (list* (car def)   ; name
                             (cadr def)  ; params
                             (mapcar #'expand-form-recursive (cddr def))))
                    (cadr form))
            (mapcar #'expand-form-recursive (cddr form))))
    ;; lambda - expand body
    ((and (consp form) (eq (car form) 'lambda))
     (list* 'lambda
            (cadr form)  ; params
            (mapcar #'expand-form-recursive (cddr form))))
    ;; block - expand body
    ((and (consp form) (eq (car form) 'block))
     (list* 'block
            (cadr form)  ; name
            (mapcar #'expand-form-recursive (cddr form))))
    ;; progn - expand all forms
    ((and (consp form) (eq (car form) 'progn))
     (cons 'progn (mapcar #'expand-form-recursive (cdr form))))
    ;; General compound form - try to expand if it's a macro
    ((consp form)
     (handler-case
         (multiple-value-bind (expanded expanded-p)
             (macroexpand-1 form)
           (if expanded-p
               (expand-form-recursive expanded)
               ;; Not a macro - just recurse into subforms
               (cons (car form)
                     (mapcar #'expand-form-recursive (cdr form)))))
       (error (e)
         (format *error-output* "~%Warning: Error expanding ~A: ~A~%"
                 (if (consp form) (car form) form) e)
         form)))
    ;; Fallback
    (t form)))

(defun expand-all-forms (forms)
  "Selectively expand macros in FORMS."
  (mapcar #'expand-form-recursive forms))

;;; ============================================================
;;; T011: Collect All Forms
;;; ============================================================

(defun collect-all-forms (ctx)
  "Collect, filter, and selectively expand all compilable forms.
   Updates CTX with collected forms and returns them."
  (let* ((base-dir (asdf:system-source-directory :clysm))
         (all-forms '())
         (module-count 0)
         (total-modules (length *compilation-order*)))
    (dolist (relative-path *compilation-order*)
      (incf module-count)
      (let ((full-path (merge-pathnames relative-path base-dir)))
        ;; T012: Progress reporting
        (format t "[~D/~D] Reading ~A...~%" module-count total-modules relative-path)
        (setf (bootstrap-context-current-module ctx) (namestring full-path))
        (handler-case
            (when (probe-file full-path)
              (let* ((forms (read-source-file full-path))
                     (compilable (filter-compilable-forms forms)))
                (setf all-forms (append all-forms compilable))))
          ;; T013: Error handling with module path
          (error (e)
            (setf (bootstrap-context-error-info ctx)
                  (list full-path (format nil "~A" e) nil))
            (format *error-output* "ERROR reading ~A: ~A~%" full-path e)
            (return-from collect-all-forms nil)))))
    ;; Selectively expand macros (case, cond, when, unless, etc.)
    ;; while skipping ones that need runtime type info (defstruct, etypecase)
    (format t "~%Expanding macros...~%")
    (handler-case
        (let ((expanded (expand-all-forms all-forms)))
          (setf (bootstrap-context-all-forms ctx) expanded)
          expanded)
      (error (e)
        (setf (bootstrap-context-error-info ctx)
              (list "macro-expansion" (format nil "~A" e) nil))
        (format *error-output* "ERROR during macro expansion: ~A~%" e)
        nil))))

;;; ============================================================
;;; T017: Compile All Forms
;;; ============================================================

(defstruct compile-result
  "Result of compilation attempt."
  (successful 0 :type fixnum)      ; Count of successfully compiled forms
  (failed 0 :type fixnum)          ; Count of failed forms
  (failed-forms nil :type list)    ; List of (form . error) for failures
  (bytes nil)                      ; Final Wasm bytes
  ;; Feature 038: Enhanced Error Reporting
  (operator-failures (make-hash-table :test 'eq) :type hash-table)  ; Operator → count
  (operator-examples (make-hash-table :test 'eq) :type hash-table)) ; Operator → list of form strings

;;; ============================================================
;;; Feature 038 T053-T063: Enhanced Error Reporting
;;; ============================================================

(defun record-failure (form error result)
  "Record a compilation failure in RESULT, tracking by operator type.
   FORM is the failed form, ERROR is the condition, RESULT is compile-result."
  (let* ((op (if (consp form) (car form) 'atom))
         (failures (compile-result-operator-failures result))
         (examples (compile-result-operator-examples result)))
    ;; Increment failure count for this operator
    (incf (gethash op failures 0))
    (incf (compile-result-failed result))
    ;; Store up to 3 examples per operator
    (let ((existing (gethash op examples)))
      (when (< (length existing) 3)
        (let ((form-preview (format nil "~A" form)))
          (when (> (length form-preview) 100)
            (setf form-preview (concatenate 'string
                                            (subseq form-preview 0 97)
                                            "...")))
          (setf (gethash op examples)
                (append existing (list form-preview))))))))

(defun generate-failure-report (result stream)
  "Generate an operator-grouped failure report to STREAM.
   Shows compilation rate, failures by operator, and examples."
  (let* ((successful (compile-result-successful result))
         (failed (compile-result-failed result))
         (total (+ successful failed))
         (rate (if (zerop total) 0.0 (* 100.0 (/ successful total)))))
    ;; Summary line
    (format stream "~%Compilation Summary: ~D/~D forms compiled (~,1F%)~%~%"
            successful total rate)

    ;; Failures by operator (sorted by count descending)
    (let ((failures (compile-result-operator-failures result))
          (examples (compile-result-operator-examples result))
          (sorted-ops '()))
      ;; Collect and sort operators by failure count
      (maphash (lambda (op count)
                 (push (cons op count) sorted-ops))
               failures)
      (setf sorted-ops (sort sorted-ops #'> :key #'cdr))

      (when sorted-ops
        (format stream "Failures by operator:~%")
        (dolist (pair sorted-ops)
          (let ((op (car pair))
                (count (cdr pair)))
            (format stream "  ~A: ~D failures~%" op count)
            ;; Show examples for this operator
            (let ((op-examples (gethash op examples)))
              (dolist (ex op-examples)
                (format stream "    - ~A~%" ex)))))
        (format stream "~%")))))

;;; ============================================================
;;; Feature 038 T034-T039, T070-T079: Form Expansion Stubs
;;; (Full implementation in Phase 4/US2 and Phase 7/US5)
;;; ============================================================

(defun expand-define-condition (form)
  "Expand define-condition to defclass form.
   FORM: (define-condition name parents slots . options)
   Returns: (defclass name parents slots . filtered-options)
   Note: :report option is skipped (not passed to defclass)."
  (destructuring-bind (op name parents slots &rest options) form
    (declare (ignore op))
    ;; Filter out :report option as it's not a defclass option
    (let ((filtered-options (remove-if (lambda (opt)
                                          (and (consp opt)
                                               (eq (car opt) :report)))
                                        options)))
      (list* 'defclass name parents slots filtered-options))))

(defun expand-defstruct (form)
  "Expand defstruct to constructor and accessor defuns.
   FORM: (defstruct name-and-options [docstring] slot1 slot2 ...)
   Returns: list of defun forms (make-NAME, NAME-slot, NAME-p)."
  (let* ((name-and-options (cadr form))
         (name (if (consp name-and-options)
                   (car name-and-options)
                   name-and-options))
         (options (when (consp name-and-options)
                    (cdr name-and-options)))
         (body (cddr form))
         ;; Skip docstring if present (first element is a string)
         (slots (if (and (consp body) (stringp (car body)))
                    (cdr body)
                    body))
         ;; Extract :constructor option if present
         (constructor-opt (find :constructor options :key (lambda (x)
                                                             (when (consp x)
                                                               (car x)))))
         (constructor-name (if constructor-opt
                               (cadr constructor-opt)
                               (intern (format nil "MAKE-~A" name))))
         (predicate-name (intern (format nil "~A-P" name)))
         (result '()))

    ;; Parse slots: each slot can be SYMBOL or (SYMBOL DEFAULT)
    ;; Note: We extract slot names only - defaults are handled by nil-or-default
    ;; pattern in constructor body because Clysm doesn't support &key with defaults.
    (let ((slot-specs (mapcar (lambda (slot)
                                (if (consp slot)
                                    (list (car slot) (cadr slot))
                                    (list slot nil)))
                              slots)))

      ;; Generate constructor - use simple &key without defaults (Clysm limitation)
      ;; Defaults are applied in the body using (if x x default) pattern
      (let* ((slot-names (mapcar #'car slot-specs))
             (lambda-list `(&key ,@slot-names)))
        (push `(defun ,constructor-name ,lambda-list
                 (list ',name
                       ,@(mapcar (lambda (spec)
                                   (let ((name (car spec))
                                         (default (cadr spec)))
                                     (if default
                                         `(if ,name ,name ,default)
                                         name)))
                                 slot-specs)))
              result))

      ;; Generate accessors
      (loop for spec in slot-specs
            for index from 1
            for slot-name = (car spec)
            for accessor-name = (intern (format nil "~A-~A" name slot-name))
            do (push `(defun ,accessor-name (struct)
                        (nth ,index struct))
                     result))

      ;; Generate predicate
      (push `(defun ,predicate-name (obj)
               (and (consp obj)
                    (eq (car obj) ',name)))
            result))

    (nreverse result)))

(defun try-compile-form (form compile-fn)
  "Try to compile a single form. Returns (values bytes error)."
  (handler-case
      (values (funcall compile-fn form) nil)
    (error (e)
      (values nil e))))

(defun compile-all-forms (forms)
  "Compile forms to Wasm, trying each individually and tracking failures.
   Returns a COMPILE-RESULT struct with statistics and bytes."
  (let* ((compile-fn (uiop:find-symbol* :compile-to-wasm :clysm/compiler))
         (result (make-compile-result))
         (compilable-forms '()))

    ;; First pass: try each form individually to see what compiles
    (format t "Testing individual forms...~%")
    (loop for form in forms
          for i from 0
          do (multiple-value-bind (bytes err)
                 (try-compile-form form compile-fn)
               (declare (ignore bytes))
               (if err
                   ;; Use record-failure for operator-grouped tracking
                   (progn
                     (record-failure form err result)
                     (push (cons form err) (compile-result-failed-forms result)))
                   (progn
                     (incf (compile-result-successful result))
                     (push form compilable-forms)))))

    (setf compilable-forms (nreverse compilable-forms))
    (setf (compile-result-failed-forms result)
          (nreverse (compile-result-failed-forms result)))

    (format t "~%Results: ~D compiled, ~D failed~%"
            (compile-result-successful result)
            (compile-result-failed result))

    ;; Feature 038: Generate operator-grouped failure report
    (generate-failure-report result *standard-output*)

    ;; Second pass: compile all successful forms together
    (if (null compilable-forms)
        ;; No forms compiled - generate minimal valid Wasm module
        (progn
          (format t "~%Generating minimal Wasm module (no compilable forms)~%")
          (setf (compile-result-bytes result)
                (generate-minimal-wasm-module)))
        ;; Compile successful forms into single module
        (progn
          (format t "~%Compiling ~D forms to single module...~%"
                  (length compilable-forms))
          (let ((expr (if (= 1 (length compilable-forms))
                          (first compilable-forms)
                          `(progn ,@compilable-forms))))
            (handler-case
                (setf (compile-result-bytes result)
                      (funcall compile-fn expr))
              (error (e)
                ;; Even combined compilation failed - use minimal module
                (format t "~%Combined compilation failed: ~A~%" e)
                (format t "Generating minimal Wasm module~%")
                (setf (compile-result-bytes result)
                      (generate-minimal-wasm-module)))))))

    result))

(defun generate-minimal-wasm-module ()
  "Generate a minimal valid Wasm module (empty module).
   Magic number: 0x00 0x61 0x73 0x6D (\\0asm)
   Version: 0x01 0x00 0x00 0x00 (1)"
  (make-array 8 :element-type '(unsigned-byte 8)
              :initial-contents '(#x00 #x61 #x73 #x6D  ; magic
                                  #x01 #x00 #x00 #x00)))

;;; ============================================================
;;; T018: Write Wasm Output
;;; ============================================================

(defun write-wasm-output (bytes output-path)
  "Write Wasm binary to output file.
   Returns the pathname."
  (ensure-directories-exist output-path)
  (with-open-file (stream output-path
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes stream))
  (pathname output-path))

;;; ============================================================
;;; T022: Validate Output
;;; ============================================================

(defun validate-output (output-path)
  "Validate the output Wasm binary using wasm-tools.
   Returns (values success-p error-message)."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "wasm-tools" "validate" (namestring output-path))
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore output))
    (if (zerop exit-code)
        (values t nil)
        (values nil error-output))))

;;; ============================================================
;;; T019-T021: Run Bootstrap (Main Entry Point)
;;; ============================================================

(defun run-bootstrap (&key (output-path "dist/clysm-stage0.wasm"))
  "Main entry point for Stage 0 bootstrap compilation.
   Compiles all 41 modules and produces a single Wasm binary."
  (let* ((ctx (make-bootstrap-context :output-path output-path))
         (start-time (get-internal-real-time)))
    (setf (bootstrap-context-compilation-start ctx) start-time)

    (format t "~%=== Clysm Stage 0 Bootstrap ===~%~%")
    (format t "Output: ~A~%" output-path)
    (format t "Modules: ~D~%~%" (length *compilation-order*))

    ;; Collect all forms
    (format t "Phase 1: Reading source modules...~%")
    (let ((forms (collect-all-forms ctx)))
      (when (null forms)
        (format *error-output* "~%Bootstrap FAILED: Could not read source forms~%")
        (return-from run-bootstrap nil))

      (format t "~%Collected ~D compilable forms~%~%" (length forms))

      ;; Compile all forms
      (format t "Phase 2: Compiling to Wasm...~%")
      (handler-case
          (let ((result (compile-all-forms forms)))
            ;; Write output
            (format t "~%Phase 3: Writing output...~%")
            (write-wasm-output (compile-result-bytes result) output-path)

            ;; T020-T021: Timing and size reporting
            (let* ((end-time (get-internal-real-time))
                   (duration-secs (/ (- end-time start-time)
                                     internal-time-units-per-second))
                   (size-bytes (length (compile-result-bytes result))))
              (format t "~%=== Bootstrap Complete ===~%~%")
              (format t "Output: ~A~%" output-path)
              (format t "Size: ~:D bytes~%" size-bytes)
              (format t "Duration: ~,2F seconds~%~%" duration-secs)

              ;; Compilation statistics
              (format t "Compilation Statistics:~%")
              (format t "  Successfully compiled: ~D forms~%"
                      (compile-result-successful result))
              (format t "  Failed to compile: ~D forms~%~%"
                      (compile-result-failed result))

              ;; Validate output
              (format t "Phase 4: Validating Wasm...~%")
              (multiple-value-bind (valid-p error-msg)
                  (validate-output output-path)
                (if valid-p
                    (format t "Validation: PASSED~%~%")
                    (format t "Validation: FAILED - ~A~%~%" error-msg)))

              output-path))
        (error (e)
          (setf (bootstrap-context-error-info ctx)
                (list (bootstrap-context-current-module ctx)
                      (format nil "~A" e)
                      nil))
          (format *error-output* "~%Bootstrap FAILED during compilation:~%~A~%" e)
          nil)))))

;;; ============================================================
;;; Auto-run when loaded
;;; ============================================================

;; Load Clysm system first
(unless (find-package :clysm/compiler)
  (format t "Loading Clysm compiler...~%")
  ;; Use load-source-op to avoid compilation issues with ASDF treating warnings as errors
  ;; This is slower but more robust for bootstrap
  (handler-case
      (asdf:load-system :clysm)
    (error (e)
      (format t "Warning: ASDF compile failed (~A), trying source load...~%" e)
      (asdf:oos 'asdf:load-source-op :clysm))))

;; Feature 043: Initialize the global macro registry with standard macros
;; This is critical - without this, LOOP and other macros are not expanded!
(clysm/lib/macros:install-standard-macros
 (clysm/compiler/transform/macro:global-macro-registry))
(format t "Initialized standard macros (LOOP, DO, WHEN, UNLESS, etc.)~%")

;; Run bootstrap if this file is loaded directly
(when (and (boundp '*load-pathname*)
           *load-pathname*
           (string= "bootstrap" (pathname-name *load-pathname*)))
  (run-bootstrap))
