;;;; ffi-usage.lisp - FFI usage analysis for selective import emission
;;;;
;;;; Feature: 001-ffi-import-architecture
;;;; Purpose: Detect which FFI functions are used in compiled code, enabling
;;;;          selective import of only needed functions. Also detects dynamic
;;;;          call patterns that require runtime function resolution.
;;;;
;;;; References:
;;;;   - [funcall](resources/HyperSpec/Body/f_funcal.htm)
;;;;   - [apply](resources/HyperSpec/Body/f_apply.htm)

(in-package #:clysm/compiler/analyzer/ffi-usage)

;;; ==========================================================================
;;; T003: FFI Analysis Result Structure
;;; ==========================================================================

(defstruct ffi-analysis
  "Result of FFI usage analysis on compiled code.
Returned by ANALYZE-FFI-USAGE."
  (used-ffis nil :type list)
  (has-dynamic-call-p nil :type boolean)
  (static-funcalls nil :type list)
  (dynamic-sites nil :type list))

;;; ==========================================================================
;;; T006: FFI Function Registry Access
;;; ==========================================================================

(defun get-ffi-function-names ()
  "Return a list of all registered FFI function names.
Reads from *ffi-environment* defined in clysm/ffi/macros.
The ffi-environment struct has an 'imports' hash-table keyed by symbol name."
  (let ((names nil)
        (env clysm/ffi::*ffi-environment*))
    (when (and env (clysm/ffi::ffi-environment-p env))
      (maphash (lambda (name decl)
                 (declare (ignore decl))
                 (push name names))
               (clysm/ffi::ffi-env-imports env)))
    names))

(defun ffi-function-p (name)
  "Check if NAME is a registered FFI function.
NAME can be a symbol or string. Looks up in *ffi-environment*'s imports hash-table.
The hash-table is keyed by symbols (the Lisp name of the FFI function)."
  (let ((env clysm/ffi::*ffi-environment*))
    (when (and name env (clysm/ffi::ffi-environment-p env))
      ;; Keys in the imports hash-table are symbols
      ;; Handle both symbol and string lookups
      (let ((imports (clysm/ffi::ffi-env-imports env)))
        (typecase name
          (symbol
           ;; Direct symbol lookup
           (or (gethash name imports)
               ;; Also try looking up by symbol-name if the symbol is from a different package
               (block found
                 (maphash (lambda (k v)
                            (when (string= (symbol-name k) (symbol-name name))
                              (return-from found v)))
                          imports)
                 nil)))
          (string
           ;; String lookup - search by symbol-name
           (block found
             (maphash (lambda (k v)
                        (when (string= (symbol-name k) name)
                          (return-from found v)))
                      imports)
             nil))
          (t nil))))))

;;; ==========================================================================
;;; T022-T024: Static/Dynamic Call Detection
;;; ==========================================================================

(defun quoted-symbol-p (form)
  "Check if FORM is a quoted symbol: (quote sym) or 'sym."
  (and (consp form)
       (eq (car form) 'quote)
       (consp (cdr form))
       (symbolp (cadr form))))

(defun function-ref-p (form)
  "Check if FORM is a function reference: (function sym) or #'sym."
  (and (consp form)
       (eq (car form) 'function)
       (consp (cdr form))
       (symbolp (cadr form))))

(defun detect-static-funcall-p (form)
  "Check if FORM is a static funcall (function known at compile time).
Returns the function name symbol if static, NIL if dynamic.

Static patterns:
  (funcall 'sym ...)     - quoted symbol
  (funcall #'sym ...)    - function reference"
  (when (and (consp form)
             (member (car form) '(funcall apply)))
    (let ((fn-arg (cadr form)))
      (cond
        ((quoted-symbol-p fn-arg)
         (cadr fn-arg))
        ((function-ref-p fn-arg)
         (cadr fn-arg))
        (t nil)))))

(defun detect-dynamic-call-p (form)
  "Check if FORM is a dynamic call (function computed at runtime).
Returns T if the funcall/apply uses a non-literal function argument."
  (when (and (consp form)
             (member (car form) '(funcall apply)))
    (let ((fn-arg (cadr form)))
      (not (or (quoted-symbol-p fn-arg)
               (function-ref-p fn-arg))))))

;;; ==========================================================================
;;; T010-T011, T021, T025, T031-T033: FFI Usage Walker
;;; ==========================================================================

(defun analyze-ffi-usage (form)
  "Analyze FORM to detect FFI function usage and dynamic call patterns.
FORM is a macro-expanded Lisp S-expression.

Returns an FFI-ANALYSIS struct containing:
  - USED-FFIS: List of FFI function names statically referenced
  - HAS-DYNAMIC-CALL-P: T if any dynamic funcall/apply detected
  - STATIC-FUNCALLS: Symbols from quoted funcall patterns
  - DYNAMIC-SITES: Forms containing dynamic calls (for error messages)"
  (let ((used-ffis nil)
        (has-dynamic-call-p nil)
        (static-funcalls nil)
        (dynamic-sites nil))
    (labels ((walk (form)
               (cond
                 ;; Atoms: nothing to analyze
                 ((atom form)
                  nil)

                 ;; Quoted forms: don't walk inside
                 ((eq (car form) 'quote)
                  nil)

                 ;; Function references: check if FFI
                 ((eq (car form) 'function)
                  (let ((fn-name (cadr form)))
                    (when (and (symbolp fn-name)
                               (ffi-function-p fn-name))
                      (pushnew fn-name used-ffis))))

                 ;; funcall/apply: check static vs dynamic
                 ((member (car form) '(funcall apply))
                  (let ((static-fn (detect-static-funcall-p form)))
                    (cond
                      ;; Static funcall with FFI function
                      ((and static-fn (ffi-function-p static-fn))
                       (pushnew static-fn used-ffis)
                       (pushnew static-fn static-funcalls))
                      ;; Static funcall with non-FFI function
                      (static-fn
                       (pushnew static-fn static-funcalls))
                      ;; Dynamic call
                      (t
                       (setf has-dynamic-call-p t)
                       (push form dynamic-sites))))
                  ;; Also walk arguments
                  (mapc #'walk (cddr form)))

                 ;; let/let*: walk bindings and body
                 ((member (car form) '(let let*))
                  (let ((bindings (cadr form))
                        (body (cddr form)))
                    (dolist (binding bindings)
                      (when (consp binding)
                        (walk (cadr binding))))
                    (mapc #'walk body)))

                 ;; progn/block/tagbody: walk body
                 ((member (car form) '(progn block tagbody))
                  (mapc #'walk (cdr form)))

                 ;; if: walk all branches
                 ((eq (car form) 'if)
                  (mapc #'walk (cdr form)))

                 ;; lambda: walk body
                 ((eq (car form) 'lambda)
                  (mapc #'walk (cddr form)))

                 ;; labels/flet: walk local function bodies and body
                 ((member (car form) '(labels flet))
                  (let ((defs (cadr form))
                        (body (cddr form)))
                    (dolist (def defs)
                      (mapc #'walk (cddr def)))
                    (mapc #'walk body)))

                 ;; Direct function call: check if FFI
                 ((symbolp (car form))
                  (when (ffi-function-p (car form))
                    (pushnew (car form) used-ffis))
                  ;; Walk arguments
                  (mapc #'walk (cdr form)))

                 ;; Other compound forms: walk recursively
                 (t
                  (mapc #'walk form)))))
      (walk form)
      (make-ffi-analysis
       :used-ffis (nreverse used-ffis)
       :has-dynamic-call-p has-dynamic-call-p
       :static-funcalls (nreverse static-funcalls)
       :dynamic-sites (nreverse dynamic-sites)))))
