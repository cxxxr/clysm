;;;; compiler.lisp - Main compilation driver

(in-package #:clysm/compiler)

;;; Macro Expansion (using host Lisp)
;;; We use SBCL's macroexpand to expand macros before compilation.
;;; This allows us to use defmacro in source code and have it expanded
;;; on the host before generating WASM.

(defun normalize-sbcl-internals (form)
  "Replace SBCL internal functions with standard equivalents."
  (cond
    ((atom form) form)
    ;; Arithmetic
    ((eq (car form) 'sb-impl::xsubtract)
     ;; (sb-impl::xsubtract a b) => (- b a)
     `(- ,(normalize-sbcl-internals (third form))
         ,(normalize-sbcl-internals (second form))))
    ;; Lambda
    ((eq (car form) 'sb-int:named-lambda)
     ;; (sb-int:named-lambda name (...) body) => (lambda (...) body)
     `(lambda ,(third form) ,@(mapcar #'normalize-sbcl-internals (cdddr form))))
    ;; Loop internals
    ((eq (car form) 'sb-loop::loop-desetq)
     ;; (sb-loop::loop-desetq var value) => (setq var value)
     `(setq ,(second form) ,(normalize-sbcl-internals (third form))))
    ((eq (car form) 'sb-loop::loop-collect-rplacd)
     ;; (sb-loop::loop-collect-rplacd (head tail) list-form)
     ;; This adds list-form to the tail of the collection list
     ;; Simplified: (progn (rplacd tail list-form) (setq tail (last tail)))
     ;; For now, we'll just evaluate list-form and let it be handled later
     (let ((vars (second form))
            (list-form (normalize-sbcl-internals (third form))))
       (let ((head (first vars))
             (tail (second vars)))
         `(if (null ,head)
              (progn
                (setq ,head ,list-form)
                (setq ,tail (last ,head)))
              (progn
                (rplacd ,tail ,list-form)
                (setq ,tail (last ,tail)))))))
    ((eq (car form) 'sb-loop::loop-collect-answer)
     ;; (sb-loop::loop-collect-answer head) => head
     (normalize-sbcl-internals (second form)))
    ((eq (car form) 'endp)
     ;; endp is same as null/atom for proper lists
     `(null ,(normalize-sbcl-internals (second form))))
    (t
     (cons (normalize-sbcl-internals (car form))
           (mapcar #'normalize-sbcl-internals (cdr form))))))

(defun expand-macros (form)
  "Recursively expand all macros in FORM using the host Lisp's macroexpand.
   This is similar to macroexpand-all but handles our special forms."
  (cond
    ;; Atoms don't need expansion
    ((atom form) form)
    ;; Quote - don't expand inside
    ((eq (car form) 'quote) form)
    ;; Special forms we handle - expand their subforms
    ((member (car form) '(if let let* progn setq when unless cond and or
                          block return-from return dotimes dolist
                          tagbody go
                          lambda funcall defun defparameter defconstant
                          defvar defstruct))
     (cons (car form) (mapcar #'expand-macros (cdr form))))
    ;; Try to macroexpand the form
    (t
     (multiple-value-bind (expanded expandedp)
         (macroexpand-1 form)
       (if expandedp
           ;; Was expanded - recursively expand the result
           (expand-macros expanded)
           ;; Not a macro - expand subforms
           (cons (car form) (mapcar #'expand-macros (cdr form))))))))

(defun expand-toplevel-macros (forms)
  "Expand macros in a list of top-level forms."
  (mapcar (lambda (form)
            (normalize-sbcl-internals (expand-macros form)))
          forms))

;;; Form Compilation

(defun compile-form (form env)
  "Compile a Lisp form to WASM instructions."
  (cond
    ;; Self-evaluating
    ((null form)
     `((,+op-i32-const+ 0)))
    ((eq form t)
     `((,+op-i32-const+ 1)))
    ((integerp form)
     `((,+op-i32-const+ ,form)))
    ((floatp form)
     `((,+op-f64-const+ ,(float form 1.0d0))))
    ;; Variable reference
    ((symbolp form)
     (let ((local-info (env-lookup env form :variable)))
       (if local-info
           `((,+op-local-get+ ,(local-info-index local-info)))
           ;; Check for global variable
           (let ((global-info (env-lookup env form :global)))
             (if global-info
                 `((,+op-global-get+ ,(global-info-index global-info)))
                 (error "Undefined variable: ~A" form))))))
    ;; Compound form
    ((consp form)
     (let ((op (car form)))
       (cond
         ;; Special form
         ((special-form-p op)
          (compile-special-form form env))
         ;; Primitive
         ((primitive-p op)
          (compile-primitive op (cdr form) env))
         ;; User-defined function call
         ((and (symbolp op) (env-lookup env op :function))
          (compile-call form env))
         ;; Unknown function
         (t
          (error "Unknown function: ~A" op)))))
    (t
     (error "Cannot compile: ~A" form))))

;;; Function Calls

(defun compile-call (form env)
  "Compile a call to a user-defined function."
  (let* ((name (car form))
         (args (cdr form))
         (func-info (env-lookup env name :function))
         (func-idx (func-info-index func-info)))
    ;; Compile all arguments
    (let ((arg-code nil))
      (dolist (arg args)
        (setf arg-code (append arg-code (compile-form arg env))))
      ;; Emit call instruction
      (append arg-code `((,+op-call+ ,func-idx))))))

;;; Top-level Compilation

(defun compile-toplevel (form env)
  "Compile a top-level form."
  (cond
    ((and (consp form) (eq (car form) 'defun))
     (compile-defun form env))
    (t
     ;; Wrap as main function
     (compile-main form env))))

(defun find-max-local-index (code)
  "Find the maximum local index used in the generated code."
  (let ((max-idx -1))
    (labels ((scan (items)
               (dolist (item items)
                 (cond
                   ((and (listp item)
                         (or (eql (car item) +op-local-get+)
                             (eql (car item) +op-local-set+)
                             (eql (car item) +op-local-tee+)))
                    (setf max-idx (max max-idx (cadr item))))
                   ((listp item)
                    (scan item))))))
      (scan code))
    max-idx))

(defun compile-defun (form env)
  "Compile a function definition."
  (destructuring-bind (name params &rest body) (cdr form)
    (let* ((module (compile-env-module env))
           ;; Add function type
           (param-types (make-list (length params) :initial-element +type-i32+))
           (result-types (list +type-i32+)))
      (multiple-value-bind (new-env func-idx type-idx)
          (env-add-function env name param-types result-types)
        (declare (ignore func-idx))  ; We'll get actual index from add-function
        ;; Create local environment for function body
        (let ((body-env new-env))
          ;; Add parameters as locals
          (loop for param in params
                do (setf body-env (env-add-local body-env param +type-i32+)))
          ;; Compile body (may add lambda functions to module first!)
          (let* ((body-code (compile-progn body body-env))
                 ;; Find max local index used in the code
                 (max-local-idx (find-max-local-index body-code))
                 ;; Locals = indices from (length params) to max-local-idx
                 (num-extra-locals (max 0 (- max-local-idx (1- (length params)))))
                 (locals (if (> num-extra-locals 0)
                             (list (cons num-extra-locals +type-i32+))
                             nil))
                 ;; Add function to module and get ACTUAL function index
                 (actual-func-idx (add-function module type-idx locals body-code)))
            ;; Export function with actual index (not the pre-calculated one)
            (add-export module (string-downcase (symbol-name name))
                        +export-func+ actual-func-idx)
            new-env))))))

(defun compile-main (form env)
  "Compile a form as the main function."
  (let* ((module (compile-env-module env))
         (type-idx (add-func-type module nil (list +type-i32+)))
         (body-code (compile-form form env))
         (func-idx (add-function module type-idx nil body-code)))
    (add-export module "main" +export-func+ func-idx)
    env))

;;; Module Compilation

(defun compile-module (forms &key (enable-memory t))
  "Compile a list of top-level forms to a WASM module.
   Uses two-pass compilation to give defuns stable indices."
  (let* ((module (make-wasm-module))
         (env (make-initial-env module))
         (defuns nil)
         (lambda-holder (list nil)))  ; Box to collect lambda functions
    ;; Reset symbol table, closure type cache, and structure registry
    (reset-symbol-table)
    (reset-closure-type-cache)
    (reset-struct-registry)
    ;; Expand macros using host Lisp before compilation
    (setf forms (expand-toplevel-macros forms))
    ;; Add memory and heap pointer if enabled
    (when enable-memory
      (setup-runtime module))
    ;; First pass: collect all defuns and register globals (defparameter/defconstant)
    (dolist (form forms)
      (cond
        ;; Register defuns
        ((and (consp form) (eq (car form) 'defun))
         (push form defuns)
         (destructuring-bind (name params &rest body) (cdr form)
           (declare (ignore body))
           (let ((param-types (make-list (length params) :initial-element +type-i32+))
                 (result-types (list +type-i32+)))
             (setf env (env-add-function env name param-types result-types)))))
        ;; Register global constants
        ((and (consp form) (eq (car form) 'defconstant))
         (destructuring-bind (name value &optional doc) (cdr form)
           (declare (ignore doc))
           (when (integerp value)
             (setf env (env-add-global env name +type-i32+ nil value :constant-p t)))))
        ;; Register global parameters
        ((and (consp form) (eq (car form) 'defparameter))
         (destructuring-bind (name value &optional doc) (cdr form)
           (declare (ignore doc))
           (when (integerp value)
             (setf env (env-add-global env name +type-i32+ t value :constant-p nil)))))
        ;; Register defvar
        ((and (consp form) (eq (car form) 'defvar))
         (destructuring-bind (name &optional (value 0) doc) (cdr form)
           (declare (ignore doc))
           (unless (env-lookup env name :global)
             (when (integerp value)
               (setf env (env-add-global env name +type-i32+ t value :constant-p nil))))))
        ;; Register defstruct
        ((and (consp form) (eq (car form) 'defstruct))
         (register-defstruct-type (cdr form)))))
    (setf defuns (nreverse defuns))
    ;; Store lambda-holder in a dynamic variable for lambda compilation to use
    (let ((*pending-lambdas* lambda-holder))
      ;; Second pass: compile defun bodies (lambdas go to pending list)
      (dolist (form defuns)
        (compile-defun-body form env lambda-holder module))
      ;; Compile any non-defun, non-global, non-defstruct forms as main
      (dolist (form forms)
        (unless (and (consp form)
                     (member (car form) '(defun defconstant defparameter defvar defstruct)))
          (setf env (compile-toplevel form env))))
      ;; Add pending lambda functions to module
      (dolist (lambda-entry (car lambda-holder))
        (destructuring-bind (type-idx locals body) lambda-entry
          (add-function module type-idx locals body))))
    ;; Add element section with all functions for indirect calls
    (setup-element-section module)
    ;; Add data section for symbols and strings
    (setup-static-data module)
    (finalize-module module)
    module))


(defun compile-defun-body (form env lambda-holder module)
  "Compile a defun body after registration."
  (destructuring-bind (name params &rest body) (cdr form)
    (let ((func-info (env-lookup env name :function)))
      (unless func-info
        (error "Function ~A not registered" name))
      (let ((type-idx (func-info-type-index func-info))
            (func-idx (func-info-index func-info)))
        ;; Create local environment for function body
        (let ((body-env env))
          ;; Add parameters as locals
          (loop for param in params
                do (setf body-env (env-add-local body-env param +type-i32+)))
          ;; Compile body
          (let* ((body-code (compile-progn body body-env))
                 (max-local-idx (find-max-local-index body-code))
                 (num-extra-locals (max 0 (- max-local-idx (1- (length params)))))
                 (locals (if (> num-extra-locals 0)
                             (list (cons num-extra-locals +type-i32+))
                             nil)))
            ;; Add function to module
            (add-function module type-idx locals body-code)
            ;; Export function
            (add-export module (string-downcase (symbol-name name))
                        +export-func+ func-idx)))))))

(defun setup-element-section (module)
  "Populate the function table with all functions for indirect calls."
  (let ((func-count (+ (wasm-module-import-func-count module)
                       (wasm-module-func-count module))))
    (when (plusp func-count)
      ;; Create element segment with all functions starting at index 0
      (add-element module
                   0  ; table index
                   `((,+op-i32-const+ 0))  ; offset expression
                   (loop for i from 0 below func-count collect i)))))

;;; Runtime Setup

(defparameter *heap-pointer-global* 0
  "Index of the heap pointer global variable.")

(defparameter *cons-size* 8
  "Size of a cons cell in bytes (car + cdr).")

(defun setup-runtime (module)
  "Set up runtime support: memory, heap pointer, and function table."
  ;; Add memory (1 page = 64KB, can grow)
  (add-memory module 1 16)
  ;; Add heap pointer global (starts at 1024 to leave room for constants)
  (setf *heap-pointer-global*
        (length (wasm-module-globals module)))
  (add-global module +type-i32+ t `((,+op-i32-const+ 1024)))
  ;; Add function table for indirect calls (closures)
  ;; Initial size 256, can grow to 4096
  (add-table module +type-funcref+ 256 4096))

;;; Closure Type Index Cache

(defparameter *closure-type-indices* (make-hash-table)
  "Cache of type indices for closure types by arity.
   Key is arity (number of declared params, not including closure-env).
   Value is type index.")

(defun get-closure-type-index (module arity)
  "Get or create type index for a closure with ARITY parameters.
   All closures have signature (closure-env, arg1, ..., argN) -> i32."
  (or (gethash arity *closure-type-indices*)
      (let* ((param-count (1+ arity))  ; +1 for closure-env
             (params (make-list param-count :initial-element +type-i32+))
             (results (list +type-i32+))
             (type-idx (add-func-type module params results)))
        (setf (gethash arity *closure-type-indices*) type-idx)
        type-idx)))

(defun reset-closure-type-cache ()
  "Reset the closure type cache (called at start of compile-module)."
  (clrhash *closure-type-indices*))

;;; Static Data Setup

(defparameter *static-data-base* 256
  "Base address for static data in linear memory.")

;;; Structure Support

(defun parse-defstruct-options (name-and-options)
  "Parse defstruct name and options. Returns (name . options-alist)."
  (if (consp name-and-options)
      (let ((name (car name-and-options))
            (options (cdr name-and-options)))
        (cons name
              (loop for opt in options
                    collect (if (consp opt)
                                (cons (car opt) (cdr opt))
                                (cons opt nil)))))
      (cons name-and-options nil)))

(defun parse-defstruct-slots (slot-descriptions)
  "Parse defstruct slot descriptions. Returns list of (name default)."
  (loop for slot in slot-descriptions
        collect (if (consp slot)
                    (list (car slot) (or (cadr slot) 0))
                    (list slot 0))))

(defun register-defstruct-type (form)
  "Parse and register a defstruct definition."
  (destructuring-bind (name-and-options &rest slot-descriptions) form
    (let* ((parsed (parse-defstruct-options name-and-options))
           (name (car parsed))
           (options (cdr parsed))
           (include-opt (assoc :include options))
           (parent (when include-opt (cadr include-opt)))
           (slots (parse-defstruct-slots slot-descriptions)))
      (let ((struct-info (register-struct name slots :parent parent)))
        ;; Register accessor primitives for each slot
        (let ((all-slots (struct-all-slots struct-info)))
          (loop for (slot-name default) in all-slots
                for offset from 4 by 4
                do (register-struct-accessor name slot-name offset)))
        ;; Register constructor primitive
        (register-struct-constructor struct-info)
        ;; Register predicate primitive
        (register-struct-predicate struct-info)
        struct-info))))

(defun register-struct-accessor (struct-name slot-name offset)
  "Register a structure accessor as a primitive."
  (let ((accessor-name (intern (format nil "~A-~A" struct-name slot-name))))
    (setf (gethash accessor-name *primitives*)
          (lambda (args env)
            (declare (ignorable env))
            (unless (= (length args) 1)
              (error "~A requires exactly 1 argument" accessor-name))
            `(,@(compile-form (first args) env)
              (,+op-i32-load+ 2 ,offset))))))

(defun register-struct-constructor (struct-info)
  "Register a structure constructor as a primitive."
  (let* ((name (struct-info-name struct-info))
         (constructor (struct-info-constructor struct-info))
         (all-slots (struct-all-slots struct-info))
         (num-slots (length all-slots))
         (struct-size (+ 4 (* 4 num-slots)))  ; type-id + slots
         (type-id (struct-info-type-id struct-info)))
    (setf (gethash constructor *primitives*)
          (lambda (args env)
            (declare (ignorable env))
            ;; For now, only support positional arguments matching slot order
            (when (> (length args) num-slots)
              (error "~A: too many arguments" constructor))
            `(;; Reserve space
              (,+op-global-get+ ,*heap-pointer-global*)
              (,+op-i32-const+ ,struct-size)
              ,+op-i32-add+
              (,+op-global-set+ ,*heap-pointer-global*)
              ;; Push struct address as return value
              (,+op-global-get+ ,*heap-pointer-global*)
              (,+op-i32-const+ ,struct-size)
              ,+op-i32-sub+
              ;; Store type-id at offset 0
              (,+op-global-get+ ,*heap-pointer-global*)
              (,+op-i32-const+ ,struct-size)
              ,+op-i32-sub+
              (,+op-i32-const+ ,type-id)
              (,+op-i32-store+ 2 0)
              ;; Store each slot
              ,@(loop for (slot-name default) in all-slots
                      for i from 0
                      for offset from 4 by 4
                      for arg = (nth i args)
                      append `((,+op-global-get+ ,*heap-pointer-global*)
                               (,+op-i32-const+ ,struct-size)
                               ,+op-i32-sub+
                               ,@(if arg
                                     (compile-form arg env)
                                     `((,+op-i32-const+ ,default)))
                               (,+op-i32-store+ 2 ,offset)))
              ;; Struct address already on stack
              )))))

(defun register-struct-predicate (struct-info)
  "Register a structure predicate as a primitive."
  (let ((predicate (struct-info-predicate struct-info))
        (type-id (struct-info-type-id struct-info)))
    (setf (gethash predicate *primitives*)
          (lambda (args env)
            (declare (ignorable env))
            (unless (= (length args) 1)
              (error "~A requires exactly 1 argument" predicate))
            ;; Check if type-id at offset 0 matches
            `(,@(compile-form (first args) env)
              (,+op-i32-load+ 2 0)  ; load type-id
              (,+op-i32-const+ ,type-id)
              ,+op-i32-eq+)))))

(defun setup-static-data (module)
  "Add data segments for interned symbols and strings."
  (let ((data-buffer (make-array 1024
                                 :element-type '(unsigned-byte 8)
                                 :adjustable t
                                 :fill-pointer 0)))
    ;; Collect all string entries sorted by address
    (let ((strings nil))
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (push value strings))
               *string-table*)
      (setf strings (sort strings #'< :key #'first))
      ;; Write strings to data buffer
      (dolist (entry strings)
        (destructuring-bind (addr len bytes) entry
          (let ((relative-addr (- addr *static-data-base*)))
            ;; Pad to the correct offset
            (loop while (< (length data-buffer) relative-addr)
                  do (vector-push-extend 0 data-buffer))
            ;; Write length (little-endian i32)
            (vector-push-extend (ldb (byte 8 0) len) data-buffer)
            (vector-push-extend (ldb (byte 8 8) len) data-buffer)
            (vector-push-extend (ldb (byte 8 16) len) data-buffer)
            (vector-push-extend (ldb (byte 8 24) len) data-buffer)
            ;; Write UTF-8 bytes
            (loop for byte across bytes
                  do (vector-push-extend byte data-buffer))))))
    ;; Collect all symbol entries sorted by address
    (let ((symbols nil))
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (push value symbols))
               *symbol-table*)
      (setf symbols (sort symbols #'< :key #'first))
      ;; Write symbols to data buffer
      (dolist (entry symbols)
        (destructuring-bind (sym-addr string-addr sym) entry
          (declare (ignore sym))
          (let ((relative-addr (- sym-addr *static-data-base*)))
            ;; Pad to the correct offset
            (loop while (< (length data-buffer) relative-addr)
                  do (vector-push-extend 0 data-buffer))
            ;; Write name-ptr (little-endian i32)
            (vector-push-extend (ldb (byte 8 0) string-addr) data-buffer)
            (vector-push-extend (ldb (byte 8 8) string-addr) data-buffer)
            (vector-push-extend (ldb (byte 8 16) string-addr) data-buffer)
            (vector-push-extend (ldb (byte 8 24) string-addr) data-buffer)
            ;; Write value (0 = unbound, little-endian i32)
            (dotimes (i 4) (vector-push-extend 0 data-buffer))
            ;; Write function (0 = unbound, little-endian i32)
            (dotimes (i 4) (vector-push-extend 0 data-buffer))
            ;; Write plist (0 = nil, little-endian i32)
            (dotimes (i 4) (vector-push-extend 0 data-buffer))))))
    ;; Add data segment if there's any data
    (when (plusp (length data-buffer))
      (add-data module
                0  ; memory index
                `((,+op-i32-const+ ,*static-data-base*))
                (coerce data-buffer '(vector (unsigned-byte 8)))))))
