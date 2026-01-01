;;;; loader.lisp - Runtime Library Source Loader and Dependency Analyzer
;;;; Feature 001-runtime-library-system: Layer 2 Runtime Library
;;;;
;;;; Provides functions to load runtime source files, analyze dependencies,
;;;; and compute compilation order via topological sort.

(in-package #:clysm/runtime-library)

;;; ============================================================================
;;; Conditions (T035)
;;; ============================================================================

(define-condition undefined-primitive-error (error)
  ((name :initarg :name
         :reader undefined-primitive-error-name
         :documentation "The name of the undefined primitive"))
  (:report (lambda (condition stream)
             (format stream "Undefined primitive: ~S~%~
                            This function is neither a registered primitive ~
                            nor a runtime library function."
                     (undefined-primitive-error-name condition))))
  (:documentation "Signaled when a function call references an undefined primitive."))

(define-condition circular-dependency-error (error)
  ((functions :initarg :functions
              :reader circular-dependency-error-functions
              :documentation "List of functions in the dependency cycle"))
  (:report (lambda (condition stream)
             (format stream "Circular dependency detected among functions: ~S"
                     (circular-dependency-error-functions condition))))
  (:documentation "Signaled when runtime functions have circular dependencies."))

;;; ============================================================================
;;; Runtime Source Loading (T031)
;;; ============================================================================

(defun load-runtime-source (pathname)
  "Load and parse a runtime library source file.

PATHNAME should be a pathname to a .lisp file containing DEFUN forms
for Layer 2 runtime functions.

Returns a RUNTIME-MODULE struct containing all parsed functions.

The source file should contain forms like:
  (defun assoc (item alist &key key test test-not)
    ...)

Each DEFUN is converted to a RUNTIME-FUNCTION struct with:
  - NAME: the function name
  - LAMBDA-LIST: the parameter list
  - BODY: the function body forms
  - SOURCE-FILE: the pathname
  - DEPENDENCIES: computed by ANALYZE-DEPENDENCIES"
  (let ((pathname (pathname pathname))
        (functions '())
        (exports '()))
    (with-open-file (stream pathname :direction :input)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (when (and (consp form)
                          (eq (car form) 'defun)
                          (symbolp (cadr form)))
                 (let ((name (cadr form))
                       (lambda-list (caddr form))
                       (body (cdddr form)))
                   (push (make-runtime-function
                          :name name
                          :lambda-list lambda-list
                          :body body
                          :source-file pathname)
                         functions)
                   (push name exports)))))
    ;; Analyze dependencies for all functions
    (let ((fn-table (make-hash-table :test 'eq)))
      ;; Build lookup table
      (dolist (fn functions)
        (setf (gethash (runtime-function-name fn) fn-table) fn))
      ;; Compute dependencies for each function
      (dolist (fn functions)
        (setf (runtime-function-dependencies fn)
              (analyze-dependencies fn fn-table))))
    ;; Create module
    (make-runtime-module
     :name (intern (string-upcase (pathname-name pathname)) :keyword)
     :source-file pathname
     :functions (nreverse functions)
     :exports (nreverse exports))))

;;; ============================================================================
;;; Dependency Analysis (T032)
;;; ============================================================================

(defun analyze-dependencies (runtime-fn fn-table)
  "Analyze a runtime function to find its dependencies.

RUNTIME-FN is a RUNTIME-FUNCTION struct.
FN-TABLE is a hash-table mapping function names to runtime-function structs.

Returns a list of function names that this function calls, excluding:
  1. Primitives (registered in the primitives registry)
  2. Self-recursive calls
  3. Lambda-bound variables

Dependencies are determined by walking the function body and collecting
all symbols in function call position that are either:
  - Other runtime library functions (in FN-TABLE)
  - Unknown functions (which will trigger undefined-primitive-error later)"
  (let ((deps '())
        (name (runtime-function-name runtime-fn))
        (body (runtime-function-body runtime-fn))
        (lambda-list (runtime-function-lambda-list runtime-fn)))
    ;; Get bound parameter names
    (let ((bound-vars (extract-lambda-vars lambda-list)))
      (labels ((walk (form env)
                 (cond
                   ;; Atoms: nothing to do
                   ((atom form) nil)
                   ;; Special forms
                   ((eq (car form) 'quote) nil)
                   ((eq (car form) 'function)
                    (when (and (symbolp (cadr form))
                               (not (member (cadr form) env))
                               (not (eq (cadr form) name))
                               (gethash (cadr form) fn-table))
                      (pushnew (cadr form) deps)))
                   ((member (car form) '(let let*))
                    (let ((new-env env))
                      (dolist (binding (cadr form))
                        (when (consp binding)
                          (walk (cadr binding) new-env))
                        (push (if (consp binding) (car binding) binding) new-env))
                      (dolist (body-form (cddr form))
                        (walk body-form new-env))))
                   ((eq (car form) 'lambda)
                    (let ((new-env (append (extract-lambda-vars (cadr form)) env)))
                      (dolist (body-form (cddr form))
                        (walk body-form new-env))))
                   ((member (car form) '(flet labels))
                    (let ((local-fns (mapcar #'car (cadr form)))
                          (new-env env))
                      ;; For labels, local functions can see each other
                      (when (eq (car form) 'labels)
                        (setf new-env (append local-fns env)))
                      ;; Walk function bodies
                      (dolist (fn-def (cadr form))
                        (let ((fn-env (if (eq (car form) 'labels)
                                          (append (extract-lambda-vars (cadr fn-def)) new-env)
                                          (append (extract-lambda-vars (cadr fn-def)) env))))
                          (dolist (body-form (cddr fn-def))
                            (walk body-form fn-env))))
                      ;; Walk body with local functions visible
                      (dolist (body-form (cddr form))
                        (walk body-form (append local-fns new-env)))))
                   ;; Function call
                   (t
                    (let ((fn-name (car form)))
                      ;; Check if it's a runtime function dependency
                      (when (and (symbolp fn-name)
                                 (not (member fn-name env))
                                 (not (eq fn-name name))
                                 (not (is-primitive-p fn-name))
                                 (gethash fn-name fn-table))
                        (pushnew fn-name deps))
                      ;; Walk arguments
                      (dolist (arg (cdr form))
                        (walk arg env)))))))
        (dolist (body-form body)
          (walk body-form bound-vars))))
    (nreverse deps)))

(defun extract-lambda-vars (lambda-list)
  "Extract variable names from a lambda list."
  (let ((vars '()))
    (dolist (item lambda-list)
      (cond
        ((member item '(&optional &rest &key &aux &allow-other-keys &whole &environment))
         nil)
        ((symbolp item)
         (push item vars))
        ((consp item)
         (push (car item) vars)
         ;; Handle (var init supplied-p) forms
         (when (and (cddr item) (symbolp (caddr item)))
           (push (caddr item) vars)))))
    (nreverse vars)))

(defun is-primitive-p (name)
  "Check if NAME is a registered primitive.
Delegates to the primitives registry if available."
  ;; Use the primitives registry from the codegen package
  (and (find-package :clysm)
       (let ((fn (find-symbol "REGISTERED-PRIMITIVE-P" :clysm)))
         (and fn (fboundp fn) (funcall fn name)))))

;;; ============================================================================
;;; Topological Sort (T033)
;;; ============================================================================

(defun topological-sort (functions)
  "Sort runtime functions in dependency order for compilation.

FUNCTIONS is a list of RUNTIME-FUNCTION structs.

Returns a list of functions ordered so that each function appears
after all its dependencies. This ensures functions are compiled
in the correct order.

Signals CIRCULAR-DEPENDENCY-ERROR if a cycle is detected."
  (let ((sorted '())
        (visiting (make-hash-table :test 'eq))
        (visited (make-hash-table :test 'eq))
        (fn-table (make-hash-table :test 'eq)))
    ;; Build lookup table
    (dolist (fn functions)
      (setf (gethash (runtime-function-name fn) fn-table) fn))
    ;; DFS-based topological sort
    (labels ((visit (fn)
               (let ((name (runtime-function-name fn)))
                 (cond
                   ((gethash name visited)
                    ;; Already processed
                    nil)
                   ((gethash name visiting)
                    ;; Cycle detected - collect cycle members
                    (let ((cycle (list name)))
                      (maphash (lambda (k v)
                                 (declare (ignore v))
                                 (when (gethash k visiting)
                                   (pushnew k cycle)))
                               visiting)
                      (error 'circular-dependency-error
                             :functions cycle)))
                   (t
                    ;; Mark as being visited
                    (setf (gethash name visiting) t)
                    ;; Visit dependencies first
                    (dolist (dep-name (runtime-function-dependencies fn))
                      (let ((dep-fn (gethash dep-name fn-table)))
                        (when dep-fn
                          (visit dep-fn))))
                    ;; Mark as visited and add to result
                    (setf (gethash name visiting) nil)
                    (setf (gethash name visited) t)
                    (push fn sorted))))))
      (dolist (fn functions)
        (visit fn)))
    (nreverse sorted)))
