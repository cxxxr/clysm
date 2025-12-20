;;;; feature-map.lisp - Map cl-wasm implementation status

(in-package #:cl-wasm/ansi-tests)

;;; Feature implementation status for cl-wasm
;;; Used to determine which ANSI tests can be run

;; Implemented special forms
(defparameter *implemented-special-forms*
  '(if let let* progn setq quote
    when unless cond and or
    block return-from return
    dotimes dolist
    lambda funcall)
  "Special forms implemented in cl-wasm.")

;; Implemented primitive functions
(defparameter *implemented-primitives*
  '(;; Arithmetic
    + - * / mod rem 1+ 1-
    abs max min gcd lcm
    floor ceiling truncate round
    ;; Comparison (multi-argument supported)
    < > <= >= = /=
    ;; Boolean/predicates
    not null zerop plusp minusp
    evenp oddp
    numberp integerp
    eq eql
    ;; Bitwise
    logand logior logxor ash
    ;; List operations
    cons car cdr
    rplaca rplacd
    consp atom
    list list*
    first rest second third fourth
    nth nthcdr
    length)
  "Primitive functions implemented in cl-wasm.")

;; Features that are NOT implemented yet
(defparameter *unimplemented-features*
  '(;; Data types
    :symbol :string :character :array :hash-table :struct
    :float :complex :ratio :bignum
    ;; Operations
    :multiple-values :apply :reduce :mapcar :mapc :maplist
    :format :print :read :write
    :tagbody :go :catch :throw :unwind-protect
    ;; System
    :package :clos :condition :stream :file :pathname
    :eval :compile :load
    ;; Misc
    :loop :type-declarations :documentation)
  "Features not yet implemented in cl-wasm.")

;; Forms that require specific unimplemented features
(defparameter *form-requirements*
  '((loop . :loop)
    (format . :format)
    (print . :print)
    (write . :write)
    (read . :read)
    (make-string . :string)
    (string . :string)
    (char . :character)
    (make-array . :array)
    (aref . :array)
    (make-hash-table . :hash-table)
    (gethash . :hash-table)
    (intern . :symbol)
    (symbol-name . :symbol)
    (symbol-value . :symbol)
    (make-instance . :clos)
    (defclass . :clos)
    (defmethod . :clos)
    (defgeneric . :clos)
    (signal . :condition)
    (error . :condition)
    (handler-case . :condition)
    (handler-bind . :condition)
    (restart-case . :condition)
    (open . :file)
    (with-open-file . :file)
    (pathname . :pathname)
    (values . :multiple-values)
    (multiple-value-bind . :multiple-values)
    (multiple-value-list . :multiple-values)
    (apply . :apply)
    (mapcar . :mapcar)
    (mapc . :mapcar)
    (reduce . :reduce)
    (random . :random)
    (float . :float)
    (complex . :complex)
    (ratio . :ratio)
    (numerator . :ratio)
    (denominator . :ratio)
    (tagbody . :tagbody)
    (go . :tagbody)
    (catch . :catch)
    (throw . :catch)
    (unwind-protect . :unwind-protect)
    (eval . :eval)
    (compile . :compile)
    (load . :load))
  "Mapping from forms to required features.")

;;; Feature checking functions

(defun implemented-special-form-p (symbol)
  "Check if SYMBOL is an implemented special form."
  (member symbol *implemented-special-forms*))

(defun implemented-primitive-p (symbol)
  "Check if SYMBOL is an implemented primitive."
  (member symbol *implemented-primitives*))

(defun implemented-function-p (symbol)
  "Check if SYMBOL names an implemented function (special form or primitive)."
  (or (implemented-special-form-p symbol)
      (implemented-primitive-p symbol)))

(defun get-required-feature (symbol)
  "Get the feature required by SYMBOL, or NIL if none."
  (cdr (assoc symbol *form-requirements*)))

(defun feature-implemented-p (feature)
  "Check if FEATURE is implemented."
  (not (member feature *unimplemented-features*)))

;;; Form analysis

(defun extract-symbols (form)
  "Extract all symbols (in function position) from FORM."
  (let ((symbols nil))
    (labels ((walk (f)
               (cond
                 ((null f) nil)
                 ((symbolp f) nil)  ; don't collect bare symbols
                 ((atom f) nil)
                 ((consp f)
                  (when (symbolp (car f))
                    (pushnew (car f) symbols))
                  (walk (car f))
                  (walk (cdr f))))))
      (walk form))
    symbols))

(defun analyze-form-requirements (form)
  "Analyze FORM and return list of unimplemented features it requires."
  (let ((symbols (extract-symbols form))
        (missing-features nil))
    (dolist (sym symbols)
      (let ((required (get-required-feature sym)))
        (when (and required (not (feature-implemented-p required)))
          (pushnew required missing-features)))
      ;; Also check if it's an unknown function
      (unless (or (implemented-function-p sym)
                  (get-required-feature sym)
                  (member sym '(defun deftest)))  ; known meta-forms
        ;; Unknown function, might require implementation
        nil))
    missing-features))

(defun form-can-be-compiled-p (form)
  "Check if FORM can potentially be compiled by cl-wasm."
  (null (analyze-form-requirements form)))

;;; Test filtering

(defun simple-arithmetic-test-p (form expected)
  "Check if this is a simple arithmetic test (no complex features)."
  (and (form-can-be-compiled-p form)
       (or (integerp expected)
           (and (listp expected)
                (= (length expected) 1)
                (integerp (first expected))))))

(defun simple-boolean-test-p (form expected)
  "Check if this is a simple boolean test."
  (and (form-can-be-compiled-p form)
       (or (member expected '(t nil))
           (and (listp expected)
                (= (length expected) 1)
                (member (first expected) '(t nil))))))

(defun runnable-test-p (form expected)
  "Determine if a test can be run on cl-wasm."
  (or (simple-arithmetic-test-p form expected)
      (simple-boolean-test-p form expected)))

;;; Category support status

(defparameter *category-support*
  '((:numbers . :partial)      ; fixnum arithmetic works
    (:cons . :partial)         ; basic cons/car/cdr work
    (:data-and-control-flow . :partial)  ; if, let, cond, etc.
    (:iteration . :partial)    ; dotimes, dolist
    (:eval-and-compile . :minimal)  ; defun, lambda
    (:types-and-classes . :none)
    (:conditions . :none)
    (:symbols . :none)
    (:packages . :none)
    (:arrays . :none)
    (:strings . :none)
    (:sequences . :minimal)    ; list-based only
    (:hash-tables . :none)
    (:filenames . :none)
    (:files . :none)
    (:streams . :none)
    (:printer . :none)
    (:reader . :none)
    (:system-construction . :none)
    (:environment . :none))
  "Support level for each category: :full, :partial, :minimal, :none")

(defun category-support-level (category)
  "Get the support level for a category."
  (cdr (assoc category *category-support*)))

(defun category-worth-running-p (category)
  "Check if it's worth running tests for this category."
  (member (category-support-level category) '(:full :partial :minimal)))
