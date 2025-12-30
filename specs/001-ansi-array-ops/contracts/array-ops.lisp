;;;; Array Operations Contracts
;;;; Phase 15C - ANSI Array Operations Enhancement
;;;;
;;;; This file defines the function signatures and contracts for the 9 array
;;;; operation compilers to be implemented in func-section.lisp.
;;;;
;;;; References:
;;;;   array-rank: resources/HyperSpec/Body/f_ar_ran.htm
;;;;   array-dimension: resources/HyperSpec/Body/f_ar_dim.htm
;;;;   array-dimensions: resources/HyperSpec/Body/f_ar_di1.htm
;;;;   array-total-size: resources/HyperSpec/Body/f_ar_tot.htm
;;;;   array-row-major-index: resources/HyperSpec/Body/f_ar_row.htm
;;;;   row-major-aref: resources/HyperSpec/Body/f_row_ma.htm
;;;;   adjustable-array-p: resources/HyperSpec/Body/f_adjust.htm
;;;;   adjust-array: resources/HyperSpec/Body/f_adj_ar.htm

(in-package :clysm)

;;; ============================================================================
;;; Metadata Query Functions
;;; ============================================================================

(defun compile-array-rank (args env)
  "Compile (array-rank array) - return number of dimensions.

   Args:
     args - (array) where array is any array expression
     env  - compilation environment

   Stack effect: [] -> [i31ref]

   Behavior:
     - For $mdarray: return (length $dimensions)
     - For $mv_array (simple-vector): return 1
     - For non-array: signal type-error

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

(defun compile-array-dimension (args env)
  "Compile (array-dimension array axis-number) - return size of specific axis.

   Args:
     args - (array axis-number) where axis-number is 0-indexed
     env  - compilation environment

   Stack effect: [] -> [i31ref]

   Behavior:
     - For $mdarray: return (nth axis-number $dimensions)
     - For $mv_array: if axis=0, return (array.len); else error
     - If axis-number >= rank: signal type-error

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

(defun compile-array-dimensions (args env)
  "Compile (array-dimensions array) - return list of all dimension sizes.

   Args:
     args - (array)
     env  - compilation environment

   Stack effect: [] -> [anyref] (cons list)

   Behavior:
     - For $mdarray: return $dimensions field directly
     - For $mv_array: return (list (array.len array))

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

(defun compile-array-total-size (args env)
  "Compile (array-total-size array) - return total number of elements.

   Args:
     args - (array)
     env  - compilation environment

   Stack effect: [] -> [i31ref]

   Behavior:
     - For $mdarray: return (array.len $storage)
     - For $mv_array: return (array.len array)

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

;;; ============================================================================
;;; Row-Major Access Functions
;;; ============================================================================

(defun compile-array-row-major-index (args env)
  "Compile (array-row-major-index array &rest subscripts) - compute linear index.

   Args:
     args - (array subscript*) variable arity
     env  - compilation environment

   Stack effect: [] -> [i31ref]

   Behavior:
     Compute: s₀×(d₁×d₂×...×d_{n-1}) + s₁×(d₂×...×d_{n-1}) + ... + s_{n-1}
     where sᵢ are subscripts and dᵢ are dimensions.

   Validation:
     - Number of subscripts must equal rank
     - Each subscript must be in range [0, dimension)

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

(defun compile-row-major-aref (args env)
  "Compile (row-major-aref array index) - access element by row-major index.

   Args:
     args - (array index) where index is a non-negative integer
     env  - compilation environment

   Stack effect: [] -> [anyref]

   Behavior:
     - For $mdarray: (array.get $storage index)
     - For $mv_array: (array.get array index)
     - Bounds check: 0 <= index < total-size

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

(defun compile-setf-row-major-aref (args env)
  "Compile (%setf-row-major-aref array value index) - set element by row-major index.

   Args:
     args - (array value index)
     env  - compilation environment

   Stack effect: [] -> [anyref] (returns value for setf semantics)

   Behavior:
     - For $mdarray: (array.set $storage index value)
     - For $mv_array: (array.set array index value)
     - Return value after setting

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

;;; ============================================================================
;;; Adjustability Functions
;;; ============================================================================

(defun compile-adjustable-array-p (args env)
  "Compile (adjustable-array-p array) - check if array can be resized.

   Args:
     args - (array)
     env  - compilation environment

   Stack effect: [] -> [anyref] (T or NIL)

   Behavior:
     - For $mdarray: return T if $adjustable = 1, else NIL
     - For $mv_array: always return NIL

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

(defun compile-adjust-array (args env)
  "Compile (adjust-array array new-dimensions &key initial-element) - resize array.

   Args:
     args - (array new-dimensions &key initial-element)
     env  - compilation environment

   Stack effect: [] -> [anyref] (new array)

   Behavior:
     1. Verify array is adjustable (signal error if not)
     2. Compute new total size from new-dimensions
     3. Allocate new storage array with new total size
     4. Copy existing elements in row-major order
     5. Fill remaining positions with initial-element (default NIL)
     6. Create new $mdarray struct with new dimensions and storage
     7. Return new array

   Limitation:
     - :displaced-to and :fill-pointer not supported in this phase

   Returns: List of Wasm instructions"
  (declare (ignore args env))
  (error "Not yet implemented"))

;;; ============================================================================
;;; Setf Expander Registration
;;; ============================================================================

;; To be added to src/clysm/lib/setf-expanders.lisp:
;;
;; (defun make-row-major-aref-setf-expander ()
;;   "Expander for (setf (row-major-aref array index) value)"
;;   (lambda (access-form env)
;;     (declare (ignore env))
;;     (let* ((array-expr (second access-form))
;;            (index-expr (third access-form))
;;            (array-temp (gensym "ARRAY"))
;;            (index-temp (gensym "INDEX"))
;;            (value-temp (gensym "VALUE")))
;;       (values
;;        (list array-temp index-temp)           ; temps
;;        (list array-expr index-expr)           ; values
;;        (list value-temp)                      ; stores
;;        `(%setf-row-major-aref ,array-temp ,value-temp ,index-temp) ; store-form
;;        `(row-major-aref ,array-temp ,index-temp)))))               ; access-form

;;; ============================================================================
;;; Function Registration
;;; ============================================================================

;; To be added to *builtin-compilers* in func-section.lisp:
;;
;; (array-rank . compile-array-rank)
;; (array-dimension . compile-array-dimension)
;; (array-dimensions . compile-array-dimensions)
;; (array-total-size . compile-array-total-size)
;; (array-row-major-index . compile-array-row-major-index)
;; (row-major-aref . compile-row-major-aref)
;; (%setf-row-major-aref . compile-setf-row-major-aref)
;; (adjustable-array-p . compile-adjustable-array-p)
;; (adjust-array . compile-adjust-array)
