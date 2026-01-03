;;;; contracts/functions.lisp - Function contracts for sequence-array-runtime
;;;; Feature: 001-sequence-array-runtime
;;;;
;;;; These are the public function signatures that will be implemented
;;;; in src/clysm/lib/sequence-runtime.lisp and registered for dispatch.
;;;;
;;;; HyperSpec references:
;;;;   [subseq](resources/HyperSpec/Body/f_subseq.htm)
;;;;   [adjust-array](resources/HyperSpec/Body/f_adjust.htm)

(in-package #:clysm)

;;; ============================================================
;;; SUBSEQ Runtime Function
;;; ============================================================

(defun subseq-rt (sequence start end)
  "Return a subsequence of SEQUENCE from START to END.

   See [subseq](resources/HyperSpec/Body/f_subseq.htm).

   Parameters:
     SEQUENCE - A proper sequence (string, list, or vector)
     START    - Non-negative integer, the starting index (inclusive)
     END      - Non-negative integer or NIL; ending index (exclusive)
                If NIL, defaults to (length sequence)

   Returns:
     A fresh sequence of the same type as SEQUENCE containing elements
     from index START up to but not including index END.

   Signals:
     error - If START or END are out of bounds
     error - If END < START
     error - If SEQUENCE is not a proper sequence

   Examples:
     (subseq-rt \"hello\" 1 3)     => \"el\"
     (subseq-rt '(a b c d) 1 3)   => (b c)
     (subseq-rt #(1 2 3 4) 2 nil) => #(3 4)

   Implementation Notes:
     - For strings, indices refer to character positions, not bytes
     - UTF-8 multi-byte characters are handled correctly
     - Uses only Layer 1 primitives"
  (declare (ignore sequence start end))
  (error "Contract only - not implemented"))

;;; ============================================================
;;; ADJUST-ARRAY Runtime Function
;;; ============================================================

(defun adjust-array-rt (array new-dimensions initial-element initial-element-p)
  "Return an array with dimensions adjusted to NEW-DIMENSIONS.

   See [adjust-array](resources/HyperSpec/Body/f_adjust.htm).

   Parameters:
     ARRAY             - A 1-dimensional array to adjust
     NEW-DIMENSIONS    - Integer or list of one integer for new size
     INITIAL-ELEMENT   - Value to fill new slots (if array grows)
     INITIAL-ELEMENT-P - T if :initial-element was explicitly provided

   Returns:
     A new array with the specified dimensions:
     - Existing elements preserved up to min(old-size, new-size)
     - New slots filled with INITIAL-ELEMENT (if provided) or NIL

   Signals:
     error - If ARRAY is not a 1-dimensional array
     error - If NEW-DIMENSIONS is invalid (negative or wrong structure)

   Examples:
     (adjust-array-rt #(1 2 3) 5 0 t)   => #(1 2 3 0 0)
     (adjust-array-rt #(1 2 3 4 5) 3 nil nil) => #(1 2 3)
     (adjust-array-rt #(a b c) '(3) nil nil)  => #(a b c)

   Implementation Notes:
     - MVP supports 1D arrays only
     - Multidimensional arrays will signal an error
     - Uses only Layer 1 primitives (make-array, aref, length)"
  (declare (ignore array new-dimensions initial-element initial-element-p))
  (error "Contract only - not implemented"))

;;; ============================================================
;;; Registration Function
;;; ============================================================

(defun register-sequence-array-runtime-functions ()
  "Register subseq and adjust-array for runtime dispatch.
   Called during module initialization to populate *runtime-function-table*."
  ;; HyperSpec: resources/HyperSpec/Body/f_subseq.htm
  (register-runtime-function 'subseq :$subseq-rt nil)  ; variadic (optional end)
  ;; HyperSpec: resources/HyperSpec/Body/f_adjust.htm
  (register-runtime-function 'adjust-array :$adjust-array-rt nil))  ; variadic (keyword args)

;;; EOF
