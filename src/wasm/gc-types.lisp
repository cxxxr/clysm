;;;; gc-types.lisp - WasmGC type definitions for Lisp values

(in-package #:clysm/wasm)

;;; WasmGC Type Indices
;;; These are the type indices for our Lisp value representations

(defparameter *lisp-type-indices*
  '((:value . 0)       ; Universal Lisp value type (ref eq)
    (:cons . 1)        ; Cons cell
    (:symbol . 2)      ; Symbol
    (:string . 3)      ; Immutable string
    (:vector . 4)      ; Mutable vector
    (:closure . 5)     ; Function closure
    (:env . 6)         ; Closure environment
    (:float . 7)       ; Boxed float
    (:bignum . 8)      ; Big integer
    (:values . 9))     ; Multiple values container
  "Mapping from Lisp type names to type indices.")

(defun get-type-index (type-name)
  "Get the type index for a Lisp type name."
  (or (cdr (assoc type-name *lisp-type-indices*))
      (error "Unknown Lisp type: ~A" type-name)))

;;; Type Definitions
;;; These define how Lisp values are represented in WasmGC

(defun make-lisp-type-section ()
  "Generate the type section entries for Lisp value types.
Returns a list of type definitions to be encoded."
  (list
   ;; Type 0: Universal value type - using (ref eq)
   ;; This is not a struct, but we mark it as the base

   ;; Type 1: Cons cell
   ;; (type $cons (struct
   ;;   (field $car (mut (ref eq)))
   ;;   (field $cdr (mut (ref eq)))))
   (make-gc-struct-type
    (list (make-gc-field +type-eqref+ t)   ; car - mutable ref eq
          (make-gc-field +type-eqref+ t))) ; cdr - mutable ref eq

   ;; Type 2: Symbol
   ;; (type $symbol (struct
   ;;   (field $name (ref $string))
   ;;   (field $value (mut (ref null eq)))
   ;;   (field $function (mut (ref null $closure)))
   ;;   (field $plist (mut (ref null eq)))))
   (make-gc-struct-type
    (list (make-gc-field 3 nil)            ; name - ref to string type
          (make-gc-field +type-eqref+ t)   ; value - mutable
          (make-gc-field +type-eqref+ t)   ; function - mutable
          (make-gc-field +type-eqref+ t))) ; plist - mutable

   ;; Type 3: String (immutable byte array)
   ;; (type $string (array i8))
   (make-gc-array-type +type-i32+ nil)  ; Using i32 for UTF-8 code units

   ;; Type 4: Vector (mutable array of values)
   ;; (type $vector (array (mut (ref eq))))
   (make-gc-array-type +type-eqref+ t)

   ;; Type 5: Closure
   ;; (type $closure (struct
   ;;   (field $code (ref $func))
   ;;   (field $env (ref null $env))))
   (make-gc-struct-type
    (list (make-gc-field +type-funcref+ nil)  ; code - function reference
          (make-gc-field 6 nil)))             ; env - ref to env type (nullable)

   ;; Type 6: Environment (for closures)
   ;; (type $env (array (mut (ref eq))))
   (make-gc-array-type +type-eqref+ t)

   ;; Type 7: Boxed Float
   ;; (type $float (struct (field $value f64)))
   (make-gc-struct-type
    (list (make-gc-field +type-f64+ nil)))  ; value - immutable f64

   ;; Type 8: Bignum
   ;; (type $bignum (struct
   ;;   (field $sign i32)
   ;;   (field $digits (ref $digit-array))))
   (make-gc-struct-type
    (list (make-gc-field +type-i32+ nil)   ; sign
          (make-gc-field 9 nil)))          ; digits array ref

   ;; Type 9: Digit array for bignum
   ;; (type $digit-array (array i64))
   (make-gc-array-type +type-i64+ nil)

   ;; Type 10: Multiple values container
   ;; (type $values (struct
   ;;   (field $count i32)
   ;;   (field $values (ref $vector))))
   (make-gc-struct-type
    (list (make-gc-field +type-i32+ nil)   ; count
          (make-gc-field 4 nil)))))        ; values vector ref

;;; Value Representation Helpers

(defun fixnum-p (value)
  "Check if value fits in i31ref range (-2^30 to 2^30-1)."
  (and (integerp value)
       (<= (- (expt 2 30)) value (1- (expt 2 30)))))

(defun immediate-p (value)
  "Check if value can be represented as an immediate (no allocation)."
  (or (fixnum-p value)
      (null value)
      (eq value t)))

;;; Encoding GC Type Definitions

(defun encode-gc-struct-type (buffer struct-type)
  "Encode a WasmGC struct type definition."
  (let ((fields (gc-struct-type-fields struct-type)))
    ;; struct type prefix
    (buffer-write-byte buffer #x5f)  ; struct
    ;; number of fields
    (buffer-write-uleb128 buffer (length fields))
    ;; encode each field
    (dolist (field fields)
      (encode-gc-field buffer field))))

(defun encode-gc-array-type (buffer array-type)
  "Encode a WasmGC array type definition."
  ;; array type prefix
  (buffer-write-byte buffer #x5e)  ; array
  ;; element type
  (encode-storage-type buffer (gc-array-type-element-type array-type))
  ;; mutability
  (buffer-write-byte buffer (if (gc-array-type-mutable array-type) 1 0)))

(defun encode-gc-field (buffer field)
  "Encode a WasmGC struct field."
  (encode-storage-type buffer (gc-field-type field))
  (buffer-write-byte buffer (if (gc-field-mutable field) 1 0)))

(defun encode-storage-type (buffer type)
  "Encode a storage type (value type or type index)."
  (etypecase type
    ((unsigned-byte 8)
     ;; Value type
     (buffer-write-byte buffer type))
    (integer
     ;; Type index (for references)
     (buffer-write-sleb128 buffer type))))
