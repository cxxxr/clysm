;;;; primitive-registry.lisp - Primitive compiler registrations
;;;;
;;;; Part of the Clysm compiler (002-primitive-dispatch-table)
;;;; Contains all primitive compiler registrations.
;;;; Loaded after func-section.lisp which defines the compiler functions.

(in-package #:clysm/compiler/codegen/primitive-dispatch)

;;; This file contains all calls to register-primitive-compiler.
;;; Each section corresponds to a category of primitives from the
;;; original case statement in func-section.lisp.
;;;
;;; Registration format:
;;;   (register-primitive-compiler 'symbol
;;;     (lambda (op args env) ...)
;;;     :arity N)
;;;
;;; Note: The lambda wrapper adapts the case statement's calling convention
;;; to the dispatch table's (op args env) signature.

;;; Helper macro for cleaner registration
(defmacro defprimitive (name compiler-form &key arity)
  "Register a primitive compiler.
   NAME: Symbol name of the primitive
   COMPILER-FORM: Form to compile the primitive, with ARGS and ENV available
   ARITY: Expected argument count (nil = variadic)"
  `(register-primitive-compiler ',name
     (lambda (op args env)
       (declare (ignore op))
       ,compiler-form)
     :arity ,arity))

;;; ==========================================================================
;;; Arithmetic Operators
;;; See: resources/HyperSpec/Body/f_pl.htm, f__.htm, f_st.htm, f_sl.htm
;;; ==========================================================================

(defprimitive + (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.add args env 0))
(defprimitive - (if (= 1 (length args))
                    (clysm/compiler/codegen/func-section::compile-unary-minus (first args) env)
                    (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.sub args env nil)))
(defprimitive * (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.mul args env 1))
(defprimitive / (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.div_s args env nil))
(defprimitive truncate (clysm/compiler/codegen/func-section::compile-truncate args env))
(defprimitive mod (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.rem_s args env nil) :arity 2)
(defprimitive rem (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.rem_s args env nil) :arity 2)

;; Rounding functions (001-division-rounding-primitives)
(defprimitive floor (clysm/compiler/codegen/func-section::compile-floor args env))
(defprimitive ceiling (clysm/compiler/codegen/func-section::compile-ceiling args env))
(defprimitive round (clysm/compiler/codegen/func-section::compile-round args env))
(defprimitive ffloor (clysm/compiler/codegen/func-section::compile-ffloor args env))
(defprimitive fceiling (clysm/compiler/codegen/func-section::compile-fceiling args env))
(defprimitive fround (clysm/compiler/codegen/func-section::compile-fround args env))

;; Increment/decrement primitives (001-arithmetic-primitives)
(defprimitive 1- (clysm/compiler/codegen/func-section::compile-1- args env) :arity 1)
(defprimitive 1+ (clysm/compiler/codegen/func-section::compile-1+ args env) :arity 1)

;; ANSI Numeric Functions (001-numeric-functions)
(defprimitive abs (clysm/compiler/codegen/func-section::compile-abs args env) :arity 1)
(defprimitive max (clysm/compiler/codegen/func-section::compile-max args env))
(defprimitive min (clysm/compiler/codegen/func-section::compile-min args env))
(defprimitive gcd (clysm/compiler/codegen/func-section::compile-gcd args env))
(defprimitive lcm (clysm/compiler/codegen/func-section::compile-lcm args env))

;;; ==========================================================================
;;; Bitwise Operators
;;; See: resources/HyperSpec/Body/f_logand.htm, f_logior.htm, f_ash.htm
;;; ==========================================================================

(defprimitive logand (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.and args env -1))
(defprimitive logior (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.or args env 0))
(defprimitive logxor (clysm/compiler/codegen/func-section::compile-arithmetic-op :i32.xor args env 0))
(defprimitive lognot (clysm/compiler/codegen/func-section::compile-lognot args env) :arity 1)
(defprimitive ash (clysm/compiler/codegen/func-section::compile-ash args env) :arity 2)
(defprimitive logcount (clysm/compiler/codegen/func-section::compile-logcount args env) :arity 1)
(defprimitive integer-length (clysm/compiler/codegen/func-section::compile-integer-length args env) :arity 1)

;; Bit Testing Functions (001-numeric-predicates)
(defprimitive logbitp (clysm/compiler/codegen/func-section::compile-logbitp args env) :arity 2)
(defprimitive logtest (clysm/compiler/codegen/func-section::compile-logtest args env) :arity 2)

;; Byte Specifier Functions (001-numeric-predicates)
(defprimitive byte (clysm/compiler/codegen/func-section::compile-byte args env) :arity 2)
(defprimitive byte-size (clysm/compiler/codegen/func-section::compile-byte-size args env) :arity 1)
(defprimitive byte-position (clysm/compiler/codegen/func-section::compile-byte-position args env) :arity 1)

;; Byte Operations (001-numeric-predicates)
(defprimitive ldb (clysm/compiler/codegen/func-section::compile-ldb args env) :arity 2)
(defprimitive dpb (clysm/compiler/codegen/func-section::compile-dpb args env) :arity 3)
(defprimitive mask-field (clysm/compiler/codegen/func-section::compile-mask-field args env) :arity 2)
(defprimitive deposit-field (clysm/compiler/codegen/func-section::compile-deposit-field args env) :arity 3)

;;; ==========================================================================
;;; Comparison Operators
;;; See: resources/HyperSpec/Body/f_eq_sle.htm
;;; ==========================================================================

(defprimitive < (clysm/compiler/codegen/func-section::compile-comparison-op :i32.lt_s args env))
(defprimitive > (clysm/compiler/codegen/func-section::compile-comparison-op :i32.gt_s args env))
(defprimitive <= (clysm/compiler/codegen/func-section::compile-comparison-op :i32.le_s args env))
(defprimitive >= (clysm/compiler/codegen/func-section::compile-comparison-op :i32.ge_s args env))
(defprimitive = (clysm/compiler/codegen/func-section::compile-comparison-op :i32.eq args env))
(defprimitive /= (clysm/compiler/codegen/func-section::compile-not-equal args env))

;;; ==========================================================================
;;; List Operations
;;; See: resources/HyperSpec/Body/f_cons.htm, f_car_c.htm, f_list_.htm
;;; ==========================================================================

(defprimitive cons (clysm/compiler/codegen/func-section::compile-cons args env) :arity 2)
(defprimitive car (clysm/compiler/codegen/func-section::compile-car args env) :arity 1)
(defprimitive cdr (clysm/compiler/codegen/func-section::compile-cdr args env) :arity 1)
(defprimitive list (clysm/compiler/codegen/func-section::compile-list args env))

;; Destructive modification
(defprimitive rplaca (clysm/compiler/codegen/func-section::compile-rplaca args env) :arity 2)
(defprimitive rplacd (clysm/compiler/codegen/func-section::compile-rplacd args env) :arity 2)

;; List accessors - aliases for car/cdr
(defprimitive first (clysm/compiler/codegen/func-section::compile-car args env) :arity 1)
(defprimitive rest (clysm/compiler/codegen/func-section::compile-cdr args env) :arity 1)
(defprimitive second (clysm/compiler/codegen/func-section::compile-nth-accessor 1 args env) :arity 1)
(defprimitive third (clysm/compiler/codegen/func-section::compile-nth-accessor 2 args env) :arity 1)
(defprimitive fourth (clysm/compiler/codegen/func-section::compile-nth-accessor 3 args env) :arity 1)
(defprimitive fifth (clysm/compiler/codegen/func-section::compile-nth-accessor 4 args env) :arity 1)
(defprimitive sixth (clysm/compiler/codegen/func-section::compile-nth-accessor 5 args env) :arity 1)
(defprimitive seventh (clysm/compiler/codegen/func-section::compile-nth-accessor 6 args env) :arity 1)
(defprimitive eighth (clysm/compiler/codegen/func-section::compile-nth-accessor 7 args env) :arity 1)
(defprimitive ninth (clysm/compiler/codegen/func-section::compile-nth-accessor 8 args env) :arity 1)
(defprimitive tenth (clysm/compiler/codegen/func-section::compile-nth-accessor 9 args env) :arity 1)
(defprimitive nth (clysm/compiler/codegen/func-section::compile-nth args env) :arity 2)
(defprimitive nthcdr (clysm/compiler/codegen/func-section::compile-nthcdr args env) :arity 2)

;; cXXr accessors
(defprimitive caar (clysm/compiler/codegen/func-section::compile-caar args env) :arity 1)
(defprimitive cadr (clysm/compiler/codegen/func-section::compile-cadr args env) :arity 1)
(defprimitive cdar (clysm/compiler/codegen/func-section::compile-cdar args env) :arity 1)
(defprimitive cddr (clysm/compiler/codegen/func-section::compile-cddr args env) :arity 1)
(defprimitive caaar (clysm/compiler/codegen/func-section::compile-caaar args env) :arity 1)
(defprimitive caadr (clysm/compiler/codegen/func-section::compile-caadr args env) :arity 1)
(defprimitive cadar (clysm/compiler/codegen/func-section::compile-cadar args env) :arity 1)
(defprimitive caddr (clysm/compiler/codegen/func-section::compile-caddr args env) :arity 1)
(defprimitive cdaar (clysm/compiler/codegen/func-section::compile-cdaar args env) :arity 1)
(defprimitive cdadr (clysm/compiler/codegen/func-section::compile-cdadr args env) :arity 1)
(defprimitive cddar (clysm/compiler/codegen/func-section::compile-cddar args env) :arity 1)
(defprimitive cdddr (clysm/compiler/codegen/func-section::compile-cdddr args env) :arity 1)

;; List utilities
(defprimitive nconc (clysm/compiler/codegen/func-section::compile-nconc args env))
(defprimitive endp (clysm/compiler/codegen/func-section::compile-endp args env) :arity 1)
(defprimitive list* (clysm/compiler/codegen/func-section::compile-list* args env))

;;; ==========================================================================
;;; Type Predicates
;;; See: resources/HyperSpec/Body/f_consp.htm, f_null.htm, f_nump.htm
;;; ==========================================================================

(defprimitive consp (clysm/compiler/codegen/func-section::compile-consp args env) :arity 1)
(defprimitive null (clysm/compiler/codegen/func-section::compile-null args env) :arity 1)
(defprimitive not (clysm/compiler/codegen/func-section::compile-not args env) :arity 1)
(defprimitive atom (clysm/compiler/codegen/func-section::compile-atom args env) :arity 1)
(defprimitive listp (clysm/compiler/codegen/func-section::compile-listp args env) :arity 1)

;; ANSI CL Type Predicates (023-type-predicates)
(defprimitive integerp (clysm/compiler/codegen/func-section::compile-integerp args env) :arity 1)
(defprimitive floatp (clysm/compiler/codegen/func-section::compile-floatp args env) :arity 1)
(defprimitive rationalp (clysm/compiler/codegen/func-section::compile-rationalp args env) :arity 1)
(defprimitive complexp (clysm/compiler/codegen/func-section::compile-complexp args env) :arity 1)
(defprimitive numberp (clysm/compiler/codegen/func-section::compile-numberp args env) :arity 1)
(defprimitive symbolp (clysm/compiler/codegen/func-section::compile-symbolp args env) :arity 1)
(defprimitive functionp (clysm/compiler/codegen/func-section::compile-functionp args env) :arity 1)
(defprimitive characterp (clysm/compiler/codegen/func-section::compile-characterp args env) :arity 1)
(defprimitive stringp (clysm/compiler/codegen/func-section::compile-stringp args env) :arity 1)

;; ANSI CL Numeric Predicates (023-type-predicates)
(defprimitive zerop (clysm/compiler/codegen/func-section::compile-zerop args env) :arity 1)
(defprimitive plusp (clysm/compiler/codegen/func-section::compile-plusp args env) :arity 1)
(defprimitive minusp (clysm/compiler/codegen/func-section::compile-minusp args env) :arity 1)
(defprimitive oddp (clysm/compiler/codegen/func-section::compile-oddp args env) :arity 1)
(defprimitive evenp (clysm/compiler/codegen/func-section::compile-evenp args env) :arity 1)

;; Signum (023-type-predicates)
(defprimitive signum (clysm/compiler/codegen/func-section::compile-signum args env) :arity 1)

;; Type predicate
(defprimitive typep (clysm/compiler/codegen/func-section::compile-typep args env) :arity 2)

;;; ==========================================================================
;;; Equality Predicates
;;; See: resources/HyperSpec/Body/f_eq.htm, f_eql.htm, f_equal.htm, f_equalp.htm
;;; ==========================================================================

(defprimitive eq (clysm/compiler/codegen/func-section::compile-eq args env) :arity 2)
(defprimitive eql (clysm/compiler/codegen/func-section::compile-eql args env) :arity 2)
(defprimitive equal (clysm/compiler/codegen/func-section::compile-equal args env) :arity 2)
(defprimitive equalp (clysm/compiler/codegen/func-section::compile-equalp args env) :arity 2)

;;; ==========================================================================
;;; Symbol Operations
;;; See: resources/HyperSpec/Body/f_symb_2.htm
;;; ==========================================================================

(defprimitive symbol-name (clysm/compiler/codegen/func-section::compile-symbol-name args env) :arity 1)
(defprimitive keywordp (clysm/compiler/codegen/func-section::compile-keywordp args env) :arity 1)
(defprimitive gensym (clysm/compiler/codegen/func-section::compile-gensym args env))

;;; ==========================================================================
;;; Sequence Functions
;;; See: resources/HyperSpec/Body/f_length.htm, f_append.htm, f_revers.htm
;;; ==========================================================================

(defprimitive length (clysm/compiler/codegen/func-section::compile-length args env) :arity 1)
(defprimitive append (clysm/compiler/codegen/func-section::compile-append args env))
(defprimitive reverse (clysm/compiler/codegen/func-section::compile-reverse args env) :arity 1)
(defprimitive nreverse (clysm/compiler/codegen/func-section::compile-nreverse args env) :arity 1)
(defprimitive last (clysm/compiler/codegen/func-section::compile-last args env))
(defprimitive butlast (clysm/compiler/codegen/func-section::compile-butlast args env))
(defprimitive copy-list (clysm/compiler/codegen/func-section::compile-copy-list args env) :arity 1)
(defprimitive subseq (clysm/compiler/codegen/func-section::compile-subseq args env))
(defprimitive concatenate (clysm/compiler/codegen/func-section::compile-concatenate args env))
(defprimitive copy-seq (clysm/compiler/codegen/func-section::compile-copy-seq args env) :arity 1)

;; Higher-order sequence functions
(defprimitive mapcar (clysm/compiler/codegen/func-section::compile-mapcar args env))
(defprimitive mapc (clysm/compiler/codegen/func-section::compile-mapc args env))
(defprimitive maplist (clysm/compiler/codegen/func-section::compile-maplist args env))
(defprimitive reduce (clysm/compiler/codegen/func-section::compile-reduce args env))

;; Alist construction (001-ansi-list-ops)
(defprimitive acons (clysm/compiler/codegen/func-section::compile-acons args env) :arity 3)
(defprimitive pairlis (clysm/compiler/codegen/func-section::compile-pairlis args env))
(defprimitive copy-alist (clysm/compiler/codegen/func-section::compile-copy-alist args env) :arity 1)

;; Set operations (001-ansi-list-ops)
(defprimitive adjoin (clysm/compiler/codegen/func-section::compile-adjoin args env))
(defprimitive union (clysm/compiler/codegen/func-section::compile-union args env))
(defprimitive intersection (clysm/compiler/codegen/func-section::compile-intersection args env))
(defprimitive set-difference (clysm/compiler/codegen/func-section::compile-set-difference args env))
(defprimitive subsetp (clysm/compiler/codegen/func-section::compile-subsetp args env))

;; Quantifier predicates
(defprimitive every (clysm/compiler/codegen/func-section::compile-every args env))
(defprimitive some (clysm/compiler/codegen/func-section::compile-some args env))
(defprimitive notany (clysm/compiler/codegen/func-section::compile-notany args env))
(defprimitive notevery (clysm/compiler/codegen/func-section::compile-notevery args env))

;;; ==========================================================================
;;; Character Functions
;;; See: resources/HyperSpec/Body/f_char_c.htm, f_code_c.htm
;;; ==========================================================================

(defprimitive char-code (clysm/compiler/codegen/func-section::compile-char-code args env) :arity 1)
(defprimitive code-char (clysm/compiler/codegen/func-section::compile-code-char args env) :arity 1)
(defprimitive char= (clysm/compiler/codegen/func-section::compile-char= args env))
(defprimitive char/= (clysm/compiler/codegen/func-section::compile-char/= args env))
(defprimitive char< (clysm/compiler/codegen/func-section::compile-char< args env))
(defprimitive char> (clysm/compiler/codegen/func-section::compile-char> args env))
(defprimitive char<= (clysm/compiler/codegen/func-section::compile-char<= args env))
(defprimitive char>= (clysm/compiler/codegen/func-section::compile-char>= args env))
(defprimitive char-equal (clysm/compiler/codegen/func-section::compile-char-equal args env))
(defprimitive char-lessp (clysm/compiler/codegen/func-section::compile-char-lessp args env))
(defprimitive char-greaterp (clysm/compiler/codegen/func-section::compile-char-greaterp args env))
(defprimitive char-not-lessp (clysm/compiler/codegen/func-section::compile-char-not-lessp args env))
(defprimitive char-not-greaterp (clysm/compiler/codegen/func-section::compile-char-not-greaterp args env))
(defprimitive char-upcase (clysm/compiler/codegen/func-section::compile-char-upcase args env) :arity 1)
(defprimitive char-downcase (clysm/compiler/codegen/func-section::compile-char-downcase args env) :arity 1)
(defprimitive alpha-char-p (clysm/compiler/codegen/func-section::compile-alpha-char-p args env) :arity 1)
(defprimitive digit-char-p (clysm/compiler/codegen/func-section::compile-digit-char-p args env))
(defprimitive alphanumericp (clysm/compiler/codegen/func-section::compile-alphanumericp args env) :arity 1)
(defprimitive upper-case-p (clysm/compiler/codegen/func-section::compile-upper-case-p args env) :arity 1)
(defprimitive lower-case-p (clysm/compiler/codegen/func-section::compile-lower-case-p args env) :arity 1)

;; Extended character functions (001-ansi-char-functions)
(defprimitive graphic-char-p (clysm/compiler/codegen/func-section::compile-graphic-char-p args env) :arity 1)
(defprimitive standard-char-p (clysm/compiler/codegen/func-section::compile-standard-char-p args env) :arity 1)
(defprimitive both-case-p (clysm/compiler/codegen/func-section::compile-both-case-p args env) :arity 1)
(defprimitive char-name (clysm/compiler/codegen/func-section::compile-char-name args env) :arity 1)
(defprimitive name-char (clysm/compiler/codegen/func-section::compile-name-char args env) :arity 1)
(defprimitive digit-char (clysm/compiler/codegen/func-section::compile-digit-char args env))
(defprimitive char-int (clysm/compiler/codegen/func-section::compile-char-int args env) :arity 1)

;;; ==========================================================================
;;; String Functions
;;; See: resources/HyperSpec/Body/f_char_.htm, f_stgeq_.htm
;;; ==========================================================================

(defprimitive char (clysm/compiler/codegen/func-section::compile-string-char args env) :arity 2)
(defprimitive schar (clysm/compiler/codegen/func-section::compile-string-char args env) :arity 2)
(defprimitive string= (clysm/compiler/codegen/func-section::compile-string= args env))
(defprimitive string/= (clysm/compiler/codegen/func-section::compile-string/= args env))
(defprimitive string< (clysm/compiler/codegen/func-section::compile-string< args env))
(defprimitive string> (clysm/compiler/codegen/func-section::compile-string> args env))
(defprimitive string<= (clysm/compiler/codegen/func-section::compile-string<= args env))
(defprimitive string>= (clysm/compiler/codegen/func-section::compile-string>= args env))
(defprimitive string-equal (clysm/compiler/codegen/func-section::compile-string-equal args env))
(defprimitive string-lessp (clysm/compiler/codegen/func-section::compile-string-lessp args env))
(defprimitive string-greaterp (clysm/compiler/codegen/func-section::compile-string-greaterp args env))
(defprimitive string-not-lessp (clysm/compiler/codegen/func-section::compile-string-not-lessp args env))
(defprimitive string-not-greaterp (clysm/compiler/codegen/func-section::compile-string-not-greaterp args env))
(defprimitive string-not-equal (clysm/compiler/codegen/func-section::compile-string-not-equal args env))
(defprimitive make-string (clysm/compiler/codegen/func-section::compile-make-string args env))
(defprimitive string (clysm/compiler/codegen/func-section::compile-string args env) :arity 1)
(defprimitive string-upcase (clysm/compiler/codegen/func-section::compile-string-upcase args env))
(defprimitive string-downcase (clysm/compiler/codegen/func-section::compile-string-downcase args env))
(defprimitive string-capitalize (clysm/compiler/codegen/func-section::compile-string-capitalize args env))
(defprimitive string-trim (clysm/compiler/codegen/func-section::compile-string-trim args env) :arity 2)
(defprimitive string-left-trim (clysm/compiler/codegen/func-section::compile-string-left-trim args env) :arity 2)
(defprimitive string-right-trim (clysm/compiler/codegen/func-section::compile-string-right-trim args env) :arity 2)
(defprimitive nstring-upcase (clysm/compiler/codegen/func-section::compile-nstring-upcase args env))
(defprimitive nstring-downcase (clysm/compiler/codegen/func-section::compile-nstring-downcase args env))
(defprimitive nstring-capitalize (clysm/compiler/codegen/func-section::compile-nstring-capitalize args env))
(defprimitive write-to-string (clysm/compiler/codegen/func-section::compile-write-to-string args env))

;;; ==========================================================================
;;; Numeric Accessors
;;; See: resources/HyperSpec/Body/f_numera.htm, f_denomi.htm
;;; ==========================================================================

(defprimitive numerator (clysm/compiler/codegen/func-section::compile-numerator args env) :arity 1)
(defprimitive denominator (clysm/compiler/codegen/func-section::compile-denominator args env) :arity 1)

;; Complex Number Functions (001-numeric-functions)
(defprimitive complex (clysm/compiler/codegen/func-section::compile-complex args env))
(defprimitive realpart (clysm/compiler/codegen/func-section::compile-realpart args env) :arity 1)
(defprimitive imagpart (clysm/compiler/codegen/func-section::compile-imagpart args env) :arity 1)
(defprimitive conjugate (clysm/compiler/codegen/func-section::compile-conjugate args env) :arity 1)
(defprimitive phase (clysm/compiler/codegen/func-section::compile-phase args env) :arity 1)

;;; ==========================================================================
;;; Trigonometric Functions
;;; See: resources/HyperSpec/Body/f_sin_c.htm, f_asin_.htm
;;; ==========================================================================

(defprimitive sin (clysm/compiler/codegen/func-section::compile-sin args env) :arity 1)
(defprimitive cos (clysm/compiler/codegen/func-section::compile-cos args env) :arity 1)
(defprimitive tan (clysm/compiler/codegen/func-section::compile-tan args env) :arity 1)
(defprimitive asin (clysm/compiler/codegen/func-section::compile-asin args env) :arity 1)
(defprimitive acos (clysm/compiler/codegen/func-section::compile-acos args env) :arity 1)
(defprimitive atan (clysm/compiler/codegen/func-section::compile-atan args env))

;;; ==========================================================================
;;; Mathematical Functions
;;; See: resources/HyperSpec/Body/f_exp_e.htm, f_log.htm, f_sqrt_.htm
;;; ==========================================================================

(defprimitive exp (clysm/compiler/codegen/func-section::compile-exp args env) :arity 1)
(defprimitive log (clysm/compiler/codegen/func-section::compile-log args env))
(defprimitive sqrt (clysm/compiler/codegen/func-section::compile-sqrt args env) :arity 1)
(defprimitive expt (clysm/compiler/codegen/func-section::compile-expt args env) :arity 2)

;;; ==========================================================================
;;; Hyperbolic Functions
;;; See: resources/HyperSpec/Body/f_sinh_.htm
;;; ==========================================================================

(defprimitive sinh (clysm/compiler/codegen/func-section::compile-sinh args env) :arity 1)
(defprimitive cosh (clysm/compiler/codegen/func-section::compile-cosh args env) :arity 1)
(defprimitive tanh (clysm/compiler/codegen/func-section::compile-tanh args env) :arity 1)
(defprimitive asinh (clysm/compiler/codegen/func-section::compile-asinh args env) :arity 1)
(defprimitive acosh (clysm/compiler/codegen/func-section::compile-acosh args env) :arity 1)
(defprimitive atanh (clysm/compiler/codegen/func-section::compile-atanh args env) :arity 1)

;;; ==========================================================================
;;; Numeric Type Conversion
;;; See: resources/HyperSpec/Body/f_float.htm, f_ration.htm
;;; ==========================================================================

(defprimitive float (clysm/compiler/codegen/func-section::compile-float args env))
(defprimitive rational (clysm/compiler/codegen/func-section::compile-rational args env) :arity 1)
(defprimitive rationalize (clysm/compiler/codegen/func-section::compile-rationalize args env) :arity 1)
(defprimitive parse-integer (clysm/compiler/codegen/func-section::compile-parse-integer args env))

;;; ==========================================================================
;;; Hash Table Operations
;;; See: resources/HyperSpec/Body/f_gethas.htm, f_remhas.htm
;;; ==========================================================================

(defprimitive make-hash-table (clysm/compiler/codegen/func-section::compile-make-hash-table args env))
(defprimitive gethash (clysm/compiler/codegen/func-section::compile-gethash args env))
(defprimitive puthash (clysm/compiler/codegen/func-section::compile-puthash args env) :arity 3)
(defprimitive remhash (clysm/compiler/codegen/func-section::compile-remhash args env) :arity 2)
(defprimitive maphash (clysm/compiler/codegen/func-section::compile-maphash args env) :arity 2)
(defprimitive hash-table-count (clysm/compiler/codegen/func-section::compile-hash-table-count args env) :arity 1)
(defprimitive hash-table-size (clysm/compiler/codegen/func-section::compile-hash-table-size args env) :arity 1)
(defprimitive clrhash (clysm/compiler/codegen/func-section::compile-clrhash args env) :arity 1)

;;; ==========================================================================
;;; Property Lists
;;; See: resources/HyperSpec/Body/f_getf.htm
;;; ==========================================================================

(defprimitive getf (clysm/compiler/codegen/func-section::compile-getf args env))

;;; ==========================================================================
;;; Error Signaling
;;; See: resources/HyperSpec/Body/f_error.htm
;;; ==========================================================================

(defprimitive error (clysm/compiler/codegen/func-section::compile-error args env))

;;; ==========================================================================
;;; Array Operations
;;; See: resources/HyperSpec/Body/f_aref.htm, f_mk_ar.htm
;;; ==========================================================================

(defprimitive make-array (clysm/compiler/codegen/func-section::compile-make-array args env))
(defprimitive aref (clysm/compiler/codegen/func-section::compile-aref args env))
(defprimitive svref (clysm/compiler/codegen/func-section::compile-svref args env) :arity 2)
(defprimitive elt (clysm/compiler/codegen/func-section::compile-elt args env) :arity 2)
(defprimitive coerce (clysm/compiler/codegen/func-section::compile-coerce args env) :arity 2)
(defprimitive vector-push-extend (clysm/compiler/codegen/func-section::compile-vector-push-extend args env))

;; ANSI Array Operations (001-ansi-array-ops)
(defprimitive array-rank (clysm/compiler/codegen/func-section::compile-array-rank args env) :arity 1)
(defprimitive array-dimension (clysm/compiler/codegen/func-section::compile-array-dimension args env) :arity 2)
(defprimitive array-dimensions (clysm/compiler/codegen/func-section::compile-array-dimensions args env) :arity 1)
(defprimitive array-total-size (clysm/compiler/codegen/func-section::compile-array-total-size args env) :arity 1)
(defprimitive array-row-major-index (clysm/compiler/codegen/func-section::compile-array-row-major-index args env))
(defprimitive row-major-aref (clysm/compiler/codegen/func-section::compile-row-major-aref args env) :arity 2)
(defprimitive adjustable-array-p (clysm/compiler/codegen/func-section::compile-adjustable-array-p args env) :arity 1)
(defprimitive adjust-array (clysm/compiler/codegen/func-section::compile-adjust-array args env))

;;; ==========================================================================
;;; Control Operations
;;; See: resources/HyperSpec/Body/f_apply.htm, f_funcal.htm
;;; ==========================================================================

(defprimitive apply (clysm/compiler/codegen/func-section::compile-apply args env))
(defprimitive funcall (clysm/compiler/codegen/func-section::compile-funcall args env))

;;; ==========================================================================
;;; I/O Operations
;;; See: resources/HyperSpec/Body/f_wr_by.htm
;;; ==========================================================================

(defprimitive write-byte (clysm/compiler/codegen/func-section::compile-write-byte args env) :arity 2)

;;; ==========================================================================
;;; Package Operations
;;; See: resources/HyperSpec/Body/f_find_p.htm, f_intern.htm
;;; ==========================================================================

(defprimitive find-package (clysm/compiler/codegen/func-section::compile-find-package args env) :arity 1)
(defprimitive intern (clysm/compiler/codegen/func-section::compile-intern args env))

;;; ==========================================================================
;;; Cross-Package Symbols (String-Based Lookup)
;;; Used for setf expanders and CLOS primitives
;;; ==========================================================================

;; These are registered with string-name for cross-package matching
;; They're already handled by the cond in compile-primitive-call, but
;; registering them here allows future migration to pure dispatch

;;; End of primitive-registry.lisp
