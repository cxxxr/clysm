(module
  ;; ============================================================
  ;; CLYSM Kernel - WebAssembly GC Common Lisp Runtime
  ;; ============================================================

  ;; === Type Definitions ===

  ;; String type (UTF-8 encoded, using i8 packed type)
  (type $string (array (mut i8)))

  ;; Cons cell
  (type $cons (struct
    (field $car (mut anyref))
    (field $cdr (mut anyref))
  ))

  ;; Symbol
  (type $symbol (struct
    (field $name (ref $string))      ;; print name (immutable)
    (field $value (mut anyref))      ;; value cell
    (field $function (mut anyref))   ;; function cell
    (field $plist (mut anyref))      ;; property list
    (field $package (mut anyref))    ;; home package
  ))

  ;; Simple vector
  (type $vector (array (mut anyref)))

  ;; Boxed float
  (type $float (struct
    (field $value f64)
  ))

  ;; Unbound marker
  (type $unbound (struct))

  ;; Package (symbol table)
  (type $package (struct
    (field $pkg_name (ref $string))     ;; package name
    (field $symbols (mut (ref $vector))) ;; hash table buckets
    (field $symbol_count (mut i32))     ;; number of symbols
  ))

  ;; Environment frame (lexical environment)
  (type $env_frame (struct
    (field $bindings (ref $vector))     ;; variable bindings
    (field $parent (mut anyref))        ;; parent environment (or NIL)
  ))

  ;; Function signature: (env, args_list) -> result
  (type $func_sig (func (param anyref anyref) (result anyref)))

  ;; Closure (function with captured environment)
  (type $closure (struct
    (field $code (ref $func_sig))       ;; Wasm function reference
    (field $env (mut anyref))           ;; lexical environment
    (field $name (mut anyref))          ;; function name (for debugging)
    (field $lambda_list (mut anyref))   ;; parameter list (for debugging)
    (field $arity (mut i32))            ;; expected number of arguments (-1 for &rest)
  ))

  ;; Primitive function (built-in function)
  (type $primitive (struct
    (field $code (ref $func_sig))       ;; Wasm function reference
    (field $name (ref $string))         ;; function name
    (field $arity i32)                  ;; expected number of arguments
  ))

  ;; Interpreted closure (Wasm-side evaluator)
  (type $interpreted_closure (struct
    (field $params anyref)              ;; lambda list
    (field $body anyref)                ;; body forms
    (field $env anyref)                 ;; captured lexical environment
    (field $name (mut anyref))          ;; optional name
    (field $arity (mut i32))            ;; cached arity
  ))

  ;; Control transfer object for non-local exits and errors
  ;; kind: 1=RETURN-FROM, 2=THROW, 3=ERROR
  (type $control (struct
    (field $kind i32)
    (field $code i32)                   ;; error code when kind=ERROR
    (field $a anyref)                   ;; payload A
    (field $b anyref)                   ;; payload B
  ))

  ;; === Imports ===

  (import "env" "raise_error" (func $raise_error (param i32 anyref anyref)))

  ;; === Global Special Objects ===

  ;; Unbound marker singleton
  (global $UNBOUND (ref $unbound)
    (struct.new $unbound))

  ;; NIL symbol (initialized in $init_globals)
  (global $NIL (mut (ref null $symbol)) (ref.null $symbol))

  ;; T symbol (initialized in $init_globals)
  (global $T (mut (ref null $symbol)) (ref.null $symbol))

  ;; COMMON-LISP-USER package (initialized in $init_globals)
  (global $CL_USER (mut (ref null $package)) (ref.null $package))

  ;; KEYWORD package (initialized in $init_globals)
  (global $KEYWORD (mut (ref null $package)) (ref.null $package))

  ;; Default hash table size for packages
  (global $HASH_TABLE_SIZE i32 (i32.const 127))

  ;; === Evaluator Globals ===

  ;; Special-form symbols (initialized in $init_interpreter)
  (global $SYM_QUOTE (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_IF (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_PROGN (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_SETQ (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_FUNCTION (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_LAMBDA (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_LET (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_LETSTAR (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_DEFUN (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_DEFVAR (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_BLOCK (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_RETURN_FROM (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_CATCH (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_THROW (mut (ref null $symbol)) (ref.null $symbol))
  (global $SYM_UNWIND_PROTECT (mut (ref null $symbol)) (ref.null $symbol))

  ;; Keyword used on symbol plist to mark specials
  (global $KW_SPECIAL (mut (ref null $symbol)) (ref.null $symbol))

  ;; Special binding trail (shallow binding)
  (global $TRAIL (mut (ref null $vector)) (ref.null $vector))
  (global $TRAIL_SP (mut i32) (i32.const 0))

  ;; === Helper Functions ===

  ;; Create a 3-character string
  (func $make_string_3 (param $c0 i32) (param $c1 i32) (param $c2 i32) (result (ref $string))
    (local $arr (ref $string))
    (local.set $arr (array.new $string (i32.const 0) (i32.const 3)))
    (array.set $string (local.get $arr) (i32.const 0) (local.get $c0))
    (array.set $string (local.get $arr) (i32.const 1) (local.get $c1))
    (array.set $string (local.get $arr) (i32.const 2) (local.get $c2))
    (local.get $arr))

  ;; Create a 1-character string
  (func $make_string_1 (param $c0 i32) (result (ref $string))
    (local $arr (ref $string))
    (local.set $arr (array.new $string (i32.const 0) (i32.const 1)))
    (array.set $string (local.get $arr) (i32.const 0) (local.get $c0))
    (local.get $arr))

  ;; === String Utility Functions ===

  ;; Hash function for strings (djb2 algorithm)
  (func $string_hash (export "string_hash") (param $s (ref $string)) (result i32)
    (local $hash i32)
    (local $i i32)
    (local $len i32)
    (local $c i32)

    (local.set $hash (i32.const 5381))
    (local.set $len (array.len (local.get $s)))
    (local.set $i (i32.const 0))

    (block $break
      (loop $continue
        (br_if $break (i32.ge_u (local.get $i) (local.get $len)))

        (local.set $c (array.get_u $string (local.get $s) (local.get $i)))
        ;; hash = hash * 33 + c = ((hash << 5) + hash) + c
        (local.set $hash
          (i32.add
            (i32.add
              (i32.shl (local.get $hash) (i32.const 5))
              (local.get $hash))
            (local.get $c)))

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $continue)
      )
    )
    ;; Return positive hash value
    (i32.and (local.get $hash) (i32.const 0x7FFFFFFF))
  )

  ;; Compare two strings for equality
  (func $string_equal (export "string_equal") (param $a (ref $string)) (param $b (ref $string)) (result i32)
    (local $len_a i32)
    (local $len_b i32)
    (local $i i32)

    (local.set $len_a (array.len (local.get $a)))
    (local.set $len_b (array.len (local.get $b)))

    ;; Check if lengths are equal
    (if (i32.ne (local.get $len_a) (local.get $len_b))
      (then (return (i32.const 0)))
    )

    ;; Compare byte by byte
    (local.set $i (i32.const 0))
    (block $break
      (loop $continue
        (br_if $break (i32.ge_u (local.get $i) (local.get $len_a)))

        (if (i32.ne
              (array.get_u $string (local.get $a) (local.get $i))
              (array.get_u $string (local.get $b) (local.get $i)))
          (then (return (i32.const 0)))
        )

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $continue)
      )
    )
    (i32.const 1)
  )

  ;; Create a new symbol with given name
  (func $make_symbol_internal (param $name (ref $string)) (result (ref $symbol))
    (struct.new $symbol
      (local.get $name)
      (global.get $UNBOUND)           ;; value = unbound
      (global.get $NIL)               ;; function = NIL
      (global.get $NIL)               ;; plist = NIL
      (global.get $NIL)               ;; package = NIL (will be set later)
    )
  )

  ;; Create a package with given name
  (func $make_package_internal (param $name (ref $string)) (result (ref $package))
    (struct.new $package
      (local.get $name)
      (array.new $vector (global.get $NIL) (global.get $HASH_TABLE_SIZE))
      (i32.const 0)
    )
  )

  ;; === Package and Symbol Interning ===

  ;; Register a symbol in a package's hash table
  ;; Uses separate chaining: each bucket contains a list of (name . symbol) pairs
  (func $register_symbol_in_package (param $sym (ref $symbol)) (param $pkg (ref $package))
    (local $name (ref $string))
    (local $hash i32)
    (local $index i32)
    (local $buckets (ref $vector))
    (local $bucket anyref)
    (local $entry (ref $cons))

    (local.set $name (struct.get $symbol $name (local.get $sym)))
    (local.set $hash (call $string_hash (local.get $name)))
    (local.set $index (i32.rem_u (local.get $hash) (global.get $HASH_TABLE_SIZE)))
    (local.set $buckets (struct.get $package $symbols (local.get $pkg)))
    (local.set $bucket (array.get $vector (local.get $buckets) (local.get $index)))

    ;; Create entry: (cons name symbol)
    (local.set $entry (struct.new $cons (local.get $name) (local.get $sym)))

    ;; Prepend to bucket: (cons entry bucket)
    (array.set $vector
      (local.get $buckets)
      (local.get $index)
      (struct.new $cons (local.get $entry) (local.get $bucket)))

    ;; Increment symbol count
    (struct.set $package $symbol_count
      (local.get $pkg)
      (i32.add (struct.get $package $symbol_count (local.get $pkg)) (i32.const 1)))
  )

  ;; Find a symbol by name in a package
  ;; Returns the symbol if found, or NIL if not found
  (func $find_symbol_in_package (export "find_symbol") (param $name (ref $string)) (param $pkg (ref $package)) (result anyref)
    (local $hash i32)
    (local $index i32)
    (local $buckets (ref $vector))
    (local $bucket anyref)
    (local $entry anyref)
    (local $entry_name anyref)
    (local $entry_sym anyref)

    (local.set $hash (call $string_hash (local.get $name)))
    (local.set $index (i32.rem_u (local.get $hash) (global.get $HASH_TABLE_SIZE)))
    (local.set $buckets (struct.get $package $symbols (local.get $pkg)))
    (local.set $bucket (array.get $vector (local.get $buckets) (local.get $index)))

    ;; Search through the bucket (list of (name . symbol) pairs)
    (block $not_found
      (loop $search
        ;; If bucket is NIL, symbol not found
        (br_if $not_found (call $is_nil (local.get $bucket)))

        ;; Get the first entry from the bucket
        (if (ref.test (ref $cons) (local.get $bucket))
          (then
            (local.set $entry (struct.get $cons $car (ref.cast (ref $cons) (local.get $bucket))))

            ;; Entry is (name . symbol)
            (if (ref.test (ref $cons) (local.get $entry))
              (then
                (local.set $entry_name (struct.get $cons $car (ref.cast (ref $cons) (local.get $entry))))
                (local.set $entry_sym (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $entry))))

                ;; Compare names
                (if (ref.test (ref $string) (local.get $entry_name))
                  (then
                    (if (call $string_equal (local.get $name) (ref.cast (ref $string) (local.get $entry_name)))
                      (then
                        ;; Found! Return the symbol
                        (return (local.get $entry_sym))
                      )
                    )
                  )
                )
              )
            )

            ;; Move to next entry in bucket
            (local.set $bucket (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $bucket))))
            (br $search)
          )
        )
      )
    )

    ;; Not found, return UNBOUND marker (not NIL, because NIL is a valid symbol)
    (global.get $UNBOUND)
  )

  ;; Helper: check if value is UNBOUND marker
  (func $is_unbound (param $x anyref) (result i32)
    (if (result i32) (ref.test (ref eq) (local.get $x))
      (then (ref.eq (ref.cast (ref eq) (local.get $x)) (global.get $UNBOUND)))
      (else (i32.const 0))
    )
  )

  ;; Intern a symbol: find existing or create new
  (func $intern (export "intern") (param $name (ref $string)) (param $pkg (ref $package)) (result (ref $symbol))
    (local $found anyref)
    (local $new_sym (ref $symbol))

    ;; First, try to find existing symbol
    (local.set $found (call $find_symbol_in_package (local.get $name) (local.get $pkg)))

    ;; If found (not UNBOUND), return it
    (if (result (ref $symbol)) (i32.eqz (call $is_unbound (local.get $found)))
      (then
        (ref.cast (ref $symbol) (local.get $found))
      )
      (else
        ;; Create new symbol
        (local.set $new_sym (call $make_symbol_internal (local.get $name)))

        ;; Set the symbol's package
        (struct.set $symbol $package (local.get $new_sym) (local.get $pkg))

        ;; Register in package
        (call $register_symbol_in_package (local.get $new_sym) (local.get $pkg))

        ;; Return new symbol
        (local.get $new_sym)
      )
    )
  )

  ;; Intern a symbol in the default (CL-USER) package
  (func $intern_default (export "intern_default") (param $name (ref $string)) (result (ref $symbol))
    (call $intern (local.get $name) (ref.as_non_null (global.get $CL_USER)))
  )

  ;; Intern a keyword symbol
  (func $intern_keyword (export "intern_keyword") (param $name (ref $string)) (result (ref $symbol))
    (local $sym (ref $symbol))
    (local.set $sym (call $intern (local.get $name) (ref.as_non_null (global.get $KEYWORD))))
    ;; Keywords evaluate to themselves
    (struct.set $symbol $value (local.get $sym) (local.get $sym))
    (local.get $sym)
  )

  ;; Helper: check if value is NIL
  (func $is_nil (param $x anyref) (result i32)
    (if (result i32) (ref.test (ref eq) (local.get $x))
      (then (ref.eq (ref.cast (ref eq) (local.get $x)) (global.get $NIL)))
      (else (i32.const 0))
    )
  )

  ;; Get symbol name
  (func $symbol_name (export "symbol_name") (param $sym (ref $symbol)) (result (ref $string))
    (struct.get $symbol $name (local.get $sym))
  )

  ;; Get symbol's package
  (func $symbol_package (export "symbol_package") (param $sym (ref $symbol)) (result anyref)
    (struct.get $symbol $package (local.get $sym))
  )

  ;; Get default package
  (func (export "get_cl_user_package") (result (ref $package))
    (ref.as_non_null (global.get $CL_USER))
  )

  ;; Get keyword package
  (func (export "get_keyword_package") (result (ref $package))
    (ref.as_non_null (global.get $KEYWORD))
  )

  ;; Check if value is a package
  (func (export "packagep") (param $x anyref) (result i32)
    (ref.test (ref $package) (local.get $x))
  )

  ;; Get package name
  (func (export "package_name") (param $pkg (ref $package)) (result (ref $string))
    (struct.get $package $pkg_name (local.get $pkg))
  )

  ;; Get unbound marker
  (func (export "get_unbound") (result anyref)
    (global.get $UNBOUND)
  )

  ;; Check if value is unbound marker
  (func (export "unboundp") (param $x anyref) (result i32)
    (if (result i32) (ref.test (ref eq) (local.get $x))
      (then (ref.eq (ref.cast (ref eq) (local.get $x)) (global.get $UNBOUND)))
      (else (i32.const 0))
    )
  )

  ;; === Initialization ===

  ;; Create package name strings
  (func $make_cl_user_name (result (ref $string))
    (local $s (ref $string))
    ;; "COMMON-LISP-USER"
    (local.set $s (array.new $string (i32.const 0) (i32.const 16)))
    (array.set $string (local.get $s) (i32.const 0) (i32.const 67))   ;; C
    (array.set $string (local.get $s) (i32.const 1) (i32.const 79))   ;; O
    (array.set $string (local.get $s) (i32.const 2) (i32.const 77))   ;; M
    (array.set $string (local.get $s) (i32.const 3) (i32.const 77))   ;; M
    (array.set $string (local.get $s) (i32.const 4) (i32.const 79))   ;; O
    (array.set $string (local.get $s) (i32.const 5) (i32.const 78))   ;; N
    (array.set $string (local.get $s) (i32.const 6) (i32.const 45))   ;; -
    (array.set $string (local.get $s) (i32.const 7) (i32.const 76))   ;; L
    (array.set $string (local.get $s) (i32.const 8) (i32.const 73))   ;; I
    (array.set $string (local.get $s) (i32.const 9) (i32.const 83))   ;; S
    (array.set $string (local.get $s) (i32.const 10) (i32.const 80))  ;; P
    (array.set $string (local.get $s) (i32.const 11) (i32.const 45))  ;; -
    (array.set $string (local.get $s) (i32.const 12) (i32.const 85))  ;; U
    (array.set $string (local.get $s) (i32.const 13) (i32.const 83))  ;; S
    (array.set $string (local.get $s) (i32.const 14) (i32.const 69))  ;; E
    (array.set $string (local.get $s) (i32.const 15) (i32.const 82))  ;; R
    (local.get $s)
  )

  (func $make_keyword_name (result (ref $string))
    (local $s (ref $string))
    ;; "KEYWORD"
    (local.set $s (array.new $string (i32.const 0) (i32.const 7)))
    (array.set $string (local.get $s) (i32.const 0) (i32.const 75))   ;; K
    (array.set $string (local.get $s) (i32.const 1) (i32.const 69))   ;; E
    (array.set $string (local.get $s) (i32.const 2) (i32.const 89))   ;; Y
    (array.set $string (local.get $s) (i32.const 3) (i32.const 87))   ;; W
    (array.set $string (local.get $s) (i32.const 4) (i32.const 79))   ;; O
    (array.set $string (local.get $s) (i32.const 5) (i32.const 82))   ;; R
    (array.set $string (local.get $s) (i32.const 6) (i32.const 68))   ;; D
    (local.get $s)
  )

  ;; Initialize NIL and T at module start
  (func $init_globals
    (local $nil_name (ref $string))
    (local $t_name (ref $string))
    (local $cl_user_pkg (ref $package))
    (local $keyword_pkg (ref $package))

    ;; Create "NIL" string
    (local.set $nil_name (call $make_string_3
      (i32.const 78)   ;; 'N'
      (i32.const 73)   ;; 'I'
      (i32.const 76))) ;; 'L'

    ;; Create "T" string
    (local.set $t_name (call $make_string_1
      (i32.const 84))) ;; 'T'

    ;; Create NIL symbol first (needed for package initialization)
    (global.set $NIL
      (struct.new $symbol
        (local.get $nil_name)
        (ref.null any)              ;; value (will be set to self)
        (ref.null any)              ;; function
        (ref.null any)              ;; plist (will be set to self)
        (ref.null any)))            ;; package

    ;; Set NIL's value to NIL itself
    (struct.set $symbol $value
      (global.get $NIL)
      (global.get $NIL))

    ;; Set NIL's plist to NIL itself
    (struct.set $symbol $plist
      (global.get $NIL)
      (global.get $NIL))

    ;; Create T symbol
    (global.set $T
      (struct.new $symbol
        (local.get $t_name)
        (ref.null any)              ;; value (will be set to self)
        (ref.null any)              ;; function
        (ref.null any)              ;; plist
        (ref.null any)))            ;; package

    ;; Set T's value to T itself
    (struct.set $symbol $value
      (global.get $T)
      (global.get $T))

    ;; Set T's plist to NIL
    (struct.set $symbol $plist
      (global.get $T)
      (global.get $NIL))

    ;; Create COMMON-LISP-USER package
    (local.set $cl_user_pkg (call $make_package_internal (call $make_cl_user_name)))
    (global.set $CL_USER (local.get $cl_user_pkg))

    ;; Create KEYWORD package
    (local.set $keyword_pkg (call $make_package_internal (call $make_keyword_name)))
    (global.set $KEYWORD (local.get $keyword_pkg))

    ;; Register NIL in CL-USER package
    (struct.set $symbol $package (ref.as_non_null (global.get $NIL)) (local.get $cl_user_pkg))
    (call $register_symbol_in_package (ref.as_non_null (global.get $NIL)) (local.get $cl_user_pkg))

    ;; Register T in CL-USER package
    (struct.set $symbol $package (ref.as_non_null (global.get $T)) (local.get $cl_user_pkg))
    (call $register_symbol_in_package (ref.as_non_null (global.get $T)) (local.get $cl_user_pkg))

    ;; Initialize evaluator (special forms, primitives, binding trail)
    (call $init_interpreter)
  )

  ;; Start function to initialize globals
  (start $init_globals)

  ;; === Exported Accessor Functions ===

  ;; Get NIL
  (func (export "get_nil") (result anyref)
    (global.get $NIL))

  ;; Get T
  (func (export "get_t") (result anyref)
    (global.get $T))

  ;; === Fixnum Functions ===
  ;; Fixnum uses i31ref with tag bit = 0 (LSB)
  ;; Value is stored as (value << 1) | 0

  ;; Create fixnum from i32
  (func (export "make_fixnum") (param $val i32) (result (ref i31))
    (ref.i31
      (i32.shl (local.get $val) (i32.const 1))))

  ;; Get fixnum value
  (func (export "fixnum_value") (param $x (ref i31)) (result i32)
    (i32.shr_s
      (i31.get_s (local.get $x))
      (i32.const 1)))

  ;; Check if value is fixnum (i31ref with LSB = 0)
  (func (export "fixnump") (param $x anyref) (result i32)
    (if (result i32) (ref.test (ref i31) (local.get $x))
      (then
        (i32.eqz
          (i32.and
            (i31.get_s (ref.cast (ref i31) (local.get $x)))
            (i32.const 1))))
      (else (i32.const 0))))

  ;; === Character Functions ===
  ;; Character uses i31ref with tag bit = 1 (LSB)
  ;; Code point is stored as (codepoint << 1) | 1

  ;; Create character from code point
  (func (export "make_char") (param $code i32) (result (ref i31))
    (ref.i31
      (i32.or
        (i32.shl (local.get $code) (i32.const 1))
        (i32.const 1))))

  ;; Get character code point
  (func (export "char_code") (param $x (ref i31)) (result i32)
    (i32.shr_u
      (i31.get_s (local.get $x))
      (i32.const 1)))

  ;; Check if value is character (i31ref with LSB = 1)
  (func (export "characterp") (param $x anyref) (result i32)
    (if (result i32) (ref.test (ref i31) (local.get $x))
      (then
        (i32.and
          (i31.get_s (ref.cast (ref i31) (local.get $x)))
          (i32.const 1)))
      (else (i32.const 0))))

  ;; === Cons Functions ===

  ;; Create cons cell
  (func (export "cons") (param $car anyref) (param $cdr anyref) (result (ref $cons))
    (struct.new $cons
      (local.get $car)
      (local.get $cdr)))

  ;; Get car
  (func (export "car") (param $x anyref) (result anyref)
    (if (result anyref) (ref.test (ref $cons) (local.get $x))
      (then
        (struct.get $cons $car
          (ref.cast (ref $cons) (local.get $x))))
      (else
        ;; car of NIL is NIL
        (if (result anyref) (ref.test (ref $symbol) (local.get $x))
          (then
            (if (result anyref) (ref.eq
                  (ref.cast (ref $symbol) (local.get $x))
                  (global.get $NIL))
              (then (global.get $NIL))
              (else (unreachable))))
          (else (unreachable))))))

  ;; Get cdr
  (func (export "cdr") (param $x anyref) (result anyref)
    (if (result anyref) (ref.test (ref $cons) (local.get $x))
      (then
        (struct.get $cons $cdr
          (ref.cast (ref $cons) (local.get $x))))
      (else
        ;; cdr of NIL is NIL
        (if (result anyref) (ref.test (ref $symbol) (local.get $x))
          (then
            (if (result anyref) (ref.eq
                  (ref.cast (ref $symbol) (local.get $x))
                  (global.get $NIL))
              (then (global.get $NIL))
              (else (unreachable))))
          (else (unreachable))))))

  ;; Check if cons
  (func (export "consp") (param $x anyref) (result i32)
    (ref.test (ref $cons) (local.get $x)))

  ;; Replace car
  (func (export "rplaca") (param $cell (ref $cons)) (param $val anyref) (result (ref $cons))
    (struct.set $cons $car (local.get $cell) (local.get $val))
    (local.get $cell))

  ;; Replace cdr
  (func (export "rplacd") (param $cell (ref $cons)) (param $val anyref) (result (ref $cons))
    (struct.set $cons $cdr (local.get $cell) (local.get $val))
    (local.get $cell))

  ;; === Symbol Functions ===

  ;; Check if symbol
  (func (export "symbolp") (param $x anyref) (result i32)
    (ref.test (ref $symbol) (local.get $x)))

  ;; Check if null (eq to NIL)
  (func (export "null_") (param $x anyref) (result i32)
    (if (result i32) (ref.test (ref eq) (local.get $x))
      (then (ref.eq
        (ref.cast (ref eq) (local.get $x))
        (global.get $NIL)))
      (else (i32.const 0))))

  ;; Get symbol value
  (func (export "symbol_value") (param $sym (ref $symbol)) (result anyref)
    (struct.get $symbol $value (local.get $sym)))

  ;; Set symbol value
  (func (export "set_symbol_value") (param $sym (ref $symbol)) (param $val anyref) (result anyref)
    (struct.set $symbol $value (local.get $sym) (local.get $val))
    (local.get $val))

  ;; Get symbol function
  (func (export "symbol_function") (param $sym (ref $symbol)) (result anyref)
    (struct.get $symbol $function (local.get $sym)))

  ;; Set symbol function
  (func (export "set_symbol_function") (param $sym (ref $symbol)) (param $val anyref) (result anyref)
    (struct.set $symbol $function (local.get $sym) (local.get $val))
    (local.get $val))

  ;; === Comparison Functions ===

  ;; eq - pointer equality
  (func (export "eq") (param $a anyref) (param $b anyref) (result i32)
    (if (result i32) (i32.and
          (ref.test (ref eq) (local.get $a))
          (ref.test (ref eq) (local.get $b)))
      (then (ref.eq
        (ref.cast (ref eq) (local.get $a))
        (ref.cast (ref eq) (local.get $b))))
      (else (i32.const 0))))

  ;; === Fixnum Arithmetic ===

  ;; Add two fixnums
  (func (export "fx_add") (param $a (ref i31)) (param $b (ref i31)) (result (ref i31))
    (ref.i31
      (i32.add
        (i31.get_s (local.get $a))
        (i31.get_s (local.get $b)))))

  ;; Subtract two fixnums
  (func (export "fx_sub") (param $a (ref i31)) (param $b (ref i31)) (result (ref i31))
    (ref.i31
      (i32.sub
        (i31.get_s (local.get $a))
        (i31.get_s (local.get $b)))))

  ;; Multiply two fixnums
  (func (export "fx_mul") (param $a (ref i31)) (param $b (ref i31)) (result (ref i31))
    (local $va i32)
    (local $vb i32)
    ;; Extract values (remove tag)
    (local.set $va (i32.shr_s (i31.get_s (local.get $a)) (i32.const 1)))
    (local.set $vb (i32.shr_s (i31.get_s (local.get $b)) (i32.const 1)))
    ;; Multiply and re-tag
    (ref.i31
      (i32.shl
        (i32.mul (local.get $va) (local.get $vb))
        (i32.const 1))))

  ;; Compare fixnums: less than
  (func (export "fx_lt") (param $a (ref i31)) (param $b (ref i31)) (result i32)
    (i32.lt_s
      (i31.get_s (local.get $a))
      (i31.get_s (local.get $b))))

  ;; Compare fixnums: less than or equal
  (func (export "fx_le") (param $a (ref i31)) (param $b (ref i31)) (result i32)
    (i32.le_s
      (i31.get_s (local.get $a))
      (i31.get_s (local.get $b))))

  ;; Compare fixnums: greater than
  (func (export "fx_gt") (param $a (ref i31)) (param $b (ref i31)) (result i32)
    (i32.gt_s
      (i31.get_s (local.get $a))
      (i31.get_s (local.get $b))))

  ;; Compare fixnums: greater than or equal
  (func (export "fx_ge") (param $a (ref i31)) (param $b (ref i31)) (result i32)
    (i32.ge_s
      (i31.get_s (local.get $a))
      (i31.get_s (local.get $b))))

  ;; Compare fixnums: equal
  (func (export "fx_eq") (param $a (ref i31)) (param $b (ref i31)) (result i32)
    (i32.eq
      (i31.get_s (local.get $a))
      (i31.get_s (local.get $b))))

  ;; === String Functions ===

  ;; Create a string of given length (initialized to 0)
  (func (export "make_string") (param $len i32) (result (ref $string))
    (array.new $string (i32.const 0) (local.get $len)))

  ;; Get string length
  (func (export "string_length") (param $s (ref $string)) (result i32)
    (array.len (local.get $s)))

  ;; Check if string
  (func (export "stringp") (param $x anyref) (result i32)
    (ref.test (ref $string) (local.get $x)))

  ;; Get character at index (returns byte value)
  (func (export "schar") (param $s (ref $string)) (param $i i32) (result i32)
    (array.get_u $string (local.get $s) (local.get $i)))

  ;; Set character at index
  (func (export "schar_set") (param $s (ref $string)) (param $i i32) (param $c i32) (result (ref $string))
    (array.set $string (local.get $s) (local.get $i) (local.get $c))
    (local.get $s))

  ;; === Vector Functions ===

  ;; Create a vector of given length (initialized to NIL)
  (func (export "make_vector") (param $len i32) (result (ref $vector))
    (array.new $vector (global.get $NIL) (local.get $len)))

  ;; Get vector length
  (func (export "vector_length") (param $v (ref $vector)) (result i32)
    (array.len (local.get $v)))

  ;; Check if vector
  (func (export "vectorp") (param $x anyref) (result i32)
    (ref.test (ref $vector) (local.get $x)))

  ;; Get element at index
  (func (export "svref") (param $v (ref $vector)) (param $i i32) (result anyref)
    (array.get $vector (local.get $v) (local.get $i)))

  ;; Set element at index
  (func (export "svset") (param $v (ref $vector)) (param $i i32) (param $val anyref) (result anyref)
    (array.set $vector (local.get $v) (local.get $i) (local.get $val))
    (local.get $val))

  ;; === Environment Functions ===

  ;; Create an environment frame
  (func (export "make_env_frame") (param $parent anyref) (param $size i32) (result (ref $env_frame))
    (struct.new $env_frame
      (array.new $vector (global.get $UNBOUND) (local.get $size))
      (local.get $parent)))

  ;; Check if value is an environment frame
  (func (export "env_framep") (param $x anyref) (result i32)
    (ref.test (ref $env_frame) (local.get $x)))

  ;; Get binding at index in environment frame
  (func (export "env_ref") (param $env (ref $env_frame)) (param $index i32) (result anyref)
    (array.get $vector
      (struct.get $env_frame $bindings (local.get $env))
      (local.get $index)))

  ;; Set binding at index in environment frame
  (func (export "env_set") (param $env (ref $env_frame)) (param $index i32) (param $val anyref) (result anyref)
    (array.set $vector
      (struct.get $env_frame $bindings (local.get $env))
      (local.get $index)
      (local.get $val))
    (local.get $val))

  ;; Get parent environment
  (func (export "env_parent") (param $env (ref $env_frame)) (result anyref)
    (struct.get $env_frame $parent (local.get $env)))

  ;; Get size of environment frame
  (func (export "env_size") (param $env (ref $env_frame)) (result i32)
    (array.len (struct.get $env_frame $bindings (local.get $env))))

  ;; Lookup variable by traversing environment chain
  ;; depth = how many frames to go up, index = slot in that frame
  (func (export "env_lookup") (param $env anyref) (param $depth i32) (param $index i32) (result anyref)
    (local $current anyref)
    (local $i i32)

    (local.set $current (local.get $env))
    (local.set $i (i32.const 0))

    ;; Traverse up the chain
    (block $done
      (loop $next
        (br_if $done (i32.ge_s (local.get $i) (local.get $depth)))
        (if (ref.test (ref $env_frame) (local.get $current))
          (then
            (local.set $current
              (struct.get $env_frame $parent
                (ref.cast (ref $env_frame) (local.get $current)))))
          (else
            ;; Error: invalid environment chain
            (return (global.get $UNBOUND))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $next)))

    ;; Get value at index
    (if (result anyref) (ref.test (ref $env_frame) (local.get $current))
      (then
        (array.get $vector
          (struct.get $env_frame $bindings
            (ref.cast (ref $env_frame) (local.get $current)))
          (local.get $index)))
      (else
        (global.get $UNBOUND))))

  ;; Set variable by traversing environment chain
  (func (export "env_set_at") (param $env anyref) (param $depth i32) (param $index i32) (param $val anyref) (result anyref)
    (local $current anyref)
    (local $i i32)

    (local.set $current (local.get $env))
    (local.set $i (i32.const 0))

    ;; Traverse up the chain
    (block $done
      (loop $next
        (br_if $done (i32.ge_s (local.get $i) (local.get $depth)))
        (if (ref.test (ref $env_frame) (local.get $current))
          (then
            (local.set $current
              (struct.get $env_frame $parent
                (ref.cast (ref $env_frame) (local.get $current)))))
          (else
            (return (global.get $UNBOUND))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $next)))

    ;; Set value at index
    (if (result anyref) (ref.test (ref $env_frame) (local.get $current))
      (then
        (array.set $vector
          (struct.get $env_frame $bindings
            (ref.cast (ref $env_frame) (local.get $current)))
          (local.get $index)
          (local.get $val))
        (local.get $val))
      (else
        (global.get $UNBOUND))))

  ;; === Closure Functions ===

  ;; Create a closure
  (func (export "make_closure") (param $code (ref $func_sig)) (param $env anyref) (param $name anyref) (param $lambda_list anyref) (param $arity i32) (result (ref $closure))
    (struct.new $closure
      (local.get $code)
      (local.get $env)
      (local.get $name)
      (local.get $lambda_list)
      (local.get $arity)))

  ;; Check if value is a closure
  (func (export "closurep") (param $x anyref) (result i32)
    (ref.test (ref $closure) (local.get $x)))

  ;; Get closure code
  (func (export "closure_code") (param $c (ref $closure)) (result (ref $func_sig))
    (struct.get $closure $code (local.get $c)))

  ;; Get closure environment
  (func (export "closure_env") (param $c (ref $closure)) (result anyref)
    (struct.get $closure $env (local.get $c)))

  ;; Get closure name
  (func (export "closure_name") (param $c (ref $closure)) (result anyref)
    (struct.get $closure $name (local.get $c)))

  ;; Get closure lambda list
  (func (export "closure_lambda_list") (param $c (ref $closure)) (result anyref)
    (struct.get $closure $lambda_list (local.get $c)))

  ;; Get closure arity
  (func (export "closure_arity") (param $c (ref $closure)) (result i32)
    (struct.get $closure $arity (local.get $c)))

  ;; Set closure name
  (func (export "set_closure_name") (param $c (ref $closure)) (param $name anyref) (result anyref)
    (struct.set $closure $name (local.get $c) (local.get $name))
    (local.get $name))

  ;; === Primitive Functions ===

  ;; Create a primitive function
  (func (export "make_primitive") (param $code (ref $func_sig)) (param $name (ref $string)) (param $arity i32) (result (ref $primitive))
    (struct.new $primitive
      (local.get $code)
      (local.get $name)
      (local.get $arity)))

  ;; Check if value is a primitive
  (func (export "primitivep") (param $x anyref) (result i32)
    (ref.test (ref $primitive) (local.get $x)))

  ;; Get primitive code
  (func (export "primitive_code") (param $p (ref $primitive)) (result (ref $func_sig))
    (struct.get $primitive $code (local.get $p)))

  ;; Get primitive name
  (func (export "primitive_name") (param $p (ref $primitive)) (result (ref $string))
    (struct.get $primitive $name (local.get $p)))

  ;; Get primitive arity
  (func (export "primitive_arity") (param $p (ref $primitive)) (result i32)
    (struct.get $primitive $arity (local.get $p)))

  ;; === Function Type Check ===

  ;; Check if value is any kind of function (closure, primitive, interpreted closure)
  (func (export "functionp") (param $x anyref) (result i32)
    (i32.or
      (ref.test (ref $closure) (local.get $x))
      (i32.or
        (ref.test (ref $primitive) (local.get $x))
        (ref.test (ref $interpreted_closure) (local.get $x)))))

  ;; === Apply Function ===

  ;; Call a closure with given arguments list
  (func (export "apply_closure") (param $c (ref $closure)) (param $args anyref) (result anyref)
    (call_ref $func_sig
      (struct.get $closure $env (local.get $c))
      (local.get $args)
      (struct.get $closure $code (local.get $c))))

  ;; Call a primitive with given arguments list
  (func (export "apply_primitive") (param $p (ref $primitive)) (param $args anyref) (result anyref)
    (call_ref $func_sig
      (global.get $NIL)  ;; primitives don't use env
      (local.get $args)
      (struct.get $primitive $code (local.get $p))))

  ;; ============================================================
  ;; Evaluator (Wasm-side interpreter)
  ;; ============================================================

  ;; === Evaluator String Constants ===

  (data $str_SPECIAL "SPECIAL")
  (data $str_QUOTE "QUOTE")
  (data $str_IF "IF")
  (data $str_PROGN "PROGN")
  (data $str_SETQ "SETQ")
  (data $str_FUNCTION "FUNCTION")
  (data $str_LAMBDA "LAMBDA")
  (data $str_LET "LET")
  (data $str_LETSTAR "LET*")
  (data $str_DEFUN "DEFUN")
  (data $str_DEFVAR "DEFVAR")
  (data $str_BLOCK "BLOCK")
  (data $str_RETURN_FROM "RETURN-FROM")
  (data $str_CATCH "CATCH")
  (data $str_THROW "THROW")
  (data $str_UNWIND_PROTECT "UNWIND-PROTECT")

  (data $str_NULL "NULL")
  (data $str_ATOM "ATOM")
  (data $str_CONSP "CONSP")
  (data $str_SYMBOLP "SYMBOLP")
  (data $str_NUMBERP "NUMBERP")
  (data $str_STRINGP "STRINGP")
  (data $str_FUNCTIONP "FUNCTIONP")
  (data $str_CAR "CAR")
  (data $str_CDR "CDR")
  (data $str_CONS "CONS")
  (data $str_RPLACA "RPLACA")
  (data $str_RPLACD "RPLACD")
  (data $str_LIST "LIST")
  (data $str_LENGTH "LENGTH")
  (data $str_EQ "EQ")
  (data $str_EQL "EQL")
  (data $str_EQUAL "EQUAL")
  (data $str_PLUS "+")
  (data $str_MINUS "-")
  (data $str_STAR "*")
  (data $str_SLASH "/")
  (data $str_LT "<")
  (data $str_GT ">")
  (data $str_LE "<=")
  (data $str_GE ">=")
  (data $str_NUMEQ "=")
  (data $str_NOT "NOT")
  (data $str_PRINT "PRINT")
  (data $str_FUNCALL "FUNCALL")
  (data $str_APPLY "APPLY")

  ;; === Evaluator Helpers ===

  (func $eq (param $a anyref) (param $b anyref) (result i32)
    (if (result i32)
        (i32.and
          (ref.test (ref eq) (local.get $a))
          (ref.test (ref eq) (local.get $b)))
      (then
        (ref.eq
          (ref.cast (ref eq) (local.get $a))
          (ref.cast (ref eq) (local.get $b))))
      (else (i32.const 0))))

  (func $make_cons (param $car anyref) (param $cdr anyref) (result (ref $cons))
    (struct.new $cons (local.get $car) (local.get $cdr)))

  (func $car_safe (param $x anyref) (result anyref)
    (if (result anyref) (ref.test (ref $cons) (local.get $x))
      (then
        (struct.get $cons $car (ref.cast (ref $cons) (local.get $x))))
      (else (global.get $NIL))))

  (func $cdr_safe (param $x anyref) (result anyref)
    (if (result anyref) (ref.test (ref $cons) (local.get $x))
      (then
        (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $x))))
      (else (global.get $NIL))))

  (func $make_fixnum_i32 (param $val i32) (result (ref i31))
    (ref.i31 (i32.shl (local.get $val) (i32.const 1))))

  (func $fixnum_value_i32 (param $x (ref i31)) (result i32)
    (i32.shr_s (i31.get_s (local.get $x)) (i32.const 1)))

  (func $is_fixnum (param $x anyref) (result i32)
    (if (result i32) (ref.test (ref i31) (local.get $x))
      (then
        (i32.eqz
          (i32.and
            (i31.get_s (ref.cast (ref i31) (local.get $x)))
            (i32.const 1))))
      (else (i32.const 0))))

  (func $list_length (param $list anyref) (result i32)
    (local $count i32)
    (local $cur anyref)
    (local.set $count (i32.const 0))
    (local.set $cur (local.get $list))
    (block $done
      (loop $loop
        (br_if $done (i32.eqz (ref.test (ref $cons) (local.get $cur))))
        (local.set $count (i32.add (local.get $count) (i32.const 1)))
        (local.set $cur
          (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $cur))))
        (br $loop)))
    (local.get $count))

  ;; === Control Objects ===

  (func $controlp (param $x anyref) (result i32)
    (ref.test (ref $control) (local.get $x)))

  (func $make_return_from (param $name anyref) (param $value anyref) (result (ref $control))
    (struct.new $control
      (i32.const 1)      ;; kind
      (i32.const 0)      ;; code
      (local.get $name)
      (local.get $value)))

  (func $make_throw (param $tag anyref) (param $value anyref) (result (ref $control))
    (struct.new $control
      (i32.const 2)      ;; kind
      (i32.const 0)      ;; code
      (local.get $tag)
      (local.get $value)))

  (func $make_error (param $code i32) (param $a anyref) (param $b anyref) (result (ref $control))
    (struct.new $control
      (i32.const 3)      ;; kind
      (local.get $code)
      (local.get $a)
      (local.get $b)))

  ;; === Special Binding Trail (Shallow Binding) ===

  (func $trail_ensure_capacity (param $needed i32)
    (local $vec (ref $vector))
    (local $len i32)
    (local $new_len i32)
    (local $new_vec (ref $vector))
    (local $i i32)

    (local.set $vec (ref.as_non_null (global.get $TRAIL)))
    (local.set $len (array.len (local.get $vec)))
    (if (i32.ge_u (local.get $len) (local.get $needed))
      (then (return)))

    (local.set $new_len (i32.mul (local.get $len) (i32.const 2)))
    (if (i32.lt_u (local.get $new_len) (local.get $needed))
      (then (local.set $new_len (local.get $needed))))

    (local.set $new_vec (array.new $vector (global.get $NIL) (local.get $new_len)))
    (local.set $i (i32.const 0))
    (block $done
      (loop $copy
        (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
        (array.set $vector (local.get $new_vec) (local.get $i)
          (array.get $vector (local.get $vec) (local.get $i)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $copy)))
    (global.set $TRAIL (local.get $new_vec)))

  (func $trail_push (param $sym (ref $symbol)) (param $old anyref)
    (local $sp i32)
    (local $vec (ref $vector))
    (local.set $sp (global.get $TRAIL_SP))
    (call $trail_ensure_capacity (i32.add (local.get $sp) (i32.const 2)))
    (local.set $vec (ref.as_non_null (global.get $TRAIL)))
    (array.set $vector (local.get $vec) (local.get $sp) (local.get $sym))
    (array.set $vector (local.get $vec) (i32.add (local.get $sp) (i32.const 1)) (local.get $old))
    (global.set $TRAIL_SP (i32.add (local.get $sp) (i32.const 2))))

  (func $trail_restore_to (param $sp0 i32)
    (local $sp i32)
    (local $vec (ref $vector))
    (local $sym anyref)
    (local $old anyref)
    (block $done
      (loop $loop
        (local.set $sp (global.get $TRAIL_SP))
        (br_if $done (i32.le_u (local.get $sp) (local.get $sp0)))
        (local.set $sp (i32.sub (local.get $sp) (i32.const 2)))
        (local.set $vec (ref.as_non_null (global.get $TRAIL)))
        (local.set $sym (array.get $vector (local.get $vec) (local.get $sp)))
        (local.set $old (array.get $vector (local.get $vec) (i32.add (local.get $sp) (i32.const 1))))
        (struct.set $symbol $value (ref.cast (ref $symbol) (local.get $sym)) (local.get $old))
        (global.set $TRAIL_SP (local.get $sp))
        (br $loop))))

  (func $bind_special (param $sym (ref $symbol)) (param $val anyref)
    (local $old anyref)
    (local.set $old (struct.get $symbol $value (local.get $sym)))
    (call $trail_push (local.get $sym) (local.get $old))
    (struct.set $symbol $value (local.get $sym) (local.get $val)))

  ;; === Special Variable Helpers ===

  (func $declare_special (param $sym (ref $symbol))
    (local $plist anyref)
    (local.set $plist (struct.get $symbol $plist (local.get $sym)))
    (struct.set $symbol $plist (local.get $sym)
      (call $make_cons
        (ref.as_non_null (global.get $KW_SPECIAL))
        (call $make_cons (global.get $T) (local.get $plist)))))

  (func $symbol_special_p (param $sym (ref $symbol)) (result i32)
    (local $p anyref)
    (local $key anyref)
    (local $name (ref $string))
    (local $len i32)
    (local $first i32)
    (local $last i32)

    ;; Check plist marker (:SPECIAL T ...)
    (local.set $p (struct.get $symbol $plist (local.get $sym)))
    (block $plist_done
      (loop $plist_loop
        (br_if $plist_done (call $is_nil (local.get $p)))
        (br_if $plist_done (i32.eqz (ref.test (ref $cons) (local.get $p))))
        (local.set $key (struct.get $cons $car (ref.cast (ref $cons) (local.get $p))))
        (local.set $p (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $p))))
        ;; Skip value cell
        (br_if $plist_done (call $is_nil (local.get $p)))
        (br_if $plist_done (i32.eqz (ref.test (ref $cons) (local.get $p))))
        (local.set $p (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $p))))
        (if (call $eq (local.get $key) (ref.as_non_null (global.get $KW_SPECIAL)))
          (then (return (i32.const 1))))
        (br $plist_loop)))

    ;; Naming convention: *FOO*
    (local.set $name (struct.get $symbol $name (local.get $sym)))
    (local.set $len (array.len (local.get $name)))
    (if (result i32) (i32.ge_u (local.get $len) (i32.const 2))
      (then
        (local.set $first (array.get_u $string (local.get $name) (i32.const 0)))
        (local.set $last (array.get_u $string (local.get $name) (i32.sub (local.get $len) (i32.const 1))))
        (i32.and
          (i32.eq (local.get $first) (i32.const 42))
          (i32.eq (local.get $last) (i32.const 42))))
      (else (i32.const 0))))

  ;; === Lexical Environment Search (linear, symbol/value pairs) ===

  (func $env_lookup_symbol (param $env anyref) (param $sym (ref $symbol)) (result anyref)
    (local $current anyref)
    (local $frame (ref $env_frame))
    (local $bindings (ref $vector))
    (local $size i32)
    (local $i i32)

    (local.set $current (local.get $env))
    (block $done
      (loop $next
        (br_if $done (i32.eqz (ref.test (ref $env_frame) (local.get $current))))
        (local.set $frame (ref.cast (ref $env_frame) (local.get $current)))
        (local.set $bindings (struct.get $env_frame $bindings (local.get $frame)))
        (local.set $size (array.len (local.get $bindings)))
        (local.set $i (i32.const 0))
        (block $scan_done
          (loop $scan
            (br_if $scan_done (i32.ge_u (local.get $i) (local.get $size)))
            (if (call $eq
                  (array.get $vector (local.get $bindings) (local.get $i))
                  (local.get $sym))
              (then
                (return (array.get $vector (local.get $bindings) (i32.add (local.get $i) (i32.const 1))))))
            (local.set $i (i32.add (local.get $i) (i32.const 2)))
            (br $scan)))
        (local.set $current (struct.get $env_frame $parent (local.get $frame)))
        (br $next)))
    (global.get $UNBOUND))

  (func $env_set_symbol (param $env anyref) (param $sym (ref $symbol)) (param $val anyref) (result i32)
    (local $current anyref)
    (local $frame (ref $env_frame))
    (local $bindings (ref $vector))
    (local $size i32)
    (local $i i32)

    (local.set $current (local.get $env))
    (block $done
      (loop $next
        (br_if $done (i32.eqz (ref.test (ref $env_frame) (local.get $current))))
        (local.set $frame (ref.cast (ref $env_frame) (local.get $current)))
        (local.set $bindings (struct.get $env_frame $bindings (local.get $frame)))
        (local.set $size (array.len (local.get $bindings)))
        (local.set $i (i32.const 0))
        (block $scan_done
          (loop $scan
            (br_if $scan_done (i32.ge_u (local.get $i) (local.get $size)))
            (if (call $eq
                  (array.get $vector (local.get $bindings) (local.get $i))
                  (local.get $sym))
              (then
                (array.set $vector (local.get $bindings) (i32.add (local.get $i) (i32.const 1)) (local.get $val))
                (return (i32.const 1))))
            (local.set $i (i32.add (local.get $i) (i32.const 2)))
            (br $scan)))
        (local.set $current (struct.get $env_frame $parent (local.get $frame)))
        (br $next)))
    (i32.const 0))

  ;; === Primitive Implementations ===

  (func $prim_null (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref) (call $is_nil (call $car_safe (local.get $args)))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_atom (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref) (ref.test (ref $cons) (call $car_safe (local.get $args)))
      (then (global.get $NIL))
      (else (global.get $T))))

  (func $prim_consp (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref) (ref.test (ref $cons) (call $car_safe (local.get $args)))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_symbolp (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref) (ref.test (ref $symbol) (call $car_safe (local.get $args)))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_numberp (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref) (call $is_fixnum (call $car_safe (local.get $args)))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_stringp (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref) (ref.test (ref $string) (call $car_safe (local.get $args)))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_functionp (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $x anyref)
    (local.set $x (call $car_safe (local.get $args)))
    (if (result anyref)
        (i32.or
          (ref.test (ref $closure) (local.get $x))
          (i32.or
            (ref.test (ref $primitive) (local.get $x))
            (ref.test (ref $interpreted_closure) (local.get $x))))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_car (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $x anyref)
    (local.set $x (call $car_safe (local.get $args)))
    (if (call $is_nil (local.get $x))
      (then (return (global.get $NIL))))
    (if (i32.eqz (ref.test (ref $cons) (local.get $x)))
      (then (return (call $make_error (i32.const 1) (local.get $x) (global.get $NIL)))))
    (struct.get $cons $car (ref.cast (ref $cons) (local.get $x))))

  (func $prim_cdr (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $x anyref)
    (local.set $x (call $car_safe (local.get $args)))
    (if (call $is_nil (local.get $x))
      (then (return (global.get $NIL))))
    (if (i32.eqz (ref.test (ref $cons) (local.get $x)))
      (then (return (call $make_error (i32.const 2) (local.get $x) (global.get $NIL)))))
    (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $x))))

  (func $prim_cons (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (call $make_cons
      (call $car_safe (local.get $args))
      (call $car_safe (call $cdr_safe (local.get $args)))))

  (func $prim_rplaca (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $cell anyref)
    (local $val anyref)
    (local.set $cell (call $car_safe (local.get $args)))
    (local.set $val (call $car_safe (call $cdr_safe (local.get $args))))
    (if (result anyref) (ref.test (ref $cons) (local.get $cell))
      (then
        (struct.set $cons $car (ref.cast (ref $cons) (local.get $cell)) (local.get $val))
        (local.get $cell))
      (else (call $make_error (i32.const 7) (local.get $cell) (global.get $NIL)))))

  (func $prim_rplacd (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $cell anyref)
    (local $val anyref)
    (local.set $cell (call $car_safe (local.get $args)))
    (local.set $val (call $car_safe (call $cdr_safe (local.get $args))))
    (if (result anyref) (ref.test (ref $cons) (local.get $cell))
      (then
        (struct.set $cons $cdr (ref.cast (ref $cons) (local.get $cell)) (local.get $val))
        (local.get $cell))
      (else (call $make_error (i32.const 7) (local.get $cell) (global.get $NIL)))))

  (func $prim_list (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local.get $args))

  (func $prim_length (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (call $make_fixnum_i32 (call $list_length (call $car_safe (local.get $args)))))

  (func $prim_eq (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref)
        (call $eq
          (call $car_safe (local.get $args))
          (call $car_safe (call $cdr_safe (local.get $args))))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_eql (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (call $prim_eq (local.get $env) (local.get $args)))

  (func $equalp (param $x anyref) (param $y anyref) (result i32)
    (if (call $eq (local.get $x) (local.get $y))
      (then (return (i32.const 1))))
    (if (i32.and
          (ref.test (ref $cons) (local.get $x))
          (ref.test (ref $cons) (local.get $y)))
      (then
        (return
          (i32.and
            (call $equalp
              (struct.get $cons $car (ref.cast (ref $cons) (local.get $x)))
              (struct.get $cons $car (ref.cast (ref $cons) (local.get $y))))
            (call $equalp
              (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $x)))
              (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $y))))))))
    (if (i32.and
          (ref.test (ref $string) (local.get $x))
          (ref.test (ref $string) (local.get $y)))
      (then
        (return
          (call $string_equal
            (ref.cast (ref $string) (local.get $x))
            (ref.cast (ref $string) (local.get $y))))))
    (i32.const 0))

  (func $prim_equal (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref)
        (call $equalp
          (call $car_safe (local.get $args))
          (call $car_safe (call $cdr_safe (local.get $args))))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_add (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $sum i32)
    (local $cur anyref)
    (local.set $sum (i32.const 0))
    (local.set $cur (local.get $args))
    (block $done
      (loop $loop
        (br_if $done (call $is_nil (local.get $cur)))
        (local.set $sum
          (i32.add
            (local.get $sum)
            (call $fixnum_value_i32
              (ref.cast (ref i31)
                (struct.get $cons $car (ref.cast (ref $cons) (local.get $cur)))))))
        (local.set $cur (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $cur))))
        (br $loop)))
    (call $make_fixnum_i32 (local.get $sum)))

  (func $prim_mul (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $prod i32)
    (local $cur anyref)
    (local.set $prod (i32.const 1))
    (local.set $cur (local.get $args))
    (block $done
      (loop $loop
        (br_if $done (call $is_nil (local.get $cur)))
        (local.set $prod
          (i32.mul
            (local.get $prod)
            (call $fixnum_value_i32
              (ref.cast (ref i31)
                (struct.get $cons $car (ref.cast (ref $cons) (local.get $cur)))))))
        (local.set $cur (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $cur))))
        (br $loop)))
    (call $make_fixnum_i32 (local.get $prod)))

  (func $prim_sub (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $cur anyref)
    (local $res i32)
    (local $rest anyref)

    (local.set $cur (local.get $args))
    (if (call $is_nil (local.get $cur))
      (then (return (call $make_fixnum_i32 (i32.const 0)))))

    (local.set $res
      (call $fixnum_value_i32
        (ref.cast (ref i31)
          (struct.get $cons $car (ref.cast (ref $cons) (local.get $cur))))))
    (local.set $rest (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $cur))))

    (if (call $is_nil (local.get $rest))
      (then (return (call $make_fixnum_i32 (i32.sub (i32.const 0) (local.get $res))))))

    (local.set $cur (local.get $rest))
    (block $done
      (loop $loop
        (br_if $done (call $is_nil (local.get $cur)))
        (local.set $res
          (i32.sub
            (local.get $res)
            (call $fixnum_value_i32
              (ref.cast (ref i31)
                (struct.get $cons $car (ref.cast (ref $cons) (local.get $cur)))))))
        (local.set $cur (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $cur))))
        (br $loop)))
    (call $make_fixnum_i32 (local.get $res)))

  (func $floor_div (param $a i32) (param $b i32) (result i32)
    (local $q i32)
    (local $r i32)
    (local.set $q (i32.div_s (local.get $a) (local.get $b)))
    (local.set $r (i32.rem_s (local.get $a) (local.get $b)))
    (if (i32.eqz (local.get $r))
      (then (return (local.get $q))))
    ;; If signs differ, adjust toward -inf
    (if (i32.xor (i32.lt_s (local.get $a) (i32.const 0)) (i32.lt_s (local.get $b) (i32.const 0)))
      (then (return (i32.sub (local.get $q) (i32.const 1)))))
    (local.get $q))

  (func $prim_div (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $cur anyref)
    (local $res i32)
    (local $rest anyref)

    (local.set $cur (local.get $args))
    (if (call $is_nil (local.get $cur))
      (then (return (call $make_error (i32.const 7) (global.get $NIL) (global.get $NIL)))))

    (local.set $res
      (call $fixnum_value_i32
        (ref.cast (ref i31)
          (struct.get $cons $car (ref.cast (ref $cons) (local.get $cur))))))
    (local.set $rest (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $cur))))

    ;; Unary: floor(1 / x)
    (if (call $is_nil (local.get $rest))
      (then (return (call $make_fixnum_i32 (call $floor_div (i32.const 1) (local.get $res))))))

    (local.set $cur (local.get $rest))
    (block $done
      (loop $loop
        (br_if $done (call $is_nil (local.get $cur)))
        (local.set $res
          (call $floor_div
            (local.get $res)
            (call $fixnum_value_i32
              (ref.cast (ref i31)
                (struct.get $cons $car (ref.cast (ref $cons) (local.get $cur)))))))
        (local.set $cur (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $cur))))
        (br $loop)))
    (call $make_fixnum_i32 (local.get $res)))

  (func $prim_lt (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $a i32)
    (local $b i32)
    (local.set $a (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (local.get $args)))))
    (local.set $b (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (call $cdr_safe (local.get $args))))))
    (if (result anyref) (i32.lt_s (local.get $a) (local.get $b))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_gt (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $a i32)
    (local $b i32)
    (local.set $a (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (local.get $args)))))
    (local.set $b (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (call $cdr_safe (local.get $args))))))
    (if (result anyref) (i32.gt_s (local.get $a) (local.get $b))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_le (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $a i32)
    (local $b i32)
    (local.set $a (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (local.get $args)))))
    (local.set $b (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (call $cdr_safe (local.get $args))))))
    (if (result anyref) (i32.le_s (local.get $a) (local.get $b))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_ge (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $a i32)
    (local $b i32)
    (local.set $a (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (local.get $args)))))
    (local.set $b (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (call $cdr_safe (local.get $args))))))
    (if (result anyref) (i32.ge_s (local.get $a) (local.get $b))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_numeq (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (local $a i32)
    (local $b i32)
    (local.set $a (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (local.get $args)))))
    (local.set $b (call $fixnum_value_i32 (ref.cast (ref i31) (call $car_safe (call $cdr_safe (local.get $args))))))
    (if (result anyref) (i32.eq (local.get $a) (local.get $b))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_not (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (if (result anyref) (call $is_nil (call $car_safe (local.get $args)))
      (then (global.get $T))
      (else (global.get $NIL))))

  (func $prim_print (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (call $car_safe (local.get $args)))

  ;; Forward declaration: evaluator apply
  (func $apply (param $func anyref) (param $args anyref) (result anyref)
    (local $p (ref $primitive))
    (local $c (ref $closure))
    (local $ic (ref $interpreted_closure))

    (if (ref.test (ref $primitive) (local.get $func))
      (then
        (local.set $p (ref.cast (ref $primitive) (local.get $func)))
        (return
          (call_ref $func_sig
            (global.get $NIL)
            (local.get $args)
            (struct.get $primitive $code (local.get $p))))))

    (if (ref.test (ref $closure) (local.get $func))
      (then
        (local.set $c (ref.cast (ref $closure) (local.get $func)))
        (return
          (call_ref $func_sig
            (struct.get $closure $env (local.get $c))
            (local.get $args)
            (struct.get $closure $code (local.get $c))))))

    (if (ref.test (ref $interpreted_closure) (local.get $func))
      (then
        (local.set $ic (ref.cast (ref $interpreted_closure) (local.get $func)))
        (return (call $apply_interpreted (local.get $ic) (local.get $args)))))

    (call $make_error (i32.const 7) (local.get $func) (global.get $NIL)))

  (func $prim_funcall (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (call $apply
      (call $car_safe (local.get $args))
      (call $cdr_safe (local.get $args))))

  (func $prim_apply (type $func_sig) (param $env anyref) (param $args anyref) (result anyref)
    (call $apply
      (call $car_safe (local.get $args))
      (call $car_safe (call $cdr_safe (local.get $args)))))

  ;; Declare functions that are referenced via `ref.func` (required by V8).
  (elem declare func
    $prim_null
    $prim_atom
    $prim_consp
    $prim_symbolp
    $prim_numberp
    $prim_stringp
    $prim_functionp
    $prim_car
    $prim_cdr
    $prim_cons
    $prim_rplaca
    $prim_rplacd
    $prim_list
    $prim_length
    $prim_eq
    $prim_eql
    $prim_equal
    $prim_add
    $prim_sub
    $prim_mul
    $prim_div
    $prim_lt
    $prim_gt
    $prim_le
    $prim_ge
    $prim_numeq
    $prim_not
    $prim_print
    $prim_funcall
    $prim_apply)

  ;; === Interpreter Initialization ===

  (func $init_interpreter
    (local $sym (ref $symbol))

    ;; Initialize trail
    (global.set $TRAIL (array.new $vector (global.get $NIL) (i32.const 256)))
    (global.set $TRAIL_SP (i32.const 0))

    ;; Keyword marker for specials (:SPECIAL)
    (global.set $KW_SPECIAL
      (call $intern_keyword
        (array.new_data $string $str_SPECIAL (i32.const 0) (i32.const 7))))

    ;; Special forms
    (global.set $SYM_QUOTE (call $intern_default (array.new_data $string $str_QUOTE (i32.const 0) (i32.const 5))))
    (global.set $SYM_IF (call $intern_default (array.new_data $string $str_IF (i32.const 0) (i32.const 2))))
    (global.set $SYM_PROGN (call $intern_default (array.new_data $string $str_PROGN (i32.const 0) (i32.const 5))))
    (global.set $SYM_SETQ (call $intern_default (array.new_data $string $str_SETQ (i32.const 0) (i32.const 4))))
    (global.set $SYM_FUNCTION (call $intern_default (array.new_data $string $str_FUNCTION (i32.const 0) (i32.const 8))))
    (global.set $SYM_LAMBDA (call $intern_default (array.new_data $string $str_LAMBDA (i32.const 0) (i32.const 6))))
    (global.set $SYM_LET (call $intern_default (array.new_data $string $str_LET (i32.const 0) (i32.const 3))))
    (global.set $SYM_LETSTAR (call $intern_default (array.new_data $string $str_LETSTAR (i32.const 0) (i32.const 4))))
    (global.set $SYM_DEFUN (call $intern_default (array.new_data $string $str_DEFUN (i32.const 0) (i32.const 5))))
    (global.set $SYM_DEFVAR (call $intern_default (array.new_data $string $str_DEFVAR (i32.const 0) (i32.const 6))))
    (global.set $SYM_BLOCK (call $intern_default (array.new_data $string $str_BLOCK (i32.const 0) (i32.const 5))))
    (global.set $SYM_RETURN_FROM (call $intern_default (array.new_data $string $str_RETURN_FROM (i32.const 0) (i32.const 11))))
    (global.set $SYM_CATCH (call $intern_default (array.new_data $string $str_CATCH (i32.const 0) (i32.const 5))))
    (global.set $SYM_THROW (call $intern_default (array.new_data $string $str_THROW (i32.const 0) (i32.const 5))))
    (global.set $SYM_UNWIND_PROTECT (call $intern_default (array.new_data $string $str_UNWIND_PROTECT (i32.const 0) (i32.const 14))))

    ;; Primitives (function cells)
    (local.set $sym (call $intern_default (array.new_data $string $str_NULL (i32.const 0) (i32.const 4))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_null) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_ATOM (i32.const 0) (i32.const 4))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_atom) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_CONSP (i32.const 0) (i32.const 5))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_consp) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_SYMBOLP (i32.const 0) (i32.const 7))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_symbolp) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_NUMBERP (i32.const 0) (i32.const 7))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_numberp) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_STRINGP (i32.const 0) (i32.const 7))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_stringp) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_FUNCTIONP (i32.const 0) (i32.const 9))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_functionp) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_CAR (i32.const 0) (i32.const 3))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_car) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_CDR (i32.const 0) (i32.const 3))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_cdr) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_CONS (i32.const 0) (i32.const 4))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_cons) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_RPLACA (i32.const 0) (i32.const 6))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_rplaca) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_RPLACD (i32.const 0) (i32.const 6))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_rplacd) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_LIST (i32.const 0) (i32.const 4))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_list) (struct.get $symbol $name (local.get $sym)) (i32.const -1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_LENGTH (i32.const 0) (i32.const 6))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_length) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_EQ (i32.const 0) (i32.const 2))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_eq) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_EQL (i32.const 0) (i32.const 3))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_eql) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_EQUAL (i32.const 0) (i32.const 5))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_equal) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_PLUS (i32.const 0) (i32.const 1))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_add) (struct.get $symbol $name (local.get $sym)) (i32.const -1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_MINUS (i32.const 0) (i32.const 1))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_sub) (struct.get $symbol $name (local.get $sym)) (i32.const -1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_STAR (i32.const 0) (i32.const 1))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_mul) (struct.get $symbol $name (local.get $sym)) (i32.const -1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_SLASH (i32.const 0) (i32.const 1))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_div) (struct.get $symbol $name (local.get $sym)) (i32.const -1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_LT (i32.const 0) (i32.const 1))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_lt) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_GT (i32.const 0) (i32.const 1))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_gt) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_LE (i32.const 0) (i32.const 2))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_le) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_GE (i32.const 0) (i32.const 2))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_ge) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_NUMEQ (i32.const 0) (i32.const 1))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_numeq) (struct.get $symbol $name (local.get $sym)) (i32.const 2)))

    (local.set $sym (call $intern_default (array.new_data $string $str_NOT (i32.const 0) (i32.const 3))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_not) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_PRINT (i32.const 0) (i32.const 5))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_print) (struct.get $symbol $name (local.get $sym)) (i32.const 1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_FUNCALL (i32.const 0) (i32.const 7))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_funcall) (struct.get $symbol $name (local.get $sym)) (i32.const -1)))

    (local.set $sym (call $intern_default (array.new_data $string $str_APPLY (i32.const 0) (i32.const 5))))
    (struct.set $symbol $function (local.get $sym) (struct.new $primitive (ref.func $prim_apply) (struct.get $symbol $name (local.get $sym)) (i32.const 2))))

  ;; === Evaluator Core ===

  (func $eval (param $expr anyref) (param $env anyref) (result anyref)
    (local $sym (ref $symbol))
    (if (ref.test (ref i31) (local.get $expr))
      (then (return (local.get $expr))))
    (if (ref.test (ref $string) (local.get $expr))
      (then (return (local.get $expr))))
    (if (call $is_nil (local.get $expr))
      (then (return (local.get $expr))))
    (if (ref.test (ref $symbol) (local.get $expr))
      (then
        (local.set $sym (ref.cast (ref $symbol) (local.get $expr)))
        (return (call $eval_symbol (local.get $sym) (local.get $env)))))
    (if (ref.test (ref $cons) (local.get $expr))
      (then (return (call $eval_list (ref.cast (ref $cons) (local.get $expr)) (local.get $env)))))
    (local.get $expr))

  (func $eval_symbol (param $sym (ref $symbol)) (param $env anyref) (result anyref)
    (local $pkg anyref)
    (local $val anyref)

    ;; T evaluates to itself
    (if (call $eq (local.get $sym) (global.get $T))
      (then (return (global.get $T))))

    ;; Keywords evaluate to themselves
    (local.set $pkg (struct.get $symbol $package (local.get $sym)))
    (if (i32.and
          (i32.eqz (call $is_nil (local.get $pkg)))
          (call $eq (local.get $pkg) (ref.as_non_null (global.get $KEYWORD))))
      (then (return (local.get $sym))))

    ;; Lexical lookup
    (local.set $val (call $env_lookup_symbol (local.get $env) (local.get $sym)))
    (if (i32.eqz (call $is_unbound (local.get $val)))
      (then (return (local.get $val))))

    ;; Global/special value cell
    (local.set $val (struct.get $symbol $value (local.get $sym)))
    (if (call $is_unbound (local.get $val))
      (then (return (call $make_error (i32.const 3) (local.get $sym) (global.get $NIL)))))
    (local.get $val))

  (func $eval_args (param $args anyref) (param $env anyref) (result anyref)
    (local $first anyref)
    (local $rest anyref)
    (local $res anyref)
    (if (call $is_nil (local.get $args))
      (then (return (global.get $NIL))))
    (local.set $first
      (call $eval
        (struct.get $cons $car (ref.cast (ref $cons) (local.get $args)))
        (local.get $env)))
    (if (call $controlp (local.get $first))
      (then (return (local.get $first))))
    (local.set $rest
      (call $eval_args
        (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $args)))
        (local.get $env)))
    (if (call $controlp (local.get $rest))
      (then (return (local.get $rest))))
    (local.set $res (call $make_cons (local.get $first) (local.get $rest)))
    (local.get $res))

  (func $eval_progn (param $forms anyref) (param $env anyref) (result anyref)
    (local $cur anyref)
    (local $result anyref)
    (local.set $cur (local.get $forms))
    (local.set $result (global.get $NIL))
    (block $done
      (loop $loop
        (br_if $done (call $is_nil (local.get $cur)))
        (local.set $result
          (call $eval
            (struct.get $cons $car (ref.cast (ref $cons) (local.get $cur)))
            (local.get $env)))
        (if (call $controlp (local.get $result))
          (then (return (local.get $result))))
        (local.set $cur (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $cur))))
        (br $loop)))
    (local.get $result))

  (func $eval_quote (param $args anyref) (param $env anyref) (result anyref)
    (call $car_safe (local.get $args)))

  (func $eval_if (param $args anyref) (param $env anyref) (result anyref)
    (local $test anyref)
    (local $then_form anyref)
    (local $else_forms anyref)
    (local $val anyref)

    (local.set $test (call $eval (call $car_safe (local.get $args)) (local.get $env)))
    (if (call $controlp (local.get $test))
      (then (return (local.get $test))))

    (local.set $then_form (call $car_safe (call $cdr_safe (local.get $args))))
    (local.set $else_forms (call $cdr_safe (call $cdr_safe (local.get $args))))

    (if (call $is_nil (local.get $test))
      (then
        (if (call $is_nil (local.get $else_forms))
          (then (return (global.get $NIL))))
        (local.set $val (call $eval (call $car_safe (local.get $else_forms)) (local.get $env)))
        (return (local.get $val))))

    (local.set $val (call $eval (local.get $then_form) (local.get $env)))
    (local.get $val))

  (func $eval_setq (param $args anyref) (param $env anyref) (result anyref)
    (local $pairs anyref)
    (local $sym (ref $symbol))
    (local $val anyref)
    (local $result anyref)
    (local $found i32)

    (local.set $pairs (local.get $args))
    (local.set $result (global.get $NIL))

    (block $done
      (loop $loop
        (br_if $done (call $is_nil (local.get $pairs)))

        (local.set $sym (ref.cast (ref $symbol) (call $car_safe (local.get $pairs))))
        (local.set $pairs (call $cdr_safe (local.get $pairs)))

        (if (call $is_nil (local.get $pairs))
          (then (return (call $make_error (i32.const 5) (global.get $NIL) (global.get $NIL)))))

        (local.set $val (call $eval (call $car_safe (local.get $pairs)) (local.get $env)))
        (if (call $controlp (local.get $val))
          (then (return (local.get $val))))
        (local.set $pairs (call $cdr_safe (local.get $pairs)))

        (local.set $found (call $env_set_symbol (local.get $env) (local.get $sym) (local.get $val)))
        (if (i32.eqz (local.get $found))
          (then (struct.set $symbol $value (local.get $sym) (local.get $val))))

        (local.set $result (local.get $val))
        (br $loop)))
    (local.get $result))

  (func $eval_lambda (param $args anyref) (param $env anyref) (result anyref)
    (local $params anyref)
    (local $body anyref)
    (local $arity i32)
    (local.set $params (call $car_safe (local.get $args)))
    (local.set $body (call $cdr_safe (local.get $args)))
    (local.set $arity (call $list_length (local.get $params)))
    (struct.new $interpreted_closure
      (local.get $params)
      (local.get $body)
      (local.get $env)
      (global.get $NIL)
      (local.get $arity)))

  (func $apply_interpreted (param $c (ref $interpreted_closure)) (param $args anyref) (result anyref)
    (local $params anyref)
    (local $arity i32)
    (local $new_env (ref $env_frame))
    (local $bindings (ref $vector))
    (local $i i32)
    (local $pcur anyref)
    (local $acur anyref)
    (local $sym anyref)
    (local $val anyref)

    (local.set $params (struct.get $interpreted_closure $params (local.get $c)))
    (local.set $arity (struct.get $interpreted_closure $arity (local.get $c)))
    (local.set $new_env
      (struct.new $env_frame
        (array.new $vector (global.get $UNBOUND) (i32.mul (local.get $arity) (i32.const 2)))
        (struct.get $interpreted_closure $env (local.get $c))))
    (local.set $bindings (struct.get $env_frame $bindings (local.get $new_env)))
    (local.set $i (i32.const 0))
    (local.set $pcur (local.get $params))
    (local.set $acur (local.get $args))

    (block $done
      (loop $loop
        (br_if $done (i32.ge_u (local.get $i) (local.get $arity)))
        (local.set $sym (call $car_safe (local.get $pcur)))
        (if (call $is_nil (local.get $acur))
          (then (local.set $val (global.get $NIL)))
          (else (local.set $val (call $car_safe (local.get $acur)))))
        (array.set $vector (local.get $bindings) (i32.mul (local.get $i) (i32.const 2)) (local.get $sym))
        (array.set $vector (local.get $bindings) (i32.add (i32.mul (local.get $i) (i32.const 2)) (i32.const 1)) (local.get $val))
        (local.set $pcur (call $cdr_safe (local.get $pcur)))
        (if (call $is_nil (local.get $acur))
          (then)
          (else (local.set $acur (call $cdr_safe (local.get $acur)))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)))
    (call $eval_progn (struct.get $interpreted_closure $body (local.get $c)) (local.get $new_env)))

  (func $eval_let (param $args anyref) (param $env anyref) (result anyref)
    (local $bindings_list anyref)
    (local $body anyref)
    (local $cur anyref)
    (local $binding anyref)
    (local $sym (ref null $symbol))
    (local $val anyref)
    (local $val_form anyref)
    (local $lex_pairs anyref)
    (local $special_pairs anyref)
    (local $lex_count i32)
    (local $new_env anyref)
    (local $frame (ref null $env_frame))
    (local $vec (ref null $vector))
    (local $i i32)
    (local $pair anyref)
    (local $sp0 i32)
    (local $result anyref)

    (local.set $bindings_list (call $car_safe (local.get $args)))
    (local.set $body (call $cdr_safe (local.get $args)))
    (local.set $cur (local.get $bindings_list))
    (local.set $lex_pairs (global.get $NIL))
    (local.set $special_pairs (global.get $NIL))
    (local.set $lex_count (i32.const 0))

    ;; Evaluate all init forms in original env
    (block $bind_done
      (loop $bind_loop
        (br_if $bind_done (call $is_nil (local.get $cur)))
        (local.set $binding (call $car_safe (local.get $cur)))
        (local.set $cur (call $cdr_safe (local.get $cur)))

        (if (ref.test (ref $symbol) (local.get $binding))
          (then
            (local.set $sym (ref.cast (ref $symbol) (local.get $binding)))
            (local.set $val (global.get $NIL)))
          (else
            (local.set $sym (ref.cast (ref $symbol) (call $car_safe (local.get $binding))))
            (local.set $val_form (call $car_safe (call $cdr_safe (local.get $binding))))
            (local.set $val (call $eval (local.get $val_form) (local.get $env)))
            (if (call $controlp (local.get $val))
              (then (return (local.get $val))))))

        (if (call $symbol_special_p (ref.as_non_null (local.get $sym)))
          (then
            (local.set $special_pairs
              (call $make_cons
                (call $make_cons (local.get $sym) (local.get $val))
                (local.get $special_pairs))))
          (else
            (local.set $lex_pairs
              (call $make_cons
                (call $make_cons (local.get $sym) (local.get $val))
                (local.get $lex_pairs)))
            (local.set $lex_count (i32.add (local.get $lex_count) (i32.const 1)))))
        (br $bind_loop)))

    ;; Create lexical frame if needed
    (local.set $new_env (local.get $env))
    (if (i32.gt_u (local.get $lex_count) (i32.const 0))
      (then
        (local.set $frame
          (struct.new $env_frame
            (array.new $vector (global.get $UNBOUND) (i32.mul (local.get $lex_count) (i32.const 2)))
            (local.get $env)))
        (local.set $vec (struct.get $env_frame $bindings (ref.as_non_null (local.get $frame))))
        (local.set $i (i32.const 0))
        (local.set $cur (local.get $lex_pairs))
        (block $fill_done
          (loop $fill
            (br_if $fill_done (call $is_nil (local.get $cur)))
            (local.set $pair (call $car_safe (local.get $cur)))
            (array.set $vector (ref.as_non_null (local.get $vec)) (i32.mul (local.get $i) (i32.const 2))
              (struct.get $cons $car (ref.cast (ref $cons) (local.get $pair))))
            (array.set $vector (ref.as_non_null (local.get $vec)) (i32.add (i32.mul (local.get $i) (i32.const 2)) (i32.const 1))
              (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $pair))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (local.set $cur (call $cdr_safe (local.get $cur)))
            (br $fill)))
        (local.set $new_env (local.get $frame))))

    ;; Bind specials dynamically (after all init forms)
    (local.set $sp0 (global.get $TRAIL_SP))
    (local.set $cur (local.get $special_pairs))
    (block $special_done
      (loop $special_loop
        (br_if $special_done (call $is_nil (local.get $cur)))
        (local.set $pair (call $car_safe (local.get $cur)))
        (call $bind_special
          (ref.cast (ref $symbol) (struct.get $cons $car (ref.cast (ref $cons) (local.get $pair))))
          (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $pair))))
        (local.set $cur (call $cdr_safe (local.get $cur)))
        (br $special_loop)))

    (local.set $result (call $eval_progn (local.get $body) (local.get $new_env)))
    (call $trail_restore_to (local.get $sp0))
    (local.get $result))

  (func $eval_letstar (param $args anyref) (param $env anyref) (result anyref)
    (local $bindings_list anyref)
    (local $body anyref)
    (local $cur anyref)
    (local $binding anyref)
    (local $sym (ref null $symbol))
    (local $val anyref)
    (local $val_form anyref)
    (local $current_env anyref)
    (local $new_frame (ref $env_frame))
    (local $vec (ref $vector))
    (local $sp0 i32)
    (local $result anyref)

    (local.set $bindings_list (call $car_safe (local.get $args)))
    (local.set $body (call $cdr_safe (local.get $args)))
    (local.set $current_env (local.get $env))
    (local.set $cur (local.get $bindings_list))
    (local.set $sp0 (global.get $TRAIL_SP))

    (block $bind_done
      (loop $bind_loop
        (br_if $bind_done (call $is_nil (local.get $cur)))
        (local.set $binding (call $car_safe (local.get $cur)))
        (local.set $cur (call $cdr_safe (local.get $cur)))

        (if (ref.test (ref $symbol) (local.get $binding))
          (then
            (local.set $sym (ref.cast (ref $symbol) (local.get $binding)))
            (local.set $val (global.get $NIL)))
          (else
            (local.set $sym (ref.cast (ref $symbol) (call $car_safe (local.get $binding))))
            (local.set $val_form (call $car_safe (call $cdr_safe (local.get $binding))))
            (local.set $val (call $eval (local.get $val_form) (local.get $current_env)))
            (if (call $controlp (local.get $val))
              (then
                (call $trail_restore_to (local.get $sp0))
                (return (local.get $val))))))

        (if (call $symbol_special_p (ref.as_non_null (local.get $sym)))
          (then
            (call $bind_special (ref.as_non_null (local.get $sym)) (local.get $val)))
          (else
            (local.set $new_frame
              (struct.new $env_frame
                (array.new $vector (global.get $UNBOUND) (i32.const 2))
                (local.get $current_env)))
            (local.set $vec (struct.get $env_frame $bindings (local.get $new_frame)))
            (array.set $vector (local.get $vec) (i32.const 0) (local.get $sym))
            (array.set $vector (local.get $vec) (i32.const 1) (local.get $val))
            (local.set $current_env (local.get $new_frame))))

        (br $bind_loop)))

    (local.set $result (call $eval_progn (local.get $body) (local.get $current_env)))
    (call $trail_restore_to (local.get $sp0))
    (local.get $result))

  (func $eval_defun (param $args anyref) (param $env anyref) (result anyref)
    (local $name (ref $symbol))
    (local $params anyref)
    (local $body anyref)
    (local $arity i32)
    (local $closure (ref $interpreted_closure))

    (local.set $name (ref.cast (ref $symbol) (call $car_safe (local.get $args))))
    (local.set $params (call $car_safe (call $cdr_safe (local.get $args))))
    (local.set $body (call $cdr_safe (call $cdr_safe (local.get $args))))
    (local.set $arity (call $list_length (local.get $params)))
    (local.set $closure
      (struct.new $interpreted_closure
        (local.get $params)
        (local.get $body)
        (global.get $NIL)
        (global.get $NIL)
        (local.get $arity)))
    (struct.set $symbol $function (local.get $name) (local.get $closure))
    (local.get $name))

  (func $eval_defvar (param $args anyref) (param $env anyref) (result anyref)
    (local $name (ref $symbol))
    (local $rest anyref)
    (local $val anyref)

    (local.set $name (ref.cast (ref $symbol) (call $car_safe (local.get $args))))
    (call $declare_special (local.get $name))

    (if (call $is_unbound (struct.get $symbol $value (local.get $name)))
      (then
        (local.set $rest (call $cdr_safe (local.get $args)))
        (if (call $is_nil (local.get $rest))
          (then
            (struct.set $symbol $value (local.get $name) (global.get $NIL)))
          (else
            (local.set $val (call $eval (call $car_safe (local.get $rest)) (local.get $env)))
            (if (call $controlp (local.get $val))
              (then (return (local.get $val))))
            (struct.set $symbol $value (local.get $name) (local.get $val))))))
    (local.get $name))

  (func $eval_function (param $args anyref) (param $env anyref) (result anyref)
    (local $spec anyref)
    (local $head anyref)
    (local $func anyref)

    (local.set $spec (call $car_safe (local.get $args)))

    (if (ref.test (ref $symbol) (local.get $spec))
      (then
        (local.set $func (struct.get $symbol $function (ref.cast (ref $symbol) (local.get $spec))))
        (if (i32.or
              (ref.is_null (local.get $func))
              (i32.or
                (call $is_nil (local.get $func))
                (call $is_unbound (local.get $func))))
          (then (return (call $make_error (i32.const 4) (local.get $spec) (global.get $NIL)))))
        (return (local.get $func))))

    ;; Lambda expression: (function (lambda (...) ...))
    (if (ref.test (ref $cons) (local.get $spec))
      (then
        (local.set $head (call $car_safe (local.get $spec)))
        (if (i32.and
              (ref.test (ref $symbol) (local.get $head))
              (call $eq (local.get $head) (ref.as_non_null (global.get $SYM_LAMBDA))))
          (then
            (return (call $eval_lambda (call $cdr_safe (local.get $spec)) (local.get $env)))))))

    (call $make_error (i32.const 6) (local.get $spec) (global.get $NIL)))

  (func $eval_block (param $args anyref) (param $env anyref) (result anyref)
    (local $name anyref)
    (local $body anyref)
    (local $res anyref)
    (local $ctrl (ref $control))
    (local.set $name (call $car_safe (local.get $args)))
    (local.set $body (call $cdr_safe (local.get $args)))
    (local.set $res (call $eval_progn (local.get $body) (local.get $env)))
    (if (call $controlp (local.get $res))
      (then
        (local.set $ctrl (ref.cast (ref $control) (local.get $res)))
        (if (i32.and
              (i32.eq (struct.get $control $kind (local.get $ctrl)) (i32.const 1))
              (call $eq (struct.get $control $a (local.get $ctrl)) (local.get $name)))
          (then (return (struct.get $control $b (local.get $ctrl)))))
        (return (local.get $res))))
    (local.get $res))

  (func $eval_return_from (param $args anyref) (param $env anyref) (result anyref)
    (local $name anyref)
    (local $rest anyref)
    (local $val anyref)
    (local.set $name (call $car_safe (local.get $args)))
    (local.set $rest (call $cdr_safe (local.get $args)))
    (if (call $is_nil (local.get $rest))
      (then (local.set $val (global.get $NIL)))
      (else
        (local.set $val (call $eval (call $car_safe (local.get $rest)) (local.get $env)))
        (if (call $controlp (local.get $val))
          (then (return (local.get $val))))))
    (call $make_return_from (local.get $name) (local.get $val)))

  (func $eval_catch (param $args anyref) (param $env anyref) (result anyref)
    (local $tag anyref)
    (local $body anyref)
    (local $res anyref)
    (local $ctrl (ref $control))
    (local.set $tag (call $eval (call $car_safe (local.get $args)) (local.get $env)))
    (if (call $controlp (local.get $tag))
      (then (return (local.get $tag))))
    (local.set $body (call $cdr_safe (local.get $args)))
    (local.set $res (call $eval_progn (local.get $body) (local.get $env)))
    (if (call $controlp (local.get $res))
      (then
        (local.set $ctrl (ref.cast (ref $control) (local.get $res)))
        (if (i32.and
              (i32.eq (struct.get $control $kind (local.get $ctrl)) (i32.const 2))
              (call $eq (struct.get $control $a (local.get $ctrl)) (local.get $tag)))
          (then (return (struct.get $control $b (local.get $ctrl)))))
        (return (local.get $res))))
    (local.get $res))

  (func $eval_throw (param $args anyref) (param $env anyref) (result anyref)
    (local $tag anyref)
    (local $val anyref)
    (local $rest anyref)
    (local.set $tag (call $eval (call $car_safe (local.get $args)) (local.get $env)))
    (if (call $controlp (local.get $tag))
      (then (return (local.get $tag))))
    (local.set $rest (call $cdr_safe (local.get $args)))
    (local.set $val (call $eval (call $car_safe (local.get $rest)) (local.get $env)))
    (if (call $controlp (local.get $val))
      (then (return (local.get $val))))
    (call $make_throw (local.get $tag) (local.get $val)))

  (func $eval_unwind_protect (param $args anyref) (param $env anyref) (result anyref)
    (local $protected anyref)
    (local $cleanup anyref)
    (local $res anyref)
    (local $cleanup_res anyref)

    (local.set $protected (call $car_safe (local.get $args)))
    (local.set $cleanup (call $cdr_safe (local.get $args)))

    (local.set $res (call $eval (local.get $protected) (local.get $env)))
    (local.set $cleanup_res (call $eval_progn (local.get $cleanup) (local.get $env)))

    ;; Cleanup non-local exit overrides protected
    (if (call $controlp (local.get $cleanup_res))
      (then (return (local.get $cleanup_res))))
    (local.get $res))

  (func $eval_list (param $list (ref $cons)) (param $env anyref) (result anyref)
    (local $head anyref)
    (local $args anyref)
    (local $sym_head (ref $symbol))

    (local.set $head (struct.get $cons $car (local.get $list)))
    (local.set $args (struct.get $cons $cdr (local.get $list)))

    (if (ref.test (ref $symbol) (local.get $head))
      (then
        (local.set $sym_head (ref.cast (ref $symbol) (local.get $head)))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_QUOTE)))
          (then (return (call $eval_quote (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_IF)))
          (then (return (call $eval_if (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_PROGN)))
          (then (return (call $eval_progn (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_SETQ)))
          (then (return (call $eval_setq (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_LAMBDA)))
          (then (return (call $eval_lambda (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_LET)))
          (then (return (call $eval_let (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_LETSTAR)))
          (then (return (call $eval_letstar (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_DEFUN)))
          (then (return (call $eval_defun (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_DEFVAR)))
          (then (return (call $eval_defvar (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_FUNCTION)))
          (then (return (call $eval_function (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_BLOCK)))
          (then (return (call $eval_block (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_RETURN_FROM)))
          (then (return (call $eval_return_from (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_CATCH)))
          (then (return (call $eval_catch (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_THROW)))
          (then (return (call $eval_throw (local.get $args) (local.get $env)))))
        (if (call $eq (local.get $sym_head) (ref.as_non_null (global.get $SYM_UNWIND_PROTECT)))
          (then (return (call $eval_unwind_protect (local.get $args) (local.get $env)))))))

    (call $eval_function_call (local.get $head) (local.get $args) (local.get $env)))

  (func $eval_function_call (param $head anyref) (param $args anyref) (param $env anyref) (result anyref)
    (local $func anyref)
    (local $evaled_args anyref)
    (local $sym (ref $symbol))

    ;; Resolve callee
    (if (ref.test (ref $symbol) (local.get $head))
      (then
        (local.set $sym (ref.cast (ref $symbol) (local.get $head)))
        (local.set $func (struct.get $symbol $function (local.get $sym)))
        (if (i32.or
              (ref.is_null (local.get $func))
              (i32.or
                (call $is_nil (local.get $func))
                (call $is_unbound (local.get $func))))
          (then (return (call $make_error (i32.const 4) (local.get $sym) (global.get $NIL))))))
      (else
        (if (ref.test (ref $cons) (local.get $head))
          (then
            (local.set $func (call $eval (local.get $head) (local.get $env)))
            (if (call $controlp (local.get $func))
              (then (return (local.get $func)))))
          (else
            (local.set $func (local.get $head))))))

    (local.set $evaled_args (call $eval_args (local.get $args) (local.get $env)))
    (if (call $controlp (local.get $evaled_args))
      (then (return (local.get $evaled_args))))
    (call $apply (local.get $func) (local.get $evaled_args)))

  ;; === Exports ===

  (func (export "interpreted_closurep") (param $x anyref) (result i32)
    (ref.test (ref $interpreted_closure) (local.get $x)))

  (func (export "eval") (param $expr anyref) (result anyref)
    (local $res anyref)
    (local $ctrl (ref $control))
    (local $kind i32)

    (local.set $res (call $eval (local.get $expr) (global.get $NIL)))
    (if (call $controlp (local.get $res))
      (then
        (local.set $ctrl (ref.cast (ref $control) (local.get $res)))
        (local.set $kind (struct.get $control $kind (local.get $ctrl)))
        (if (i32.eq (local.get $kind) (i32.const 3))
          (then
            (call $raise_error
              (struct.get $control $code (local.get $ctrl))
              (struct.get $control $a (local.get $ctrl))
              (struct.get $control $b (local.get $ctrl)))
            (unreachable)))
        (if (i32.eq (local.get $kind) (i32.const 1))
          (then
            (call $raise_error (i32.const 8) (struct.get $control $a (local.get $ctrl)) (struct.get $control $b (local.get $ctrl)))
            (unreachable)))
        (if (i32.eq (local.get $kind) (i32.const 2))
          (then
            (call $raise_error (i32.const 9) (struct.get $control $a (local.get $ctrl)) (struct.get $control $b (local.get $ctrl)))
            (unreachable)))
        (call $raise_error (i32.const 0) (struct.get $control $a (local.get $ctrl)) (struct.get $control $b (local.get $ctrl)))
        (unreachable)))
    (local.get $res))
)
