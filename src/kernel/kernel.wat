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

  ;; Check if value is any kind of function (closure or primitive)
  (func (export "functionp") (param $x anyref) (result i32)
    (i32.or
      (ref.test (ref $closure) (local.get $x))
      (ref.test (ref $primitive) (local.get $x))))

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
)
