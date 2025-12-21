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

  ;; === Global Special Objects ===

  ;; Unbound marker singleton
  (global $UNBOUND (ref $unbound)
    (struct.new $unbound))

  ;; NIL symbol (initialized in $init_globals)
  (global $NIL (mut (ref null $symbol)) (ref.null $symbol))

  ;; T symbol (initialized in $init_globals)
  (global $T (mut (ref null $symbol)) (ref.null $symbol))

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

  ;; === Initialization ===

  ;; Initialize NIL and T at module start
  (func $init_globals
    (local $nil_name (ref $string))
    (local $t_name (ref $string))

    ;; Create "NIL" string
    (local.set $nil_name (call $make_string_3
      (i32.const 78)   ;; 'N'
      (i32.const 73)   ;; 'I'
      (i32.const 76))) ;; 'L'

    ;; Create "T" string
    (local.set $t_name (call $make_string_1
      (i32.const 84))) ;; 'T'

    ;; Create NIL symbol
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
)
