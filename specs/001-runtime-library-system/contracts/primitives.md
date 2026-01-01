# Primitives Contract: Runtime Library System

**Feature**: 001-runtime-library-system
**Date**: 2026-01-01
**Status**: Draft

## Layer 1 Primitives Interface

### Memory Primitives

| Primitive | Signature | Wasm Instructions | Description |
|-----------|-----------|-------------------|-------------|
| [cons](resources/HyperSpec/Body/f_cons.htm) | `(car cdr) -> cons` | `struct.new $cons` | Create cons cell |
| [car](resources/HyperSpec/Body/f_car_c.htm) | `(cons) -> any` | `struct.get $cons 0` | Get car of cons |
| [cdr](resources/HyperSpec/Body/f_car_c.htm) | `(cons) -> any` | `struct.get $cons 1` | Get cdr of cons |
| [rplaca](resources/HyperSpec/Body/f_rplaca.htm) | `(cons value) -> cons` | `struct.set $cons 0` | Destructive car update |
| [rplacd](resources/HyperSpec/Body/f_rplaca.htm) | `(cons value) -> cons` | `struct.set $cons 1` | Destructive cdr update |

### Type Predicates

| Primitive | Signature | Wasm Instructions | Description |
|-----------|-----------|-------------------|-------------|
| [consp](resources/HyperSpec/Body/f_consp.htm) | `(obj) -> boolean` | `ref.test $cons` | Test for cons cell |
| [null](resources/HyperSpec/Body/f_null.htm) | `(obj) -> boolean` | `ref.eq $nil` | Test for NIL |
| [numberp](resources/HyperSpec/Body/f_nump.htm) | `(obj) -> boolean` | `ref.test i31` + float/ratio | Test for number |
| [stringp](resources/HyperSpec/Body/f_stgp.htm) | `(obj) -> boolean` | `ref.test $string` | Test for string |
| [symbolp](resources/HyperSpec/Body/f_symbol.htm) | `(obj) -> boolean` | `ref.test $symbol` | Test for symbol |

### Arithmetic Primitives

| Primitive | Signature | Wasm Instructions | Description |
|-----------|-----------|-------------------|-------------|
| [+](resources/HyperSpec/Body/f_pl.htm) | `(&rest numbers) -> number` | `i32.add` / `f64.add` | Addition |
| [-](resources/HyperSpec/Body/f__.htm) | `(number &rest numbers) -> number` | `i32.sub` / `f64.sub` | Subtraction |
| [*](resources/HyperSpec/Body/f_st.htm) | `(&rest numbers) -> number` | `i32.mul` / `f64.mul` | Multiplication |
| [/](resources/HyperSpec/Body/f_sl.htm) | `(number &rest numbers) -> number` | `i32.div_s` / `f64.div` | Division |
| [mod](resources/HyperSpec/Body/f_mod_r.htm) | `(number divisor) -> number` | `i32.rem_s` | Modulus |
| [<](resources/HyperSpec/Body/f_eq_sle.htm) | `(a b) -> boolean` | `i32.lt_s` / `f64.lt` | Less than |
| [>](resources/HyperSpec/Body/f_eq_sle.htm) | `(a b) -> boolean` | `i32.gt_s` / `f64.gt` | Greater than |
| [=](resources/HyperSpec/Body/f_eq_sle.htm) | `(a b) -> boolean` | `i32.eq` / `f64.eq` | Numeric equality |
| [<=](resources/HyperSpec/Body/f_eq_sle.htm) | `(a b) -> boolean` | `i32.le_s` / `f64.le` | Less or equal |
| [>=](resources/HyperSpec/Body/f_eq_sle.htm) | `(a b) -> boolean` | `i32.ge_s` / `f64.ge` | Greater or equal |

### FFI Host Calls

| Primitive | Signature | Host Module | Description |
|-----------|-----------|-------------|-------------|
| %host-write-char | `(fd codepoint) -> void` | `clysm:io.write-char` | Write Unicode char |
| %host-write-string | `(fd string) -> void` | `clysm:io.write-string` | Write string |
| %host-read-char | `(fd) -> fixnum` | `clysm:io.read-char` | Read Unicode char (-1 on EOF) |
| %host-read-line | `(fd) -> string/null` | `clysm:io.read-line` | Read line (null on EOF) |

## Layer 2 Runtime Library Interface

### I/O Functions (io.lisp)

| Function | Signature | Dependencies | HyperSpec |
|----------|-----------|--------------|-----------|
| terpri | `(&optional stream) -> nil` | %host-write-char | [f_terpri.htm](resources/HyperSpec/Body/f_terpri.htm) |
| write-char | `(char &optional stream) -> char` | %host-write-char | [f_wr_cha.htm](resources/HyperSpec/Body/f_wr_cha.htm) |
| write-string | `(string &optional stream &key start end) -> string` | %host-write-string | [f_wr_stg.htm](resources/HyperSpec/Body/f_wr_stg.htm) |
| princ | `(object &optional stream) -> object` | write-string, write-char, type predicates | [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm) |
| prin1 | `(object &optional stream) -> object` | princ, write-char | [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm) |
| print | `(object &optional stream) -> object` | prin1, terpri, write-char | [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm) |
| write | `(object &key stream ...) -> object` | prin1, princ | [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm) |
| format | `(destination control-string &rest args) -> string/nil` | write, princ | [f_format.htm](resources/HyperSpec/Body/f_format.htm) |

### List Operations (list-ops.lisp)

| Function | Signature | Dependencies | HyperSpec |
|----------|-----------|--------------|-----------|
| assoc | `(item alist &key key test) -> cons/nil` | car, cdr, consp | [f_assocc.htm](resources/HyperSpec/Body/f_assocc.htm) |
| member | `(item list &key key test) -> tail/nil` | car, cdr, consp | [f_mem_m.htm](resources/HyperSpec/Body/f_mem_m.htm) |
| find | `(item sequence &key key test start end) -> item/nil` | car, cdr, consp | [f_find_.htm](resources/HyperSpec/Body/f_find_.htm) |
| position | `(item sequence &key key test start end) -> index/nil` | car, cdr, consp, + | [f_pos_p.htm](resources/HyperSpec/Body/f_pos_p.htm) |
| nth | `(n list) -> element` | car, cdr | [f_nth.htm](resources/HyperSpec/Body/f_nth.htm) |
| nthcdr | `(n list) -> tail` | cdr | [f_nthcdr.htm](resources/HyperSpec/Body/f_nthcdr.htm) |

### Sequence Operations (sequences.lisp)

| Function | Signature | Dependencies | HyperSpec |
|----------|-----------|--------------|-----------|
| remove | `(item sequence &key key test start end count) -> sequence` | cons, car, cdr, consp | [f_rm_rm.htm](resources/HyperSpec/Body/f_rm_rm.htm) |
| remove-if | `(predicate sequence &key key start end count) -> sequence` | remove | [f_rm_rm.htm](resources/HyperSpec/Body/f_rm_rm.htm) |
| count | `(item sequence &key key test start end) -> integer` | car, cdr, consp, + | [f_countc.htm](resources/HyperSpec/Body/f_countc.htm) |
| count-if | `(predicate sequence &key key start end) -> integer` | count | [f_countc.htm](resources/HyperSpec/Body/f_countc.htm) |
| substitute | `(newitem olditem sequence &key key test start end count) -> sequence` | cons, car, cdr, consp | [f_sbs_s.htm](resources/HyperSpec/Body/f_sbs_s.htm) |

### String Operations (strings.lisp)

| Function | Signature | Dependencies | HyperSpec |
|----------|-----------|--------------|-----------|
| princ-to-string | `(object) -> string` | princ (to string stream) | [f_wr_to_.htm](resources/HyperSpec/Body/f_wr_to_.htm) |
| prin1-to-string | `(object) -> string` | prin1 (to string stream) | [f_wr_to_.htm](resources/HyperSpec/Body/f_wr_to_.htm) |
| write-to-string | `(object &key ...) -> string` | write (to string stream) | [f_wr_to_.htm](resources/HyperSpec/Body/f_wr_to_.htm) |

## Contract Tests

### Primitive Registration Tests

```lisp
;; T001: All memory primitives registered
(deftest primitives-memory-registered ()
  (testing "Memory primitives exist in registry"
    (ok (primitive-p 'cons))
    (ok (primitive-p 'car))
    (ok (primitive-p 'cdr))
    (ok (primitive-p 'rplaca))
    (ok (primitive-p 'rplacd))))

;; T002: All predicates registered
(deftest primitives-predicates-registered ()
  (testing "Type predicates exist in registry"
    (ok (primitive-p 'consp))
    (ok (primitive-p 'null))
    (ok (primitive-p 'numberp))
    (ok (primitive-p 'stringp))
    (ok (primitive-p 'symbolp))))
```

### Runtime Compilation Tests

```lisp
;; T003: Runtime function compiles to valid Wasm
(deftest runtime-compile-valid ()
  (testing "Runtime I/O functions compile"
    (let ((wasm (compile-runtime-module :io)))
      (ok (wasm-valid-p wasm)))))

;; T004: Undefined primitive detection
(deftest runtime-undefined-primitive-error ()
  (testing "Undefined primitive in runtime body signals error"
    (ok (signals 'undefined-primitive-error
          (compile-runtime-function
           '(defun bad-fn () (%undefined-op 1 2)))))))
```

### Migration Parity Tests

```lisp
;; T005: princ behavioral parity
(deftest princ-parity ()
  (testing "Runtime princ matches old codegen princ"
    (let ((old (with-old-codegen (princ-output "hello")))
          (new (with-runtime-library (princ-output "hello"))))
      (ok (string= old new)))))
```
