;;;; runtime/printer.lisp - Wasm Printer Functions
;;;;
;;;; Generates Wasm functions for printing Lisp objects.
;;;; Used by the REPL to display evaluation results.

(in-package #:clysm)

;;; ============================================================
;;; Printer Function Registry
;;; ============================================================

(defstruct (printer-registry (:constructor %make-printer-registry))
  "Registry of printer function indices."
  ;; Helper functions
  (string-append nil :type (or null integer))
  (count-digits nil :type (or null integer))
  (fixnum-to-string nil :type (or null integer))
  ;; Type-specific printers
  (print-fixnum nil :type (or null integer))
  (print-nil nil :type (or null integer))
  (print-cons nil :type (or null integer))
  (print-cdr-list nil :type (or null integer))
  (print-symbol nil :type (or null integer))
  (print-character nil :type (or null integer))
  ;; Main dispatcher
  (prin1-to-string nil :type (or null integer))
  ;; Result storage (for REPL)
  (result-global nil :type (or null integer))
  (get-result-char nil :type (or null integer)))

(defun make-printer-registry ()
  "Create an empty printer registry."
  (%make-printer-registry))

;;; ============================================================
;;; String Append Function
;;; ============================================================

(defun gen-string-append-body (registry)
  "Generate body for $string-append function.
Signature: (a: ref $string, b: ref $string) -> ref $string
Concatenates two strings by creating a new array."
  (let ((string-idx (string-type-index registry)))
    (append
     ;; Local declarations: len_a, len_b, result, i
     ;; Locals are indexed 2, 3, 4, 5 (params are 0, 1)

     ;; len_a = array.len(a)
     (emit-local.get 0)                    ; a
     (emit-array.len)
     (emit-local.set 2)                    ; len_a

     ;; len_b = array.len(b)
     (emit-local.get 1)                    ; b
     (emit-array.len)
     (emit-local.set 3)                    ; len_b

     ;; result = array.new_default($string, len_a + len_b)
     (emit-local.get 2)                    ; len_a
     (emit-local.get 3)                    ; len_b
     (list (opcode :i32.add))
     (emit-array.new-default string-idx)
     (emit-local.set 4)                    ; result

     ;; Copy from a: for i = 0; i < len_a; i++
     (emit-i32.const 0)
     (emit-local.set 5)                    ; i = 0

     (list (opcode :block))                ; block $done_a
     (encode-blocktype nil)
     (list (opcode :loop))                 ; loop $copy_a
     (encode-blocktype nil)

     ;; if i >= len_a: break
     (emit-local.get 5)
     (emit-local.get 2)
     (list (opcode :i32.ge_u))
     (list (opcode :br_if) 1)              ; br_if $done_a

     ;; result[i] = a[i]
     (emit-local.get 4)                    ; result
     (emit-local.get 5)                    ; i
     (emit-local.get 0)                    ; a
     (emit-local.get 5)                    ; i
     (emit-array.get string-idx)
     (emit-array.set string-idx)

     ;; i++
     (emit-local.get 5)
     (emit-i32.const 1)
     (list (opcode :i32.add))
     (emit-local.set 5)

     (list (opcode :br) 0)                 ; br $copy_a
     (emit-end)                            ; end loop
     (emit-end)                            ; end block

     ;; Copy from b: for i = 0; i < len_b; i++
     (emit-i32.const 0)
     (emit-local.set 5)                    ; i = 0

     (list (opcode :block))
     (encode-blocktype nil)
     (list (opcode :loop))
     (encode-blocktype nil)

     ;; if i >= len_b: break
     (emit-local.get 5)
     (emit-local.get 3)
     (list (opcode :i32.ge_u))
     (list (opcode :br_if) 1)

     ;; result[len_a + i] = b[i]
     (emit-local.get 4)                    ; result
     (emit-local.get 2)                    ; len_a
     (emit-local.get 5)                    ; i
     (list (opcode :i32.add))
     (emit-local.get 1)                    ; b
     (emit-local.get 5)                    ; i
     (emit-array.get string-idx)
     (emit-array.set string-idx)

     ;; i++
     (emit-local.get 5)
     (emit-i32.const 1)
     (list (opcode :i32.add))
     (emit-local.set 5)

     (list (opcode :br) 0)
     (emit-end)
     (emit-end)

     ;; return result
     (emit-local.get 4)
     (emit-end))))

;;; ============================================================
;;; Count Digits Function
;;; ============================================================

(defun gen-count-digits-body ()
  "Generate body for $count-digits function.
Signature: (n: i32) -> i32
Returns the number of decimal digits in n (assumes n >= 0)."
  (append
   ;; Local: count (index 1)

   ;; count = 1
   (emit-i32.const 1)
   (emit-local.set 1)

   ;; loop while n >= 10
   (list (opcode :block))
   (encode-blocktype nil)
   (list (opcode :loop))
   (encode-blocktype nil)

   ;; if n < 10: break
   (emit-local.get 0)                      ; n
   (emit-i32.const 10)
   (list (opcode :i32.lt_u))
   (list (opcode :br_if) 1)

   ;; n = n / 10
   (emit-local.get 0)
   (emit-i32.const 10)
   (list (opcode :i32.div_u))
   (emit-local.set 0)

   ;; count++
   (emit-local.get 1)
   (emit-i32.const 1)
   (list (opcode :i32.add))
   (emit-local.set 1)

   (list (opcode :br) 0)
   (emit-end)
   (emit-end)

   ;; return count
   (emit-local.get 1)
   (emit-end)))

;;; ============================================================
;;; Fixnum to String Function
;;; ============================================================

(defun gen-fixnum-to-string-body (registry printer-reg)
  "Generate body for $fixnum-to-string function.
Signature: (n: i32) -> ref $string
Converts an i32 to its decimal string representation."
  (let ((string-idx (string-type-index registry))
        (count-digits-idx (printer-registry-count-digits printer-reg)))
    (append
     ;; Locals: negative (1), abs_n (2), digits (3), result (4), pos (5)

     ;; Check if negative
     (emit-local.get 0)                    ; n
     (emit-i32.const 0)
     (list (opcode :i32.lt_s))
     (emit-local.set 1)                    ; negative

     ;; abs_n = negative ? -n : n
     (emit-local.get 1)
     (list (opcode :if))
     (encode-blocktype :i32)
     (emit-i32.const 0)
     (emit-local.get 0)
     (list (opcode :i32.sub))              ; 0 - n
     (list (opcode :else))
     (emit-local.get 0)
     (emit-end)
     (emit-local.set 2)                    ; abs_n

     ;; digits = count_digits(abs_n)
     (emit-local.get 2)
     (list (opcode :call))
     (encode-uleb128 count-digits-idx)
     (emit-local.set 3)                    ; digits

     ;; result = array.new_default($string, digits + negative)
     (emit-local.get 3)
     (emit-local.get 1)
     (list (opcode :i32.add))
     (emit-array.new-default string-idx)
     (emit-local.set 4)                    ; result

     ;; pos = digits + negative - 1
     (emit-local.get 3)
     (emit-local.get 1)
     (list (opcode :i32.add))
     (emit-i32.const 1)
     (list (opcode :i32.sub))
     (emit-local.set 5)                    ; pos

     ;; Fill digits from right to left
     (list (opcode :block))
     (encode-blocktype nil)
     (list (opcode :loop))
     (encode-blocktype nil)

     ;; result[pos] = '0' + (abs_n % 10)
     (emit-local.get 4)                    ; result
     (emit-local.get 5)                    ; pos
     (emit-local.get 2)                    ; abs_n
     (emit-i32.const 10)
     (list (opcode :i32.rem_u))
     (emit-i32.const 48)                   ; '0'
     (list (opcode :i32.add))
     (emit-array.set string-idx)

     ;; abs_n = abs_n / 10
     (emit-local.get 2)
     (emit-i32.const 10)
     (list (opcode :i32.div_u))
     (emit-local.set 2)

     ;; if abs_n == 0: break
     (emit-local.get 2)
     (list (opcode :i32.eqz))
     (list (opcode :br_if) 1)

     ;; pos--
     (emit-local.get 5)
     (emit-i32.const 1)
     (list (opcode :i32.sub))
     (emit-local.set 5)

     (list (opcode :br) 0)
     (emit-end)
     (emit-end)

     ;; If negative, set result[0] = '-'
     (emit-local.get 1)
     (list (opcode :if))
     (encode-blocktype nil)
     (emit-local.get 4)                    ; result
     (emit-i32.const 0)                    ; index 0
     (emit-i32.const 45)                   ; '-'
     (emit-array.set string-idx)
     (emit-end)

     ;; return result
     (emit-local.get 4)
     (emit-end))))

;;; ============================================================
;;; Print Fixnum Function
;;; ============================================================

(defun gen-print-fixnum-body (printer-reg)
  "Generate body for $print-fixnum function.
Signature: (obj: anyref) -> ref $string
Extracts i32 from i31ref and converts to string."
  (let ((fixnum-to-string-idx (printer-registry-fixnum-to-string printer-reg)))
    (append
     ;; Extract i32 from i31ref: ref.cast i31, i31.get_s
     (emit-local.get 0)                    ; obj
     (list #xFB #x17 #x6C)                 ; ref.cast i31
     (emit-i31.get-s)

     ;; Call fixnum-to-string
     (list (opcode :call))
     (encode-uleb128 fixnum-to-string-idx)

     (emit-end))))

;;; ============================================================
;;; Print NIL Function
;;; ============================================================

(defun gen-print-nil-body (registry)
  "Generate body for $print-nil function.
Signature: () -> ref $string
Returns the string \"NIL\"."
  (let ((string-idx (string-type-index registry)))
    (append
     ;; Create "NIL" string using array.new_fixed
     (emit-i32.const 78)                   ; 'N'
     (emit-i32.const 73)                   ; 'I'
     (emit-i32.const 76)                   ; 'L'
     (emit-array.new-fixed string-idx 3)
     (emit-end))))

;;; ============================================================
;;; Print Cons Function
;;; ============================================================

(defun gen-print-cons-body (registry printer-reg)
  "Generate body for $print-cons function.
Signature: (cons: anyref) -> ref $string
Prints a cons cell as (car . cdr) or proper list."
  (let ((string-idx (string-type-index registry))
        (cons-idx (cons-type-index registry))
        (string-append-idx (printer-registry-string-append printer-reg))
        (prin1-idx (printer-registry-prin1-to-string printer-reg))
        (print-cdr-idx (printer-registry-print-cdr-list printer-reg)))
    (append
     ;; Locals: cons-struct (1), car (2), cdr (3), result (4)

     ;; Cast to cons type
     (emit-local.get 0)
     (list #xFB #x17)                      ; ref.cast
     (encode-uleb128 cons-idx)
     (emit-local.set 1)                    ; cons-struct

     ;; Get car
     (emit-local.get 1)
     (emit-struct.get cons-idx +cons-car+)
     (emit-local.set 2)                    ; car

     ;; Get cdr
     (emit-local.get 1)
     (emit-struct.get cons-idx +cons-cdr+)
     (emit-local.set 3)                    ; cdr

     ;; result = "("
     (emit-i32.const 40)                   ; '('
     (emit-array.new-fixed string-idx 1)
     (emit-local.set 4)

     ;; result = result + prin1-to-string(car)
     (emit-local.get 4)
     (emit-local.get 2)
     (list (opcode :call))
     (encode-uleb128 prin1-idx)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)
     (emit-local.set 4)

     ;; Check if cdr is NIL (null reference)
     (emit-local.get 3)
     (emit-ref.is-null)
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))

     ;; cdr is NIL: result + ")"
     (emit-local.get 4)
     (emit-i32.const 41)                   ; ')'
     (emit-array.new-fixed string-idx 1)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)

     (list (opcode :else))

     ;; Check if cdr is cons
     (emit-local.get 3)
     (emit-ref.test cons-idx)
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))

     ;; cdr is cons: result + " " + print-cdr-list(cdr) + ")"
     (emit-local.get 4)
     (emit-i32.const 32)                   ; ' '
     (emit-array.new-fixed string-idx 1)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)
     (emit-local.get 3)
     (list (opcode :call))
     (encode-uleb128 print-cdr-idx)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)
     (emit-i32.const 41)                   ; ')'
     (emit-array.new-fixed string-idx 1)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)

     (list (opcode :else))

     ;; cdr is atom: result + " . " + prin1(cdr) + ")"
     (emit-local.get 4)
     (emit-i32.const 32)                   ; ' '
     (emit-i32.const 46)                   ; '.'
     (emit-i32.const 32)                   ; ' '
     (emit-array.new-fixed string-idx 3)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)
     (emit-local.get 3)
     (list (opcode :call))
     (encode-uleb128 prin1-idx)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)
     (emit-i32.const 41)                   ; ')'
     (emit-array.new-fixed string-idx 1)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)

     (emit-end)                            ; end if (cons check)
     (emit-end)                            ; end if (nil check)

     (emit-end))))

;;; ============================================================
;;; Print CDR List Function
;;; ============================================================

(defun gen-print-cdr-list-body (registry printer-reg)
  "Generate body for $print-cdr-list function.
Signature: (cons: anyref) -> ref $string
Prints the cdr of a list without outer parentheses."
  (let ((string-idx (string-type-index registry))
        (cons-idx (cons-type-index registry))
        (string-append-idx (printer-registry-string-append printer-reg))
        (prin1-idx (printer-registry-prin1-to-string printer-reg))
        (print-cdr-idx (printer-registry-print-cdr-list printer-reg)))
    (append
     ;; Locals: cons-struct (1), car (2), cdr (3), result (4)

     ;; Cast to cons type
     (emit-local.get 0)
     (list #xFB #x17)                      ; ref.cast
     (encode-uleb128 cons-idx)
     (emit-local.set 1)

     ;; Get car
     (emit-local.get 1)
     (emit-struct.get cons-idx +cons-car+)
     (emit-local.set 2)

     ;; Get cdr
     (emit-local.get 1)
     (emit-struct.get cons-idx +cons-cdr+)
     (emit-local.set 3)

     ;; result = prin1-to-string(car)
     (emit-local.get 2)
     (list (opcode :call))
     (encode-uleb128 prin1-idx)
     (emit-local.set 4)

     ;; Check if cdr is NIL (null reference)
     (emit-local.get 3)
     (emit-ref.is-null)
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))

     ;; cdr is NIL: return result
     (emit-local.get 4)

     (list (opcode :else))

     ;; Check if cdr is cons
     (emit-local.get 3)
     (emit-ref.test cons-idx)
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))

     ;; cdr is cons: result + " " + print-cdr-list(cdr)
     (emit-local.get 4)
     (emit-i32.const 32)                   ; ' '
     (emit-array.new-fixed string-idx 1)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)
     (emit-local.get 3)
     (list (opcode :call))
     (encode-uleb128 print-cdr-idx)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)

     (list (opcode :else))

     ;; cdr is atom: result + " . " + prin1(cdr)
     (emit-local.get 4)
     (emit-i32.const 32)
     (emit-i32.const 46)
     (emit-i32.const 32)
     (emit-array.new-fixed string-idx 3)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)
     (emit-local.get 3)
     (list (opcode :call))
     (encode-uleb128 prin1-idx)
     (list (opcode :call))
     (encode-uleb128 string-append-idx)

     (emit-end)
     (emit-end)

     (emit-end))))

;;; ============================================================
;;; Print Symbol Function
;;; ============================================================

(defun gen-print-symbol-body (registry)
  "Generate body for $print-symbol function.
Signature: (obj: anyref) -> ref $string
Extracts the symbol's name and returns it."
  (let ((symbol-idx (symbol-type-index registry))
        (string-idx (string-type-index registry)))
    (append
     ;; Cast to symbol type and get name field
     (emit-local.get 0)                    ; obj
     (list #xFB #x17)                      ; ref.cast
     (encode-uleb128 symbol-idx)
     (emit-struct.get symbol-idx +symbol-name+)

     ;; Cast name (anyref) to (ref null $string)
     (list #xFB #x17)                      ; ref.cast
     (encode-uleb128 string-idx)

     (emit-end))))

;;; ============================================================
;;; Print Character Function
;;; ============================================================

(defun gen-print-character-body (registry)
  "Generate body for $print-character function.
Signature: (obj: anyref) -> ref $string
Prints character as #\\x format."
  (let ((string-idx (string-type-index registry)))
    (append
     ;; Locals: char-code (1), result (2), name-len (3)

     ;; Extract i32 from i31ref
     (emit-local.get 0)                    ; obj
     (list #xFB #x17 #x6C)                 ; ref.cast i31
     (emit-i31.get-s)
     (emit-local.set 1)                    ; char-code

     ;; Check for named characters (Space, Newline, Tab)
     ;; Space = 32
     (emit-local.get 1)
     (emit-i32.const 32)
     (list (opcode :i32.eq))
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))
     ;; Create "#\\Space"
     (emit-i32.const 35)                   ; '#'
     (emit-i32.const 92)                   ; '\\'
     (emit-i32.const 83)                   ; 'S'
     (emit-i32.const 112)                  ; 'p'
     (emit-i32.const 97)                   ; 'a'
     (emit-i32.const 99)                   ; 'c'
     (emit-i32.const 101)                  ; 'e'
     (emit-array.new-fixed string-idx 7)

     (list (opcode :else))

     ;; Newline = 10
     (emit-local.get 1)
     (emit-i32.const 10)
     (list (opcode :i32.eq))
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))
     ;; Create "#\\Newline"
     (emit-i32.const 35)                   ; '#'
     (emit-i32.const 92)                   ; '\\'
     (emit-i32.const 78)                   ; 'N'
     (emit-i32.const 101)                  ; 'e'
     (emit-i32.const 119)                  ; 'w'
     (emit-i32.const 108)                  ; 'l'
     (emit-i32.const 105)                  ; 'i'
     (emit-i32.const 110)                  ; 'n'
     (emit-i32.const 101)                  ; 'e'
     (emit-array.new-fixed string-idx 9)

     (list (opcode :else))

     ;; Tab = 9
     (emit-local.get 1)
     (emit-i32.const 9)
     (list (opcode :i32.eq))
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))
     ;; Create "#\\Tab"
     (emit-i32.const 35)                   ; '#'
     (emit-i32.const 92)                   ; '\\'
     (emit-i32.const 84)                   ; 'T'
     (emit-i32.const 97)                   ; 'a'
     (emit-i32.const 98)                   ; 'b'
     (emit-array.new-fixed string-idx 5)

     (list (opcode :else))

     ;; Regular character: "#\\x"
     (emit-i32.const 35)                   ; '#'
     (emit-i32.const 92)                   ; '\\'
     (emit-local.get 1)                    ; char-code
     (emit-array.new-fixed string-idx 3)

     (emit-end)                            ; end Tab check
     (emit-end)                            ; end Newline check
     (emit-end)                            ; end Space check

     (emit-end))))

;;; ============================================================
;;; Prin1-to-String Main Dispatcher
;;; ============================================================

(defun gen-prin1-to-string-body (registry printer-reg)
  "Generate body for $prin1-to-string function.
Signature: (obj: anyref) -> ref $string
Dispatches to type-specific printers."
  (let ((string-idx (string-type-index registry))
        (cons-idx (cons-type-index registry))
        (symbol-idx (symbol-type-index registry))
        (print-fixnum-idx (printer-registry-print-fixnum printer-reg))
        (print-nil-idx (printer-registry-print-nil printer-reg))
        (print-cons-idx (printer-registry-print-cons printer-reg))
        (print-symbol-idx (printer-registry-print-symbol printer-reg)))
    (append
     ;; Check for NIL (null reference) first
     (emit-local.get 0)
     (emit-ref.is-null)
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))

     ;; Is NIL (null)
     (list (opcode :call))
     (encode-uleb128 print-nil-idx)

     (list (opcode :else))

     ;; Check for i31ref (fixnum)
     (emit-local.get 0)
     (list #xFB #x14 #x6C)                 ; ref.test i31
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))

     ;; Is fixnum
     (emit-local.get 0)
     (list (opcode :call))
     (encode-uleb128 print-fixnum-idx)

     (list (opcode :else))

     ;; Check for cons
     (emit-local.get 0)
     (emit-ref.test cons-idx)
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))

     ;; Is cons
     (emit-local.get 0)
     (list (opcode :call))
     (encode-uleb128 print-cons-idx)

     (list (opcode :else))

     ;; Check for symbol
     (emit-local.get 0)
     (emit-ref.test symbol-idx)
     (list (opcode :if))
     (encode-blocktype `(:ref :null ,string-idx))

     ;; Is symbol
     (emit-local.get 0)
     (list (opcode :call))
     (encode-uleb128 print-symbol-idx)

     (list (opcode :else))

     ;; Unknown type: return "#<?>"
     (emit-i32.const 35)                   ; '#'
     (emit-i32.const 60)                   ; '<'
     (emit-i32.const 63)                   ; '?'
     (emit-i32.const 62)                   ; '>'
     (emit-array.new-fixed string-idx 4)

     (emit-end)                            ; end symbol check
     (emit-end)                            ; end cons check
     (emit-end)                            ; end fixnum check
     (emit-end)                            ; end nil check

     (emit-end))))

;;; ============================================================
;;; Get Result Character Function
;;; ============================================================

(defun gen-get-result-char-body (printer-reg string-idx)
  "Generate body for $get_result_char function.
Signature: (i: i32) -> i32
Returns the character code at index i from the result string.
Returns -1 if null or out of bounds."
  (let ((result-global-idx (printer-registry-result-global printer-reg)))
    (append
     ;; Get the global result string
     (list (opcode :global.get))
     (encode-uleb128 result-global-idx)

     ;; Check if null
     (emit-ref.is-null)
     (list (opcode :if))
     (encode-blocktype :i32)

     ;; Is null: return -1
     (emit-i32.const -1)

     (list (opcode :else))

     ;; Not null: check bounds
     ;; First get string again (since we consumed it)
     (list (opcode :global.get))
     (encode-uleb128 result-global-idx)
     (emit-ref.as-non-null)
     (emit-array.len)                        ; len

     (emit-local.get 0)                      ; i
     (list (opcode :i32.le_u))               ; len <= i (out of bounds)
     (list (opcode :if))
     (encode-blocktype :i32)

     ;; Out of bounds: return -1
     (emit-i32.const -1)

     (list (opcode :else))

     ;; In bounds: get character
     (list (opcode :global.get))
     (encode-uleb128 result-global-idx)
     (emit-ref.as-non-null)
     (emit-local.get 0)                      ; i
     (emit-array.get string-idx)

     (emit-end)                              ; end bounds check
     (emit-end)                              ; end null check

     (emit-end))))

;;; ============================================================
;;; Register Printer Functions
;;; ============================================================

(defun register-printer-functions (module registry)
  "Register all printer functions with the module.
Returns a printer-registry with function indices."
  (let ((printer-reg (make-printer-registry))
        (string-idx (string-type-index registry))
        (cons-idx (cons-type-index registry)))

    ;; Create function types - use nullable refs for compatibility with locals
    ;; (ref null $string, ref null $string) -> ref null $string
    (let* ((string-ref `(:ref :null ,string-idx))
           (append-type (make-functype (list string-ref string-ref)
                                       (list string-ref)))
           (append-type-idx (module-add-type module (make-wasm-type append-type))))

      ;; (i32) -> i32
      (let* ((i32-i32-type (make-functype '(:i32) '(:i32)))
             (i32-i32-type-idx (module-add-type module (make-wasm-type i32-i32-type))))

        ;; (i32) -> ref null $string
        (let* ((i32-string-type (make-functype '(:i32) (list string-ref)))
               (i32-string-type-idx (module-add-type module (make-wasm-type i32-string-type))))

          ;; (anyref) -> ref null $string
          (let* ((any-string-type (make-functype '(:anyref) (list string-ref)))
                 (any-string-type-idx (module-add-type module (make-wasm-type any-string-type))))

            ;; () -> ref null $string
            (let* ((void-string-type (make-functype nil (list string-ref)))
                   (void-string-type-idx (module-add-type module (make-wasm-type void-string-type))))

              ;; Register functions in dependency order

              ;; 1. string-append
              (let* ((body (gen-string-append-body registry))
                     (func (make-wasm-func append-type-idx
                                          :name "$string-append"
                                          :locals `((1 . :i32) (1 . :i32)
                                                    (1 . (:ref :null ,string-idx))
                                                    (1 . :i32))
                                          :body body))
                     (func-idx (module-add-func module func)))
                (setf (printer-registry-string-append printer-reg) func-idx))

              ;; 2. count-digits
              (let* ((body (gen-count-digits-body))
                     (func (make-wasm-func i32-i32-type-idx
                                          :name "$count-digits"
                                          :locals '((1 . :i32))
                                          :body body))
                     (func-idx (module-add-func module func)))
                (setf (printer-registry-count-digits printer-reg) func-idx))

              ;; 3. fixnum-to-string
              (let* ((body (gen-fixnum-to-string-body registry printer-reg))
                     (func (make-wasm-func i32-string-type-idx
                                          :name "$fixnum-to-string"
                                          :locals `((1 . :i32) (1 . :i32) (1 . :i32)
                                                    (1 . (:ref :null ,string-idx))
                                                    (1 . :i32))
                                          :body body))
                     (func-idx (module-add-func module func)))
                (setf (printer-registry-fixnum-to-string printer-reg) func-idx))

              ;; 4. print-fixnum
              (let* ((body (gen-print-fixnum-body printer-reg))
                     (func (make-wasm-func any-string-type-idx
                                          :name "$print-fixnum"
                                          :locals nil
                                          :body body))
                     (func-idx (module-add-func module func)))
                (setf (printer-registry-print-fixnum printer-reg) func-idx))

              ;; 5. print-nil
              (let* ((body (gen-print-nil-body registry))
                     (func (make-wasm-func void-string-type-idx
                                          :name "$print-nil"
                                          :locals nil
                                          :body body))
                     (func-idx (module-add-func module func)))
                (setf (printer-registry-print-nil printer-reg) func-idx))

              ;; 6. print-symbol
              (let* ((body (gen-print-symbol-body registry))
                     (func (make-wasm-func any-string-type-idx
                                          :name "$print-symbol"
                                          :locals nil
                                          :body body))
                     (func-idx (module-add-func module func)))
                (setf (printer-registry-print-symbol printer-reg) func-idx))

              ;; 7. print-character
              (let* ((body (gen-print-character-body registry))
                     (func (make-wasm-func any-string-type-idx
                                          :name "$print-character"
                                          :locals '((1 . :i32))
                                          :body body))
                     (func-idx (module-add-func module func)))
                (setf (printer-registry-print-character printer-reg) func-idx))

              ;; Reserve indices for mutually recursive functions
              ;; 8. prin1-to-string (placeholder)
              (let ((prin1-idx (module-func-count module)))
                (setf (printer-registry-prin1-to-string printer-reg) prin1-idx))

              ;; 9. print-cons (placeholder)
              (let ((print-cons-idx (1+ (printer-registry-prin1-to-string printer-reg))))
                (setf (printer-registry-print-cons printer-reg) print-cons-idx))

              ;; 10. print-cdr-list (placeholder)
              (let ((print-cdr-idx (1+ (printer-registry-print-cons printer-reg))))
                (setf (printer-registry-print-cdr-list printer-reg) print-cdr-idx))

              ;; Now add the actual functions
              ;; 8. prin1-to-string
              (let* ((body (gen-prin1-to-string-body registry printer-reg))
                     (func (make-wasm-func any-string-type-idx
                                          :name "$prin1-to-string"
                                          :locals nil
                                          :body body)))
                (module-add-func module func))

              ;; 9. print-cons
              ;; Locals: cons-struct (ref cons), car (anyref), cdr (anyref), result (ref string)
              (let* ((body (gen-print-cons-body registry printer-reg))
                     (func (make-wasm-func any-string-type-idx
                                          :name "$print-cons"
                                          :locals `((1 . (:ref :null ,cons-idx))
                                                    (1 . :anyref) (1 . :anyref)
                                                    (1 . (:ref :null ,string-idx)))
                                          :body body)))
                (module-add-func module func))

              ;; 10. print-cdr-list
              ;; Locals: cons-struct (ref cons), car (anyref), cdr (anyref), result (ref string)
              (let* ((body (gen-print-cdr-list-body registry printer-reg))
                     (func (make-wasm-func any-string-type-idx
                                          :name "$print-cdr-list"
                                          :locals `((1 . (:ref :null ,cons-idx))
                                                    (1 . :anyref) (1 . :anyref)
                                                    (1 . (:ref :null ,string-idx)))
                                          :body body)))
                (module-add-func module func))

              ;; 11. Global for storing result string
              ;; Type: (ref null $string), mutable, initialized to null
              (let* ((global-type `(:ref :null ,string-idx))
                     (init-expr (append
                                 (list (opcode :ref.null))
                                 (encode-sleb128 string-idx)
                                 (emit-end)))
                     (global (make-global global-type init-expr
                                          :mutable t
                                          :name "$result_string"))
                     (global-idx (module-add-global module global)))
                (setf (printer-registry-result-global printer-reg) global-idx))

              ;; 12. get_result_char function
              ;; Signature: (i: i32) -> i32
              ;; Returns the character code at index i, or -1 if out of bounds or null
              (let* ((body (gen-get-result-char-body printer-reg string-idx))
                     (func (make-wasm-func i32-i32-type-idx
                                          :name "$get_result_char"
                                          :locals nil
                                          :body body))
                     (func-idx (module-add-func module func)))
                (setf (printer-registry-get-result-char printer-reg) func-idx)
                ;; Export the function
                (module-add-export module
                                   (make-export "get_result_char" :func func-idx))))))))

    printer-reg))
