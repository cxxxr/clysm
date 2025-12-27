;;;; interpreter-builtins.lisp - Built-in functions for Tier 1 interpreter
;;;; Feature 044: Interpreter Bootstrap Strategy

(in-package #:clysm/eval/interpreter)

;;; ============================================================
;;; Extended Built-in Functions
;;; ============================================================
;;;
;;; This file extends the basic built-ins in interpreter.lisp
;;; with the full set of functions required by the compiler source.
;;; Organized by category per blessed-subset.lisp.

(defun install-extended-builtins (env)
  "Install extended built-in functions into ENV.
   Called after install-builtins for the root environment."

  ;; ============================================================
  ;; Hash Table Functions
  ;; ============================================================
  (env-bind env 'make-hash-table
            (lambda (&key (test 'eql) size)
              (declare (ignore size))
              (make-hash-table :test (case test
                                       ((eq) #'eq)
                                       ((eql) #'eql)
                                       ((equal) #'equal)
                                       ((equalp) #'equalp)
                                       (t test)))))
  (env-bind env 'gethash
            (lambda (key hash-table &optional default)
              (gethash key hash-table default)))
  (env-bind env 'puthash
            (lambda (key hash-table value)
              (setf (gethash key hash-table) value)))
  (env-bind env 'remhash #'remhash)
  (env-bind env 'clrhash #'clrhash)
  (env-bind env 'maphash #'maphash)
  (env-bind env 'hash-table-count #'hash-table-count)
  (env-bind env 'hash-table-p #'hash-table-p)

  ;; ============================================================
  ;; Extended Sequence Functions
  ;; ============================================================
  (env-bind env 'mapcar #'mapcar)
  (env-bind env 'mapcan #'mapcan)
  (env-bind env 'mapc #'mapc)
  (env-bind env 'maplist #'maplist)
  (env-bind env 'reduce #'reduce)
  (env-bind env 'find #'find)
  (env-bind env 'find-if #'find-if)
  (env-bind env 'find-if-not #'find-if-not)
  (env-bind env 'position #'position)
  (env-bind env 'position-if #'position-if)
  (env-bind env 'position-if-not #'position-if-not)
  (env-bind env 'remove #'remove)
  (env-bind env 'remove-if #'remove-if)
  (env-bind env 'remove-if-not #'remove-if-not)
  (env-bind env 'delete #'delete)
  (env-bind env 'delete-if #'delete-if)
  (env-bind env 'delete-if-not #'delete-if-not)
  (env-bind env 'substitute #'substitute)
  (env-bind env 'substitute-if #'substitute-if)
  (env-bind env 'count #'count)
  (env-bind env 'count-if #'count-if)
  (env-bind env 'count-if-not #'count-if-not)
  (env-bind env 'member #'member)
  (env-bind env 'member-if #'member-if)
  (env-bind env 'assoc #'assoc)
  (env-bind env 'assoc-if #'assoc-if)
  (env-bind env 'rassoc #'rassoc)
  (env-bind env 'rassoc-if #'rassoc-if)
  (env-bind env 'subseq #'subseq)
  (env-bind env 'copy-seq #'copy-seq)
  (env-bind env 'elt #'elt)
  (env-bind env 'sort #'sort)
  (env-bind env 'stable-sort #'stable-sort)
  (env-bind env 'merge #'merge)
  (env-bind env 'every #'every)
  (env-bind env 'some #'some)
  (env-bind env 'notevery #'notevery)
  (env-bind env 'notany #'notany)
  (env-bind env 'map #'map)
  (env-bind env 'concatenate #'concatenate)
  (env-bind env 'fill #'fill)
  (env-bind env 'replace #'replace)
  (env-bind env 'search #'search)
  (env-bind env 'mismatch #'mismatch)

  ;; Set operations
  (env-bind env 'adjoin #'adjoin)
  (env-bind env 'union #'union)
  (env-bind env 'intersection #'intersection)
  (env-bind env 'set-difference #'set-difference)
  (env-bind env 'set-exclusive-or #'set-exclusive-or)
  (env-bind env 'subsetp #'subsetp)

  ;; ============================================================
  ;; String Functions
  ;; ============================================================
  (env-bind env 'string= #'string=)
  (env-bind env 'string/= #'string/=)
  (env-bind env 'string< #'string<)
  (env-bind env 'string> #'string>)
  (env-bind env 'string<= #'string<=)
  (env-bind env 'string>= #'string>=)
  (env-bind env 'string-equal #'string-equal)
  (env-bind env 'string-upcase #'string-upcase)
  (env-bind env 'string-downcase #'string-downcase)
  (env-bind env 'string-capitalize #'string-capitalize)
  (env-bind env 'string-trim #'string-trim)
  (env-bind env 'string-left-trim #'string-left-trim)
  (env-bind env 'string-right-trim #'string-right-trim)
  (env-bind env 'char #'char)
  (env-bind env 'schar #'schar)
  (env-bind env 'string #'string)
  (env-bind env 'make-string #'make-string)

  ;; ============================================================
  ;; Extended Numeric Functions
  ;; ============================================================
  (env-bind env 'floor #'floor)
  (env-bind env 'ceiling #'ceiling)
  (env-bind env 'truncate #'truncate)
  (env-bind env 'round #'round)
  (env-bind env 'mod #'mod)
  (env-bind env 'rem #'rem)
  (env-bind env 'abs #'abs)
  (env-bind env 'min #'min)
  (env-bind env 'max #'max)
  (env-bind env 'gcd #'gcd)
  (env-bind env 'lcm #'lcm)
  (env-bind env 'expt #'expt)
  (env-bind env 'sqrt #'sqrt)
  (env-bind env 'log #'log)
  (env-bind env 'exp #'exp)
  (env-bind env 'sin #'sin)
  (env-bind env 'cos #'cos)
  (env-bind env 'tan #'tan)
  (env-bind env 'asin #'asin)
  (env-bind env 'acos #'acos)
  (env-bind env 'atan #'atan)
  (env-bind env '1+ #'1+)
  (env-bind env '1- #'1-)
  (env-bind env 'plusp #'plusp)
  (env-bind env 'minusp #'minusp)
  (env-bind env 'zerop #'zerop)
  (env-bind env 'evenp #'evenp)
  (env-bind env 'oddp #'oddp)
  (env-bind env 'signum #'signum)
  (env-bind env 'numerator #'numerator)
  (env-bind env 'denominator #'denominator)
  (env-bind env 'float #'float)
  (env-bind env 'rational #'rational)
  (env-bind env 'rationalize #'rationalize)
  (env-bind env 'complex #'complex)
  (env-bind env 'realpart #'realpart)
  (env-bind env 'imagpart #'imagpart)
  (env-bind env 'logand #'logand)
  (env-bind env 'logior #'logior)
  (env-bind env 'logxor #'logxor)
  (env-bind env 'lognot #'lognot)
  (env-bind env 'ash #'ash)
  (env-bind env 'logbitp #'logbitp)
  (env-bind env 'integer-length #'integer-length)

  ;; ============================================================
  ;; Type Functions
  ;; ============================================================
  (env-bind env 'typep #'typep)
  (env-bind env 'type-of #'type-of)
  (env-bind env 'subtypep #'subtypep)
  (env-bind env 'coerce #'coerce)

  ;; Extended type predicates
  (env-bind env 'integerp #'integerp)
  (env-bind env 'floatp #'floatp)
  (env-bind env 'rationalp #'rationalp)
  (env-bind env 'realp #'realp)
  (env-bind env 'complexp #'complexp)
  (env-bind env 'characterp #'characterp)
  (env-bind env 'arrayp #'arrayp)
  (env-bind env 'vectorp #'vectorp)
  (env-bind env 'simple-vector-p #'simple-vector-p)
  (env-bind env 'bit-vector-p #'bit-vector-p)
  (env-bind env 'simple-string-p #'simple-string-p)
  (env-bind env 'hash-table-p #'hash-table-p)
  (env-bind env 'packagep #'packagep)
  (env-bind env 'pathnamep #'pathnamep)
  (env-bind env 'streamp #'streamp)

  ;; ============================================================
  ;; Condition Functions
  ;; ============================================================
  (env-bind env 'error #'error)
  (env-bind env 'warn #'warn)
  (env-bind env 'signal #'signal)
  (env-bind env 'cerror #'cerror)
  (env-bind env 'break #'break)

  ;; ============================================================
  ;; Symbol Functions
  ;; ============================================================
  (env-bind env 'symbol-name #'symbol-name)
  (env-bind env 'symbol-package #'symbol-package)
  (env-bind env 'symbol-value #'symbol-value)
  (env-bind env 'symbol-function #'symbol-function)
  (env-bind env 'symbol-plist #'symbol-plist)
  (env-bind env 'boundp #'boundp)
  (env-bind env 'fboundp #'fboundp)
  (env-bind env 'makunbound #'makunbound)
  (env-bind env 'fmakunbound #'fmakunbound)
  (env-bind env 'make-symbol #'make-symbol)
  (env-bind env 'gensym #'gensym)
  (env-bind env 'gentemp #'gentemp)
  (env-bind env 'get #'get)
  (env-bind env 'getf #'getf)
  (env-bind env 'intern #'intern)

  ;; ============================================================
  ;; Package Functions
  ;; ============================================================
  (env-bind env 'find-package #'find-package)
  (env-bind env 'make-package #'make-package)
  (env-bind env 'package-name #'package-name)
  (env-bind env 'package-nicknames #'package-nicknames)
  (env-bind env 'package-use-list #'package-use-list)
  (env-bind env 'package-used-by-list #'package-used-by-list)
  (env-bind env 'export #'export)
  (env-bind env 'unexport #'unexport)
  (env-bind env 'import #'import)
  (env-bind env 'shadow #'shadow)
  (env-bind env 'shadowing-import #'shadowing-import)
  (env-bind env 'use-package #'use-package)
  (env-bind env 'unuse-package #'unuse-package)
  (env-bind env 'find-symbol #'find-symbol)

  ;; ============================================================
  ;; I/O Functions
  ;; ============================================================
  (env-bind env 'format #'format)
  (env-bind env 'print #'print)
  (env-bind env 'prin1 #'prin1)
  (env-bind env 'princ #'princ)
  (env-bind env 'terpri #'terpri)
  (env-bind env 'fresh-line #'fresh-line)
  (env-bind env 'write #'write)
  (env-bind env 'write-char #'write-char)
  (env-bind env 'write-string #'write-string)
  (env-bind env 'write-line #'write-line)
  (env-bind env 'read #'read)
  (env-bind env 'read-char #'read-char)
  (env-bind env 'read-line #'read-line)
  (env-bind env 'peek-char #'peek-char)
  (env-bind env 'unread-char #'unread-char)
  (env-bind env 'read-from-string #'read-from-string)

  ;; ============================================================
  ;; List/Cons Functions
  ;; ============================================================
  (env-bind env 'second #'second)
  (env-bind env 'third #'third)
  (env-bind env 'fourth #'fourth)
  (env-bind env 'fifth #'fifth)
  (env-bind env 'sixth #'sixth)
  (env-bind env 'seventh #'seventh)
  (env-bind env 'eighth #'eighth)
  (env-bind env 'ninth #'ninth)
  (env-bind env 'tenth #'tenth)
  (env-bind env 'caar #'caar)
  (env-bind env 'cadr #'cadr)
  (env-bind env 'cdar #'cdar)
  (env-bind env 'cddr #'cddr)
  (env-bind env 'caaar #'caaar)
  (env-bind env 'caadr #'caadr)
  (env-bind env 'cadar #'cadar)
  (env-bind env 'caddr #'caddr)
  (env-bind env 'cdaar #'cdaar)
  (env-bind env 'cdadr #'cdadr)
  (env-bind env 'cddar #'cddar)
  (env-bind env 'cdddr #'cdddr)
  (env-bind env 'last #'last)
  (env-bind env 'butlast #'butlast)
  (env-bind env 'nbutlast #'nbutlast)
  (env-bind env 'copy-list #'copy-list)
  (env-bind env 'copy-tree #'copy-tree)
  (env-bind env 'list* #'list*)
  (env-bind env 'make-list #'make-list)
  (env-bind env 'nconc #'nconc)
  (env-bind env 'nreverse #'nreverse)
  (env-bind env 'revappend #'revappend)
  (env-bind env 'nreconc #'nreconc)
  (env-bind env 'acons #'acons)
  (env-bind env 'pairlis #'pairlis)
  (env-bind env 'subst #'subst)
  (env-bind env 'subst-if #'subst-if)
  (env-bind env 'nsubst #'nsubst)
  (env-bind env 'tree-equal #'tree-equal)
  (env-bind env 'endp #'endp)
  (env-bind env 'ldiff #'ldiff)
  (env-bind env 'tailp #'tailp)
  (env-bind env 'rplaca #'rplaca)
  (env-bind env 'rplacd #'rplacd)

  ;; ============================================================
  ;; Array Functions
  ;; ============================================================
  (env-bind env 'make-array #'make-array)
  (env-bind env 'aref #'aref)
  (env-bind env 'svref #'svref)
  (env-bind env 'row-major-aref #'row-major-aref)
  (env-bind env 'array-dimensions #'array-dimensions)
  (env-bind env 'array-dimension #'array-dimension)
  (env-bind env 'array-total-size #'array-total-size)
  (env-bind env 'array-rank #'array-rank)
  (env-bind env 'array-element-type #'array-element-type)
  (env-bind env 'adjustable-array-p #'adjustable-array-p)
  (env-bind env 'vector #'vector)
  (env-bind env 'vector-push #'vector-push)
  (env-bind env 'vector-push-extend #'vector-push-extend)
  (env-bind env 'vector-pop #'vector-pop)
  (env-bind env 'fill-pointer #'fill-pointer)

  ;; ============================================================
  ;; Character Functions
  ;; ============================================================
  (env-bind env 'char-code #'char-code)
  (env-bind env 'code-char #'code-char)
  (env-bind env 'char-upcase #'char-upcase)
  (env-bind env 'char-downcase #'char-downcase)
  (env-bind env 'upper-case-p #'upper-case-p)
  (env-bind env 'lower-case-p #'lower-case-p)
  (env-bind env 'alpha-char-p #'alpha-char-p)
  (env-bind env 'digit-char-p #'digit-char-p)
  (env-bind env 'alphanumericp #'alphanumericp)
  (env-bind env 'graphic-char-p #'graphic-char-p)
  (env-bind env 'char= #'char=)
  (env-bind env 'char/= #'char/=)
  (env-bind env 'char< #'char<)
  (env-bind env 'char> #'char>)
  (env-bind env 'char<= #'char<=)
  (env-bind env 'char>= #'char>=)
  (env-bind env 'char-equal #'char-equal)

  ;; ============================================================
  ;; Control Functions
  ;; ============================================================
  (env-bind env 'values #'values)
  (env-bind env 'values-list #'values-list)

  ;; ============================================================
  ;; Misc Functions
  ;; ============================================================
  (env-bind env 'equalp #'equalp)
  (env-bind env 'constantly #'constantly)
  (env-bind env 'complement #'complement)
  (env-bind env 'identity #'identity))
