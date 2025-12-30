;;; feature-registry.lisp - CL Feature Registry for Clysm Validation
;;;
;;; Defines the CL-Feature struct and *clysm-features* hash-table
;;; for tracking which Common Lisp features Clysm supports.

(in-package :clysm-validation)

;;; T006: CL-Feature struct definition

(defstruct cl-feature
  "Represents a Common Lisp feature with its support status in Clysm.

   Slots:
   - symbol: The CL symbol (e.g., DEFUN, LOOP)
   - category: One of :special-form, :macro, :function, :type, :declaration
   - status: One of :supported, :partial, :unsupported, :internal
   - notes: Optional string with usage notes (especially for :partial)"
  (symbol nil :type symbol)
  (category :function :type keyword)
  (status :unsupported :type keyword)
  (notes nil :type (or null string)))

;;; T010: *clysm-features* hash-table with initial Clysm-supported CL symbols

(defparameter *clysm-features*
  (let ((ht (make-hash-table :test 'eq)))
    ;; Special forms - fully supported
    (dolist (sym '(if progn let let* lambda quote function setq
                   tagbody go block return-from catch throw
                   unwind-protect multiple-value-call multiple-value-prog1
                   the locally eval-when))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :special-form
                             :status :supported)))

    ;; Defining macros - fully supported
    (dolist (sym '(defun defmacro defvar defparameter defconstant
                   defstruct defclass defmethod defgeneric))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :macro
                             :status :supported)))

    ;; Control flow macros - fully supported
    (dolist (sym '(when unless cond case typecase etypecase ctypecase
                   and or not
                   dolist dotimes do do*
                   prog1 prog2 progn))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :macro
                             :status :supported)))

    ;; LOOP macro - partial support
    (setf (gethash 'loop ht)
          (make-cl-feature :symbol 'loop
                           :category :macro
                           :status :partial
                           :notes "Only :for :collect :do :return :while :until :initially :finally supported"))

    ;; Setf macros - fully supported
    (dolist (sym '(setf psetf incf decf push pop pushnew rotatef shiftf))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :macro
                             :status :supported)))

    ;; Multiple values - fully supported
    (dolist (sym '(values multiple-value-bind multiple-value-list
                   multiple-value-setq nth-value values-list))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :macro
                             :status :supported)))

    ;; Destructuring - fully supported
    (setf (gethash 'destructuring-bind ht)
          (make-cl-feature :symbol 'destructuring-bind
                           :category :macro
                           :status :supported))

    ;; Arithmetic functions - fully supported
    (dolist (sym '(+ - * / mod rem floor ceiling truncate round
                   1+ 1- abs signum
                   max min
                   expt sqrt log exp
                   sin cos tan asin acos atan
                   gcd lcm
                   ash logand logior logxor lognot
                   ;; 001-numeric-functions: bitwise extensions
                   logcount integer-length
                   ;; 001-numeric-predicates: bit testing
                   logbitp logtest
                   ;; 001-numeric-predicates: byte specifiers
                   byte byte-size byte-position
                   ;; 001-numeric-predicates: byte operations
                   ldb dpb mask-field deposit-field))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Numeric comparison - fully supported
    (dolist (sym '(= /= < <= > >=
                   zerop plusp minusp evenp oddp))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Type predicates - fully supported
    (dolist (sym '(null symbolp consp listp atom
                   numberp integerp rationalp floatp realp complexp
                   characterp stringp vectorp arrayp
                   functionp
                   typep))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Equality predicates - fully supported
    (dolist (sym '(eq eql equal equalp))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Cons/list functions - fully supported
    (dolist (sym '(cons car cdr caar cadr cdar cddr
                   first second third fourth fifth
                   sixth seventh eighth ninth tenth
                   rest last butlast nthcdr nth
                   list list* append nconc reverse nreverse
                   member assoc rassoc
                   mapcar maplist mapc mapl mapcan mapcon
                   length copy-list copy-tree))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Sequence functions - fully supported
    (dolist (sym '(elt subseq copy-seq concatenate
                   find find-if find-if-not
                   position position-if position-if-not
                   count count-if count-if-not
                   remove remove-if remove-if-not
                   substitute substitute-if substitute-if-not
                   reduce
                   some every notany notevery
                   map))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; String functions - fully supported
    (dolist (sym '(string string= string/= string< string> string<= string>=
                   string-equal string-not-equal
                   string-lessp string-greaterp
                   string-upcase string-downcase string-capitalize
                   char schar
                   make-string))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Character functions - fully supported
    (dolist (sym '(char= char/= char< char> char<= char>=
                   char-equal char-not-equal
                   char-code code-char char-name
                   alpha-char-p digit-char-p alphanumericp
                   upper-case-p lower-case-p
                   char-upcase char-downcase))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Array functions - fully supported
    (dolist (sym '(aref svref make-array array-dimensions array-rank
                   array-total-size array-dimension
                   vector))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Hash table functions - fully supported
    (dolist (sym '(make-hash-table gethash remhash clrhash maphash
                   hash-table-count hash-table-p))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Symbol functions - fully supported
    (dolist (sym '(symbol-name symbol-package symbol-value symbol-function
                   symbol-plist get
                   make-symbol gensym gentemp
                   boundp fboundp))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Package functions - fully supported
    (dolist (sym '(find-package make-package in-package
                   export unexport import use-package
                   find-symbol intern))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; I/O functions - fully supported
    (dolist (sym '(read read-char read-line peek-char unread-char
                   write write-char write-string write-line
                   print prin1 princ terpri fresh-line))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; FORMAT - partial support
    (setf (gethash 'format ht)
          (make-cl-feature :symbol 'format
                           :category :function
                           :status :partial
                           :notes "Supports ~A ~S ~D ~B ~O ~X ~R ~C ~% ~& ~~ ~* ~? ~{ ~} ~[ ~] ~( ~)"))

    ;; Condition system - fully supported
    (dolist (sym '(error warn signal
                   handler-bind handler-case ignore-errors
                   restart-bind restart-case invoke-restart
                   find-restart compute-restarts
                   cerror assert check-type))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; CLOS - fully supported
    (dolist (sym '(make-instance slot-value slot-boundp
                   class-of class-name
                   call-next-method next-method-p))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Miscellaneous - fully supported
    (dolist (sym '(funcall apply identity constantly
                   coerce type-of
                   documentation))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Type specifiers - fully supported
    (dolist (sym '(t nil fixnum integer rational float real number
                   character string symbol cons list
                   vector array hash-table
                   function))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :type
                             :status :supported)))

    ;; Declarations - partial support
    (dolist (sym '(declare special type ignore ignorable
                   inline notinline optimize))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :declaration
                             :status :partial
                             :notes "Declaration forms parsed but some may not affect codegen")))

    ;; Lambda list keywords - supported (recognized in parsing)
    (dolist (sym '(&optional &rest &key &body &whole &environment &allow-other-keys &aux))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :declaration
                             :status :supported)))

    ;; Additional special forms - supported
    (dolist (sym '(flet labels macrolet symbol-macrolet))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :special-form
                             :status :supported)))

    ;; Additional macros - supported
    (dolist (sym '(defpackage ecase return otherwise
                   with-output-to-string with-open-file
                   with-simple-restart print-unreadable-object
                   define-condition))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :macro
                             :status :supported)))

    ;; Additional functions - supported
    (dolist (sym '(read-from-string write-byte write-sequence
                   vector-push-extend parse-integer
                   numerator denominator realpart imagpart conjugate
                   macroexpand macroexpand-1 compile
                   stable-sort compute-applicable-methods
                   keywordp getf))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Condition types - supported
    (dolist (sym '(condition error serious-condition warning
                   simple-error simple-condition simple-warning
                   type-error type-error-datum type-error-expected-type
                   cell-error cell-error-name
                   unbound-variable undefined-function
                   program-error parse-error))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :type
                             :status :supported)))

    ;; CLOS types - supported
    (dolist (sym '(class standard-class generic-function method
                   method-combination))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :type
                             :status :supported)))

    ;; Additional type specifiers - supported
    (dolist (sym '(unsigned-byte byte stream pathname package keyword
                   boolean single-float double-float ratio complex
                   simple-vector simple-array))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :type
                             :status :supported)))

    ;; Stream variables - supported
    (dolist (sym '(*standard-input* *standard-output* *error-output*
                   *terminal-io* *query-io* *debug-io* *trace-output*))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function  ; actually special variables
                             :status :supported)))

    ;; Additional cons accessors - supported
    (dolist (sym '(caddr cdddr caaar caadr cadar cdaar cdadr cddar))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; More bit operations - supported
    (setf (gethash 'logbitp ht)
          (make-cl-feature :symbol 'logbitp
                           :category :function
                           :status :supported))

    ;; Additional condition types - supported
    (dolist (sym '(stream-error end-of-file file-error control-error))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :type
                             :status :supported)))

    ;; Restart names and functions - supported
    (dolist (sym '(abort use-value store-value continue muffle-warning))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; Additional condition functions - supported
    (dolist (sym '(make-condition invoke-debugger invoke-restart-interactively
                   simple-condition-format-control simple-condition-format-arguments))
      (setf (gethash sym ht)
            (make-cl-feature :symbol sym
                             :category :function
                             :status :supported)))

    ;; RESTART type - supported
    (setf (gethash 'restart ht)
          (make-cl-feature :symbol 'restart
                           :category :type
                           :status :supported))

    ;; Declaration symbol - supported
    (setf (gethash 'declaration ht)
          (make-cl-feature :symbol 'declaration
                           :category :declaration
                           :status :supported))

    ht)
  "Hash-table mapping CL symbols to their CL-Feature structs.
   Used for looking up whether Clysm supports a given CL feature.")

;;; T021: feature-status lookup function

(defun feature-status (symbol)
  "Return the support status of SYMBOL in Clysm.
   Returns one of: :supported, :partial, :unsupported, :internal, :unknown

   :unknown is returned for symbols not in the registry."
  (let ((feature (gethash symbol *clysm-features*)))
    (if feature
        (cl-feature-status feature)
        :unknown)))

(defun feature-status-with-notes (symbol)
  "Return the support status and notes for SYMBOL.
   Returns two values: status and notes (or nil)."
  (let ((feature (gethash symbol *clysm-features*)))
    (if feature
        (values (cl-feature-status feature)
                (cl-feature-notes feature))
        (values :unknown nil))))

(defun get-feature (symbol)
  "Return the CL-Feature struct for SYMBOL, or NIL if not in registry."
  (gethash symbol *clysm-features*))

(defun list-features-by-status (status)
  "Return a list of all symbols with the given STATUS."
  (let ((result nil))
    (maphash (lambda (sym feature)
               (when (eq (cl-feature-status feature) status)
                 (push sym result)))
             *clysm-features*)
    (sort result #'string< :key #'symbol-name)))

(defun list-features-by-category (category)
  "Return a list of all symbols in the given CATEGORY."
  (let ((result nil))
    (maphash (lambda (sym feature)
               (when (eq (cl-feature-category feature) category)
                 (push sym result)))
             *clysm-features*)
    (sort result #'string< :key #'symbol-name)))
