;;;; frontend.lisp — macroexpansion and conversion of Lisp forms to an AST.
;;;;
;;;; AST nodes are lists (:op ...).  Lexical variables are LVAR structures.
;;;; While converting we compute, for every lambda, the list of free lexical
;;;; variables (closure conversion) and which variables are captured/mutated
;;;; (assignment conversion: captured+mutated variables are boxed).

(in-package :clysm)

(defstruct (lvar (:print-function (lambda (v s d) (declare (ignore d))
                                    (format s "#<LVAR ~S>" (lvar-name v)))))
  name owner captured mutated special (refs 0) ignorable
  (type nil)    ; union of the types of its values (types.lisp)
  (decl nil)    ; a declared type specifier (trusted)
  (rep nil))    ; :f64 / :i32 when held unboxed in a local of that type

(defvar *current-source* nil
  "(FILE . LINE) of the toplevel form being compiled, or NIL: where the
functions made from it are, for the clysm.sources section (stack traces).")

(defstruct (lam (:print-function (lambda (l s d) (declare (ignore d))
                                   (format s "#<LAM ~S ~D>" (lam-name l) (lam-id l)))))
  id name parent
  (source *current-source*)
  req opt rest keyp keys allow-other-keys
  body free-vars
  (toplevel nil)          ; global defun with no free vars
  (static-p nil)          ; closure object is a module constant
  (may-exit :unknown)     ; NIL when no call of it can return with an exit pending (codegen.lisp)
  (self nil))             ; the variable of a LABELS function referred to in its own body: its closure

(defun lam-simple-p (l)
  (and (null (lam-opt l)) (null (lam-rest l)) (not (lam-keyp l))))

(defstruct blk name lam tag-var (nonlocal nil) label (mv nil) dyn-depth uwp-depth saved-bsp uwp-frames)
(defstruct tb lam tags tag-var (nonlocal nil) labels dispatch pc dyn-depth uwp-depth saved-bsp uwp-frames
  modes loop-labels cur)
(defstruct fun name lvar lam)

;;; ------------------------------------------------------------
;;; Global compile-time state
;;; ------------------------------------------------------------

(defvar *macros* (make-hash-table :test 'eq) "symbol -> host expander (form env)")
(defvar *symbol-macros* (make-hash-table :test 'eq))
(defvar *specials* (make-hash-table :test 'eq))
(defvar *constants* (make-hash-table :test 'eq) "symbol -> (value)")
(defvar *notinline* (make-hash-table :test 'eq))
(defvar *redefinable-functions* nil
  "True while compiling files whose functions may be redefined in the running
image: they are all NOTINLINE, so their callers go through the function cell.")
(defvar *compiler-macros* (make-hash-table :test 'eq) "symbol -> host fn (form env)")
(defvar *global-defuns* (make-hash-table :test 'eq) "symbol -> list of lams defined at toplevel")
(defvar *lam-counter* 0)
(defvar *dynamic-functions* (make-hash-table :test 'eq) "functions defined at runtime (generic functions)")
(defvar *current-lam* nil)
(defvar *warnings* nil)

;;; Live mode: the compiler runs inside a Clysm image and compiles code that
;;; is linked into that running image.  These hooks give access to the
;;; image's own macros, special declarations and constants.
(defvar *live-mode* nil)
(defvar *struct-constructors* (make-hash-table :test 'eq)
  "In the image: the name of a constructor of a structure -> the name of the structure.")

(defvar *live-macro-hook* nil "symbol -> macro function or NIL")
(defvar *live-special-hook* nil "symbol -> true if proclaimed special")
(defvar *live-constant-hook* nil "symbol -> (values value constantp)")
(defvar *live-var-type-hook* nil "symbol -> the type proclaimed for the global variable, or NIL")
(defvar *var-types* (make-hash-table :test 'eq)
  "symbol -> the type proclaimed for the global variable (DECLAIM in the forms compiled)")
(defvar *suppress-var-types* nil "true while converting the check of a reference of a typed special variable")
(defun global-var-type (sym)
  "The type proclaimed for the special variable SYM, or NIL."
  (multiple-value-bind (ty found) (gethash sym *var-types*)
    (cond (found ty)
          (*live-var-type-hook* (funcall *live-var-type-hook* sym))
          (t nil))))
(defvar *live-symbol-macro-hook* nil "symbol -> (values expansion foundp)")
(defvar *live-compiler-macro-hook* nil "symbol -> the compiler macro function defined in the image, or NIL")
(defun global-symbol-macro (sym)
  "The expansion of the global symbol macro SYM, and whether there is one."
  (multiple-value-bind (exp found) (gethash sym *symbol-macros*)
    (cond (found (values exp t))
          (*live-symbol-macro-hook* (funcall *live-symbol-macro-hook* sym))
          (t (values nil nil)))))

(defun cwarn (fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    (push msg *warnings*)
    (format *error-output* "~&; warning: ~A~%" msg)))

(defun special-var-p (sym)
  (or (gethash sym *specials*)
      (and *live-special-hook* (funcall *live-special-hook* sym))))

(defparameter *special-forms*
  '(quote function if progn let let* setq flet labels macrolet symbol-macrolet
    block return-from tagbody go catch throw unwind-protect multiple-value-call
    multiple-value-prog1 the locally eval-when load-time-value progv))

;;; ------------------------------------------------------------
;;; Environments: list of entries
;;;  (:var sym lvar) (:special sym) (:smacro sym expansion)
;;;  (:fun sym fun) (:macro sym expander) (:block sym blk) (:tag sym tb index)
;;;  (:decl-special sym) — free special declaration
;;; ------------------------------------------------------------

(defstruct (cenv (:constructor %make-cenv)) entries)
(defun make-cenv (&optional entries) (%make-cenv :entries entries))
(defun env-extend (env &rest entries) (make-cenv (append entries (cenv-entries env))))

(defun env-lookup-var (env sym)
  (dolist (e (cenv-entries env) nil)
    (when (and (member (first e) '(:var :special :smacro)) (eq (second e) sym))
      (return e))))

(defun env-lookup-fun (env sym)
  (dolist (e (cenv-entries env) nil)
    (when (and (member (first e) '(:fun :macro)) (equal (second e) sym))
      (return e))))

(defun env-lookup (env kind sym)
  (dolist (e (cenv-entries env) nil)
    (when (and (eq (first e) kind) (eq (second e) sym)) (return e))))

;;; ------------------------------------------------------------
;;; Macroexpansion
;;; ------------------------------------------------------------

(defun macro-function-in (sym env)
  (let ((e (and env (env-lookup-fun env sym))))
    (cond ((and e (eq (first e) :macro)) (third e))
          (e nil)
          ((gethash sym *macros*))
          ((and *live-macro-hook* (not (get sym 'clysm-native)) (not (member sym *special-forms*)))
           (funcall *live-macro-hook* sym))
          (t nil))))

(defun cmacroexpand-1 (form env)
  (cond ((and (consp form) (symbolp (car form)))
         (let ((m (macro-function-in (car form) env)))
           (if m (values (let ((*package* *read-package*)) (funcall m form env)) t) (values form nil))))
        ((and (symbolp form) form (not (keywordp form)))
         (let ((e (env-lookup-var env form)))
           (cond ((and e (eq (first e) :smacro)) (values (third e) t))
                 (e (values form nil))
                 (t (multiple-value-bind (exp found) (global-symbol-macro form)
                      (if found (values exp t) (values form nil)))))))
        (t (values form nil))))

(defun cmacroexpand (form env)
  (let ((expanded nil))
    (loop
      (multiple-value-bind (new changed) (cmacroexpand-1 form env)
        (unless changed (return (values form expanded)))
        (setf form new expanded t)))))

;;; Host-visible entry points used by macros evaluated on the host
(defmacro define-host-function (name ll &body body)
  "Define a SYSTEM function used by host-evaluated code.  When the compiler is
itself compiled by Clysm and shares the image's SYSTEM package, an existing
runtime definition is kept."
  #-clysm `(defun ,name ,ll ,@body)
  #+clysm `(unless (fboundp ',name) (setf (fdefinition ',name) (lambda ,ll ,@body))))

(define-host-function clysm-sys::%macroexpand-1 (form &optional env)
  (cmacroexpand-1 form (if (cenv-p env) env (make-cenv))))
(define-host-function clysm-sys::%macroexpand (form &optional env)
  (cmacroexpand form (if (cenv-p env) env (make-cenv))))

;;; ------------------------------------------------------------
;;; Declarations
;;; ------------------------------------------------------------

(defun parse-body (body &key (doc-allowed t))
  "Returns (values decls forms doc). DECLS is a list of declaration specifiers."
  (let ((decls nil) (doc nil))
    (loop
      (cond ((and (consp (car body)) (eq (caar body) 'declare))
             (setf decls (append decls (cdar body)))
             (pop body))
            ((and doc-allowed (stringp (car body)) (cdr body) (null doc))
             (setf doc (pop body)))
            (t (return))))
    (values decls body doc)))

(defparameter *decl-type-names*
  '(fixnum integer float double-float single-float short-float long-float character base-char
    string simple-string simple-vector cons list symbol number real rational bit function
    unsigned-byte signed-byte null simple-bit-vector simple-base-string))

(defun apply-type-decls (decls lvars)
  "Record the types declared in DECLS for the variables LVARS."
  (dolist (d decls)
    (when (consp d)
      (multiple-value-bind (spec vars)
          (cond ((eq (car d) 'type) (values (second d) (cddr d)))
                ((member (car d) *decl-type-names*) (values (car d) (cdr d)))
                ((and (consp (car d)) (member (caar d) '(integer unsigned-byte signed-byte simple-array simple-vector simple-string
                                                    simple-bit-vector)))
                 (values (car d) (cdr d))))
        (when spec
          (dolist (v vars)
            (let ((lv (find v lvars :key #'lvar-name :from-end t)))
              (when (and lv (symbolp v)) (setf (lvar-decl lv) spec)))))))))

;;; A type declaration about a lexical variable bound outside the form whose
;;; declarations hold it (a free declaration, CLHS 3.3.4) applies to the
;;; references of the variable within the form: they are converted as
;;; (THE type var), under an environment entry (:TDECL symbol type).  As for
;;; the variable's own declaration, the type is trusted, except an integer
;;; type, which is checked where the reference is (CHECKED-VALUE) while
;;; *CHECK-DECLARED-INTEGERS*.
(defun type-decl-specs (decls)
  "The (type . variables) of the type declarations in DECLS."
  (loop for d in decls
        when (consp d)
          append (multiple-value-bind (spec vars)
                     (cond ((eq (car d) 'type) (values (second d) (cddr d)))
                           ((member (car d) *decl-type-names*) (values (car d) (cdr d)))
                           ((and (consp (car d)) (member (caar d) '(integer unsigned-byte signed-byte simple-array
                                                               simple-vector simple-string simple-bit-vector)))
                            (values (car d) (cdr d))))
                   (and spec (list (cons spec vars))))))

(defun free-type-decl-env (decls bound env)
  "ENV with the free type declarations of DECLS (about lexical variables of
ENV that are not among the symbols BOUND by the form, nor declared where
they are bound)."
  (let ((entries nil))
    (loop for (spec . vars) in (type-decl-specs decls)
          do (dolist (v vars)
               (when (and (symbolp v) (not (member v bound)))
                 (let ((e (env-lookup-var env v)))
                   (when (or (and e (eq (first e) :var) (null (lvar-decl (third e))))
                             ;; a special variable (TYPED-SPECIAL-REF)
                             (and e (eq (first e) :special))
                             (and (null e) (special-var-p v)))
                     (push (list :tdecl v spec) entries))))))
    (if entries (apply #'env-extend env (nreverse entries)) env)))

(defvar *ignore-tdecls* nil "true while converting the check of a reference under a free declaration")

(defun free-type-decl (env sym)
  "The type a free declaration in ENV gives the variable SYM, or NIL."
  (unless *ignore-tdecls*
    (dolist (e (cenv-entries env) nil)
      (cond ((and (eq (first e) :tdecl) (eq (second e) sym)) (return (third e)))
            ((and (member (first e) '(:var :special :smacro)) (eq (second e) sym)) (return nil))))))

(defun integer-decl-p (spec)
  (or (eq spec 'fixnum)
      (and (consp spec) (member (car spec) '(integer unsigned-byte signed-byte)))))

(defun declared-spec (sym decls)
  "The type DECLS declare for the variable SYM (the last one), or NIL."
  (let ((found nil))
    (dolist (d decls found)
      (when (consp d)
        (multiple-value-bind (spec vars)
            (cond ((eq (car d) 'type) (values (second d) (cddr d)))
                  ((member (car d) *decl-type-names*) (values (car d) (cdr d)))
                  ((and (consp (car d)) (member (caar d) '(integer unsigned-byte signed-byte)))
                   (values (car d) (cdr d))))
          (when (and spec (member sym vars)) (setf found spec)))))))

(defun integer-decl-test (v spec)
  "A form that is true when the value of the variable V is of the integer type SPEC."
  (flet ((range (lo hi)
           (let ((tests (append (and (integerp lo) (list `(<= ,lo ,v)))
                                (and (integerp hi) (list `(<= ,v ,hi))))))
             (if (and (integerp lo) (integerp hi)
                      (<= +most-negative-fixnum+ lo) (<= hi +most-positive-fixnum+))
                 `(and (clysm-sys::fixnump ,v) ,@tests)
                 `(and (integerp ,v) ,@tests)))))
    (cond ((eq spec 'fixnum) `(clysm-sys::fixnump ,v))
          ((and (eq (car spec) 'integer) (listp (cdr spec)))
           (range (second spec) (third spec)))
          ((and (eq (car spec) 'unsigned-byte) (integerp (second spec)))
           (range 0 (1- (expt 2 (second spec)))))
          ((and (eq (car spec) 'signed-byte) (integerp (second spec)))
           (range (- (expt 2 (1- (second spec)))) (1- (expt 2 (1- (second spec))))))
          ((eq (car spec) 'unsigned-byte) (range 0 nil))
          (t `(integerp ,v)))))

(defvar *check-declared-integers* t
  "True when the value given to a variable of a declared integer type is
checked (CHECKED-VALUE).  False while the runtime is compiled: its
declarations are kept by how it is written, and it must not pay for the checks.")

(defun checked-value (form spec)
  "FORM as the value of a variable declared to be of the integer type SPEC:
checked, since the code after it computes with the variable as a machine
integer (a value past the type would become another number, or a character).
The check is left out by the compiler where the type of FORM shows it holds."
  ;; (a literal NIL is how macros bind a variable before its first value,
  ;; under the declarations of their body: DOLIST's variable for its result form)
  (if (or (not *check-declared-integers*) (not (integer-decl-p spec)) (integerp form) (null form))
      form
      (let ((v (gensym "V")))
        `(let ((,v ,form))
           (if ,(type-test-form v spec)
               ,v
               (clysm-sys::%signal-type-error ,v ',spec))))))

;;; FTYPE proclamations (CLHS 3.3.3.1 FTYPE: calls of the function with
;;; arguments not of the types, or values not of the type, are undefined).
;;; DECLAIM records (function (types...) result) for the name, at compile
;;; time (COMPILE-TIME-PROCLAIM: *DECLARED-FTYPES*) and, in the image, on its
;;; plist when the proclamation is evaluated (PROCLAIM: SYSTEM::%FTYPE, so a
;;; fasl loaded later still gives it).  Two uses, both checked, so that a
;;; wrong proclamation signals a TYPE-ERROR as in SBCL:
;;;  * a function defined at top level checks its required arguments of the
;;;    types on entry, into variables declared of the types (FTYPE-CHECKED-FUNCTION);
;;;  * the value of a call of a function whose result is (VALUES TYPE &OPTIONAL)
;;;    is checked where it is used as a value that is not returned (an argument,
;;;    a variable's value, a test: CONVERT-VALUE), so tail calls stay tail calls.
;;; Only types of which the compiler makes something (DECLARED-TY not T:
;;; numbers, characters, lists, symbols, functions, simple arrays) are used.
(defvar *declared-ftypes* (make-hash-table :test 'eq)
  "function name (symbol, or that of a SETF function) -> its proclaimed FTYPE")

(defun declared-ftype (sym)
  (and (symbolp sym)
       (or (gethash sym *declared-ftypes*)
           (and *live-mode* (get sym 'clysm-sys::%ftype)))))

(defun ftype-check-spec (spec &optional (depth 0))
  "SPEC as a type that TYPEP accepts and the code checks (its DEFTYPEs
expanded, (FUNCTION ...) as FUNCTION, also inside OR, AND and NOT), or NIL
when it is not one (VALUES, SATISFIES, a type not defined yet, an error)."
  (let ((e (or (ignore-errors (expand-deftypes spec)) spec)))
    (cond ((> depth 8) nil)
          ((and (consp e) (eq (car e) 'function)) 'function)
          ((and (consp e) (member (car e) '(or and not)))
           (let ((parts (loop for x in (cdr e) collect (ftype-check-spec x (1+ depth)))))
             (and (listp (cdr e)) (notany #'null parts) (cons (car e) parts))))
          ((and (consp e) (member (car e) '(values satisfies))) nil)
          (t e))))

(defun ftype-usable-spec (spec)
  "The type a check of a value of type SPEC (from an FTYPE) tests, or NIL."
  (let ((s (ignore-errors (ftype-check-spec spec))))
    (and s (ftype-useful-spec-p s) s)))

(defun ftype-arg-specs (sym)
  "The types of the required parameters FSYM's FTYPE gives (NIL for one not
checked), or NIL."
  (let ((ft (declared-ftype sym)))
    (and (consp ft) (eq (car ft) 'function) (consp (cdr ft)) (listp (second ft))
         (ignore-errors (list-length (second ft)))
         (loop for s in (second ft)
               until (member s lambda-list-keywords)
               collect (ftype-usable-spec s)))))

(defun ftype-result-spec (sym)
  "The type of the single value FSYM's FTYPE gives, (VALUES TYPE &OPTIONAL), or NIL."
  (let ((ft (declared-ftype sym)))
    (when (and (consp ft) (eq (car ft) 'function) (consp (cdr ft)) (consp (cddr ft)))
      (let ((r (third ft)))
        (and (consp r) (eq (car r) 'values) (eql (ignore-errors (list-length r)) 3) (eq (third r) '&optional)
             (ftype-usable-spec (second r)))))))

(defun type-checked-form (var spec)
  "A form of the value of the variable VAR, checked to be of type SPEC."
  `(the ,spec (if ,(type-test-form var spec)
                  ,var
                  (clysm-sys::%signal-type-error ,var ',spec))))

(defun ftype-checked-function (sym fn-form)
  "FN-FORM, #'(NAMED-LAMBDA name ll . body) of the toplevel function SYM, with
the required parameters of the types SYM's FTYPE gives checked on entry."
  (let ((specs (ftype-arg-specs sym)))
    (unless (and specs (consp fn-form) (eq (car fn-form) 'function) (consp (cdr fn-form))
                 (consp (second fn-form)) (eq (car (second fn-form)) 'clysm-sys::named-lambda)
                 (consp (cdr (second fn-form))) (listp (third (second fn-form))))
      (return-from ftype-checked-function fn-form))
    (destructuring-bind (name ll &rest body) (cdr (second fn-form))
      (let* ((nreq (or (position-if (lambda (x) (member x lambda-list-keywords)) ll) (length ll))))
        ;; (the init forms of the other parameters may refer to the required ones)
        (unless (and (every #'symbolp ll) (every #'symbolp (subseq ll 0 nreq)))
          (return-from ftype-checked-function fn-form))
        (multiple-value-bind (decls forms doc) (parse-body body)
          (let ((checks (loop for p in (subseq ll 0 nreq) for spec in specs
                              when (and (ftype-useful-spec-p spec) (not (declared-spec p decls))
                                        (not (member p (declared-specials decls))))
                                collect (list p (gensym (symbol-name p)) spec))))
            (if (null checks)
                fn-form
                `(function
                  (clysm-sys::named-lambda ,name
                      ,(mapcar (lambda (p) (let ((c (assoc p checks))) (if c (second c) p))) ll)
                    ,@(when doc (list doc))
                    (let ,(loop for (p g spec) in checks collect `(,p ,(type-checked-form g spec)))
                      (declare ,@(loop for (p nil spec) in checks collect `(type ,spec ,p)))
                      ,@(when decls `((declare ,@decls)))
                      ,@forms))))))))))

(defun ftype-call-form (form env)
  "FORM, a value that is not returned, with the value of a call of a function
of a proclaimed result type checked, or FORM."
  (let ((op (and (consp form) (car form))))
    (if (and op (symbolp op) (not (keywordp op))
             (not (member op *special-forms*))
             (not (get op 'clysm-native))
             (not (env-lookup-fun env op))
             (not (macro-function-in op env))
             (ftype-result-spec op))
        (let ((g (gensym "V")) (spec (ftype-result-spec op)))
          `(let ((,g (clysm-sys::%ftype-raw ,form))) ,(type-checked-form g spec)))
        form)))

(defun convert-value (form env)
  "Convert FORM, whose value is used but not returned."
  (convert (ftype-call-form form env) env))

(defvar *local-notinline* nil "function names declared NOTINLINE in the forms being converted")
(defun notinline-name (name)
  "The name under which a call of the function NAME is looked up: a call of
\(SETF X) is one of the symbol SETF-FUNCTION-SYMBOL gives."
  (if (and (consp name) (eq (car name) 'setf) (consp (cdr name)) (symbolp (second name)))
      (setf-function-symbol (second name))
      name))
(defun declared-notinline (decls)
  (loop for d in decls
        when (and (consp d) (eq (car d) 'notinline)) append (mapcar #'notinline-name (cdr d))))
(defmacro with-notinline-decls ((decls) &body body)
  `(let ((*local-notinline* (let ((n (declared-notinline ,decls)))
                              (if n (append n *local-notinline*) *local-notinline*))))
     ,@body))

(defun declared-specials (decls)
  (loop for d in decls
        when (and (consp d) (eq (car d) 'special)) append (cdr d)))

;;; ------------------------------------------------------------
;;; Variable references and capture analysis
;;; ------------------------------------------------------------

(defun note-ref (lv)
  (incf (lvar-refs lv))
  (unless (eq (lvar-owner lv) *current-lam*)
    (setf (lvar-captured lv) t)
    (loop for l = *current-lam* then (lam-parent l)
          until (or (null l) (eq l (lvar-owner lv)))
          do (pushnew lv (lam-free-vars l))))
  lv)

(defun new-lvar (name &optional special)
  (make-lvar :name name :owner *current-lam* :special special))

(defun binding-special-p (sym decl-specials)
  (or (member sym decl-specials) (special-var-p sym)))

;;; ------------------------------------------------------------
;;; Conversion
;;; ------------------------------------------------------------

(defun convert (form env)
  (cond ((symbolp form) (convert-symbol form env))
        ((atom form) `(:const ,form))
        ((consp (car form))
         (if (eq (caar form) 'lambda)
             (convert-lambda-application form env)
             (error "Illegal function call: ~S" form)))
        (t (convert-compound form env))))

(defun convert-body (forms env)
  (cond ((null forms) '(:const nil))
        ((null (cdr forms)) (convert (car forms) env))
        (t `(:progn ,@(mapcar (lambda (f) (convert f env)) forms)))))

(defun printable-constant-p (x depth)
  "Whether X, the value of a constant, can be written in a fasl: a number,
character, symbol or string, or a list or simple vector of them (to DEPTH)."
  (cond ((or (numberp x) (characterp x) (symbolp x) (stringp x)) t)
        ((<= depth 0) nil)
        ((consp x) (and (handler-case (list-length x) (error () nil))
                        (every (lambda (e) (printable-constant-p e (- depth 1))) x)))
        ((simple-vector-p x) (every (lambda (e) (printable-constant-p e (- depth 1))) x))
        (t nil)))

(defun convert-symbol (sym env)
  (cond ((or (null sym) (eq sym t) (keywordp sym)) `(:const ,sym))
        (t
         (let ((e (env-lookup-var env sym)))
           (cond ((and e (eq (first e) :var))
                  (let ((spec (free-type-decl env sym)))
                    (cond ((null spec) `(:lref ,(note-ref (third e))))
                          ((and *check-declared-integers* (integer-decl-p spec))
                           `(:the ,spec ,(let ((*ignore-tdecls* t)) (convert (checked-value sym spec) env))))
                          (t `(:the ,spec (:lref ,(note-ref (third e))))))))
                 ((and e (eq (first e) :special)) (typed-special-ref sym env))
                 ((and e (eq (first e) :smacro)) (convert (third e) env))
                 (t (multiple-value-bind (exp found) (global-symbol-macro sym)
                      (cond (found (convert exp env))
                            ((nth-value 1 (gethash sym *constants*))
                             (let ((v (car (gethash sym *constants*))))
                               (if (or (numberp v) (characterp v) (symbolp v) (stringp v))
                                   `(:const ,v)
                                   `(:sref ,sym))))
                            ((and *live-constant-hook* (nth-value 1 (funcall *live-constant-hook* sym)))
                             (let ((v (funcall *live-constant-hook* sym)))
                               ;; (a constant that a fasl cannot write, a
                               ;; package...: COMPILE-FILE would leave the
                               ;; form as source, compiled at every load)
                               (if (printable-constant-p v 3)
                                   `(:const ,v)
                                   `(:sref ,sym))))
                            (t (unless (special-var-p sym)
                                 (cwarn "undefined variable ~S" sym))
                               (typed-special-ref sym env))))))))))

(defun typed-special-ref (sym env)
  "A reference of the special variable SYM: of the type a declaration in ENV
or a proclamation gives it, as for a lexical variable (an integer type
checked, CHECKED-VALUE, the others trusted)."
  (let ((spec (and (not *suppress-var-types*) (or (free-type-decl env sym) (global-var-type sym)))))
    (cond ((null spec) `(:sref ,sym))
          ((and *check-declared-integers* (integer-decl-p spec))
           `(:the ,spec ,(let ((*ignore-tdecls* t) (*suppress-var-types* t))
                           (convert (checked-value sym spec) env))))
          (t `(:the ,spec (:sref ,sym))))))

(defvar *unit-env* nil
  "The environment of the toplevel forms of the compilation unit (COMPILE-FILE)
being compiled, or NIL (src/live.lisp).")

;;; Code evaluated in the image (the REPL, LOAD of a source file) may contain
;;; a copy of the body of a small global function defined there earlier, as
;;; whole-program compilation does: the copy is used as long as the symbol's
;;; function is the one whose source it is (the closure is compared at each
;;; call; a redefinition makes the call an ordinary one).
(defvar *live-inline-ok* nil "true while compiling a form that is evaluated at once in the image")
(defvar *live-inline-sources* (make-hash-table :test 'eq)
  "symbol -> (lambda-list body stale): the source of its definition; STALE is
the function the symbol had when the source was recorded")
(defvar *live-inline-closure-hook* nil "symbol -> the function made by its recorded definition, if it is still the symbol's function")
(defvar *live-inline-depth* 0)
(defparameter *live-inline-max-size* 60)
(defparameter *fasl-inline-max-depth* 1 "copies of functions of fasls in a copied body")
(defparameter *fasl-inline-max-size* 40 "the largest body (conses and atoms) of a function of a fasl that is copied")

(defun form-size (x limit)
  "The number of conses and atoms of X, or more than LIMIT."
  (let ((n 0))
    (labels ((walk (x)
               (when (> (incf n) limit) (return-from form-size n))
               (when (consp x) (walk (car x)) (walk (cdr x)))))
      (walk x))
    n))

(defun tree-mentions-p (x sym)
  (if (consp x) (or (tree-mentions-p (car x) sym) (tree-mentions-p (cdr x) sym)) (eq x sym)))

(defun uses-redefinable-macros-p (x)
  "X mentions a macro or a global symbol macro that is not COMMON-LISP's: a
copy of X made later could expand it differently than the function did."
  (cond ((consp x)
         (or (and (symbolp (car x)) (car x)
                  (not (eq (symbol-package (car x)) (symbol-package 'car)))
                  (macro-function-in (car x) nil))
             (uses-redefinable-macros-p (car x))
             (uses-redefinable-macros-p (cdr x))))
        ((and (symbolp x) x (not (keywordp x))) (nth-value 1 (global-symbol-macro x)))
        (t nil)))

(defun live-inline-source (sym fn-form)
  "(lambda-list body) of SYM's definition FN-FORM, #'(named-lambda name ll . body),
when it may be copied into callers compiled later, else NIL."
  (when (and (consp fn-form) (eq (car fn-form) 'function) (consp (second fn-form))
             (member (car (second fn-form)) '(clysm-sys::named-lambda lambda)))
    (let* ((l (second fn-form))
           (ll (if (eq (car l) 'lambda) (second l) (third l)))
           (body (if (eq (car l) 'lambda) (cddr l) (cdddr l))))
      (when (and (listp ll)
                 (notany (lambda (x) (member x '(&environment &whole))) ll)
                 (not (tree-mentions-p (cons ll body) sym))
                 (not (uses-redefinable-macros-p (cons ll body)))
                 ;; a loop stays in its own function (which the engine optimizes when it is called again)
                 (notany (lambda (op) (tree-mentions-p body op))
                         '(dotimes dolist do do* loop tagbody go prog prog* do-symbols do-external-symbols
                           do-all-symbols))
                 (<= (form-size body *live-inline-max-size*) *live-inline-max-size*))
        (list ll body)))))

(defun live-inline-record (sym fn-form)
  "Record the source of SYM's definition FN-FORM, #'(named-lambda name ll . body), when it may be copied."
  (remhash sym *live-inline-sources*)
  (let ((src (live-inline-source sym fn-form)))
    (when src
      (setf (gethash sym *live-inline-sources*)
            (list (first src) (second src)
                  (and *live-inline-closure-hook* (funcall *live-inline-closure-hook* sym))))
      t)))

;;; A small function defined at top level in a file given to COMPILE-FILE
;;; keeps its source in the fasl, with a mark of that compilation
;;; (SYS::%NOTE-FASL-INLINE: the property SYS::%FASL-INLINE of its name,
;;; (lambda-list body mark function)), and calls of it compiled in the image
;;; after the fasl was loaded (other files, the REPL) contain a guarded copy
;;; of its body: in a fasl the function the guard compares with is found when
;;; the code first runs and kept in a global of the module (%FASL-INLINE-GUARD
;;; calls SYS::%FASL-INLINE-FUNCTION: the function of the name if it is still
;;; the one of that definition, else an object that is no function), so that
;;; a redefinition, before or after, makes the call an ordinary one.
(defun fasl-inline-entry (op)
  "The recorded source of OP from a loaded fasl, (lambda-list body mark function), or NIL."
  (and (symbolp op) (fboundp op)
       (let ((e (get op 'clysm-sys::%fasl-inline)))
         (and (consp e) (= (length e) 4) (functionp (fourth e)) e))))

(defun live-inline-call (form)
  "FORM with a guarded copy of the body of the function it calls, or NIL."
  (let* ((op (car form))
         (ok (and *live-mode* (< *live-inline-depth* 2)
                  (not clysm-sys::*record-frames*)
                  ;; (not into the code of a toplevel form, which runs once)
                  *current-lam* (lam-parent *current-lam*)
                  (not (gethash op *notinline*)) (not (member op *local-notinline*))
                  (not (get op 'clysm-sys::%notinline))))
         (rec (and ok *live-inline-ok* *live-inline-closure-hook*
                   (gethash op *live-inline-sources*))))
    (when (and ok (not rec) (< *live-inline-depth* *fasl-inline-max-depth*)
               ;; (not a function of the file being compiled, called directly)
               (not (gethash op *global-defuns*)))
      (let ((e (fasl-inline-entry op)))
        (when (and e (eq (fourth e) (symbol-function op)))
          (let* ((temps (loop for nil in (cdr form) collect (gensym "ARG")))
                 (copy (inline-lambda-application op (first e) (second e) temps (cdr form))))
            (when copy
              (return-from live-inline-call
                `(let ,(mapcar #'list temps (cdr form))
                   (if ,(if *unit-env*
                            ;; (a fasl: the function is found when the code first runs)
                            `(clysm-sys::%fasl-inline-guard ',op ,(third e))
                            `(clysm-sys::%function-cell-eq ',op ',(fourth e)))
                       ,copy
                       (locally (declare (notinline ,op)) (,op ,@temps))))))))))
    (when rec
      (let ((closure (funcall *live-inline-closure-hook* op)))
        (when (and closure (not (eq closure (third rec))))
          (let* ((temps (loop for nil in (cdr form) collect (gensym "ARG")))
                 ;; (the body sees none of the caller's lexical bindings)
                 (copy (inline-lambda-application op (first rec) (second rec) temps (cdr form))))
            (when copy
              `(let ,(mapcar #'list temps (cdr form))
                 (if (clysm-sys::%function-cell-eq ',op ',closure)
                     ,copy
                     (locally (declare (notinline ,op)) (,op ,@temps)))))))))))

;;; In the image, a function declaimed INLINE that is defined at top level
;;; keeps its source (the property SYS::%INLINE-EXPANSION of its name,
;;; (LAMBDA lambda-list . body)), also in the fasl, and later calls of it in
;;; code compiled in the image (other files, the REPL) are replaced by a copy
;;; of its body, as SBCL does: no call is made and the types of the
;;; arguments flow into the body.  The body sees none of the caller's lexical
;;; bindings.  NOTINLINE (global or local) prevents it.
(defvar *inline-expansion-depth* 0)
(defparameter *inline-expansion-max-size* 400)

(defun inline-expansion-source (fn-form)
  "(LAMBDA lambda-list . body) for a definition #'(named-lambda name lambda-list . body)
that may be copied into the callers, or NIL."
  (when (and (consp fn-form) (eq (car fn-form) 'function) (consp (cdr fn-form))
             (consp (second fn-form)) (eq (car (second fn-form)) 'clysm-sys::named-lambda)
             (consp (cdr (second fn-form))) (consp (cddr (second fn-form))))
    (let* ((l (second fn-form)) (ll (third l)) (body (cdddr l)))
      (and (listp ll)
           (notany (lambda (x) (member x '(&environment &whole))) ll)
           (<= (form-size body *inline-expansion-max-size*) *inline-expansion-max-size*)
           (list* 'lambda ll body)))))

(defun inline-key-name (form)
  "The keyword a constant argument FORM names, or NIL."
  (cond ((keywordp form) form)
        ((and (consp form) (eq (car form) 'quote) (consp (cdr form)) (symbolp (second form)))
         (second form))
        (t nil)))

(defun inline-lambda-application (name ll body args &optional (key-forms args))
  "A form binding the parameters of lambda list LL to ARGS (forms) and
running BODY (with the block NAME) in a null lexical environment, or NIL when
the arguments do not fit (they are then passed by a call)."
  (let* ((pl (parse-lambda-list ll))
         (temps (loop for nil in args collect (gensym "A")))
         (rest-temps temps) (rest-args args)
         (bindings nil))
    (flet ((next () (pop rest-args) (pop rest-temps)))
      (dolist (p (getf pl :req))
        (unless rest-temps (return-from inline-lambda-application nil))
        (push (list p (next)) bindings))
      (dolist (o (getf pl :opt))
        (destructuring-bind (var &optional init (sv nil svp)) o
          (if rest-temps
              (progn (push (list var (next)) bindings)
                     (when svp (push (list sv t) bindings)))
              (progn (push (list var init) bindings)
                     (when svp (push (list sv nil) bindings))))))
      (when (getf pl :rest)
        (push (list (getf pl :rest) `(list ,@rest-temps)) bindings))
      (cond ((getf pl :keyp)
             (when (oddp (length rest-args)) (return-from inline-lambda-application nil))
             (let ((pairs (loop for (k nil) on (nthcdr (- (length args) (length rest-args)) key-forms) by #'cddr
                                for (nil tv) on rest-temps by #'cddr
                                collect (let ((kw (inline-key-name k)))
                                          (unless kw (return-from inline-lambda-application nil))
                                          (cons kw tv))))
                   (keys (mapcar (lambda (k)
                                   (destructuring-bind (spec &optional init (sv nil svp)) k
                                     (let ((var (if (consp spec) (second spec) spec))
                                           (kw (if (consp spec) (first spec) (intern (symbol-name spec) "KEYWORD"))))
                                       (list kw var init sv svp))))
                                 (getf pl :keys))))
               (unless (or (getf pl :aok)
                           (let ((a (assoc :allow-other-keys pairs)))
                             (and a (let ((form (nth (position (cdr a) temps) key-forms)))
                                      (and (constantp form) (not (null form)) (not (eq form 'nil)))))))
                 (dolist (p pairs)
                   (unless (or (eq (car p) :allow-other-keys) (find (car p) keys :key #'first))
                     (return-from inline-lambda-application nil))))
               (dolist (k keys)
                 (destructuring-bind (kw var init sv svp) k
                   (let ((p (assoc kw pairs)))
                     (push (list var (if p (cdr p) init)) bindings)
                     (when svp (push (list sv (if p t nil)) bindings)))))))
            ((and rest-temps (not (getf pl :rest)))
             (return-from inline-lambda-application nil)))
      (dolist (a (getf pl :aux))
        (push (list (first a) (second a)) bindings)))
    (multiple-value-bind (decls forms) (parse-body body)
      `(let ,(mapcar #'list temps args)
         (clysm-sys::%with-only-variables ,temps
           (let* ,(nreverse bindings)
             ,@(when decls `((declare ,@decls)))
             (block ,name ,@forms)))))))

(defun inline-expansion-call (form)
  "FORM, a call of a function with an inline expansion, as a copy of its body, or NIL."
  (let* ((op (car form))
         (src (and *live-mode* (symbolp op) (< *inline-expansion-depth* 3)
                   (not (gethash op *notinline*)) (not (member op *local-notinline*))
                   (not (get op 'clysm-sys::%notinline))
                   (get op 'clysm-sys::%inline-expansion))))
    (when (and (consp src) (eq (car src) 'lambda) (consp (cdr src)))
      (inline-lambda-application op (second src) (cddr src) (cdr form)))))

(defun convert-compound (form env)
  (let ((op (car form)))
    ;; local functions shadow everything
    (let ((fe (env-lookup-fun env op)))
      (when (and fe (eq (first fe) :fun))
        (return-from convert-compound
          `(:lcall ,(note-fun (third fe)) ,@(convert-args (cdr form) env)))))
    (let ((native (get op 'clysm-native)))
      (when native
        (return-from convert-compound (funcall native form env))))
    (when (member op *special-forms*)
      (return-from convert-compound (convert-special form env)))
    (let ((m (macro-function-in op env)))
      (when m
        (return-from convert-compound
          (convert (let ((*package* *read-package*)) (funcall m form env)) env))))
    ;; compiler macros
    (let ((cm (or (gethash op *compiler-macros*)
                  (and *live-compiler-macro-hook* (symbolp op) (funcall *live-compiler-macro-hook* op)))))
      (when (and cm (not (gethash op *notinline*)) (not (member op *local-notinline*)))
        (let ((new (let ((*package* *read-package*)) (funcall cm form env))))
          (unless (eq new form)
            (return-from convert-compound (convert new env))))))
    (let ((tr (and (symbolp op) (get op 'clysm-transform))))
      (when tr
        (let ((new (funcall tr form)))
          (unless (eq new form)
            (return-from convert-compound (convert new env))))))
    (let ((prim (find-primitive op (length (cdr form)))))
      (when prim
        (return-from convert-compound
          `(:prim ,prim ,@(convert-args (cdr form) env)))))
    (let ((expansion (inline-expansion-call form)))
      (when expansion
        (let ((node (handler-case
                        (let ((*inline-expansion-depth* (1+ *inline-expansion-depth*))) (convert expansion env))
                      ;; (a source that cannot be compiled here is not used again)
                      (error () (remprop op 'clysm-sys::%inline-expansion) nil))))
          (when node (return-from convert-compound node)))))
    (let ((inlined (and (symbolp op) (not (member op *local-notinline*)) (live-inline-call form))))
      (when inlined
        (return-from convert-compound
          (let ((*live-inline-depth* (1+ *live-inline-depth*))) (convert inlined env)))))
    ;; a function defined at run time (a generic function)
    (when (and (member op '(ensure-generic-function clysm-sys::%add-method-to clysm-sys::%defmethod
                            clysm-sys::%defgeneric))
               (consp (second form)) (eq (car (second form)) 'quote))
      (let ((name (second (second form))))
        (cond ((symbolp name) (setf (gethash name *dynamic-functions*) t))
              ((and (consp name) (eq (car name) 'setf) (consp (cdr name)) (symbolp (second name)))
               (setf (gethash (setf-function-symbol (second name)) *dynamic-functions*) t)))))
    `(:call ,op ,@(convert-args (cdr form) env))))

(defun note-fun (fun)
  (note-ref (fun-lvar fun))
  fun)

(defun convert-args (args env)
  (mapcar (lambda (a) (convert-value a env)) args))

(defun convert-lambda-application (form env)
  (destructuring-bind ((lambda-sym ll &rest body) &rest args) form
    (declare (ignore lambda-sym))
    (if (and (every #'symbolp ll)
             (notany (lambda (s) (member s lambda-list-keywords)) ll)
             (= (length ll) (length args)))
        (convert `(let ,(mapcar #'list ll args) ,@body) env)
        `(:funcall ,(convert `(function (lambda ,ll ,@body)) env)
                   ,@(convert-args args env)))))

;;; ------------------------------------------------------------
;;; Lambda conversion
;;; ------------------------------------------------------------

(defun parse-lambda-list (ll)
  "Returns plist with :req :opt :rest :keyp :keys :aok :aux :whole :env"
  (let ((req nil) (opt nil) (rest nil) (keyp nil) (keys nil) (aok nil) (aux nil)
        (state :req))
    (loop
      (when (null ll) (return))
      (when (atom ll) (setf rest ll) (return))  ; dotted rest
      (let ((x (pop ll)))
        (case x
          (&optional (setf state :opt))
          ((&rest &body) (setf rest (pop ll)) (setf state :after-rest))
          (&key (setf state :key keyp t))
          (&allow-other-keys (setf aok t))
          (&aux (setf state :aux))
          (t (ecase state
               (:req (push x req))
               (:opt (push (if (consp x) x (list x)) opt))
               (:key (push (if (consp x) x (list x)) keys))
               (:aux (push (if (consp x) x (list x)) aux)))))))
    (list :req (nreverse req) :opt (nreverse opt) :rest rest :keyp keyp
          :keys (nreverse keys) :aok aok :aux (nreverse aux))))

(defun convert-lambda (name ll body env &key block-name)
  "Convert a lambda; returns the LAM."
  (multiple-value-bind (decls forms) (parse-body body)
    (let* ((pl (parse-lambda-list ll))
           (dspecials (declared-specials decls))
           (lam (make-lam :id (incf *lam-counter*) :name name :parent *current-lam*))
           (*current-lam* lam)
           (*local-notinline* (append (declared-notinline decls) *local-notinline*))
           (env (if dspecials
                    (apply #'env-extend env
                           (loop for s in dspecials
                                 unless (member s ll) collect (list :special s)))
                    env)))
      (flet ((bind-var (sym)
               (let* ((sp (binding-special-p sym dspecials))
                      (lv (new-lvar sym sp)))
                 (setf env (env-extend env (if sp (list :special sym) (list :var sym lv))))
                 lv)))
        (setf (lam-req lam) (mapcar #'bind-var (getf pl :req)))
        (setf (lam-opt lam)
              (loop for (var init svar) in (getf pl :opt)
                    collect (let* ((init-node (convert init env))
                                   (lv (bind-var var))
                                   (sv (and svar (bind-var svar))))
                              (list lv init-node sv))))
        (when (getf pl :rest) (setf (lam-rest lam) (bind-var (getf pl :rest))))
        (setf (lam-keyp lam) (getf pl :keyp)
              (lam-allow-other-keys lam) (getf pl :aok))
        (setf (lam-keys lam)
              (loop for (spec init svar) in (getf pl :keys)
                    collect (multiple-value-bind (kw var)
                                (if (consp spec) (values (first spec) (second spec))
                                    (values (intern (symbol-name spec) :keyword) spec))
                              (let* ((init-node (convert init env))
                                     (lv (bind-var var))
                                     (sv (and svar (bind-var svar))))
                                (list kw lv init-node sv)))))
        (apply-type-decls decls (append (lam-req lam) (mapcar #'first (lam-opt lam))
                                        (when (lam-rest lam) (list (lam-rest lam)))
                                        (mapcar #'second (lam-keys lam))))
        (let ((body-forms (if block-name `((block ,block-name ,@forms)) forms)))
          (when (getf pl :aux)
            (setf body-forms `((let* ,(getf pl :aux) (declare ,@decls) ,@body-forms))))
          (setf (lam-body lam)
                (convert-body body-forms
                              (free-type-decl-env decls (append (getf pl :req) (mapcar #'first (getf pl :opt))
                                                                (mapcar #'third (getf pl :opt))
                                                                (and (getf pl :rest) (list (getf pl :rest)))
                                                                (mapcar (lambda (k) (let ((s (first k))) (if (consp s) (second s) s)))
                                                                        (getf pl :keys))
                                                                (mapcar #'third (getf pl :keys))
                                                                (mapcar #'first (getf pl :aux)))
                                                  env)))
          (when (and *live-mode* clysm-sys::*record-frames* (debug-frames-wanted-p decls))
            (setf (lam-body lam) (add-debug-frame lam (lam-body lam))))))
      lam)))

;;; Frames for the debugger (SWANK's frame locals): with
;;; SYSTEM::*RECORD-FRAMES*, a function compiled in the image makes a frame
;;; record, #(name variable-names nparams parent value...), on entry and
;;; binds SYSTEM::*DEBUG-FRAME-STACK* to it (PARENT is the frame of the
;;; caller, the previous value); the values of its
;;; parameters and of the variables it binds (LET, LET*,
;;; MULTIPLE-VALUE-BIND) are stored into it when they are bound or assigned
;;; (not by closures).  The binding makes calls in tail position ordinary calls.
;;; A function declared (OPTIMIZE (DEBUG 0)) or (DEBUG 1), or (SPEED 3)
;;; without DEBUG, has no frame record.
(defvar clysm-sys::*record-frames* nil)

(defun debug-frames-wanted-p (decls)
  (let ((debug nil) (speed nil))
    (dolist (d decls)
      (when (and (consp d) (eq (car d) 'optimize))
        (dolist (q (cdr d))
          (let ((name (if (consp q) (car q) q))
                (value (if (consp q) (second q) 3)))
            (case name (debug (setq debug value)) (speed (setq speed value)))))))
    (cond (debug (>= debug 2))
          (t (not (eql speed 3))))))

(defun add-debug-frame (lam body)
  (let* ((frame-sym (gensym "FRAME")) (var-sym (gensym "VAR"))
         (frame-lv (new-lvar frame-sym))
         (slots (make-hash-table :test 'eq)) (names nil) (n 0))
    (labels ((slot (lv)
               (when (and (lvar-p lv) (not (lvar-special lv)) (symbolp (lvar-name lv))
                          (symbol-package (lvar-name lv)))
                 (or (gethash lv slots)
                     (progn (push (lvar-name lv) names)
                            (setf (gethash lv slots) (prog1 (+ n 4) (incf n)))))))
             (store (lv)
               ;; (%set-svref frame slot lv), converted where FRAME and the variable are visible
               (let ((idx (slot lv)))
                 (when idx
                   (convert `(clysm-sys::%set-svref ,frame-sym ,idx ,var-sym)
                            (env-extend (make-cenv nil) (list :var frame-sym frame-lv)
                                        (list :var var-sym lv))))))
             (stores (lvs) (remove nil (mapcar #'store lvs)))
             (walk (x)
               (cond ((atom x) x)
                     ((keywordp (car x)) (node x))
                     (t (cons (walk (car x)) (walk (cdr x))))))
             (node (x)
               (case (car x)
                 ((:const :wasm :wasm-test :symtab :sref :go) x)
                 (t (let ((x (cons (car x) (walk (cdr x)))))
                      (case (car x)
                        (:let (destructuring-bind (bindings body) (cdr x)
                                (let ((s (stores (mapcar #'car bindings))))
                                  (if s `(:let ,bindings (:progn ,@s ,body)) x))))
                        (:mvbind (destructuring-bind (lvars v body) (cdr x)
                                   (let ((s (stores lvars)))
                                     (if s `(:mvbind ,lvars ,v (:progn ,@s ,body)) x))))
                        (:lset (let ((s (store (second x))))
                                 (if s `(:progn ,x ,s) x)))
                        (t x)))))))
      (let* ((params (append (lam-req lam)
                             (loop for (lv nil sv) in (lam-opt lam) collect lv when sv collect sv)
                             (when (lam-rest lam) (list (lam-rest lam)))
                             (loop for (nil lv nil sv) in (lam-keys lam) collect lv when sv collect sv)))
             (param-stores (stores params))
             (nparams n)
             (body (walk body))
             (env (env-extend (make-cenv nil) (list :var frame-sym frame-lv)))
             (stack-lv (new-lvar 'clysm-sys::*debug-frame-stack* t)))
        `(:let ((,frame-lv . ,(convert `(clysm-sys::%make-simple-vector ,(+ n 4) ',(symbol-value 'clysm-sys::%no-value))
                                       env)))
           (:progn
             ,(convert `(progn (clysm-sys::%set-svref ,frame-sym 0 ',(lam-name lam))
                               (clysm-sys::%set-svref ,frame-sym 1 ',(coerce (reverse names) 'simple-vector))
                               (clysm-sys::%set-svref ,frame-sym 2 ,nparams)
                               (clysm-sys::%set-svref ,frame-sym 3 clysm-sys::*debug-frame-stack*))
                       env)
             (:let ((,stack-lv . (:lref ,(note-ref frame-lv))))
               (:progn ,@param-stores ,body))))))))

(defun lambda-form-p (x) (and (consp x) (eq (car x) 'lambda)))

(defun convert-function-designator (spec env)
  (cond ((symbolp spec)
         (let ((fe (env-lookup-fun env spec)))
           (if (and fe (eq (first fe) :fun))
               `(:lref ,(note-ref (fun-lvar (third fe))))
               `(:fref ,spec))))
        ((and (consp spec) (eq (car spec) 'setf))
         (let ((fe (env-lookup-fun env spec)))
           (if (and fe (eq (first fe) :fun))
               `(:lref ,(note-ref (fun-lvar (third fe))))
               `(:fref ,(setf-function-symbol (second spec))))))
        ((lambda-form-p spec)
         `(:lambda ,(convert-lambda nil (second spec) (cddr spec) env)))
        ((and (consp spec) (eq (car spec) 'clysm-sys::named-lambda))
         (destructuring-bind (name ll &rest body) (cdr spec)
           `(:lambda ,(convert-lambda name ll body env
                                      :block-name (if (consp name) (second name) name)))))
        (t (error "bad function designator ~S" spec))))

(defun setf-function-symbol (sym)
  (let ((pkg (symbol-package sym)))
    (intern (format nil "(SETF ~A::~A)" (if pkg (target-package-name pkg) "") (symbol-name sym))
            :clysm-sys)))

;;; ------------------------------------------------------------
;;; Special forms
;;; ------------------------------------------------------------

(defun convert-special (form env)
  (let ((op (car form)))
    (ecase op
      (quote `(:const ,(second form)))
      (function (convert-function-designator (second form) env))
      (if (destructuring-bind (test then &optional else) (cdr form)
            `(:if ,(convert-value test env) ,(convert then env) ,(convert else env))))
      (progn (convert-body (cdr form) env))
      (let (convert-let (second form) (cddr form) env))
      (let* (convert-let* (second form) (cddr form) env))
      (setq (convert-setq (cdr form) env))
      ((flet labels) (convert-flet (second form) (cddr form) env (eq op 'labels)))
      (macrolet (convert-macrolet (second form) (cddr form) env))
      (symbol-macrolet
       (multiple-value-bind (decls forms) (parse-body (cddr form) :doc-allowed nil)
         (convert-locally decls forms
                          (apply #'env-extend env
                                 (loop for (s exp) in (second form) collect (list :smacro s exp))))))
      (locally (multiple-value-bind (decls forms) (parse-body (cdr form) :doc-allowed nil)
                 (convert-locally decls forms env)))
      (block (convert-block (second form) (cddr form) env))
      (return-from (convert-return-from (second form) (third form) env))
      (tagbody (convert-tagbody (cdr form) env))
      (go (convert-go (second form) env))
      (catch `(:catch ,(convert (second form) env) ,(convert-body (cddr form) env)))
      (throw `(:throw ,(convert (second form) env) ,(convert (third form) env)))
      (unwind-protect `(:unwind-protect ,(convert (second form) env)
                                        ,(convert-body (cddr form) env)))
      (multiple-value-call `(:mvcall ,(convert (second form) env) ,@(convert-args (cddr form) env)))
      (multiple-value-prog1 `(:mvprog1 ,(convert (second form) env) ,(convert-body (cddr form) env)))
      (the `(:the ,(second form) ,(convert (third form) env)))
      (eval-when (if (intersection (second form) '(:execute eval))
                     (convert-body (cddr form) env)
                     '(:const nil)))
      ;; the form is compiled inline in the current function (in a null lexical
      ;; environment), so blocks and tags inside it belong to that function
      (load-time-value `(:ltv ,(convert (second form) (make-cenv))))
      (progv `(:progv ,(convert (second form) env) ,(convert (third form) env)
                      ,(convert-body (cdddr form) env))))))

(defun convert-locally (decls forms env &optional bound)
  (let ((sp (declared-specials decls)))
    (with-notinline-decls (decls)
      (convert-body forms (free-type-decl-env
                           decls bound
                           (if sp (apply #'env-extend env (mapcar (lambda (s) (list :special s)) sp))
                               env))))))

(defun convert-let (bindings body env)
  (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
    (let* ((dspecials (declared-specials decls))
           (inits (loop for b in bindings
                        collect (convert-value (if (consp b)
                                                   (checked-value (second b) (declared-spec (first b) decls))
                                                   nil)
                                               env)))
           (lvars nil) (entries nil))
      (dolist (b bindings)
        (let* ((sym (if (consp b) (first b) b))
               (sp (binding-special-p sym dspecials))
               (lv (new-lvar sym sp)))
          (push lv lvars)
          (push (if sp (list :special sym) (list :var sym lv)) entries)))
      (setf lvars (nreverse lvars))
      (apply-type-decls decls lvars)
      (let ((free-specials (loop for s in dspecials
                                 unless (find s bindings :key (lambda (b) (if (consp b) (car b) b)))
                                   collect (list :special s))))
        (let ((new-env (free-type-decl-env
                        decls (mapcar (lambda (b) (if (consp b) (car b) b)) bindings)
                        (apply #'env-extend env (append (reverse entries) free-specials))))
              (*local-notinline* (append (declared-notinline decls) *local-notinline*)))
          (if (null bindings)
              (convert-body forms new-env)
              `(:let ,(mapcar #'cons lvars inits) ,(convert-body forms new-env))))))))

(defun convert-let* (bindings body env)
  (if (null bindings)
      (convert-let nil body env)
      (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
        (let ((dspecials (declared-specials decls)))
          ;; Nest, keeping declarations relevant to each variable.
          (labels ((nest (bs env)
                     (if (null bs)
                         (convert-locally (remove-if (lambda (d) (and (consp d) (eq (car d) 'special)))
                                                     decls)
                                          forms
                                          (apply #'env-extend env
                                                 (loop for s in dspecials
                                                       unless (find s bindings :key (lambda (b) (if (consp b) (car b) b)))
                                                         collect (list :special s)))
                                          (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
                         (let* ((b (car bs))
                                (sym (if (consp b) (first b) b))
                                (init (convert-value (if (consp b)
                                                         (checked-value (second b) (declared-spec sym decls))
                                                         nil)
                                                     env))
                                (sp (binding-special-p sym dspecials))
                                (lv (new-lvar sym sp))
                                (new-env (env-extend env (if sp (list :special sym) (list :var sym lv)))))
                           (apply-type-decls decls (list lv))
                           `(:let ((,lv . ,init)) ,(nest (cdr bs) new-env))))))
            (nest bindings env))))))

(defun convert-setq (pairs env)
  (when (oddp (length pairs)) (error "odd number of args to SETQ"))
  (let ((nodes
          (loop for (var val) on pairs by #'cddr
                collect
                (let ((e (env-lookup-var env var)))
                  (cond ((and e (eq (first e) :var))
                         (let ((lv (third e)))
                           (note-ref lv)
                           (setf (lvar-mutated lv) t)
                           `(:lset ,lv ,(convert-value (checked-value val (lvar-decl lv)) env))))
                        ((and e (eq (first e) :smacro))
                         (convert `(setf ,(third e) ,val) env))
                        ((and (null e) (nth-value 1 (global-symbol-macro var)))
                         (convert `(setf ,(global-symbol-macro var) ,val) env))
                        (t
                         (unless (or e (special-var-p var))
                           (cwarn "undefined variable ~S (setq)" var))
                         `(:sset ,var ,(convert val env))))))))
    (if (= (length nodes) 1) (first nodes) `(:progn ,@nodes))))

(defun convert-flet (defs body env recursive)
  (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
    (let* ((funs (loop for (name) in defs
                       collect (make-fun :name name :lvar (new-lvar name))))
           (inner-env (apply #'env-extend env
                             (loop for f in funs collect (list :fun (fun-name f) f))))
           (def-env (if recursive inner-env env)))
      (loop for f in funs for (name ll . fbody) in defs
            do (setf (fun-lam f)
                     (convert-lambda name ll fbody def-env
                                     :block-name (if (consp name) (second name) name))))
      ;; a LABELS function that refers to itself finds its closure in its
      ;; first parameter, not among its free variables
      (when recursive
        (dolist (f funs)
          (let ((lam (fun-lam f)))
            (when (member (fun-lvar f) (lam-free-vars lam))
              (setf (lam-free-vars lam) (remove (fun-lvar f) (lam-free-vars lam))
                    (lam-self lam) (fun-lvar f))))))
      `(,(if recursive :labels :flet) ,funs ,(convert-locally decls forms inner-env)))))

(defun make-host-macro-expander (name ll body)
  "Build a host lambda form (FORM ENV) for a macro."
  (let ((form (gensym "FORM")) (envv (gensym "ENV"))
        (whole nil) (envp nil) (clean nil))
    ;; extract &whole and &environment
    (loop while ll
          do (let ((x (if (consp ll) (pop ll) (prog1 ll (setf ll nil) (push '&rest clean)))))
               (cond ((eq x '&whole) (setf whole (pop ll)))
                     ((eq x '&environment) (setf envp (pop ll)))
                     (t (push x clean)))))
    (setf clean (nreverse clean))
    (multiple-value-bind (decls forms) (parse-body body)
      (let ((inner `(destructuring-bind ,clean (cdr ,form)
                      (declare ,@decls)
                      (block ,(if (consp name) (second name) name) ,@forms))))
        ;; &whole may be a destructuring pattern
        (when (consp whole)
          (setf inner `(destructuring-bind ,whole ,form ,inner) whole nil))
        `(lambda (,form ,envv)
           (declare (ignorable ,form ,envv))
           (let (,@(when whole `((,whole ,form)))
                 ,@(when envp `((,envp ,envv))))
             (declare (ignorable ,@(when whole (list whole)) ,@(when envp (list envp))))
             ,inner))))))

(defparameter *env-function-substitutions*
  '((get-setf-expansion . clysm-sys::%get-setf-expansion)
    (macroexpand-1 . clysm-sys::%macroexpand-1)
    (macroexpand . clysm-sys::%macroexpand)
    (macro-function . clysm-sys::%host-macro-function*))
  "CL functions taking an environment argument: host-evaluated macro bodies
must call the compiler's versions, which understand compiler environments.")

(defparameter *package-function-substitutions*
  '((find-package . clysm-sys::%host-find-package)
    (intern . clysm-sys::%host-intern)
    (find-symbol . clysm-sys::%host-find-symbol)
    (package-name . clysm-sys::%host-package-name))
  "Package functions called by host-evaluated code (macro bodies, #., eval-when):
target package names designate the host packages that mirror them.")

(defvar *substitutions* nil)

(defun substitute-host-functions (form)
  "Substitutions for code the compiler evaluates itself: the environment
functions (only when hosted by SBCL; in the self-hosted compiler the image's
own functions accept compiler environments) and the package functions."
  (let ((*substitutions* (append #-clysm *env-function-substitutions*
                                 *package-function-substitutions*)))
    (substitute-env-functions form)))

(defun substitute-env-functions (form)
  "Replace calls (and #') of the functions in *ENV-FUNCTION-SUBSTITUTIONS*,
leaving quoted data alone."
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        ;; a backquote template is data except where it is unquoted
        ((eq (car form) 'clysm-sys::quasiquote)
         (list (car form) (substitute-in-template (second form) 1)))
        ((and (eq (car form) 'function) (consp (cdr form)) (symbolp (second form)))
         (let ((e (assoc (second form) (or *substitutions* *env-function-substitutions*))))
           (if e (list 'function (cdr e)) form)))
        (t (let ((e (and (symbolp (car form)) (assoc (car form) (or *substitutions* *env-function-substitutions*)))))
             (cons (if e (cdr e) (substitute-env-functions (car form)))
                   (labels ((walk (tail)
                              (if (consp tail)
                                  (cons (substitute-env-functions (car tail)) (walk (cdr tail)))
                                  tail)))
                     (walk (cdr form))))))))

(defun substitute-in-template (x depth)
  (cond ((atom x) x)
        ((member (car x) '(clysm-sys::unquote clysm-sys::unquote-splicing))
         (list (car x) (if (= depth 1)
                           (substitute-env-functions (second x))
                           (substitute-in-template (second x) (1- depth)))))
        ((eq (car x) 'clysm-sys::quasiquote)
         (list (car x) (substitute-in-template (second x) (1+ depth))))
        (t (cons (substitute-in-template (car x) depth)
                 (substitute-in-template (cdr x) depth)))))

(defun host-compile (lambda-form)
  (setf lambda-form (substitute-host-functions lambda-form))
  (let ((*package* (find-package :clysm)))
    (handler-bind ((warning #'muffle-warning))
      (compile nil lambda-form))))

(defun local-macro-expander (lambda-form)
  "The expander of a local macro (MACROLET) made of LAMBDA-FORM.  In Clysm
it is interpreted: it runs a few times, while compiling it was a module of
its own, which made compiling a form with a LOOP (whose expansion has a
MACROLET of LOOP-FINISH) twice as long."
  #-clysm (host-compile lambda-form)
  #+clysm (let ((*package* (find-package :clysm)))
            (eval `(function ,(substitute-host-functions lambda-form)))))

(defun convert-macrolet (defs body env)
  (let ((new-env (apply #'env-extend env
                        (loop for (name ll . mbody) in defs
                              collect (list :macro name
                                            (local-macro-expander (make-host-macro-expander name ll mbody)))))))
    (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
      (convert-locally decls forms new-env))))

;;; BLOCK / RETURN-FROM

(defun convert-block (name body env)
  (let* ((blk (make-blk :name name :lam *current-lam*))
         (tag (new-lvar (gensym "BLOCK-TAG"))))
    (setf (blk-tag-var blk) tag)
    `(:block ,blk ,(convert-body body (env-extend env (list :block name blk))))))

(defun convert-return-from (name value env)
  (let ((e (env-lookup env :block name)))
    (unless e (error "return-from unknown block ~S" name))
    (let ((blk (third e)))
      (unless (eq (blk-lam blk) *current-lam*)
        (setf (blk-nonlocal blk) t)
        (note-ref (blk-tag-var blk)))
      `(:return-from ,blk ,(convert value env)))))

;;; TAGBODY / GO

(defun convert-tagbody (body env)
  (let* ((tb (make-tb :lam *current-lam*))
         (tags (loop for x in body when (or (symbolp x) (integerp x)) collect x))
         (new-env (apply #'env-extend env
                         (loop for tg in tags for i from 1 collect (list :tag tg tb i)))))
    (setf (tb-tag-var tb) (new-lvar (gensym "TAGBODY-TAG")))
    (setf (tb-tags tb) tags)
    ;; segments: index 0 = before first tag
    (let ((segments (list (list 0))) (i 0))
      (dolist (x body)
        (if (or (symbolp x) (integerp x))
            (push (list (incf i)) segments)
            (push (convert x new-env) (cdr (first segments)))))
      (setf segments (mapcar (lambda (s) (cons (car s) (reverse (cdr s)))) (nreverse segments)))
      (if (null tags)
          `(:progn ,@(cdr (first segments)) (:const nil))
          `(:tagbody ,tb ,segments)))))

(defun convert-go (tag env)
  (let ((e (dolist (e (cenv-entries env) nil)
             (when (and (eq (first e) :tag) (eql (second e) tag)) (return e)))))
    (unless e (error "go to unknown tag ~S" tag))
    (let ((tb (third e)))
      (unless (eq (tb-lam tb) *current-lam*)
        (setf (tb-nonlocal tb) t)
        (note-ref (tb-tag-var tb)))
      `(:go ,tb ,(fourth e)))))

;;; ------------------------------------------------------------
;;; Compiler-native operators (override macros of the same name)
;;; ------------------------------------------------------------

(defmacro defnative (name (form env) &body body)
  `(setf (get ',name 'clysm-native) (lambda (,form ,env) (declare (ignorable ,env)) ,@body)))

(defnative multiple-value-bind (form env)
  (destructuring-bind (vars value-form &rest body) (cdr form)
   ;; one variable (as SETF expands): a LET
   (if (and (consp vars) (null (cdr vars)))
    (convert `(let ((,(car vars) ,value-form)) ,@body) env)
    (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
      (let* ((dspecials (declared-specials decls))
             (vnode (convert value-form env))
             (lvars (let ((l (loop for v in vars collect (new-lvar v (binding-special-p v dspecials)))))
                      (apply-type-decls decls l)
                      l))
             (new-env (apply #'env-extend env
                             (append (loop for v in vars for lv in lvars
                                           collect (if (lvar-special lv) (list :special v) (list :var v lv)))
                                     (loop for s in dspecials unless (member s vars)
                                           collect (list :special s))))))
        `(:mvbind ,lvars ,vnode ,(convert-body forms (free-type-decl-env decls vars new-env))))))))

(defnative clysm-sys::%with-only-variables (form env)
  ;; (%with-only-variables (var...) form): FORM converted in a lexical
  ;; environment with only those variables of the current one
  (let ((*local-notinline* nil))
    (convert (third form)
             (make-cenv (loop for v in (second form)
                              for e = (env-lookup-var env v)
                              when e collect e)))))

;;; (sys:%ftype-raw (f ...)): the call, its value not checked (FTYPE-CALL-FORM)
(defnative clysm-sys::%ftype-raw (form env)
  (convert (second form) env))

(defnative clysm-sys::named-lambda (form env)
  (convert-function-designator form env))

(defnative lambda (form env)
  (convert-function-designator form env))

(defnative clysm-sys::quasiquote (form env)
  (convert (qq-expand (second form) 0) env))

(defnative funcall (form env)
  (let ((f (second form)))
    (cond ((and (consp f) (eq (car f) 'function) (symbolp (second f)))
           (convert `(,(second f) ,@(cddr form)) env))
          ((and (consp f) (eq (car f) 'quote) (symbolp (second f))
                (not (env-lookup-fun env (second f))))
           (convert `(,(second f) ,@(cddr form)) env))
          ((and (consp f) (eq (car f) 'function) (consp (second f)) (eq (car (second f)) 'setf)
                (not (env-lookup-fun env (second f))))
           (convert `(,(setf-function-symbol (second (second f))) ,@(cddr form)) env))
          (t `(:funcall ,(convert f env) ,@(convert-args (cddr form) env))))))

;;; (sys:%wasm result-type (args...) instrs...) — inline wasm with argument forms
(defnative clysm-sys::%wasm (form env)
  (destructuring-bind (rtype args &rest instrs) (cdr form)
    `(:wasm ,rtype ,(convert-args args env) ,instrs)))

;;; (sys:%catch tag-form body...) — CATCH for tags that only SYS:%THROW throws
;;; to while the %CATCH is active (fresh objects, as in HANDLER-CASE): the tag
;;; is not recorded in the list of active catch tags
(defnative clysm-sys::%catch (form env)
  `(:catch ,(convert (second form) env) ,(convert-body (cddr form) env) t))

;;; (sys:%wasm-test (args...) instrs...) — inline wasm producing an i32 boolean
(defnative clysm-sys::%wasm-test (form env)
  (destructuring-bind (args &rest instrs) (cdr form)
    `(:wasm-test ,(convert-args args env) ,instrs)))

(defnative clysm-sys::%symbol-table (form env)
  '(:symtab))

(defnative clysm-sys::%defun (form env)
  (destructuring-bind (qname fn-form) (cdr form)
    (let* ((name (second qname))
           (sym (if (consp name) (setf-function-symbol (second name)) name)))
      `(:defun ,sym ,(convert fn-form env)))))

;;; ------------------------------------------------------------
;;; source transforms for variadic arithmetic and comparisons
;;; ------------------------------------------------------------

(defmacro deftransform (name (form) &body body)
  `(setf (get ',name 'clysm-transform) (lambda (,form) ,@body)))

(defun fold-left (op args)
  (let ((acc (first args)))
    (dolist (a (rest args) acc) (setf acc (list op acc a)))))

(deftransform + (form)
  (let ((args (cdr form)))
    (cond ((null args) 0)
          ((cddr args) (fold-left '+ args))
          (t form))))
(deftransform * (form)
  (let ((args (cdr form)))
    (cond ((null args) 1)
          ((cddr args) (fold-left '* args))
          (t form))))
(deftransform - (form)
  (let ((args (cdr form)))
    (if (cddr args) (fold-left '- args) form)))

(dolist (op '(logand logior logxor))
  (let ((op op))
    (setf (get op 'clysm-transform)
          (lambda (form)
            (let ((args (cdr form)) (identity (if (eq op 'logand) -1 0)))
              (cond ((cddr args) (fold-left op args))
                    ((null args) identity)
                    ;; one argument: itself, checked to be an integer
                    ((null (cdr args)) (list op (car args) identity))
                    (t form)))))))

(defun eq-comparable-constant-p (x)
  "A literal for which EQL is EQ (fixnums, characters, symbols)."
  (let ((v (cond ((and (consp x) (eq (car x) 'quote) (consp (cdr x))) (list (second x)))
                 ((or (and (symbolp x) (or (keywordp x) (member x '(nil t)))) (characterp x) (integerp x))
                  (list x)))))
    (and v (let ((v (car v)))
             (or (symbolp v) (characterp v)
                 (and (integerp v) (<= +most-negative-fixnum+ v +most-positive-fixnum+)))))))

(deftransform eql (form)
  (if (and (= (length form) 3)
           (or (eq-comparable-constant-p (second form)) (eq-comparable-constant-p (third form))))
      `(eq ,(second form) ,(third form))
      form))

(defun eql-test-form-p (f)
  "F is #'EQL, 'EQL, #'EQ or 'EQ (an EQ test is EQL on the items EQ compares)."
  (and (consp f) (member (car f) '(function quote)) (consp (cdr f)) (null (cddr f))
       (member (second f) '(eql eq))))
(defun named-test-form (f)
  "EQUAL, EQUALP or STRING= when F is #'NAME or 'NAME of one of them."
  (and (consp f) (member (car f) '(function quote)) (consp (cdr f)) (null (cddr f))
       (car (member (second f) '(equal equalp string=)))))
(defun direct-test-name (kind test)
  (ecase test
    (equal (if (eq kind :member) 'clysm-sys::%member-equal 'clysm-sys::%assoc-equal))
    (equalp (if (eq kind :member) 'clysm-sys::%member-equalp 'clysm-sys::%assoc-equalp))
    (string= (if (eq kind :member) 'clysm-sys::%member-string= 'clysm-sys::%assoc-string=))))
(deftransform member (form)
  (let ((args (cdr form)))
    (cond ((and (= (length args) 4) (eq (third args) :test) (eql-test-form-p (fourth args))
                (eq (second (fourth args)) 'eql))
           `(clysm-sys::%member-eql ,(first args) ,(second args)))
          ((and (= (length args) 4) (eq (third args) :test) (named-test-form (fourth args)))
           `(,(direct-test-name :member (named-test-form (fourth args))) ,(first args) ,(second args)))
          ((/= (length args) 2) form)
          ((and (consp (second args)) (eq (car (second args)) 'quote)
                (let ((l (second (second args))))
                  (and (listp l) (null (cdr (last l))) (<= (length l) 64)
                       (every (lambda (x) (eq-comparable-constant-p (list 'quote x))) l))))
           `(clysm-sys::%memq ,@args))
          (t `(clysm-sys::%member-eql ,@args)))))
(deftransform assoc (form)
  (cond ((= (length form) 3) `(clysm-sys::%assoc-eql ,@(cdr form)))
        ((and (= (length form) 5) (eq (fourth form) :test) (eql-test-form-p (fifth form))
              (eq (second (fifth form)) 'eql))
         `(clysm-sys::%assoc-eql ,(second form) ,(third form)))
        ((and (= (length form) 5) (eq (fourth form) :test) (named-test-form (fifth form)))
         `(,(direct-test-name :assoc (named-test-form (fifth form))) ,(second form) ,(third form)))
        (t form)))
(deftransform rassoc (form)
  (if (= (length form) 3) `(clysm-sys::%rassoc-eql ,@(cdr form)) form))

(defun chain-compare (op args)
  (let ((temps (loop for nil in args collect (gensym "C"))))
    `(let ,(mapcar #'list temps args)
       (and ,@(loop for (a b) on temps while b collect (list op a b))))))

(dolist (op '(< > <= >= =))
  (let ((op op))
    (setf (get op 'clysm-transform)
          (lambda (form)
            (let ((args (cdr form)))
              (if (cddr args) (chain-compare op args) form))))))
