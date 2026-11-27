;;;; frontend.lisp — macroexpansion and conversion of Lisp forms to an AST.
;;;;
;;;; AST nodes are lists (:op ...).  Lexical variables are LVAR structures.
;;;; While converting we compute, for every lambda, the list of free lexical
;;;; variables (closure conversion) and which variables are captured/mutated
;;;; (assignment conversion: captured+mutated variables are boxed).

(in-package :clysm)

;;; The symbols the compiler makes, and those the macros of the program make
;;; when the compiler expands them (CL:GENSYM in what it evaluates is this:
;;; *PACKAGE-FUNCTION-SUBSTITUTIONS*), are numbered by a counter of its own,
;;; from 0 in each program (COMPILE-PROGRAM): the names of those that are in
;;; the module are the same whichever Lisp the compiler runs on.  CL:GENSYM
;;; also counted the host's own (SBCL compiling an expander, or Clysm's
;;; evaluator expanding the macros of one each time it runs).

;;; (at compile time too: the compiler's own macros call it, and when Clysm
;;; compiles the compiler they are evaluated before the file is loaded)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cgensym-counter* 0 "The number in the name of the next CGENSYM.")

  (defun cgensym (&optional (x "G"))
    "A new uninterned symbol, as GENSYM makes, numbered by *CGENSYM-COUNTER*."
    (let ((*print-base* 10) (*print-radix* nil))
      (if (integerp x)
          (make-symbol (concatenate 'string "G" (princ-to-string x)))
          (make-symbol (concatenate 'string (string x)
                                    (princ-to-string (prog1 *cgensym-counter* (incf *cgensym-counter*)))))))))

(defstruct (lvar (:print-function (lambda (v s d) (declare (ignore d))
                                    (format s "#<LVAR ~S>" (lvar-name v)))))
  name owner captured mutated special (refs 0) ignorable
  (sets 0)      ; how many of REFS are SETQs of it (CHECK-UNUSED: a variable only assigned is not used)
  (type nil)    ; union of the types of its values (types.lisp)
  (decl nil)    ; a declared type specifier (trusted)
  (rep nil)     ; :f64 / :i32 when held unboxed in a local of that type
  ;; the types of the nodes that name it (NODE-TYPE in types.lisp), for each
  ;; table of node types: (table count (node . type)...)...
  (ntys nil))

(defvar *current-source* nil
  "(FILE . LINE) of the toplevel form being compiled, or NIL: where the
functions made from it are, for the clysm.sources section (stack traces).")

(defvar *check-declared-integers*)
(defstruct (lam (:print-function (lambda (l s d) (declare (ignore d))
                                   (format s "#<LAM ~S ~D>" (lam-name l) (lam-id l)))))
  id name parent
  (source *current-source*)
  req opt rest keyp keys allow-other-keys
  body free-vars
  (toplevel nil)          ; global defun with no free vars
  (static-p nil)          ; closure object is a module constant
  (may-exit :unknown)     ; NIL when no call of it can return with an exit pending (codegen.lisp)
  (safety nil)            ; the SAFETY of its body (POLICY-SAFETY), NIL for the default
  (program-p *check-declared-integers*)   ; of a program's code, not the runtime's (set after FINISH-RUNTIME)
  (self nil))             ; the variable of a LABELS function referred to in its own body: its closure

(defun lam-simple-p (l)
  (and (null (lam-opt l)) (null (lam-rest l)) (not (lam-keyp l))))

(defstruct blk name lam tag-var (nonlocal nil) label (mv nil) dyn-depth uwp-depth saved-bsp uwp-frames
  (used nil)              ; a RETURN-FROM refers to it
  (rep :eqref))           ; the representation of its value (set by codegen)
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
(defvar *not-copied* (make-hash-table :test 'eq)
  "Functions proclaimed CLYSM-SYS::NOT-COPIED: called directly, as other
known functions are, but never copied into their callers (unlike NOTINLINE,
which also calls them through their symbols).  The typed, raw and catch
clones of a function, one per signature and not per call, are still made.
In the image the declaration is the symbol's property %NOT-COPIED.")
(defun not-copied-p (sym)
  "SYM is proclaimed CLYSM-SYS::NOT-COPIED (in the compilation, or in the image)."
  (or (gethash sym *not-copied*) (and (symbolp sym) (get sym 'clysm-sys::%not-copied))))
(defvar *redefinable-functions* nil
  "True while compiling files whose functions may be redefined in the running
image: they are all NOTINLINE, so their callers go through the function cell.")
(defvar *compiler-macros* (make-hash-table :test 'eq) "symbol -> host fn (form env)")
(defvar *global-defuns* (make-hash-table :test 'eq) "symbol -> list of lams defined at toplevel")
(defvar *lam-counter* 0)

;;; A batch: consecutive toplevel forms of a source file that LOAD compiles
;;; into one module (%LOAD-BATCH, runtime/eval.lisp), so that the calls
;;; between the functions they define are direct calls (which the engine can
;;; inline, with the types of the arguments and values flowing between them).
;;; The forms are still evaluated one by one, so a function of the batch may
;;; be redefined later (by another form, at the REPL, by TRACE): a call from
;;; one form of the batch to a function defined by another is guarded,
;;; (IF (%DEFUN-GUARD 'F) (direct call) (call through the function cell)),
;;; the guard comparing the function of F with the closure of its definition
;;; in the module.  (Calls between the functions of one form are direct, as
;;; when the form is compiled alone.)
(defvar *batch-forms* nil
  "In a batch: a table, symbol -> the index of the form of the batch that defines it.")
(defvar *batch-form-index* nil "In a batch: the index of the form being converted.")
(defvar *batch-unknown-calls* nil
  "In a batch: symbol -> the indexes of the forms calling it before a form defined it.")
(defvar *batch-no-direct* nil
  "In a batch: the functions that a form called unguarded before another form
defined them (they are not known functions of the module).")

(defun batch-guarded-p (op)
  "True when a call of OP, a function name, from the form being converted
must be guarded (OP is defined by another form of the batch)."
  (when (and *batch-forms* (symbolp op))
    (multiple-value-bind (k found) (gethash op *batch-forms*)
      (if found
          (/= k *batch-form-index*)
          (progn (pushnew *batch-form-index* (gethash op *batch-unknown-calls*)) nil)))))

(defun note-batch-defun (sym)
  "SYM is defined by the form being converted (of a batch)."
  (when *batch-forms*
    (multiple-value-bind (k found) (gethash sym *batch-forms*)
      (declare (ignore k))
      (unless found
        (setf (gethash sym *batch-forms*) *batch-form-index*)
        (when (some (lambda (i) (/= i *batch-form-index*)) (gethash sym *batch-unknown-calls*))
          (setf (gethash sym *batch-no-direct*) t))))))
(defvar *dynamic-functions* (make-hash-table :test 'eq) "functions defined at runtime (generic functions)")
(defvar *current-lam* nil)
(defvar *warnings* nil)
(defvar clysm-sys::*proclaimed-safety* 1
  "The SAFETY of the OPTIMIZE proclamations (DECLAIM, PROCLAIM): 1 unless one
says otherwise.  COMPILE-FILE binds it around a file, as SBCL's policy.")

;;; Live mode: the compiler runs inside a Clysm image and compiles code that
;;; is linked into that running image.  These hooks give access to the
;;; image's own macros, special declarations and constants.
(defvar *live-mode* nil)
(defvar *struct-constructors* (make-hash-table :test 'eq)
  "In the image: the name of a constructor of a structure -> the name of the structure.")

;;; A program compiled as a whole in the image that builds it
;;; (src/static.lisp): the compiler has two states there, the image's own
;;; (code compiled into the image: what the program's macros and EVAL-WHEN
;;; forms run) and that of the program being built.
(defvar *static-image* nil
  "While a program is compiled as a whole in the image that builds it:
:RUNTIME for the files the image itself is made of (what they define at
compile time is in effect in the image already), :LOADED for a file that is
loaded in the image and cannot be evaluated again (ASDF), :USER for the others.")
(defvar *static-build* nil
  "The program being built in this image: #(static-state live-state sink ...),
the states ((variable . value) ...) (src/static.lisp).")
(defvar *static-state-p* nil "True while the state of *STATIC-BUILD*'s program is bound.")

(defun call-in-live-state (thunk)
  "Call THUNK with the compiler in the image's own state: where it is when
no program is being built, or when the state of the program built is not
bound.  The program's state is kept, and bound again after (THUNK may have
added to the program: a system loaded by a form evaluated at compile time)."
  (if (not *static-state-p*)
      (funcall thunk)
      (let ((static (svref *static-build* 0)) (live (svref *static-build* 1)))
        (dolist (c static) (setf (cdr c) (symbol-value (car c))))
        (multiple-value-prog1
            (progv (mapcar #'car live) (mapcar #'cdr live)
              (unwind-protect (funcall thunk)
                (dolist (c live) (setf (cdr c) (symbol-value (car c))))))
          (dolist (c static) (set (car c) (cdr c)))))))

(defun table-put (variable key value &optional program-only)
  "Set KEY in the table that is the value of VARIABLE, and in that of the
program being built in this image (the form that records KEY was evaluated
in the image's state: an EVAL-WHEN of the program); with PROGRAM-ONLY, or
for a file of the runtime (what it records is the image's already), only in
the program's."
  (let ((table (symbol-value variable))
        (sink (and *static-build* (cdr (assoc variable (svref *static-build* 2))))))
    (cond ((or (null sink) (eq sink table)) (setf (gethash key table) value))
          (t
           (unless (or program-only (member (svref *static-build* 3) '(:runtime :loaded)))
             (setf (gethash key table) value))
           (setf (gethash key sink) value)))
    value))

(defun table-remove (variable key)
  "As TABLE-PUT: KEY is no longer in the tables."
  (let ((table (symbol-value variable))
        (sink (and *static-build* (cdr (assoc variable (svref *static-build* 2))))))
    (unless (and sink (not (eq sink table)) (member (svref *static-build* 3) '(:runtime :loaded)))
      (remhash key table))
    (when sink (remhash key sink))
    nil))

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

(defun batch-guarded-catch (form env)
  "(CATCH 'TAG (F ARG...)), F a function of another form of the batch and the
ARGs variables or constants: the form testing the guard before the CATCH, so
that the CATCH of the direct call may call F's copy whose throws to TAG are
returns (CATCH-CLONE-CALL), or NIL."
  (destructuring-bind (tag &rest body) (cdr form)
    (let ((call (car body)))
      (when (and (consp tag) (eq (car tag) 'quote) (consp (cdr tag)) (symbolp (second tag))
                 (= (length body) 1) (consp call) (symbolp (car call)) (car call)
                 (not (env-lookup-fun env (car call)))
                 (not (macro-function-in (car call) env)) (not (special-operator-p (car call)))
                 (not (gethash (car call) *notinline*)) (not (member (car call) *local-notinline*))
                 (not (gethash (car call) *compiler-macros*))
                 (not (and *live-compiler-macro-hook* (funcall *live-compiler-macro-hook* (car call))))
                 (ignore-errors (list-length (cdr call)))
                 (every (lambda (a)
                          (or (and (atom a) (not (symbolp a))) (keywordp a) (member a '(nil t))
                              (and (consp a) (eq (car a) 'quote))
                              (and (symbolp a) (let ((e (env-lookup-var env a))) (and e (eq (first e) :var))))))
                        (cdr call))
                 (batch-guarded-p (car call)))
        `(if (clysm-sys::%defun-guard ',(car call))
             (catch ,tag (clysm-sys::%batch-direct-call ,@call))
             (catch ,tag (clysm-sys::%batch-cell-call ,@call)))))))

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

(defun struct-class-type-p (spec)
  "SPEC names a structure class defined by DEFSTRUCT (without :TYPE) known
at compile time: a declaration whose identifier it is declares a type."
  (and spec (symbolp spec)
       (let ((d (get spec 'clysm-sys::%struct-description)))
         (and (consp d) (null (car d))))))

(defun apply-type-decls (decls lvars)
  "Record the types declared in DECLS for the variables LVARS."
  (dolist (d decls)
    (when (consp d)
      (multiple-value-bind (spec vars)
          (cond ((eq (car d) 'type) (values (second d) (cddr d)))
                ((member (car d) *decl-type-names*) (values (car d) (cdr d)))
                ((and (consp (car d)) (member (caar d) '(integer unsigned-byte signed-byte simple-array simple-vector simple-string
                                                    simple-bit-vector)))
                 (values (car d) (cdr d)))
                ((struct-class-type-p (car d)) (values (car d) (cdr d))))
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
                            (values (car d) (cdr d)))
                           ((struct-class-type-p (car d)) (values (car d) (cdr d))))
                   (and spec (list (cons spec vars))))))

(defun decls-safety (decls)
  "The SAFETY that an OPTIMIZE declaration among DECLS gives (the last), or NIL."
  (let ((found nil))
    (dolist (d decls found)
      (when (and (consp d) (eq (car d) 'optimize))
        (dolist (q (cdr d))
          (cond ((eq q 'safety) (setq found 3))
                ((and (consp q) (eq (car q) 'safety) (consp (cdr q)) (typep (second q) '(integer 0 3)))
                 (setq found (second q)))))))))

(defun policy-safety (env)
  "The SAFETY in effect in ENV: that of the innermost OPTIMIZE declaration
around (an entry (:SAFETY n) of ENV, FREE-TYPE-DECL-ENV), else the
proclaimed one."
  (dolist (e (and env (cenv-entries env)) clysm-sys::*proclaimed-safety*)
    (when (eq (first e) :safety) (return (second e)))))

(defun free-type-decl-env (decls bound env)
  "ENV with the free type declarations of DECLS (about lexical variables of
ENV that are not among the symbols BOUND by the form, nor declared where
they are bound), and the SAFETY of an OPTIMIZE declaration among them."
  (let ((entries nil))
    (let ((safety (decls-safety decls)))
      (when safety (push (list :safety safety) entries)))
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

(defun checked-decl-p (spec)
  "Whether a value given to a variable declared of type SPEC is checked (at
SAFETY 1 and more): an integer type, or a type the code relies on so that a
value of another would trap (DECL-CHECK-TY in src/codegen.lisp: a float, a
vector of a kind of storage, a cons, a function...), or a structure class."
  (or (integer-decl-p spec)
      (struct-class-type-p spec)
      (and spec (not (eq spec t)) (decl-check-ty spec) t)))

(defun declared-spec (sym decls)
  "The type DECLS declare for the variable SYM (the last one), or NIL."
  (let ((found nil))
    (dolist (d decls found)
      (when (consp d)
        (multiple-value-bind (spec vars)
            (cond ((eq (car d) 'type) (values (second d) (cddr d)))
                  ((member (car d) *decl-type-names*) (values (car d) (cdr d)))
                  ((and (consp (car d)) (member (caar d) '(integer unsigned-byte signed-byte simple-array
                                                           simple-vector simple-string simple-bit-vector)))
                   (values (car d) (cdr d)))
                  ((struct-class-type-p (car d)) (values (car d) (cdr d))))
          (when (and spec (member sym vars)) (setf found spec)))))))

(defun integer-decl-test (v spec)
  "A form that is true when the value of the variable V is of the integer type SPEC."
  (flet ((range (lo hi)
           ;; (exclusive bounds, (N), as the inclusive ones: Alexandria's
           ;; ARRAY-INDEX is (INTEGER 0 (ARRAY-DIMENSION-LIMIT)))
           (when (and (consp lo) (integerp (car lo))) (setf lo (1+ (car lo))))
           (when (and (consp hi) (integerp (car hi))) (setf hi (1- (car hi))))
           (let ((tests (append (and (integerp lo) (list `(<= ,lo ,v)))
                                (and (integerp hi) (list `(<= ,v ,hi))))))
             (if (and (integerp lo) (integerp hi)
                      (<= +most-negative-fixnum+ lo) (<= hi +most-positive-fixnum+))
                 `(and (clysm-sys::fixnump ,v) ,@tests)
                 `(and (clysm-sys::%integerp ,v) ,@tests)))))
    (cond ((eq spec 'fixnum) `(clysm-sys::fixnump ,v))
          ((and (eq (car spec) 'integer) (listp (cdr spec)))
           (range (second spec) (third spec)))
          ((and (eq (car spec) 'unsigned-byte) (integerp (second spec)))
           (range 0 (1- (expt 2 (second spec)))))
          ((and (eq (car spec) 'signed-byte) (integerp (second spec)))
           (range (- (expt 2 (1- (second spec)))) (1- (expt 2 (1- (second spec))))))
          ((eq (car spec) 'unsigned-byte) (range 0 nil))
          (t `(clysm-sys::%integerp ,v)))))

(defvar *check-declared-integers* t
  "True when the value given to a variable of a declared integer type is
checked (CHECKED-VALUE).  False while the runtime is compiled: its
declarations are kept by how it is written, and it must not pay for the checks.")

(defun trust-types-p (env)
  "Whether declared types are trusted without checks in ENV: (SAFETY 0), as
in SBCL.  The values are then of the types by declaration (CLHS 3.3.1: a
value not of the declared type is undefined behaviour); a FIXNUM is Clysm's
31-bit fixnum, and arithmetic stored into one is computed modulo 2^32 with
no test of overflow."
  (and *check-declared-integers* (eql (policy-safety env) 0)))

(defun checked-value (form spec &optional env)
  "FORM as the value of a variable declared to be of the integer type SPEC:
checked, since the code after it computes with the variable as a machine
integer (a value past the type would become another number, or a character).
The check is left out by the compiler where the type of FORM shows it holds.
At (SAFETY 0) in ENV the value is trusted: (THE SPEC FORM), unchecked."
  ;; (a literal NIL is how macros bind a variable before its first value,
  ;; under the declarations of their body: DOLIST's variable for its result form)
  (if (or (not *check-declared-integers*) (not (checked-decl-p spec)) (and (integerp form) (integer-decl-p spec))
          (null form))
      form
      (if (and env (trust-types-p env))
          `(the ,spec ,form)
          (let ((v (cgensym "V")))
            `(let ((,v ,form))
               (if ,(type-test-form v spec)
                   ,v
                   (clysm-sys::%signal-type-error ,v ',spec)))))))

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
                                collect (list p (cgensym (symbol-name p)) spec))))
            (cond
              ((null checks) fn-form)
              ;; (SAFETY 0): the parameters are declared of the types, unchecked
              ((and *check-declared-integers*
                    (eql (or (decls-safety decls) clysm-sys::*proclaimed-safety*) 0))
               `(function
                 (clysm-sys::named-lambda ,name ,ll
                   ,@(when doc (list doc))
                   (declare ,@(loop for (p nil spec) in checks collect `(type ,spec ,p)))
                   ,@(when decls `((declare ,@decls)))
                   ,@forms)))
              (t
                `(function
                  (clysm-sys::named-lambda ,name
                      ,(mapcar (lambda (p) (let ((c (assoc p checks))) (if c (second c) p))) ll)
                    ,@(when doc (list doc))
                    (let ,(loop for (p g spec) in checks collect `(,p ,(type-checked-form g spec)))
                      (declare ,@(loop for (p nil spec) in checks collect `(type ,spec ,p)))
                      ,@(when decls `((declare ,@decls)))
                      ,@forms)))))))))))

(defun ftype-call-form (form env)
  "FORM, a value that is not returned, with the value of a call of a function
of a proclaimed result type checked, or FORM."
  (let ((op (and (consp form) (car form))))
    (if (and op (symbolp op) (not (keywordp op))
             ;; (first the test that most calls fail: no FTYPE)
             (declared-ftype op)
             (not (member op *special-forms*))
             (not (get op 'clysm-native))
             (not (env-lookup-fun env op))
             (not (macro-function-in op env))
             (ftype-result-spec op))
        (let ((g (cgensym "V")) (spec (ftype-result-spec op)))
          (if (trust-types-p env)
              `(the ,spec (clysm-sys::%ftype-raw ,form))
              `(let ((,g (clysm-sys::%ftype-raw ,form))) ,(type-checked-form g spec))))
        form)))

(defun the-trust (env)
  "The tail of a :THE node converted in ENV: (:TRUST) at (SAFETY 0), where
the code may assume the type without keeping the representation of other
values (an integer arithmetic result declared FIXNUM is wrapped to 32 bits:
typed.lisp)."
  (when (trust-types-p env) '(:trust)))

(defun convert-value (form env)
  "Convert FORM, whose value is used but not returned."
  (convert (checked-the-form (ftype-call-form form env) env) env))

(defun checked-the-form (form env)
  "FORM, a value that is not returned; (THE type x) of a type CHECKED-DECL-P
gives, at SAFETY 1 and more, with X checked (CHECKED-VALUE), as SBCL checks
it (the type is that of the value from there on: a value of another would
trap).  (Not where the value is returned: X may have more values.)"
  (if (and (consp form) (eq (car form) 'the) (consp (cdr form)) (consp (cddr form)) (null (cdddr form))
           *check-declared-integers* (not (trust-types-p env))
           (checked-decl-p (second form)))
      `(the ,(second form) ,(checked-value (third form) (second form) env))
      form))

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
(defvar *local-inline* nil
  "function names declared INLINE in the forms being converted: a NOTINLINE
proclamation does not keep their inline expansions out there (esrap's
FIND-CHUNK, declaimed INLINE, defined, declaimed NOTINLINE, and inlined
where it is used by a (LOCALLY (DECLARE (INLINE FIND-CHUNK)) ...)).")
(defun declared-inline (decls)
  (loop for d in decls
        when (and (consp d) (eq (car d) 'inline)) append (mapcar #'notinline-name (cdr d))))
(defun local-notinline-after (decls)
  "*LOCAL-NOTINLINE* in the scope of DECLS (an INLINE there undoes an outer NOTINLINE)."
  (let ((n (declared-notinline decls)) (i (declared-inline decls)))
    (cond ((and (null n) (null i)) *local-notinline*)
          (t (append n (if i (remove-if (lambda (s) (member s i)) *local-notinline*) *local-notinline*))))))
(defun local-inline-after (decls)
  "*LOCAL-INLINE* in the scope of DECLS."
  (let ((n (declared-notinline decls)) (i (declared-inline decls)))
    (cond ((and (null n) (null i)) *local-inline*)
          (t (append i (if n (remove-if (lambda (s) (member s n)) *local-inline*) *local-inline*))))))
(defmacro with-notinline-decls ((decls) &body body)
  `(let* ((*local-inline* (local-inline-after ,decls))
          (*local-notinline* (local-notinline-after ,decls)))
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

;;; Variables never used: with CLYSM_WARN_UNUSED in the environment, a
;;; lexical variable of a program's code (not of the runtime's) that nothing
;;; refers to when its binding form has been converted is warned of, as a
;;; compiler does (SBCL's style warning).  It is told after macroexpansion:
;;; a variable that a MACROLET's expansion reads is used, and a binding of a
;;; special variable is not a lexical one.  IGNORE and IGNORABLE declarations
;;; quiet it; so does a name that is not interned (a macro's GENSYM).

(defvar *warn-unused* nil
  "True when variables never used are warned of (CLYSM_WARN_UNUSED, read by
RESET-COMPILER); :ERROR when they are also an error at the end (FAIL-ON-UNUSED).")

(defvar *unused-warned* nil
  "The (source name function) of the warnings of variables never used already
given: a form converted again (a batch that falls back) is not told twice.")

(defun warn-unused-from-environment ()
  "Set *WARN-UNUSED* from CLYSM_WARN_UNUSED (set and not empty; \"error\": :ERROR)
and forget the warnings given."
  (let ((value #+sbcl (sb-ext:posix-getenv "CLYSM_WARN_UNUSED")
               #-sbcl (sys::%getenv "CLYSM_WARN_UNUSED")))
    (setf *warn-unused* (cond ((or (null value) (zerop (length value))) nil)
                              ((string-equal value "error") :error)
                              (t t))
          *unused-warned* nil)))

(defun fail-on-unused ()
  "With CLYSM_WARN_UNUSED=error, signal an error when variables never used were told:
the compilation fails (after all were told)."
  (when (and (eq *warn-unused* :error) *unused-warned*)
    (error "~D variable~:P never used (CLYSM_WARN_UNUSED=error)" (length *unused-warned*))))

;;; (the toplevel form as it was read: a variable it does not name was
;;; bound by a macro, which CHECK-UNUSED says)
(defvar *current-toplevel-form* nil
  "The toplevel form being compiled as it was read, or NIL (COMPILE-SOURCE-FORMS).")

(defun note-ignored-outer (decls env bound)
  "IGNORE and IGNORABLE declarations in DECLS of variables of ENV that the
form does not bind (not in BOUND): as a macro puts a body's declarations in a
form inside the binding, the variables are not told of either."
  (dolist (name (declared-ignored decls))
    (unless (member name bound)
      (let ((e (env-lookup-var env name)))
        (when (and e (eq (first e) :var))
          (setf (lvar-ignorable (third e)) t))))))

(defun lambda-list-names (ll)
  "The symbols of the lambda list LL that are not lambda list keywords (a superset of the variables it binds)."
  (let ((names nil))
    (labels ((walk (x)
               (cond ((consp x) (walk (car x)) (walk (cdr x)))
                     ((and x (symbolp x) (not (member x lambda-list-keywords))) (push x names)))))
      (walk ll))
    names))

(defun declared-ignored (decls)
  "The variables DECLS declare IGNORE or IGNORABLE."
  (loop for d in decls
        when (and (consp d) (member (car d) '(ignore ignorable)))
          append (remove-if-not #'symbolp (cdr d))))

(defun enclosing-name ()
  "The name of the nearest named function being converted, or NIL."
  (loop for l = *current-lam* then (lam-parent l)
        while l
        when (and (lam-name l) (not (and (symbolp (lam-name l)) (null (symbol-package (lam-name l))))))
          return (lam-name l)))

(defun check-unused (lvars decls)
  "Warn of each of LVARS, just bound and its scope converted, that nothing refers to."
  (when (and *warn-unused* *check-declared-integers*)
    (let ((ignored (declared-ignored decls)))
      (dolist (lv lvars)
        (let ((name (and lv (lvar-name lv))))
          ;; (a variable that is only assigned, as LOOP's destructuring and
          ;; DESTRUCTURING-BIND's expansions do, is not read either)
          (when (and lv (= (lvar-refs lv) (lvar-sets lv)) (not (lvar-special lv)) (not (lvar-ignorable lv))
                     (symbolp name) (symbol-package name)
                     (not (member name ignored)))
            (let* ((function (enclosing-name))
                   (key (list *current-source* name function)))
              (unless (member key *unused-warned* :test #'equal)
                (push key *unused-warned*)
                (cwarn "variable ~S is ~:[never used~;assigned but never read~]~@[ in ~S~]~@[ (~A~]~@[:~D)~]~:[~;, bound by a macro~]"
                       name (plusp (lvar-sets lv)) function (car *current-source*) (cdr *current-source*)
                       (and *current-toplevel-form* (not (tree-mentions-p *current-toplevel-form* name))))))))))))

(defun binding-special-p (sym decl-specials)
  (or (member sym decl-specials) (special-var-p sym)))

;;; ------------------------------------------------------------
;;; Conversion
;;; ------------------------------------------------------------

;;; The constants of a whole program are globals of the module, made of
;;; numbers, characters, symbols, strings, conses, simple vectors and arrays
;;; (CONST-GLOBAL).  A program built in the image (src/static.lisp) may have
;;; any object that a fasl can hold as a constant (a vector of octets, a hash
;;; table, a pathname, a list that contains itself: read by #. or made by a
;;; macro of a library): such a one is made when the program starts, read
;;; from its text as a fasl writes it or by its MAKE-LOAD-FORM.

(defun plain-constant-p (obj)
  "OBJ can be a constant of a whole program as it is (CONST-GLOBAL)."
  (let ((seen nil))
    (labels ((enter (x)
               ;; true if X is being walked (a cycle); it is then noted
               (unless seen (setq seen (make-hash-table :test 'eq)))
               (case (gethash x seen)
                 (:open (return-from plain-constant-p nil))
                 (:done nil)
                 (t (setf (gethash x seen) :open) t)))
             (walk (x)
               (cond ((or (symbolp x) (numberp x) (characterp x)) t)
                     ((consp x)
                      (let ((cells nil))
                        (do ((l x (cdr l)))
                            ((atom l) (walk l))
                          (unless (enter l) (return))
                          (push l cells)
                          (walk (car l)))
                        (dolist (c cells) (setf (gethash c seen) :done))))
                     ((stringp x) (or (simple-string-p x) (return-from plain-constant-p nil)))
                     ((simple-vector-p x)
                      (when (enter x)
                        (dotimes (i (length x)) (walk (svref x i)))
                        (setf (gethash x seen) :done)))
                     ((simple-bit-vector-p x) t)
                     ((and (arrayp x) (not (vectorp x)) (eq (array-element-type x) t)
                           (not (adjustable-array-p x)))
                      (when (enter x)
                        (dotimes (i (array-total-size x)) (walk (row-major-aref x i)))
                        (setf (gethash x seen) :done)))
                     (t (return-from plain-constant-p nil)))))
      (walk obj)
      t)))

;;; Such a constant is a cell (a cons, which the module holds) whose car is
;;; the object once it is made and whose cdr is the function that makes it,
;;; set by a toplevel form put before the one being compiled
;;; (*STATIC-PENDING-FORMS*): the object is made when it is first referred
;;; to, once, and every reference of the program to the same object is to
;;; that one.  An instance, a structure or a hash table is made by its
;;; MAKE-LOAD-FORM (the objects in its forms the same way); anything else is
;;; read from its text as a fasl writes it, the instances in it by reference
;;; (SYS::%FASL-REF, as in a fasl).

(defvar *static-literal-cells* nil "A program built in the image: object -> its cell (an EQ table).")
(defvar *static-cell-conses* nil
  "A program built in the image: the cells of *STATIC-LITERAL-CELLS* (an EQ
table).  Each is a quoted cons that the program's code changes (RPLACA,
RPLACD) and all look alike, (NIL), when they are compiled: CONST-GLOBAL
must keep each one a cons of its own, and none may be the last cons of
another literal list that ends with NIL.")
(defvar *static-pending-forms* nil
  "A program built in the image: toplevel forms to put before the one being converted, last first.")
(defvar *static-literal-counter* 0)

(defun static-literal-builder (obj cell)
  "The form of a function of no arguments that makes OBJ, puts it in CELL and returns it."
  (if (clysm-sys::%externalizable-p obj)
      (multiple-value-bind (creation init)
          (let ((r nil)) (call-in-live-state (lambda () (setq r (multiple-value-list (make-load-form obj)))))
            (values (first r) (second r)))
        (let ((o (cgensym "OBJECT")))
          `(lambda ()
             (let ((,o ,creation))
               (rplaca ',cell ,o)
               ,@(when init (list init))
               ,o))))
      (let* ((inner (clysm-sys::%literal-objects obj nil))
             (numbers (mapcar (lambda (y) (declare (ignore y)) (incf *static-literal-counter*)) inner))
             (memo (make-hash-table :test 'eq)))
        (labels ((copy (x)
                   (cond ((clysm-sys::%externalizable-p x)
                          (clysm-sys::%make-fasl-marker "static" (nth (position x inner) numbers)))
                         ((consp x)
                          (or (gethash x memo)
                              (let ((c (cons nil nil)))
                                (setf (gethash x memo) c)
                                ;; (along the list in a loop)
                                (do ((l x (cdr l)) (tail c))
                                    (nil)
                                  (setf (car tail) (copy (car l)))
                                  (let ((next (cdr l)))
                                    (cond ((and (consp next) (not (gethash next memo)))
                                           (let ((n (cons nil nil)))
                                             (setf (gethash next memo) n (cdr tail) n tail n)))
                                          (t (setf (cdr tail) (copy next)) (return)))))
                                c)))
                         ((simple-vector-p x)
                          (or (gethash x memo)
                              (let ((v (make-array (length x))))
                                (setf (gethash x memo) v)
                                (dotimes (i (length x) v) (setf (svref v i) (copy (svref x i)))))))
                         (t x))))
          (let ((text (let ((printed (if inner (copy obj) obj)))
                        (clysm-sys::%call-with-fasl-syntax (lambda () (prin1-to-string printed))))))
            `(lambda ()
               ,@(loop for y in inner for n in numbers
                       collect `(clysm-sys::%fasl-set "static" ,n ',y))
               (car (rplaca ',cell
                            (clysm-sys::%call-with-fasl-syntax
                             (lambda () (values (read-from-string ,text))))))))))))

;;; Literals that are similar are made one object (CONST-GLOBAL) only within
;;; one source file, which is all that CLHS 3.2.4.4 allows of COMPILE-FILE: a
;;; whole program is many files, of systems that know nothing of each other,
;;; and a file that changes a quoted list of its own (undefined, but done:
;;; (defvar *entries* '(nil)) of a test framework, then (setf (cdr *entries*)
;;; ...)) must not change the literals of another file nor the records that
;;; the compiler makes for its own code.  The scope of a literal is the file
;;; whose form it was first seen in, noted when the form is converted: the
;;; function that code is generated for may be of another file (a copy of an
;;; inline function).

(defvar *literal-scopes* nil
  "Whole-program compilation: literal object -> its scope (an EQ table), the
name of the source file it was first seen in, :PROGRAM (a form of the program
that is of no file), or :RUNTIME (the runtime's own and what the compiler
makes for itself: their conses are never shared).")

(defun current-literal-scope ()
  (if *check-declared-integers* (or (car *current-source*) :program) :runtime))

(defun note-literal-scope (obj)
  (when (and *literal-scopes* (not *live-mode*) (or (consp obj) (arrayp obj)))
    (multiple-value-bind (scope found) (gethash obj *literal-scopes*)
      (declare (ignore scope))
      (unless found (setf (gethash obj *literal-scopes*) (current-literal-scope))))))

(defun note-compiler-literal (obj)
  "OBJ, a literal of code that the compiler makes (the record of a
discriminating function), shares no cons with the literals of the program."
  (when (and *literal-scopes* (not *live-mode*))
    (setf (gethash obj *literal-scopes*) :runtime))
  obj)

(defun constant-node (obj env)
  "The node of the constant OBJ."
  (declare (ignorable env))
  (note-literal-scope obj)
  (if (and (member *static-image* '(:user :loaded)) (not (or (symbolp obj) (numberp obj) (characterp obj)))
           (not (plain-constant-p obj)))
      (let ((cell (gethash obj *static-literal-cells*)) (c (cgensym "CELL")))
        (unless cell
          (setq cell (setf (gethash obj *static-literal-cells*) (list nil)))
          (setf (gethash cell *static-cell-conses*) t)
          (push `(rplacd ',cell ,(static-literal-builder obj cell)) *static-pending-forms*))
        (convert `(let ((,c ',cell)) (or (car ,c) (funcall (cdr ,c)))) (make-cenv)))
      `(:const ,obj)))

(defun convert (form env)
  (cond ((symbolp form) (convert-symbol form env))
        ((atom form) (constant-node form env))
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
                          ((and *check-declared-integers* (checked-decl-p spec) (not (trust-types-p env)))
                           `(:the ,spec ,(let ((*ignore-tdecls* t)) (convert (checked-value sym spec) env))))
                          (t `(:the ,spec (:lref ,(note-ref (third e))) ,@(the-trust env))))))
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
                               (if (and (printable-constant-p v 3)
                                        ;; (a program built in the image: as a
                                        ;; constant of its own files, above)
                                        (or (not *static-image*)
                                            (numberp v) (characterp v) (symbolp v) (stringp v)))
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
          ((and *check-declared-integers* (checked-decl-p spec) (not (trust-types-p env)))
           `(:the ,spec ,(let ((*ignore-tdecls* t) (*suppress-var-types* t))
                           (convert (checked-value sym spec) env))))
          (t `(:the ,spec (:sref ,sym) ,@(the-trust env))))))

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
(defvar *live-toplevel-loop-p* nil "the form compiled in the image has a loop: its toplevel code gets copies too")
(defparameter *loop-operators*
  '(dotimes dolist do do* loop tagbody go prog prog* do-symbols do-external-symbols do-all-symbols))
(defparameter *live-inline-max-size* 60)
(defvar *fasl-guard-globals* (make-hash-table :test 'equal) "(symbol code . mark code) -> the global of a guard (prims.lisp)")
(defvar *fasl-guards-set* (make-hash-table :test 'equal) "(symbol . mark) whose guard global the unit sets")
(defvar *fasl-guards-pending* nil "(symbol . mark) of the copies made by the toplevel form being processed")
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

(defun form-has-loop-p (form &optional (limit 5000))
  "FORM mentions an operator of iteration, looking at no more than LIMIT
conses (a literal may be circular)."
  (let ((n 0))
    (labels ((walk (x)
               (cond ((consp x)
                      (when (> (incf n) limit) (return-from form-has-loop-p nil))
                      (walk (car x)) (walk (cdr x)))
                     ((and x (symbolp x) (member x *loop-operators*))
                      (return-from form-has-loop-p t)))))
      (walk form)
      nil)))

(defun strip-return-froms (x name)
  "X with the block name NAME of its (RETURN-FROM NAME ...) forms left out."
  (cond ((atom x) x)
        ((and (eq (car x) 'return-from) (consp (cdr x)) (eq (cadr x) name))
         (cons 'return-from (strip-return-froms (cddr x) name)))
        (t (let ((a (strip-return-froms (car x) name)) (d (strip-return-froms (cdr x) name)))
             (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))))

(defun tree-mentions-p (x sym)
  (if (consp x) (or (tree-mentions-p (car x) sym) (tree-mentions-p (cdr x) sym)) (eq x sym)))

(defun tree-calls-p (x sym)
  "X mentions SYM other than in a quoted object ((FIND-CLASS 'VEC3) in the
constructor VEC3 is no call of it)."
  (cond ((atom x) (eq x sym))
        ((and (eq (car x) 'quote) (consp (cdr x)) (null (cddr x))) nil)
        (t (or (tree-calls-p (car x) sym) (tree-calls-p (cdr x) sym)))))

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
                 (<= (form-size body *live-inline-max-size*) *live-inline-max-size*)
                 (notany (lambda (x) (member x '(&environment &whole))) ll)
                 ;; (not recursive; a RETURN-FROM of its block is copied with the block)
                 (not (tree-calls-p (strip-return-froms (cons ll body) sym) sym))
                 (not (uses-redefinable-macros-p (cons ll body)))
                 ;; a loop stays in its own function (which the engine optimizes when it is called again)
                 (notany (lambda (op) (tree-mentions-p body op)) *loop-operators*)
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
;;; of its body: in a fasl the guard (%FASL-INLINE-GUARD) compares the
;;; function of the name with a global of the module, one for each function
;;; and mark, that a toplevel form after the first form making such a copy
;;; sets (%FASL-INLINE-GUARD-SET, from SYS::%FASL-INLINE-FUNCTION: the
;;; function of the name if it is still the one of that definition, else an
;;; object that is no function); until then it is null and the call is made.
;;; So a redefinition, before or after, makes the call an ordinary one.
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
                  ;; (not into the code of a toplevel form, which runs once,
                  ;; unless it has a loop)
                  *current-lam* (or (lam-parent *current-lam*) *live-toplevel-loop-p*)
                  (not (gethash op *notinline*)) (not (member op *local-notinline*))
                  (not (get op 'clysm-sys::%notinline)) (not (not-copied-p op))
                  ;; (not a function that signals, SYS::%TYPE-ERROR: its
                  ;; call is on a path not expected to be taken, where a
                  ;; copy, ERROR with its arguments, only makes the code
                  ;; larger; as INLINE-CANDIDATE)
                  (not (cold-function-p op))))
         (rec (and ok *live-inline-ok* *live-inline-closure-hook*
                   (gethash op *live-inline-sources*))))
    (when (and ok (not rec) (< *live-inline-depth* *fasl-inline-max-depth*)
               ;; (not a function of the file being compiled, called directly)
               (not (gethash op *global-defuns*)))
      (let ((e (fasl-inline-entry op)))
        (when (and e (eq (fourth e) (symbol-function op)))
          (let* ((temps (loop for nil in (cdr form) collect (cgensym "ARG")))
                 (copy (inline-lambda-application op (first e) (second e) temps (cdr form))))
            (when copy
              (return-from live-inline-call
                `(let ,(mapcar #'list temps (cdr form))
                   (if ,(if *unit-env*
                            ;; (a fasl: the function is found when the module runs)
                            (progn (pushnew (cons op (third e)) *fasl-guards-pending* :test #'equal)
                                   `(clysm-sys::%fasl-inline-guard ',op ,(third e)))
                            `(clysm-sys::%function-cell-eq ',op ',(fourth e)))
                       ,copy
                       (locally (declare (notinline ,op)) (,op ,@temps))))))))))
    ;; a small function of another form of the batch (defined before this
    ;; one): a copy guarded by the comparison with its definition in the module
    ;; (not of a later form: the source recorded is then that of a
    ;; definition before the batch, whose copy is guarded by its own closure)
    (when (and rec (batch-guarded-p op) (< (gethash op *batch-forms*) *batch-form-index*))
      (let* ((temps (loop for nil in (cdr form) collect (cgensym "ARG")))
             (copy (inline-lambda-application op (first rec) (second rec) temps (cdr form))))
        (return-from live-inline-call
          (and copy
               `(let ,(mapcar #'list temps (cdr form))
                  (if (clysm-sys::%defun-guard ',op)
                      ,copy
                      (clysm-sys::%batch-cell-call ,op ,@temps)))))))
    (when rec
      (let ((closure (funcall *live-inline-closure-hook* op)))
        (when (and closure (not (eq closure (third rec))))
          (let* ((temps (loop for nil in (cdr form) collect (cgensym "ARG")))
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

(defun inline-lambda-application (name ll body args &optional (key-forms args) (depth-var '*live-inline-depth*))
  "A form binding the parameters of lambda list LL to ARGS (forms) and
running BODY (with the block NAME) in a null lexical environment, or NIL when
the arguments do not fit (they are then passed by a call).  The body (not
the arguments, which are the caller's code) is converted with DEPTH-VAR,
the depth of copies, one more."
  (let* ((pl (parse-lambda-list ll))
         (temps (loop for nil in args collect (cgensym "A")))
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
         (clysm-sys::%inline-copy ,depth-var
           (clysm-sys::%with-only-variables ,temps
             (let* ,(nreverse bindings)
               ,@(when decls `((declare ,@decls)))
               (block ,name ,@forms))))))))

(defvar *program-inline-expansions* nil
  "In a whole program, once its runtime is compiled (FINISH-RUNTIME): symbol
-> the source (LAMBDA lambda-list . body) of a function of the program
declaimed INLINE whose lambda list has more than required parameters
(PROCESS-DEFUN).  The copies of functions of required parameters are made of
their nodes (src/inline.lisp), which the others are not: a call of such a
function is the copy of its source, as in code compiled in the image, so
that the types of the arguments given and of the value reach it (Babel's
DECODING-ERROR, &OPTIONAL, in each arm of its decoders: without the copy
the code point of every arm was boxed).")

(defun inline-expansion-call (form)
  "FORM, a call of a function with an inline expansion, as a copy of its body, or NIL."
  (let* ((op (car form))
         (src (and (symbolp op) (< *inline-expansion-depth* 3)
                   (not (member op *local-notinline*)) (not (not-copied-p op))
                   (if *live-mode*
                       (and (or (member op *local-inline*)
                                (and (not (gethash op *notinline*)) (not (get op 'clysm-sys::%notinline))))
                            (get op 'clysm-sys::%inline-expansion))
                       (and *program-inline-expansions*
                            (not (gethash op *notinline*))
                            (gethash op *program-inline-expansions*))))))
    (when (and (consp src) (eq (car src) 'lambda) (consp (cdr src)))
      (inline-lambda-application op (second src) (cddr src) (cdr form) (cdr form) '*inline-expansion-depth*))))

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
    ;; integer arithmetic on constants, done now: (- (expt 2 31)) was two
    ;; calls each time it ran
    (multiple-value-bind (v found) (constant-integer-form form env)
      (when found (return-from convert-compound `(:const ,v))))
    ;; compiler macros
    (let* ((host-cm (gethash op *compiler-macros*))
           (cm (or host-cm
                   (and *live-compiler-macro-hook* (symbolp op) (funcall *live-compiler-macro-hook* op)))))
      (when (and cm (not (gethash op *notinline*)) (not (member op *local-notinline*)))
        (let ((new (let ((*package* *read-package*))
                     (if host-cm
                         (funcall cm form env)
                         ;; (a compiler macro defined in the image whose
                         ;; expansion fails, such as one calling a function of
                         ;; its file not defined yet, is not used: the call
                         ;; stays as it is)
                         (handler-case (funcall cm form env) (error () form))))))
          (unless (eq new form)
            (return-from convert-compound (convert new env))))))
    ;; a call of a generic function whose methods the unit being compiled
    ;; defines or that was defined before: a call of its discriminating
    ;; function (runtime/clos.lisp, "Dispatch compiled as code"; the
    ;; generic functions of one or two arguments with a compiler macro for
    ;; their call sites get there from it)
    (when (and clysm-sys::*gf-unit* (symbolp op) (not (gethash op *notinline*))
               (not (member op *local-notinline*)))
      (let ((new (clysm-sys::%gf-unit-site form)))
        (when new (return-from convert-compound (convert new env)))))
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
                        (convert expansion env)
                      ;; (a source that cannot be compiled here is not used again)
                      (error () (remprop op 'clysm-sys::%inline-expansion) nil))))
          (when node (return-from convert-compound node)))))
    (let ((inlined (and (symbolp op) (not (member op *local-notinline*)) (live-inline-call form))))
      (when inlined
        (return-from convert-compound (convert inlined env))))
    ;; a function defined at run time (a generic function)
    (when (and (member op '(ensure-generic-function clysm-sys::%add-method-to clysm-sys::%defmethod
                            clysm-sys::%defgeneric))
               (consp (second form)) (eq (car (second form)) 'quote))
      (let ((name (second (second form))))
        (cond ((symbolp name) (setf (gethash name *dynamic-functions*) t))
              ((and (consp name) (eq (car name) 'setf) (consp (cdr name)) (symbolp (second name)))
               (setf (gethash (setf-function-symbol (second name)) *dynamic-functions*) t)))))
    ;; a function of another form of the batch: a guarded direct call
    ;; (NOTINLINE: through the function cell)
    (when (and (not (gethash op *notinline*)) (batch-guarded-p op))
      (let ((temps (loop for nil in (cdr form) collect (cgensym "ARG"))))
        (return-from convert-compound
          (convert (if (member op *local-notinline*)
                       `(clysm-sys::%batch-cell-call ,op ,@(cdr form))
                       `(let ,(mapcar #'list temps (cdr form))
                          (if (clysm-sys::%defun-guard ',op)
                              (clysm-sys::%batch-direct-call ,op ,@temps)
                              (clysm-sys::%batch-cell-call ,op ,@temps))))
                   env))))
    `(:call ,op ,@(convert-args (trusted-ftype-args op (cdr form) env) env))))

(defvar *defun-param-specs* nil
  "(name . specs) of the global function being converted: the integer types
declared for its required parameters (NIL for one not declared so).")

(defun param-decl-specs (ll decls)
  "The integer types DECLS declare for the required parameters of the lambda list LL."
  (loop for p in ll
        until (member p lambda-list-keywords)
        collect (let ((spec (and (symbolp p) (declared-spec p decls))))
                  (and spec (integer-decl-p spec) spec))))

(defun known-param-specs (op)
  "The integer types declared for the required parameters of the global
function OP of this compilation (or being converted), or NIL."
  (if (and *defun-param-specs* (eq (car *defun-param-specs*) op))
      (cdr *defun-param-specs*)
      (let ((lam (and (fboundp 'known-global-lam) (known-global-lam op))))
        (and lam (loop for lv in (lam-req lam)
                       collect (let ((spec (lvar-decl lv))) (and spec (integer-decl-p spec) spec)))))))

(defun trusted-ftype-args (op args env)
  "ARGS of a call of the global function OP; at (SAFETY 0), those of the
types OP's FTYPE gives as (THE type arg), trusted (as SBCL does), and else
those of the integer types its parameters are declared of (a known
function of this compilation, whose code takes them so unchecked)."
  (let ((specs (and (symbolp op) (not (env-lookup-fun env op)) (trust-types-p env)
                    (not (gethash op *notinline*)) (not (member op *local-notinline*))
                    (or (ftype-arg-specs op) (known-param-specs op)))))
    (if (and specs (<= (length specs) (length args)))
        (loop for a in args
              for i from 0
              collect (let ((spec (nth i specs)))
                        (if (and spec (ftype-useful-spec-p spec)) `(the ,spec ,a) a)))
        args)))

(defparameter *foldable-integer-functions*
  '(+ - * expt ash 1+ 1- logand logior logxor lognot abs min max integer-length
    mod rem gcd lcm isqrt logcount))

(defconstant +fold-integer-bits+ 4096
  "The longest integer made by folding arithmetic on constants (512 octets
of a module's data; SBCL folds any).")

(defun constant-integer-form (form env)
  "(values N T) when FORM is an integer known when it is compiled: a literal,
a constant variable, or a call of one of *FOLDABLE-INTEGER-FUNCTIONS* on such
forms whose value is an integer of at most +FOLD-INTEGER-BITS+ bits."
  (flet ((small (n) (and (integerp n) (<= (integer-length n) +fold-integer-bits+))))
    (cond ((integerp form) (values form t))
          ((and (consp form) (eq (car form) 'quote) (consp (cdr form)) (null (cddr form))
                (integerp (second form)))
           (values (second form) t))
          ((and (symbolp form) form (not (eq form t)) (not (keywordp form))
                (not (env-lookup-var env form))
                (not (nth-value 1 (global-symbol-macro form))))
           (multiple-value-bind (v found) (gethash form *constants*)
             (cond ((and found (integerp (car v))) (values (car v) t))
                   ((and (not found) *live-constant-hook*)
                    (multiple-value-bind (v found) (funcall *live-constant-hook* form)
                      (if (and found (integerp v)) (values v t) (values nil nil))))
                   (t (values nil nil)))))
          ((and (consp form) (symbolp (car form))
                (member (car form) *foldable-integer-functions*)
                (not (env-lookup-fun env (car form)))
                (not (gethash (car form) *notinline*))
                (not (member (car form) *local-notinline*))
                (handler-case (list-length form) (error () nil)))
           (let ((args nil))
             (dolist (a (cdr form))
               (multiple-value-bind (v found) (constant-integer-form a env)
                 (unless (and found (small v)) (return-from constant-integer-form (values nil nil)))
                 (push v args)))
             (setf args (nreverse args))
             ;; (no huge powers or shifts: the length of the result is
             ;; bounded before it is computed)
             (when (and (member (car form) '(expt ash))
                        (not (and (= (length args) 2)
                                  (<= (abs (second args)) +fold-integer-bits+)
                                  (or (eq (car form) 'ash)
                                      (<= (* (integer-length (first args)) (abs (second args)))
                                          +fold-integer-bits+)))))
               (return-from constant-integer-form (values nil nil)))
             (let ((v (handler-case (apply (car form) args) (error () nil))))
               (if (small v) (values v t) (values nil nil)))))
          (t (values nil nil)))))

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

(defun struct-param-checks (lam env)
  "Forms testing that the parameters of LAM declared of a structure class
are of it (at SAFETY 1 and more: the code after takes them to be, an object
of another type would trap where an accessor casts it).  The parameters of
other types are tested by the code generator, before their values are
unboxed (PARAM-CHECK-CODE)."
  (when (and *check-declared-integers* (not (trust-types-p env)))
    (loop for lv in (append (lam-req lam) (mapcar #'first (lam-opt lam)) (mapcar #'second (lam-keys lam)))
          for spec = (lvar-decl lv)
          when (and spec (symbolp spec) (struct-class-type-p spec) (not (lvar-special lv)) (symbolp (lvar-name lv)))
            collect `(unless (typep (clysm-sys::%untyped ,(lvar-name lv)) ',spec)
                       (clysm-sys::%signal-type-error ,(lvar-name lv) ',spec)))))

(defun convert-lambda (name ll body env &key block-name)
  "Convert a lambda; returns the LAM."
  (multiple-value-bind (decls forms) (parse-body body)
    (note-ignored-outer decls env (lambda-list-names ll))
    (let* ((pl (parse-lambda-list ll))
           (dspecials (declared-specials decls))
           (lam (make-lam :id (incf *lam-counter*) :name name :parent *current-lam*))
           (*current-lam* lam)
           (*local-inline* (local-inline-after decls))
           (*local-notinline* (local-notinline-after decls))
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
          (let ((body-env (free-type-decl-env decls (append (getf pl :req) (mapcar #'first (getf pl :opt))
                                                            (mapcar #'third (getf pl :opt))
                                                            (and (getf pl :rest) (list (getf pl :rest)))
                                                            (mapcar (lambda (k) (let ((s (first k))) (if (consp s) (second s) s)))
                                                                    (getf pl :keys))
                                                            (mapcar #'third (getf pl :keys))
                                                            (mapcar #'first (getf pl :aux)))
                                              env)))
            (setf (lam-safety lam) (and *check-declared-integers* (policy-safety body-env)))
            (setf (lam-body lam) (convert-body (append (struct-param-checks lam body-env) body-forms) body-env))
            (check-unused (append (lam-req lam)
                                  (loop for (lv nil sv) in (lam-opt lam) collect lv collect sv)
                                  (when (lam-rest lam) (list (lam-rest lam)))
                                  (loop for (nil lv nil sv) in (lam-keys lam) collect lv collect sv))
                          decls))
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
  (let* ((frame-sym (cgensym "FRAME")) (var-sym (cgensym "VAR"))
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

(defun function-cell-node (sym)
  "The node of the function that SYM, the name of a function defined in the
module, has when the code runs: its function cell, read in place (the
helper signals UNDEFINED-FUNCTION when it is empty).  (FDEFINITION of the
name was a call of SYS::%FUNCTION-NAME-SYMBOL and SYMBOL-FUNCTION's tests
for a macro, some 300 octets at each #'F of a function of another file of
the system.)"
  (let ((l (cgensym "FCELL")))
    `(:the function
           (:wasm :eqref ()
                  ((:block ,l :eqref
                     (:global.get ,(sym-global sym)) (:struct.get :symbol ,+sym-function+) (:br_on_non_null ,l)
                     (:global.get ,(sym-global sym)) (:call :fdefinition)))))))

(defun convert-function-designator (spec env)
  (cond ((symbolp spec)
         (let ((fe (env-lookup-fun env spec)))
           (cond ((and fe (eq (first fe) :fun)) `(:lref ,(note-ref (fun-lvar (third fe)))))
                 ;; (a function of another form of a batch: its current definition)
                 ((batch-guarded-p spec) (function-cell-node spec))
                 (t `(:fref ,spec)))))
        ((and (consp spec) (eq (car spec) 'setf))
         (let ((fe (env-lookup-fun env spec)))
           (cond ((and fe (eq (first fe) :fun)) `(:lref ,(note-ref (fun-lvar (third fe)))))
                 ((batch-guarded-p (setf-function-symbol (second spec)))
                  (convert `(fdefinition ',spec) env))
                 (t `(:fref ,(setf-function-symbol (second spec)))))))
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
      (quote (constant-node (second form) env))
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
      (catch (let ((guarded (batch-guarded-catch form env)))
               (if guarded
                   (convert guarded env)
                   `(:catch ,(convert (second form) env) ,(convert-body (cddr form) env)))))
      (throw `(:throw ,(convert (second form) env) ,(convert (third form) env)))
      (unwind-protect `(:unwind-protect ,(convert (second form) env)
                                        ,(convert-body (cddr form) env)))
      (multiple-value-call (or (fixed-mvcall-form form env)
                               `(:mvcall ,(convert (second form) env) ,@(convert-args (cddr form) env))))
      (multiple-value-prog1 `(:mvprog1 ,(convert (second form) env) ,(convert-body (cddr form) env)))
      (the `(:the ,(second form) ,(convert (third form) env) ,@(the-trust env)))
      (eval-when (if (intersection (second form) '(:execute eval))
                     (convert-body (cddr form) env)
                     '(:const nil)))
      ;; the form is compiled inline in the current function (in a null lexical
      ;; environment), so blocks and tags inside it belong to that function
      (load-time-value `(:ltv ,(convert (second form) (make-cenv))))
      (progv `(:progv ,(convert (second form) env) ,(convert (third form) env)
                      ,(convert-body (cdddr form) env))))))

(defun convert-locally (decls forms env &optional bound)
  (note-ignored-outer decls env bound)
  (let ((sp (declared-specials decls)))
    (with-notinline-decls (decls)
      (convert-body forms (free-type-decl-env
                           decls bound
                           (if sp (apply #'env-extend env (mapcar (lambda (s) (list :special s)) sp))
                               env))))))

(defun convert-let (bindings body env)
  (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
    (note-ignored-outer decls env (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
    (let* ((dspecials (declared-specials decls))
           (inits (loop for b in bindings
                        collect (convert-value (if (consp b)
                                                   (checked-value (second b) (declared-spec (first b) decls) env)
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
              (*local-inline* (local-inline-after decls))
              (*local-notinline* (local-notinline-after decls)))
          (if (null bindings)
              (convert-body forms new-env)
              (let ((body-node (convert-body forms new-env)))
                (check-unused lvars decls)
                `(:let ,(mapcar #'cons lvars inits) ,body-node))))))))

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
                                                         (checked-value (second b) (declared-spec sym decls) env)
                                                         nil)
                                                     env))
                                (sp (binding-special-p sym dspecials))
                                (lv (new-lvar sym sp))
                                (new-env (env-extend env (if sp (list :special sym) (list :var sym lv)))))
                           (apply-type-decls decls (list lv))
                           (let ((inner (nest (cdr bs) new-env)))
                             (check-unused (list lv) decls)
                             `(:let ((,lv . ,init)) ,inner))))))
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
                           (incf (lvar-sets lv))
                           (setf (lvar-mutated lv) t)
                           `(:lset ,lv ,(convert-value (checked-value val (lvar-decl lv) env) env))))
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
  (let ((form (cgensym "FORM")) (envv (cgensym "ENV"))
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
    (macro-function . clysm-sys::%host-macro-function*)
    (constantp . clysm-sys::%host-constantp))
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
functions, the package functions and GENSYM (CGENSYM).  The environment
functions on both Lisps: the self-hosted compiler's image takes compiler
environments in its own, but a macro's expander that expands a form with
them (MACROEXPAND-1 in the compiler macro of STRING=) then had the image's
macros (those the host expands, PROCESS-DEFMACRO), not the compiler's."
  (let ((*substitutions* (append *env-function-substitutions*
                                 *package-function-substitutions*
                                 '((gensym . cgensym)))))
    (substitute-env-functions form)))

;;; The substitutions keep each cons under which nothing is replaced (REUSE-
;;; CONS): a form is substituted more than once (the expander of a macro in
;;; the self-hosted compiler is interpreted, then compiled:
;;; GLOBAL-MACRO-EXPANDER), and the data of its backquotes is what its
;;; expansions return.  A copy was other literals than the expansions before
;;; it gave, where the runtime's code shares none (CONST-GLOBAL-1).

(defun reuse-cons (x a d)
  "X when its car is A and its cdr D, else a cons of them."
  (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d)))

(defun substitute-env-functions (form)
  "Replace calls (and #') of the functions in *ENV-FUNCTION-SUBSTITUTIONS*,
leaving quoted data alone."
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        ;; a backquote template is data except where it is unquoted
        ((eq (car form) 'clysm-sys::quasiquote)
         (let ((template (substitute-in-template (second form) 1)))
           (if (eq template (second form)) form (list (car form) template))))
        ((and (eq (car form) 'function) (consp (cdr form)) (symbolp (second form)))
         (let ((e (assoc (second form) (or *substitutions* *env-function-substitutions*))))
           (if e (list 'function (cdr e)) form)))
        (t (let ((e (and (symbolp (car form)) (assoc (car form) (or *substitutions* *env-function-substitutions*)))))
             (reuse-cons form
                         (if e (cdr e) (substitute-env-functions (car form)))
                         (labels ((walk (tail)
                                    (if (consp tail)
                                        (reuse-cons tail (substitute-env-functions (car tail)) (walk (cdr tail)))
                                        tail)))
                           (walk (cdr form))))))))

(defun substitute-in-template (x depth)
  (cond ((atom x) x)
        ((member (car x) '(clysm-sys::unquote clysm-sys::unquote-splicing))
         (let* ((in (second x))
                (out (if (= depth 1)
                         (substitute-env-functions in)
                         (substitute-in-template in (1- depth)))))
           (if (eq out in) x (list (car x) out))))
        ((eq (car x) 'clysm-sys::quasiquote)
         (let ((out (substitute-in-template (second x) (1+ depth))))
           (if (eq out (second x)) x (list (car x) out))))
        (t (reuse-cons x (substitute-in-template (car x) depth)
                       (substitute-in-template (cdr x) depth)))))

(defun host-compile (lambda-form)
  (setf lambda-form (substitute-host-functions lambda-form))
  (let ((*package* (find-package :clysm)))
    (handler-bind ((warning #'muffle-warning))
      (call-in-live-state (lambda () (compile nil lambda-form))))))

(defun local-macro-expander (lambda-form)
  "The expander of a local macro (MACROLET) made of LAMBDA-FORM.  In Clysm
it is interpreted: it runs a few times, while compiling it was a module of
its own, which made compiling a form with a LOOP (whose expansion has a
MACROLET of LOOP-FINISH) twice as long."
  #-clysm (host-compile lambda-form)
  #+clysm (let ((*package* (find-package :clysm)))
            (call-in-live-state
             (lambda () (eval `(function ,(substitute-host-functions lambda-form)))))))

(defparameter *macro-compile-after* 8
  "How many expansions of a global macro defined in the image (DEFMACRO in
COMPILE-FILE, LOAD or at the REPL) are interpreted before its expander is
compiled (GLOBAL-MACRO-EXPANDER).")

(defun global-macro-expander (lambda-form)
  "The expander of a global macro (DEFMACRO) made of LAMBDA-FORM.  In Clysm
the first expansions are interpreted and the expander is compiled when it
is used more: most macros of a file are expanded a few times, and
compiling one is a module of its own."
  #-clysm (host-compile lambda-form)
  #+clysm (let ((interpreted (local-macro-expander lambda-form)) (compiled nil) (uses 0))
            (lambda (form env)
              (cond (compiled (funcall compiled form env))
                    ((< (setq uses (+ uses 1)) *macro-compile-after*)
                     (funcall interpreted form env))
                    (t (setq compiled (host-compile lambda-form))
                       (funcall compiled form env))))))

(defun convert-macrolet (defs body env)
  (let ((new-env (apply #'env-extend env
                        (loop for (name ll . mbody) in defs
                              collect (list :macro name
                                            (local-macro-expander (make-host-macro-expander name ll mbody)))))))
    (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
      (convert-locally decls forms new-env))))

;;; BLOCK / RETURN-FROM

;;; MULTIPLE-VALUE-CALL (and MULTIPLE-VALUE-LIST, NTH-VALUE...) of forms
;;; whose number of values is known when the form is compiled: a constant or
;;; a variable, (VALUES ...), a call of a function of the file whose every
;;; tail returns the same number of values.  The values are bound to
;;; variables and the function called with them (a direct call, inline
;;; arithmetic for #'+), instead of collecting them in a list and calling
;;; the function's entry of any number of arguments with a vector of them.

(defun ast-nvalues (node &optional (depth 0))
  "The number of values NODE always returns, or NIL if not known."
  (case (car node)
    ((:const :lref :sref :fref :lset :sset :lambda) 1)
    (:values (length (cdr node)))
    (:progn (if (cdr node) (ast-nvalues (car (last node)) depth) 1))
    ((:let :flet :labels) (ast-nvalues (third node) depth))
    (:mvbind (ast-nvalues (fourth node) depth))
    (:the (ast-nvalues (third node) depth))
    (:if (let ((a (ast-nvalues (third node) depth)))
           (and a (eql a (ast-nvalues (fourth node) depth)) a)))
    (:block (and (not (blk-nonlocal (second node)))
                 (not (block-returned-to-p (second node) (third node)))
                 (ast-nvalues (third node) depth)))
    (:call (and (< depth 3) (known-call-nvalues (second node) (1+ depth))))
    (t nil)))

(defun known-call-nvalues (sym depth)
  (let ((lam (and (symbolp sym) (known-global-lam sym))))
    (and lam (ast-nvalues (lam-body lam) depth))))

(defparameter *cl-nvalues*
  '((floor . 2) (ceiling . 2) (truncate . 2) (round . 2) (ffloor . 2) (fceiling . 2) (ftruncate . 2)
    (fround . 2) (gethash . 2) (parse-integer . 2) (read-from-string . 2) (decode-float . 3)
    (integer-decode-float . 3) (find-symbol . 2) (intern . 2) (get-decoded-time . 9)
    (decode-universal-time . 9) (macroexpand . 2) (macroexpand-1 . 2) (get-properties . 3)
    (+ . 1) (- . 1) (* . 1) (/ . 1) (1+ . 1) (1- . 1) (car . 1) (cdr . 1) (cons . 1) (list . 1)
    (list* . 1) (length . 1) (aref . 1) (svref . 1) (char . 1) (schar . 1) (elt . 1) (nth . 1)
    (first . 1) (second . 1) (third . 1) (rest . 1) (not . 1) (null . 1) (eq . 1) (eql . 1)
    (equal . 1) (vector . 1) (make-list . 1) (reverse . 1) (append . 1) (gensym . 1)
    (symbol-name . 1) (string . 1) (format . 1) (max . 1) (min . 1) (abs . 1) (mod . 1) (rem . 1)
    (ash . 1) (logand . 1) (logior . 1) (logxor . 1) (= . 1) (< . 1) (> . 1) (<= . 1) (>= . 1)
    (zerop . 1) (plusp . 1) (minusp . 1) (evenp . 1) (oddp . 1) (consp . 1) (atom . 1)
    (symbolp . 1) (numberp . 1) (integerp . 1) (stringp . 1) (assoc . 1) (member . 1)
    (find . 1) (position . 1) (sqrt . 1) (float . 1) (code-char . 1) (char-code . 1))
  "Numbers of values of standard functions, which a program may not redefine.")

(defun form-nvalues (form env)
  "The number of values FORM (macroexpanded) always returns, or NIL."
  (progn
    (cond ((symbolp form) 1)            ; (symbol macros are expanded)
          ((atom form) 1)
          ((member (car form) '(quote function)) 1)
          ((not (symbolp (car form))) nil)
          ((env-lookup-fun env (car form)) nil)
          ((eq (car form) 'values) (length (cdr form)))
          ((assoc (car form) *cl-nvalues*) (cdr (assoc (car form) *cl-nvalues*)))
          ((and (not (special-operator-p (car form))) (not (member (car form) *special-forms*)))
           (known-call-nvalues (car form) 0))
          (t nil))))

(defun fixed-mvcall-form (form env)
  "The AST of (MULTIPLE-VALUE-CALL f . args) as a call of f with variables
bound to the values of ARGS, when their number is known; else NIL."
  (destructuring-bind (f &rest args) (cdr form)
    (let* ((args (mapcar (lambda (a) (cmacroexpand a env)) args))
           (counts (mapcar (lambda (a) (form-nvalues a env)) args)))
      (when (and (every #'identity counts) (<= (reduce #'+ counts) 16))
        (let* ((fvar (unless (and (consp f) (eq (car f) 'function)) (cgensym "F")))
               (vars (mapcar (lambda (n) (loop repeat n collect (cgensym "V"))) counts))
               (call `(funcall ,(or fvar f) ,@(apply #'append vars)))
               (body (reduce (lambda (a+vs inner)
                               (destructuring-bind (a . vs) a+vs
                                 (if vs
                                     `(multiple-value-bind ,vs ,a ,inner)
                                     `(progn ,a ,inner))))
                             (mapcar #'cons args vars) :from-end t :initial-value call)))
          (convert (if fvar `(let ((,fvar ,f)) ,body) body) env))))))

(defun convert-block (name body env)
  (let* ((blk (make-blk :name name :lam *current-lam*))
         (tag (new-lvar (cgensym "BLOCK-TAG")))
         (node (progn (setf (blk-tag-var blk) tag)
                      (convert-body body (env-extend env (list :block name blk))))))
    ;; a block no RETURN-FROM refers to (that of a DEFUN, of an inline
    ;; function's copy) is its body: it then leaves its value in any
    ;; representation (an inline function's double stays an f64, typed.lisp)
    (if (blk-used blk)
        `(:block ,blk ,node)
        node)))

(defun convert-return-from (name value env)
  (let ((e (env-lookup env :block name)))
    (unless e (error "return-from unknown block ~S" name))
    (let ((blk (third e)))
      (setf (blk-used blk) t)
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
    (setf (tb-tag-var tb) (new-lvar (cgensym "TAGBODY-TAG")))
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

;;; Functions of a compilation unit known by a symbol that names nothing at
;;; run time (the functions of methods and the discriminating functions of
;;; generic functions, runtime/clos.lisp): (%STATIC-LAMBDA sym (lambda ...))
;;; is the function, known as SYM to (%STATIC-CALL sym arg...), a direct
;;; call, when it closes over nothing; (%STATIC-KNOWN-P sym) is decided when
;;; the code is generated (the function may be defined by a later form), and
;;; (%STATIC-FUNCTION sym) is the function, or NIL.
(defvar clysm-sys::*gf-unit* nil
  "The generic functions, methods and classes of the unit being compiled
(SYS::%MAKE-GF-UNIT), or NIL.")

(defnative clysm-sys::%static-lambda (form env)
  (let ((node (convert (third form) env)))
    (when (and (eq (car node) :lambda) (null (lam-free-vars (second node))))
      (setf (lam-static-p (second node)) t)
      (push (second node) (gethash (second form) *global-defuns*)))
    node))

(defnative clysm-sys::%static-call (form env)
  `(:call ,(second form) ,@(convert-args (cddr form) env)))

(defnative clysm-sys::%static-known-p (form env)
  (let ((sym (second form)))
    `(:prim ,(make-prim :name 'clysm-sys::%static-known-p :nargs 0 :kind :test
                        :fn (lambda (argv) (declare (ignore argv))
                              (if (known-global-lam sym) (list (list :i32.const 1)) (list (list :i32.const 0))))))))

(defnative clysm-sys::%static-function (form env)
  (let ((sym (second form)))
    `(:prim ,(make-prim :name 'clysm-sys::%static-function :nargs 0 :kind :value
                        :fn (lambda (argv) (declare (ignore argv))
                              (let ((lam (known-global-lam sym)))
                                (if lam (cg-closure lam) (nil-code))))))))

(defun gf-unit-flush (env)
  "Process the definitions of the discriminating functions that the forms
converted so far call (the end of a unit, or of a part of it)."
  (when clysm-sys::*gf-unit*
    (let ((forms (clysm-sys::%gf-unit-flush (lambda (sym) (and (known-global-lam sym) t)))))
      ;; (the functions are known by their symbols, and compiled when a
      ;; call refers to them: no code of a toplevel form)
      (let ((clysm-sys::*gf-unit* nil))
        (dolist (f forms)
          (let ((*current-lam* (ensure-toplevel-lam)))
            (convert f env)))))))

;; (a guarded call of a function of another form of a batch, BATCH-GUARDED-P)
(defnative clysm-sys::%batch-direct-call (form env)
  (let ((op (second form)) (args (cddr form)))
    `(:call ,op ,@(convert-args (trusted-ftype-args op args env) env))))

(defnative clysm-sys::%batch-cell-call (form env)
  `(:funcall ,(convert `(quote ,(second form)) env) ,@(convert-args (cddr form) env)))

(defnative multiple-value-bind (form env)
  (destructuring-bind (vars value-form &rest body) (cdr form)
   ;; one variable (as SETF expands): a LET
   (if (and (consp vars) (null (cdr vars)))
    (convert `(let ((,(car vars) ,value-form)) ,@body) env)
    (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
      (note-ignored-outer decls env vars)
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
        (let ((body-node (convert-body forms (free-type-decl-env decls vars new-env))))
          (check-unused lvars decls)
          `(:mvbind ,lvars ,vnode ,body-node)))))))

(defnative clysm-sys::%with-only-variables (form env)
  ;; (%with-only-variables (var...) form): FORM converted in a lexical
  ;; environment with only those variables of the current one
  (let ((*local-notinline* nil) (*local-inline* nil))
    (convert (third form)
             (make-cenv (loop for v in (second form)
                              for e = (env-lookup-var env v)
                              when e collect e)))))

;;; (%inline-copy depth-var form): FORM, the body of a copied function,
;;; converted with the depth of copies DEPTH-VAR one more (the arguments of
;;; the copied call are converted at the caller's depth)
(defnative clysm-sys::%inline-copy (form env)
  (progv (list (second form)) (list (1+ (symbol-value (second form))))
    (convert (third form) env)))

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

;;; (sys:%when-reached 'SYMBOL form) — FORM for effect, then the value SYMBOL;
;;; in a shaken program (--shake) FORM is left out unless something else
;;; refers to SYMBOL, as the definition of a function is (CG-DEFUN)
;;; (FORM is a function of its own then, run by the one CG-DEFUN makes)
(declaim (special *shake*))
(defnative clysm-sys::%when-reached (form env)
  (destructuring-bind (qsym body) (cdr form)
    (if *shake*
        `(:defun ,(second qsym) ,(convert `(lambda () ,body) env) :form)
        (convert `(progn ,body ,qsym) env))))

;;; (sys:%weak-symbol SYMBOL) — the symbol SYMBOL (not evaluated), by a
;;; reference that does not keep what it names in a shaken program
;;; (SHAKE-MODULE and the heap of src/preinit.lisp do not follow it): its
;;; function, value and properties are there only if something else refers
;;; to SYMBOL, so that code can use them when the program has them anyway
;;; (the message of an unhandled error, cond.lisp)
(defnative clysm-sys::%weak-symbol (form env)
  (declare (ignore env))
  `(:wasm :eqref () ((:global.get ,(sym-global (second form)) :weak))))

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

;; (EQUAL x #\") of a character, a symbol or a fixnum: EQ (a JSON reader's
;; test of the character it read)
(deftransform equal (form)
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
  (let ((temps (loop for nil in args collect (cgensym "C"))))
    `(let ,(mapcar #'list temps args)
       (and ,@(loop for (a b) on temps while b collect (list op a b))))))

(dolist (op '(< > <= >= =))
  (let ((op op))
    (setf (get op 'clysm-transform)
          (lambda (form)
            (let ((args (cdr form)))
              (if (cddr args) (chain-compare op args) form))))))

;;; ------------------------------------------------------------
;;; Dispatch compiled as code: the making of the discriminating functions
;;; ------------------------------------------------------------
;;; (See "Dispatch compiled as code" in runtime/clos.lisp, whose macros call
;;; these in a unit, and which compares the trees with the generic functions
;;; at run time.  Here, and not in the runtime: a program does not carry it.)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clysm-sys::%make-gf-unit ()
    "#(methods classes sites count): name -> ((qualifiers specializers
lambda-list function-symbol...) ...), name -> (index . superclasses), and
((name . symbol) ...) of the discriminating functions to make."
    (vector (make-hash-table :test 'eq) (make-hash-table :test 'eq) nil 0 (make-hash-table :test 'eq)))

  (defun clysm-sys::%gf-unit-note-combination (name combination)
    "Note the method combination (name . options), or NIL, that a DEFGENERIC
of the unit gives the generic function NAME."
    (let ((u clysm-sys::*gf-unit*))
      (when (and u (symbolp name)) (setf (gethash name (svref u 4)) combination))))

  (defun clysm-sys::%gf-unit-note-method (name quals specs ll &optional nextp)
    "Note a method that the unit defines; returns the symbol its function is
known by (%STATIC-LAMBDA), or NIL outside a unit.  NEXTP: the function takes
the next method (a function of the arguments, or NIL) and a cell before the
arguments."
    (let ((u clysm-sys::*gf-unit*))
      (when (and u (symbolp name) name)
        (let* ((tb (svref u 0))
               (old (find-if (lambda (r) (and (equal (first r) quals) (equal (second r) specs))) (gethash name tb)))
               (sym (make-symbol (if nextp "NEXT-METHOD" "METHOD"))))
          (if old
              ;; (a form expanded again: either function may be the one compiled)
              (setf (third old) ll (cdddr old) (cons sym (cdddr old)))
              (push (list quals specs ll sym) (gethash name tb)))
          sym))))

  (defun clysm-sys::%gf-unit-note-class (name supers)
    (let ((u clysm-sys::*gf-unit*))
      (when (and u (symbolp name) (listp supers))
        (setf (svref u 3) (+ (svref u 3) 1))
        (setf (gethash name (svref u 1)) (cons (svref u 3) supers)))))

  (defun clysm-sys::%ct-live-p ()
    "True in the image: the generic functions and classes defined before the
unit are there to be looked at."
    (and (boundp 'clysm-sys::*native-eval-hook*) (symbol-value 'clysm-sys::*native-eval-hook*) t))

  (defvar clysm-sys::*ct-js-name-cells* (make-hash-table :test 'eq)
    "The cells that %JS-NAME-LITERAL made for modules compiled from files (JS-NAME-CELL-P).")

  (defun clysm-sys::%ct-js-name-cell (string)
    "A new cell (STRING . %JS-UNMADE) of the literal name STRING of
JavaScript (%JS-NAME-LITERAL in runtime/js.lisp) for a module compiled from
files, which the compiler knows by itself (JS-NAME-CELL-P in src/codegen.lisp)."
    (let ((cell (cons string 'clysm-sys::%js-unmade)))
      (setf (gethash cell clysm-sys::*ct-js-name-cells*) t)
      cell))

  (defun clysm-sys::%ct-ll-shape (ll)
    "(nreq nopt more) of the lambda list LL of a method when the generic
function may have a tree: its methods are functions of at most four
arguments, the required ones or the spread ones (SYS::%FAST-SHAPE)."
    (let ((n (clysm-sys::%fast-arity ll)))
      (if n
          (list n 0 0)
          (let ((s (clysm-sys::%fast-shape ll)))
            (and s (<= (+ (first s) (second s) (third s)) 4) (<= (first s) 4) s)))))

  (defun clysm-sys::%ct-live-gf (name)
    "The metaobject of the generic function NAME defined in the image and its
shape (nreq nopt more), when its methods are functions of at most four
(spread) arguments, or NIL."
    (and (clysm-sys::%ct-live-p) (fboundp name) (not (macro-function name)) (not (special-operator-p name))
         (let ((info (clysm-sys::%gf-info (fdefinition name))))
           (and info
                (let ((nreq (clysm-sys::%gf-slot info clysm-sys::+gf-nreq+))
                      (nopt (clysm-sys::%gf-slot info clysm-sys::+gf-nopt+))
                      (more (if (clysm-sys::%gf-slot info clysm-sys::+gf-restp+) 1 0)))
                  (and (integerp nreq) (integerp nopt) (<= 1 nreq 4) (<= (+ nreq nopt more) 4)
                       (eql (clysm-sys::%svref (clysm-sys::%gf-slot info clysm-sys::+gf-cache+) 1) (+ nreq nopt more))
                       (values info (list nreq nopt more))))))))

  (defun clysm-sys::%ct-live-methods (info n)
    "The methods of the generic function INFO as (qualifiers specializers
kind keys), KIND :PLAIN (its function of the arguments is known), :NEXT (that of
the next method, a cell and the arguments) or NIL, KEYS the keywords it
accepts (or :ANY); :BAD when a specializer cannot be named here."
    (let ((r nil) (top (find-class t)))
      (dolist (m (clysm-sys::%gf-slot info clysm-sys::+gf-methods+) (nreverse r))
        (let ((specs (mapcar (lambda (s)
                               (cond ((consp s)
                                      (or (clysm-sys::%ct-eql-spec (list 'eql (list 'quote (cadr s))))
                                          (return-from clysm-sys::%ct-live-methods :bad)))
                                     ((eq s top) t)
                                     (t (let ((nm (class-name s)))
                                          (if (and nm (symbolp nm) (eq (find-class nm nil) s))
                                              nm
                                              (return-from clysm-sys::%ct-live-methods :bad))))))
                             (clysm-sys::%method-specializers m))))
          (unless (eql (length specs) n) (return-from clysm-sys::%ct-live-methods :bad))
          (push (list (clysm-sys::%method-qualifiers m) specs
                      (cond ((clysm-sys::%method-extra m clysm-sys::+method-static+) :next)
                            ((clysm-sys::%method-extra m clysm-sys::+method-fast-function+) :plain)
                            (t nil))
                      (clysm-sys::%instance-ref m 6))
                r)))))

  (defun clysm-sys::%ct-unit-shape (recs)
    "The shape that the methods RECS of the unit all have, or NIL."
    (let ((s (clysm-sys::%ct-ll-shape (third (car recs)))))
      (and s (every (lambda (r) (equal (clysm-sys::%ct-ll-shape (third r)) s)) recs) s)))

  (defun clysm-sys::%gf-unit-site (form)
    "The call FORM of a generic function with methods in the unit, or defined
before with methods whose functions are known, as a call of its
discriminating function, or NIL.  With optional or rest/keyword parameters
the function takes the spread arguments: the required ones, the optional
ones (the unbound marker for those not given) and the list of the rest."
    (let* ((u clysm-sys::*gf-unit*) (name (car form))
           (k (do ((l (cdr form) (cdr l)) (i 0 (+ i 1))) ((atom l) (if (null l) i -1)))))
      (when (and u (<= 1 k 16) (symbolp name) name)
        (let* ((recs (gethash name (svref u 0)))
               (shape (if recs
                          (clysm-sys::%ct-unit-shape recs)
                          (multiple-value-bind (info s) (clysm-sys::%ct-live-gf name)
                            (and info
                                 (some (lambda (m) (or (clysm-sys::%method-extra m clysm-sys::+method-fast-function+)
                                                       (clysm-sys::%method-extra m clysm-sys::+method-static+)))
                                       (clysm-sys::%gf-slot info clysm-sys::+gf-methods+))
                                 s)))))
          (when (and shape
                     (>= k (first shape))
                     (or (eql (third shape) 1) (<= k (+ (first shape) (second shape))))
                     ;; In the image a call with constant keywords of a
                     ;; generic function with &KEY passes them in the slots
                     ;; of its keyed main function (SYS::%GF-KEYED-ENTRY: no
                     ;; list, and the methods do not search one), which is
                     ;; faster than the tree's list of them: 66 against 156
                     ;; ms for six million calls with two keywords.
                     (not (and (clysm-sys::%ct-live-p)
                               (eql (third shape) 1)
                               (let ((rest (nthcdr (+ (first shape) (second shape)) (cdr form))))
                                 (and rest
                                      (do ((l rest (cddr l))) ((null l) t)
                                        (unless (and (keywordp (car l)) (consp (cdr l))) (return nil)))))
                               (if recs
                                   (some (lambda (r) (member '&key (third r))) recs)
                                   (let ((info (clysm-sys::%ct-live-gf name)))
                                     (and info (clysm-sys::%gf-slot info clysm-sys::+gf-keyp+))))))
                     (symbol-package name)
                     (not (member (package-name (symbol-package name)) '("COMMON-LISP" "SYSTEM" "CLYSM" "KEYWORD")
                                  :test #'string=)))
            (let* ((e (assoc name (svref u 2)))
                   (d (if e (cdr e) (let ((s (make-symbol "DISCRIMINATOR"))) (push (cons name s) (svref u 2)) s)))
                   (vars (let ((r nil)) (dotimes (i k r) (push (cgensym "A") r))))
                   (npos (+ (first shape) (second shape))))
              `(let ,(mapcar #'list vars (cdr form))
                 (if (clysm-sys::%static-known-p ,d)
                     (clysm-sys::%static-call ,d ,@(subseq vars 0 (min k npos))
                                              ,@(make-list (max 0 (- npos k)) :initial-element '(clysm-sys::%unbound-marker))
                                              ,@(when (eql (third shape) 1)
                                                  (list (if (> k npos) `(list ,@(nthcdr npos vars)) nil))))
                     ,(clysm-sys::%gf-unit-fallback name vars)))))))))

  (defun clysm-sys::%gf-unit-fallback (name vars)
    "The call of the generic function NAME as it is made without a tree."
    (let ((n (length vars)))
      (if (and (<= n 2) (eql (get name 'clysm-sys::%gf-site-macro) n))
          (clysm-sys::%gf-site-expansion-1 (cons name vars) vars name n)
          `(locally (declare (notinline ,name)) (,name ,@vars)))))

  (defun clysm-sys::%gf-unit-flush (known)
    "The forms defining the discriminating functions that the sites of the
unit converted so far call; KNOWN tells whether a symbol of
%GF-UNIT-NOTE-METHOD names a function that can be called directly."
    (let ((u clysm-sys::*gf-unit*) (forms nil))
      (when u
        (dolist (d (svref u 2))
          (dolist (f (clysm-sys::%gf-disc-form u (car d) (cdr d) known)) (push f forms)))
        (setf (svref u 2) nil))
      forms))

  ;; the built-in classes told by a test of the representation: (class test
  ;; precedence-list sample), the sample :CLASS when the class is the one
  ;; of that name (CLASS-OF gives every string that is not simple the class
  ;; STRING, every complex number COMPLEX); the tests exclude each other
  (defun clysm-sys::%ct-kinds ()
    '((fixnum (clysm-sys::fixnump :x) (fixnum integer rational real number) 0)
      (character (characterp :x) (character) #\a)
      (null (null :x) (null symbol list sequence) nil)
      (keyword (and (symbolp :x) :x (eq (clysm-sys::%symbol-package :x) clysm-sys::*keyword-package*)) (keyword symbol) :k)
      (symbol (and (symbolp :x) :x (not (eq (clysm-sys::%symbol-package :x) clysm-sys::*keyword-package*))) (symbol) t)
      (cons (consp :x) (cons list sequence) (nil))
      (simple-string (clysm-sys::%simple-string-p :x) (simple-string string vector array sequence) "")
      (double-float (clysm-sys::%double-float-p :x) (double-float float real number) 1d0)
      (single-float (clysm-sys::%single-float-p :x) (single-float float real number) 1f0)
      (simple-vector (clysm-sys::%simple-vector-p :x) (simple-vector vector array sequence) #())
      (bignum (clysm-sys::%bignump :x) (bignum integer rational real number) 1208925819614629174706176)
      (ratio (clysm-sys::%ratiop :x) (ratio rational real number) 1/2)
      (string (and (stringp :x) (not (clysm-sys::%simple-string-p :x))) (string vector array sequence) :class)
      (complex (complexp :x) (complex number) :class)
      ;; (a function that is not a generic function or another funcallable instance)
      (compiled-function (and (functionp :x) (not (clysm-sys::%instancep (clysm-sys::%closure-name :x))))
       (compiled-function function) :class)))

  ;; a group of kinds with one test
  (defun clysm-sys::%ct-kind-groups ()
    '(((null keyword symbol) (symbolp :x))
      ((fixnum bignum) (integerp :x))
      ((double-float single-float) (floatp :x))
      ((simple-string string) (stringp :x))
      ((null cons) (listp :x))))

  ;; the classes all of whose objects are of the kinds of %CT-KINDS
  (defun clysm-sys::%ct-closed-classes ()
    '(fixnum bignum integer ratio rational single-float double-float float real complex number
      symbol keyword null list cons character simple-string string))

  (defun clysm-sys::%ct-live-class (name)
    "The class named NAME in the image whose instances are told by their
class (not a built-in class), or NIL."
    (and (clysm-sys::%ct-live-p) (symbolp name)
         (let ((c (find-class name nil)))
           (and c (clysm-sys::%class-p c) (not (eq (class-of c) (find-class 'built-in-class)))
                (clysm-sys::%class-cpl c)
                c))))

  (defun clysm-sys::%ct-class-cpl (u name &optional (depth 0))
    "What is known of the precedence list of the class NAME (names, without
T): from the unit, or from the image; or NIL."
    (multiple-value-bind (e found) (gethash name (svref u 1))
      (cond ((and found (< depth 40))
             (let ((r (list name)))
               (dolist (s (cdr e))
                 (setq r (append r (or (clysm-sys::%ct-class-cpl u s (+ depth 1)) (list s)))))
               (setq r (remove-duplicates r))
               (if (or (member 'standard-object r) (member 'structure-object r))
                   r
                   (append r (list 'standard-object)))))
            (found nil)
            (t (let ((c (clysm-sys::%ct-live-class name)))
                 (and c (remove t (mapcar #'class-name (clysm-sys::%class-cpl c)))))))))

  (defun clysm-sys::%ct-live-subclasses (name limit)
    "The names of the class NAME of the image and of its subclasses there, at most LIMIT.
Only the names in packages that the code being compiled is sure to find
where it is loaded: NAME's own (the source names it), CL and SYSTEM, the
current package and those it uses.  The tree names its classes, and a fasl
compiled in an image with more (Lem's, for micros' inspector) is loaded in
images without them (a world): their packages would not be there to read
the fasl.  A subclass left out goes the generic way."
    (let ((c (clysm-sys::%ct-live-class name)) (r nil) (count 0) (visited 0)
          (packages (list* (symbol-package name) (find-package "COMMON-LISP") (find-package "SYSTEM")
                           *package* (package-use-list *package*))))
      (when c
        (let ((queue (list c)))
          (loop
            (when (or (null queue) (>= count limit) (>= visited (* 8 limit))) (return))
            (let* ((x (pop queue)) (nm (class-name x)))
              (setq visited (+ visited 1))
              (when (and nm (symbolp nm) (eq (find-class nm nil) x) (not (member nm r)))
                (when (member (symbol-package nm) packages)
                  (push nm r)
                  (setq count (+ count 1)))
                (setq queue (append queue (clysm-sys::class-direct-subclasses x))))))))
      (nreverse r)))

  (defun clysm-sys::%ct-key-cpl (u key)
    (cond ((atom key) nil)
          ((eq (car key) :kind) (third (assoc (cadr key) (clysm-sys::%ct-kinds))))
          ((eq (car key) :class) (clysm-sys::%ct-class-cpl u (cadr key)))
          (t (let ((o (cadr key)))
               (third (assoc (cond ((integerp o) 'fixnum) ((characterp o) 'character) ((null o) 'null)
                                   ((keywordp o) 'keyword) (t 'symbol))
                             (clysm-sys::%ct-kinds)))))))

  (defun clysm-sys::%ct-eql-spec (s)
    "The specializer (EQL form) as (EQL object) when the object is known
here and compared with EQ, else NIL."
    (let ((form (cadr s)))
      (cond ((and (integerp form) (<= -536870912 form 536870911)) (list 'eql form))
            ((characterp form) (list 'eql form))
            ((or (null form) (eq form t) (keywordp form)) (list 'eql form))
            ((and (consp form) (eq (car form) 'quote) (consp (cdr form)) (null (cddr form))
                  (let ((o (cadr form)))
                    (or (symbolp o) (characterp o) (and (integerp o) (<= -536870912 o 536870911)))))
             (list 'eql (cadr form)))
            (t nil))))

  (defun clysm-sys::%ct-spec-admits-p (u spec key)
    (cond ((eq spec t) t)
          ((consp spec) (and (consp key) (eq (car key) :eql) (eql (cadr key) (cadr spec))))
          (t (and (member spec (clysm-sys::%ct-key-cpl u key)) t))))

  (defun clysm-sys::%ct-substitute (tree x)
    (cond ((eq tree :x) x)
          ((atom tree) tree)
          (t (cons (clysm-sys::%ct-substitute (car tree) x) (clysm-sys::%ct-substitute (cdr tree) x)))))

  (defun clysm-sys::%ct-mentions-p (tree sym)
    (cond ((eq tree sym) t)
          ((atom tree) nil)
          (t (or (clysm-sys::%ct-mentions-p (car tree) sym) (clysm-sys::%ct-mentions-p (cdr tree) sym)))))

  (defun clysm-sys::%gf-disc-form (u name dsym known)
    "The forms defining the discriminating function DSYM of the generic
function NAME from the methods of the unit U and those defined before, or
NIL when there is nothing it could call."
    (let* ((recs (gethash name (svref u 0)))
           (live-shape nil)
           (info (multiple-value-bind (i s) (clysm-sys::%ct-live-gf name) (setq live-shape s) i))
           ;; (nreq nopt more): the tree is over the NREQ required arguments,
           ;; N of them, and its functions take the spread ones
           (shape (if recs (clysm-sys::%ct-unit-shape recs) live-shape))
           (n (first shape))
           (spreadp (and shape (not (and (eql (second shape) 0) (eql (third shape) 0)))))
           (methods nil) (fsyms nil) (index 0) (nfns 0)
           (vars (let ((r nil)) (dotimes (i (if shape (+ (first shape) (second shape) (third shape)) 0) r) (push (cgensym "A") r))))
           ;; the cell of the keywords of the generic function itself (0: its
           ;; keyword arguments are not checked), after those of the functions
           (keys-cell nil)
           ;; the cell of the generic function's own function of the spread
           ;; arguments, for the calls that the tree does not take
           (core-cell nil)
           (disc (cgensym "DISC")) (tree (cgensym "TREE")) (cells-var (cgensym "CELLS")) (state (cgensym "STATE"))
           (checks nil) (class-cells nil) (nclasses 0) (leaves 0) (tails nil) (tail-forms nil)
           ;; NIL (the standard method combination) or (operator . options)
           ;; of a short one: from the DEFGENERIC of the unit, else from the
           ;; generic function defined before
           (comb (multiple-value-bind (c found) (gethash name (svref u 4))
                   (cond (found c)
                         (info (let ((mc (clysm-sys::%gf-slot info clysm-sys::+gf-combination+)))
                                 (if (eq (clysm-sys::%instance-ref mc 0) 'standard)
                                     nil
                                     (cons (clysm-sys::%instance-ref mc 0) (clysm-sys::%instance-ref mc 1)))))
                         (t nil)))))
      (when (and comb (equal comb '(standard))) (setq comb nil))
      (when comb
        (unless (and (member (car comb) '(+ append list max min nconc and or progn))
                     (member (cdr comb) '(nil (:most-specific-first) (:most-specific-last)) :test #'equal))
          (return-from clysm-sys::%gf-disc-form nil)))
      (when (and info recs (not (equal shape live-shape))) (setq info nil))
      (unless (and n (<= 1 n 4) (<= (length recs) 40)) (return-from clysm-sys::%gf-disc-form nil))
      ;; METHODS: #(qualifiers specializers symbol function-index kind cell keys)
      (dolist (r recs)
        (let ((specs (mapcar (lambda (s)
                               (cond ((and (consp s) (eq (car s) 'eql) (consp (cdr s)) (null (cddr s)))
                                      (or (clysm-sys::%ct-eql-spec s) (return-from clysm-sys::%gf-disc-form nil)))
                                     ((and (symbolp s) s) s)
                                     (t (return-from clysm-sys::%gf-disc-form nil))))
                             (second r)))
              (sym (find-if known (cdddr r))))
          (unless (eql (length specs) n) (return-from clysm-sys::%gf-disc-form nil))
          (push (vector (first r) specs sym (and sym index)
                        (cond ((null sym) nil) ((string= (symbol-name sym) "NEXT-METHOD") :next) (t :plain))
                        nil
                        (multiple-value-bind (nreq nopt restp keyp keys aokp) (clysm-sys::%ll-info (third r))
                          (declare (ignore nreq nopt restp))
                          (cond (aokp :any) (keyp keys) (t nil))))
                methods)
          (when sym (push sym fsyms) (setq index (+ index 1)))))
      (setq fsyms (nreverse fsyms))
      ;; the methods defined before that the unit does not define again
      (when info
        (let ((live (clysm-sys::%ct-live-methods info n)))
          (when (eq live :bad) (return-from clysm-sys::%gf-disc-form nil))
          (dolist (l live)
            (unless (find-if (lambda (m) (and (equal (svref m 0) (first l)) (equal (svref m 1) (second l)))) methods)
              (push (vector (first l) (second l) nil nil (third l) (and (third l) (+ 3 nfns)) (fourth l)) methods)
              (when (third l) (setq nfns (+ nfns 1)))))))
      (unless (and methods (<= (length methods) 40)) (return-from clysm-sys::%gf-disc-form nil))
      (when (eql (third shape) 1) (setq keys-cell (+ 3 nfns) nfns (+ nfns 1)))
      (when spreadp (setq core-cell (+ 3 nfns) nfns (+ nfns 1)))
      (labels ((spec (m p) (nth p (svref m 1)))
               (class-specs (ms p)
                 (let ((r nil))
                   (dolist (m ms (nreverse r))
                     (let ((s (spec m p))) (when (and (symbolp s) (not (eq s t))) (pushnew s r))))))
               (note-check (p key)
                 ;; (the class specializers of all the methods at P)
                 ;; (a fresh list: REMOVE-IF-NOT of SBCL gives the tail it
                 ;; kept, of a literal that its COMPILE-FILE made one with
                 ;; others, and the constant made of CHECKS shared it; Clysm's
                 ;; copies, and the compiler in Wasm made another module)
                 (let ((assumed (copy-list (remove-if-not (lambda (c) (member c (class-specs methods p)))
                                                          (clysm-sys::%ct-key-cpl u key)))))
                   (pushnew (list p key assumed) checks :test #'equal)))
               (class-cell (p cname)
                 (let ((e (assoc (cons p cname) class-cells :test #'equal)))
                   (if e
                       (cdr e)
                       (let ((i (+ 3 nfns nclasses)))
                         (setq nclasses (+ nclasses 1))
                         (push (cons (cons p cname) i) class-cells)
                         (note-check p (list :class cname i))
                         i))))
               (rank (s key)
                 (cond ((consp s) 0)
                       ((eq s t) 1000000)
                       (t (+ 1 (or (position s (clysm-sys::%ct-key-cpl u key)) 999998)))))
               (more-specific-p (m1 m2 keys)
                 (do ((p 0 (+ p 1)) (ks keys (cdr ks))) ((>= p n) nil)
                   (let ((r1 (rank (spec m1 p) (car ks))) (r2 (rank (spec m2 p) (car ks))))
                     (cond ((< r1 r2) (return t)) ((> r1 r2) (return nil))))))
               (nextp (m) (eq (svref m 4) :next))
               (call (m args)
                 (if (svref m 2)
                     `(clysm-sys::%static-call ,(svref m 2) ,@args)
                     `(funcall (the function (clysm-sys::%svref ,cells-var ,(svref m 5))) ,@args)))
               (fn (m)
                 (if (svref m 2) `(clysm-sys::%static-function ,(svref m 2)) `(clysm-sys::%svref ,cells-var ,(svref m 5))))
               (tail (key maker)
                 ;; the form of the function of the arguments that KEY names
                 ;; (methods called in turn), defined once
                 (let ((e (assoc key tails :test #'equal)))
                   (unless e
                     (let ((sym (make-symbol "NEXT")) (vs (mapcar (lambda (v) (declare (ignore v)) (cgensym "A")) vars)))
                       (setq e (cons key sym))
                       (push e tails)
                       (push (list sym vs (funcall maker vs)) tail-forms)))
                   `(clysm-sys::%static-function ,(cdr e))))
               (leaf (ms keys)
                 ;; the forms calling the effective method of MS, or NIL: the
                 ;; next method of a method with CALL-NEXT-METHOD is a function
                 ;; calling the rest of the methods (TAIL)
                 (let ((sorted (sort (copy-list ms) (lambda (a b) (more-specific-p a b keys))))
                       (primary nil) (before nil) (after nil) (around nil))
                   (dolist (m sorted)
                     (let ((q (svref m 0)))
                       (cond ((equal q '(:around)) (push m around))
                             (comb (if (equal q (list (car comb))) (push m primary) (return-from leaf nil)))
                             ((null q) (push m primary))
                             ((equal q '(:before)) (push m before))
                             ((equal q '(:after)) (push m after))
                             (t (return-from leaf nil)))))
                   (setq primary (nreverse primary) before (nreverse before) around (nreverse around))
                   ;; (a short combination: every primary method is called, none with a next method)
                   (when comb
                     (when (or (some #'nextp primary) (notevery (lambda (m) (svref m 4)) primary))
                       (return-from leaf nil))
                     (when (equal (cdr comb) '(:most-specific-last)) (setq primary (reverse primary))))
                   (when (or (null primary) (some #'nextp before) (some #'nextp after))
                     (return-from leaf nil))
                   ;; the functions of the methods that are called must be
                   ;; known: up to the first without CALL-NEXT-METHOD
                   (flet ((upto (ms) (let ((r nil)) (dolist (m ms (nreverse r)) (push m r) (unless (nextp m) (return (nreverse r)))))))
                     (unless (every (lambda (m) (svref m 4))
                                    (if (and around (notevery #'nextp around))
                                        (upto around)
                                        (append around before after (upto primary))))
                       (return-from leaf nil)))
                   (labels ((primary-call (ps args)
                              (cond ((not (nextp (car ps))) (call (car ps) args))
                                    ((null (cdr ps)) (return-from leaf nil))
                                    (t (call (car ps) (list* (primary-fn (cdr ps)) nil args)))))
                            (primary-fn (ps)
                              (if (nextp (car ps))
                                  (tail (cons :primary ps) (lambda (vs) (primary-call ps vs)))
                                  (fn (car ps))))
                            (core-call (args)
                              (cond (comb
                                     (if (and (null (cdr primary)) (not (eq (car comb) 'list)))
                                         (call (car primary) args)
                                         `(,(car comb) ,@(mapcar (lambda (m) (call m args)) primary))))
                                    ((and (null before) (null after)) (primary-call primary args))
                                    ((null after)
                                     `(progn ,@(mapcar (lambda (m) (call m args)) before) ,(primary-call primary args)))
                                    (t `(progn ,@(mapcar (lambda (m) (call m args)) before)
                                               (multiple-value-prog1 ,(primary-call primary args)
                                                 ,@(mapcar (lambda (m) (call m args)) after))))))
                            (core-fn ()
                              (cond ((and comb (null (cdr primary)) (not (eq (car comb) 'list))) (fn (car primary)))
                                    (comb (tail (list :core before primary after) #'core-call))
                                    ((and (null before) (null after)) (primary-fn primary))
                                    (t (tail (list :core before primary after) #'core-call))))
                            (around-call (as args)
                              (cond ((null as) (core-call args))
                                    ((not (nextp (car as))) (call (car as) args))
                                    (t (call (car as) (list* (around-fn (cdr as)) nil args)))))
                            (around-fn (as)
                              (cond ((null as) (core-fn))
                                    ((not (nextp (car as))) (fn (car as)))
                                    (t (tail (list :around as before primary after)
                                             (lambda (vs) (around-call as vs)))))))
                     (let ((form (around-call around vars)))
                       (setq leaves (+ leaves 1))
                       (when keys-cell
                         ;; the keyword arguments of a call with some are
                         ;; checked against the keywords of the generic
                         ;; function (in its cell) and of these methods
                         (let ((more (car (last vars))) (g (cgensym "KEYS"))
                               (mkeys (let ((r nil))
                                        (dolist (m ms (nreverse r))
                                          (let ((k (svref m 6)))
                                            (when (eq k :any) (return :any))
                                            (dolist (x k) (pushnew x r)))))))
                           (setq form `(progn
                                         (when ,more
                                           (let ((,g (clysm-sys::%svref ,cells-var ,keys-cell)))
                                             (unless (eq ,g 0)
                                               (clysm-sys::%gf-disc-check-keys ',name ,g ',mkeys ,more))))
                                         ,form))))
                       (list `(return-from ,disc ,form))))))
               (build (p ms keys)
                 ;; the forms dispatching on the arguments from P on, among
                 ;; the methods MS; KEYS: the keys of the arguments before, reversed
                 (cond ((null ms) nil)
                       ((>= p n) (leaf ms (reverse keys)))
                       ((every (lambda (m) (eq (spec m p) t)) ms) (build (+ p 1) ms (cons :any keys)))
                       (t
                        (let* ((x (nth p vars))
                               (cspecs (class-specs ms p))
                               (closed (every (lambda (c) (member c (clysm-sys::%ct-closed-classes))) cspecs))
                               (forms nil))
                          (flet ((sub (key)
                                   (build (+ p 1)
                                          (remove-if-not (lambda (m) (clysm-sys::%ct-spec-admits-p u (spec m p) key)) ms)
                                          (cons key keys)))
                                 (branch (test sub)
                                   (push `(when ,test ,@sub (return-from ,tree nil)) forms)))
                            (let ((other (and closed (sub :other))))
                              ;; the objects of EQL specializers
                              (let ((seen nil))
                                (dolist (m ms)
                                  (let ((s (spec m p)))
                                    (when (and (consp s) (not (member (cadr s) seen)))
                                      (push (cadr s) seen)
                                      (let ((key (list :eql (cadr s))))
                                        (note-check p key)
                                        (branch `(eq ,x ',(cadr s)) (sub key)))))))
                              ;; the built-in classes
                              (let ((subs nil))
                                (dolist (k (clysm-sys::%ct-kinds))
                                  (when (some (lambda (c) (member c (third k))) cspecs)
                                    (let* ((key (list :kind (car k) (fourth k))) (s (sub key)))
                                      (note-check p key)
                                      (unless (or (and closed (equal s other)) (and (not closed) (null s)))
                                        (let ((e (assoc s subs :test #'equal)))
                                          (if e (push (car k) (cdr e)) (push (list s (car k)) subs)))))))
                                (dolist (e (reverse subs))
                                  (let* ((kinds (reverse (cdr e)))
                                         (tests nil))
                                    (dolist (g (clysm-sys::%ct-kind-groups))
                                      (when (every (lambda (k) (member k kinds)) (car g))
                                        (push (clysm-sys::%ct-substitute (cadr g) x) tests)
                                        (setq kinds (remove-if (lambda (k) (member k (car g))) kinds))))
                                    (dolist (k kinds)
                                      (push (clysm-sys::%ct-substitute (second (assoc k (clysm-sys::%ct-kinds))) x) tests))
                                    (branch (if (cdr tests) `(or ,@(reverse tests)) (car tests)) (car e)))))
                              ;; the classes of the unit, then those defined before
                              (let ((subs nil) (l (cgensym "L")) (classes nil) (names nil))
                                (maphash (lambda (cname e)
                                           (when (some (lambda (c) (member c (clysm-sys::%ct-class-cpl u cname))) cspecs)
                                             (push (cons (car e) cname) classes)))
                                         (svref u 1))
                                (setq names (mapcar #'cdr (sort classes (lambda (a b) (< (car a) (car b))))))
                                (dolist (c cspecs)
                                  (dolist (nm (clysm-sys::%ct-live-subclasses c 12))
                                    (unless (or (member nm names) (nth-value 1 (gethash nm (svref u 1))))
                                      (setq names (append names (list nm))))))
                                (dolist (c names)
                                  (let ((s (sub (list :class c))))
                                    (when s
                                      (let ((e (assoc s subs :test #'equal))
                                            (cell (class-cell p c)))
                                        (if e (push cell (cdr e)) (push (list s cell) subs))))))
                                (when subs
                                  (push `(when (clysm-sys::%instancep ,x)
                                           (let ((,l (clysm-sys::%instance-layout ,x)))
                                             ,@(mapcar (lambda (e)
                                                         (let ((tests (mapcar (lambda (cell) `(eq ,l (clysm-sys::%svref ,cells-var ,cell)))
                                                                              (reverse (cdr e)))))
                                                           `(when ,(if (cdr tests) `(or ,@tests) (car tests))
                                                              ,@(car e) (return-from ,tree nil))))
                                                       (reverse subs))))
                                        forms)))
                              ;; any other object
                              (when other (setq forms (append (reverse other) forms)))
                              (nreverse forms))))))))
        (let ((forms (build 0 methods nil)))
          (when (and forms (> leaves 0) (<= leaves 48))
            (let ((cells (make-array (+ 3 nfns nclasses) :initial-element 0)))
              ;; (its record is destructured by the runtime: not a cons of
              ;; it is one of a literal of the program, which may change it)
              (note-compiler-literal cells)
              (setf (svref cells 0) nil
                    (svref cells 1) (list name n
                                          (mapcar (lambda (m)
                                                    (list (svref m 0) (svref m 1)
                                                          (cond ((svref m 2) (svref m 3))
                                                                ((svref m 5) (cons (svref m 5) (svref m 4)))
                                                                (t nil))
                                                          (svref m 6)))
                                                  methods)
                                          checks comb shape keys-cell core-cell
                                          ;; (made in the image for a dead tree: SYS::%GF-DISC-REGENERATE)
                                          (eql (svref u 3) -1))
                    (svref cells 2) (+ 3 nfns))
              (cons `(clysm-sys::%static-lambda ,dsym
                       (lambda ,vars
                         (block ,disc
                           (let* ((,cells-var ',cells) (,state (clysm-sys::%svref ,cells-var 0)))
                             (if (eq ,state t)
                                 (progn
                                   (block ,tree ,@forms)
                                   ,@(when core-cell
                                       `((return-from ,disc
                                           (funcall (the function (clysm-sys::%svref ,cells-var ,core-cell)) ,@vars)))))
                                 (unless (eq ,state :dead)
                                   ;; (a function made later for other methods)
                                   (when (functionp ,state)
                                     (return-from ,disc (funcall (the function ,state) ,@vars)))
                                   (clysm-sys::%gf-disc-arm ,cells-var
                                                 (lambda () (vector ,@(mapcar (lambda (s) `(clysm-sys::%static-function ,s)) fsyms)))))))
                           ,(if spreadp
                                `(clysm-sys::%gf-disc-spread-fallback ',name ,@shape (list ,@vars))
                                (clysm-sys::%gf-unit-fallback name vars)))))
                    (mapcar (lambda (tf)
                              (destructuring-bind (sym vs body) tf
                                `(clysm-sys::%static-lambda ,sym
                                   (lambda ,vs
                                     ,(if (clysm-sys::%ct-mentions-p body cells-var)
                                          `(let ((,cells-var ',cells)) ,body)
                                          body)))))
                            tail-forms)))))))))

