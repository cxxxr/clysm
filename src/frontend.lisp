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

(defstruct (lam (:print-function (lambda (l s d) (declare (ignore d))
                                   (format s "#<LAM ~S ~D>" (lam-name l) (lam-id l)))))
  id name parent
  req opt rest keyp keys allow-other-keys
  body free-vars
  (toplevel nil)          ; global defun with no free vars
  (static-p nil))         ; closure object is a module constant

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
(defvar *live-macro-hook* nil "symbol -> macro function or NIL")
(defvar *live-special-hook* nil "symbol -> true if proclaimed special")
(defvar *live-constant-hook* nil "symbol -> (values value constantp)")
(defvar *live-symbol-macro-hook* nil "symbol -> (values expansion foundp)")

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
                 (t (multiple-value-bind (exp found) (gethash form *symbol-macros*)
                      (cond (found (values exp t))
                            (*live-symbol-macro-hook*
                             (multiple-value-bind (exp2 found2) (funcall *live-symbol-macro-hook* form)
                               (if found2 (values exp2 t) (values form nil))))
                            (t (values form nil))))))))
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
    unsigned-byte signed-byte null))

(defun apply-type-decls (decls lvars)
  "Record the types declared in DECLS for the variables LVARS."
  (dolist (d decls)
    (when (consp d)
      (multiple-value-bind (spec vars)
          (cond ((eq (car d) 'type) (values (second d) (cddr d)))
                ((member (car d) *decl-type-names*) (values (car d) (cdr d)))
                ((and (consp (car d)) (member (caar d) '(integer unsigned-byte signed-byte)))
                 (values (car d) (cdr d))))
        (when spec
          (dolist (v vars)
            (let ((lv (find v lvars :key #'lvar-name :from-end t)))
              (when (and lv (symbolp v)) (setf (lvar-decl lv) spec)))))))))

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

(defun convert-symbol (sym env)
  (cond ((or (null sym) (eq sym t) (keywordp sym)) `(:const ,sym))
        (t
         (let ((e (env-lookup-var env sym)))
           (cond ((and e (eq (first e) :var)) `(:lref ,(note-ref (third e))))
                 ((and e (eq (first e) :special)) `(:sref ,sym))
                 ((and e (eq (first e) :smacro)) (convert (third e) env))
                 (t (multiple-value-bind (exp found) (gethash sym *symbol-macros*)
                      (cond (found (convert exp env))
                            ((nth-value 1 (gethash sym *constants*))
                             (let ((v (car (gethash sym *constants*))))
                               (if (or (numberp v) (characterp v) (symbolp v) (stringp v))
                                   `(:const ,v)
                                   `(:sref ,sym))))
                            ((and *live-constant-hook* (nth-value 1 (funcall *live-constant-hook* sym)))
                             `(:const ,(funcall *live-constant-hook* sym)))
                            (t (unless (special-var-p sym)
                                 (cwarn "undefined variable ~S" sym))
                               `(:sref ,sym))))))))))

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
    (let ((cm (gethash op *compiler-macros*)))
      (when (and cm (not (gethash op *notinline*)))
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
    (when (and (member op '(ensure-generic-function clysm-sys::%add-method-to))
               (consp (second form)) (eq (car (second form)) 'quote) (symbolp (second (second form))))
      (setf (gethash (second (second form)) *dynamic-functions*) t))
    `(:call ,op ,@(convert-args (cdr form) env))))

(defun note-fun (fun)
  (note-ref (fun-lvar fun))
  fun)

(defun convert-args (args env)
  (mapcar (lambda (a) (convert a env)) args))

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
          (setf (lam-body lam) (convert-body body-forms env))))
      lam)))

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
            `(:if ,(convert test env) ,(convert then env) ,(convert else env))))
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

(defun convert-locally (decls forms env)
  (let ((sp (declared-specials decls)))
    (convert-body forms (if sp (apply #'env-extend env (mapcar (lambda (s) (list :special s)) sp))
                            env))))

(defun convert-let (bindings body env)
  (multiple-value-bind (decls forms) (parse-body body :doc-allowed nil)
    (let* ((dspecials (declared-specials decls))
           (inits (loop for b in bindings
                        collect (convert (if (consp b) (second b) nil) env)))
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
        (let ((new-env (apply #'env-extend env (append (reverse entries) free-specials))))
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
                                                         collect (list :special s))))
                         (let* ((b (car bs))
                                (sym (if (consp b) (first b) b))
                                (init (convert (if (consp b) (second b) nil) env))
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
                           `(:lset ,lv ,(convert val env))))
                        ((and e (eq (first e) :smacro))
                         (convert `(setf ,(third e) ,val) env))
                        ((and (null e) (nth-value 1 (gethash var *symbol-macros*)))
                         (convert `(setf ,(gethash var *symbol-macros*) ,val) env))
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

(defun convert-macrolet (defs body env)
  (let ((new-env (apply #'env-extend env
                        (loop for (name ll . mbody) in defs
                              collect (list :macro name
                                            (host-compile (make-host-macro-expander name ll mbody)))))))
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
        `(:mvbind ,lvars ,vnode ,(convert-body forms new-env))))))

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
            (let ((args (cdr form)))
              (if (cddr args) (fold-left op args) form))))))

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

(deftransform member (form)
  (let ((args (cdr form)))
    (cond ((/= (length args) 2) form)
          ((and (consp (second args)) (eq (car (second args)) 'quote)
                (let ((l (second (second args))))
                  (and (listp l) (null (cdr (last l))) (<= (length l) 64)
                       (every (lambda (x) (eq-comparable-constant-p (list 'quote x))) l))))
           `(clysm-sys::%memq ,@args))
          (t `(clysm-sys::%member-eql ,@args)))))
(deftransform assoc (form)
  (if (= (length form) 3) `(clysm-sys::%assoc-eql ,@(cdr form)) form))

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
