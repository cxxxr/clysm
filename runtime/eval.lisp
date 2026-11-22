;;;; eval.lisp — the evaluator (an interpreter running in the image),
;;;; MACROEXPAND, LOAD, COMPILE and the REPL.
;;;;
;;;; Lexical environments are lists of entries:
;;;;  (:var sym . cell) (:special sym) (:smacro sym . expansion)
;;;;  (:fun name . function) (:macro name . expander)
;;;;  (:block name . tag) (:tag name tag . index)

(defvar *macroexpand-hook* 'funcall)
(defvar *evalhook* nil)

(defun %env-lookup (env kind name)
  (dolist (e env nil)
    (when (and (eq (car e) kind) (equal (cadr e) name)) (return e))))

(defun %env-var (env sym)
  (dolist (e env nil)
    (when (and (member (car e) '(:var :special :smacro)) (eq (cadr e) sym)) (return e))))

(defun %env-fun (env name)
  (dolist (e env nil)
    (when (and (member (car e) '(:fun :macro)) (equal (cadr e) name)) (return e))))

(defun %env-macro (env sym)
  (let ((e (and (listp env) (%env-fun env sym))))
    (and e (eq (car e) :macro) (cddr e))))

;;; ------------------------------------------------------------
;;; macroexpansion
;;; ------------------------------------------------------------

(defvar *foreign-env-macroexpand-hook* nil
  "called for environments that are not interpreter environments (the embedded compiler's)")

(defun macroexpand-1 (form &optional env)
  (when (and env (not (listp env)) *foreign-env-macroexpand-hook*)
    (return-from macroexpand-1 (funcall *foreign-env-macroexpand-hook* form env)))
  (cond ((and (consp form) (symbolp (car form)))
         (let ((e (and env (%env-fun env (car form)))))
           (cond ((and e (eq (car e) :fun)) (values form nil))
                 (t (let ((m (if e (cddr e) (get (car form) '%macro-function))))
                      (if m
                          (values (funcall *macroexpand-hook* m form env) t)
                          (values form nil)))))))
        ((and (symbolp form) form (not (keywordp form)))
         (let ((e (and env (%env-var env form))))
           (cond ((and e (eq (car e) :smacro)) (values (cddr e) t))
                 (e (values form nil))
                 (t (let ((sm (get form '%symbol-macro)))
                      (if sm (values (car sm) t) (values form nil)))))))
        (t (values form nil))))

(defun macroexpand (form &optional env)
  (let ((expanded nil))
    (loop
      (multiple-value-bind (new changed) (macroexpand-1 form env)
        (unless changed (return (values form expanded)))
        (setq form new expanded t)))))

(defun %macroexpand-1 (form &optional env) (macroexpand-1 form env))

;;; MACROEXPAND-ALL: FORM with every macro form expanded, as far as a walk of
;;; the special forms finds them (SWANK's and micros's macroexpand-all, for
;;; C-c M-m).  The environment is the interpreter's, extended by the local
;;; macros, symbol macros, functions and variables of the forms walked
;;; through; an unknown special operator is left as it is.

(defun macroexpand-all (form &optional env)
  (%walk-form form env))

(defun %walk-forms (forms env)
  (if (listp forms)
      (mapcar (lambda (f) (%walk-form f env)) forms)
      forms))

(defun %walk-body (body env)
  "BODY (declarations kept as they are, documentation strings among them)."
  (let ((head '()))
    (loop while (and (consp body)
                     (or (and (consp (car body)) (eq (caar body) 'declare))
                         (and (stringp (car body)) (cdr body))))
          do (push (pop body) head))
    (append (nreverse head) (%walk-forms body env))))

(defun %shadow-variables (vars env)
  (dolist (v vars env)
    (when (symbolp v) (push (list* :var v nil) env))))

(defun %walk-lambda-list (ll env)
  "LL with its default forms walked; the environment of the body."
  (let ((out '()) (mode nil))
    (loop
      (cond ((null ll) (return))
            ((atom ll) (push ll out) (setq env (%shadow-variables (list ll) env)) (return)))
      (let ((p (pop ll)))
        (cond ((member p lambda-list-keywords) (setq mode p) (push p out))
              ((symbolp p) (push p out) (setq env (%shadow-variables (list p) env)))
              ((member mode '(&optional &key &aux))
               (let* ((var (if (and (eq mode '&key) (consp (car p))) (second (car p)) (car p)))
                      (init (and (cdr p) (%walk-form (second p) env))))
                 (push (if (cdr p) (list* (car p) init (cddr p)) p) out)
                 (setq env (%shadow-variables (list var (third p)) env))))
              (t (push p out)))))
    (values (nreverse out) env)))

(defun %walk-lambda (lambda env)
  ;; (LAMBDA lambda-list . body), or a named lambda (OP name lambda-list . body)
  (let ((named (not (eq (car lambda) 'lambda))))
    (multiple-value-bind (ll body-env)
        (%walk-lambda-list (if named (third lambda) (second lambda)) env)
      (if named
          (list* (first lambda) (second lambda) ll (%walk-body (cdddr lambda) body-env))
          (list* 'lambda ll (%walk-body (cddr lambda) body-env))))))

(defun %walk-form (form env)
  (cond ((symbolp form)
         (multiple-value-bind (new changed) (macroexpand-1 form env)
           (if changed (%walk-form new env) form)))
        ((atom form) form)
        ((and (consp (car form)) (eq (caar form) 'lambda))
         (cons (%walk-lambda (car form) env) (%walk-forms (cdr form) env)))
        ((not (symbolp (car form))) form)
        (t
         (let ((op (car form)))
           (case op
             ((quote go) form)
             (function
              (if (and (consp (second form)) (not (eq (car (second form)) 'setf)))
                  (list 'function (%walk-lambda (second form) env))
                  form))
             ((progn if catch throw unwind-protect multiple-value-call
               multiple-value-prog1 progv)
              (cons op (%walk-forms (cdr form) env)))
             ((block return-from the eval-when)
              (list* op (second form) (%walk-forms (cddr form) env)))
             (load-time-value
              (list* op (%walk-form (second form) env) (cddr form)))
             (locally (cons op (%walk-body (cdr form) env)))
             (tagbody
              (cons op (mapcar (lambda (x) (if (consp x) (%walk-form x env) x)) (cdr form))))
             (setq
              ;; a symbol macro set is SETF of its expansion
              (if (loop for (var) on (cdr form) by #'cddr
                        thereis (let ((e (and env (%env-var env var))))
                                  (if e (eq (car e) :smacro) (get var '%symbol-macro))))
                  (%walk-form (cons 'setf (cdr form)) env)
                  (cons op (loop for (var value) on (cdr form) by #'cddr
                                 collect var collect (%walk-form value env)))))
             ((let let*)
              (let ((body-env env))
                (list* op
                       (mapcar (lambda (b)
                                 (let ((var (if (consp b) (car b) b)))
                                   (prog1 (if (and (consp b) (cdr b))
                                              (list var (%walk-form (second b)
                                                                    (if (eq op 'let*) body-env env)))
                                              b)
                                     (setq body-env (%shadow-variables (list var) body-env)))))
                               (second form))
                       (%walk-body (cddr form) body-env))))
             ((flet labels)
              (let ((body-env env))
                (dolist (def (second form))
                  (push (list* :fun (car def) nil) body-env))
                (list* op
                       (mapcar (lambda (def)
                                 (let ((l (%walk-lambda (cons 'lambda (cdr def))
                                                        (if (eq op 'labels) body-env env))))
                                   (cons (car def) (cdr l))))
                               (second form))
                       (%walk-body (cddr form) body-env))))
             (macrolet
              (let ((body-env env))
                (dolist (def (second form))
                  (push (list* :macro (car def)
                               (%eval-function (%macro-lambda (car def) (cadr def) (cddr def)) env))
                        body-env))
                ;; the local macros are gone once expanded
                (cons 'locally (%walk-body (cddr form) body-env))))
             (symbol-macrolet
              (let ((body-env env))
                (dolist (b (second form))
                  (push (list* :smacro (car b) (cadr b)) body-env))
                (cons 'locally (%walk-body (cddr form) body-env))))
             (t
              (cond ((special-operator-p op) form)
                    (t (multiple-value-bind (new changed) (macroexpand-1 form env)
                         (if changed
                             (%walk-form new env)
                             (cons op (%walk-forms (cdr form) env))))))))))))

;;; the target DEFMACRO (the compiler handles DEFMACRO natively)
(defun %parse-macro-lambda-list (ll)
  "Returns (values whole-var env-var clean-ll)."
  (let ((whole nil) (envv nil) (clean nil))
    (loop
      (cond ((null ll) (return))
            ((atom ll) (setq clean (append clean (list '&rest ll))) (return))
            ((eq (car ll) '&whole) (setq whole (cadr ll) ll (cddr ll)))
            ((eq (car ll) '&environment) (setq envv (cadr ll) ll (cddr ll)))
            (t (setq clean (append clean (list (car ll))) ll (cdr ll)))))
    (values whole envv clean)))

(defun %split-decls-and-doc (body)
  "Like %SPLIT-DECLS, but a documentation string may appear among the declarations."
  (let ((decls nil) (doc nil))
    (loop
      (cond ((and (consp (car body)) (eq (caar body) 'declare))
             (setq decls (cons (car body) decls) body (cdr body)))
            ((and (stringp (car body)) (cdr body) (not doc))
             (setq doc t body (cdr body)))
            (t (return (values (nreverse decls) body)))))))

(defun %make-compiler-macro-function (name ll body)
  (eval `(function ,(%macro-lambda (if (consp name) (cadr name) name) ll body))))

(defun %macro-lambda (name ll body)
  (multiple-value-bind (whole envv clean) (%parse-macro-lambda-list ll)
    (let ((form (gensym "FORM")) (env (gensym "ENV")))
      (multiple-value-bind (decls forms) (%split-decls-and-doc body)
        (let ((inner `(destructuring-bind ,clean (cdr ,form)
                        ,@decls
                        (block ,name ,@forms))))
          ;; &whole may be a destructuring pattern
          (when (consp whole)
            (setq inner `(destructuring-bind ,whole ,form ,inner) whole nil))
          `(named-lambda ,name (,form ,env)
             (declare (ignorable ,form ,env))
             (let (,@(when whole `((,whole ,form))) ,@(when envv `((,envv ,env))))
               ,inner)))))))

(%set-macro-function 'defmacro
  (lambda (form env)
    (declare (ignore env))
    (destructuring-bind (name ll &rest body) (cdr form)
      `(progn (%set-macro-function ',name (function ,(%macro-lambda name ll body)))
              (%put ',name '%macro-lambda-list ',ll)
              ',name))))

(%set-macro-function 'lambda
  (lambda (form env) (declare (ignore env)) `(function ,form)))

;;; ------------------------------------------------------------
;;; the interpreter
;;; ------------------------------------------------------------

(defun eval (form) (%eval form nil))

(defun %special-declared-p (sym decls)
  (dolist (d decls nil)
    (when (and (consp d) (eq (car d) 'special) (member sym (cdr d))) (return t))))

(defun %decls-of (body)
  (let ((decls nil))
    (do () ((not (and (consp (car body)) (eq (caar body) 'declare))))
      (setq decls (append decls (cdar body)))
      (setq body (cdr body)))
    (values decls body)))

(defun %eval-locally-body (body env)
  "Evaluate BODY, which may start with declarations; SPECIAL declarations
affect references in BODY."
  (multiple-value-bind (decls forms) (%decls-of body)
    (let ((new-env env))
      (dolist (d decls)
        (when (and (consp d) (eq (car d) 'special))
          (dolist (s (cdr d)) (push (list :special s) new-env))))
      (%eval-body forms new-env))))

(defun %eval-body (forms env)
  (cond ((null forms) nil)
        ((null (cdr forms)) (%eval (car forms) env))
        (t (%eval (car forms) env) (%eval-body (cdr forms) env))))

(defun %eval (form env)
  (cond ((symbolp form) (%eval-symbol form env))
        ((atom form) form)
        (t (let ((op (car form)))
             (cond ((consp op)
                    (if (eq (car op) 'lambda)
                        (apply (%eval-function op env) (%eval-args (cdr form) env))
                        (error "Illegal function call: ~S" form)))
                   ((not (symbolp op)) (error "Illegal function call: ~S" form))
                   (t
                    (let ((e (%env-fun env op)))
                      (cond ((and e (eq (car e) :fun))
                             (apply (cddr e) (%eval-args (cdr form) env)))
                            ((and e (eq (car e) :macro))
                             (%eval (funcall *macroexpand-hook* (cddr e) form env) env))
                            (t (%eval-compound op form env))))))))))

(defun %eval-args (args env)
  (let ((r nil)) (dolist (a args (nreverse r)) (push (%eval a env) r))))

(defun %eval-symbol (sym env)
  (cond ((or (null sym) (eq sym t) (keywordp sym)) sym)
        (t (let ((e (%env-var env sym)))
             (cond ((null e)
                    (let ((sm (get sym '%symbol-macro)))
                      (if sm (%eval (car sm) env) (symbol-value sym))))
                   ((eq (car e) :var) (car (cddr e)))
                   ((eq (car e) :special) (symbol-value sym))
                   (t (%eval (cddr e) env)))))))

(defun %eval-setq (pairs env)
  (let ((result nil))
    (do ((p pairs (cddr p))) ((null p) result)
      (let* ((sym (car p)) (e (%env-var env sym)))
        (cond ((and e (eq (car e) :smacro))
               (setq result (%eval `(setf ,(cddr e) ,(cadr p)) env)))
              ((and (null e) (get sym '%symbol-macro))
               (setq result (%eval `(setf ,(car (get sym '%symbol-macro)) ,(cadr p)) env)))
              (t
               (let ((v (%eval (cadr p) env)))
                 (setq result v)
                 (if (and e (eq (car e) :var))
                     (setf (car (cddr e)) v)
                     (set sym v)))))))))

(defun %eval-let (bindings body env sequential)
  (multiple-value-bind (decls forms) (%decls-of body)
    (let ((new-env env) (specials nil) (special-vals nil))
      (dolist (b bindings)
        (let* ((sym (if (consp b) (car b) b))
               (val (if (consp b) (%eval (cadr b) (if sequential new-env env)) nil)))
          (if (or (%special-declared-p sym decls) (%special-p sym))
              (if sequential
                  ;; bind immediately so that later inits see the binding
                  (return-from %eval-let
                    (progv (list sym) (list val)
                      (%eval-let (cdr (member b bindings)) body
                                 (cons (list :special sym) new-env) t)))
                  (progn (push sym specials) (push val special-vals)
                         (push (list :special sym) new-env)))
              (push (list* :var sym (list val)) new-env))))
      (dolist (d decls)
        (when (and (consp d) (eq (car d) 'special))
          (dolist (s (cdr d)) (push (list :special s) new-env))))
      (if specials
          (progv specials special-vals (%eval-body forms new-env))
          (%eval-body forms new-env)))))

(defun %eval-compound (op form env)
  (case op
    (quote (cadr form))
    (if (if (%eval (cadr form) env) (%eval (caddr form) env) (%eval (cadddr form) env)))
    (progn (%eval-body (cdr form) env))
    (setq (%eval-setq (cdr form) env))
    (let (%eval-let (cadr form) (cddr form) env nil))
    (let* (%eval-let (cadr form) (cddr form) env t))
    (function (%eval-function (cadr form) env))
    ((flet labels)
     (let* ((new-env env)
            (cells (mapcar (lambda (def)
                             (let ((e (list* :fun (car def) nil)))
                               (push e new-env)
                               e))
                           (cadr form)))
            (def-env (if (eq op 'labels) new-env env)))
       (mapc (lambda (e def)
               (setf (cddr e) (%make-interpreted-function (car def) (cadr def) (cddr def) def-env
                                                          (if (consp (car def)) (cadar def) (car def)))))
             cells (cadr form))
       (%eval-locally-body (cddr form) new-env)))
    (macrolet
     (let ((new-env env))
       (dolist (def (cadr form))
         (push (list* :macro (car def)
                      (%eval-function (%macro-lambda (car def) (cadr def) (cddr def)) env))
               new-env))
       (%eval-locally-body (cddr form) new-env)))
    (symbol-macrolet
     (let ((new-env env))
       (dolist (b (cadr form))
         (when (or (not (symbolp (car b))) (constantp (car b)) (%special-p (car b)))
           (error 'simple-program-error :format-control "SYMBOL-MACROLET: cannot bind ~S"
                                        :format-arguments (list (car b))))
         (push (list* :smacro (car b) (cadr b)) new-env))
       (dolist (d (%decls-of (cddr form)))
         (when (and (consp d) (eq (car d) 'special)
                    (some (lambda (s) (assoc s (cadr form))) (cdr d)))
           (error 'simple-program-error :format-control "SYMBOL-MACROLET: ~S declared special"
                                        :format-arguments (list (cdr d)))))
       (%eval-locally-body (cddr form) new-env)))
    (block
     (let ((tag (list (cadr form))))
       (catch tag (%eval-body (cddr form) (cons (list* :block (cadr form) tag) env)))))
    (return-from
     (let ((e (%env-lookup env :block (cadr form))))
       (unless e (error "RETURN-FROM: unknown block ~S" (cadr form)))
       (throw (cddr e) (%eval (caddr form) env))))
    (tagbody (%eval-tagbody (cdr form) env))
    (go (let ((e (dolist (x env nil) (when (and (eq (car x) :tag) (eql (cadr x) (cadr form))) (return x)))))
          (unless e (error "GO: unknown tag ~S" (cadr form)))
          (throw (caddr e) (cdddr e))))
    ((catch %catch) (catch (%eval (cadr form) env) (%eval-body (cddr form) env)))
    (throw (throw (%eval (cadr form) env) (%eval (caddr form) env)))
    (unwind-protect (unwind-protect (%eval (cadr form) env) (%eval-body (cddr form) env)))
    (multiple-value-call
     (let ((f (%eval (cadr form) env)) (args nil))
       (dolist (a (cddr form)) (setq args (append args (multiple-value-list (%eval a env)))))
       (apply f args)))
    (multiple-value-prog1 (multiple-value-prog1 (%eval (cadr form) env) (%eval-body (cddr form) env)))
    (multiple-value-bind
     (destructuring-bind (vars value-form &rest body) (cdr form)
       (let ((vals (multiple-value-list (%eval value-form env))))
         (%eval-let (mapcar (lambda (v) (list v (list 'quote (pop vals)))) vars) body env nil))))
    (the (%eval (caddr form) env))
    (locally (%eval-locally-body (cdr form) env))
    (eval-when (if (or (member :execute (cadr form)) (member 'eval (cadr form)))
                   (%eval-body (cddr form) env)
                   nil))
    (load-time-value (%eval (cadr form) nil))
    (progv (progv (%eval (cadr form) env) (%eval (caddr form) env) (%eval-body (cdddr form) env)))
    (%defun (let ((name (%eval (cadr form) env)) (fn (%eval (caddr form) env)))
              (when (and (null env) (consp (caddr form)) (eq (car (caddr form)) 'function))
                (%put (%function-name-symbol name) '%lambda-source (cadr (caddr form))))
              (%set-fdefinition name fn)
              (when (and (symbolp name) (null (%closure-name fn))) (%set-closure-name fn name))
              name))
    (named-lambda (%eval-function form env))
    (declaim (%proclaim-list (cdr form)))
    (otherwise
     (let ((m (get op '%macro-function)))
       (if m
           (%eval (funcall *macroexpand-hook* m form env) env)
           (apply (symbol-function op) (%eval-args (cdr form) env)))))))

(defun %eval-tagbody (body env)
  (let* ((tag (list 'tagbody))
         (segments (let ((r (list (list 0))) (i 0))
                     (dolist (x body)
                       (if (or (symbolp x) (integerp x))
                           (push (list (setq i (+ i 1))) r)
                           (push x (cdr (car r)))))
                     (mapcar (lambda (s) (cons (car s) (reverse (cdr s)))) (reverse r))))
         (new-env env)
         (i 0))
    (dolist (x body)
      (when (or (symbolp x) (integerp x))
        (setq i (+ i 1))
        (push (list* :tag x tag i) new-env)))
    (let ((pc 0))
      (loop
        (let ((result (catch tag
                        (dolist (seg (nthcdr pc segments) :done)
                          (dolist (f (cdr seg)) (%eval f new-env))))))
          (if (eq result :done)
              (return nil)
              (setq pc result)))))))

(defun %eval-function (spec env)
  (cond ((symbolp spec)
         (let ((e (%env-fun env spec)))
           (if (and e (eq (car e) :fun)) (cddr e) (fdefinition spec))))
        ((and (consp spec) (eq (car spec) 'setf))
         (let ((e (%env-fun env spec)))
           (if (and e (eq (car e) :fun)) (cddr e) (fdefinition spec))))
        ((and (consp spec) (eq (car spec) 'lambda))
         (%make-interpreted-function nil (cadr spec) (cddr spec) env nil))
        ((and (consp spec) (eq (car spec) 'named-lambda))
         (%make-interpreted-function (cadr spec) (caddr spec) (cdddr spec) env
                                     (if (consp (cadr spec)) (cadadr spec) (cadr spec))))
        (t (error "Bad function designator ~S" spec))))

(defun %function-body-parts (body)
  "Split a function body into declaration specifiers and forms; one
documentation string may appear among the declarations."
  (let ((decls nil) (doc nil))
    (loop
      (cond ((and (consp (car body)) (eq (caar body) 'declare))
             (setq decls (append decls (cdar body)) body (cdr body)))
            ((and (stringp (car body)) (cdr body) (not doc))
             (setq doc t body (cdr body)))
            (t (return (values decls body)))))))

(defvar *eval-closures* t
  "True when the body of an interpreted function is made closures at its second call (%CC-BODY).")

(defun %make-interpreted-function (name ll body env block-name &optional code-cell)
  (multiple-value-bind (decls forms) (%function-body-parts body)
    (let* ((body-forms (if block-name `((block ,block-name ,@forms)) forms))
           ;; (the body made closures when the function is called again: %CC-BODY)
           (code (or code-cell (list nil)))
           ;; (required parameters only, none special: bound without the
           ;; walk of the lambda list and the frame that restores specials)
           (simple (and (dolist (x ll t)
                          (when (or (not (symbolp x)) (member x lambda-list-keywords) (%special-p x))
                            (return nil)))
                        (dolist (d decls t)
                          (when (and (consp d) (eq (car d) 'special)) (return nil)))))
           ;; (one function for both, with the same free variables: the
           ;; functions of a generic function made by interpreted code are
           ;; put together into one closure, %MAKE-GF-CLOSURE, which takes
           ;; them to have the same layout)
           (f (lambda (&rest args)
                (if simple
                    (%apply-interpreted-simple ll body-forms env args code)
                    (%apply-interpreted ll decls body-forms env args code)))))
      (%set-closure-name f name)
      f)))

(defun %run-interpreted-body (body env code)
  "Evaluate the forms BODY of a function in ENV, the environment of a call
of it: by %EVAL the first time, then by the closures made of them (%CC-BODY,
kept in the cell CODE)."
  ;; (the first call is evaluated: a function called once, a local function
  ;; made again each time its FLET is evaluated)
  (let ((c (car code)))
    (cond ((functionp c) (funcall c env))
          ((or (null c) (not *eval-closures*))
           (setf (car code) :called)
           (%eval-body body env))
          (t (setq c (%cc-body body env))
             (setf (car code) c)
             (funcall (the function c) env)))))

(defun %apply-interpreted-simple (ll body env args code)
  (let ((new-env env) (rest args))
    (dolist (sym ll)
      (when (null rest)
        (error 'simple-program-error :format-control "too few arguments" :format-arguments nil))
      (setq new-env (cons (list* :var sym (list (car rest))) new-env))
      (setq rest (cdr rest)))
    (when rest
      (error 'simple-program-error :format-control "too many arguments: ~S" :format-arguments (list args)))
    (%run-interpreted-body body new-env code)))

(defun %apply-interpreted (ll decls body env args code)
  ;; special parameters are bound as soon as they are processed, so that
  ;; later init forms see the bindings; SAVED restores them on exit
  (let ((saved nil))
    (unwind-protect (%apply-interpreted-1 ll decls body env args code
                                          (lambda (sym val)
                                            (push (if (boundp sym) (list sym (symbol-value sym)) (list sym))
                                                  saved)
                                            (set sym val)))
      (do () ((null saved))
        (let ((e (car saved)))
          (setq saved (cdr saved))
          (if (cdr e) (set (car e) (cadr e)) (makunbound (car e))))))))

(defun %apply-interpreted-1 (ll decls body env args code bind-special)
  (let ((new-env env) (state :req) (keys-seen nil) (rest-args args)
        (aok nil))
    (labels ((bind (sym val)
               (if (or (%special-declared-p sym decls) (%special-p sym))
                   (progn (funcall bind-special sym val) (push (list :special sym) new-env))
                   (push (list* :var sym (list val)) new-env)))
             (init (form) (%eval form new-env)))
      (do ((l ll (cdr l))) ((null l))
        (let ((x (car l)))
          (case x
            (&optional (setq state :opt))
            ((&rest &body) (bind (cadr l) rest-args) (setq l (cdr l)) (setq state :after-rest))
            (&key (setq state :key keys-seen t))
            (&allow-other-keys (setq aok t))
            (&aux (setq state :aux))
            (t (case state
                 (:req (when (null rest-args) (error 'simple-program-error :format-control "too few arguments"
                                                                           :format-arguments nil))
                  (bind x (pop rest-args)))
                 (:opt (let ((var (if (consp x) (car x) x)) (default (if (consp x) (cadr x) nil))
                             (svar (if (consp x) (caddr x) nil)))
                         (if rest-args
                             (progn (bind var (pop rest-args)) (when svar (bind svar t)))
                             (progn (bind var (init default)) (when svar (bind svar nil))))))
                 (:key (let* ((spec (if (consp x) (car x) x))
                              (var (if (consp spec) (cadr spec) spec))
                              (kw (if (consp spec) (car spec) (intern (symbol-name spec) *keyword-package*)))
                              (default (if (consp x) (cadr x) nil))
                              (svar (if (consp x) (caddr x) nil))
                              (v (%db-getf rest-args kw)))
                         (if (eq v '%db-none)
                             (progn (bind var (init default)) (when svar (bind svar nil)))
                             (progn (bind var v) (when svar (bind svar t))))))
                 (:aux (bind (if (consp x) (car x) x) (if (consp x) (init (cadr x)) nil)))
                 (t nil))))))
      (when (and (member state '(:req :opt)) rest-args (not (member '&rest ll)) (not (member '&body ll)))
        (error 'simple-program-error :format-control "too many arguments: ~S" :format-arguments (list args)))
      (when (and keys-seen (not aok) (not (getf rest-args :allow-other-keys)))
        (let ((allowed (mapcar (lambda (x)
                                 (let ((spec (if (consp x) (car x) x)))
                                   (if (consp spec) (car spec) (intern (symbol-name spec) *keyword-package*))))
                               (let ((after (cdr (member '&key ll))))
                                 (ldiff after (member-if (lambda (y) (member y '(&aux &allow-other-keys))) after))))))
          (do ((k rest-args (cddr k))) ((atom k))
            (unless (or (member (car k) allowed) (eq (car k) :allow-other-keys))
              (error 'simple-program-error :format-control "unknown keyword argument ~S"
                                           :format-arguments (list (car k)))))))
      (dolist (d decls)
        (when (and (consp d) (eq (car d) 'special))
          (dolist (s (cdr d)) (push (list :special s) new-env))))
      (%run-interpreted-body body new-env code))))

;;; ------------------------------------------------------------
;;; the body of an interpreted function as closures
;;; ------------------------------------------------------------
;;;
;;; %EVAL walks a form each time it is evaluated: it finds what the operator
;;; is, looks the variables up by name in the environment, and expands the
;;; macros again (a backquote, DESTRUCTURING-BIND, COND: a third of the time
;;; of a macro's expander that is interpreted).  The body of an interpreted
;;; function is made closures when the function is called a second time (%CC: a
;;; function of the environment for each form, the macros expanded once, a
;;; lexical variable found by its place in the environment), and they are
;;; called from then on.  The environment is the same list as %EVAL's, with
;;; the same entries, so that a form %CC does not take apart is evaluated by
;;; %EVAL in it (%CC-FALLBACK: the forms that bind special variables or
;;; local functions and macros, symbol macros, PROGV...), and the static
;;; environment that %CC works with is the environment of that call
;;; itself, extended with entries of the same kinds in the same order as the
;;; closures will push: a function's environment has the same shape at every
;;; call.  *EVAL-CLOSURES* NIL evaluates the bodies with %EVAL as before.

(defun %cc-fallback (form)
  (lambda (env) (%eval form env)))

(defun %cc-body (forms senv)
  (cond ((null forms) (lambda (env) (declare (ignore env)) nil))
        ((null (cdr forms)) (%cc (car forms) senv))
        ((null (cddr forms))
         (let ((a (%cc (car forms) senv)) (b (%cc (cadr forms) senv)))
           (declare (function a b))
           (lambda (env) (funcall a env) (funcall b env))))
        (t (let ((first (mapcar (lambda (f) (%cc f senv)) (butlast forms)))
                 (last (%cc (car (last forms)) senv)))
             (declare (function last))
             (lambda (env)
               (dolist (c first) (funcall (the function c) env))
               (funcall last env))))))

(defun %cc-position (entry senv)
  (let ((i 0))
    (declare (fixnum i))
    (do ((l senv (cdr l))) ((null l) nil)
      (when (eq (car l) entry) (return i))
      (setq i (+ i 1)))))

(defun %cc-symbol (sym senv)
  (if (or (null sym) (eq sym t) (keywordp sym))
      (lambda (env) (declare (ignore env)) sym)
      (let ((e (%env-var senv sym)))
        (cond ((null e)
               (if (get sym '%symbol-macro)
                   (%cc-fallback sym)
                   (lambda (env) (declare (ignore env)) (symbol-value sym))))
              ((eq (car e) :var)
               (let ((i (%cc-position e senv)))
                 (declare (fixnum i))
                 (case i
                   (0 (lambda (env) (car (cddr (car env)))))
                   (1 (lambda (env) (car (cddr (cadr env)))))
                   (2 (lambda (env) (car (cddr (caddr env)))))
                   (t (lambda (env) (car (cddr (nth i env))))))))
              ((eq (car e) :special)
               (lambda (env) (declare (ignore env)) (symbol-value sym)))
              (t (%cc-fallback sym))))))

(defun %cc-simple-bindings-p (bindings body)
  "Whether a LET, LET* or MULTIPLE-VALUE-BIND of BINDINGS (variables, or
(variable form)) with BODY binds lexical variables only and declares
nothing special."
  (and (dolist (b bindings t)
         (let ((sym (if (consp b) (car b) b)))
           (when (or (not (symbolp sym)) (%special-p sym)) (return nil))))
       (dolist (d (%decls-of body) t)
         (when (and (consp d) (eq (car d) 'special)) (return nil)))))

(defun %cc-let (form senv sequential)
  (let ((bindings (cadr form)) (body (cddr form)))
    (if (not (%cc-simple-bindings-p bindings body))
        (%cc-fallback form)
        (let ((syms nil) (inits nil) (new senv))
          (dolist (b bindings)
            (let ((sym (if (consp b) (car b) b)))
              (push sym syms)
              (push (if (and (consp b) (cdr b)) (%cc (cadr b) (if sequential new senv)) nil) inits)
              (push (list :var sym) new)))
          (setq syms (nreverse syms) inits (nreverse inits))
          (let ((code (multiple-value-bind (decls forms) (%decls-of body)
                        (declare (ignore decls))
                        (%cc-body forms new))))
            (declare (function code))
            (cond ((null syms) code)
                  ((null (cdr syms))
                   (let ((sym (car syms)) (init (car inits)))
                     (lambda (env)
                       (funcall code (cons (list* :var sym (list (if init (funcall (the function init) env) nil)))
                                           env)))))
                  (sequential
                   (lambda (env)
                     (let ((new env))
                       (do ((s syms (cdr s)) (i inits (cdr i))) ((null s))
                         (setq new (cons (list* :var (car s)
                                                (list (if (car i) (funcall (the function (car i)) new) nil)))
                                         new)))
                       (funcall code new))))
                  (t
                   (lambda (env)
                     (let ((new env))
                       (do ((s syms (cdr s)) (i inits (cdr i))) ((null s))
                         (setq new (cons (list* :var (car s)
                                                (list (if (car i) (funcall (the function (car i)) env) nil)))
                                         new)))
                       (funcall code new))))))))))

(defun %cc-setq (form senv)
  (let ((pairs (cdr form)))
    (cond ((null pairs) (lambda (env) (declare (ignore env)) nil))
          ((cddr pairs)
           (%cc-body (let ((r nil))
                       (do ((p pairs (cddr p))) ((null p) (nreverse r))
                         (push (list 'setq (car p) (cadr p)) r)))
                     senv))
          (t (let* ((sym (car pairs)) (e (%env-var senv sym)))
               (cond ((or (and e (eq (car e) :smacro)) (and (null e) (get sym '%symbol-macro)))
                      (%cc-fallback form))
                     ((and e (eq (car e) :var))
                      (let ((i (%cc-position e senv)) (v (%cc (cadr pairs) senv)))
                        (declare (fixnum i) (function v))
                        (lambda (env) (setf (car (cddr (nth i env))) (funcall v env)))))
                     (t (let ((v (%cc (cadr pairs) senv)))
                          (declare (function v))
                          (lambda (env) (set sym (funcall v env)))))))))))

(defun %cc-call (op args senv)
  "A call of the global function OP."
  (let ((n (length args)) (cs (mapcar (lambda (a) (%cc a senv)) args)))
    (case n
      (0 (lambda (env) (declare (ignore env)) (funcall (symbol-function op))))
      (1 (let ((a (car cs)))
           (declare (function a))
           (lambda (env) (funcall (symbol-function op) (funcall a env)))))
      (2 (let ((a (car cs)) (b (cadr cs)))
           (declare (function a b))
           (lambda (env) (funcall (symbol-function op) (funcall a env) (funcall b env)))))
      (3 (let ((a (car cs)) (b (cadr cs)) (c (caddr cs)))
           (declare (function a b c))
           (lambda (env) (funcall (symbol-function op) (funcall a env) (funcall b env) (funcall c env)))))
      (t (lambda (env)
           (let ((r nil))
             (dolist (c cs) (push (funcall (the function c) env) r))
             (apply (symbol-function op) (nreverse r))))))))

(defun %cc-tagbody (form senv)
  (let* ((body (cdr form)) (new senv) (i 0) (entries nil))
    (dolist (x body)
      (when (or (symbolp x) (integerp x))
        (setq i (+ i 1))
        (push (cons x i) entries)
        (push (list :tag x) new)))
    (setq entries (nreverse entries))
    (let ((segments (let ((r (list nil)))
                      (dolist (x body)
                        (if (or (symbolp x) (integerp x))
                            (push nil r)
                            (push (%cc x new) (car r))))
                      (coerce (mapcar #'reverse (reverse r)) 'simple-vector))))
      (lambda (env)
        (let ((tag (list 'tagbody)) (new env) (n (length segments)))
          (dolist (e entries) (push (list* :tag (car e) tag (cdr e)) new))
          (let ((pc 0))
            (loop
              (let ((result (catch tag
                              (do ((k pc (+ k 1))) ((>= k n) :done)
                                (dolist (c (svref segments k)) (funcall (the function c) new))))))
                (if (eq result :done)
                    (return nil)
                    (setq pc result))))))))))

(defun %cc-compound (op form senv)
  (case op
    (quote (let ((v (cadr form))) (lambda (env) (declare (ignore env)) v)))
    (if (let ((c (%cc (cadr form) senv)) (a (%cc (caddr form) senv)) (b (%cc (cadddr form) senv)))
          (declare (function c a b))
          (lambda (env) (if (funcall c env) (funcall a env) (funcall b env)))))
    (progn (%cc-body (cdr form) senv))
    (setq (%cc-setq form senv))
    (let (%cc-let form senv nil))
    (let* (%cc-let form senv t))
    (function
     (let ((spec (cadr form)))
       (cond ((symbolp spec)
              (if (%env-fun senv spec)
                  (%cc-fallback form)
                  (lambda (env) (declare (ignore env)) (fdefinition spec))))
             ((and (consp spec) (eq (car spec) 'lambda))
              ;; (the closures of its body are those of every function made here)
              (let ((cell (list nil)) (ll (cadr spec)) (body (cddr spec)))
                (lambda (env) (%make-interpreted-function nil ll body env nil cell))))
             (t (%cc-fallback form)))))
    (block
     (let* ((name (cadr form))
            (code (%cc-body (cddr form) (cons (list :block name) senv))))
       (declare (function code))
       (lambda (env)
         (let ((tag (list name)))
           (catch tag (funcall code (cons (list* :block name tag) env)))))))
    (return-from
     (let ((e (%env-lookup senv :block (cadr form))))
       (if (null e)
           (%cc-fallback form)
           (let ((i (%cc-position e senv)) (v (%cc (caddr form) senv)))
             (declare (fixnum i) (function v))
             (lambda (env) (throw (cddr (nth i env)) (funcall v env)))))))
    (tagbody (%cc-tagbody form senv))
    (go (let ((e (dolist (x senv nil)
                   (when (and (eq (car x) :tag) (eql (cadr x) (cadr form))) (return x)))))
          (if (null e)
              (%cc-fallback form)
              (let ((i (%cc-position e senv)))
                (declare (fixnum i))
                (lambda (env) (let ((x (nth i env))) (throw (caddr x) (cdddr x))))))))
    ((catch %catch)
     (let ((tag (%cc (cadr form) senv)) (code (%cc-body (cddr form) senv)))
       (declare (function tag code))
       (lambda (env) (catch (funcall tag env) (funcall code env)))))
    (throw (let ((tag (%cc (cadr form) senv)) (v (%cc (caddr form) senv)))
             (declare (function tag v))
             (lambda (env) (throw (funcall tag env) (funcall v env)))))
    (unwind-protect
         (let ((code (%cc (cadr form) senv)) (cleanup (%cc-body (cddr form) senv)))
           (declare (function code cleanup))
           (lambda (env) (unwind-protect (funcall code env) (funcall cleanup env)))))
    (multiple-value-prog1
        (let ((code (%cc (cadr form) senv)) (rest (%cc-body (cddr form) senv)))
          (declare (function code rest))
          (lambda (env) (multiple-value-prog1 (funcall code env) (funcall rest env)))))
    (multiple-value-call
        (let ((f (%cc (cadr form) senv)) (cs (mapcar (lambda (a) (%cc a senv)) (cddr form))))
          (declare (function f))
          (lambda (env)
            (let ((fn (funcall f env)) (args nil))
              (dolist (c cs) (setq args (append args (multiple-value-list (funcall (the function c) env)))))
              (apply fn args)))))
    (multiple-value-bind
          (let ((vars (cadr form)) (body (cdddr form)))
            (if (not (%cc-simple-bindings-p vars body))
                (%cc-fallback form)
                (let* ((value (%cc (caddr form) senv))
                       (new (let ((n senv)) (dolist (v vars n) (push (list :var v) n))))
                       (code (multiple-value-bind (decls forms) (%decls-of body)
                               (declare (ignore decls))
                               (%cc-body forms new))))
                  (declare (function value code))
                  (lambda (env)
                    (let ((vals (multiple-value-list (funcall value env))) (new env))
                      (dolist (v vars)
                        (setq new (cons (list* :var v (list (car vals))) new))
                        (setq vals (cdr vals)))
                      (funcall code new)))))))
    (the (%cc (caddr form) senv))
    (locally
        (multiple-value-bind (decls forms) (%decls-of (cdr form))
          (if (dolist (d decls nil) (when (and (consp d) (eq (car d) 'special)) (return t)))
              (%cc-fallback form)
              (%cc-body forms senv))))
    (eval-when (if (or (member :execute (cadr form)) (member 'eval (cadr form)))
                   (%cc-body (cddr form) senv)
                   (lambda (env) (declare (ignore env)) nil)))
    ;; (the other forms that %EVAL-COMPOUND knows: evaluated by it)
    ((flet labels macrolet symbol-macrolet load-time-value progv %defun named-lambda declaim)
     (%cc-fallback form))
    (otherwise
     (let ((m (get op '%macro-function)))
       (if m
           (%cc (funcall *macroexpand-hook* m form senv) senv)
           (%cc-call op (cdr form) senv))))))

(defun %cc (form senv)
  "A function of an environment that evaluates FORM in it, for the
environments of the shape of SENV (see above)."
  (cond ((symbolp form) (%cc-symbol form senv))
        ((atom form) (lambda (env) (declare (ignore env)) form))
        (t (let ((op (car form)))
             (cond ((not (symbolp op)) (%cc-fallback form))
                   ;; (a local function or macro)
                   ((%env-fun senv op) (%cc-fallback form))
                   (t (%cc-compound op form senv)))))))

;;; ------------------------------------------------------------
;;; compile, load
;;; ------------------------------------------------------------

(defvar *native-compile-hook* nil "set when the compiler is part of the image")
(defvar *interrupt-checks* nil
  "When true, code compiled in the image checks for interrupts on function
entry and at the head of loops: %INTERRUPT-POLL is called every so often.")
(defvar *interrupt-poll-function* nil
  "A function of no arguments called by %INTERRUPT-POLL, or NIL.")

(defun %interrupt-poll ()
  (%thread-poll)
  (let ((f *interrupt-poll-function*))
    (when f (funcall f)))
  nil)

(defvar *record-frames* nil
  "When true, functions compiled in the image record their variables for the
debugger (see *DEBUG-FRAME-STACK*).")
(defvar *debug-frame-stack* nil
  "The innermost frame of the functions compiled with *RECORD-FRAMES* that
are active: #(name variable-names nparams parent value...), PARENT the
frame of the caller or NIL.")
(defvar %no-value (make-symbol "NO-VALUE") "The value of a variable of a frame not bound yet.")

(defvar *native-eval-hook* nil "when set, the REPL compiles each form natively")
(defvar *native-eval-forms-hook* nil
  "When the compiler is part of the image: a function evaluating a list of
toplevel forms compiled into one module (NIL when that fails before any of
them ran, the caller then evaluating them one by one).")
(defvar *native-compile-form-hook* nil
  "When the compiler is part of the image: a function compiling a toplevel form
to a Wasm module linked with the image, returning (values octets constants).")
(defvar *native-run-module-hook* nil
  "When the compiler is part of the image: a function instantiating and running
a module made by *NATIVE-COMPILE-FORM-HOOK* (octets, constants).")
(defvar *native-run-octets-hook* nil
  "When the compiler is part of the image: a function instantiating and running
a module made by *NATIVE-COMPILE-FORM-HOOK* from its octets and constants.")
(defvar *direct-closures* nil
  "While a module compiled in the image is instantiated: the functions whose
entries it imports (the helper :DIRECT-ENTRY reads it, src/prims.lisp).")
(defun %run-module-direct (bytes consts direct)
  ;; (set, not bound: the module's code runs in the same call, after the
  ;; host has instantiated it, and must not see a binding)
  (setq *direct-closures* direct)
  (%compile-module bytes consts))
(defvar *image-id* nil
  "Identifies the image build; compiled modules in fasls are only valid for it.")
(defvar *native-unit-hooks* nil
  "When the compiler is part of the image: (CALL-WITH-UNIT ADD-FORM END-SEGMENT
FINISH BEGIN-FILE OPEN CALL-IN-UNIT RUNNING-MODULE), the functions with which
COMPILE-FILE compiles a file into one module and %SYSTEM-COMPILE-FILE a
system's files (src/live.lisp, \"Compilation units\").")
(defvar *fasl-unit-runner* nil
  "While loading a compiled file: the function of a segment number and a
vector of objects that runs a segment of the module of the file (set by its
first segment), or :SOURCE when the module was made for another image.")

(defun compile (name &optional definition)
  (when *native-compile-hook*
    (return-from compile (funcall *native-compile-hook* name definition)))
  (let ((fn (cond ((functionp definition) definition)
                  (definition (eval (if (and (consp definition) (eq (car definition) 'lambda))
                                        `(function ,definition) definition)))
                  (t (fdefinition name)))))
    (when name (%set-fdefinition name fn))
    (values (or name fn) nil nil)))

(defun compiled-function-p (x) (functionp x))

(defvar *load-pathname* nil)
(defvar *load-truename* nil)
(defvar *load-verbose* nil)
(defvar *load-print* nil)
(defvar *compile-verbose* nil)
(defvar *compile-print* nil)
(defvar *compile-file-pathname* nil)
(defvar *compile-file-truename* nil)

;;; Compiled files.  COMPILE-FILE performs the file compiler's toplevel
;;; processing (CLHS 3.2.3.1): macros are expanded, EVAL-WHEN and definers
;;; with compile-time side effects are evaluated, and the forms to be
;;; processed at load time are written to the output file, printed readably
;;; with every symbol package-qualified.  LOAD recognizes such a file by its
;;; first line and reads it with standard syntax; each form is compiled when
;;; it is loaded.

(defvar %fasl-header ";;; Clysm compiled file")

(defun %eval-toplevel (form)
  (if *native-eval-hook* (funcall *native-eval-hook* form) (eval form)))

(defun %load-file-stream-p (stream)
  (let ((pos (file-position stream)))
    (let ((line (read-line stream nil nil)))
      (if (and line (string= line %fasl-header))
          t
          (progn (file-position stream pos) nil)))))

(defun %load-pathname-of (filespec)
  (cond ((typep filespec 'file-stream) (pathname filespec))
        ((streamp filespec) nil)
        (t (merge-pathnames filespec))))

(defun %call-with-fasl-syntax (fn)
  (let ((*fasl-syntax* t)
        (*package* *keyword-package*)
        (*print-array* t) (*print-base* 10) (*print-case* :upcase) (*print-circle* t)
        (*print-escape* t) (*print-gensym* t) (*print-length* nil) (*print-level* nil)
        (*print-lines* nil) (*print-pretty* nil) (*print-radix* nil)
        (*print-readably* t) (*print-right-margin* nil) (*read-base* 10)
        (*read-default-float-format* 'single-float) (*read-eval* t) (*read-suppress* nil)
        (*readtable* *standard-readtable*))
    (funcall fn)))

(defvar *fasl-modules* nil "While loading a compiled file: (native-name . start octet).")

(defun %load-fasl-entry (entry stream)
  "Evaluate a fasl entry (see COMPILE-FILE): its compiled module is run when it
was made for this image, otherwise its source form (stored after the module)
is read and compiled again."
  (destructuring-bind (id offset length consts source-offset source-length) (cdr entry)
    (declare (ignore source-length))
    (if (and *native-run-module-hook* (equal id *image-id*))
        (funcall *native-run-module-hook*
                 (car *fasl-modules*) (+ (cdr *fasl-modules*) offset) length consts)
        (%eval-toplevel (%fasl-entry-source stream source-offset)))))

(defun %fasl-entry-source (stream source-offset)
  "The source form stored in the module octets of the fasl at SOURCE-OFFSET.
STREAM is the fasl, or a function returning it (opened when first needed)."
  (let* ((stream (if (functionp stream) (funcall stream) stream))
         (here (file-position stream)))
    (file-position stream (+ (cdr *fasl-modules*) source-offset))
    (prog1 (%call-with-fasl-syntax
            (lambda () (let ((*print-readably* nil)) (read stream))))
      (file-position stream here))))

(defun %load-fasl-unit (entry stream)
  "The first segment of the module of a compiled file (see COMPILE-FILE): the
module is instantiated and the segment run (it sets *FASL-UNIT-RUNNER*), or,
when it was made for another image, the segment's source forms evaluated."
  (destructuring-bind (id offset length consts source-offset source-length) (cdr entry)
    (declare (ignore source-length))
    (if (and *native-run-module-hook* (equal id *image-id*))
        (funcall *native-run-module-hook*
                 (car *fasl-modules*) (+ (cdr *fasl-modules*) offset) length consts)
        (progn (setq *fasl-unit-runner* :source)
               (%eval-toplevel (%fasl-entry-source stream source-offset))))))

(defun %load-fasl-segment (entry stream)
  "Another segment of the module of a compiled file."
  (destructuring-bind (k consts source-offset source-length) (cdr entry)
    (declare (ignore source-length))
    (if (functionp *fasl-unit-runner*)
        (funcall *fasl-unit-runner* k consts)
        (%eval-toplevel (%fasl-entry-source stream source-offset)))))

(defun %skip-fasl-modules (stream)
  "After the header line: skip the module octets; returns (native-name . start)."
  (let* ((pos (file-position stream))
         (line (read-line stream nil "")))
    (if (and (> (length line) 9) (string= line ";;; wasm " :end1 9))
        (let ((n (parse-integer line :start 9)) (start (file-position stream)))
          (file-position stream (+ start n))
          (cons (namestring (truename stream)) start))
        (progn (file-position stream pos) nil))))

;;; Where definitions are, for development environments (SWANK's M-.): of
;;; the toplevel forms of a file loaded as source or compiled by
;;; COMPILE-FILE, those whose operator's name begins with DEF are recorded on
;;; the property list of the name defined as (DSPEC FILE POSITION), where
;;; DSPEC is (OPERATOR NAME) or, for DEFMETHOD, (DEFMETHOD NAME QUALIFIER...
;;; (SPECIALIZER...)), and POSITION the octet where the form begins.  A
;;; compiled file has (%NOTE-SOURCE DSPEC FILE POSITION) entries for them.

(defun %definition-spec (form)
  (when (and (consp form) (symbolp (car form)) (consp (cdr form)))
    (let ((op (car form)) (name (second form)))
      (when (and (> (length (symbol-name op)) 3) (string= "DEF" (symbol-name op) :end2 3)
                 (or (and (symbolp name) name (not (keywordp name)))
                     (and (consp name) (eq (car name) 'setf) (consp (cdr name))
                          (symbolp (cadr name)) (null (cddr name)))))
        (if (eq op 'defmethod)
            (let ((qualifiers nil) (rest (cddr form)))
              (loop (when (or (null rest) (listp (car rest))) (return))
                    (push (pop rest) qualifiers))
              (list* op name (append (nreverse qualifiers)
                                     (list (let ((specs nil))
                                             (dolist (p (car rest) (nreverse specs))
                                               (when (member p lambda-list-keywords) (return (nreverse specs)))
                                               (push (if (consp p) (cadr p) t) specs)))))))
            (list op name))))))

(defun %note-source (dspec file position)
  (let ((sym (ignore-errors (%function-name-symbol (second dspec)))))
    (when sym
      (%put sym '%sources
            (cons (list dspec file position)
                  (remove dspec (get sym '%sources) :key #'car :test #'equal))))
    nil))

(defun %install-definitions (definitions)
  "Definitions of the image's own functions and macros, ((NAME ARGLIST-OR-NIL
DSPEC FILE POSITION) ...); an argument list is a list of the lambda list."
  (dolist (d definitions)
    (destructuring-bind (name arglist dspec file position) d
      (when arglist (%put (%function-name-symbol name) '%arglist (car arglist)))
      (when file (%note-source dspec file position)))))

(defun %definition-sources (name)
  "The places where NAME is defined: ((DSPEC FILE POSITION) ...)."
  (let ((sym (ignore-errors (%function-name-symbol name))))
    (and sym (get sym '%sources))))

(defun %form-start (stream)
  "Skip the whitespace and line comments before the next form of STREAM, a
file stream; its position."
  (loop
    (let ((c (peek-char nil stream nil nil)))
      (cond ((null c) (return))
            ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (read-char stream))
            ((char= c #\;) (read-line stream nil nil))
            (t (return)))))
  (file-position stream))

(defun %fasl-text-stream (stream)
  "The rest of the compiled file STREAM (the text of its forms) as a string
stream: the reader reads strings much faster than files (it reads the
symbols of a string straight from it).  The host decodes the text when it
can (%FASL-TEXT-FROM-HOST), which is faster still."
  (let* ((pos (file-position stream))
         (length (max 0 (- (file-length stream) pos)))
         (text (and *fasl-modules* (%fasl-text-from-host (car *fasl-modules*) pos length))))
    (if text
        (make-string-input-stream text)
        (let* ((text (make-string length))
               (end (read-sequence text stream)))
          (make-string-input-stream text 0 end)))))

(defun %load-stream (stream print)
  (let* ((fasl (and (typep stream 'file-stream) (%load-file-stream-p stream)))
         (*fasl-modules* (and fasl (%skip-fasl-modules stream)))
         (*fasl-unit-runner* nil)
         ;; (the uninterned symbols of the file, shared by its entries)
         (*fasl-load-gensyms* (and fasl (make-hash-table)))
         (*fasl-load-symbols* (and fasl (list (make-array 256 :initial-element nil))))
         ;; (the source forms of the modules are read from STREAM itself)
         (in (if fasl (%fasl-text-stream stream) stream))
         (file (and (not fasl) *load-truename* (typep stream 'file-stream)
                    (namestring *load-truename*))))
    (%load-forms stream in fasl file print)))

(defun %load-fasl-from-host (native head print)
  "Load the compiled file NATIVE (a native file name) whose octets the host
reads (HEAD: see %FASL-HEAD-FROM-HOST), without a file stream, which reads
the whole file (most of it the modules: 4.5 MB for ASDF) into Lisp octets;
one is opened only if a source form of the file must be read.  NIL if the
host cannot decode its text."
  (destructuring-bind (start n size) head
    (let ((text (%fasl-text-from-host native (+ start n) (- size start n))))
      (when text
        (let* ((*fasl-modules* (cons native start))
               (*fasl-unit-runner* nil)
               (*fasl-load-gensyms* (make-hash-table))
               (*fasl-load-symbols* (list (make-array 256 :initial-element nil)))
               (stream nil))
          (unwind-protect
               (%load-forms (lambda () (or stream (setq stream (open native :direction :input))))
                            (make-string-input-stream text) t nil print)
            (when stream (close stream)))
          t)))))

(defparameter %load-batch-max 32)

(defvar *native-batch-final-p-hook* nil
  "When the compiler is part of the image: a function telling whether a
toplevel form may end a batch of LOAD (%LOAD-BATCH): one that is compiled
(not one evaluated without the compiler, SIMPLE-FORM).")

(defun %method-definition-p (form)
  (and (consp form) (member (car form) '(defmethod defgeneric)) (consp (cdr form))))

(defun %batch-definition-p (form)
  (and (consp form) (member (car form) '(defun defmethod defgeneric)) (consp (cdr form))))

(defvar *batch-unsafe* nil
  "(operators proclaimers . symbols), made when first needed (%BATCH-UNSAFE):
the operators of a toplevel form after which %LOAD-BATCH reads and compiles
no more forms before it has run, those of the proclamations that the
compiler notes as it compiles the forms after them, and the symbols whose
mention in its code ends the batch as the operators do.  (Names, not
symbols, in the code: a symbol in a constant keeps its function in a program
whose unused functions are left out.)")

(defun %batch-unsafe ()
  (or *batch-unsafe*
      (flet ((syms (names) (mapcar (lambda (n) (find-symbol n "COMMON-LISP")) names)))
        (let ((ops (syms '("DEFMACRO" "DEFINE-COMPILER-MACRO" "DEFSTRUCT" "DEFCLASS" "DEFINE-CONDITION"
                           "DEFTYPE" "DEFCONSTANT" "DEFSETF" "DEFINE-SETF-EXPANDER" "DEFINE-SYMBOL-MACRO"
                           "DEFINE-MODIFY-MACRO" "DEFINE-METHOD-COMBINATION" "DEFPACKAGE" "IN-PACKAGE"
                           "MACROLET" "SYMBOL-MACROLET"))))
          (setq *batch-unsafe*
                (list* ops (syms '("DECLAIM" "DEFVAR" "DEFPARAMETER"))
                      (append ops
                              (syms '("SET-MACRO-CHARACTER" "SET-DISPATCH-MACRO-CHARACTER"
                                      "MAKE-DISPATCH-MACRO-CHARACTER" "SET-SYNTAX-FROM-CHAR" "READTABLE-CASE"
                                      "COPY-READTABLE" "*READTABLE*" "*PACKAGE*" "*READ-BASE*"
                                      "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*" "*READ-SUPPRESS*" "*FEATURES*"
                                      "EVAL" "COMPILE" "LOAD" "REQUIRE" "PROCLAIM" "DECLAIM" "EVAL-WHEN"
                                      "MACRO-FUNCTION" "COMPILER-MACRO-FUNCTION" "MAKE-PACKAGE" "USE-PACKAGE"
                                      "UNUSE-PACKAGE" "EXPORT" "UNEXPORT" "IMPORT" "SHADOW" "SHADOWING-IMPORT"
                                      "UNINTERN" "DELETE-PACKAGE" "RENAME-PACKAGE" "COMPILE-FILE")))))))))

(defun %batch-mentions-unsafe-p (tree budget)
  "True when TREE mentions a symbol of the last of %BATCH-UNSAFE, or is larger
than BUDGET conses (a list: the budget left in its car)."
  (cond ((symbolp tree) (and tree (member tree (cddr (%batch-unsafe))) t))
        ((consp tree)
         (loop
           (when (<= (setf (car budget) (- (car budget) 1)) 0) (return t))
           (when (%batch-mentions-unsafe-p (car tree) budget) (return t))
           (setq tree (cdr tree))
           (unless (consp tree) (return (%batch-mentions-unsafe-p tree budget)))))
        (t nil)))

(defun %batch-passable-p (form &optional (depth 0))
  "True when %LOAD-BATCH may read and compile the forms after the toplevel
FORM (one that is compiled) before FORM has run: FORM, as the compiler
processes it at top level, defines no macro, type, structure, class or
package and its code does not mention what changes the reader or the
compiler's environment (what it changes at run time anyway is seen by
%BATCH-CHECK, after FORM has run)."
  (cond ((> depth 30) nil)
        ((atom form) t)
        ((not (symbolp (car form))) (not (%batch-mentions-unsafe-p form (list 2000))))
        ((member (car form) (car (%batch-unsafe))) nil)
        ((eq (car form) 'progn)
         (every (lambda (f) (%batch-passable-p f (+ depth 1))) (cdr form)))
        ((eq (car form) 'eval-when)
         (and (consp (cdr form)) (listp (second form))
              (notany (lambda (s) (and (symbolp s) (member (symbol-name s) '("COMPILE-TOPLEVEL" "COMPILE") :test #'string=)))
                      (second form))
              (every (lambda (f) (%batch-passable-p f (+ depth 1))) (cddr form))))
        ((member (car form) (cadr (%batch-unsafe)))
         ;; (proclamations the compiler notes as it compiles the forms after it)
         (not (%batch-mentions-unsafe-p (cddr form) (list 2000))))
        ((%batch-definition-p form)
         (not (%batch-mentions-unsafe-p (cddr form) (list 2000))))
        ((or (special-operator-p (car form)) (not (macro-function (car form))))
         (not (%batch-mentions-unsafe-p form (list 2000))))
        (t (multiple-value-bind (expansion ok)
               (ignore-errors (values (macroexpand-1 form) t))
             (and ok (%batch-passable-p expansion (+ depth 1)))))))

(defun %batch-snapshot ()
  (list *env-epoch* *package* *readtable* *read-base* *read-default-float-format*
        *read-eval* *read-suppress* *features*))

(defvar *batch-snapshot* nil)

(defun %batch-begin ()
  (setq *batch-snapshot* (%batch-snapshot))
  nil)

(defun %batch-check (k)
  "In a batch of LOAD, after a form that was compiled with the forms after
it before it ran: if it changed what reading or compiling them depends on,
the rest of the batch does not run (%LOAD-BATCH reads them again)."
  (unless (every #'eq *batch-snapshot* (%batch-snapshot))
    (throw '%batch-stop k)))

(defun %batch-run (forms positions)
  "Evaluate the FORMS of a batch: NIL, or the position of the file where
reading must go on when a %BATCH-CHECK stopped them."
  (when (> (length positions) 0)
    ;; (what compiling the forms changes, the compiler saw)
    (push '(%batch-begin) forms))
  (let* ((*batch-snapshot* nil)
         (k (catch '%batch-stop
              (unless (and (cdr forms) (funcall *native-eval-forms-hook* forms))
                (dolist (f forms) (%eval-toplevel f)))
              nil)))
    (and k (aref positions k))))

(defun %load-batch (stream file form)
  "Evaluate the DEFUN, DEFMETHOD or DEFGENERIC FORM, read from the source file
STREAM, with the definitions of functions, methods and generic functions
that follow it (and the forms after them that are compiled: calls of them,
with more definitions after them while %BATCH-PASSABLE-P lets those forms
pass), in one module.  The engine inlines calls within a module, never into
another, and the compiler makes the calls between the functions of a module
direct, with the types of their arguments and values: a call of a function
defined by another form of the batch is guarded by a comparison of the
function of its name with that definition, so that a redefinition (by a
later form, at the REPL, by TRACE) is seen as when the forms are compiled one
by one (*BATCH-FORMS*, src/frontend.lisp).  The forms are evaluated in their
order; reading a form before the definitions before it have run makes a
difference only to a #. calling them (the forms are then read again after
the definitions, when STREAM is a file: a reader error makes the batch run
first).  Returns the form read after the batch, the position where it
starts and whether there is one (NIL when the batch took the last form it
read, or the stream was positioned to read it again)."
  (let ((forms (list form)) (next nil) (start nil) (pending t)
        ;; (after a compiled form that does not end the batch: the position
        ;; after it, and those of the %BATCH-CHECKs before)
        (after nil) (positions (make-array 0 :adjustable t :fill-pointer 0)) (count 1))
    (loop
      (setq start (and file (%form-start stream)))
      (let ((failed nil) (ahead t))
        (catch '%batch-read-ahead
          (handler-case (let ((*batch-read-ahead* (or after (> (length positions) 0))))
                          (setq next (read stream nil %eof-marker)))
            (error (c) (setq failed c)))
          (setq ahead nil))
        (when (or failed ahead)
          ;; (the definitions before it run first; the form is read again
          ;; after them, or the error is signaled again)
          (let ((resume (%batch-run (nreverse forms) positions)))
            (cond (resume (file-position stream resume))
                  ((not start) (error failed))
                  (t (file-position stream start))))
          (return-from %load-batch (values nil nil nil))))
      (cond ((eq next %eof-marker) (return))
            ((>= count %load-batch-max) (return))
            ((%batch-definition-p next))
            ((and *native-batch-final-p-hook* (funcall *native-batch-final-p-hook* next))
             (unless (and file (%batch-passable-p next))
               (setq pending nil)))
            (t (return)))
      (when after
        ;; (the forms after a compiled one run only if it changed nothing
        ;; that reading or compiling them depended on)
        (push `(%batch-check ,(vector-push-extend after positions)) forms)
        (setq after nil))
      (when start
        (let ((dspec (%definition-spec next)))
          (when dspec (%note-source dspec file start))))
      (push next forms)
      (setq count (+ count 1))
      (unless pending (return))
      (unless (%batch-definition-p next)
        (setq after (file-position stream))))
    (when (and after pending (not (eq next %eof-marker)))
      ;; (NEXT, read after a compiled form, before it ran)
      (push `(%batch-check ,(vector-push-extend after positions)) forms))
    (let ((resume (%batch-run (nreverse forms) positions)))
      (if resume
          (progn (file-position stream resume) (values nil nil nil))
          (values next start pending)))))

(defun %load-forms (stream in fasl file print)
  "Evaluate the forms read from IN, those of STREAM: a compiled file when FASL
(STREAM is then the file, or a function returning it), or a source file (of
native name FILE, if any)."
  (let ((pending nil) (pending-start nil))
    (loop
      (let* ((start (if pending pending-start (and file (%form-start stream))))
             (form (cond (pending (prog1 (car pending) (setq pending nil)))
                         (fasl (%fasl-read in))
                         (t (read stream nil %eof-marker)))))
        (when (eq form %eof-marker) (return t))
        (when start
          (let ((dspec (%definition-spec form)))
            (when dspec (%note-source dspec file start))))
        (if (and (not fasl) (not print) *native-eval-forms-hook* (%batch-definition-p form)
                 ;; (DEFUNs from a file, whose forms can be read again)
                 (or file (%method-definition-p form)))
            ;; (consecutive definitions in one module)
            (multiple-value-bind (next next-start pendingp) (%load-batch stream file form)
              (when pendingp (setq pending (list next) pending-start next-start)))
            (let ((vals (multiple-value-list
                         (cond ((and fasl *fasl-modules* (consp form) (eq (car form) '%fasl-wasm))
                                (%load-fasl-entry form stream))
                               ((and fasl *fasl-modules* (consp form) (eq (car form) '%fasl-unit))
                                (%load-fasl-unit form stream))
                               ((and fasl *fasl-modules* (consp form) (eq (car form) '%fasl-segment))
                                (%load-fasl-segment form stream))
                               ((and fasl (consp form) (eq (car form) '%note-source))
                                (apply #'%note-source (cdr form)))
                               (t (%eval-toplevel form))))))
              (when print (format t "~&~{~S~^ ~}~%" vals))))))))

(defun load (filespec &key (verbose *load-verbose*) (print *load-print*)
                           (if-does-not-exist t) external-format)
  (declare (ignore external-format))
  ;; (what the file defines: %LOAD-BATCH)
  (%note-env-change)
  (let* ((path (%load-pathname-of filespec))
         (file (and (not (streamp filespec))
                    (if (and (null (pathname-type path)) (not (probe-file path)))
                        (or (probe-file (make-pathname :type "lisp" :defaults path))
                            (probe-file (make-pathname :type "fasl" :defaults path))
                            path)
                        path)))
         ;; a compiled file is read by the host
         (truename (and file (probe-file file)))
         (native (and truename (%native-name truename)))
         (head (and native (%fasl-head-from-host native))))
    (when head
      (let ((*package* *package*) (*readtable* *readtable*)
            (*load-pathname* path)
            (*load-truename* truename))
        (when verbose (format t "~&; loading ~S~%" (or path filespec)))
        (when (%load-fasl-from-host native head print)
          (return-from load t))))
    (%load-1 filespec path file verbose print if-does-not-exist)))

(defun %load-1 (filespec path file verbose print if-does-not-exist)
  (let ((stream (if (streamp filespec) filespec
                    (open file :direction :input :if-does-not-exist (if if-does-not-exist :error nil)))))
    (if (null stream)
        nil
        (let ((*package* *package*) (*readtable* *readtable*)
              (*load-pathname* path)
              (*load-truename* (and path (truename stream))))
          (when verbose (format t "~&; loading ~S~%" (or path filespec)))
          (unwind-protect (%load-stream stream print)
            (unless (streamp filespec) (close stream)))))))

(defparameter %compile-time-definers
  '(defmacro in-package defpackage declaim deftype defstruct defclass define-condition
    defsetf define-setf-expander define-modify-macro define-symbol-macro
    define-compiler-macro define-method-combination))

;;; (not DEFGENERIC and DEFMETHOD: the first of a generic function may expand
;;; to a PROGN with the DEFINE-COMPILER-MACRO that gives its call sites caches,
;;; for the rest of the file too)
(defparameter %load-time-definers
  '(defun defvar defparameter defconstant))

(defvar *cf-externalizer* nil
  "During COMPILE-FILE, a function returning the forms that replace a form
whose literal objects are externalized with MAKE-LOAD-FORM.")

;;; A compiled file is
;;;   ;;; Clysm compiled file
;;;   ;;; wasm N
;;;   <N octets: the compiled modules, one after the other>
;;;   <the forms, as text>
;;; where a form (%FASL-WASM image-id offset length constants form) stands for
;;; a module (offset relative to the module octets) to be run with the
;;; constants vector; when the image is not the one it was compiled for, the
;;; source form is compiled again.

(defvar *cf-modules* nil "During COMPILE-FILE: (modules-reversed total-length).")

(defun %cf-compiled-form (form)
  "FORM as a fasl entry for its compiled module, or NIL when it cannot be
compiled or its constants cannot be written readably."
  (when (and *native-compile-form-hook* *image-id* *cf-modules*)
    (multiple-value-bind (octets consts)
        (handler-case (funcall *native-compile-form-hook* form)
          (error () nil))
      (when (and octets
                 ;; the constants must read back as equivalent objects
                 (%call-with-fasl-syntax
                  (lambda () (handler-case (progn (prin1-to-string consts) t)
                               (error () nil)))))
        (let* ((offset (second *cf-modules*))
               (source (%string-utf8-octets
                        (%call-with-fasl-syntax
                         (lambda () (let ((*print-readably* nil)) (prin1-to-string form))))))
               (source-offset (+ offset (length octets))))
          (push octets (first *cf-modules*))
          (push source (first *cf-modules*))
          (setf (second *cf-modules*) (+ source-offset (length source)))
          (list '%fasl-wasm *image-id* offset (length octets) consts
                source-offset (length source)))))))

(defun %string-utf8-octets (string)
  (let ((v (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (dotimes (i (length string) v)
      (let ((code (char-code (char string i))))
        (cond ((< code #x80) (vector-push-extend code v))
              ((< code #x800)
               (vector-push-extend (logior #xC0 (ash code -6)) v)
               (vector-push-extend (logior #x80 (logand code #x3F)) v))
              ((< code #x10000)
               (vector-push-extend (logior #xE0 (ash code -12)) v)
               (vector-push-extend (logior #x80 (logand (ash code -6) #x3F)) v)
               (vector-push-extend (logior #x80 (logand code #x3F)) v))
              (t
               (vector-push-extend (logior #xF0 (ash code -18)) v)
               (vector-push-extend (logior #x80 (logand (ash code -12) #x3F)) v)
               (vector-push-extend (logior #x80 (logand (ash code -6) #x3F)) v)
               (vector-push-extend (logior #x80 (logand code #x3F)) v)))))))

(defun %write-fasl (path text modules total)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line %fasl-header out)
    (format out ";;; wasm ~D~%" total)
    (dolist (m modules)
      (%fs-put-octets out m))
    (write-string text out)))

(defvar *cf-pending* nil
  "During COMPILE-FILE: ordinary forms (reversed) waiting to be compiled
together into one module.  Forms are only batched between compile-time side
effects, so compiling them later sees the same environment.")
(defparameter %cf-batch-size 64)

;;; A fasl entry in a form of its own (%FASL-BIN-ENCODE), which LOAD decodes
;;; without the reader (and without binding the printer's and reader's
;;; variables for each entry): it is written as
;;;   !N;<N characters: the entry>
;;; The reader spent most of the time of reading a compiled file on its
;;; symbols (taking the token, upcasing it, finding the package and the
;;; symbol), every time an entry had one.  An object is
;;;   s<k>;<p>;<package><n>;<name>  the symbol K of the file (P -1: a keyword),
;;;                             the first time the entry has it
;;;   S<k>;                     the symbol K of the file again
;;;   g<k>;<n>;<name>           the uninterned symbol K of the file (%FASL-GENSYM)
;;;   I<integer>;               a fixnum
;;;   "<n>;<characters>         a simple string
;;;   C<character>
;;;   (<n>;<objects>) or (<n>;<objects>.<object>   a list (dotted)
;;;   #<n>;<objects>            a simple vector
;;;   R<n>;<text>               another number, or a pathname: read
;;;   =<k>;<object>             the object K, written once: ^<k>; refers to it
;;; Objects of other kinds, characters beyond #xFFFF, and cycles leave the
;;; constants to the reader.

(defun %fasl-bin-encode (vector)
  "The constants VECTOR as the text of %FASL-BIN-DECODE, or NIL."
  (let ((seen (make-hash-table :test 'eq)) (active (make-hash-table :test 'eq)))
    ;; the objects met more than once (labelled), and whether all can be written
    (labels ((ok-char (c) (let ((n (char-code c))) (not (or (> n #xFFFF) (<= #xD800 n #xDFFF)))))
             (ok-string (x) (every #'ok-char x))
             (walk (x)
               (cond ((or (null x) (fixnump x)) t)
                     ((characterp x) (ok-char x))
                     ((symbolp x) (and (ok-string (symbol-name x))
                                       (or (symbol-package x) *fasl-gensyms*)
                                       (or (null (symbol-package x)) (ok-string (package-name (symbol-package x))))))
                     ((or (consp x) (simple-vector-p x) (simple-string-p x))
                      (cond ((gethash x active) nil)      ; a cycle
                            ((gethash x seen) (setf (gethash x seen) :shared) t)
                            (t (setf (gethash x seen) t)
                               (cond ((simple-string-p x) (ok-string x))
                                     ((simple-vector-p x)
                                      (setf (gethash x active) t)
                                      (prog1 (every #'walk x) (remhash x active)))
                                     (t (let ((cells nil) (r t))
                                          (do ((l x (cdr l))) ((not (consp l)) (setq r (and r (walk l))))
                                            (unless (eq l x)
                                              (cond ((gethash l active) (setq r nil) (return))
                                                    ((gethash l seen) (setf (gethash l seen) :shared) (return))
                                                    (t (setf (gethash l seen) t))))
                                            (setf (gethash l active) t) (push l cells)
                                            (unless (walk (car l)) (setq r nil) (return)))
                                          (dolist (c cells) (remhash c active))
                                          r))))))
                     ((or (numberp x) (pathnamep x)) t)
                     (t nil))))
      (unless (and *fasl-symbol-numbers* (walk vector)) (return-from %fasl-bin-encode nil)))
    (let ((labels (make-hash-table :test 'eq)) (nlabels 0)
          (symbols (make-hash-table :test 'eq)))
      (with-output-to-string (out)
        (labels ((int (n) (format out "~D;" n))
                 (str (s) (int (length s)) (write-string s out))
                 (emit (x)
                   (if (or (consp x) (simple-vector-p x) (simple-string-p x))
                       (let ((k (gethash x labels)))
                         (cond (k (write-char #\^ out) (int k))
                               (t (when (eq (gethash x seen) :shared)
                                    (setf (gethash x labels) nlabels)
                                    (write-char #\= out) (int nlabels) (incf nlabels))
                                  (body x))))
                       (body x)))
                 (body (x)
                   (cond ((symbolp x)
                          (let ((k (gethash x symbols)) (pkg (symbol-package x)))
                            (cond (k (write-char #\S out) (int k))
                                  ((null pkg)
                                   (write-char #\g out)
                                   (int (or (gethash x *fasl-gensyms*)
                                            (setf (gethash x *fasl-gensyms*) (hash-table-count *fasl-gensyms*))))
                                   (str (symbol-name x)))
                                  (t (setf (gethash x symbols)
                                           (or (gethash x *fasl-symbol-numbers*)
                                               (setf (gethash x *fasl-symbol-numbers*)
                                                     (hash-table-count *fasl-symbol-numbers*))))
                                     (write-char #\s out) (int (gethash x symbols))
                                     (if (eq pkg *keyword-package*) (int -1) (str (package-name pkg)))
                                     (str (symbol-name x))))))
                         ((fixnump x) (write-char #\I out) (int x))
                         ((characterp x) (write-char #\C out) (write-char x out))
                         ((simple-string-p x) (write-char #\" out) (str x))
                         ((simple-vector-p x)
                          (write-char #\# out) (int (length x))
                          (dotimes (i (length x)) (emit (svref x i))))
                         ((consp x)
                          ;; (the cells up to one that is shared: that one is the tail)
                          (let ((n 0) (l x))
                            (loop (incf n) (setq l (cdr l))
                                  (when (or (not (consp l)) (eq (gethash l seen) :shared)) (return)))
                            (write-char #\( out) (int n)
                            (setq l x)
                            (dotimes (i n) (emit (car l)) (setq l (cdr l)))
                            (if (null l) (write-char #\) out) (progn (write-char #\. out) (emit l)))))
                         (t (write-char #\R out)
                            (str (%call-with-fasl-syntax (lambda () (prin1-to-string x))))))))
          (emit vector))))))

(defun %fasl-grow (v k)
  "A copy of the simple vector V with room for the index K."
  (let ((new (make-array (max (* 2 (length v)) (1+ k)) :initial-element nil)))
    (replace new v)
    new))

(defun %fasl-bin-decode (string start)
  "The object written by %FASL-BIN-ENCODE in the simple string STRING from START."
  (let* ((pos start) (labels (make-array 8 :initial-element nil))
         (cache (or *fasl-load-symbols* (list (make-array 16 :initial-element nil))))
         (symbols (car cache))
         (packages nil))
    (declare (fixnum pos) (simple-string string) (simple-vector labels symbols))
    (labels ((next () (prog1 (schar string pos) (incf pos)))
             (int ()
               (let ((n 0) (sign 1))
                 (declare (fixnum n sign))
                 (when (char= (schar string pos) #\-) (setq sign -1) (incf pos))
                 (loop (let ((c (next)))
                         (when (char= c #\;) (return (* sign n)))
                         (setq n (+ (* n 10) (- (char-code c) 48)))))))
             (str ()
               (let* ((n (int)) (s (make-string n)))
                 (declare (fixnum n))
                 (replace s string :start2 pos :end2 (+ pos n))
                 (incf pos n)
                 s))
             (package (n)
               ;; (the package of the N characters at POS: found once in an entry)
               (let ((e (assoc-if (lambda (name) (and (= (length name) n)
                                                      (string= name string :start2 pos :end2 (+ pos n))))
                                  packages)))
                 (incf pos n)
                 (if e (cdr e)
                     (let* ((name (subseq string (- pos n) pos))
                            (p (or (find-package name)
                                   (error "package ~A not found (in a compiled file)" name))))
                       (push (cons name p) packages)
                       p))))
             (obj ()
               (let ((c (next)))
                 (case c
                   (#\S (svref symbols (int)))
                   (#\s (let* ((k (int)) (n (int))
                               (p (if (< n 0) *keyword-package* (package n)))
                               (old (and (< k (length symbols)) (svref symbols k))))
                          (if (and old (eq (symbol-package old) p))
                              (let ((n (int))) (incf pos n) old)
                              (let ((sym (values (intern (str) p))))
                                (unless (< k (length symbols)) (setq symbols (%fasl-grow symbols k)))
                                (setf (car cache) symbols)
                                (setf (svref symbols k) sym)))))
                   (#\g (let ((k (int))) (%fasl-gensym k (str))))
                   (#\I (int))
                   (#\" (str))
                   (#\C (next))
                   (#\# (let* ((n (int)) (v (make-array n)))
                          (dotimes (i n v) (setf (svref v i) (obj)))))
                   (#\( (let* ((n (int)) (head (cons nil nil)) (tail head))
                          (dotimes (i n)
                            (let ((cell (cons (obj) nil))) (setf (cdr tail) cell tail cell)))
                          (when (char= (next) #\.) (setf (cdr tail) (obj)))
                          (cdr head)))
                   (#\R (let ((text (str)))
                          (%call-with-fasl-syntax (lambda () (values (read-from-string text))))))
                   (#\= (let ((k (int)) (x (obj)))
                          (unless (< k (length labels)) (setq labels (%fasl-grow labels k)))
                          (setf (svref labels k) x)))
                   (#\^ (svref labels (int)))
                   (t (error "the constants of a compiled file are damaged at ~D" (1- pos)))))))
      (obj))))

(defun %fasl-consts-place (form)
  "The position of the constants in the fasl entry FORM, or NIL."
  (and (consp form)
       (case (car form) ((%fasl-wasm %fasl-unit) 4) (%fasl-segment 2))))

(defun %cf-write (x out)
  ;; (an entry, when it can, in its own form)
  (let ((bin (and (%fasl-consts-place x) (%fasl-bin-encode x))))
    (when bin
      (write-char #\! out) (format out "~D;" (length bin)) (write-string bin out) (terpri out)
      (return-from %cf-write nil)))
  (%call-with-fasl-syntax
   (lambda ()
     (handler-case (prin1 x out)
       (print-not-readable ()
         (let ((*print-readably* nil)) (prin1 x out))))
     (terpri out))))

(defun %fasl-read (in)
  "The next form of the text IN of a compiled file (see %CF-WRITE), or the
end-of-file marker."
  (let ((c (peek-char t in nil nil)))
    (if (and (eql c #\!) (eq (stream-kind in) :string-in) (null (stream-unread in))
             (simple-string-p (stream-buffer in)))
        (let* ((string (stream-buffer in))
               (start (1+ (stream-index in)))
               (semi (position #\; string :start start))
               (n (parse-integer string :start start :end semi))
               (form (%fasl-bin-decode string (1+ semi))))
          (setf (stream-index in) (+ semi 1 n))
          form)
        (%call-with-fasl-syntax (lambda () (read in nil %eof-marker))))))

(defvar *cf-unit* nil
  "During COMPILE-FILE into one module: the entries of the file, reversed:
(:SEGMENT K FORMS) for a segment of the module, (:SOURCE FORM) for a form
the compiler failed on, (:ENTRY ENTRY) for a form compiled and run at
compile time (%CF-COMPILE-AND-RUN).")
(defvar *cf-unit-forms* nil "During COMPILE-FILE into one module: the forms of the current segment, reversed.")
(defvar *cf-unit-p* nil "True during COMPILE-FILE into one module.")

(defun %cf-unit-flush ()
  "End the current segment of the module of the file."
  (let ((k (%call-with-world-lock (third *native-unit-hooks*)))
        (forms (reverse *cf-unit-forms*)))
    (setq *cf-unit-forms* nil)
    (if k
        (push (list :segment k forms) *cf-unit*)
        (dolist (f forms) (push (list :source f) *cf-unit*)))))

(defun %cf-unit-add (form)
  (if (handler-case (progn (%call-with-world-lock
                            (lambda () (funcall (second *native-unit-hooks*) form)))
                           t)
        (error () nil))
      (push form *cf-unit-forms*)
      ;; the compiler failed on the form: it is compiled when the file is loaded
      (progn (%cf-unit-flush) (push (list :source form) *cf-unit*))))

(defun %cf-write-unit (out)
  "Write the entries of the file compiled into one module; the module and
the source of its segments go to *CF-MODULES*.  When the module cannot be
made, or the objects of a segment cannot be written readably, the source
forms are written instead."
  (%cf-unit-flush)
  (let ((entries (reverse *cf-unit*)))
    (multiple-value-bind (octets vectors)
        (handler-case (%call-with-world-lock (fourth *native-unit-hooks*))
          (error () nil))
      (if (and octets
               (every (lambda (v)
                        (%call-with-fasl-syntax
                         (lambda () (handler-case (progn (prin1-to-string v) t)
                                      (error () nil)))))
                      vectors))
          (let ((offset (second *cf-modules*)))
            (push octets (first *cf-modules*))
            (setf (second *cf-modules*) (+ offset (length octets)))
            (dolist (e entries)
              (case (car e)
                (:entry (write-string (second e) out))
                (:source (%cf-write (second e) out))
                (t
                  (let* ((k (second e))
                         (source (%string-utf8-octets
                                  (%call-with-fasl-syntax
                                   (lambda () (let ((*print-readably* nil))
                                                (prin1-to-string `(progn ,@(third e))))))))
                         (source-offset (second *cf-modules*)))
                    (push source (first *cf-modules*))
                    (setf (second *cf-modules*) (+ source-offset (length source)))
                    (%cf-write (if (= k 0)
                                   (list '%fasl-unit *image-id* offset (length octets) (nth k vectors)
                                         source-offset (length source))
                                   (list '%fasl-segment k (nth k vectors) source-offset (length source)))
                               out))))))
          (dolist (e entries)
            (case (car e)
              (:entry (write-string (second e) out))
              (:source (%cf-write (second e) out))
              (t (dolist (f (third e)) (%cf-write f out)))))))))

(defun %cf-flush (out)
  "Write the pending forms: one module for all of them if possible, else one
per form, else the source forms."
  (when *cf-unit-p* (return-from %cf-flush (%cf-unit-flush)))
  (let ((forms (reverse *cf-pending*)))
    (setq *cf-pending* nil)
    (when forms
      (let ((entry (and (cdr forms) (%cf-compiled-form `(progn ,@forms)))))
        (if entry
            (%cf-write entry out)
            (dolist (f forms)
              (%cf-write (or (%cf-compiled-form f) f) out)))))))

(defun %cf-emit (form out)
  (let ((forms (if *cf-externalizer* (funcall *cf-externalizer* form) (list form))))
    (cond ((not (and *native-compile-form-hook* *image-id* *cf-modules*))
           (dolist (f forms) (%cf-write f out)))
          (*cf-unit-p*
           (if (cdr forms)
               (progn (%cf-flush out)
                      (dolist (f forms) (%cf-unit-add f) (%cf-flush out)))
               (progn (%cf-unit-add (car forms))
                      (when (>= (length *cf-unit-forms*) %cf-batch-size) (%cf-flush out)))))
          ((cdr forms)
           ;; literal objects externalized with MAKE-LOAD-FORM: the forms
           ;; refer to objects that the preceding ones create when loaded (and
           ;; their constants are read only then), so each is a module of its own
           (%cf-flush out)
           (dolist (f forms) (push f *cf-pending*) (%cf-flush out)))
          (t (push (car forms) *cf-pending*)
             (when (>= (length *cf-pending*) %cf-batch-size) (%cf-flush out))))))

(defvar *cf-env* nil "the interpreter environment of local macros around toplevel forms")
(defvar *cf-wrappers* nil "(op bindings declarations...) of the forms around toplevel forms, innermost first")

(defun %cf-toplevel-expansion (form)
  "Expand FORM until it is a form that toplevel processing treats specially.
Returns the expansion, or NIL when FORM is an ordinary form."
  (loop
    (unless (and (consp form) (symbolp (car form))) (return nil))
    (let ((op (car form)))
      (when (and (member op '(progn eval-when macrolet symbol-macrolet locally))
                 (not (%env-fun *cf-env* op)))
        (return form))
      (when (and (member op %compile-time-definers) (not (%env-fun *cf-env* op)))
        (return form))
      (when (eq op 'defconstant) (%cf-note-defconstant form))
      (when (or (member op %load-time-definers) (special-operator-p op))
        (return nil))
      (multiple-value-bind (new changed) (macroexpand-1 form *cf-env*)
        (unless changed (return nil))
        (setq form new)))))

;;; CLHS DEFCONSTANT: the compiler must recognize the name as a constant
;;; variable, and may evaluate the value form at compile time.  A value that
;;; is a number, character, symbol or string and can be computed now defines
;;; the constant now (as SBCL does), so that the rest of the file is compiled
;;; with the value: (FIND C +WHITESPACE+ :TEST #'CHAR=) as comparisons inline,
;;; arithmetic on the value.  Other values (lists, vectors, structures) are not:
;;; a copy in the fasl would not be EQ to the value made when it is loaded.
(defun %cf-note-defconstant (form)
  (when (and (consp (cdr form)) (symbolp (second form)) (second form)
             (not (keywordp (second form))) (consp (cddr form))
             (not (boundp (second form))))
    (let ((sym (second form)))
      (multiple-value-bind (v ok)
          (handler-bind ((warning #'muffle-warning))
            (ignore-errors (values (%cf-eval (%cf-wrap (third form))) t)))
        ;; (OK is the condition when the evaluation failed)
        (when (and (eq ok t) (not (boundp sym))
                   (or (numberp v) (characterp v) (symbolp v) (stringp v)))
          (%defconstant sym v))))))

(defun %cf-wrap (form)
  "FORM inside the MACROLET, SYMBOL-MACROLET and LOCALLY forms around it."
  (dolist (w *cf-wrappers* form)
    (setq form (append w (list form)))))

(defun %cf-local (x out compile-time-too)
  "Toplevel MACROLET, SYMBOL-MACROLET and LOCALLY: the body forms are processed
as toplevel forms (CLHS 3.2.3.1) in the environment they establish."
  (let* ((op (car x))
         (bindings (if (eq op 'locally) nil (cadr x)))
         (body (if (eq op 'locally) (cdr x) (cddr x)))
         (decls (let ((r nil))
                  (loop (if (and (consp (car body)) (eq (caar body) 'declare)) (push (pop body) r) (return)))
                  (nreverse r)))
         (env *cf-env*))
    (case op
      (macrolet
       (dolist (def bindings)
         (push (list* :macro (car def)
                      (%eval-function (%macro-lambda (car def) (cadr def) (cddr def)) *cf-env*))
               env)))
      (symbol-macrolet
       (dolist (b bindings) (push (list* :smacro (car b) (cadr b)) env))))
    (let ((*cf-env* env)
          (*cf-wrappers* (cons (if (eq op 'locally) (cons 'locally decls) (list* op bindings decls))
                               *cf-wrappers*)))
      (dolist (f body) (%cf-process f out compile-time-too)))))

(defun %cf-uninterned-symbol-in (form)
  "Whether FORM has an uninterned symbol in it (in its conses and simple vectors)."
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (x)
               (cond ((symbolp x) (and x (null (symbol-package x))))
                     ((consp x)
                      (unless (gethash x seen)
                        (setf (gethash x seen) t)
                        (or (walk (car x)) (walk (cdr x)))))
                     ((and (simple-vector-p x) (not (gethash x seen)))
                      (setf (gethash x seen) t)
                      (some #'walk x)))))
      (walk form))))

(defvar *cf-unit-compile-time* :interpret
  "How COMPILE-FILE into one module evaluates the forms evaluated at compile
time too (other than DEFUNs, defined to be compiled when first called): with
the interpreter (:INTERPRET), or compiled to a module run at once (:NATIVE);
either way they are compiled into the file's module.  ASDF: :INTERPRET
compiles in 2.6 s, :NATIVE (compiling them twice) in 4.4 s.")

(defun %cf-eval (form)
  "Evaluate FORM at compile time (see *CF-UNIT-COMPILE-TIME*)."
  (if (and *cf-unit-p* (eq *cf-unit-compile-time* :interpret))
      (eval form)
      (%eval-toplevel form)))

(defun %cf-compile-and-run (form out)
  "An ordinary FORM evaluated at compile time too (EVAL-WHEN with
:COMPILE-TOPLEVEL and :LOAD-TOPLEVEL): compiled once, to a module that is run
now and written to the fasl as an entry of its own.  NIL (nothing done) when
it cannot be done so: the form is then evaluated and compiled apart."
  ;; (not in a file compiled into one module: the forms compiled once would
  ;; be modules of their own beside the file's, and loading ASDF from its
  ;; fasl took 0.5 s instead of 0.2 for 2 s saved once when compiling it)
  (when (and (not *cf-unit-p*)
             *native-run-octets-hook* *native-compile-form-hook* *image-id* *cf-modules*
             ;; (no literal objects made by MAKE-LOAD-FORM)
             (or (null *cf-externalizer*) (null (%literal-objects form nil)))
             ;; nor uninterned symbols: an entry of its own would read its
             ;; own copy of one that the forms around it share (Lem's
             ;; DEFINE-MAJOR-MODE names the class of its command with
             ;; MAKE-SYMBOL, in a DEFCLASS and in forms that refer to it)
             (not (%cf-uninterned-symbol-in form)))
    (%cf-flush out)
    (let ((entry (%cf-compiled-form form)))
      (when entry
        ;; (%CF-COMPILED-FORM pushed the module's octets, then its source)
        (let ((octets (second (first *cf-modules*))))
          ;; (in a file compiled into one module, the entries are written
          ;; at the end, in order: this one as its text now, before running
          ;; the module changes its constants, e.g. the caches of call sites)
          (if *cf-unit-p*
              (push (list :entry (with-output-to-string (o) (%cf-write entry o))) *cf-unit*)
              (%cf-write entry out))
          (funcall *native-run-octets-hook* octets (nth 4 entry))
          t)))))

(defun %cf-defun-name (form)
  "The name FORM, a DEFUN, defines, or NIL."
  (and (consp form) (eq (car form) 'defun) (consp (cdr form))
       (let ((name (second form)))
         (and (or (and (symbolp name) name)
                  (and (consp name) (eq (car name) 'setf) (consp (cdr name))
                       (symbolp (second name)) (null (cddr name))))
              name))))

(defun %cf-define-lazily (name form)
  "Define NAME at compile time as a function that evaluates FORM, its DEFUN,
when it is first called, and calls the function it defines."
  (let ((stub nil))
    (setq stub (lambda (&rest args)
                 (when (eq (fdefinition name) stub)
                   (%eval-toplevel form)
                   (when (eq (fdefinition name) stub)
                     (error "~S was not defined by ~S" name form)))
                 (apply (fdefinition name) args)))
    (%set-fdefinition name stub)
    name))

(defvar *cf-constants* nil
  "During COMPILE-FILE: symbol -> value of the constants that the DEFCONSTANT
forms read so far define with a value written as a constant.")

(defun %cf-note-constant (form)
  "A toplevel DEFCONSTANT whose value is written as a constant (a number, a
character or a symbol, or another such constant): the code of the rest of the
file is compiled with the value (CLHS DEFCONSTANT: the compiler recognizes
the name as a constant variable), as the forms of a file compiled by the host."
  (let ((sym (second form)) (vform (third form)))
    (when (and (symbolp sym) sym (not (keywordp sym)) (consp (cddr form)))
      (multiple-value-bind (v ok)
          (cond ((and (consp vform) (eq (car vform) 'quote) (consp (cdr vform))) (values (second vform) t))
                ((consp vform) (values nil nil))
                ((and (symbolp vform) (nth-value 1 (gethash vform *cf-constants*)))
                 (gethash vform *cf-constants*))
                ((symbolp vform)
                 (if (or (null vform) (eq vform t) (keywordp vform)) (values vform t) (values nil nil)))
                (t (values vform t)))
        (when (and ok (or (numberp v) (characterp v) (symbolp v)))
          (setf (gethash sym *cf-constants*) v))))))

(defun %cf-process (form out compile-time-too)
  (when (and *cf-constants* (consp form) (eq (car form) 'defconstant) (not (%env-fun *cf-env* 'defconstant)))
    (%cf-note-constant form))
  (let ((x (%cf-toplevel-expansion form)))
    ;; a compile-time side effect ends the batch of pending forms
    ;; (definers with compile-time effects, EVAL-WHEN, special proclamations;
    ;; other forms evaluated at compile time, like DEFUN in an EVAL-WHEN, do
    ;; not change how the pending forms compile)
    (when (or x (and (consp form) (member (car form) '(defvar defparameter proclaim))))
      (%cf-flush out))
    (cond
      ;; a DEFUN evaluated at compile time too, in a file compiled into one
      ;; module: the function is part of the module (direct calls, copies
      ;; into its callers), and at compile time it is compiled when it is
      ;; first called (most are not: ASDF's WITH-UPGRADABILITY)
      ((and (null x) compile-time-too *cf-unit-p* (%cf-defun-name form))
       (%cf-define-lazily (%cf-defun-name form) (%cf-wrap form))
       (%cf-emit (%cf-wrap form) out))
      ((and (null x) compile-time-too (%cf-compile-and-run (%cf-wrap form) out))
       (when (and (consp form) (member (car form) '(defvar defparameter))
                  (consp (cdr form)) (symbolp (second form)))
         (proclaim `(special ,(second form)))))
      ((null x)
       (when compile-time-too (%cf-eval (%cf-wrap form)))
       ;; CLHS DEFVAR/DEFPARAMETER: the compiler must recognize the variable
       ;; as special in the rest of the file
       (when (and (consp form) (member (car form) '(defvar defparameter))
                  (consp (cdr form)) (symbolp (second form)))
         (proclaim `(special ,(second form))))
       (%cf-emit (%cf-wrap form) out))
      ((member (car x) '(macrolet symbol-macrolet locally))
       (%cf-local x out compile-time-too))
      ((eq (car x) 'progn)
       (dolist (f (cdr x)) (%cf-process f out compile-time-too)))
      ((eq (car x) 'eval-when)
       (let* ((sits (second x))
              (ct (or (member :compile-toplevel sits) (member 'compile sits)))
              (lt (or (member :load-toplevel sits) (member 'load sits)))
              (ex (or (member :execute sits) (member 'eval sits))))
         (cond ((and lt (or ct (and ex compile-time-too)))
                (dolist (f (cddr x)) (%cf-process f out t))
                ;; the forms may change how the following ones are read (by
                ;; SHADOW, IMPORT ...): the constants of a module are read
                ;; together when it is loaded
                (%cf-flush out))
               (lt (dolist (f (cddr x)) (%cf-process f out nil)))
               ((or ct (and ex compile-time-too))
                (%eval-toplevel (%cf-wrap `(progn ,@(cddr x))))))))
      ;; in a file compiled into one module, the functions of a DEFSTRUCT
      ;; (constructors, accessors...) are part of the module like other
      ;; DEFUNs evaluated at compile time (its other forms still are)
      ((and *cf-unit-p* (eq (car x) 'defstruct)
            (let ((e (macroexpand-1 x *cf-env*)))
              (and (consp e) (eq (car e) 'progn) (some #'%cf-defun-name (cdr e)))))
       ;; (the other forms are compiled and run at once together)
       (let* ((e (macroexpand-1 x *cf-env*))
              (others (remove-if #'%cf-defun-name (cdr e))))
         (unless (%cf-compile-and-run (%cf-wrap `(progn ,@others)) out)
           (%cf-eval (%cf-wrap `(progn ,@others)))
           (%cf-emit (%cf-wrap `(progn ,@others)) out))
         (dolist (f (cdr e))
           (when (%cf-defun-name f)
             (%cf-define-lazily (%cf-defun-name f) (%cf-wrap f))
             (%cf-emit (%cf-wrap f) out)))))
      (t (unless (%cf-compile-and-run (%cf-wrap x) out)
           (%cf-eval (%cf-wrap x))
           (%cf-emit (%cf-wrap x) out))
         ;; the symbols of the package are read after it is made
         (when (eq (car x) 'defpackage) (%cf-flush out))))))

(defun compile-file-pathname (input-file &key output-file &allow-other-keys)
  (let* ((in (if (typep input-file 'file-stream) (pathname input-file) (pathname input-file)))
         (default (make-pathname :type "fasl" :version nil
                                 :defaults (translate-logical-pathname (merge-pathnames in)))))
    (if output-file
        (let ((o (if (streamp output-file) (pathname output-file) (pathname output-file))))
          (if (typep o 'logical-pathname)
              (translate-logical-pathname (merge-pathnames o))
              (merge-pathnames o default)))
        default)))

(defun compile-file (input-file &key output-file (verbose *compile-verbose*) (print *compile-print*)
                                     external-format)
  (declare (ignore external-format))
  (let* ((in-path (merge-pathnames (pathname input-file)))
         (out-path (compile-file-pathname input-file :output-file output-file))
         (warnings-p nil) (failure-p nil))
    (with-open-file (in in-path :direction :input)
      (let ((*compile-file-pathname* in-path)
            (*compile-file-truename* (truename in))
            (*package* *package*) (*readtable* *readtable*)
            ;; a DECLAIM of OPTIMIZE in the file holds to its end (as in SBCL)
            (*proclaimed-safety* *proclaimed-safety*)
            (*cf-externalizer* (%new-externalizer in-path)))
        (when verbose (format t "~&; compiling file ~A~%" (namestring *compile-file-truename*)))
        (handler-bind ((warning (lambda (c)
                                  (setq warnings-p t)
                                  (unless (typep c 'style-warning) (setq failure-p t))))
                       (error (lambda (c) (declare (ignore c)) (setq warnings-p t failure-p t))))
          (let* ((*cf-modules* (list nil 0))
                 (*cf-constants* (make-hash-table :test 'eq))
                 (*cf-pending* nil)
                 (*cf-unit* nil) (*cf-unit-forms* nil)
                 (*fasl-gensyms* (make-hash-table :test 'eq))
                 (*fasl-symbol-numbers* (make-hash-table :test 'eq))
                 (*cf-unit-p* (and *native-unit-hooks* *image-id* t))
                 (notes nil)
                 (text (with-output-to-string (out)
                         (flet ((body ()
                                  (loop
                                    (let* ((start (%form-start in))
                                           (form (read in nil %eof-marker)))
                                      (when (eq form %eof-marker) (return))
                                      (when print (format t "~&; ~S~%" (if (consp form) (car form) form)))
                                      (let ((dspec (%definition-spec form)))
                                        (when dspec
                                          (push (list '%note-source dspec (namestring *compile-file-truename*) start)
                                                notes)))
                                      (%cf-process form out nil)))
                                  (if *cf-unit-p*
                                      (%cf-write-unit out)
                                      (%cf-flush out))))
                           (if *cf-unit-p*
                               (funcall (first *native-unit-hooks*) #'body)
                               (body)))
                         ;; at the end, where the packages of the names exist
                         (dolist (note (nreverse notes))
                           (when (%call-with-fasl-syntax
                                  (lambda () (ignore-errors (prin1-to-string note))))
                             (%cf-write note out))))))
            (%write-fasl out-path text (reverse (first *cf-modules*)) (second *cf-modules*))))
        (when verbose (format t "~&; wrote ~A~%" (namestring out-path)))))
    (values (truename out-path) warnings-p failure-p)))

;;; A system's files compiled into one fasl of one module (contrib/asdf-hooks.lisp),
;;; in the course of ASDF's compiling and loading each file in turn, so that
;;; each file is compiled, and its compile-time side effects happen, once:
;;; %SYSTEM-COMPILE-FILE compiles the next file into the unit of the system
;;; (src/live.lisp, UNIT-OPEN), processing its forms as COMPILE-FILE does
;;; (with the current *PACKAGE*, *READTABLE* and policy), the module being cut
;;; into a segment at the start of each file; %SYSTEM-LOAD-FILE loads it by
;;; running modules made for its segments alone (UNIT-RUNNING-MODULE), their
;;; objects written and read as in a fasl; %SYSTEM-UNIT-FINISH writes the
;;; unit's module, loading which is loading the files in order.  A call from
;;; one file to a function another file defines is a guarded direct call (a
;;; later redefinition is still seen); calls within a file are those of
;;; COMPILE-FILE.

(defvar *system-units* 0)

(defstruct (%system-unit (:constructor %make-system-unit (state prefix)) (:conc-name %su-))
  state                                 ; the compiler's (UNIT-OPEN)
  prefix                                ; of the exports of its modules
  (modules (list nil 0))                ; *CF-MODULES* of the fasl
  (entries nil)                         ; *CF-UNIT* of the fasl
  (gensyms (make-hash-table :test 'eq)) ; *FASL-GENSYMS* of the fasl
  (load-gensyms (make-hash-table))      ; *FASL-LOAD-GENSYMS* of the loads
  (notes nil)                           ; %NOTE-SOURCE entries, reversed
  (broken nil)                          ; true: no fasl can be made of it
  (files nil))                          ; what loads each file, reversed

(defun %system-unit-open ()
  "A unit for the files of a system, or NIL when the image has no compiler."
  (when (and (nth 7 *native-unit-hooks*) *image-id*)
    (let ((prefix (format nil "u:~D:" (setq *system-units* (+ *system-units* 1)))))
      (%make-system-unit (%call-with-world-lock (lambda () (funcall (nth 5 *native-unit-hooks*) prefix)))
                         prefix))))

(defun %su-text (su x)
  "X written as in the fasl of SU, and whether it is written readably."
  (let ((*fasl-gensyms* (%su-gensyms su)) (*fasl-symbol-numbers* (make-hash-table :test 'eq)))
    (let ((bin (and (%fasl-consts-place x) (%fasl-bin-encode x))))
      (if bin
          (values (format nil "!~D;~A~%" (length bin) bin) t)
          (values (with-output-to-string (out) (%cf-write x out))
                  (or (not (%fasl-consts-place x))
                      (%call-with-fasl-syntax
                       (lambda () (handler-case (progn (prin1-to-string x) t) (error () nil))))))))))

(defun %forms-have-fasl-ref-p (forms)
  "Whether FORMS have an object made by a MAKE-LOAD-FORM in them."
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (x)
               (cond ((consp x)
                      (unless (gethash x seen)
                        (setf (gethash x seen) t)
                        (or (walk (car x)) (walk (cdr x)))))
                     ((simple-vector-p x)
                      (unless (gethash x seen)
                        (setf (gethash x seen) t)
                        (some #'walk x)))
                     (t (%fasl-marker-p x)))))
      (walk forms))))

(defun %system-compile-file (su input &key verbose)
  "Compile the file INPUT as the next file of SU.  Returns (values
warnings-p failure-p), as COMPILE-FILE."
  (let* ((in-path (merge-pathnames (pathname input)))
         (k (length (%su-files su))) (warnings-p nil) (failure-p nil) (notes nil) (loads nil))
    (with-open-file (in in-path :direction :input)
      (let ((*compile-file-pathname* in-path)
            (*compile-file-truename* (truename in))
            (*package* *package*) (*readtable* *readtable*)
            (*proclaimed-safety* *proclaimed-safety*)
            (*cf-externalizer* (%new-externalizer in-path))
            (*cf-modules* (%su-modules su)) (*cf-constants* (make-hash-table :test 'eq))
            (*cf-pending* nil) (*cf-unit* nil) (*cf-unit-forms* nil)
            (*fasl-gensyms* (%su-gensyms su)) (*cf-unit-p* t))
        (when verbose (format t "~&; compiling file ~A~%" (namestring *compile-file-truename*)))
        (handler-bind ((warning (lambda (c)
                                  (setq warnings-p t)
                                  (unless (typep c 'style-warning) (setq failure-p t))))
                       (error (lambda (c) (declare (ignore c)) (setq warnings-p t failure-p t))))
          (funcall
           (nth 6 *native-unit-hooks*) (%su-state su)
           (lambda ()
             (when (> k 0)
               (%call-with-world-lock (lambda () (funcall (nth 4 *native-unit-hooks*) k))))
             (%cf-unit-add '(%system-file-start))
             (loop
               (let* ((start (%form-start in))
                      (form (read in nil %eof-marker)))
                 (when (eq form %eof-marker) (return))
                 (let ((dspec (%definition-spec form)))
                   (when dspec
                     (push (list '%note-source dspec (namestring *compile-file-truename*) start) notes)))
                 (%cf-process form (make-broadcast-stream) nil)))
             (%cf-unit-flush)
             ;; what loads the file: a module for its consecutive segments
             ;; (their source forms if one cannot be made), the forms the
             ;; compiler failed on
             (let ((entries (reverse *cf-unit*)))
               (loop while entries
                     do (let ((group (loop while (and entries (eq (car (car entries)) :segment))
                                           collect (pop entries)
                                           ;; (objects made by the segments before, MAKE-LOAD-FORM:
                                           ;; its objects are read after they ran)
                                           until (and entries (%forms-have-fasl-ref-p (third (car entries)))))))
                          (if group
                              (push (or (handler-case
                                            (multiple-value-bind (octets vector)
                                                (%call-with-world-lock
                                                 (lambda () (funcall (nth 7 *native-unit-hooks*)
                                                                     (mapcar #'second group))))
                                              ;; (objects that cannot be written, from the
                                              ;; expansion of a macro: the source forms are
                                              ;; loaded, as COMPILE-FILE writes them, and the
                                              ;; system fasl is not made)
                                              (multiple-value-bind (text ok)
                                                  (%su-text su (list '%fasl-segment 0 vector 0 0))
                                                (if ok
                                                    (list octets text)
                                                    (progn (setf (%su-broken su) t) nil))))
                                          (error () nil))
                                        (%su-text su `(progn ,@(mapcan (lambda (e) (copy-list (third e))) group))))
                                    loads)
                              (push (%su-text su (second (pop entries))) loads)))))
             (setf (%su-entries su) (append *cf-unit* (%su-entries su))))))))
    (setf (%su-notes su) (append notes (%su-notes su)))
    (push (cons (nreverse loads) (reverse notes)) (%su-files su))
    (values warnings-p failure-p)))

(defvar *system-load-readtable* nil "The *READTABLE* when the loading of a system fasl began.")

(defun %system-file-start ()
  "At the start of each file of a system fasl: *PACKAGE* and *READTABLE* as
ASDF's loading of the file's own fasl would have them."
  (setq *package* (find-package "COMMON-LISP-USER"))
  (when *system-load-readtable* (setq *readtable* *system-load-readtable*))
  nil)

(defun %system-load-file (su k)
  "Load the Kth file compiled into SU."
  (let* ((*fasl-load-gensyms* (%su-load-gensyms su)) (*fasl-unit-runner* nil)
         (*package* *package*) (*readtable* *readtable*) (*system-load-readtable* *readtable*))
    (destructuring-bind (loads . notes) (nth k (reverse (%su-files su)))
      (dolist (l loads)
        (if (consp l)
            (funcall *native-run-octets-hook* (first l)
                     (third (%fasl-read (make-string-input-stream (second l)))))
            (%eval-toplevel (%fasl-read (make-string-input-stream l)))))
      (dolist (n notes) (apply #'%note-source (cdr n))))))

(defun %system-unit-finish (su output-file)
  "Write the fasl of the files compiled into SU to OUTPUT-FILE; its truename."
  (let ((out-path (merge-pathnames (pathname output-file))))
    (funcall
     (nth 6 *native-unit-hooks*) (%su-state su)
     (lambda ()
       (let* ((*cf-modules* (%su-modules su)) (*cf-unit* (%su-entries su)) (*cf-unit-forms* nil)
              (*cf-unit-p* t) (*fasl-gensyms* (%su-gensyms su))
              (*fasl-symbol-numbers* (make-hash-table :test 'eq))
              (text (with-output-to-string (out)
                      (%cf-write-unit out)
                      (dolist (note (reverse (%su-notes su)))
                        (when (%call-with-fasl-syntax
                               (lambda () (ignore-errors (prin1-to-string note))))
                          (%cf-write note out))))))
         (%write-fasl out-path text (reverse (first *cf-modules*)) (second *cf-modules*)))))
    (truename out-path)))

;;; ------------------------------------------------------------
;;; REPL
;;; ------------------------------------------------------------

(defvar * nil) (defvar ** nil) (defvar *** nil)
(defvar + nil) (defvar ++ nil) (defvar +++ nil)
(defvar / nil) (defvar // nil) (defvar /// nil)
(defvar - nil)

(defun %repl-eval-print (form)
  (setq - form)
  (let ((vals (multiple-value-list (if *native-eval-hook* (funcall *native-eval-hook* form) (eval form)))))
    (setq +++ ++ ++ + + -)
    (setq /// // // / / vals)
    (setq *** ** ** * * (car vals))
    (if (null vals)
        (format t "~&; No value~%")
        (dolist (v vals) (prin1 v) (terpri)))))

(defvar *debugger-level* 0)

(defun %restart-report (r)
  (let ((f (restart-report-function r)))
    (if f (with-output-to-string (s) (funcall f s)) (string (restart-name r)))))

(defun %repl-debugger (condition)
  "A small interactive debugger: list the restarts and let the user choose."
  (let* ((*debugger-level* (+ *debugger-level* 1))
         (restarts (compute-restarts condition)))
    (format t "~&; Error: ~A~%; [Condition of type ~S]~%;~%; Restarts:~%" condition (type-of condition))
    (let ((i 0))
      (dolist (r restarts)
        (format t ";  ~D: [~A] ~A~%" i (restart-name r) (%restart-report r))
        (setq i (+ i 1))))
    (loop
      (format t "~&~D] " *debugger-level*)
      (finish-output)
      (let ((form (handler-case (read *standard-input* nil %eof-marker)
                    (error () %eof-marker))))
        (cond ((eq form %eof-marker)
               (invoke-restart (car (last restarts))))
              ((and (integerp form) (< -1 form (length restarts)))
               (invoke-restart-interactively (nth form restarts)))
              ((member form '(:abort :q :top))
               (invoke-restart (car (last restarts))))
              (t (handler-case (%repl-eval-print form)
                   (error (c) (format t "~&; Error: ~A~%" c)))))))))

(defvar %read-error-marker (make-symbol "READ-ERROR"))

(defun repl (&key (prompt "* "))
  (let ((*package* *package*))
    (loop
      (fresh-line)
      (write-string prompt)
      (finish-output)
      (let ((form (handler-case (read *standard-input* nil %eof-marker)
                    (end-of-file () %eof-marker)
                    (error (c)
                      (format t "~&; read error: ~A~%" c)
                      ;; discard the rest of the line, which belongs to the bad form
                      (loop (let ((ch (read-char *standard-input* nil nil)))
                              (when (or (null ch) (char= ch #\Newline)) (return))))
                      %read-error-marker))))
        (when (eq form %eof-marker) (terpri) (return))
        (unless (eq form %read-error-marker)
          (restart-case
              ;; (a debugger of the program's own, SWANK's, is *DEBUGGER-HOOK*,
              ;; which INVOKE-DEBUGGER calls first)
              (let ((*debugger-fallback*
                      (lambda (c)
                        (if (interactive-stream-p *standard-input*)
                            (%repl-debugger c)
                            (progn (format t "~&; Error: ~A~%" c)
                                   (invoke-restart 'abort-to-toplevel))))))
                (%repl-eval-print form))
            (abort-to-toplevel () :report "Return to the top level." nil)))))))

(defun %init-file ()
  "The user's init file, ~/.clysmrc (as ~/.sbclrc for SBCL), if it exists."
  (probe-file (merge-pathnames ".clysmrc" (user-homedir-pathname))))

(defun %load-init-file ()
  (let ((file (%init-file)))
    (when file
      (handler-case (load file)
        (error (c) (format *error-output* "~&; error while loading ~A: ~A~%" (namestring file) c))))))

;;; The REPL, scripts, threads and JavaScript calling Lisp act when the
;;; debugger would be entered (*DEBUGGER-FALLBACK*, called by
;;; INVOKE-DEBUGGER after *DEBUGGER-HOOK*), not on every error signalled: a handler of ERROR there
;;; also took the conditions of type ERROR given to SIGNAL, which no handler
;;; of the program handled and which SIGNAL then returns from.
(defun %script-main (path)
  (let ((*debugger-fallback*
          (lambda (c)
            (format *error-output* "~&Unhandled ~S: ~A~%" (type-of c) c)
            (finish-output *error-output*)
            (%backtrace 2)
            (%exit 1))))
    (load path)))

(defun %command-line-arguments ()
  (let ((r nil))
    (dotimes (i (%argc) (nreverse r))
      (let* ((n (%arg-len i)) (str (make-string n)))
        (dotimes (j n) (setf (char str j) (%arg-char i j)))
        (push str r)))))

(defvar *posix-argv* nil)

(defun main ()
  "Entry point of the interactive image: run a script given as argument, or the REPL."
  (setq *posix-argv* (%command-line-arguments))
  (%init-default-pathname)
  (%init-host-features)
  ;; as a thread that can wait (runtime/thread.lisp)
  (%call-as-main-thread #'%main-1))

(defun %main-1 ()
  (let* ((args *posix-argv*)
         (userinit (not (and args (string= (car args) "--no-userinit")))))
    (unless userinit (setq args (cdr args) *posix-argv* args))
    (cond ((and args (string= (car args) "--eval"))
           (when userinit (%load-init-file))
           (dolist (form-string (cdr args))
             (%repl-eval-print (read-from-string form-string))))
          ;; scripts do not load the init file (as sbcl --script)
          (args (%script-main (car args)))
          (t (format t "Clysm Common Lisp ~A (WebAssembly GC)~%" (lisp-implementation-version))
             (setq *interactive* (eql (%is-tty) 1))
             (when userinit (%load-init-file))
             (repl)))))

