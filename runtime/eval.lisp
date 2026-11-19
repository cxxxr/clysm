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

(defun %make-interpreted-function (name ll body env block-name)
  (multiple-value-bind (decls forms) (%function-body-parts body)
    (let* ((body-forms (if block-name `((block ,block-name ,@forms)) forms))
           (f (lambda (&rest args) (%apply-interpreted ll decls body-forms env args))))
      (%set-closure-name f name)
      f)))

(defun %apply-interpreted (ll decls body env args)
  ;; special parameters are bound as soon as they are processed, so that
  ;; later init forms see the bindings; SAVED restores them on exit
  (let ((saved nil))
    (unwind-protect (%apply-interpreted-1 ll decls body env args
                                          (lambda (sym val)
                                            (push (if (boundp sym) (list sym (symbol-value sym)) (list sym))
                                                  saved)
                                            (set sym val)))
      (do () ((null saved))
        (let ((e (car saved)))
          (setq saved (cdr saved))
          (if (cdr e) (set (car e) (cadr e)) (makunbound (car e))))))))

(defun %apply-interpreted-1 (ll decls body env args bind-special)
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
      (%eval-body body new-env))))

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
(defvar *native-compile-form-hook* nil
  "When the compiler is part of the image: a function compiling a toplevel form
to a Wasm module linked with the image, returning (values octets constants).")
(defvar *native-run-module-hook* nil
  "When the compiler is part of the image: a function instantiating and running
a module made by *NATIVE-COMPILE-FORM-HOOK* (octets, constants).")
(defvar *native-run-octets-hook* nil
  "When the compiler is part of the image: a function instantiating and running
a module made by *NATIVE-COMPILE-FORM-HOOK* from its octets and constants.")
(defvar *image-id* nil
  "Identifies the image build; compiled modules in fasls are only valid for it.")
(defvar *native-unit-hooks* nil
  "When the compiler is part of the image: (CALL-WITH-UNIT ADD-FORM END-SEGMENT
FINISH), the functions with which COMPILE-FILE compiles a file into one
module (src/live.lisp, \"Compilation units\").")
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
    (if (and *native-run-module-hook* (equal id *image-id*))
        (funcall *native-run-module-hook*
                 (car *fasl-modules*) (+ (cdr *fasl-modules*) offset) length consts)
        (let ((here (file-position stream)))
          (file-position stream (+ (cdr *fasl-modules*) source-offset))
          (let ((form (%call-with-fasl-syntax
                       (lambda () (let ((*print-readably* nil)) (read stream))))))
            (declare (ignore source-length))
            (file-position stream here)
            (%eval-toplevel form))))))

(defun %fasl-entry-source (stream source-offset)
  "The source form stored in the module octets of the fasl at SOURCE-OFFSET."
  (let ((here (file-position stream)))
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
         ;; (the source forms of the modules are read from STREAM itself)
         (in (if fasl (%fasl-text-stream stream) stream))
         (file (and (not fasl) *load-truename* (typep stream 'file-stream)
                    (namestring *load-truename*))))
    (loop
      (let* ((start (and file (%form-start stream)))
             (form (if fasl
                       (%call-with-fasl-syntax (lambda () (read in nil %eof-marker)))
                       (read stream nil %eof-marker))))
        (when (eq form %eof-marker) (return t))
        (when start
          (let ((dspec (%definition-spec form)))
            (when dspec (%note-source dspec file start))))
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
          (when print (format t "~&~{~S~^ ~}~%" vals)))))))

(defun load (filespec &key (verbose *load-verbose*) (print *load-print*)
                           (if-does-not-exist t) external-format)
  (declare (ignore external-format))
  (let* ((path (%load-pathname-of filespec))
         (stream (if (streamp filespec) filespec
                     (open (if (and (null (pathname-type path)) (not (probe-file path)))
                               (or (probe-file (make-pathname :type "lisp" :defaults path))
                                   (probe-file (make-pathname :type "fasl" :defaults path))
                                   path)
                               path)
                           :direction :input :if-does-not-exist (if if-does-not-exist :error nil)))))
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
      (dotimes (i (length m)) (%fs-put-octet out (aref m i))))
    (write-string text out)))

(defvar *cf-pending* nil
  "During COMPILE-FILE: ordinary forms (reversed) waiting to be compiled
together into one module.  Forms are only batched between compile-time side
effects, so compiling them later sees the same environment.")
(defparameter %cf-batch-size 64)

(defun %cf-write (x out)
  (%call-with-fasl-syntax
   (lambda ()
     (handler-case (prin1 x out)
       (print-not-readable ()
         (let ((*print-readably* nil)) (prin1 x out))))
     (terpri out))))

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
      (when (or (member op %load-time-definers) (special-operator-p op))
        (return nil))
      (multiple-value-bind (new changed) (macroexpand-1 form *cf-env*)
        (unless changed (return nil))
        (setq form new)))))

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

(defun %cf-process (form out compile-time-too)
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
       (when compile-time-too (%eval-toplevel (%cf-wrap form)))
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
      (t (unless (%cf-compile-and-run (%cf-wrap x) out)
           (%eval-toplevel (%cf-wrap x))
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
            (*cf-externalizer* (%new-externalizer in-path)))
        (when verbose (format t "~&; compiling file ~A~%" (namestring *compile-file-truename*)))
        (handler-bind ((warning (lambda (c)
                                  (setq warnings-p t)
                                  (unless (typep c 'style-warning) (setq failure-p t))))
                       (error (lambda (c) (declare (ignore c)) (setq warnings-p t failure-p t))))
          (let* ((*cf-modules* (list nil 0))
                 (*cf-pending* nil)
                 (*cf-unit* nil) (*cf-unit-forms* nil)
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

