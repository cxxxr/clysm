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

(defun %macro-lambda (name ll body)
  (multiple-value-bind (whole envv clean) (%parse-macro-lambda-list ll)
    (let ((form (gensym "FORM")) (env (gensym "ENV")))
      (multiple-value-bind (decls forms) (%split-decls body)
        `(named-lambda ,name (,form ,env)
           (declare (ignorable ,form ,env))
           (let (,@(when whole `((,whole ,form))) ,@(when envv `((,envv ,env))))
             (destructuring-bind ,clean (cdr ,form)
               ,@decls
               (block ,name ,@forms))))))))

(%set-macro-function 'defmacro
  (lambda (form env)
    (declare (ignore env))
    (destructuring-bind (name ll &rest body) (cdr form)
      `(progn (%set-macro-function ',name (function ,(%macro-lambda name ll body)))
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
       (multiple-value-bind (decls forms) (%decls-of (cddr form))
         (declare (ignore decls))
         (%eval-body forms new-env))))
    (macrolet
     (let ((new-env env))
       (dolist (def (cadr form))
         (push (list* :macro (car def)
                      (%eval-function (%macro-lambda (car def) (cadr def) (cddr def)) env))
               new-env))
       (%eval-body (cddr form) new-env)))
    (symbol-macrolet
     (let ((new-env env))
       (dolist (b (cadr form)) (push (list* :smacro (car b) (cadr b)) new-env))
       (multiple-value-bind (decls forms) (%decls-of (cddr form))
         (declare (ignore decls))
         (%eval-body forms new-env))))
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
    (catch (catch (%eval (cadr form) env) (%eval-body (cddr form) env)))
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
    (locally (multiple-value-bind (decls forms) (%decls-of (cdr form))
               (let ((new-env env))
                 (dolist (d decls)
                   (when (and (consp d) (eq (car d) 'special))
                     (dolist (s (cdr d)) (push (list :special s) new-env))))
                 (%eval-body forms new-env))))
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

(defun %make-interpreted-function (name ll body env block-name)
  (multiple-value-bind (decls forms) (%decls-of body)
    (when (and (stringp (car forms)) (cdr forms)) (setq forms (cdr forms)))
    (let* ((body-forms (if block-name `((block ,block-name ,@forms)) forms))
           (f (lambda (&rest args) (%apply-interpreted ll decls body-forms env args))))
      (%set-closure-name f name)
      f)))

(defun %apply-interpreted (ll decls body env args)
  (let ((new-env env) (specials nil) (special-vals nil) (state :req) (keys-seen nil) (rest-args args)
        (aok nil))
    (labels ((bind (sym val)
               (if (or (%special-declared-p sym decls) (%special-p sym))
                   (progn (push sym specials) (push val special-vals) (push (list :special sym) new-env))
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
      (if specials
          (progv specials special-vals (%eval-body body new-env))
          (%eval-body body new-env)))))

;;; ------------------------------------------------------------
;;; compile, load
;;; ------------------------------------------------------------

(defvar *native-compile-hook* nil "set when the compiler is part of the image")
(defvar *native-eval-hook* nil "when set, the REPL compiles each form natively")

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

(defun load (filespec &key verbose print (if-does-not-exist t) external-format)
  (declare (ignore verbose external-format))
  (let ((stream (if (streamp filespec) filespec
                    (open filespec :direction :input :if-does-not-exist (if if-does-not-exist :error nil)))))
    (if (null stream)
        nil
        (let ((*package* *package*) (*readtable* *readtable*)
              (*load-pathname* (if (streamp filespec) nil (pathname filespec)))
              (*load-truename* (if (streamp filespec) nil (pathname filespec))))
          (loop
            (let ((form (read stream nil %eof-marker)))
              (when (eq form %eof-marker) (return t))
              (let ((vals (multiple-value-list
                           (if *native-eval-hook* (funcall *native-eval-hook* form) (eval form)))))
                (when print (format t "~&~{~S~^ ~}~%" vals)))))))))

(defun compile-file (input-file &key output-file verbose print external-format)
  (declare (ignore output-file verbose print external-format))
  ;; there is no file compiler in the image; load the source instead
  (load input-file)
  (values (pathname input-file) nil nil))

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

(defun repl (&key (prompt "* "))
  (let ((*package* *package*))
    (loop
      (fresh-line)
      (write-string prompt)
      (finish-output)
      (let ((form (handler-case (read *standard-input* nil %eof-marker)
                    (end-of-file () %eof-marker)
                    (error (c) (format t "~&; read error: ~A~%" c) '(values)))))
        (when (eq form %eof-marker) (terpri) (return))
        (restart-case
            (handler-bind ((error (lambda (c)
                                    (if (interactive-stream-p *standard-input*)
                                        (%repl-debugger c)
                                        (progn (format t "~&; Error: ~A~%" c)
                                               (invoke-restart 'abort-to-toplevel))))))
              (%repl-eval-print form))
          (abort-to-toplevel () :report "Return to the top level." nil))))))

(defun %script-main (path)
  (handler-bind ((error (lambda (c)
                          (format *error-output* "~&Unhandled ~S: ~A~%" (type-of c) c)
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
  (let ((args *posix-argv*))
    (cond ((and args (string= (car args) "--eval"))
           (dolist (form-string (cdr args))
             (%repl-eval-print (read-from-string form-string))))
          (args (%script-main (car args)))
          (t (format t "Clysm Common Lisp ~A (WebAssembly GC)~%" (lisp-implementation-version))
             (setq *interactive* (eql (%is-tty) 1))
             (repl)))))
