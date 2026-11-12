;;;; boot.lisp — first stage of the Clysm runtime: fundamental macros
;;;;
;;;; Read in package SYSTEM.  Macro expanders run on the host at compile time
;;;; and are also compiled into the image (for EVAL / MACROEXPAND at runtime).
;;;; Expanders in this file must only use simple list functions, since they
;;;; are compiled into the target before most of the library exists.

(declaim (special *package* *standard-output* *standard-input* *error-output* *terminal-io*
                  *trace-output* *debug-io* *query-io* *print-escape* *print-readably* *print-base*
                  *print-radix* *print-case* *print-circle* *print-pretty* *print-level*
                  *print-length* *print-array* *print-gensym* *print-lines* *print-right-margin*
                  *print-miser-width* *print-pprint-dispatch* *read-base* *read-default-float-format*
                  *read-eval* *read-suppress* *readtable* *features* *modules* *gensym-counter*
                  *random-state* *break-on-signals* *debugger-hook* *macroexpand-hook*
                  *compile-file-pathname* *compile-file-truename* *load-pathname* *load-truename*
                  *compile-print* *compile-verbose* *load-print* *load-verbose*
                  *default-pathname-defaults* * ** *** + ++ +++ / // /// -
                  *keyword-package* *system-package* *cl-package* *package-table*
                  *subtype-table* *handler-clusters* *restart-clusters* *print-object-hook*))

(defmacro defun (name lambda-list &body body)
  `(%defun ',name (function (named-lambda ,name ,lambda-list ,@body))))

(defmacro when (test &body body) `(if ,test (progn ,@body) nil))
(defmacro unless (test &body body) `(if ,test nil (progn ,@body)))

(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (car clauses)) (rest (cdr clauses)))
        (if (null (cdr clause))
            (let ((g (gensym)))
              `(let ((,g ,(car clause))) (if ,g ,g (cond ,@rest))))
            (if (and (eq (car clause) t) (null rest))
                `(progn ,@(cdr clause))
                `(if ,(car clause) (progn ,@(cdr clause)) (cond ,@rest)))))))

(defmacro and (&rest forms)
  (if (null forms) t
      (if (null (cdr forms)) (car forms)
          `(if ,(car forms) (and ,@(cdr forms)) nil))))

(defmacro or (&rest forms)
  (if (null forms) nil
      (if (null (cdr forms)) (car forms)
          (let ((g (gensym)))
            `(let ((,g ,(car forms))) (if ,g ,g (or ,@(cdr forms))))))))

(defmacro prog1 (first &body body)
  (let ((g (gensym))) `(let ((,g ,first)) ,@body ,g)))
(defmacro prog2 (first second &body body)
  `(progn ,first (prog1 ,second ,@body)))
(defmacro return (&optional value) `(return-from nil ,value))

(defmacro defvar (var &optional (value nil valuep) doc)
  (declare (ignore doc))
  `(progn (declaim (special ,var))
          ,@(when valuep `((eval-when (:compile-toplevel) (%ct-initial-value ,var ,value))
                           (unless (boundp ',var) (setq ,var ,value))))
          ',var))

(defmacro defparameter (var value &optional doc)
  (declare (ignore doc))
  `(progn (declaim (special ,var))
          (eval-when (:compile-toplevel) (%ct-initial-value ,var ,value))
          (setq ,var ,value) ',var))

(defmacro defconstant (var value &optional doc)
  (declare (ignore doc))
  `(progn (eval-when (:compile-toplevel) (%ct-constant ,var ,value))
          (declaim (special ,var))
          (%defconstant ',var ,value)))

(defconstant lambda-list-keywords
  '(&optional &rest &key &aux &body &whole &environment &allow-other-keys))
(defconstant call-arguments-limit 536870911)
(defconstant lambda-parameters-limit 536870911)
(defconstant multiple-values-limit 1024)

(defmacro psetq (&rest pairs)
  (let ((bindings nil) (sets nil))
    (do ((p pairs (cddr p))) ((null p))
      (let ((g (gensym)))
        (setq bindings (cons (list g (cadr p)) bindings))
        (setq sets (cons `(setq ,(car p) ,g) sets))))
    `(let ,(reverse bindings) ,@(reverse sets) nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %split-decls (body)
    (if (and (consp (car body)) (eq (caar body) 'declare))
        (multiple-value-bind (d f) (%split-decls (cdr body))
          (values (cons (car body) d) f))
        (values nil body))))

(defmacro dolist ((var list &optional result) &body body)
  (let ((l (gensym "LIST")) (top (gensym "TOP")))
    (multiple-value-bind (decls forms) (%split-decls body)
      `(block nil
         (let ((,l ,list))
           (tagbody
              ,top
              (if (atom ,l) (return-from nil (let ((,var nil)) (declare (ignorable ,var)) ,result)))
              (let ((,var (car ,l)))
                ,@decls
                (tagbody ,@forms))
              (setq ,l (cdr ,l))
              (go ,top)))))))

(defmacro do (varlist endlist &body body)
  (%expand-do varlist endlist body 'let 'psetq))
(defmacro do* (varlist endlist &body body)
  (%expand-do varlist endlist body 'let* 'setq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expand-do (varlist endlist body let-op set-op)
    (let ((top (gensym "DO-TOP"))
          (bindings nil) (steps nil) (decls nil) (forms body))
      (dolist (v varlist)
        (if (symbolp v)
            (setq bindings (cons (list v nil) bindings))
            (progn
              (setq bindings (cons (list (car v) (cadr v)) bindings))
              (when (cddr v)
                (setq steps (cons (car v) steps))
                (setq steps (cons (caddr v) steps))))))
      (multiple-value-bind (d f) (%split-decls body) (setq decls d) (setq forms f))
      `(block nil
         (,let-op ,(reverse bindings)
          ,@(reverse decls)
          (tagbody
             ,top
             (if ,(car endlist) (return-from nil (progn ,@(cdr endlist))))
             (tagbody ,@forms)
             ,@(when steps `((,set-op ,@(reverse steps))))
             (go ,top)))))))

(defmacro dotimes ((var count &optional result) &body body)
  (let ((n (gensym "N")) (top (gensym "TOP")))
    (multiple-value-bind (decls forms) (%split-decls body)
      `(block nil
         (let ((,n ,count) (,var 0))
           (declare (ignorable ,var))
           ,@decls
           (tagbody
              ,top
              (if (>= ,var ,n) (return-from nil ,result))
              (tagbody ,@forms)
              (setq ,var (1+ ,var))
              (go ,top)))))))

(defmacro case (key &rest clauses)
  (let ((k (gensym "KEY")))
    `(let ((,k ,key))
       (cond ,@(mapcar (lambda (c)
                         (let ((keys (car c)))
                           (cond ((member keys '(t otherwise)) `(t ,@(cdr c)))
                                 ((listp keys) `((%member-eql ,k ',keys) ,@(cdr c)))
                                 (t `((eql ,k ',keys) ,@(cdr c))))))
                       clauses)))))

(defmacro ecase (key &rest clauses)
  (let ((k (gensym "KEY")))
    `(let ((,k ,key))
       (case ,k ,@clauses
         (otherwise (%case-failure ,k ',(%case-keys clauses)))))))

(defmacro ccase (key &rest clauses) `(ecase ,key ,@clauses))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %case-keys (clauses)
    (let ((keys nil))
      (dolist (c clauses)
        (if (listp (car c))
            (dolist (k (car c)) (setq keys (cons k keys)))
            (setq keys (cons (car c) keys))))
      (reverse keys))))

(defmacro multiple-value-list (form) `(multiple-value-call #'list ,form))
(defmacro nth-value (n form) `(nth ,n (multiple-value-list ,form)))

(defmacro multiple-value-setq (vars form)
  (let ((temps (mapcar (lambda (v) (declare (ignore v)) (gensym)) vars)))
    (if (null vars)
        `(values ,form)
        `(multiple-value-bind ,temps ,form
           ,@(mapcar (lambda (v tmp) `(setq ,v ,tmp)) vars temps)
           ,(car temps)))))

(defmacro prog (vars &body body)
  (multiple-value-bind (decls forms) (%split-decls body)
    `(block nil (let ,vars ,@decls (tagbody ,@forms)))))
(defmacro prog* (vars &body body)
  (multiple-value-bind (decls forms) (%split-decls body)
    `(block nil (let* ,vars ,@decls (tagbody ,@forms)))))

(defmacro declaim (&rest specs) `(%proclaim-list ',specs))

;;; ------------------------------------------------------------
;;; DESTRUCTURING-BIND
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %db-bindings (ll whole)
    "Returns a list of LET* bindings destructuring WHOLE according to LL."
    (let ((bindings nil) (cur (gensym "CUR")) (state :req) (restp nil) (keyp nil) (aok nil)
          (keys nil))
      (setq bindings (cons (list cur whole) bindings))
      (do () ((null ll))
        (if (atom ll)
            (progn (setq bindings (cons (list ll cur) bindings))
                   (setq restp t)
                   (setq ll nil))
            (let ((x (car ll)))
              (setq ll (cdr ll))
              (cond
                ((eq x '&optional) (setq state :opt))
                ((or (eq x '&rest) (eq x '&body))
                 (let ((v (car ll)))
                   (setq ll (cdr ll))
                   (setq restp t)
                   (if (consp v)
                       (let ((g (gensym)))
                         (setq bindings (cons (list g cur) bindings))
                         (setq bindings (append (reverse (%db-bindings v g)) bindings)))
                       (setq bindings (cons (list v cur) bindings)))))
                ((eq x '&key) (setq state :key) (setq keyp t))
                ((eq x '&allow-other-keys) (setq aok t))
                ((eq x '&aux) (setq state :aux))
                ((eq x '&whole)
                 (setq bindings (cons (list (car ll) cur) bindings))
                 (setq ll (cdr ll)))
                ((eq x '&environment) (setq ll (cdr ll)))
                ((eq state :req)
                 (if (consp x)
                     (let ((g (gensym)))
                       (setq bindings (cons `(,g (if ,cur (car ,cur) (%db-error "too few elements"))) bindings))
                       (setq bindings (append (reverse (%db-bindings x g)) bindings)))
                     (setq bindings (cons `(,x (if ,cur (car ,cur) (%db-error "too few elements"))) bindings)))
                 (setq bindings (cons `(,cur (cdr ,cur)) bindings)))
                ((eq state :opt)
                 (let* ((spec (if (consp x) x (list x)))
                        (var (car spec)) (init (cadr spec)) (svar (caddr spec)))
                   (when svar (setq bindings (cons `(,svar (if ,cur t nil)) bindings)))
                   (if (consp var)
                       (let ((g (gensym)))
                         (setq bindings (cons `(,g (if ,cur (car ,cur) ,init)) bindings))
                         (setq bindings (append (reverse (%db-bindings var g)) bindings)))
                       (setq bindings (cons `(,var (if ,cur (car ,cur) ,init)) bindings)))
                   (setq bindings (cons `(,cur (cdr ,cur)) bindings))))
                ((eq state :key)
                 (let* ((spec (if (consp x) x (list x)))
                        (vs (car spec))
                        (var (if (consp vs) (cadr vs) vs))
                        (kw (if (consp vs) (car vs) (intern (symbol-name vs) "KEYWORD")))
                        (init (cadr spec)) (svar (caddr spec))
                        (g (gensym)))
                   (setq keys (cons kw keys))
                   (setq bindings (cons `(,g (%db-getf ,cur ',kw)) bindings))
                   (when svar (setq bindings (cons `(,svar (if (eq ,g '%db-none) nil t)) bindings)))
                   (if (consp var)
                       (let ((g2 (gensym)))
                         (setq bindings (cons `(,g2 (if (eq ,g '%db-none) ,init ,g)) bindings))
                         (setq bindings (append (reverse (%db-bindings var g2)) bindings)))
                       (setq bindings (cons `(,var (if (eq ,g '%db-none) ,init ,g)) bindings)))))
                ((eq state :aux)
                 (setq bindings (cons (if (consp x) (list (car x) (cadr x)) (list x nil)) bindings)))))))
      (when keyp
        (setq bindings (cons `(,(gensym) (%db-check-keys ,cur ',keys ,aok)) bindings)))
      (when (not (or restp keyp))
        (setq bindings (cons `(,(gensym) (if ,cur (%db-error "too many elements") nil)) bindings)))
      (reverse bindings))))

(defmacro destructuring-bind (lambda-list expression &body body)
  (let ((whole (gensym "WHOLE")))
    (multiple-value-bind (decls forms) (%split-decls body)
      (let ((bindings (%db-bindings lambda-list whole)))
        `(let* ((,whole ,expression) ,@bindings)
           (declare (ignorable ,@(mapcar #'car bindings)))
           ,@decls
           ,@forms)))))

;;; from now on, macros are also installed in the target image
(%emit-deferred-macros)

;;; ------------------------------------------------------------
;;; SETF machinery
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-setf-expansion (place env)
    (cond ((symbolp place)
           (multiple-value-bind (exp changed) (%macroexpand-1 place env)
             (if changed
                 (%get-setf-expansion exp env)
                 (let ((store (gensym "NEW")))
                   (values nil nil (list store) `(setq ,place ,store) place)))))
          ((consp place)
           (let* ((op (car place))
                  (expander (if (symbolp op) (get op 'setf-expander) nil)))
             (cond (expander (funcall expander place env))
                   ((and (symbolp op) (get op 'setf-update))
                    (let ((temps (mapcar (lambda (x) (declare (ignore x)) (gensym)) (cdr place)))
                          (store (gensym "NEW")))
                      (values temps (cdr place) (list store)
                              `(,(get op 'setf-update) ,@temps ,store)
                              `(,op ,@temps))))
                   (t (multiple-value-bind (exp changed) (%macroexpand-1 place env)
                        (if changed
                            (%get-setf-expansion exp env)
                            (let ((temps (mapcar (lambda (x) (declare (ignore x)) (gensym)) (cdr place)))
                                  (store (gensym "NEW")))
                              (values temps (cdr place) (list store)
                                      `(funcall #'(setf ,op) ,store ,@temps)
                                      `(,op ,@temps)))))))))
          (t (error "Invalid place: ~S" place))))

  (defun %expand-setf-1 (place value env)
    (if (and (symbolp place)
             (not (nth-value 1 (%macroexpand-1 place env))))
        `(setq ,place ,value)
        (multiple-value-bind (temps vals stores setter) (%get-setf-expansion place env)
          (if (and (consp place) (symbolp (car place)) (get (car place) 'setf-update)
                   (null (cdr stores)))
              ;; simple defsetf form: avoid temporaries for simple args
              `(let* (,@(mapcar #'list temps vals) (,(car stores) ,value)) ,setter)
              `(let* (,@(mapcar #'list temps vals))
                 (multiple-value-bind ,stores ,value ,setter)))))))

(defmacro setf (&rest pairs &environment env)
  (cond ((null pairs) nil)
        ((null (cddr pairs)) (%expand-setf-1 (car pairs) (cadr pairs) env))
        (t (let ((forms nil))
             (do ((p pairs (cddr p))) ((null p))
               (setq forms (cons (%expand-setf-1 (car p) (cadr p) env) forms)))
             `(progn ,@(reverse forms))))))

(defmacro psetf (&rest pairs &environment env)
  (let ((lets nil) (sets nil))
    (do ((p pairs (cddr p))) ((null p))
      (let ((g (gensym)))
        (setq lets (cons (list g (cadr p)) lets))
        (setq sets (cons (%expand-setf-1 (car p) g env) sets))))
    `(let ,(reverse lets) ,@(reverse sets) nil)))

(defmacro defsetf (access-fn &rest rest)
  (if (and rest (symbolp (car rest)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (%put ',access-fn 'setf-update ',(car rest))
         ',access-fn)
      ;; long form: (defsetf name lambda-list (store-vars) body)
      (let ((ll (car rest)) (stores (cadr rest)) (body (cddr rest))
            (place (gensym)) (env (gensym)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (%put ',access-fn 'setf-expander
                 (lambda (,place ,env)
                   (declare (ignore ,env))
                   (let* ((temps (mapcar (lambda (x) (declare (ignore x)) (gensym)) (cdr ,place)))
                          (stores (mapcar (lambda (x) (declare (ignore x)) (gensym)) ',stores)))
                     (values temps (cdr ,place) stores
                             (apply (lambda (,@stores ,@ll) ,@body) (append stores temps))
                             (cons ',access-fn temps)))))
           ',access-fn))))

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (let ((place (gensym)) (env (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%put ',access-fn 'setf-expander
             (lambda (,place ,env)
               (declare (ignorable ,env))
               (destructuring-bind ,(%remove-env-from-ll lambda-list env) (cdr ,place) ,@body)))
       ',access-fn)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %remove-env-from-ll (ll env)
    (cond ((null ll) nil)
          ((atom ll) ll)
          ((eq (car ll) '&environment)
           ;; bind the variable to the env via &aux
           (append (%remove-env-from-ll (cddr ll) env) (list '&aux (list (cadr ll) env))))
          (t (cons (car ll) (%remove-env-from-ll (cdr ll) env))))))

;;; Read-modify-write macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expand-rmw (place env fn)
    "FN receives the getter form and returns the new-value form."
    (if (and (symbolp place) (not (nth-value 1 (%macroexpand-1 place env))))
        `(setq ,place ,(funcall fn place))
        (multiple-value-bind (temps vals stores setter getter) (%get-setf-expansion place env)
          `(let* (,@(mapcar #'list temps vals)
                  (,(car stores) ,(funcall fn getter)))
             ,setter)))))

(defmacro incf (place &optional (delta 1) &environment env)
  (%expand-rmw place env (lambda (g) `(+ ,g ,delta))))
(defmacro decf (place &optional (delta 1) &environment env)
  (%expand-rmw place env (lambda (g) `(- ,g ,delta))))
(defmacro push (item place &environment env)
  (let ((g (gensym)))
    `(let ((,g ,item))
       ,(%expand-rmw place env (lambda (getter) `(cons ,g ,getter))))))
(defmacro pushnew (item place &rest keys &environment env)
  (let ((g (gensym)))
    `(let ((,g ,item))
       ,(%expand-rmw place env (lambda (getter) `(adjoin ,g ,getter ,@keys))))))
(defmacro pop (place &environment env)
  (if (and (symbolp place) (not (nth-value 1 (%macroexpand-1 place env))))
      `(prog1 (car ,place) (setq ,place (cdr ,place)))
      (multiple-value-bind (temps vals stores setter getter) (%get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* (,@(mapcar #'list temps vals)
                  (,g ,getter)
                  (,(car stores) (cdr ,g)))
             ,setter
             (car ,g))))))

(defmacro define-modify-macro (name lambda-list function &optional doc)
  (declare (ignore doc))
  (let* ((place (gensym "PLACE")) (env (gensym "ENV"))
         (rest-var (cadr (member '&rest lambda-list)))
         (args (remove rest-var (%ll-vars lambda-list))))
    `(defmacro ,name (,place ,@lambda-list &environment ,env)
       (%expand-rmw ,place ,env (lambda (g) (list* ',function g (append (list ,@args) ,rest-var)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %ll-vars (ll)
    (let ((vars nil))
      (dolist (x ll)
        (unless (member x '(&optional &rest &key &aux &body &allow-other-keys))
          (setq vars (cons (if (consp x) (car x) x) vars))))
      (reverse vars))))

(defmacro rotatef (&rest places &environment env)
  (if (or (null places) (null (cdr places)))
      nil
      (let ((expansions (mapcar (lambda (p) (multiple-value-list (%get-setf-expansion p env))) places)))
        (let ((lets nil) (setters nil))
          (dolist (e expansions)
            (setq lets (append lets (mapcar #'list (first e) (second e)))))
          (let ((getters (mapcar #'fifth expansions)))
            (do ((es expansions (cdr es))
                 (gs (append (cdr getters) (list (car getters))) (cdr gs)))
                ((null es))
              (setq lets (append lets (list (list (car (third (car es))) (car gs))))))
            (dolist (e expansions) (setq setters (cons (fourth e) setters))))
          `(let* ,lets ,@(reverse setters) nil)))))

(defmacro shiftf (&rest places-and-value &environment env)
  (let* ((places (butlast places-and-value))
         (value (car (last places-and-value)))
         (expansions (mapcar (lambda (p) (multiple-value-list (%get-setf-expansion p env))) places))
         (g (gensym)))
    (let ((lets nil))
      (dolist (e expansions)
        (setq lets (append lets (mapcar #'list (first e) (second e)))))
      (setq lets (append lets (list (list g (fifth (car expansions))))))
      (do ((es expansions (cdr es))
           (vals (append (mapcar #'fifth (cdr expansions)) (list value)) (cdr vals)))
          ((null es))
        (setq lets (append lets (list (list (car (third (car es))) (car vals))))))
      `(let* ,lets ,@(mapcar #'fourth expansions) ,g))))

;;; standard places
(defsetf car %setcar)
(defsetf cdr %setcdr)
(defsetf first %setcar)
(defsetf rest %setcdr)
(defsetf symbol-value %set-symbol-value)
(defsetf symbol-function %set-symbol-function)
(defsetf symbol-plist %set-symbol-plist)
(defsetf svref %svset)
(defsetf schar %scharset)
(defsetf char %charset)
(define-setf-expander aref (array &rest subscripts)
  (let ((atemp (gensym "A")) (stemps (mapcar (lambda (s) (declare (ignore s)) (gensym "I")) subscripts))
        (store (gensym "NEW")))
    (values (cons atemp stemps) (cons array subscripts) (list store)
            (if (and stemps (null (cdr stemps)))
                `(%aset1 ,atemp ,(car stemps) ,store)
                `(%aset ,atemp ,@stemps ,store))
            `(aref ,atemp ,@stemps))))
(defsetf row-major-aref %row-major-aset)
(defsetf fill-pointer %set-fill-pointer)
(defsetf gethash %puthash)
(defsetf get %put)
(defsetf nth %setnth)
(defsetf elt %setelt)
(defsetf fdefinition %set-fdefinition)
(defsetf macro-function %set-macro-function)
(defsetf documentation %set-documentation)
(defsetf compiler-macro-function %set-compiler-macro-function)
(defsetf logical-pathname-translations %set-lpt)
(defsetf bit %aset)
(defsetf sbit %aset)
(defsetf subseq %set-subseq)
(defsetf find-class %set-find-class)
(defsetf slot-value %set-slot-value)
(macrolet ((cxr-setf (name path)
             ;; (setf (cadr x) v) == (%setcar (cdr x) v)
             (let ((x (gensym)) (v (gensym)))
               (labels ((build (p) (if (null p) x
                                       `(list ',(if (eql (car p) #\A) 'car 'cdr) ,(build (cdr p))))))
                 (let ((chars (coerce path 'list)))
                   `(defsetf ,name (,x) (,v)
                      (list ',(if (eql (car chars) #\A) '%setcar '%setcdr)
                            ,(build (cdr chars))
                            ,v)))))))
  (cxr-setf caar "AA") (cxr-setf cadr "AD") (cxr-setf cdar "DA") (cxr-setf cddr "DD")
  (cxr-setf caddr "ADD") (cxr-setf cdddr "DDD") (cxr-setf second "AD") (cxr-setf third "ADD")
  (cxr-setf fourth "ADDD") (cxr-setf cadddr "ADDD"))

(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (temps vals stores setter getter) (%get-setf-expansion place env)
    (let ((itemp (gensym)) (store (gensym)) (dtemp (gensym)))
      (values `(,@temps ,itemp ,dtemp)
              `(,@vals ,indicator ,default)
              (list store)
              `(let ((,(car stores) (%putf ,getter ,itemp ,store)))
                 ,setter
                 ,store)
              `(getf ,getter ,itemp ,dtemp)))))

(define-setf-expander values (&rest places &environment env)
  (let ((temps nil) (vals nil) (stores nil) (setters nil) (getters nil))
    (dolist (p places)
      (multiple-value-bind (ts vs ss setter getter) (%get-setf-expansion p env)
        (setq temps (append temps ts) vals (append vals vs)
              stores (append stores (list (car ss))))
        (setq setters (append setters (list setter)) getters (append getters (list getter)))))
    (values temps vals stores `(values ,@setters) `(values ,@getters))))

(define-setf-expander the (type place &environment env)
  (declare (ignore type))
  (%get-setf-expansion place env))

(define-setf-expander apply (fn &rest args)
  (let ((temps (mapcar (lambda (x) (declare (ignore x)) (gensym)) args))
        (store (gensym)))
    (values temps args (list store)
            `(apply #'(setf ,(cadr fn)) ,store ,@temps)
            `(apply ,fn ,@temps))))

(define-setf-expander ldb (bytespec int &environment env)
  (multiple-value-bind (temps vals stores setter getter) (%get-setf-expansion int env)
    (let ((btemp (gensym)) (store (gensym)))
      (values `(,@temps ,btemp) `(,@vals ,bytespec) (list store)
              `(let ((,(car stores) (dpb ,store ,btemp ,getter))) ,setter ,store)
              `(ldb ,btemp ,getter)))))

(define-setf-expander mask-field (bytespec int &environment env)
  (multiple-value-bind (temps vals stores setter getter) (%get-setf-expansion int env)
    (let ((btemp (gensym)) (store (gensym)))
      (values `(,@temps ,btemp) `(,@vals ,bytespec) (list store)
              `(let ((,(car stores) (deposit-field ,store ,btemp ,getter))) ,setter ,store)
              `(mask-field ,btemp ,getter)))))

;;; ------------------------------------------------------------
;;; misc core macros
;;; ------------------------------------------------------------

(defmacro typecase (key &rest clauses)
  (let ((k (gensym "KEY")))
    `(let ((,k ,key))
       (cond ,@(mapcar (lambda (c)
                         (if (member (car c) '(t otherwise))
                             `(t ,@(cdr c))
                             `((typep ,k ',(car c)) ,@(cdr c))))
                       clauses)))))

(defmacro etypecase (key &rest clauses)
  (let ((k (gensym "KEY")))
    `(let ((,k ,key))
       (typecase ,k ,@clauses
         (otherwise (%etypecase-failure ,k '(or ,@(mapcar #'car clauses))))))))
(defmacro ctypecase (key &rest clauses) `(etypecase ,key ,@clauses))

(defmacro with-gensyms (names &body body)
  `(let ,(mapcar (lambda (n) `(,n (gensym ,(symbol-name n)))) names) ,@body))

(defmacro defsubst (name ll &body body) `(defun ,name ,ll ,@body))

(defmacro define-symbol-macro (name expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-symbol-macro ',name ',expansion)))

(defmacro define-compiler-macro (name lambda-list &body body)
  ;; compiler macros run on the host at compile time only
  `(progn (eval-when (:compile-toplevel) (%host-define-compiler-macro ,name ,lambda-list ,@body))
          ',name))

(defmacro assert (test &optional places datum &rest args)
  (declare (ignore places))
  (let ((top (gensym "ASSERT")))
    `(block ,top
       (tagbody
          again
          (when ,test (return-from ,top nil))
          (restart-case ,(if datum
                             `(error ,datum ,@args)
                             `(error "The assertion ~S failed." ',test))
            (continue () :report "Retry the assertion." nil))
          (go again)))))

(defmacro check-type (place type &optional type-string)
  (let ((top (gensym "CHECK-TYPE")))
    `(block ,top
       (tagbody
          again
          (when (typep ,place ',type) (return-from ,top nil))
          (setf ,place (%check-type-error ',place ,place ',type ,type-string))
          (go again)))))

