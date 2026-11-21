;;;; struct.lisp — DEFSTRUCT

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; slot spec: (name initform type read-only)
  (defun %parse-struct-slot (s)
    (if (consp s)
        (list (car s) (cadr s) (getf (cddr s) :type t) (getf (cddr s) :read-only))
        (list s nil t nil)))

  (defun %struct-opt (options key)
    (dolist (o options nil)
      (cond ((and (consp o) (eq (car o) key)) (return o))
            ((eq o key) (return (list o))))))

  (defun %struct-opts (options key)
    (let ((r nil))
      (dolist (o options (reverse r))
        (cond ((and (consp o) (eq (car o) key)) (push o r))
              ((eq o key) (push (list o) r))))))

  ;; A slot is matched with a BOA variable by name, not by symbol: an
  ;; included structure's slots may come from another package.
  (defun %slot-named (var slots)
    (dolist (s slots nil)
      (when (string= (symbol-name (car s)) (symbol-name var)) (return s))))

  (defun %boa-var-named (slot-name vars)
    (dolist (v vars nil)
      (when (string= (symbol-name v) (symbol-name slot-name)) (return v))))

  (defun %boa-lambda-list (ll slots)
    ;; supply slot initforms as defaults for optional/key params
    (let ((state nil) (r nil))
      (dolist (x ll (reverse r))
        (cond ((member x lambda-list-keywords) (setq state x) (push x r))
              ((and (member state '(&optional &key)) (symbolp x))
               (let ((sl (%slot-named x slots)))
                 (push (if sl (list x (cadr sl)) x) r)))
              ((and (member state '(&optional &key)) (consp x) (null (cdr x)))
               (let* ((var (if (consp (car x)) (cadr (car x)) (car x))) (sl (%slot-named var slots)))
                 (push (if sl (list (car x) (cadr sl)) x) r)))
              ((and (eq state '&aux) (symbolp x)) (push x r))
              (t (push x r))))))

  (defun %ll-bound-vars (ll)
    (let ((r nil))
      (dolist (x ll r)
        (unless (member x lambda-list-keywords)
          (let ((v (if (consp x) (car x) x)))
            (push (if (consp v) (cadr v) v) r))))))

  ;; A structure description, recorded at compile time for :INCLUDE:
  ;;   (type slots size names)
  ;; TYPE is NIL for structure classes, LIST or VECTOR otherwise; each slot is
  ;; (name initform type read-only index); NAMES is an alist (index . name) of
  ;; the names stored in a named typed structure (its own and included ones).
  (defun %struct-description (name) (get name '%struct-description))

  ;; A structure class of 1 to 32 slots keeps them in
  ;; fields of its own (the Wasm types $si1 ... in src/layout.lisp, whose
  ;; +STRUCT-INLINE-MAX+ this is), a larger one in a slot vector.
  (defun %struct-field-slots-p (size) (and (<= 1 size) (<= size 32)))
  (defun %double-slot-type-p (type) (member type '(double-float long-float)))
  ;; the number of field slots of a structure of the shape SHAPE (0: none)
  (defun %shape-field-slots (shape) (if (integerp shape) (mod shape 64) 0))

  (defun %struct-error (control &rest args)
    (error 'simple-error :format-control control :format-arguments args)))

(defmacro defstruct (name-and-options &rest slot-descs)
  (let* ((name (if (consp name-and-options) (car name-and-options) name-and-options))
         (options (if (consp name-and-options) (cdr name-and-options) nil))
         (doc (when (stringp (car slot-descs)) (car slot-descs)))
         (slot-descs (if doc (cdr slot-descs) slot-descs))
         (conc-opt (%struct-opt options :conc-name))
         (conc-name (cond ((null conc-opt) (concatenate 'string (symbol-name name) "-"))
                          ((null (cdr conc-opt)) nil)
                          ((null (cadr conc-opt)) nil)
                          (t (string (cadr conc-opt)))))
         (include-opt (%struct-opt options :include))
         (include (cadr include-opt))
         (include-desc (when include
                         (or (%struct-description include)
                             (%struct-error "DEFSTRUCT ~S: cannot include the unknown structure ~S" name include))))
         (include-overrides (mapcar #'%parse-struct-slot (cddr include-opt)))
         (type-opt (%struct-opt options :type))
         (struct-type (cadr type-opt))
         (seq-kind (cond ((null type-opt) nil)
                         ((eq struct-type 'list) 'list)
                         ((or (eq struct-type 'vector)
                              (and (consp struct-type) (eq (car struct-type) 'vector)))
                          'vector)
                         (t (%struct-error "DEFSTRUCT ~S: invalid :TYPE ~S" name struct-type))))
         (element-type (if (consp struct-type) (cadr struct-type) t))
         (named (or (null seq-kind) (not (null (%struct-opt options :named)))))
         (initial-offset (or (cadr (%struct-opt options :initial-offset)) 0))
         (own-slots (mapcar #'%parse-struct-slot slot-descs))
         (pred-opt (%struct-opt options :predicate))
         (predicate (if (and pred-opt (cdr pred-opt))
                        (cadr pred-opt)
                        (intern (concatenate 'string (symbol-name name) "-P"))))
         (copier-opt (%struct-opt options :copier))
         (copier (if (and copier-opt (cdr copier-opt)) (cadr copier-opt)
                     (intern (concatenate 'string "COPY-" (symbol-name name)))))
         (ctor-opts (%struct-opts options :constructor))
         (default-ctor (intern (concatenate 'string "MAKE-" (symbol-name name))))
         (constructors (if ctor-opts
                           (remove nil (mapcar (lambda (o) (cond ((null (cdr o)) (list default-ctor))
                                                                 ((null (cadr o)) nil)
                                                                 (t (cons (cadr o) (cddr o)))))
                                               ctor-opts))
                           (list (list default-ctor))))
         (print-fn (cadr (%struct-opt options :print-function)))
         (print-obj (cadr (%struct-opt options :print-object)))
         (x (gensym "OBJ")) (v (gensym "VAL")))
    (when (and include (not (eq (car include-desc) seq-kind)))
      (%struct-error "DEFSTRUCT ~S: the included structure ~S has a different :TYPE" name include))
    (when (and pred-opt (cdr pred-opt) (cadr pred-opt) seq-kind (not named))
      (%struct-error "DEFSTRUCT ~S: :PREDICATE requires :NAMED for a typed structure" name))
    (when (and seq-kind named (eq seq-kind 'vector) (not (subtypep 'symbol element-type)))
      (%struct-error "DEFSTRUCT ~S: the name cannot be stored in a vector of ~S" name element-type))
    ;; layout: slots are (name initform type read-only index)
    (let* ((inherited (mapcar (lambda (s)
                                (let ((ov (assoc (car s) include-overrides)))
                                  (if ov
                                      (list (car s) (cadr ov) (if (eq (caddr ov) t) (caddr s) (caddr ov))
                                            (or (cadddr ov) (cadddr s)) (nth 4 s))
                                      s)))
                              (cadr include-desc)))
           (base (+ (if include-desc (caddr include-desc) 0) (if seq-kind initial-offset 0)))
           (name-index (if (and seq-kind named) base nil))
           (first-own (+ base (if name-index 1 0)))
           (own (let ((i first-own))
                  (mapcar (lambda (s) (prog1 (append s (list i)) (setq i (+ i 1)))) own-slots)))
           (all-slots (append inherited own))
           (size (+ first-own (length own)))
           (names (append (cadddr include-desc) (if name-index (list (cons name-index name)) nil)))
           ;; the number of structures it includes (its index in +CLASS-INHERITS+)
           (depth (if include-desc (+ 1 (or (nth 5 include-desc) 0)) 0))
           ;; the slots in fields of their own (unless the structure includes
           ;; one with a slot vector, or asks for one: (SYS::%SLOT-VECTOR)):
           ;; then its shape, SIZE + 64 DEPTH, which the compiled accesses take
           (inline-rep (and (null seq-kind) (%struct-field-slots-p size)
                            (or (not (%struct-opt options '%slot-vector))
                                (and include-desc (member (nth 6 include-desc) '(:fields :typed))))
                            (or (null include-desc) (eql (caddr include-desc) 0)
                                (member (nth 6 include-desc) '(:fields :typed)))))
           ;; more slots, including a structure of field slots: a big
           ;; structure, of the Wasm type of the structures it includes (its
           ;; first slots in fields, the others in an overflow vector,
           ;; %MAKE-BIG-STRUCT), so that their compiled accessors take it
           (big-rep (and (null seq-kind) (not inline-rep) include-desc
                         (member (nth 6 include-desc) '(:fields :big))))
           ;; the kinds of its slots, when some are double floats: :D for
           ;; those, :E for the others; its type is then one of its own (a
           ;; double float slot holds the number unboxed, src/typed.lisp)
           (kinds (and inline-rep
                       (let ((ks (mapcar (lambda (s) (if (%double-slot-type-p (caddr s)) :d :e)) all-slots)))
                         (and (member :d ks) ks))))
           (field-rep (and inline-rep (not kinds)))
           ;; (for a structure of a slot vector or of typed slots: its depth only)
           (fields (and (null seq-kind) (+ (cond (field-rep size) (big-rep 32) (t 0))
                                           (* 64 depth))))
           (desc (list seq-kind all-slots size names fields depth
                       (cond (kinds :typed) (field-rep :fields) (big-rep :big) (t :vector))))
           (class-slots (mapcar (lambda (s) (subseq s 0 4)) all-slots))
           (predicate (and named predicate))
           (keyvars (mapcar (lambda (s) (declare (ignore s)) (gensym "SLOT")) all-slots)))
      (flet ((accessor (slot) (if conc-name
                                  (intern (concatenate 'string conc-name (symbol-name (car slot))))
                                  (car slot)))
             (make-form (vals)
               (if (null seq-kind)
                   (cond (kinds `(%make-tstruct (load-time-value (find-class ',name)) ',kinds ,@vals))
                         (field-rep `(%make-struct (load-time-value (find-class ',name)) ,@vals))
                         (big-rep `(%make-big-struct (load-time-value (find-class ',name)) ,@vals))
                         (t `(%make-instance (load-time-value (find-class ',name)) (vector ,@vals))))
                   (let ((cells (make-list size :initial-element nil)))
                     (dolist (n names) (setf (nth (car n) cells) `',(cdr n)))
                     (do ((s all-slots (cdr s)) (val vals (cdr val))) ((null s))
                       (setf (nth (nth 4 (car s)) cells) (car val)))
                     (if (eq seq-kind 'list)
                         `(list ,@cells)
                         `(%make-struct-vector ',element-type (list ,@cells)))))))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (%put ',name '%struct-description ',desc)
             (%put ',name '%struct-slots ',class-slots))
           ;; the keyword constructor, used by the #S reader macro
           (%put ',name '%struct-constructor ',(car (find-if (lambda (c) (null (cdr c))) constructors)))
           ,@(unless seq-kind
               `((%define-structure-class ',name ',include ',class-slots
                                          ;; a function name is looked up when printing
                                          ;; (it may be defined after the structure)
                                          ,(cond ((and print-fn (symbolp print-fn)) `',print-fn)
                                                 (print-fn `(function ,print-fn))
                                                 ((and print-obj (symbolp print-obj)) `',print-obj)
                                                 (print-obj `(function ,print-obj))
                                                 (t nil))
                                          ,(if print-obj t nil))))
           ;; constructors
           ,@(mapcar
              (lambda (c)
                (let ((cname (car c)))
                  (if (cdr c)
                      (let ((ll (%boa-lambda-list (cadr c) all-slots))
                            (vars (%ll-bound-vars (cadr c))))
                        `(defun ,cname ,ll
                           ,(make-form (mapcar (lambda (s)
                                                 (or (%boa-var-named (car s) vars) (cadr s)))
                                               all-slots))))
                      `(defun ,cname (&key ,@(mapcar (lambda (s k)
                                                       (list (list (intern (symbol-name (car s)) "KEYWORD") k)
                                                             (cadr s)))
                                                     all-slots keyvars))
                         ,(make-form keyvars)))))
              constructors)
           ;; a structure of typed slots: the functions by which generic code
           ;; (slot access by index, copying) handles its type
           ,@(when kinds
               (let ((cls `(load-time-value (find-class ',name))) (o (gensym "O")) (i (gensym "I"))
                     (val (gensym "V")))
                 `((%set-class-slot
                    (find-class ',name) +class-shape-ops+
                    (vector (lambda (,o ,i)
                              (case ,i ,@(loop for k below size collect `(,k (%struct-tref ,o ,cls ,k ',kinds)))))
                            (lambda (,o ,i ,val)
                              (case ,i ,@(loop for k below size collect `(,k (%struct-tset ,o ,cls ,k ,val ',kinds)))))
                            (lambda () (%make-tstruct-default ,cls ',kinds))
                            (lambda (,o) (%copy-tstruct ,o ,cls ',kinds)))))))
           ;; (for code compiled later in the running image: the type of a call)
           ,@(unless seq-kind
               (mapcar (lambda (c) `(%note-struct-inline :constructor ',(car c) ',name nil nil))
                       constructors))
           ;; predicate
           ,@(when (and predicate (null seq-kind))
               `((%note-struct-inline :predicate ',predicate ',name nil nil ,fields)))
           ,@(when predicate
               `((defun ,predicate (,x)
                   ,(cond ((null seq-kind)
                           `(%structure-typep ,x (load-time-value (find-class ',name)) ,fields))
                          ((eq seq-kind 'list) `(%typed-struct-list-p ,x ,name-index ',name))
                          (t `(%typed-struct-vector-p ,x ,name-index ',name))))))
           ;; copier
           ,@(when copier
               `((defun ,copier (,x) ,(cond ((eq seq-kind 'list) `(copy-list ,x))
                                            (seq-kind `(copy-seq ,x))
                                            (t `(%copy-structure-of ,x (load-time-value (find-class ',name))))))))
           ;; accessors
           ,@(let ((forms nil))
               (dolist (s all-slots (reverse forms))
                 (let ((acc (accessor s)) (idx (nth 4 s)))
                   (when (null seq-kind)
                     (push `(eval-when (:compile-toplevel)
                              (%host-inline-struct-accessor ,acc ,name ,idx ,(not (cadddr s)) ,fields ,kinds))
                           forms)
                     ;; (for code compiled later in the running image)
                     (push `(%note-struct-inline :accessor ',acc ',name ,idx ,(not (cadddr s)) ,fields ',kinds)
                           forms))
                   (push (cond ((null seq-kind)
                                `(defun ,acc (,x) ,(cond (kinds `(%struct-tref ,x (load-time-value (find-class ',name)) ,idx ',kinds))
                                                         ((or field-rep big-rep) `(%struct-iref ,x (load-time-value (find-class ',name)) ,idx ,fields))
                                                         (t `(%struct-ref ,x (load-time-value (find-class ',name)) ,idx ,fields)))))
                               ((eq seq-kind 'list) `(defun ,acc (,x) (nth ,idx ,x)))
                               (t `(defun ,acc (,x) (aref ,x ,idx))))
                         forms)
                   (unless (cadddr s)
                     (push (cond ((null seq-kind)
                                  `(defun (setf ,acc) (,v ,x)
                                     ,(cond (kinds `(%struct-tset ,x (load-time-value (find-class ',name)) ,idx ,v ',kinds))
                                            ((or field-rep big-rep) `(%struct-iset ,x (load-time-value (find-class ',name)) ,idx ,v ,fields))
                                            (t `(%struct-set ,x (load-time-value (find-class ',name)) ,idx ,v ,fields)))))
                                 ((eq seq-kind 'list) `(defun (setf ,acc) (,v ,x) (setf (nth ,idx ,x) ,v)))
                                 (t `(defun (setf ,acc) (,v ,x) (setf (aref ,x ,idx) ,v))))
                           forms)))))
           ,@(when doc `((%put ',name '%structure-documentation ,doc)))
           ',name)))))

;;; In an image with the compiler, the accessors and the predicate of a
;;; structure are compiled inline at call sites compiled after the DEFSTRUCT
;;; (the hook registers them with the compiler).
(defvar *struct-inline-hook* nil)
(defun %note-struct-inline (kind function-name name index writable &optional fields kinds)
  (when *struct-inline-hook*
    (funcall *struct-inline-hook* kind function-name name index writable fields kinds))
  function-name)

(defun %make-struct-vector (element-type cells)
  (let ((v (make-array (length cells) :element-type element-type)) (i 0))
    (dolist (c cells v)
      (when (or c (eq (upgraded-array-element-type element-type) t))
        (setf (aref v i) c))
      (setq i (+ i 1)))))

(defun %typed-struct-list-p (x index name)
  (do ((l x (cdr l)) (i 0 (+ i 1)))
      ((not (consp l)) nil)
    (when (= i index) (return (eq (car l) name)))))

(defun %typed-struct-vector-p (x index name)
  (and (vectorp x) (> (length x) index) (eq (aref x index) name)))

(defun %copy-structure-of (x class)
  (unless (%structure-typep x class) (%type-error x (%class-name class)))
  (%copy-instance x))

(defun %define-structure-class (name include slots printer print-object-p)
  ;; a structure redefined with another number of slots gets a new class: its
  ;; instances are of another Wasm type (see %STRUCT-FIELD-SLOTS-P), and the
  ;; old ones are then not taken for instances of the new
  (let ((old (find-class name nil)))
    (when (and old (eq (%class-slot old +class-kind+) :structure)
               (not (eql (length (%class-effective-slots old)) (length slots))))
      (%set-find-class name nil)))
  (let* ((super (if include (find-class include) (find-class 'structure-object)))
         (class (%ensure-class-object name (find-class 'structure-class) (list super)
                                      (mapcar (lambda (s)
                                                (%make-slotd (car s)
                                                             :initargs (list (intern (symbol-name (car s)) "KEYWORD"))
                                                             :initform (cadr s)
                                                             :type (caddr s)))
                                              ;; only the slots not inherited are direct
                                              (nthcdr (length (if include (get include '%struct-slots) nil)) slots))
                                      :structure)))
    (%set-class-slot class +class-extra+ (cons printer print-object-p))
    ;; the included structures by depth, itself last (for constant-time type tests)
    (%set-class-slot class +class-inherits+
                     (let ((up (and include (%class-slot super +class-inherits+))))
                       (concatenate 'simple-vector (or up #()) (vector class))))
    class))

;; (the functions of the primitives, for FUNCALL and the interpreter)
(defun %make-struct (layout &rest slots)
  (let ((n (length slots)))
    (if (%struct-field-slots-p n)
        (let ((x (%sinst-make n)) (i 0))
          (%set-instance-layout x layout)
          (dolist (v slots x) (%set-instance-gref x i v) (setq i (+ i 1))))
        (%make-instance layout (coerce slots 'simple-vector)))))

(defun %make-big-struct (layout &rest slots)
  (let ((x (%sinst-make (length slots))) (i 0))
    (%set-instance-layout x layout)
    (dolist (v slots x) (%set-instance-gref x i v) (setq i (+ i 1)))))

(defun %structure-typep (x class &optional size)
  (declare (ignore size))
  (and (%instancep x)
       (let ((l (%instance-layout x)))
         (or (eq l class) (and (%instancep l) (not (null (member class (%class-cpl l)))))))))

(defun %struct-ref (x class i &optional shape)
  (declare (ignore shape))
  (unless (%structure-typep x class) (%type-error x (%class-name class)))
  (%check-struct-slot x i)
  (%instance-gref x i))

(defun %struct-set (x class i v &optional shape)
  (declare (ignore shape))
  (unless (%structure-typep x class) (%type-error x (%class-name class)))
  (%check-struct-slot x i)
  (%set-instance-gref x i v))

;; an instance made before its structure was redefined with fewer slots
(defun %check-struct-slot (x i)
  (unless (< i (%instance-size x))
    (error 'simple-error :format-control "~S was made before its structure was redefined"
                         :format-arguments (list x))))

;; the number of slots of an instance
(defun %instance-size (x) (%instance-nslots x))

;;; Structures with typed slots: generic code reaches their slots by index
;;; through the functions of their class (made by DEFSTRUCT)
(defun %shape-ops (x)
  (or (%class-slot (%instance-layout x) +class-shape-ops+)
      (error "~S is not a structure of typed slots" x)))
(defun %tstruct-gref (x i) (funcall (svref (%shape-ops x) 0) x i))
(defun %tstruct-gset (x i v) (funcall (svref (%shape-ops x) 1) x i v))
(defun %tstruct-copy (x) (funcall (svref (%shape-ops x) 3) x))
;; (what the compiled code does, for the interpreter: where the class's
;; functions are interpreted too, the instances have a slot vector, which
;; the accessors take by their general way)
(defun %make-tstruct (layout kinds &rest slots)
  (let ((ops (%class-slot layout +class-shape-ops+)))
    (if ops
        (let ((x (funcall (svref ops 2))) (i 0))
          (dolist (v slots x) (%set-instance-gref x i v) (setq i (+ i 1))))
        (%make-tstruct-default layout kinds))))
(defun %make-tstruct-default (layout kinds)
  (let ((v (%make-simple-vector (length kinds) nil)) (i 0))
    (dolist (k kinds) (when (eq k :d) (setf (svref v i) 0d0)) (setq i (+ i 1)))
    (%make-instance layout v)))
(defun %copy-tstruct-slow (x class kinds)
  (declare (ignore class kinds))
  (if (%sinstp x)
      (error "~S cannot be copied by the interpreter" x)
      (%make-instance (%instance-layout x) (copy-seq (%instance-slots x)))))

(defun copy-structure (x)
  (if (%instancep x) (%copy-instance x) (%type-error x 'structure-object)))
