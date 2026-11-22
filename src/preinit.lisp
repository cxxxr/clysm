;;;; preinit.lisp — a shaken program whose runtime starts from a heap made
;;;; when the program is compiled, instead of by the runtime's toplevel forms
;;;;
;;;; The runtime's toplevel forms make its classes, generic functions and
;;;; their methods, structures, packages, readtables and tables: run in every
;;;; program, they keep their definers (%DEFMETHOD, %DEFINE-STRUCTURE-CLASS,
;;;; %FINALIZE-CLASS, %DEFPACKAGE...) and all those reach in every shaken
;;;; module.  Here (with --shake, on SBCL) the program is compiled as usual;
;;;; then a module whose start function runs only the runtime's toplevel
;;;; forms and saves the heap (%PREINIT-SAVE, runtime/image.lisp) is run on
;;;; the host, and the module made has instead a data segment of those
;;;; objects, which its start function makes (as a heap image is loaded,
;;;; :IMAGE-LOAD-PREINIT) before it runs the program's toplevel forms.
;;;;
;;;; The objects are shaken with the code (SHAKE-MODULE, *SHAKE-HEAP-HOOK*):
;;;; an object is kept when kept code, a kept global or a kept object refers
;;;; to it.  As a function definition is kept only when its symbol is
;;;; referred to, what a symbol names (its value, function and properties)
;;;; is kept only when the symbol is: by kept code, a kept global or a kept
;;;; object, other than a package (its tables of symbols keep the symbols,
;;;; not what they name).  Every symbol stays.  Some properties are kept
;;;; only when what reads them is (*PREINIT-PROPERTY-USERS*), and the
;;;; expanders of macros only when code or a constant names the macro
;;;; (*PREINIT-CODE-PROPERTIES*).  A constant whose fields are as its
;;;; initializer made them has no record, and the conses and vectors that
;;;; its initializer made are not written again (PH-MATCH-GLOBALS).  The
;;;; methods of classes that no instance of can be made are removed before
;;;; the heap is saved (PH-INSTANTIABLE-CLASSES).
(in-package :clysm)

;;; the heap saved by %PREINIT-SAVE (the format of runtime/image.lisp)
(defvar *ph-words*)                     ; (unsigned-byte 32) words
(defvar *ph-nbase*)
(defvar *ph-kind*)                      ; index -> kind (NIL: an immediate)
(defvar *ph-size*)                      ; index -> length of a vector, free variables of a closure...
(defvar *ph-imm*)                       ; index -> its immutable contents, items
(defvar *ph-rec*)                       ; index -> the record of its mutable fields, items
(defvar *ph-roots*)                     ; the word of the roots
;;; an item: (:w . word), (:r . reference word), (:c . place in the table of
;;; functions), (:b . word) of four octets (of a string)

;;; the module the heap was saved by
(defvar *ph-base-names*)                ; index -> name of the global (of the image's objects)
(defvar *ph-base-index*)                ; name -> index (the first of an object in two)
(defvar *ph-table*)                     ; place in the table -> function name
(defvar *ph-mutables*)                  ; name of a mutable global -> its value (a reference word)

;;; shaking
(defvar *ph-state*)                     ; index -> 0, 1 (kept, as a package keeps), 2 (kept)
(defvar *ph-reached*)                   ; index of a symbol of the image -> T once referred to
(defvar *ph-queue*)
(defvar *ph-draining* nil)
(defvar *ph-kept-symbols* nil)          ; index of a symbol of the image -> T once its global is kept
(defvar *ph-weak-cells* nil)            ; cons of the value of a property -> the functions that read it
(defvar *ph-pending* nil)               ; such a function, or (:code symbol) -> the values to keep once it is
(defvar *ph-code-reached* nil)          ; index of a symbol -> T once code or a constant refers to it
(defvar *ph-match* nil)                 ; index of an object of the image -> the heap objects of its initializer
(defvar *ph-cover* nil)                 ; heap index -> the object of the image whose initializer made it
(defvar *ph-owner* nil)                 ; (CLYSM_PREINIT_STATS) index -> what first kept it
(defvar *ph-cur* nil)
(defvar *ph-kept-mutables* nil)         ; name of a kept mutable global -> T
(defvar *ph-class-data* nil)            ; index of an object of the class system's own data -> T
(defvar *ph-data-symbols* nil)          ; index of a symbol kept as such data -> T

(defun ph-ref-index (w)
  (and (not (logtest w 1)) (not (zerop w)) (1- (ash w -1))))

(defun ph-read-file (path)
  "The words of the heap in the file that the host wrote (host/common.mjs saveImage)."
  (let* ((octets (with-open-file (s path :element-type '(unsigned-byte 8))
                   (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                     (read-sequence v s) v)))
         (u32 (lambda (p) (logior (aref octets p) (ash (aref octets (+ p 1)) 8)
                                  (ash (aref octets (+ p 2)) 16) (ash (aref octets (+ p 3)) 24))))
         (jlen (funcall u32 8))
         (json (map 'string #'code-char (subseq octets 12 (+ 12 jlen))))
         (field (lambda (name)
                  (let* ((key (format nil "\"~A\":" name)) (p (search key json)))
                    (unless p (error "preinit: no ~A in the heap's header" name))
                    (parse-integer json :start (+ p (length key)) :junk-allowed t))))
         (modules-offset (logand (+ 12 jlen 7) (lognot 7)))
         (heap-offset (+ modules-offset (funcall field "modulesLength")))
         (n (floor (funcall field "heapLength") 4))
         (words (make-array n :element-type '(unsigned-byte 32))))
    (dotimes (i n words)
      (setf (aref words i) (funcall u32 (+ heap-offset (* 4 i)))))))

(defun ph-parse (words)
  (let* ((pos 5) (nbase (aref words 2)) (nheap (aref words 3)) (nrec (aref words 4))
         (n (+ nbase nheap))
         (kind (make-array n :initial-element nil)) (size (make-array n :initial-element 0))
         (imm (make-array n :initial-element nil)) (rec (make-array n :initial-element nil)))
    (unless (and (= (aref words 0) +img-magic+) (= (aref words 1) +img-version+))
      (error "preinit: not a heap image"))
    ;; (the kinds of the image's objects: after the roots, at the end)
    (let ((d (- (length words) (* 2 nbase))))
      (dotimes (i nbase)
        (let ((k (aref words (+ d (* 2 i)))))
          (setf (aref kind i) (if (= k #xFFFFFFFF) nil (nth k +img-kinds+))
                (aref size i) (aref words (+ d (* 2 i) 1))))))
    (flet ((w () (prog1 (aref words pos) (incf pos))))
      (flet ((raw (n) (loop repeat n collect (cons :w (w))))
             (ref () (cons :r (w)))
             (code () (cons :c (w))))
        (loop for i from nbase below n
              do (let ((k (nth (w) +img-kinds+)))
                   (setf (aref kind i) k)
                   (setf (aref imm i)
                         (ecase k
                           ((:cons :cell :package :mdarray) nil)
                           (:vector (let ((len (w))) (setf (aref size i) len) (list (cons :w len))))
                           (:string8 (let ((len (w)))
                                        (list* (cons :w len) (loop repeat (ceiling len 4) collect (cons :b (w))))))
                           ((:string32 :limbs) (let ((len (w))) (list* (cons :w len) (raw len))))
                           (:instance (let* ((slots (ref)) (hash (w))) (list slots (cons :w hash))))
                           (:sinst (let* ((len (w)) (hash (w)))
                                     (setf (aref size i) len)
                                     (list (cons :w len) (cons :w hash))))
                           (:symbol (let* ((name (ref)) (hash (w))) (list name (cons :w hash))))
                           (:closure (let* ((codes (loop repeat 7 collect (code)))
                                            (klayout (ref)) (entry-a (code)) (env (w)))
                                       (setf (aref size i) env)
                                       (append codes (list klayout entry-a (cons :w env)))))
                           (:double (raw 2))
                           (:single (raw 1))
                           (:bignum (let* ((sign (w)) (limbs (ref))) (list* (cons :w sign) limbs (raw 2))))
                           ((:ratio :complex) (let* ((a (ref)) (b (ref))) (list a b)))
                           (:complex-d (raw 4))
                           (:complex-s (raw 2))
                           (:storage (let* ((sk (w)) (len (w)))
                                       (list* (cons :w sk) (cons :w len) (loop repeat len collect (ref)))))
                           (:fsbuf (let* ((o (ref)) (p (w)) (f (w))) (list o (cons :w p) (cons :w f))))))))
        (dotimes (r nrec)
          (let* ((i (ph-ref-index (w))) (k (aref kind i)))
            (setf (aref rec i)
                  (ecase k
                    (:cons (let* ((a (ref)) (d (ref))) (list a d)))
                    (:vector (loop repeat (aref size i) collect (ref)))
                    (:symbol (let* ((p (ref)) (v (ref)) (f (ref)) (pl (ref)) (fl (w)) (nm (ref)) (h (w)))
                               (list p v f pl (cons :w fl) nm (cons :w h))))
                    ((:closure :sinst) (let ((first (ref))) (cons first (loop repeat (aref size i) collect (ref)))))
                    (:instance (let* ((a (ref)) (b (ref))) (list a b)))
                    (:cell (list (ref)))
                    (:package (loop repeat 8 collect (ref)))
                    (:mdarray (let ((refs (loop repeat 5 collect (ref)))) (append refs (raw 3))))))))
        (setf *ph-roots* (w))))
    (setf *ph-words* words *ph-nbase* nbase *ph-kind* kind *ph-size* size *ph-imm* imm *ph-rec* rec)))

;;; marking

(defun ph-node (i)
  (if (< i *ph-nbase*)
      (cons :global (aref *ph-base-names* i))
      (cons :heap (format nil "~(~A~) ~D~@[ ~S~]~@[ ~A~]" (aref *ph-kind* i) i
                          (and (eq (aref *ph-kind* i) :closure)
                               (aref *ph-table* (cdr (first (aref *ph-imm* i)))))
                          (and *shake-why* (ph-instance-label i))))))

(defun ph-instance-label (i)
  "For --shake-why: the name of the class of the instance I, and the
instance's own name (its slot 0) when that is a symbol (a class, a generic
function), or NIL."
  (flet ((sym (w) (let ((j (and w (ph-ref-index w)))) (and j (ph-base-symbol j)))))
    (let ((l (ph-layout i)))
      (when (and l (eq (aref *ph-kind* l) :instance))
        (let ((cname (sym (ph-slot l 0))) (own (and (eq (aref *ph-kind* i) :instance) (sym (ph-slot i 0)))))
          (when cname
            (format nil "<~A~@[ ~A~]>" cname own)))))))

(defun ph-base-symbol (i)
  (let ((name (and (< i *ph-nbase*) (aref *ph-base-names* i))))
    (and (consp name) (eq (car name) :sym) (eq (aref *ph-kind* i) :symbol) (second name))))

(defvar *ph-data-mark* nil)              ; true: a global marked is the class system's data
(defvar *ph-hook-global* nil)           ; the index of the global PH-HOOK is called for
(defun ph-mark (i mode)
  (let ((old (aref *ph-state* i)))
    ;; (with --shake-symbols, a symbol that only a package's tables refer
    ;; to is left out, and the tables without it, PH-EMIT)
    (when (and *shake-symbols* (= mode 1) (< i *ph-nbase*) (ph-base-symbol i) (not (gethash i *ph-kept-symbols*)))
      (return-from ph-mark))
    (when (< old mode)
      (setf (aref *ph-state* i) mode)
      (cond ((< i *ph-nbase*)
             ;; (the globals of the symbols are kept whatever refers to
             ;; them, without what they name: unless --shake-symbols
             ;; leaves out those nothing refers to)
             (let ((sym (ph-base-symbol i)))
               (cond ((null sym)
                      ;; (a global that the class system's data refers to:
                      ;; its initializer is data too, *SHAKE-DATA-CONTEXT*)
                      (when (and (zerop old) (not (eql i *ph-hook-global*)))
                        (funcall *shake-mark* (if *ph-data-mark* :data-global :global) (aref *ph-base-names* i))))
                     ((= mode 2)
                      (when *shake-symbols*
                        (setf (gethash i *ph-kept-symbols*) t)
                        (funcall *shake-mark* :global (list :sym sym)))
                      (funcall *shake-mark* :symbol sym)))))
            (*shake-why* (shake-edge (ph-node i))))
      (when (and (zerop old) *ph-owner*) (setf (aref *ph-owner* i) *ph-cur*))
      ;; (an object that the initializer of a global made, which something
      ;; else refers to: that global's record is written after all)
      (let ((b (and (>= i *ph-nbase*) (aref *ph-cover* i))))
        (when b (ph-uncover b)))
      (push (cons i mode) *ph-queue*))))

(defun ph-uncover (b)
  (let ((covered (aref *ph-match* b)))
    (when covered
      (setf (aref *ph-match* b) nil)
      (dolist (j covered) (unless (eq j :none) (setf (aref *ph-cover* j) nil)))
      (when (plusp (aref *ph-state* b)) (push (cons b 2) *ph-queue*)))))

(defun ph-mark-ref (w mode)
  (let ((i (ph-ref-index w))) (when i (ph-mark i mode))))

(defun ph-mark-code (n)
  (unless (= n #xFFFFFFFF)
    (funcall *shake-mark* :func (aref *ph-table* n))))

(defun ph-mark-items (items mode)
  (dolist (it items)
    (case (car it)
      (:r (ph-mark-ref (cdr it) mode))
      (:c (ph-mark-code (cdr it))))))

(defun ph-symbol-reached-p (i)
  (if (< i *ph-nbase*) (gethash i *ph-reached*) (= (aref *ph-state* i) 2)))

(defun ph-walk-symbol (i mode)
  (when (>= i *ph-nbase*) (ph-mark-items (aref *ph-imm* i) 2))
  (let ((rec (aref *ph-rec* i)))
    (when rec
      (ph-mark-ref (cdr (first rec)) 2)
      (when (or (ph-symbol-reached-p i) (= mode 2))
        (ph-symbol-named i)))))

(defun ph-mark-cell (i)
  "Keep the cons I of a plist, without what it refers to."
  (let ((old (aref *ph-state* i)))
    (when (< old 2)
      (setf (aref *ph-state* i) 2)
      (when (and (< i *ph-nbase*) (zerop old))
        (funcall *shake-mark* :global (aref *ph-base-names* i))))))

(defparameter *preinit-property-users*
  '((clysm-sys::%compiler-macro-source compiler-macro-function)
    (clysm-sys::%compiler-macro-function compiler-macro-function)
    (clysm-sys::%documentation documentation)
    (clysm-sys::%structure-documentation documentation)
    ;; (the keyword constructor of a structure, which #S calls)
    (clysm-sys::%struct-constructor clysm-sys::%sharp-s))
  "Properties of symbols that only the functions named after them read: a
value is kept only if one of them is.")

(defparameter *preinit-code-properties* '(clysm-sys::%macro-function clysm-sys::%macro-lambda-list)
  "Properties of symbols kept only when code or a constant refers to the
symbol (not only objects made by the runtime's toplevel forms, as the
tables of the pretty printer): the expanders of macros, which compiled code
has used already, as SYS:%WHEN-REACHED keeps them (but those of COMMON-LISP,
PH-PROPERTY-USERS).")

(defparameter *preinit-reader-functions* '(clysm-sys::%intern-token clysm-sys::%read-plain-symbol)
  "The functions through which the reader makes the symbols of what it reads.")

(defun ph-property-users (w owner)
  "The keys of what keeps the property of indicator W of the symbol OWNER:
functions that read it, or (:CODE OWNER); NIL if it is kept."
  (let* ((i (ph-ref-index w)) (ind (and i (ph-base-symbol i))))
    (cond ((null ind) nil)
          ;; (the macros of COMMON-LISP stay when the reader does: they are
          ;; those of forms read and evaluated, LOAD of a source file)
          ((member ind *preinit-code-properties*)
           (if (eq (symbol-package (ph-base-symbol owner)) (find-package :common-lisp))
               (list* (list :code owner) *preinit-reader-functions*)
               (list (list :code owner))))
          (t (cdr (assoc ind *preinit-property-users*))))))

(defun ph-user-reached-p (users)
  (some (lambda (u)
          (if (consp u)
              (gethash (second u) *ph-code-reached*)
              (let ((i (gethash (list :sym u) *ph-base-index*))) (and i (gethash i *ph-reached*)))))
        users))

(defun ph-symbol-named (i &optional (function t))
  "What the symbol I names: its value, function (unless FUNCTION is false)
and properties."
  (let ((rec (aref *ph-rec* i)))
    (when rec
      (ph-mark-ref (cdr (second rec)) 2)
      (when function (ph-mark-ref (cdr (third rec)) 2))
      (let ((pairs (ph-plist-pairs i)) (last (cdr (fourth rec))))
        (dolist (p pairs)
          (destructuring-bind (ind val . c2) p
            (let ((c1 (ph-ref-index last)))
              (ph-mark-cell c1) (ph-mark-cell c2)
              (setf last (cdr (second (aref *ph-rec* c2))))
              (ph-mark-ref ind 2)
              (let ((users (ph-property-users ind i)))
                (let ((*ph-data-mark* (member (ph-base-symbol (or (ph-ref-index ind) 0)) *preinit-data-properties*)))
                  (when *ph-data-mark* (ph-data-tree val))
                  (if (and users (not (ph-user-reached-p users)))
                      (progn (setf (gethash c2 *ph-weak-cells*) users)
                             (dolist (u users) (push val (gethash u *ph-pending*))))
                      (ph-mark-ref val 2)))))))
        (ph-mark-ref last 2)))))

(defun ph-mark-data-symbol (j)
  "Keep the base symbol J, which an object of the class system's own data
refers to (PH-CLASS-DATA-OBJECTS), with its value and properties but not
its function, unless something else refers to it."
  (unless (gethash j *ph-data-symbols*)
    (setf (gethash j *ph-data-symbols*) t (gethash j *ph-kept-symbols*) t)
    (when *shake-symbols* (funcall *shake-mark* :global (list :sym (ph-base-symbol j))))
    (ph-mark j 1)
    (unless (ph-symbol-reached-p j) (ph-symbol-named j nil))))

(defun ph-walk (i mode)
  (when (gethash i *ph-class-data*)
    (dolist (it (append (aref *ph-imm* i)
                        ;; (not the record of an object as its initializer made it)
                        (unless (and (< i *ph-nbase*) (aref *ph-match* i)) (aref *ph-rec* i))))
      (let ((j (and (eq (car it) :r) (ph-ref-index (cdr it)))))
        (cond ((and j (ph-base-symbol j)) (ph-mark-data-symbol j))
              ((eq (car it) :c) (ph-mark-code (cdr it)))
              (j (let ((*ph-data-mark* t)) (ph-mark j mode))))))
    (return-from ph-walk))
  (case (aref *ph-kind* i)
    (:symbol (ph-walk-symbol i mode))
    ;; (its tables of symbols; the symbols that shadow others stay with
    ;; --shake-symbols)
    (:package (let ((rec (aref *ph-rec* i)))
                (ph-mark-items rec 1)
                (when *shake-symbols* (ph-mark-items (list (nth 6 rec)) 2))))
    (t (ph-mark-items (aref *ph-imm* i) mode)
       ;; (not the record of an object as its initializer made it)
       (unless (and (< i *ph-nbase*) (aref *ph-match* i))
         (ph-mark-items (aref *ph-rec* i) mode)))))

;;; The objects of the image whose fields are as their globals' initializers
;;; made them (most are constant lists and vectors) need no record, nor the
;;; objects of the heap that the initializer made as parts of them.

(defun ph-init-value (init)
  "What the constant expression INIT makes: (:g NAME), (:i31 K), (:null),
(:cons A D), (:vec ITEMS), (:str CODES), or NIL if something else."
  (let ((stack nil))
    (dolist (x init (and (= (length stack) 1) (first stack)))
      (cond ((and (consp x) (eq (car x) :i32.const)) (push (list :i32 (second x)) stack))
            ((eq x :ref.i31) (let ((v (pop stack))) (unless (eq (car v) :i32) (return nil))
                               (push (list :i31 (second v)) stack)))
            ((and (consp x) (eq (car x) :global.get)) (push (list :g (second x)) stack))
            ((and (consp x) (eq (car x) :ref.null)) (push (list :null) stack))
            ((equal x '(:struct.new :cons)) (let* ((d (pop stack)) (a (pop stack))) (push (list :cons a d) stack)))
            ((and (consp x) (eq (car x) :array.new_fixed) (member (second x) '(:vector :string)))
             (let ((items (loop repeat (third x) collect (pop stack))))
               (when (and items (null (car (last items)))) (return nil))
               (setf items (nreverse items))
               (if (eq (second x) :string)
                   (if (every (lambda (v) (eq (car v) :i32)) items)
                       (push (list :str (mapcar #'second items)) stack)
                       (return nil))
                   (push (list :vec items) stack))))
            (t (return nil))))))

(defun ph-match-value (v w covered)
  "COVERED with the heap objects of V if the reference word W is what V
makes, or :FAIL."
  (if (eq covered :fail)
      :fail
      (ecase (car v)
        (:g (if (eql (ph-ref-index w) (gethash (second v) *ph-base-index*)) covered :fail))
        (:i31 (if (= w (logand (logior (ash (second v) 1) 1) #xFFFFFFFF)) covered :fail))
        (:i32 :fail)
        (:null (if (zerop w) covered :fail))
        ((:cons :vec :str)
         (let ((j (ph-ref-index w)))
           (if (or (null j) (< j *ph-nbase*) (aref *ph-cover* j) (member j covered))
               :fail
               (ph-match-object v j (cons j covered))))))))

(defun ph-match-object (v j covered)
  (let ((rec (aref *ph-rec* j)) (kind (aref *ph-kind* j)))
    (ecase (car v)
      (:cons (if (and (eq kind :cons) rec)
                 (ph-match-value (third v) (cdr (second rec)) (ph-match-value (second v) (cdr (first rec)) covered))
                 :fail))
      (:vec (if (and (eq kind :vector) (= (aref *ph-size* j) (length (second v)))
                     (or rec (null (second v))))
                (let ((c covered))
                  (loop for item in (second v) for it in rec do (setf c (ph-match-value item (cdr it) c)))
                  c)
                :fail))
      (:str (let ((codes (second v)) (imm (aref *ph-imm* j)))
              (if (and (member kind '(:string8 :string32)) (= (cdr (first imm)) (length codes))
                       (if (eq kind :string8)
                           (loop for c in codes for k from 0
                                 always (= c (ldb (byte 8 (* 8 (mod k 4))) (cdr (nth (1+ (floor k 4)) imm)))))
                           (loop for c in codes for it in (rest imm) always (= c (cdr it)))))
                  covered
                  :fail))))))

(defun ph-match-globals (inits)
  "Find the objects of the image as their initializers (INITS: name -> code) made them."
  (dotimes (i *ph-nbase*)
    (let ((v (and (member (aref *ph-kind* i) '(:cons :vector)) (aref *ph-rec* i)
                  (eql (gethash (aref *ph-base-names* i) *ph-base-index*) i)
                  (ph-init-value (gethash (aref *ph-base-names* i) inits)))))
      (when (and v (member (car v) '(:cons :vec)))
        (let ((covered (ph-match-object v i nil)))
          (unless (eq covered :fail)
            ;; (T: none, but the record is not needed)
            (setf (aref *ph-match* i) (or covered (list :none)))
            (dolist (j covered) (unless (eq j :none) (setf (aref *ph-cover* j) i)))))))))

(defun ph-drain ()
  (unless *ph-draining*
    (let ((*ph-draining* t))
      (loop while *ph-queue*
            do (destructuring-bind (i . mode) (pop *ph-queue*)
                 (let ((*shake-walking* (if *shake-why* (ph-node i) *shake-walking*))
                       (*ph-cur* (if (or (< i *ph-nbase*) (null *ph-owner*)) i (aref *ph-owner* i))))
                   (ph-walk i mode)))))))

(defun ph-hook (kind name)
  (ecase kind
    (:global
     (let ((i (gethash name *ph-base-index*)))
       (when (and i (ph-base-symbol i)) (setf (gethash i *ph-kept-symbols*) t))
       ;; (the global is marked already, by SHAKE-MODULE)
       (when i (let ((*ph-hook-global* i)) (ph-mark i (if (ph-base-symbol i) 1 2)))))
     (let ((v (gethash name *ph-mutables*)))
       (when (and v (not (eq name :ih-key)))
         (when *ph-kept-mutables* (setf (gethash name *ph-kept-mutables*) t))
         (ph-mark-ref v 2))))
    (:data-symbol
     (let ((i (gethash (list :sym name) *ph-base-index*)))
       (when i (ph-mark-data-symbol i))))
    (:code-symbol
     (let ((i (gethash (list :sym name) *ph-base-index*)))
       (when (and i (not (gethash i *ph-code-reached*)))
         (setf (gethash i *ph-code-reached*) t)
         (let ((key (list :code i)))
           (dolist (v (gethash key *ph-pending*)) (ph-mark-ref v 2))
           (remhash key *ph-pending*)))))
    (:symbol
     (let ((i (gethash (list :sym name) *ph-base-index*)))
       (when i
         (setf (gethash i *ph-reached*) t)
         (dolist (v (gethash name *ph-pending*)) (ph-mark-ref v 2))
         (remhash name *ph-pending*)
         (ph-mark i 1)
         (when (> (aref *ph-state* i) 0) (ph-symbol-named i))))))
  (ph-drain))

;;; the module made

(defun ph-run-runtime (m init prologue runtime-tops drop)
  "Run the runtime's toplevel forms of the module M on the host, and read
the heap that they made.  DROP is NIL (%PREINIT-DROP-METHODS guesses the
classes whose methods it removes), :NONE or the names of those classes;
the names of the classes are the second value."
  (let* ((start (find-func m :start))
         (body (wasm-func-body start))
         (dir (let ((tmp (sb-ext:posix-getenv "TMPDIR")))
                (pathname (concatenate 'string (string-right-trim "/" (if (and tmp (plusp (length tmp))) tmp "/tmp"))
                                       "/clysm-preinit/"))))
         (stem (format nil "~36R" (random (expt 36 10) (make-random-state t))))
         (wasm (merge-pathnames (concatenate 'string stem ".wasm") dir))
         (img (merge-pathnames (concatenate 'string stem ".img") dir)))
    (ensure-directories-exist dir)
    (setf (wasm-func-body start)
          (append init prologue runtime-tops (lisp-call-instrs 'clysm-sys::%preinit-save 0) '(:drop))
          (wasm-func-ref-list start) :unknown)
    (let ((octets (let ((*func-body-sizes* nil)) (with-build-section (encode-module m)))))
      (setf (wasm-func-body start) body (wasm-func-ref-list start) :unknown)
      (with-open-file (o wasm :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence octets o)))
    (unwind-protect
         (let* ((out (make-string-output-stream))
                (env (append
                      (list "CLYSM_SAVE_IMAGE=1" (concatenate 'string "CLYSM_PREINIT_FILE=" (namestring img)))
                      (and drop
                           (list (format nil "CLYSM_PREINIT_DROP=(~{~A~^ ~})"
                                         (mapcar (lambda (sym)
                                                   (flet ((q (s) (with-output-to-string (o)
                                                                   (loop for c across s
                                                                         do (when (member c '(#\| #\\)) (write-char #\\ o))
                                                                            (write-char c o)))))
                                                     (format nil "|~A|::|~A|" (q (target-package-name (symbol-package sym)))
                                                             (q (symbol-name sym)))))
                                                 (if (eq drop :none) nil drop)))))
                      (remove-if (lambda (e)
                                   (or (eql 0 (search "CLYSM_" e)) (eql 0 (search "NODE_OPTIONS=" e))))
                                 (sb-ext:posix-environ))))
                (p (sb-ext:run-program (or (sb-ext:posix-getenv "NODE") "node")
                                       (list (namestring (merge-pathnames "host/run.mjs" *preinit-root*))
                                             (namestring wasm))
                                       :search t :output out :error out :input nil :environment env))
                (text (get-output-stream-string out)))
           (unless (and (zerop (sb-ext:process-exit-code p)) (probe-file img))
             (error "preinit: the runtime's toplevel forms failed on the host:~%~A" text))
           (values (ph-read-file img) (ph-dropped-classes text)))
      (when (probe-file wasm) (delete-file wasm))
      (when (probe-file img) (delete-file img)))))

(defun ph-dropped-classes (text)
  "The names of the classes that %PREINIT-DROP-METHODS wrote to the output TEXT."
  (let* ((key "clysm-preinit-drop:") (b (search key text))
         (e (and b (search "clysm-preinit-drop-end" text :start2 b))))
    (unless e (error "preinit: the classes of the methods removed are not in the output:~%~A" text))
    (with-input-from-string (s text :start (+ b (length key)) :end e)
      (loop for line = (read-line s nil)
            while line
            for tab = (position #\Tab line)
            when tab
              collect (or (find-symbol (subseq line (1+ tab)) (find-target-package (subseq line 0 tab)))
                          (error "preinit: no symbol ~A" line))))))

(defun ph-refs (body fn)
  (labels ((walk (x)
             (when (consp x)
               (when (and (eq (car x) :ref.func) (consp (cdr x))) (funcall fn (second x)))
               (do ((l x (cdr l))) ((atom l)) (walk (car l))))))
    (walk body)))

(defun ph-load-heap (m words)
  "Read the heap WORDS that the module M saved."
  (multiple-value-bind (objects mutables) (image-globals m)
    (setf *ph-base-names* (coerce objects 'simple-vector)
          *ph-base-index* (make-hash-table :test 'equal)
          *ph-table* (coerce (loop for k being the hash-keys of (wasm-module-declared-refs m) collect k)
                             'simple-vector))
    (loop for name across *ph-base-names* for i from 0
          do (unless (gethash name *ph-base-index*) (setf (gethash name *ph-base-index*) i)))
    (ph-parse words)
    (unless (= *ph-nbase* (length objects))
      (error "preinit: ~D objects of the image saved, ~D globals" *ph-nbase* (length objects)))
    (setf *ph-mutables* (make-hash-table :test 'equal))
    (let* ((roots (aref *ph-rec* (ph-ref-index *ph-roots*)))
           (mvec (aref *ph-rec* (ph-ref-index (cdr (first roots))))))
      (loop for name in mutables for it in mvec
            do (setf (gethash name *ph-mutables*) (cdr it))))))

;;; The class system's own data: the slot vectors of the classes, the lists
;;; of their superclasses, subclasses, precedence lists, slot definitions
;;; and slot locations, the slot definitions with their names, initforms,
;;; types, initargs, readers and writers, and the forms of default
;;; initargs.  The symbols they refer to are names (of slots, of classes,
;;; of accessors, in forms whose functions the class has compiled), not
;;; functions to call: such a reference keeps a symbol, its value and
;;; properties, but not its function (a file stream's slot PATHNAME, the
;;; slot CLOSE of a directive of FORMAT).  What else a class refers to
;;; (functions, the initial values of shared slots, prototypes) is marked
;;; as any object is.

(defparameter *preinit-data-properties* '(clysm-sys::%struct-description clysm-sys::%struct-slots)
  "Properties of the names of structures whose values are the class
system's data (the names, defaults and types of the slots).")

(defun ph-data-tree (w)
  "The conses of the tree W are the class system's data."
  (let ((c (ph-ref-index w)))
    (when (and c (eq (aref *ph-kind* c) :cons) (aref *ph-rec* c) (not (gethash c *ph-class-data*)))
      (setf (gethash c *ph-class-data*) t)
      (ph-data-tree (ph-rec-word c 0))
      (ph-data-tree (ph-rec-word c 1)))))

(defun ph-class-data-objects ()
  (let ((data (make-hash-table)) (seen (make-hash-table))
        (top (ph-class-of-name (gethash (list :sym t) *ph-base-index*))))
    (labels ((add (i) (when i (setf (gethash i data) t)))
             (spine (w) (mapc #'add (ph-list-conses w)))
             (tree (w) (tree-at (ph-ref-index w)))
             (tree-at (c)
               (when (and c (eq (aref *ph-kind* c) :cons) (aref *ph-rec* c) (not (gethash c data)))
                 (add c)
                 (tree (ph-rec-word c 0))
                 (tree (ph-rec-word c 1))))
             (instance-p (k) (and k (eq (aref *ph-kind* k) :instance) (ph-slots-vector k)))
             (slotd (k)
               (when (instance-p k)
                 (add (ph-slots-vector k))
                 (dolist (s '(1 4 5)) (spine (ph-slot k s)))
                 (tree (ph-slot k 2))
                 (tree (ph-slot k 7))))
             (initargs (w)
               (dolist (c (ph-list-conses w))
                 (add c)
                 (let ((e (ph-ref-index (ph-rec-word c 0))))
                   (when (and e (eq (aref *ph-kind* e) :cons))
                     (let ((parts (ph-list-conses (ph-rec-word c 0))))
                       (mapc #'add parts)
                       (when (second parts) (tree (ph-rec-word (second parts) 0))))))))
             (walk (k)
               (when (and (instance-p k) (not (gethash k seen)))
                 (setf (gethash k seen) t)
                 (add (ph-slots-vector k))
                 (dolist (s '(1 2 3 4 5)) (spine (ph-slot k s)))
                 (dolist (c (ph-list-conses (ph-slot k 8)))
                   (add c)
                   (let ((e (ph-ref-index (ph-rec-word c 0))))
                     (when (and e (eq (aref *ph-kind* e) :cons)) (add e))))
                 ;; (a structure's slots are lists (name default type read-only))
                 (dolist (s '(2 4))
                   (dolist (d (ph-list-items (ph-slot k s)))
                     (if (eq (aref *ph-kind* d) :cons) (tree-at d) (slotd d))))
                 (dolist (s '(9 14)) (initargs (ph-slot k s)))
                 (mapc #'walk (ph-list-items (ph-slot k 5))))))
      (when top (walk top)))
    (when (sb-ext:posix-getenv "CLYSM_PREINIT_STATS")
      (format *error-output* "~&; preinit: ~D objects of the class system's data~%" (hash-table-count data)))
    data))

(defun preinit-shake (m parts after-shake)
  "Shake the module M (built by BUILD-MODULE with the start function of
PARTS, (INIT PROLOGUE RUNTIME-TOPS PROGRAM-TOPS)) to start from the heap that
the runtime's toplevel forms make (see the beginning of this file); then
call AFTER-SHAKE with M, before the objects are written."
  (destructuring-bind (init prologue runtime-tops program-tops) parts
    (let ((image-funcs (mapcar (lambda (f) (cons f (wasm-func-body (find-func m f))))
                               '(:image-objects :image-mutables :set-image-mutables)))
          (words nil) (dropped nil) (drop nil) (seg nil) (start (find-func m :start)))
      (loop
        (block round
          (multiple-value-setq (words dropped)
            (handler-case (ph-run-runtime m init prologue runtime-tops drop)
              ;; (the module is then shaken as it was before)
              (error (e)
                (when (or seg *preinit-through*) (error e))
                (format *error-output* "~&; warning: the runtime's state could not be made when compiling, ~
                                        the module runs its toplevel forms: ~A~%" e)
                (shake-module m :drop-symbols *shake-symbols* :keep-globals '(:symtab :pkgtab :strtab))
                (funcall after-shake m)
                (return-from preinit-shake m))))
          (unless (or (null drop) (subsetp dropped (if (eq drop :none) nil drop)))
            (error "preinit: the methods of ~S were removed, not those of ~S" dropped drop))
          ;; the module the heap was saved by
          (ph-load-heap m words)
          ;; the start function: the objects, then the program's toplevel forms
          (unless seg
            (setf seg (add-data m (make-array 0 :element-type '(unsigned-byte 8))))
            (ph-add-loader m)
            (setf (wasm-func-body start)
                  `(,@init
                    (:i32.const 0) (:i32.const 0) (:i32.const 0) (:memory.init ,seg) (:data.drop ,seg)
                    (:i32.const 0) (:call :image-load-preinit)
                    (:global.get :img-roots) (:ref.cast! :vector) (:i32.const 0) (:array.get :vector)
                    (:call :set-image-mutables)
                    (:global.get :img-roots) ,@(lisp-call-instrs 'clysm-sys::%image-start 1) :drop
                    (:ref.null :vector) (:global.set :img-objs) (:ref.null :eq) (:global.set :img-roots)
                    ,@program-tops)
                  (wasm-func-ref-list start) :unknown))
          ;; (made again once the globals are known)
          (dolist (f '(:image-objects :image-mutables))
            (setf (wasm-func-body (find-func m f)) '((:array.new_fixed :vector 0))))
          (setf (wasm-func-body (find-func m :set-image-mutables)) nil)
          (dolist (f '(:image-objects :image-mutables :set-image-mutables))
            (setf (wasm-func-ref-list (find-func m f)) :unknown))
          ;; shake, with the objects
          (let* ((n (length *ph-kind*))
                 (*ph-state* (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
                 (*ph-match* (make-array n :initial-element nil))
                 (*ph-weak-cells* (make-hash-table))
                 (*ph-pending* (make-hash-table :test 'equal))
                 (*ph-code-reached* (make-hash-table))
                 (*ph-cover* (make-array n :initial-element nil))
                 (*ph-reached* (make-hash-table))
                 (*ph-kept-mutables* (make-hash-table :test 'equal))
                 (*ph-queue* nil)
                 (*ph-kept-symbols* (make-hash-table))
                 (*ph-data-symbols* (make-hash-table))
                 (*ph-class-data* (ph-class-data-objects))
                 (*ph-owner* (and (sb-ext:posix-getenv "CLYSM_PREINIT_STATS") (make-array n :initial-element nil)))
                 (*shake-heap-hook* #'ph-hook))
            (let ((inits (make-hash-table :test 'equal)))
              (loop for g across (wasm-module-globals m) do (setf (gethash (first g) inits) (fourth g)))
              (ph-match-globals inits))
            (when *shake-why* (clrhash *shake-why*))
            (let ((made (shake-module m :keep-globals '(:symtab :pkgtab :strtab) :drop-symbols *shake-symbols*
                                        :verify (lambda () (ph-instantiable-classes dropped)))))
              (when (consp made)
                ;; (instances of some of those classes can be made after
                ;; all: their methods are kept, and the heap made again)
                (when (sb-ext:posix-getenv "CLYSM_PREINIT_STATS")
                  (format *error-output* "~&; preinit: the methods of ~S are kept~%" made))
                (setf drop (or (set-difference dropped made) :none))
                (loop for (f . body) in image-funcs
                      do (setf (wasm-func-body (find-func m f)) body
                               (wasm-func-ref-list (find-func m f)) :unknown))
                (return-from round)))
            (when (sb-ext:posix-getenv "CLYSM_PREINIT_STATS")
              (format *error-output* "~&; preinit: the methods of ~D classes are removed: ~S~%"
                      (length dropped) dropped))
            (funcall after-shake m)
            (let ((octets (ph-emit m)) (body (wasm-func-body start)))
              (setf (aref (wasm-module-data m) seg) octets
                    (wasm-module-memory m) (max 1 (ceiling (length octets) 65536)))
              (setf (wasm-func-body start)
                    (append (subseq body 0 (length init))
                            (list* '(:i32.const 0) '(:i32.const 0) `(:i32.const ,(length octets))
                                   (nthcdr (+ 3 (length init)) body)))
                    (wasm-func-ref-list start) :unknown))
            (return))))))
  m)

(defun ph-add-loader (m)
  "The function :IMAGE-LOAD-PREINIT, which makes the objects: as a heap
image is loaded, from words written as LEB128 numbers."
  (add-func m :img-vword :v-i :locals '(:i32 :i32 :i32) :local-names '(r b sh)
            :body '((:i32.const 0) (:local.set r) (:i32.const 0) (:local.set sh)
                    (:loop again nil
                      (:global.get :img-pos) (:i32.load8_u) (:local.set b)
                      (:global.get :img-pos) (:i32.const 1) :i32.add (:global.set :img-pos)
                      (:local.get r) (:local.get b) (:i32.const 127) :i32.and (:local.get sh) :i32.shl :i32.or
                      (:local.set r)
                      (:local.get sh) (:i32.const 7) :i32.add (:local.set sh)
                      (:local.get b) (:i32.const 128) :i32.and (:br_if again))
                    (:local.get r)))
  (add-func m :img-vref :v-e :locals '(:i32) :local-names '(w)
            :body '((:call :img-vword) (:local.tee w) (:i32.const 1) :i32.and
                    (:if nil nil ((:local.get w) (:i32.const 1) :i32.shr_s :ref.i31 :return))
                    (:local.get w) :i32.eqz
                    (:if nil nil ((:ref.null :eq) :return))
                    (:global.get :img-objs) (:local.get w) (:i32.const 1) :i32.shr_u (:i32.const 1) :i32.sub
                    (:array.get :vector)))
  (add-func m :img-vi64 :v-l :locals '(:i32) :local-names '(lo)
            :body '((:call :img-vword) (:local.set lo)
                    (:call :img-vword) :i64.extend_i32_u (:i64.const 32) :i64.shl
                    (:local.get lo) :i64.extend_i32_u :i64.or))
  (let ((code (loop for x in (img-load-code t)
                    if (and (consp x) (consp (car x))) append x else collect x)))
    (setf code (subst '(:call :img-vword) '(:call :img-word) code :test #'equal)
          code (subst '(:call :img-vref) '(:call :img-ref) code :test #'equal)
          code (subst '(:call :img-vi64) '(:call :img-i64) code :test #'equal))
    (add-func m :image-load-preinit :i-v
              :locals '(:i32 :i32 :i32 :i32 :i32 :i32 :i32 :i32 (:ref :null :string) :eqref
                        (:ref :null :vector) (:ref :null :vector) :i32)
              :local-names '(nbase nheap nrec k end len j w s x chunks c prev)
              :body code)))

(defun ph-emit (m)
  "The octets of the kept objects, for :IMAGE-LOAD-PREINIT of the module M
(shaken), and the functions of its globals (:IMAGE-OBJECTS...)."
  (multiple-value-bind (objects mutables) (image-globals m)
    (let ((findex (make-hash-table :test 'equal)) (nbase (length objects))
          (new (make-array (length *ph-kind*) :initial-element nil))
          (positions (make-hash-table :test 'equal))
          (refs (wasm-module-declared-refs m))
          (heap nil) (next 0) (nil-word nil))
      (loop for name in objects for i from 0
            do (unless (gethash name findex) (setf (gethash name findex) i)))
      (setf nil-word (* 2 (1+ (gethash (list :sym nil) findex))))
      ;; the functions of the globals
      (setf (wasm-func-body (find-func m :image-objects)) (vector-of-globals-code objects)
            (wasm-func-body (find-func m :set-image-mutables))
            `((:local.get 0) (:ref.cast! :vector) (:local.set v)
              ,@(loop for g in mutables for i from 0
                      append `((:local.get v) (:i32.const ,i) (:array.get :vector) (:global.set ,g)))))
      (when (find-func m :image-mutables)
        (setf (wasm-func-body (find-func m :image-mutables)) (vector-of-globals-code mutables)))
      (dolist (f '(:image-objects :image-mutables :set-image-mutables))
        (when (find-func m f) (setf (wasm-func-ref-list (find-func m f)) :unknown)))
      ;; the table of functions: what the kept code takes references of,
      ;; and the functions of the kept closures
      (loop for f across (wasm-module-funcs m)
            do (ph-refs (wasm-func-body f) (lambda (name) (declare-func-ref m name))))
      (loop for g across (wasm-module-globals m)
            do (ph-refs (fourth g) (lambda (name) (declare-func-ref m name))))
      (dotimes (i (length *ph-kind*))
        (when (and (>= i *ph-nbase*) (plusp (aref *ph-state* i)) (eq (aref *ph-kind* i) :closure))
          (dolist (it (aref *ph-imm* i))
            (when (and (eq (car it) :c) (/= (cdr it) #xFFFFFFFF))
              (declare-func-ref m (aref *ph-table* (cdr it)))))))
      (loop for k being the hash-keys of refs for i from 0 do (setf (gethash k positions) i))
      (setf *preinit-table-check* (hash-table-count refs))
      ;; the places of the objects
      (dotimes (i (length *ph-kind*))
        (when (plusp (aref *ph-state* i))
          (if (< i *ph-nbase*)
              (setf (aref new i) (or (gethash (aref *ph-base-names* i) findex)
                                     (error "preinit: the global ~S of a kept object was dropped"
                                            (aref *ph-base-names* i))))
              (progn (setf (aref new i) (+ nbase next)) (incf next) (push i heap)))))
      (setf heap (nreverse heap))
      (let* ((roots-u (aref *ph-rec* (ph-ref-index *ph-roots*)))
             (rehash (loop for it in (aref *ph-rec* (ph-ref-index (cdr (fourth roots-u))))
                           for j = (ph-ref-index (cdr it))
                           when (and j (plusp (aref *ph-state* j))) collect j))
             ;; the new vectors of the roots: the roots, the values of the
             ;; mutable globals, the modules' (none), the tables to rehash
             (r-roots (+ nbase next)) (r-mut (+ r-roots 1)) (r-snaps (+ r-roots 2)) (r-rehash (+ r-roots 3))
             (nrec 0) (prev 0)
             ;; (the vectors of the symbols of packages, without the symbols
             ;; left out)
             (pkg-vectors (let ((h (make-hash-table)))
                            (when *shake-symbols*
                              (dolist (i heap)
                                (when (eq (aref *ph-kind* i) :package)
                                  (dolist (f '(4 5))
                                    (let ((v (ph-ref-index (cdr (nth f (aref *ph-rec* i))))))
                                      (when (and v (eq (aref *ph-kind* v) :vector))
                                        (setf (gethash v h)
                                              (remove-if (lambda (it)
                                                           (let ((j (ph-ref-index (cdr it))))
                                                             (and j (null (aref new j)))))
                                                         (aref *ph-rec* v)))))))))
                            h))
             (objs (make-array 100000 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
             (recs (make-array 100000 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
             (out objs))
        (labels ((word (w)
                   (let ((w (logand w #xFFFFFFFF)))
                     (loop (let ((b (logand w 127)))
                             (setf w (ash w -7))
                             (if (zerop w)
                                 (progn (vector-push-extend b out) (return))
                                 (vector-push-extend (logior b 128) out))))))
                 (ref-word (i) (* 2 (1+ i)))
                 (ref (w)
                   (let ((i (ph-ref-index w)))
                     (cond ((null i) w)
                           ((aref new i) (ref-word (aref new i)))
                           (t (error "preinit: a kept object refers to one dropped (~A ~D)"
                                     (aref *ph-kind* i) i)))))
                 (item (it)
                   (ecase (car it)
                     (:w (word (cdr it)))
                     (:b (dotimes (k 4) (vector-push-extend (ldb (byte 8 (* 8 k)) (cdr it)) out)))
                     (:r (word (ref (cdr it))))
                     (:c (word (if (= (cdr it) #xFFFFFFFF)
                                   #xFFFFFFFF
                                   (or (gethash (aref *ph-table* (cdr it)) positions)
                                       (error "preinit: a function of a closure is not in the table: ~S"
                                              (aref *ph-table* (cdr it))))))))))
          ;; the objects
          (dolist (i heap)
            (word (position (aref *ph-kind* i) +img-kinds+))
            (if (gethash i pkg-vectors)
                (word (length (gethash i pkg-vectors)))
                (mapc #'item (aref *ph-imm* i))))
          (dotimes (k 4) (word (position :vector +img-kinds+))
            (word (case k (0 4) (1 (length mutables)) (2 0) (3 (length rehash)))))
          ;; the records
          (setf out recs)
          (flet ((record (i)
                   (let ((rec (aref *ph-rec* i)))
                     (when (and rec (not (and (< i *ph-nbase*) (aref *ph-match* i))))
                       (incf nrec)
                       (word (- (aref new i) prev))
                       (setf prev (aref new i))
                       (case (aref *ph-kind* i)
                         (:symbol
                          ;; (what a symbol that nothing refers to names
                          ;; is dropped, but a value that is an immediate
                          ;; or the symbol itself, as a keyword's)
                          (destructuring-bind (pkg value fn plist flags &rest name-hash) rec
                            (declare (ignore name-hash))
                            (item pkg)
                            (cond
                              ((ph-symbol-reached-p i) (item value) (item fn) (item plist))
                              ;; (a name in the class system's data)
                              ((gethash i *ph-data-symbols*) (item value) (word 0) (item plist))
                              (t
                                (let ((v (cdr value)))
                                  (word (if (or (null (ph-ref-index v)) (eql (ph-ref-index v) i)) (ref v) 0))
                                  (word 0) (word nil-word))))
                            (item flags)))
                         (:cons
                          (let ((users (gethash i *ph-weak-cells*)))
                            (if (and users (not (ph-user-reached-p users)))
                                (word nil-word)
                                (item (first rec)))
                            (item (second rec))))
                         (t (mapc #'item (or (gethash i pkg-vectors) rec))))))))
            (dotimes (i (length *ph-kind*))
              (when (and (< i *ph-nbase*) (plusp (aref *ph-state* i))) (record i)))
            (dolist (i heap) (record i)))
          (word (- r-roots prev))
          (word (ref-word r-mut)) (word (ref-word r-snaps)) (word nil-word) (word (ref-word r-rehash))
          (word 1)
          ;; (not the object that the cache of identity hashes holds: its
          ;; hash is not saved)
          (loop for g in mutables
                do (word (let ((v (gethash g *ph-mutables*))) (if (and v (not (eq g :ih-key))) (ref v) 0))))
          (word 2)
          (dolist (j rehash) (word (ref-word (aref new j))))
          (incf nrec 3)
          ;; the roots
          (word (ref-word r-roots))
          ;; the header, then the objects and the records
          (setf out (make-array (+ 32 (length objs) (length recs)) :element-type '(unsigned-byte 8)
                                                                      :adjustable t :fill-pointer 0))
          (word +img-magic+) (word +img-preinit-version+) (word nbase) (word (+ next 4)) (word nrec)
          (loop for b across objs do (vector-push-extend b out))
          (loop for b across recs do (vector-push-extend b out))
          (when (sb-ext:posix-getenv "CLYSM_PREINIT_STATS")
            (format *error-output* "~&; preinit: ~D octets, objects ~D, records ~D~%" (length out) (length objs) (length recs))
            (ph-stats))
          (coerce out '(simple-array (unsigned-byte 8) (*))))))))

(defun ph-plist-pairs (i)
  "((indicator-word value-word . cons-of-the-value) ...) of the plist of the symbol I."
  (let ((w (cdr (fourth (aref *ph-rec* i)))) (r nil))
    (loop (let ((c1 (ph-ref-index w)))
            (unless (and c1 (eq (aref *ph-kind* c1) :cons) (aref *ph-rec* c1)) (return (nreverse r)))
            (let ((c2 (ph-ref-index (cdr (second (aref *ph-rec* c1))))))
              (unless (and c2 (eq (aref *ph-kind* c2) :cons) (aref *ph-rec* c2)) (return (nreverse r)))
              (push (list* (cdr (first (aref *ph-rec* c1))) (cdr (first (aref *ph-rec* c2))) c2) r)
              (setf w (cdr (second (aref *ph-rec* c2)))))))))

(defun ph-stats ()
  (format *error-output* "~&; preinit: ~D weak cells, ~S~%" (hash-table-count *ph-weak-cells*)
          (loop for (nil . users) in *preinit-property-users* collect (list users (ph-user-reached-p users))))
  (ph-stats-1)
  (let ((inds (make-hash-table :test 'equal)))
    (dotimes (i *ph-nbase*)
      (when (and (ph-base-symbol i) (ph-symbol-reached-p i) (aref *ph-rec* i))
        (dolist (p (ph-plist-pairs i))
          (let ((k (ph-ref-index (first p))))
            (incf (gethash (if k (aref *ph-base-names* k) (first p)) inds 0))))))
    (maphash (lambda (k v) (format *error-output* ";  indicator ~S ~D~%" k v)) inds))
  ;; (the kept instances by class: generic functions, methods...)
  (let ((by (make-hash-table :test 'equal)))
    (dotimes (i (length *ph-kind*))
      (when (and (plusp (aref *ph-state* i)) (eq (aref *ph-kind* i) :instance))
        (let ((label (let ((*shake-why* t)) (ph-instance-label i))))
          (when (and label (search "GENERIC-FUNCTION" label))
            (format *error-output* ";  kept ~A~%" label))
          (when label (incf (gethash (subseq label 0 (or (position #\Space label) (length label))) by 0))))))
    (maphash (lambda (k v) (format *error-output* ";  instances ~A ~D~%" k v)) by)))

(defun ph-stats-1 ()
  (let ((objs (make-hash-table)) (words (make-hash-table)) (recs (make-hash-table)) (rw (make-hash-table)))
    (dotimes (i (length *ph-kind*))
      (when (plusp (aref *ph-state* i))
        (let ((k (aref *ph-kind* i)))
          (when (>= i *ph-nbase*)
            (incf (gethash k objs 0)) (incf (gethash k words 0) (1+ (length (aref *ph-imm* i)))))
          (when (aref *ph-rec* i)
            (incf (gethash k recs 0)) (incf (gethash k rw 0) (1+ (length (aref *ph-rec* i))))))))
    (format *error-output* "~&; preinit: base ~D, symbols ~D, kept ~D, with records ~D; heap ~D~%"
            *ph-nbase*
            (loop for i below *ph-nbase* count (ph-base-symbol i))
            (loop for i below *ph-nbase* count (and (ph-base-symbol i) (plusp (aref *ph-state* i))))
            (loop for i below *ph-nbase* count (and (ph-base-symbol i) (aref *ph-rec* i)))
            (- (length *ph-kind*) *ph-nbase*))
    (let ((by (make-hash-table)))
      (dotimes (i (length *ph-kind*))
        (when (plusp (aref *ph-state* i))
          (let ((o (if (< i *ph-nbase*) i (aref *ph-owner* i))))
            (incf (gethash o by 0) (+ (if (< i *ph-nbase*) 0 (1+ (length (aref *ph-imm* i))))
                                      (if (aref *ph-rec* i) (1+ (length (aref *ph-rec* i))) 0))))))
      (let ((l (sort (loop for k being the hash-keys of by using (hash-value v) collect (cons k v)) #'> :key #'cdr)))
        (let ((cats (make-hash-table :test 'equal)))
          (loop for (o . v) in l
                do (incf (gethash (cond ((null o) :none)
                                        ((>= o *ph-nbase*) :heap)
                                        ((consp (aref *ph-base-names* o)) (list (car (aref *ph-base-names* o)) (aref *ph-kind* o)))
                                        (t (aref *ph-base-names* o)))
                                  cats 0) v))
          (maphash (lambda (k v) (format *error-output* ";  cat ~S ~D~%" k v)) cats))
        (loop for (o . v) in l repeat 60
              do (format *error-output* ";   ~8D ~S~%" v (if o (if (< o *ph-nbase*) (aref *ph-base-names* o) o) :none)))))
    (format *error-output* "~&; preinit: kind objects words records words~%")
    (dolist (k +img-kinds+)
      (when (or (gethash k objs) (gethash k recs))
        (format *error-output* ";   ~12A ~8D ~8D ~8D ~8D~%" k (gethash k objs 0) (gethash k words 0)
                (gethash k recs 0) (gethash k rw 0))))))

;;; The methods of classes that no instance of can be made.  The runtime's
;;; toplevel forms define methods on classes that a program may never make
;;; an instance of (Gray streams, the streams of subprocesses): such a
;;; method is kept with its generic function, and holds its code and what
;;; that reaches.  When the heap is saved, %PREINIT-DROP-METHODS
;;; (runtime/image.lisp) removes the methods that specialize a parameter on
;;; one of a set of classes (or a subclass), first those of the classes of
;;; metaclass STANDARD-CLASS that no metaobject is an instance of, other
;;; than conditions.  The module is then shaken, and PH-INSTANTIABLE-CLASSES
;;; checks that the kept code and objects cannot make an instance of any of
;;; those classes; if they can, the heap is made again with the methods of
;;; those classes kept.  An instance of a standard class is made by
;;; MAKE-INSTANCE, ALLOCATE-INSTANCE, CHANGE-CLASS or MAKE-CONDITION of the
;;; class or its name, so a class (or a subclass) can have instances when
;;; code or a constant names it (as a DEFCLASS of the program names its
;;; superclasses), when a kept object is an instance of it, or when a kept
;;; object or global refers to the class or its name other than as the
;;; class system does (the class's name, superclasses, subclasses and
;;; precedence list, the property of its name that FIND-CLASS reads, the
;;; cell that compiled TYPEP tests read, the table of the methods of each
;;; specializer).  A class whose name a program makes at run time (INTERN,
;;; READ) is as a function called by such a name: not there for it.

(defun ph-rec-word (i k) (cdr (nth k (aref *ph-rec* i))))

(defun ph-layout (i)
  (and (member (aref *ph-kind* i) '(:instance :sinst)) (aref *ph-rec* i) (ph-ref-index (ph-rec-word i 0))))

(defun ph-slots-vector (i)
  (and (eq (aref *ph-kind* i) :instance) (aref *ph-rec* i) (ph-ref-index (ph-rec-word i 1))))

(defun ph-slot (i k)
  "The word of the slot K of the instance I (an :INSTANCE), or NIL."
  (let ((v (ph-slots-vector i))) (and v (ph-rec-word v k))))

(defun ph-list-conses (w)
  "The conses of the list W."
  (let ((r nil))
    (loop (let ((c (ph-ref-index w)))
            (unless (and c (eq (aref *ph-kind* c) :cons) (aref *ph-rec* c)) (return (nreverse r)))
            (push c r)
            (setf w (ph-rec-word c 1))))))

(defun ph-list-items (w)
  "The indexes of the objects in the list W."
  (loop for c in (ph-list-conses w) for j = (ph-ref-index (ph-rec-word c 0)) when j collect j))

(defun ph-symbol-value-index (sym)
  (let ((i (gethash (list :sym sym) *ph-base-index*)))
    (and i (aref *ph-rec* i) (ph-ref-index (ph-rec-word i 1)))))

(defun ph-class-of-name (si)
  "The class that the symbol SI names (its property %CLASS), or NIL."
  (dolist (p (ph-plist-pairs si) nil)
    (let ((ind (ph-ref-index (first p))))
      (when (and ind (eq (ph-base-symbol ind) 'clysm-sys::%class))
        (return (ph-ref-index (second p)))))))

(defun ph-instantiable-classes (names)
  "Those of the classes NAMES (symbols) of which the kept code and objects
can make an instance (see above)."
  (when names
    (let ((system (make-hash-table)) (watch (make-hash-table)) (made (make-hash-table)) (classes nil)
          (top (ph-class-of-name (gethash (list :sym t) *ph-base-index*))))
      (labels ((sys (i) (when i (setf (gethash i system) t)))
               (subclasses (k) (ph-list-items (ph-slot k 5)))
               (walk (k seen)
                 (unless (gethash k seen)
                   (setf (gethash k seen) t)
                   (sys (ph-slots-vector k))
                   (dolist (s (list 1 3 5))
                     (mapc #'sys (ph-list-conses (ph-slot k s))))
                   (let ((v (ph-ref-index (or (ph-slot k 20) 0)))) (sys v))
                   (dolist (c (subclasses k)) (walk c seen))))
               (note (i why)
                 (dolist (k (gethash i watch)) (push why (gethash k made)))))
        (unless top (return-from ph-instantiable-classes names))
        ;; (the class system's own references)
        (walk top (make-hash-table))
        (dotimes (i *ph-nbase*)
          (when (and (ph-base-symbol i) (aref *ph-rec* i))
            (let ((spine (ph-list-conses (cdr (fourth (aref *ph-rec* i))))))
              (mapc #'sys spine)
              (dolist (p (ph-plist-pairs i))
                (let ((ind (ph-ref-index (first p))))
                  (when (and ind (eq (ph-base-symbol ind) 'clysm-sys::%type-cell))
                    (sys (ph-ref-index (second p)))))))))
        (let ((ht (ph-symbol-value-index 'clysm-sys::*specializer-methods*)))
          (when ht
            (sys ht)
            ;; (a hash table is a structure of field slots, runtime/hash.lisp)
            (let ((sv (ph-slots-vector ht)))
              (sys sv)
              (dolist (x (cond (sv (aref *ph-rec* sv))
                               ((eq (aref *ph-kind* ht) :sinst) (cdr (aref *ph-rec* ht)))))
                (let ((j (and (eq (car x) :r) (ph-ref-index (cdr x)))))
                  (when (and j (eq (aref *ph-kind* j) :vector)) (sys j)))))))
        ;; the classes and their subclasses, and their names
        (dolist (name names)
          (let* ((si (gethash (list :sym name) *ph-base-index*))
                 (k (and si (ph-class-of-name si))))
            (if (null k)
                (push (list name :not-found) (gethash name made))
                (labels ((add (c)
                           (unless (member name (gethash c watch))
                             (push name (gethash c watch))
                             (let ((ci (ph-ref-index (or (ph-slot c 0) 0))))
                               (when ci
                                 (push name (gethash ci watch))
                                 (when (gethash ci *ph-code-reached*)
                                   (push (list (ph-base-symbol ci) :named) (gethash name made)))))
                             (mapc #'add (subclasses c)))))
                  (push name classes)
                  (add k)))))
        ;; what refers to them
        (dotimes (o (length *ph-kind*))
          (let ((kind (aref *ph-kind* o)))
            (when (if (eq kind :symbol) (ph-symbol-reached-p o) (= (aref *ph-state* o) 2))
              (let ((l (ph-layout o)))
                (when (and l (gethash l watch)) (note l (list :instance (ph-node o)))))
              (unless (or (gethash o system) (eq kind :package))
                (dolist (x (append (aref *ph-imm* o)
                                   (let ((rec (aref *ph-rec* o)))
                                     (if (and rec (eq kind :symbol)) (subseq rec 1 4) rec))))
                  (when (eq (car x) :r)
                    (let ((j (ph-ref-index (cdr x))))
                      (when (and j (/= j o) (not (eql j (ph-layout o))))
                        (note j (list :referred (ph-node j) :by (ph-node o)))))))))))
        (maphash (lambda (name v)
                   (declare (ignore v))
                   (let ((j (ph-ref-index (gethash name *ph-mutables*))))
                     (when j (note j (list :referred (ph-node j) :by (list :global name))))))
                 *ph-kept-mutables*)
        (let ((r (loop for k being the hash-keys of made collect k)))
          (when (and r (sb-ext:posix-getenv "CLYSM_PREINIT_STATS"))
            (maphash (lambda (k v) (format *error-output* "~&; preinit: ~S can have instances: ~S~%" k (first v)))
                     made))
          r)))))

(unless (sb-ext:posix-getenv "CLYSM_NO_PREINIT")
  (setf *preinit-function* 'preinit-shake))

