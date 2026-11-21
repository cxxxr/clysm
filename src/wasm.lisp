;;;; wasm.lisp — WebAssembly binary encoder (GC, EH(exnref), tail calls)
;;;;
;;;; A module is described symbolically: types, functions, globals and tags are
;;;; referred to by name (any EQL key), and labels in structured instructions are
;;;; symbolic and resolved to relative depths at encoding time.

(in-package :clysm-wasm)

;;; ------------------------------------------------------------
;;; Byte buffers / LEB128
;;; ------------------------------------------------------------

(defun make-buf () (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
;; (in Clysm a call: the copies of VECTOR-PUSH-EXTEND's paths for every kind
;; of vector, a hundred in EMIT-INSTR, made the compiler's code larger, which
;; V8 compiles when it first runs)
#-clysm (declaim (inline emit-byte))
(defun emit-byte (buf b) (vector-push-extend (ldb (byte 8 0) b) buf))
(defun emit-bytes (buf bytes)
  ;; (at once: the bodies of functions and sections are copied into the module)
  (let* ((start (fill-pointer buf)) (end (+ start (length bytes))))
    (when (> end (array-dimension buf 0))
      (adjust-array buf (max end (* 2 (array-dimension buf 0)))))
    (setf (fill-pointer buf) end)
    (replace buf bytes :start1 start)))

(defun emit-u32 (buf n)
  (assert (>= n 0))
  (loop
    (let ((b (ldb (byte 7 0) n)))
      (setf n (ash n -7))
      (if (zerop n)
          (progn (emit-byte buf b) (return))
          (emit-byte buf (logior b #x80))))))

(defun emit-sleb (buf n)
  (loop
    (let ((b (ldb (byte 7 0) n)))
      (setf n (ash n -7))
      (if (or (and (zerop n) (zerop (logand b #x40)))
              (and (= n -1) (not (zerop (logand b #x40)))))
          (progn (emit-byte buf b) (return))
          (emit-byte buf (logior b #x80))))))

(defun utf8-octets (string)
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for ch across string
          for c = (char-code ch)
          do (cond ((< c #x80) (vector-push-extend c out))
                   ((< c #x800)
                    (vector-push-extend (logior #xC0 (ash c -6)) out)
                    (vector-push-extend (logior #x80 (logand c #x3F)) out))
                   ((< c #x10000)
                    (vector-push-extend (logior #xE0 (ash c -12)) out)
                    (vector-push-extend (logior #x80 (logand (ash c -6) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand c #x3F)) out))
                   (t
                    (vector-push-extend (logior #xF0 (ash c -18)) out)
                    (vector-push-extend (logior #x80 (logand (ash c -12) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand (ash c -6) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand c #x3F)) out))))
    out))

(defun emit-name (buf string)
  (let ((octets (utf8-octets string)))
    (emit-u32 buf (length octets))
    (emit-bytes buf octets)))

(defun float-bits (x mantissa-bits exponent-bits)
  "IEEE 754 bit pattern of the float X, computed portably."
  (let ((bias (1- (ash 1 (1- exponent-bits))))
        (sign-bit (ash 1 (+ mantissa-bits exponent-bits))))
    (if (zerop x)
        (if (minusp (float-sign x)) sign-bit 0)
        (multiple-value-bind (mant expo sign) (integer-decode-float x)
          ;; normalize mantissa to MANTISSA-BITS+1 bits
          (let ((len (integer-length mant)))
            (setf expo (+ expo (- len (1+ mantissa-bits))))
            (setf mant (ash mant (- (1+ mantissa-bits) len))))
          (let ((biased (+ expo mantissa-bits bias)))
            (when (<= biased 0)
              ;; subnormal
              (setf mant (ash mant (1- biased)) biased 0))
            (logior (if (minusp sign) sign-bit 0)
                    (ash biased mantissa-bits)
                    (ldb (byte mantissa-bits 0) mant)))))))

(defun emit-vec-buf (buf sub)
  (emit-u32 buf (length sub))
  (emit-bytes buf sub))

;;; ------------------------------------------------------------
;;; Module model
;;; ------------------------------------------------------------

(defvar *func-source* nil "(FILE . LINE) of the functions added now (ADD-FUNC), or NIL.")
(defvar *func-segment* 0
  "The segment of a compilation unit (src/live.lisp) whose code the functions added now are.")

(defstruct (wasm-func (:constructor %make-wasm-func))
  name type params-names locals local-names body export import-module import-name display
  (source *func-source*)
  (segment *func-segment*)
  ;; what the body refers to (WASM-FUNC-REFS), once it has been asked
  (ref-list :unknown))

(defun body-refs (body)
  "What the code BODY refers to, in the order of first occurrence, each once:
(:call . NAME) for a function it calls or takes the reference of, (:global
. NAME) for a global it reads or sets, (:cast . TYPE) for a type it casts to
(not :null)."
  (let ((seen nil) (refs nil) (small nil) (nsmall 0))
    (declare (fixnum nsmall))
    (labels ((note (kind name)
               ;; (a short list of what was seen, then a table)
               (let ((bit (case kind (:call 1) (:global 2) (t 4))))
                 (flet ((add () (push (cons kind name) refs)))
                   (if seen
                       (let ((old (gethash name seen 0)))
                         (when (zerop (logand old bit))
                           (setf (gethash name seen) (logior old bit))
                           (add)))
                       (let ((cell (assoc name small :test #'equal)))
                         (cond ((null cell)
                                (push (cons name bit) small) (incf nsmall) (add)
                                (when (> nsmall 16)
                                  (setq seen (make-hash-table :test 'equal))
                                  (dolist (c small) (setf (gethash (car c) seen) (cdr c)))))
                               ((zerop (logand (cdr cell) bit))
                                (setf (cdr cell) (logior (cdr cell) bit))
                                (add))))))))
             (walk (x)
               (when (consp x)
                 (case (car x)
                   ((:call :return_call :ref.func) (when (consp (cdr x)) (note :call (second x))))
                   ((:global.get :global.set) (when (consp (cdr x)) (note :global (second x))))
                   (:ref.cast (when (and (consp (cdr x)) (not (eq (second x) :null)))
                                (note :cast (second x)))))
                 (do ((l x (cdr l))) ((atom l)) (when (consp (car l)) (walk (car l)))))))
      (walk body)
      (nreverse refs))))

(defun wasm-func-refs (f)
  "BODY-REFS of the body of the function F, computed once."
  (let ((r (wasm-func-ref-list f)))
    (if (eq r :unknown)
        (setf (wasm-func-ref-list f) (body-refs (wasm-func-body f)))
        r)))

(defstruct wasm-module
  (types (make-array 0 :adjustable t :fill-pointer 0))   ; vector of (name . def)
  (type-table (make-hash-table :test 'equal))
  (imports (make-array 0 :adjustable t :fill-pointer 0)) ; imported funcs (wasm-func)
  (funcs (make-array 0 :adjustable t :fill-pointer 0))   ; defined funcs
  (func-table (make-hash-table :test 'equal))            ; name -> wasm-func
  (func-index-table (make-hash-table :test 'equal))
  (globals (make-array 0 :adjustable t :fill-pointer 0)) ; (name type mutable init)
  (global-table (make-hash-table :test 'equal))
  (tags (make-array 0 :adjustable t :fill-pointer 0))    ; (name type)
  (tag-table (make-hash-table :test 'equal))
  (exports nil)
  (data (make-array 0 :adjustable t :fill-pointer 0))    ; passive data segments (octet vectors)
  (declared-refs (make-hash-table :test 'equal))
  (start nil)
  (frozen nil)
  (base-types nil)                                       ; size of the first rec group
  (global-imports (make-array 0 :adjustable t :fill-pointer 0)) ; (name type mutable module field)
  (tag-imports (make-array 0 :adjustable t :fill-pointer 0))    ; (name type module field)
  (nonnull-globals (make-hash-table :test 'equal))
  (memory nil)                                           ; initial pages of a memory of its own
  (memory-import nil)                                    ; (module field min) of an imported memory
  ;; the table of functions (runtime/image.lisp): (:define) in the image, which
  ;; exports it, or (:import module field) in a module compiled in it; the
  ;; ref.func'd functions are placed in it from CODE-BASE (a global, or 0)
  (table nil)
  (code-base nil)
  ;; the image's globals whose objects a heap image fills (conses, simple
  ;; vectors, symbols): (name . initializer made empty), for the
  ;; clysm.image-defaults section
  (image-defaults nil)
  ;; the functions that V8 compiles with the module, in background threads
  ;; (a module's toplevel forms, which run once each when it is loaded):
  ;; its compilationHints section, see withEagerHints in host/common.mjs
  (eager-funcs nil)
  (cast-rewrites nil))                                  ; heap type -> function replacing ref.cast     ; nullable globals read as non-null

(defun add-type (m name def)
  "DEF is (:func params results) | (:struct fields) | (:array field) optionally
wrapped as (:sub supers final-p def). Field = (storage-type mutable-p)."
  (let ((idx (fill-pointer (wasm-module-types m))))
    (vector-push-extend (cons name def) (wasm-module-types m))
    (setf (gethash name (wasm-module-type-table m)) idx)
    idx))

(defun type-index (m name)
  (if (integerp name) name
      (or (gethash name (wasm-module-type-table m))
          (error "wasm: unknown type ~S" name))))

(defun add-import-func (m name type module field)
  (assert (zerop (fill-pointer (wasm-module-funcs m))) () "imports must precede functions")
  (let ((f (%make-wasm-func :name name :type type :import-module module :import-name field)))
    (setf (gethash name (wasm-module-func-index-table m)) (fill-pointer (wasm-module-imports m)))
    (vector-push-extend f (wasm-module-imports m))
    (setf (gethash name (wasm-module-func-table m)) f)
    f))

(defun add-func (m name type &key locals local-names body export param-names display)
  (let ((f (%make-wasm-func :name name :type type :locals locals :local-names local-names :display display
                            :params-names param-names :body body :export export)))
    (setf (gethash name (wasm-module-func-index-table m))
          (+ (fill-pointer (wasm-module-imports m)) (fill-pointer (wasm-module-funcs m))))
    (vector-push-extend f (wasm-module-funcs m))
    (setf (gethash name (wasm-module-func-table m)) f)
    (when export (add-export m export :func name))
    f))

(defun find-func (m name) (gethash name (wasm-module-func-table m)))
(defun func-defined-p (m name) (nth-value 1 (gethash name (wasm-module-func-table m))))

(defun func-index (m name)
  (or (gethash name (wasm-module-func-index-table m))
      (error "wasm: unknown function ~S" name)))

(defun order-functions-by-use (m)
  "Put the defined functions of M that are referred to the most first: a
function's index is an LEB128 number in each call and ref.func (one octet
below 128, two below 16384), and the closures' code structs and the calls
of helpers name a few functions (the entries for a wrong number of
arguments, :APPLY-GENERIC, the casts) thousands of times."
  (let ((uses (make-hash-table :test 'equal)))
    (labels ((walk (x)
               (when (consp x)
                 (when (and (member (car x) '(:call :return_call :ref.func)) (consp (cdr x)))
                   (incf (gethash (second x) uses 0)))
                 (do ((l x (cdr l))) ((atom l)) (walk (car l))))))
      (loop for f across (wasm-module-funcs m) do (walk (wasm-func-body f)))
      (loop for g across (wasm-module-globals m) do (walk (fourth g))))
    ;; (only those that get an index below 128, one octet: the others keep
    ;; their order, the order of the source, which gzip compresses better)
    (let* ((funcs (coerce (wasm-module-funcs m) 'list))
           (room (max 0 (- 128 (fill-pointer (wasm-module-imports m)))))
           (top (subseq (stable-sort (copy-list funcs) #'>
                                     :key (lambda (f) (gethash (wasm-func-name f) uses 0)))
                        0 (min room (length funcs))))
           (topset (make-hash-table :test 'eq)))
      (dolist (f top) (setf (gethash f topset) t))
      (setf (fill-pointer (wasm-module-funcs m)) 0)
      (dolist (f top) (vector-push-extend f (wasm-module-funcs m)))
      (dolist (f funcs) (unless (gethash f topset) (vector-push-extend f (wasm-module-funcs m)))))
    (let ((index (wasm-module-func-index-table m)) (i (fill-pointer (wasm-module-imports m))))
      (loop for f across (wasm-module-funcs m)
            do (setf (gethash (wasm-func-name f) index) i)
               (incf i)))
    ;; the same for the globals whose initializers refer to no other defined
    ;; global (a global may refer only to those before it): the symbols
    ;; (NIL, T), the state of the runtime (:MV-COUNT, :DYN...)
    (let ((guses (make-hash-table :test 'equal)) (defined (make-hash-table :test 'equal)))
      (labels ((walk (x)
                 (when (consp x)
                   (when (and (member (car x) '(:global.get :global.set)) (consp (cdr x)))
                     (incf (gethash (second x) guses 0)))
                   (do ((l x (cdr l))) ((atom l)) (walk (car l)))))
               (free-p (init)
                 (labels ((refers (x)
                            (and (consp x)
                                 (or (and (eq (car x) :global.get) (consp (cdr x)) (gethash (second x) defined))
                                     (some #'refers x)))))
                   (not (some #'refers init)))))
        (loop for f across (wasm-module-funcs m) do (walk (wasm-func-body f)))
        (loop for g across (wasm-module-globals m) do (walk (fourth g)) (setf (gethash (first g) defined) t))
        (let* ((globals (coerce (wasm-module-globals m) 'list))
               (room (max 0 (- 128 (fill-pointer (wasm-module-global-imports m)))))
               (candidates (remove-if-not (lambda (g) (free-p (fourth g))) globals))
               (top (subseq (stable-sort (copy-list candidates) #'> :key (lambda (g) (gethash (first g) guses 0)))
                            0 (min room (length candidates))))
               (topset (make-hash-table :test 'eq)))
          (dolist (g top) (setf (gethash g topset) t))
          (setf (fill-pointer (wasm-module-globals m)) 0)
          (dolist (g top) (vector-push-extend g (wasm-module-globals m)))
          (dolist (g globals) (unless (gethash g topset) (vector-push-extend g (wasm-module-globals m))))
          (let ((table (wasm-module-global-table m)) (i (fill-pointer (wasm-module-global-imports m))))
            (loop for g across (wasm-module-globals m)
                  do (setf (gethash (first g) table) i)
                     (incf i))))))
    m))

(defun remove-unreachable-functions (m &optional known)
  "Drop the defined functions that cannot be reached from the exports, the
start function and the global initializers (e.g. runtime helpers that a
small module does not use), and renumber the others.  KNOWN: an EQUAL hash
table of names known to be reached, with all that they refer to (they are
not walked again)."
  (let ((reached (or known (make-hash-table :test 'equal))) (queue nil)
        (rewrites (wasm-module-cast-rewrites m)))
    (labels ((mark (name)
               (unless (gethash name reached)
                 (setf (gethash name reached) t)
                 (let ((f (find-func m name)))
                   (when (and f (not (wasm-func-import-module f))) (push f queue)))))
             (walk (x)
               (when (consp x)
                 (case (car x)
                   ((:call :return_call :ref.func) (when (consp (cdr x)) (mark (second x))))
                   (:ref.cast (when (and (consp (cdr x)) (not (eq (second x) :null)))
                                (let ((fn (cdr (assoc (second x) rewrites))))
                                  (when fn (mark fn))))))
                 (do ((l x (cdr l))) ((atom l)) (walk (car l))))))
      (loop for f across (wasm-module-funcs m)
            when (wasm-func-export f) do (mark (wasm-func-name f)))
      (loop for (nil kind ref) in (wasm-module-exports m)
            when (eq kind :func) do (mark ref))
      (when (wasm-module-start m) (mark (wasm-module-start m)))
      (loop for g across (wasm-module-globals m) do (walk (fourth g)))
      (loop while queue do (walk (wasm-func-body (pop queue))))
      (flet ((keep (vector)
               (let ((kept (remove-if-not (lambda (f) (gethash (wasm-func-name f) reached))
                                          (coerce vector 'list))))
                 (setf (fill-pointer vector) 0)
                 (dolist (f kept) (vector-push-extend f vector)))))
        (keep (wasm-module-imports m))
        (keep (wasm-module-funcs m)))
      ;; renumber: imports first, then the defined functions
      (let ((index (wasm-module-func-index-table m)) (table (wasm-module-func-table m)) (i 0))
        (clrhash index) (clrhash table)
        (loop for f across (wasm-module-imports m)
              do (setf (gethash (wasm-func-name f) index) i (gethash (wasm-func-name f) table) f)
                 (incf i))
        (loop for f across (wasm-module-funcs m)
              do (setf (gethash (wasm-func-name f) index) i (gethash (wasm-func-name f) table) f)
                 (incf i))))
    m))

(defvar *shake-why* nil
  "NIL, or an EQUAL hash table that SHAKE-MODULE fills with what refers to
each thing it keeps: the keys are (:FUNC . NAME), (:GLOBAL . NAME) and
(:SYMBOL . SYMBOL), and the value of one is a list of those (or :ROOT, for
the exports, the start function and the globals kept whatever refers to
them) whose code refers to it, the first to reach it last.  See
WRITE-SHAKE-WHY.")
(defvar *shake-walking* :root)

(defun shake-edge (to)
  (let ((preds (gethash to *shake-why*)))
    (unless (equal (car preds) *shake-walking*)
      (setf (gethash to *shake-why*) (cons *shake-walking* preds)))))

(defvar *shake-heap-hook* nil
  "NIL, or a function that SHAKE-MODULE calls with :GLOBAL and the name of
each global it keeps, and with :SYMBOL and each symbol it finds referred
to, when first found (and with :CODE-SYMBOL whenever code or the initializer
of a global refers to it) (src/preinit.lisp: the objects of the heap that a
global or a symbol refers to).  While it runs, *SHAKE-MARK* is a function
of :FUNC, :GLOBAL or :SYMBOL and a name, that keeps that too.")
(defvar *shake-mark* nil)

(defun shake-module (m &key (keep-globals '(:symtab :pkgtab)) drop-symbols)
  "Drop what a whole program cannot reach: the functions and globals not
reached from the exports and the start function, and the definitions of
global functions whose symbols nothing refers to.  A definition is a call
of a function named (:install SYMBOL N) (see CG-DEFUN), which is followed
only once SYMBOL is reached: by a (:global.get (:sym SYMBOL)) in reached
code or in the initializer of a reached global other than KEEP-GLOBALS (the
table of all symbols, which INTERN uses, keeps the symbols but not their
functions).  Calls of the installs that are not reached are removed.  With
DROP-SYMBOLS, the globals of the symbols that nothing reached are dropped too
(the caller then makes the table of symbols again, of those left): a symbol
stays when something refers to it, or when the value of an install of it
that is not reached is used (not dropped at once)."
  (let ((funcs (make-hash-table :test 'equal)) (globals (make-hash-table :test 'equal))
        (symbols (make-hash-table :test 'eq)) (pending (make-hash-table :test 'eq))
        (queue nil) (queue-end nil) (gqueue nil)
        (rewrites (wasm-module-cast-rewrites m)))
    (labels ((install-p (name) (and (consp name) (eq (car name) :install)))
             (mark-func (name)
               (when *shake-why* (shake-edge (cons :func name)))
               (unless (gethash name funcs)
                 (setf (gethash name funcs) t)
                 (let ((f (find-func m name)))
                   (when (and f (not (wasm-func-import-module f)))
                     ;; (first in, first out: what reached a function first
                     ;; is then on a shortest chain, for *SHAKE-WHY*)
                     (let ((cell (list f)))
                       (if queue (setf (cdr queue-end) cell) (setf queue cell))
                       (setf queue-end cell))))))
             (mark-symbol (sym)
               (when *shake-why* (shake-edge (cons :symbol sym)))
               (unless (gethash sym symbols)
                 (setf (gethash sym symbols) t)
                 (let ((*shake-walking* (cons :symbol sym)))
                   (dolist (name (gethash sym pending)) (mark-func name))
                   (when *shake-heap-hook* (funcall *shake-heap-hook* :symbol sym)))))
             (mark-global (name)
               (when (and (consp name) (eq (car name) :sym))
                 (mark-symbol (second name))
                 (when *shake-heap-hook* (funcall *shake-heap-hook* :code-symbol (second name))))
               (when *shake-why* (shake-edge (cons :global name)))
               (unless (gethash name globals)
                 (setf (gethash name globals) t)
                 (push name gqueue)
                 (when *shake-heap-hook*
                   (let ((*shake-walking* (cons :global name)))
                     (funcall *shake-heap-hook* :global name)))))
             (walk (x)
               (when (consp x)
                 (case (car x)
                   ((:call :return_call :ref.func)
                    (when (consp (cdr x))
                      (let ((name (second x)))
                        (if (install-p name)
                            (if (gethash (second name) symbols)
                                (let ((*shake-walking* (cons :symbol (second name))))
                                  (mark-func name))
                                (pushnew name (gethash (second name) pending) :test #'equal))
                            (mark-func name)))))
                   ;; ((:GLOBAL.GET G :WEAK) of SYS:%WEAK-SYMBOL: the
                   ;; symbol's global is kept, but not what it names)
                   ((:global.get :global.set)
                    (when (consp (cdr x))
                      (if (eq (third x) :weak)
                          (unless (gethash (second x) globals)
                            (setf (gethash (second x) globals) t)
                            (push (second x) gqueue)
                            (when *shake-heap-hook*
                              (let ((*shake-walking* (cons :global (second x))))
                                (funcall *shake-heap-hook* :global (second x)))))
                          (mark-global (second x)))))
                   (:ref.cast (when (and (consp (cdr x)) (not (eq (second x) :null)))
                                (let ((fn (cdr (assoc (second x) rewrites))))
                                  (when fn (mark-func fn))))))
                 (do ((l x (cdr l))) ((atom l))
                   (walk (car l))
                   ;; the value of an install, if not reached, is its symbol
                   (when (and drop-symbols (install-call-p (car l)) (not (eq (cadr l) :drop)))
                     (let ((name (list :sym (second (second (car l))))))
                       (unless (gethash name globals)
                         (setf (gethash name globals) t)
                         (push name gqueue)))))))
             (install-call-p (x) (and (consp x) (eq (car x) :call) (install-p (second x)))))
      (setf *shake-mark* (lambda (kind name)
                           (ecase kind
                             (:func (mark-func name))
                             (:global (mark-global name))
                             (:symbol (mark-symbol name)))))
      (loop for (nil kind ref) in (wasm-module-exports m)
            do (case kind (:func (mark-func ref)) (:global (mark-global ref))))
      (loop for f across (wasm-module-funcs m) when (wasm-func-export f) do (mark-func (wasm-func-name f)))
      (when (wasm-module-start m) (mark-func (wasm-module-start m)))
      ;; every symbol stays (INTERN finds it), without making its function reached
      (dolist (g keep-globals) (when (global-defined-p m g) (setf (gethash g globals) t)))
      (unless drop-symbols
        (loop for g across (wasm-module-globals m)
              when (and (consp (first g)) (eq (car (first g)) :sym))
                do (setf (gethash (first g) globals) t) (push (first g) gqueue)
                   (when *shake-why* (shake-edge (cons :global (first g))))
                   (when *shake-heap-hook*
                     (let ((*shake-walking* (cons :global (first g))))
                       (funcall *shake-heap-hook* :global (first g))))))
      (loop while (or queue gqueue)
            do (cond (queue (let* ((f (pop queue)) (*shake-walking* (cons :func (wasm-func-name f))))
                              (walk (wasm-func-body f))))
                     (t (let* ((name (pop gqueue))
                               (g (find name (wasm-module-globals m) :key #'first :test #'equal))
                               (*shake-walking* (cons :global name)))
                          (when g (walk (fourth g)))))))
      ;; the calls of installs that were not reached: the symbol, their value
      ;; (an install whose value is dropped at once goes with the drop)
      (labels ((strip (body)
                 (let ((out nil))
                   (do ((l body (cdr l))) ((null l) (nreverse out))
                     (let ((x (car l)))
                       (cond ((and (consp x) (eq (car x) :call) (install-p (second x))
                                   (not (gethash (second x) funcs)))
                              (if (eq (cadr l) :drop)
                                  (setf l (cdr l))
                                  (push `(:global.get (:sym ,(second (second x)))) out)))
                             ((and (consp x) (member (car x) '(:block :loop :if :try_table)))
                              (push (strip-structured x) out))
                             (t (push x out)))))))
               (strip-structured (x)
                 (ecase (car x)
                   ((:block :loop) (list* (first x) (second x) (third x) (strip (cdddr x))))
                   (:if (destructuring-bind (op label bt then &optional (else nil elsep)) x
                          (if elsep
                              (list op label bt (strip then) (strip else))
                              (list op label bt (strip then)))))
                   (:try_table (list* (first x) (second x) (third x) (fourth x) (strip (nthcdr 4 x)))))))
        (loop for f across (wasm-module-funcs m)
              when (gethash (wasm-func-name f) funcs)
                do (setf (wasm-func-body f) (strip (wasm-func-body f))
                         (wasm-func-ref-list f) :unknown)))
      ;; drop functions and globals, and renumber
      (let ((kept (remove-if-not (lambda (f) (gethash (wasm-func-name f) funcs))
                                 (coerce (wasm-module-funcs m) 'list))))
        (setf (fill-pointer (wasm-module-funcs m)) 0)
        (dolist (f kept) (vector-push-extend f (wasm-module-funcs m))))
      (let ((kept (remove-if-not (lambda (g) (gethash (first g) globals))
                                 (coerce (wasm-module-globals m) 'list))))
        (setf (fill-pointer (wasm-module-globals m)) 0)
        (dolist (g kept) (vector-push-extend g (wasm-module-globals m))))
      (let ((index (wasm-module-func-index-table m)) (table (wasm-module-func-table m)) (i 0))
        (clrhash index) (clrhash table)
        (loop for f across (wasm-module-imports m)
              do (setf (gethash (wasm-func-name f) index) i (gethash (wasm-func-name f) table) f)
                 (incf i))
        (loop for f across (wasm-module-funcs m)
              do (setf (gethash (wasm-func-name f) index) i (gethash (wasm-func-name f) table) f)
                 (incf i)))
      (let ((table (wasm-module-global-table m)))
        (clrhash table)
        (loop for g across (wasm-module-global-imports m) for i from 0
              do (setf (gethash (first g) table) i))
        (loop for g across (wasm-module-globals m)
              for i from (fill-pointer (wasm-module-global-imports m))
              do (setf (gethash (first g) table) i)))
      (let ((refs (wasm-module-declared-refs m)))
        (loop for k in (loop for k being the hash-keys of refs collect k)
              unless (gethash k funcs) do (remhash k refs))))
    m))

(defun declare-func-ref (m name)
  (setf (gethash name (wasm-module-declared-refs m)) t))

(defun add-import-global (m name type mutable module field)
  (let ((idx (fill-pointer (wasm-module-global-imports m))))
    (vector-push-extend (list name type mutable module field) (wasm-module-global-imports m))
    (setf (gethash name (wasm-module-global-table m)) idx)
    idx))

(defun add-import-tag (m name type module field)
  (let ((idx (fill-pointer (wasm-module-tag-imports m))))
    (vector-push-extend (list name type module field) (wasm-module-tag-imports m))
    (setf (gethash name (wasm-module-tag-table m)) idx)
    idx))

(defun add-global (m name type mutable init)
  (let ((idx (+ (fill-pointer (wasm-module-global-imports m)) (fill-pointer (wasm-module-globals m)))))
    (vector-push-extend (list name type mutable init) (wasm-module-globals m))
    (setf (gethash name (wasm-module-global-table m)) idx)
    idx))

(defun global-defined-p (m name) (nth-value 1 (gethash name (wasm-module-global-table m))))

(defun global-index (m name)
  (or (gethash name (wasm-module-global-table m))
      (error "wasm: unknown global ~S" name)))

(defun add-tag (m name type)
  (let ((idx (+ (fill-pointer (wasm-module-tag-imports m)) (fill-pointer (wasm-module-tags m)))))
    (vector-push-extend (list name type) (wasm-module-tags m))
    (setf (gethash name (wasm-module-tag-table m)) idx)
    idx))

(defun tag-index (m name)
  (or (gethash name (wasm-module-tag-table m)) (error "wasm: unknown tag ~S" name)))

(defun add-export (m name kind ref)
  (push (list name kind ref) (wasm-module-exports m)))

(defun add-data (m octets)
  (let ((idx (fill-pointer (wasm-module-data m))))
    (vector-push-extend octets (wasm-module-data m))
    idx))

;;; ------------------------------------------------------------
;;; Types
;;; ------------------------------------------------------------

(defparameter *abs-heap-types*
  '((:nofunc . #x73) (:noextern . #x72) (:none . #x71) (:func . #x70) (:extern . #x6F)
    (:any . #x6E) (:eq . #x6D) (:i31 . #x6C) (:struct . #x6B) (:array . #x6A)
    (:exn . #x69) (:noexn . #x74)))

(defparameter *shorthand-reftypes*
  '((:funcref . #x70) (:externref . #x6F) (:anyref . #x6E) (:eqref . #x6D)
    (:i31ref . #x6C) (:structref . #x6B) (:arrayref . #x6A) (:exnref . #x69)
    (:nullref . #x71) (:nullfuncref . #x73) (:nullexternref . #x72)))

(defun emit-heap-type (m buf ht)
  (let ((abs (assoc ht *abs-heap-types*)))
    (if abs
        (emit-byte buf (cdr abs))
        (emit-sleb buf (type-index m ht)))))

(defun emit-valtype (m buf vt)
  (cond ((eq vt :i32) (emit-byte buf #x7F))
        ((eq vt :i64) (emit-byte buf #x7E))
        ((eq vt :f32) (emit-byte buf #x7D))
        ((eq vt :f64) (emit-byte buf #x7C))
        ((eq vt :v128) (emit-byte buf #x7B))
        ((assoc vt *shorthand-reftypes*) (emit-byte buf (cdr (assoc vt *shorthand-reftypes*))))
        ((and (consp vt) (eq (first vt) :ref))
         ;; (:ref ht) or (:ref :null ht)
         (if (eq (second vt) :null)
             (progn (emit-byte buf #x63) (emit-heap-type m buf (third vt)))
             (progn (emit-byte buf #x64) (emit-heap-type m buf (second vt)))))
        (t (error "wasm: bad valtype ~S" vt))))

(defun emit-storage-type (m buf st)
  (case st
    (:i8 (emit-byte buf #x78))
    (:i16 (emit-byte buf #x77))
    (t (emit-valtype m buf st))))

(defun emit-field (m buf field)
  ;; FIELD is (storage-type mutable-p)
  (destructuring-bind (st &optional mut) field
    (emit-storage-type m buf st)
    (emit-byte buf (if mut 1 0))))

(defun emit-comptype (m buf def)
  (ecase (first def)
    (:func
     (destructuring-bind (params results) (rest def)
       (emit-byte buf #x60)
       (emit-u32 buf (length params))
       (dolist (p params) (emit-valtype m buf p))
       (emit-u32 buf (length results))
       (dolist (r results) (emit-valtype m buf r))))
    (:struct
     (emit-byte buf #x5F)
     (let ((fields (second def)))
       (emit-u32 buf (length fields))
       (dolist (f fields) (emit-field m buf f))))
    (:array
     (emit-byte buf #x5E)
     (emit-field m buf (second def)))))

(defun emit-subtype (m buf def)
  (if (eq (first def) :sub)
      (destructuring-bind (supers final-p comp) (rest def)
        (emit-byte buf (if final-p #x4F #x50))
        (emit-u32 buf (length supers))
        (dolist (s supers) (emit-u32 buf (type-index m s)))
        (emit-comptype m buf comp))
      (emit-comptype m buf def)))

;;; ------------------------------------------------------------
;;; Instructions
;;; ------------------------------------------------------------

(defparameter *simple-ops* (make-hash-table))
(defparameter *gc-ops* (make-hash-table))
(defparameter *fc-ops* (make-hash-table))

(macrolet ((simple (&rest pairs)
             `(progn ,@(loop for (name code) on pairs by #'cddr
                             collect `(setf (gethash ,name *simple-ops*) ,code)))))
  (simple :unreachable #x00 :nop #x01 :return #x0F :drop #x1A :select #x1B
          :throw_ref #x0A
          :ref.is_null #xD1 :ref.eq #xD3 :ref.as_non_null #xD4
          :i32.eqz #x45 :i32.eq #x46 :i32.ne #x47 :i32.lt_s #x48 :i32.lt_u #x49
          :i32.gt_s #x4A :i32.gt_u #x4B :i32.le_s #x4C :i32.le_u #x4D :i32.ge_s #x4E :i32.ge_u #x4F
          :i64.eqz #x50 :i64.eq #x51 :i64.ne #x52 :i64.lt_s #x53 :i64.lt_u #x54 :i64.gt_s #x55
          :i64.gt_u #x56 :i64.le_s #x57 :i64.le_u #x58 :i64.ge_s #x59 :i64.ge_u #x5A
          :f32.eq #x5B :f32.ne #x5C :f32.lt #x5D :f32.gt #x5E :f32.le #x5F :f32.ge #x60
          :f64.eq #x61 :f64.ne #x62 :f64.lt #x63 :f64.gt #x64 :f64.le #x65 :f64.ge #x66
          :i32.clz #x67 :i32.ctz #x68 :i32.popcnt #x69 :i32.add #x6A :i32.sub #x6B :i32.mul #x6C
          :i32.div_s #x6D :i32.div_u #x6E :i32.rem_s #x6F :i32.rem_u #x70 :i32.and #x71
          :i32.or #x72 :i32.xor #x73 :i32.shl #x74 :i32.shr_s #x75 :i32.shr_u #x76
          :i32.rotl #x77 :i32.rotr #x78
          :i64.clz #x79 :i64.ctz #x7A :i64.popcnt #x7B :i64.add #x7C :i64.sub #x7D :i64.mul #x7E
          :i64.div_s #x7F :i64.div_u #x80 :i64.rem_s #x81 :i64.rem_u #x82 :i64.and #x83
          :i64.or #x84 :i64.xor #x85 :i64.shl #x86 :i64.shr_s #x87 :i64.shr_u #x88
          :i64.rotl #x89 :i64.rotr #x8A
          :f32.abs #x8B :f32.neg #x8C :f32.ceil #x8D :f32.floor #x8E :f32.trunc #x8F
          :f32.nearest #x90 :f32.sqrt #x91 :f32.add #x92 :f32.sub #x93 :f32.mul #x94
          :f32.div #x95 :f32.min #x96 :f32.max #x97 :f32.copysign #x98
          :f64.abs #x99 :f64.neg #x9A :f64.ceil #x9B :f64.floor #x9C :f64.trunc #x9D
          :f64.nearest #x9E :f64.sqrt #x9F :f64.add #xA0 :f64.sub #xA1 :f64.mul #xA2
          :f64.div #xA3 :f64.min #xA4 :f64.max #xA5 :f64.copysign #xA6
          :i32.wrap_i64 #xA7 :i32.trunc_f32_s #xA8 :i32.trunc_f32_u #xA9
          :i32.trunc_f64_s #xAA :i32.trunc_f64_u #xAB
          :i64.extend_i32_s #xAC :i64.extend_i32_u #xAD :i64.trunc_f32_s #xAE :i64.trunc_f32_u #xAF
          :i64.trunc_f64_s #xB0 :i64.trunc_f64_u #xB1
          :f32.convert_i32_s #xB2 :f32.convert_i32_u #xB3 :f32.convert_i64_s #xB4
          :f32.convert_i64_u #xB5 :f32.demote_f64 #xB6
          :f64.convert_i32_s #xB7 :f64.convert_i32_u #xB8 :f64.convert_i64_s #xB9
          :f64.convert_i64_u #xBA :f64.promote_f32 #xBB
          :i32.reinterpret_f32 #xBC :i64.reinterpret_f64 #xBD :f32.reinterpret_i32 #xBE
          :f64.reinterpret_i64 #xBF
          :i32.extend8_s #xC0 :i32.extend16_s #xC1 :i64.extend8_s #xC2 :i64.extend16_s #xC3
          :i64.extend32_s #xC4))

(macrolet ((gc (&rest pairs)
             `(progn ,@(loop for (name code) on pairs by #'cddr
                             collect `(setf (gethash ,name *gc-ops*) ,code)))))
  ;; ops without immediates
  (gc :array.len 15 :any.convert_extern 26 :extern.convert_any 27 :ref.i31 28
      :i31.get_s 29 :i31.get_u 30))

(macrolet ((fc (&rest pairs)
             `(progn ,@(loop for (name code) on pairs by #'cddr
                             collect `(setf (gethash ,name *fc-ops*) ,code)))))
  (fc :i32.trunc_sat_f32_s 0 :i32.trunc_sat_f32_u 1 :i32.trunc_sat_f64_s 2 :i32.trunc_sat_f64_u 3
      :i64.trunc_sat_f32_s 4 :i64.trunc_sat_f32_u 5 :i64.trunc_sat_f64_s 6 :i64.trunc_sat_f64_u 7))

(defvar *labels*)
(defvar *current-func* nil)
(defvar *module*)
(defvar *locals-map*)
(defvar *local-remap* nil "During ENCODE-FUNCTION-BODY: COALESCE-LOCALS's map, or NIL.")

(defvar *cast-fail-labels* nil
  "During ENCODE-FUNCTION-BODY: (type . label) for the blocks around the
function's body that its checked casts of TYPE branch to when they fail.")
(defvar *cast-fail-count* 0 "The number of those blocks.")

(defun label-depth (label)
  (if (integerp label)
      ;; (a depth counted from the code: the blocks of the failing casts are
      ;; between the function's own label and the code's labels)
      (if (and (plusp *cast-fail-count*) (>= label (- (length *labels*) *cast-fail-count*)))
          (+ label *cast-fail-count*)
          label)
      (let ((pos (position label *labels*)))
        (unless pos (error "wasm: unknown label ~S (in scope: ~S)" label *labels*))
        pos)))

(defun emit-blocktype (buf bt)
  (cond ((null bt) (emit-byte buf #x40))
        ((and (consp bt) (eq (first bt) :type)) (emit-sleb buf (type-index *module* (second bt))))
        (t (emit-valtype *module* buf bt))))

(defun local-index (x)
  (let ((i (if (integerp x) x
               (or (gethash x *locals-map*) (error "wasm: unknown local ~S" x)))))
    (if *local-remap* (aref *local-remap* i) i)))

(defvar *branch-hints* nil
  "The branch hints of the function being encoded: (offset . value), latest first.")

(defvar *op-table* nil
  "The operators without immediates of *SIMPLE-OPS*, *GC-OPS* and *FC-OPS*
in one table: op -> code, or (prefix . code).")

(defun op-table ()
  (or *op-table*
      (let ((h (make-hash-table :test 'eq)))
        (maphash (lambda (k v) (setf (gethash k h) v)) *simple-ops*)
        (maphash (lambda (k v) (setf (gethash k h) (cons #xFB v))) *gc-ops*)
        (maphash (lambda (k v) (setf (gethash k h) (cons #xFC v))) *fc-ops*)
        (setf *op-table* h))))

(defun emit-instr (buf ins)
  (let ((m *module*))
    (let ((op (if (consp ins) (first ins) ins)) (args (if (consp ins) (rest ins) nil)))
      ;; (:hint 0) / (:hint 1): the IF or BR_IF that follows is unlikely / likely
      ;; to be taken (the branch hinting section)
      (when (eq op :hint)
        (push (cons (length buf) (first args)) *branch-hints*)
        (return-from emit-instr))
      ;; (one lookup for the operators without immediates)
      (let ((code (gethash op (op-table))))
        (when code
          (if (consp code)
              (progn (emit-byte buf (car code)) (emit-u32 buf (cdr code)))
              (emit-byte buf code))
          (return-from emit-instr)))
      (ecase op
        ;; structured: (:block label bt . body)
        ((:block :loop)
         (destructuring-bind (label bt &rest body) args
           (emit-byte buf (if (eq op :block) #x02 #x03))
           (emit-blocktype buf bt)
           ;; (the checked casts in a loop fail in place, see EMIT-FUNCTION-CODE)
           (let ((*labels* (cons label *labels*))
                 (*cast-fail-labels* (if (eq op :loop) nil *cast-fail-labels*)))
             (dolist (i body) (emit-instr buf i)))
           (emit-byte buf #x0B)))
        ;; (:if label bt (then...) (else...))
        (:if
         (destructuring-bind (label bt then &optional else) args
           (emit-byte buf #x04)
           (emit-blocktype buf bt)
           (let ((*labels* (cons label *labels*)))
             (dolist (i then) (emit-instr buf i))
             (when else
               (emit-byte buf #x05)
               (dolist (i else) (emit-instr buf i))))
           (emit-byte buf #x0B)))
        ;; (:try_table label bt (catches...) . body)
        (:try_table
         (destructuring-bind (label bt catches &rest body) args
           (emit-byte buf #x1F)
           (emit-blocktype buf bt)
           (emit-u32 buf (length catches))
           (dolist (c catches)
             (ecase (first c)
               (:catch (emit-byte buf 0) (emit-u32 buf (tag-index m (second c)))
                (emit-u32 buf (label-depth (third c))))
               (:catch_ref (emit-byte buf 1) (emit-u32 buf (tag-index m (second c)))
                (emit-u32 buf (label-depth (third c))))
               (:catch_all (emit-byte buf 2) (emit-u32 buf (label-depth (second c))))
               (:catch_all_ref (emit-byte buf 3) (emit-u32 buf (label-depth (second c))))))
           ;; (the checked casts inside fail in place: the error they
           ;; signal is to be caught here)
           (let ((*labels* (cons label *labels*)) (*cast-fail-labels* nil))
             (dolist (i body) (emit-instr buf i)))
           (emit-byte buf #x0B)))
        (:br (emit-byte buf #x0C) (emit-u32 buf (label-depth (first args))))
        (:br_if (emit-byte buf #x0D) (emit-u32 buf (label-depth (first args))))
        (:br_table
         (emit-byte buf #x0E)
         (let ((labels (butlast args)) (default (car (last args))))
           (emit-u32 buf (length labels))
           (dolist (l labels) (emit-u32 buf (label-depth l)))
           (emit-u32 buf (label-depth default))))
        (:br_on_null (emit-byte buf #xD5) (emit-u32 buf (label-depth (first args))))
        (:br_on_non_null (emit-byte buf #xD6) (emit-u32 buf (label-depth (first args))))
        (:call (emit-byte buf #x10) (emit-u32 buf (func-index m (first args))))
        (:return_call (emit-byte buf #x12) (emit-u32 buf (func-index m (first args))))
        (:call_ref (emit-byte buf #x14) (emit-u32 buf (type-index m (first args))))
        (:return_call_ref (emit-byte buf #x15) (emit-u32 buf (type-index m (first args))))
        (:throw (emit-byte buf #x08) (emit-u32 buf (tag-index m (first args))))
        (:local.get (emit-byte buf #x20) (emit-u32 buf (local-index (first args))))
        (:local.set (emit-byte buf #x21) (emit-u32 buf (local-index (first args))))
        (:local.tee (emit-byte buf #x22) (emit-u32 buf (local-index (first args))))
        (:global.get (emit-byte buf #x23) (emit-u32 buf (global-index m (first args)))
         (when (gethash (first args) (wasm-module-nonnull-globals m)) (emit-byte buf #xD4)))
        (:global.set (emit-byte buf #x24) (emit-u32 buf (global-index m (first args))))
        ;; the table of functions (the one table, index 0)
        (:table.get (emit-byte buf #x25) (emit-u32 buf 0))
        ;; linear memory (the one memory, index 0): (:i32.load16_u offset) etc.
        ((:i32.load :i32.load8_u :i32.load16_u :i32.store :i32.store8 :i32.store16)
         (emit-byte buf (ecase op (:i32.load #x28) (:i32.load8_u #x2D) (:i32.load16_u #x2F)
                          (:i32.store #x36) (:i32.store8 #x3A) (:i32.store16 #x3B)))
         (emit-u32 buf (ecase op ((:i32.load :i32.store) 2) ((:i32.load8_u :i32.store8) 0)
                         ((:i32.load16_u :i32.store16) 1)))
         (emit-u32 buf (or (first args) 0)))
        (:memory.size (emit-byte buf #x3F) (emit-byte buf 0))
        (:memory.grow (emit-byte buf #x40) (emit-byte buf 0))
        ;; (:memory.init SEGMENT), (:data.drop SEGMENT): a passive data segment
        (:memory.init (emit-byte buf #xFC) (emit-u32 buf 8) (emit-u32 buf (first args)) (emit-byte buf 0))
        (:data.drop (emit-byte buf #xFC) (emit-u32 buf 9) (emit-u32 buf (first args)))
        (:i32.const (emit-byte buf #x41) (emit-sleb buf (let ((v (first args)))
                                                           (if (>= v #x80000000) (- v #x100000000) v))))
        (:i64.const (emit-byte buf #x42) (emit-sleb buf (let ((v (first args)))
                                                           (if (>= v #x8000000000000000) (- v #x10000000000000000) v))))
        (:f64.const (emit-byte buf #x44)
         (let ((bits (let ((v (first args)))
                       (if (integerp v) v (float-bits (coerce v 'double-float) 52 11)))))
           (dotimes (i 8) (emit-byte buf (ldb (byte 8 (* 8 i)) bits)))))
        (:f32.const (emit-byte buf #x43)
         (let ((bits (let ((v (first args)))
                       (if (integerp v) v (float-bits (coerce v 'single-float) 23 8)))))
           (dotimes (i 4) (emit-byte buf (ldb (byte 8 (* 8 i)) bits)))))
        (:ref.null (emit-byte buf #xD0) (emit-heap-type m buf (first args)))
        (:ref.func (emit-byte buf #xD2) (declare-func-ref m (first args))
         (emit-u32 buf (func-index m (first args))))
        ;; GC with immediates
        ((:struct.new :struct.new_default)
         (emit-byte buf #xFB) (emit-u32 buf (if (eq op :struct.new) 0 1))
         (emit-u32 buf (type-index m (first args))))
        ((:struct.get :struct.get_s :struct.get_u :struct.set)
         (emit-byte buf #xFB)
         (emit-u32 buf (ecase op (:struct.get 2) (:struct.get_s 3) (:struct.get_u 4) (:struct.set 5)))
         (emit-u32 buf (type-index m (first args)))
         (emit-u32 buf (second args)))
        ((:array.new :array.new_default :array.get :array.get_s :array.get_u :array.set :array.fill)
         (emit-byte buf #xFB)
         (emit-u32 buf (ecase op (:array.new 6) (:array.new_default 7) (:array.get 11)
                         (:array.get_s 12) (:array.get_u 13) (:array.set 14) (:array.fill 16)))
         (emit-u32 buf (type-index m (first args))))
        (:array.new_fixed
         (emit-byte buf #xFB) (emit-u32 buf 8)
         (emit-u32 buf (type-index m (first args))) (emit-u32 buf (second args)))
        ((:array.new_data :array.init_data)
         (emit-byte buf #xFB) (emit-u32 buf (if (eq op :array.new_data) 9 18))
         (emit-u32 buf (type-index m (first args))) (emit-u32 buf (second args)))
        (:array.copy
         (emit-byte buf #xFB) (emit-u32 buf 17)
         (emit-u32 buf (type-index m (first args))) (emit-u32 buf (type-index m (second args))))
        (:ref.cast!
         ;; unchecked cast (the type is already known): a plain ref.cast
         (emit-byte buf #xFB) (emit-u32 buf 22) (emit-heap-type m buf (first args)))
        ((:ref.test :ref.cast)
         ;; (:ref.test ht) / (:ref.test :null ht)
         ;; checked casts: a non-null ref.cast to a rewritten type calls a
         ;; function that signals a Lisp error instead of trapping
         (let ((fn (and (eq op :ref.cast) (not (eq (first args) :null))
                        (cdr (assoc (first args) (wasm-module-cast-rewrites m))))))
           (let ((fail (and fn (cdr (assoc (first args) *cast-fail-labels*)))))
             (when fail
               ;; br_on_cast_fail to the block of the type around the body,
               ;; which calls FN (ENCODE-FUNCTION-BODY)
               (emit-byte buf #xFB) (emit-u32 buf 25) (emit-byte buf 1) (emit-u32 buf (label-depth fail))
               (emit-heap-type m buf :eq) (emit-heap-type m buf (first args))
               (return-from emit-instr)))
           (when (and fn (not (equal fn (wasm-func-name *current-func*))))
             ;; block (param eqref) (result (ref T)); br_on_cast 0 eqref (ref T); call FN; unreachable; end
             ;; (the call only when the cast fails; nothing is live after it)
             (emit-byte buf #x02) (emit-sleb buf (type-index m (list :cast-bt (first args))))
             (emit-byte buf #xFB) (emit-u32 buf 24) (emit-byte buf 1) (emit-u32 buf 0)
             (emit-heap-type m buf :eq) (emit-heap-type m buf (first args))
             (emit-byte buf #x10) (emit-u32 buf (func-index m fn))
             (emit-byte buf #x00)
             (emit-byte buf #x0B)
             (return-from emit-instr)))
         (emit-byte buf #xFB)
         (let ((nullable (eq (first args) :null)))
           (emit-u32 buf (if (eq op :ref.test) (if nullable 21 20) (if nullable 23 22)))
           (emit-heap-type m buf (if nullable (second args) (first args)))))
        ((:br_on_cast :br_on_cast_fail)
         ;; (:br_on_cast label from-type to-type) types as valtypes (ref ...)
         (destructuring-bind (label from to) args
           (emit-byte buf #xFB) (emit-u32 buf (if (eq op :br_on_cast) 24 25))
           (multiple-value-bind (n1 h1) (reftype-parts from)
             (multiple-value-bind (n2 h2) (reftype-parts to)
               (emit-byte buf (logior (if n1 1 0) (if n2 2 0)))
               (emit-u32 buf (label-depth label))
               (emit-heap-type m buf h1)
               (emit-heap-type m buf h2)))))))))

(defun reftype-parts (vt)
  "Returns (values nullable-p heap-type)."
  (cond ((consp vt) (if (eq (second vt) :null) (values t (third vt)) (values nil (second vt))))
        (t (let ((e (assoc vt '((:eqref . :eq) (:anyref . :any) (:funcref . :func) (:externref . :extern)
                                (:i31ref . :i31) (:structref . :struct) (:arrayref . :array)
                                (:exnref . :exn) (:nullref . :none)))))
             (unless e (error "wasm: not a reftype ~S" vt))
             (values t (cdr e))))))

(defun emit-expr (buf instrs)
  (let ((*labels* nil))
    (dolist (i instrs) (emit-instr buf i))
    (emit-byte buf #x0B)))

(defun checked-cast-types (body m self)
  "The heap types of the checked casts in the instructions BODY of the
function named SELF (see EMIT-INSTR) outside loops and try_tables, in the
order of their first cast."
  (let ((rewrites (wasm-module-cast-rewrites m)) (types nil))
    (labels ((walk (instrs)
               (dolist (x instrs)
                 (when (consp x)
                   (case (car x)
                     (:ref.cast
                      (let ((fn (cdr (assoc (second x) rewrites))))
                        (when (and fn (not (equal fn self)) (not (member (second x) types)))
                          (push (second x) types))))
                     (:block (walk (cdddr x)))
                     (:if (walk (fourth x)) (walk (fifth x))))))))
      (walk body))
    (nreverse types)))

(defun emit-function-code (buf instrs m self)
  "The code INSTRS of a function: the checked casts that fail branch to a
block of their type around it, which calls the function that signals the
TYPE-ERROR (one call per type in the function, and nothing of the failure
in the code of the casts: a br_on_cast_fail).  Those in a loop or a
try_table fail in place (EMIT-INSTR): a branch out of the loop from each of
its casts made V8's code of the loops slower (bench/programs.lisp 2%), and
the error must be signalled inside the try_table that may catch it."
  (let* ((types (checked-cast-types instrs m self))
         (*cast-fail-labels* (mapcar (lambda (type) (cons type (list :cast-fail type))) types))
         (*cast-fail-count* (length types))
         (*labels* (reverse (mapcar #'cdr *cast-fail-labels*))))
    (dolist (type types)
      (declare (ignore type))
      (emit-byte buf #x02) (emit-valtype m buf :eqref))
    (dolist (i instrs) (emit-instr buf i))
    (when types
      (emit-byte buf #x0F)                ; return
      (dolist (type (reverse types))
        (emit-byte buf #x0B)
        (emit-byte buf #x10) (emit-u32 buf (func-index m (cdr (assoc type (wasm-module-cast-rewrites m)))))
        (emit-byte buf #x00)))           ; unreachable
    (emit-byte buf #x0B)))

;;; ------------------------------------------------------------
;;; Sections
;;; ------------------------------------------------------------

(defun emit-section (out id body)
  (emit-byte out id)
  (emit-u32 out (length body))
  (emit-bytes out body))

(defun func-type-def (m f)
  (cdr (aref (wasm-module-types m) (type-index m (wasm-func-type f)))))

;;; Locals shared by variables whose lives do not overlap.  V8's baseline
;;; compiler (Liftoff), which compiles every function the first time it is
;;; called, spends at every branch time in the number of locals: the
;;; compiler's EMIT-INSTR, with 2038 locals, took 31 ms.  A local is given
;;; the range of positions (instructions numbered in order) from its first to
;;; its last use, widened to a whole loop that the range enters or leaves (a
;;; value may go round it); locals of the same type whose ranges do not meet
;;; share one.  A local that may be read before it is set (its default
;;; value), as Wasm's validation of non-nullable locals sees it (a set counts
;;; until the end of its block), keeps a local of its own, as parameters do.

(defun coalesce-locals (body nparams types names)
  "A vector mapping the indexes of the locals of a function (parameters
first: NPARAMS of them, then locals of TYPES, named by NAMES as in
LOCAL-INDEX) to new indexes, and the types of the new locals; NIL when
nothing would change."
  (let* ((nlocals (length types))
         (n (+ nparams nlocals))
         (first (make-array n :initial-element nil))
         (last (make-array n :initial-element nil))
         (pinned (make-array n :initial-element nil))
         (assigned (make-array n :initial-element nil))
         (trail nil)
         (loops nil)
         (pos 0))
    (when (< nlocals 2) (return-from coalesce-locals nil))
    (dotimes (i nparams) (setf (aref assigned i) t))
    (labels ((index (x) (if (integerp x) x (gethash x names)))
             (note (i)
               (unless (aref first i) (setf (aref first i) pos))
               (setf (aref last i) pos))
             (scoped (instrs)
               (let ((mark trail))
                 (walk-list instrs)
                 (loop until (eq trail mark)
                       do (setf (aref assigned (pop trail)) nil))))
             (walk-list (instrs) (dolist (x instrs) (walk x)))
             (walk (ins)
               (incf pos)
               (when (consp ins)
                 (case (car ins)
                   (:local.get
                    (let ((i (index (second ins))))
                      (when i
                        (note i)
                        (unless (aref assigned i) (setf (aref pinned i) t)))))
                   ((:local.set :local.tee)
                    (let ((i (index (second ins))))
                      (when i
                        (note i)
                        (unless (aref assigned i)
                          (setf (aref assigned i) t)
                          (push i trail)))))
                   ((:block :try_table)
                    (scoped (if (eq (car ins) :block) (cdddr ins) (cddddr ins)))
                    (incf pos))
                   (:loop
                    (let ((start pos))
                      (scoped (cdddr ins))
                      (incf pos)
                      (push (cons start pos) loops)))
                   (:if
                    (scoped (fourth ins))
                    (incf pos)
                    (scoped (fifth ins))
                    (incf pos))))))
      (walk-list body))
    (dotimes (i nparams) (setf (aref pinned i) t))
    ;; ranges widened to the loops they enter or leave, inner loops first
    (setq loops (sort loops #'< :key (lambda (l) (- (cdr l) (car l)))))
    (dotimes (i n)
      (let ((s (aref first i)) (e (aref last i)))
        (when (and s (not (aref pinned i)))
          (dolist (l loops)
            (let ((ls (car l)) (le (cdr l)))
              (when (and (<= s le) (>= e ls)          ; they meet
                         (or (< s ls) (> e le)))     ; but the range is not inside
                (setq s (min s ls) e (max e le)))))
          (setf (aref first i) s (aref last i) e))))
    ;; the new locals: each pinned or unused local keeps one; the others take
    ;; the free local of their type that ended first, by start
    (let* ((map (make-array n))
           (new-types (make-array 0 :adjustable t :fill-pointer 0))
           (free (make-hash-table :test 'equal))   ; type -> ((end . new-index) ...)
           (order (sort (loop for i from nparams below n
                              when (and (aref first i) (not (aref pinned i))) collect i)
                        #'< :key (lambda (i) (aref first i)))))
      (dotimes (i nparams) (setf (aref map i) i))
      (flet ((fresh (type)
               (vector-push-extend type new-types)
               (+ nparams (1- (fill-pointer new-types)))))
        (loop for i from nparams below n
              for type in types
              do (cond ((null (aref first i)) (setf (aref map i) -1))   ; (never used)
                       ((aref pinned i) (setf (aref map i) (fresh type)))))
        (let ((type-of (coerce types 'vector)))
          (dolist (i order)
            (let* ((type (aref type-of (- i nparams)))
                   (pool (gethash type free))
                   (slot (and pool (< (car (first pool)) (aref first i)) (pop pool))))
              (let ((new (if slot (cdr slot) (fresh type))))
                (setf (aref map i) new)
                ;; (kept sorted by end)
                (setf (gethash type free)
                      (merge 'list (list (cons (aref last i) new)) pool #'< :key #'car)))))))
      (if (= (fill-pointer new-types) nlocals)
          nil
          ;; the new locals grouped by type (fewer runs in the local declarations)
          (let* ((count (fill-pointer new-types))
                 (kinds nil))
            (dotimes (j count) (pushnew (aref new-types j) kinds :test #'equal))
            (let ((perm (make-array count)) (next nparams) (sorted nil))
              (dolist (k (nreverse kinds))
                (dotimes (j count)
                  (when (equal (aref new-types j) k)
                    (setf (aref perm j) next)
                    (incf next)
                    (push k sorted))))
              (loop for i from nparams below n
                    for j = (aref map i)
                    when (>= j nparams) do (setf (aref map i) (aref perm (- j nparams))))
              (values map (nreverse sorted))))))))

(defun encode-function-body (m f)
  (let* ((*current-func* f)
         (*branch-hints* nil)
         (buf (make-buf))
         (*locals-map* (make-hash-table :test 'equal))
         (def (func-type-def m f))
         (params (second (if (eq (first def) :sub) (fourth def) def)))
         (nparams (length params))
         (locals (wasm-func-locals f))
         (*local-remap* nil))
    (loop for n in (wasm-func-params-names f) for i from 0
          when n do (setf (gethash n *locals-map*) i))
    (loop for n in (wasm-func-local-names f) for i from nparams
          when n do (setf (gethash n *locals-map*) i))
    (multiple-value-bind (map new-types)
        (coalesce-locals (wasm-func-body f) nparams locals *locals-map*)
      (when map (setq *local-remap* map locals new-types)))
    ;; locals: run-length encode
    (let ((groups nil))
      (dolist (lt locals)
        (if (and groups (equal (cdar groups) lt))
            (incf (caar groups))
            (push (cons 1 lt) groups)))
      (setf groups (nreverse groups))
      (emit-u32 buf (length groups))
      (dolist (g groups)
        (emit-u32 buf (car g))
        (emit-valtype m buf (cdr g))))
    (handler-bind ((error (lambda (c)
                            (format *error-output* "~&;; wasm encode error in function ~S: ~A~%"
                                    (wasm-func-name f) c))))
      (emit-function-code buf (wasm-func-body f) m (wasm-func-name f)))
    (values buf (nreverse *branch-hints*))))

(defvar *type-section-cache* nil
  "(base-count types section): the type section last encoded, for its types.")
(defvar *name-octets* (make-hash-table :test 'equal)
  "Function names that are not strings -> the octets of their printed form.")

;;; V8 scales the inlining budget of the optimizing tier by the share of
;;; small functions (bodies under 50 octets) in a module: none at a quarter
;;; or less, the whole budget at a half or more (BudgetScaleFactor,
;;; src/wasm/inlining-tree.h; a function may then grow by a factor of 5 up to
;;; 7000 octets, else by 2 up to 700).  A whole program has the runtime's many
;;; small functions; a module made in the image (a form, a file given to
;;; COMPILE-FILE) has mostly large ones (a function's entries and typed
;;; copies), so the engine inlined almost nothing in it: TAK's three
;;; recursive calls of its typed copy were inlined in a program and not in a
;;; script.  Such a module gets as many filler functions (never called, four
;;; octets each) as it needs to be half small.
(defun small-function-fillers (bodies)
  (let ((n 0) (small 0))
    (dolist (b bodies) (incf n) (when (< (length b) 50) (incf small)))
    (max 0 (- n (* 2 small)))))

(defvar *func-body-sizes* nil
  "NIL, or an EQUAL hash table that ENCODE-MODULE fills with the size in octets
of the body of each function (by name).")

(defun encode-module (m)
  (let ((out (make-buf))
        (*module* m))
    (emit-bytes out #(#x00 #x61 #x73 #x6D #x01 #x00 #x00 #x00))
    ;; Encode function bodies first, so that ref.func declarations are collected.
    (let* ((hints nil)
           (bodies (let ((i (length (wasm-module-imports m))) (r nil))
                     (loop for f across (wasm-module-funcs m)
                           do (multiple-value-bind (body h) (encode-function-body m f)
                                (when *func-body-sizes*
                                  (setf (gethash (wasm-func-name f) *func-body-sizes*) (length body)))
                                (push body r)
                                (when h (push (cons i h) hints))
                                (incf i)))
                     (setf hints (nreverse hints))
                     (nreverse r)))
           (fillers (small-function-fillers bodies))
           ;; the globals (before the table, whose size counts the functions
           ;; that their initializers refer to)
           (globals-section
             (when (plusp (length (wasm-module-globals m)))
               (let ((b (make-buf)))
                 (emit-u32 b (length (wasm-module-globals m)))
                 (loop for (nil type mutable init) across (wasm-module-globals m)
                       do (emit-valtype m b type)
                          (emit-byte b (if mutable 1 0))
                          (emit-expr b init))
                 b))))
      ;; type section: the base types form one rec group (identical in every
      ;; module, so that types are shared across modules); types added later
      ;; are singleton groups
      (let* ((types (wasm-module-types m))
             (base (or (wasm-module-base-types m) (length types)))
             (cache *type-section-cache*))
        ;; (modules made at run time have the same types, mostly: the
        ;; section of the last one is used again when they are equal)
        (unless (and cache (eql (car cache) base) (= (length (cadr cache)) (length types))
                     (every #'equal (cadr cache) types))
          (let ((b (make-buf)))
            (emit-u32 b (+ 1 (- (length types) base)))
            (emit-byte b #x4E)
            (emit-u32 b base)
            (loop for i below base do (emit-subtype m b (cdr (aref types i))))
            (loop for i from base below (length types)
                  do (emit-subtype m b (cdr (aref types i))))
            (setf cache (list base (coerce types 'simple-vector) b)
                  *type-section-cache* cache)))
        (emit-section out 1 (third cache)))
      ;; imports
      (when (plusp (+ (length (wasm-module-imports m)) (length (wasm-module-global-imports m))
                      (length (wasm-module-tag-imports m)) (if (wasm-module-memory-import m) 1 0)
                      (if (eq (car (wasm-module-table m)) :import) 1 0)))
        (let ((b (make-buf)))
          (emit-u32 b (+ (length (wasm-module-imports m)) (length (wasm-module-global-imports m))
                         (length (wasm-module-tag-imports m)) (if (wasm-module-memory-import m) 1 0)
                         (if (eq (car (wasm-module-table m)) :import) 1 0)))
          (when (wasm-module-memory-import m)
            (destructuring-bind (module field min) (wasm-module-memory-import m)
              (emit-name b module) (emit-name b field)
              (emit-byte b 2) (emit-byte b 0) (emit-u32 b min)))
          (when (eq (car (wasm-module-table m)) :import)
            (destructuring-bind (module field) (cdr (wasm-module-table m))
              (emit-name b module) (emit-name b field)
              (emit-byte b 1) (emit-byte b #x70) (emit-byte b 0) (emit-u32 b 0)))
          (loop for f across (wasm-module-imports m)
                do (emit-name b (wasm-func-import-module f))
                   (emit-name b (wasm-func-import-name f))
                   (emit-byte b 0)
                   (emit-u32 b (type-index m (wasm-func-type f))))
          (loop for (nil type mutable module field) across (wasm-module-global-imports m)
                do (emit-name b module) (emit-name b field)
                   (emit-byte b 3) (emit-valtype m b type) (emit-byte b (if mutable 1 0)))
          (loop for (nil type module field) across (wasm-module-tag-imports m)
                do (emit-name b module) (emit-name b field)
                   (emit-byte b 4) (emit-byte b 0) (emit-u32 b (type-index m type)))
          (emit-section out 2 b)))
      ;; function section (and the fillers of SMALL-FUNCTION-FILLERS)
      (let ((b (make-buf)))
        (emit-u32 b (+ (length (wasm-module-funcs m)) fillers))
        (loop for f across (wasm-module-funcs m)
              do (emit-u32 b (type-index m (wasm-func-type f))))
        (when (plusp fillers)
          (let ((ti (type-index m (wasm-func-type (aref (wasm-module-funcs m) 0)))))
            (loop repeat fillers do (emit-u32 b ti))))
        (emit-section out 3 b))
      ;; table
      (when (eq (car (wasm-module-table m)) :define)
        (let ((b (make-buf)))
          (emit-u32 b 1) (emit-byte b #x70) (emit-byte b 0)
          (emit-u32 b (hash-table-count (wasm-module-declared-refs m)))
          (emit-section out 4 b)))
      ;; memory
      (when (wasm-module-memory m)
        (let ((b (make-buf)))
          (emit-u32 b 1) (emit-byte b 0) (emit-u32 b (wasm-module-memory m))
          (emit-section out 5 b)))
      ;; tag section (13) comes after memory, before global
      (when (plusp (length (wasm-module-tags m)))
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-tags m)))
          (loop for (nil type) across (wasm-module-tags m)
                do (emit-byte b 0) (emit-u32 b (type-index m type)))
          (emit-section out 13 b)))
      ;; globals
      (when globals-section (emit-section out 6 globals-section))
      ;; exports
      (when (wasm-module-exports m)
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-exports m)))
          (dolist (e (reverse (wasm-module-exports m)))
            (destructuring-bind (name kind ref) e
              (emit-name b name)
              (ecase kind
                (:func (emit-byte b 0) (emit-u32 b (func-index m ref)))
                (:global (emit-byte b 3) (emit-u32 b (global-index m ref)))
                (:memory (emit-byte b 2) (emit-u32 b 0))
                (:table (emit-byte b 1) (emit-u32 b 0))
                (:tag (emit-byte b 4) (emit-u32 b (tag-index m ref))))))
          (emit-section out 7 b)))
      ;; start
      (when (wasm-module-start m)
        (let ((b (make-buf)))
          (emit-u32 b (func-index m (wasm-module-start m)))
          (emit-section out 8 b)))
      ;; element: one segment with all ref.func'd functions, declarative, or
      ;; active when the module has the table of functions (placed from its
      ;; code base)
      (let ((refs (loop for k being the hash-keys of (wasm-module-declared-refs m) collect k)))
        (when refs
          (let ((b (make-buf)))
            (emit-u32 b 1)
            (if (wasm-module-table m)
                (progn (emit-byte b 0)
                       (emit-expr b (let ((base (wasm-module-code-base m)))
                                      (if base `((:global.get ,base)) '((:i32.const 0))))))
                (progn (emit-byte b 3) (emit-byte b 0)))
            (emit-u32 b (length refs))
            (dolist (r refs) (emit-u32 b (func-index m r)))
            (emit-section out 9 b))))
      ;; datacount
      (when (plusp (length (wasm-module-data m)))
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-data m)))
          (emit-section out 12 b)))
      ;; branch hints (a custom section before the code)
      (when hints
        (let ((b (make-buf)))
          (emit-name b "metadata.code.branch_hint")
          (emit-u32 b (length hints))
          (loop for (index . h) in hints
                do (emit-u32 b index)
                   (emit-u32 b (length h))
                   (loop for (offset . value) in h
                         do (emit-u32 b offset) (emit-u32 b 1) (emit-u32 b value)))
          (emit-section out 0 b)))
      ;; compilationHints (V8's own format, before the code): the eager
      ;; functions are compiled by Liftoff with the module (the hint 0x12:
      ;; eager, Liftoff; withEagerHints in host/common.mjs)
      (when (wasm-module-eager-funcs m)
        ;; (one hint for each function of the code section, the fillers too)
        (let ((hints (make-array (+ (length bodies) fillers) :element-type '(unsigned-byte 8) :initial-element 0))
              (nimports (length (wasm-module-imports m))))
          (dolist (name (wasm-module-eager-funcs m))
            (let ((i (gethash name (wasm-module-func-index-table m))))
              (when (and i (<= nimports i) (< (- i nimports) (length hints)))
                (setf (aref hints (- i nimports)) #x12))))
          (let ((b (make-buf)))
            (emit-name b "compilationHints")
            (emit-u32 b (length hints))
            (emit-bytes b hints)
            (emit-section out 0 b))))
      ;; code
      (let ((b (make-buf)))
        (emit-u32 b (+ (length bodies) fillers))
        (dolist (body bodies) (emit-vec-buf b body))
        ;; (a filler: no locals, UNREACHABLE, END)
        (loop repeat fillers do (emit-bytes b #(3 0 0 11)))
        (emit-section out 10 b))
      ;; data (passive)
      (when (plusp (length (wasm-module-data m)))
        (let ((b (make-buf)))
          (emit-u32 b (length (wasm-module-data m)))
          (loop for d across (wasm-module-data m)
                do (emit-byte b 1) (emit-u32 b (length d)) (emit-bytes b d))
          (emit-section out 11 b)))
      ;; clysm.codes: how many functions the module places in the table of
      ;; functions (the host makes room for them before instantiating it)
      (when (eq (car (wasm-module-table m)) :import)
        (let ((b (make-buf)))
          (emit-name b "clysm.codes")
          (emit-u32 b (hash-table-count (wasm-module-declared-refs m)))
          (emit-section out 0 b)))
      ;; clysm.image-defaults: the globals of the image whose objects a heap
      ;; image fills when the image starts from it (host/common.mjs
      ;; imageVariant: there they are made empty): the initializers that
      ;; make them empty, then (global, initializer)
      (let* ((inits nil)
             (entries (loop for (name . init) in (wasm-module-image-defaults m)
                            for g = (gethash name (wasm-module-global-table m))
                            when g collect (cons g (or (position init inits :test #'equal)
                                                       (progn (setf inits (append inits (list init)))
                                                              (1- (length inits))))))))
        (when entries
          (let ((b (make-buf)))
            (emit-name b "clysm.image-defaults")
            (emit-u32 b (length inits))
            (dolist (init inits)
              (let ((e (make-buf))) (emit-expr e init) (emit-vec-buf b e)))
            (emit-u32 b (length entries))
            (loop for (g . k) in entries do (emit-u32 b g) (emit-u32 b k))
            (emit-section out 0 b))))
      ;; name section (function names) for nicer stack traces
      (let ((b (make-buf)) (sub (make-buf))
            (all (concatenate 'list (wasm-module-imports m) (wasm-module-funcs m))))
        (emit-name b "name")
        (emit-u32 sub (length all))
        (loop for f in all for i from 0
              do (emit-u32 sub i)
                 (let ((n (or (wasm-func-display f) (wasm-func-name f))))
                   (if (stringp n)
                       (emit-name sub n)
                       ;; (the names of the helpers, the same in every module)
                       (let ((octets (gethash n *name-octets*)))
                         (unless octets
                           (when (> (hash-table-count *name-octets*) 20000) (clrhash *name-octets*))
                           (setf octets (utf8-octets (let ((*package* (find-package :keyword)))
                                                       (prin1-to-string n)))
                                 (gethash n *name-octets*) octets))
                         (emit-u32 sub (length octets))
                         (emit-bytes sub octets)))))
        (emit-byte b 1)
        (emit-vec-buf b sub)
        (emit-section out 0 b))
      ;; clysm.sources: where the functions are defined, for stack traces
      ;; (host/common.mjs): the files, then (function index, file, line)
      (let* ((all (concatenate 'list (wasm-module-imports m) (wasm-module-funcs m)))
             (files nil)
             (entries (loop for f in all for i from 0
                            for src = (wasm-func-source f)
                            when (and (consp src) (stringp (car src)) (integerp (cdr src)))
                              collect (list i (or (position (car src) files :test #'string=)
                                                  (progn (setf files (append files (list (car src))))
                                                         (1- (length files))))
                                            (cdr src)))))
        (when entries
          (let ((b (make-buf)))
            (emit-name b "clysm.sources")
            (emit-u32 b (length files))
            (dolist (f files) (emit-name b f))
            (emit-u32 b (length entries))
            (loop for (i file line) in entries do (emit-u32 b i) (emit-u32 b file) (emit-u32 b line))
            (emit-section out 0 b)))))
    (subseq out 0)))

;;; The report of --shake-why.  What would go if a function, global or
;;; symbol went is what it dominates in the graph of references from the
;;; roots: what every chain from a root to it passes through (Cooper, Harvey
;;; and Kennedy, "A Simple, Fast Dominance Algorithm").
(defun shake-dominators (why)
  "The immediate dominator of each node of WHY (*SHAKE-WHY*), in an EQUAL hash
table, with :ROOT its root."
  (let ((succs (make-hash-table :test 'equal)) (order (make-hash-table :test 'equal))
        (rpo nil) (idom (make-hash-table :test 'equal)))
    (loop for to being the hash-keys of why using (hash-value preds)
          do (dolist (from preds) (push to (gethash from succs))))
    ;; postorder numbers, depth first from the root (without recursion)
    (let ((n 0) (stack (list (cons :root (gethash :root succs)))))
      (setf (gethash :root order) -1)
      (loop while stack
            do (let ((top (car stack)))
                 (if (cdr top)
                     (let ((next (pop (cdr top))))
                       (unless (gethash next order)
                         (setf (gethash next order) -1)
                         (push (cons next (gethash next succs)) stack)))
                     (progn (pop stack)
                            (setf (gethash (car top) order) n)
                            (incf n)
                            (push (car top) rpo))))))
    (setf (gethash :root idom) :root)
    (flet ((intersect (a b)
             (loop until (equal a b)
                   do (loop while (< (gethash a order) (gethash b order)) do (setf a (gethash a idom)))
                      (loop while (< (gethash b order) (gethash a order)) do (setf b (gethash b idom))))
             a))
      (loop with changed = t
            while changed
            do (setf changed nil)
               (dolist (node (cdr rpo))
                 (let ((new nil))
                   (dolist (p (gethash node why))
                     (when (gethash p idom)
                       (setf new (if new (intersect p new) p))))
                   (when (and new (not (equal new (gethash node idom))))
                     (setf (gethash node idom) new changed t))))))
    (remhash :root idom)
    idom))

(defun write-shake-why (m why sizes stream &key (largest 150))
  "Write to STREAM, for the module M that SHAKE-MODULE shook while WHY was
*SHAKE-WHY* and that ENCODE-MODULE encoded while SIZES was *FUNC-BODY-SIZES*:
the size of the kept code by source file; the LARGEST functions, globals and
symbols by the code that would go with them (what they dominate); and each
kept function, largest first, with the first chain from a root that reached
it."
  (let ((*package* (find-package :keyword)) (*print-length* 6) (*print-level* 3)
        (by-file (make-hash-table :test 'equal)) (total 0) (rows nil)
        (idom (shake-dominators why)) (dominated (make-hash-table :test 'equal)))
    (labels ((label (node)
               (if (eq node :root)
                   "root"
                   (let* ((f (and (eq (car node) :func) (find-func m (cdr node))))
                          (src (and f (wasm-func-source f))))
                     (format nil "~(~A~) ~A~@[ (~A)~]" (car node)
                             (let ((d (and f (wasm-func-display f))))
                               (if (stringp d) d (prin1-to-string (cdr node))))
                             (and (consp src) (format nil "~A:~A" (car src) (cdr src)))))))
             (chain (node)
               (let ((seen nil))
                 (loop for n = node then (car (last (gethash n why)))
                       while (and n (not (eq n :root)) (not (member n seen :test #'equal)))
                       do (push n seen)
                       finally (return (nreverse seen)))))
             (percent (v) (if (plusp total) (/ (* 100 v) total) 0)))
      (loop for f across (wasm-module-funcs m)
            for size = (gethash (wasm-func-name f) sizes 0)
            for src = (wasm-func-source f)
            do (incf total size)
               (incf (gethash (if (consp src) (car src) "?") by-file 0) size)
               (push (list size f) rows)
               ;; (its size counts for it and each of its dominators)
               (loop for n = (cons :func (wasm-func-name f)) then (gethash n idom)
                     while n do (incf (gethash n dominated 0) size)))
      (format stream "; ~D octets of code in ~D functions~%" total (length (wasm-module-funcs m)))
      (format stream "~&; by source file~%")
      (loop for (k . v) in (sort (loop for k being the hash-keys of by-file using (hash-value v)
                                       collect (cons k v))
                                 #'> :key #'cdr)
            do (format stream "~10D ~5,1F%  ~A~%" v (percent v) k))
      (format stream "~&; the ~D that the most code would go with (what they dominate), ~
                      each with what dominates it in turn~%" largest)
      (loop for (node . v) in (subseq (sort (loop for k being the hash-keys of dominated using (hash-value v)
                                                  collect (cons k v))
                                            #'> :key #'cdr)
                                      0 (min largest (hash-table-count dominated)))
            do (format stream "~10D ~5,1F%  ~A~{~%                    in ~A~}~%" v (percent v) (label node)
                       (loop for d = (gethash node idom) then (gethash d idom)
                             while (and d (not (eq d :root))) collect (label d))))
      (format stream "~&; functions, largest first: size, function, then what reached it first~%")
      (loop for (size f) in (sort rows #'> :key #'first)
            for c = (chain (cons :func (wasm-func-name f)))
            do (format stream "~8D ~A~{~%           <- ~A~}~%" size (label (first c))
                       (mapcar #'label (rest c)))))))
