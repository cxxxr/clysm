;;;; image.lisp — heap images: the heap saved to a file, and an image started
;;;; from it instead of running its toplevel forms (as SAVE-LISP-AND-DIE)
;;;;
;;;; (SAVE-IMAGE FILE FUNCTION) writes what the image has made: every object
;;;; reachable from the image's globals and from the globals of the modules
;;;; compiled in it, with the modules themselves.  A host that starts the image
;;;; from FILE (host/common.mjs restoreImage) instantiates the image and the
;;;; modules without running their toplevel forms, makes the objects again
;;;; (IMAGE_LOAD, src/prims.lisp), sets the modules' globals, and calls
;;;; %IMAGE-RESUME, which calls FUNCTION.
;;;;
;;;; A function is saved as the place of its code in the image's table of
;;;; functions (the image's own from 0, then each module's, in the order they
;;;; were loaded); the objects of the image's immutable globals are the same
;;;; objects in the image started from the file, and only their mutable
;;;; fields are written.  What cannot be saved: objects of the host
;;;; (JavaScript objects); SAVE-IMAGE signals an error naming the variable
;;;; through which one is reached.  The functions of *SAVE-IMAGE-HOOKS* are
;;;; called before saving (to drop such objects), and those of
;;;; *RESTORE-IMAGE-HOOKS* after starting from the file (to make them again).
;;;; Hash tables whose keys are hashed by identity (the host's identity hashes
;;;; are not saved) are rehashed.  Special variables are saved with their
;;;; global values (not the values bound around the call of SAVE-IMAGE).

(defvar *save-image-hooks* nil
  "Functions called (in order) before the heap is saved by SAVE-IMAGE.")
(defvar *restore-image-hooks* nil
  "Functions called (in order) when an image starts from a heap image.")
(defvar *image-restored* nil
  "True in an image started from a heap image.")

;;; the words being written: an (array i32), see src/prims.lisp
(defvar *img-buf* nil)
(defvar *img-fill* 0)

(defun %img-room (n)
  (let ((need (+ *img-fill* n)))
    (when (> need (%llen *img-buf*))
      (let ((new (%make-limbs (max need (* 2 (%llen *img-buf*))))))
        (%img-put-limbs new 0 *img-buf*)
        (setq *img-buf* new)))))

(defun %img-word (w)
  "Write W, a fixnum (an i32 when negative)."
  (%img-room 1)
  (%img-put-word *img-buf* *img-fill* w)
  (setq *img-fill* (+ *img-fill* 1)))

(defun %img-halves (hi lo)
  (%img-room 1)
  (%img-put-halves *img-buf* *img-fill* hi lo)
  (setq *img-fill* (+ *img-fill* 1)))

;;; The tables of SAVE-IMAGE are hash tables keyed by objects; the marker of
;;; removed entries of hash tables (+DELETED+), which is an object of the
;;; heap too, would be dropped from them when they grow: it is keyed by
;;; another object.
(defvar *img-deleted-key* (list 'deleted-entry))
(defmacro %img-key (x)
  (let ((k (gensym)))
    `(let ((,k ,x)) (if (eq ,k +deleted+) *img-deleted-key* ,k))))

(defvar *img-ids* nil "object -> its index")
(defvar *img-current* nil "the object being written")

(defun %img-ref (x)
  (cond ((%unboundp x) (%img-word 0))
        ((or (fixnump x) (characterp x))
         (%img-room 1)
         (%img-put-i31 *img-buf* *img-fill* x)
         (setq *img-fill* (+ *img-fill* 1)))
        (t (let ((id (gethash (%img-key x) *img-ids*)))
             (unless id
               (error "SAVE-IMAGE: an object (~A) made or reached while saving, from ~A"
                      (%img-kind x) (if *img-current* (%img-short *img-current*) "?")))
             (%img-word (* 2 (+ id 1)))))))

(defun %img-kind (x)
  (cond ((consp x) :cons)
        ((%simple-vector-p x) :vector)
        ((%simple-string-p x)
         (if (dotimes (i (%string-length x) t)
               (when (>= (%char-code (%schar x i)) 256) (return nil)))
             :string8 :string32))
        ((%limbs-p x) :limbs)
        ((symbolp x) :symbol)
        ((functionp x) :closure)
        ((%instancep x) :instance)
        ((%cellp x) :cell)
        ((%packagep x) :package)
        ((%mdarray-p x) :mdarray)
        ((%double-float-p x) :double)
        ((%single-float-p x) :single)
        ((%bignump x) :bignum)
        ((%ratiop x) :ratio)
        ((%complex-d-p x) :complex-d)
        ((%complex-s-p x) :complex-s)
        ((%complexp x) :complex)
        ((%fsbufp x) :fsbuf)
        ((%foreignp x) :foreign)
        ((>= (%storage-kind x) 0) :storage)
        (t :unknown)))

(defvar +img-kinds+
  '(:cons :vector :string8 :string32 :limbs :cell :instance :package :symbol :closure
    :mdarray :double :single :bignum :ratio :complex :complex-d :complex-s :storage :fsbuf)
  "The kinds of the objects of a heap image (as +IMG-KINDS+ in src/prims.lisp).")

(defun %img-rank (kind)
  "Objects are made in the order of their ranks: an object refers in its
immutable fields only to objects of a lower rank."
  (case kind
    ((:instance :bignum :fsbuf) 1)
    ((:ratio :symbol) 2)
    (:complex 3)
    (:closure 4)
    (t 0)))

(defun %img-global-values ()
  "Symbol -> its global value (or the unbound marker), for the symbols bound
now (the oldest binding of a symbol on the binding stack holds it)."
  (let ((table (make-hash-table :test 'eq)) (stack (%bstack)))
    (do ((i 0 (+ i 2))) ((>= i (%bsp)) table)
      (let ((s (%svref stack i)))
        (unless (nth-value 1 (gethash s table))
          (setf (gethash s table) (%svref stack (+ i 1))))))))

(defvar *img-globals* nil)

(defun %img-symbol-value (s)
  (multiple-value-bind (v found) (gethash s *img-globals*)
    (if found v (%symbol-raw-value s))))

(defun %img-children (x kind fn)
  "Call FN on the objects X refers to (raw: null for none) and the field of X
(a name or an index)."
  (case kind
    (:cons (funcall fn (%car x) "car") (funcall fn (%cdr x) "cdr"))
    (:vector (dotimes (i (%svlength x)) (funcall fn (%svref x i) i)))
    (:symbol (funcall fn (symbol-name x) "name") (funcall fn (%symbol-raw-package x) "package")
     (funcall fn (%img-symbol-value x) "value") (funcall fn (%symbol-raw-function x) "function")
     (funcall fn (%symbol-raw-plist x) "plist"))
    (:closure (funcall fn (%closure-env x) "environment") (funcall fn (%closure-raw-klayout x) "keywords")
     (funcall fn (%closure-name x) "name"))
    (:instance (funcall fn (%instance-layout x) "class") (funcall fn (%instance-slots x) "slots"))
    (:cell (funcall fn (%cell-value x) "value"))
    (:package (dotimes (i 8) (funcall fn (%package-ref x i) i)))
    (:mdarray (dotimes (i 5) (funcall fn (%mdarray-field x i) i)))
    (:bignum (funcall fn (%bignum-raw-limbs x) "digits"))
    (:ratio (funcall fn (%ratio-num x) "numerator") (funcall fn (%ratio-den x) "denominator"))
    (:complex (funcall fn (%complex-real x) "real part") (funcall fn (%complex-imag x) "imaginary part"))
    (:fsbuf (funcall fn (%fsbuf-raw-octets x) "octets"))
    (:storage (dotimes (i (%storage-length x)) (funcall fn (%simple-ref x i) i)))))

(defun %img-describe-path (x parents roots)
  "How X is reached: the chain of objects from a root (PARENTS: object ->
(parent . field))."
  (let ((chain nil))
    (loop (let ((p (gethash (%img-key x) parents)))
            (push (cons x (and (consp p) (cdr p))) chain)
            (unless (consp p) (return))
            (setq x (car p))))
    (with-output-to-string (out)
      ;; the variable it is the value of, if any
      (let ((var (loop for (e next) on chain
                       when (and next (symbolp (car e)) (equal (cdr next) "value"))
                         do (return (car e)))))
        (when var (format out "the variable ~S, from " var)))
      ;; (the names of the roots are made here: not while saving, when
      ;; FORMAT would change its caches)
      (let* ((root (car (first chain))) (name (gethash root roots)))
        (cond ((stringp name) (write-string name out))
              ((integerp name) (format out "the globals of module ~D" name))
              (t (write-string (%img-short root) out))))
      (dolist (e (cdr chain))
        (format out "~%  -> (~A) ~A" (cdr e) (%img-short (car e)))))))

(defun %img-short (x)
  (handler-case
      (let ((*print-length* 4) (*print-level* 2) (*print-readably* nil) (*print-circle* nil))
        (cond ((symbolp x) (format nil "the symbol ~S" x))
              ((%foreignp x) "a JavaScript object")
              ((%unboundp x) "nothing")
              (t (let ((s (format nil "~S" x)))
                   (if (> (length s) 100) (concatenate 'string (subseq s 0 100) "...") s)))))
    (error () (format nil "a ~A" (%img-kind x)))))

(defun %ht-identity-keys-p (h)
  "True if the hash of a key of the hash table H depends on its identity."
  (let ((kind (%ht h +ht-kind+)) (keys (%ht h +ht-keys+)))
    (dotimes (i (%ht h +ht-fill+) nil)
      (let ((k (%svref keys i)))
        (unless (or (eq k +deleted+) (fixnump k) (characterp k) (symbolp k)
                    (and (> kind 0) (numberp k))
                    (and (> kind 1) (stringp k)))
          (return t))))))

(defun %ht-rehash (h)
  (let ((keys (%ht h +ht-keys+)) (vals (%ht h +ht-vals+)) (fill (%ht h +ht-fill+)))
    (%ht-init-storage h (max 8 (%svlength keys)))
    (dotimes (i fill)
      (let ((k (%svref keys i)))
        (unless (eq k +deleted+) (%ht-put h k (%svref vals i)))))))

(defun save-image (file function)
  "Write the heap to FILE, from which a host can start this image again
(see the beginning of this file); there FUNCTION is called."
  (mapc #'funcall *save-image-hooks*)
  ;; (the JavaScript strings of literal names: made again when first used)
  (%js-forget-names)
  (let ((host-values (mapcar (lambda (e) (symbol-value (car e))) *host-variables*)))
    (dolist (e *host-variables*) (setf (symbol-value (car e)) nil))
    (unwind-protect (%save-image file function)
      (loop for e in *host-variables* for v in host-values
            do (setf (symbol-value (car e)) v)))))

(defun %save-image (file function)
  (when *image-restored*
    (error "SAVE-IMAGE: an image started from a heap image cannot be saved again (its modules are without the code that saving needs)"))
  (when (zerop (%module-count))
    (error "SAVE-IMAGE: the host does not record the modules compiled in the image (CLYSM_SAVE_IMAGE)"))
  (let* (;; (bound: their global values are saved)
         (*img-buf* nil) (*img-fill* 0) (*img-current* nil) (*img-ids* nil)
         (*img-globals* (%img-global-values))
         (parents (make-hash-table :test 'eq :size 1000000))
         (roots-names (make-hash-table :test 'eq))
         (base (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
                 (let ((chunks (%image-objects)))
                   (dotimes (i (%svlength chunks) (coerce v 'simple-vector))
                     (let ((c (%svref chunks i)))
                       (dotimes (j (%svlength c)) (vector-push-extend (%svref c j) v)))))))
         (mutables (let ((v nil) (chunks (%image-mutables)))
                     (dotimes (i (%svlength chunks))
                       (let ((c (%svref chunks i)))
                         (dotimes (j (%svlength c)) (push (%svref c j) v))))
                     (coerce (nreverse v) 'simple-vector)))
         (snaps (let ((v (make-array (%module-count))))
                  (dotimes (i (length v) v) (setf (svref v i) (%module-snap i)))))
         (rehash (make-array 0 :adjustable t :fill-pointer 0))
         (ranks (make-array 5 :initial-element nil))
         (fillable 0)
         (queue (make-array 100000 :adjustable t :fill-pointer 0))
         (head 0))
    (setq *img-ids* (make-hash-table :test 'eq :size 1000000))
    (labels ((visit (x parent &optional field)
               (unless (or (%unboundp x) (fixnump x) (characterp x)
                           (nth-value 1 (gethash (%img-key x) parents)))
                 (setf (gethash (%img-key x) parents) (if (eq parent :root) :root (cons parent field)))
                 (vector-push-extend x queue)))
             (walk ()
               ;; breadth first: the path to an object that cannot be saved is a shortest one
               (loop while (< head (fill-pointer queue))
                     do (let* ((x (aref queue head)) (kind (%img-kind x)))
                          (setf (aref queue head) nil)
                          (incf head)
                          (case kind
                            (:foreign
                             (error "SAVE-IMAGE: a JavaScript object cannot be saved; it is reached from ~A"
                                    (%img-describe-path x parents roots-names)))
                            (:unknown
                             (error "SAVE-IMAGE: an object of an unknown kind is reached from ~A"
                                    (%img-describe-path x parents roots-names))))
                          (when (and (eq kind :instance) (hash-table-p x) (%ht-identity-keys-p x))
                            (vector-push-extend x rehash))
                          (when (member kind '(:cons :vector :symbol :closure :instance :cell :package :mdarray))
                            (incf fillable))
                          (unless (gethash (%img-key x) *img-ids*)
                            (push x (svref ranks (%img-rank kind))))
                          (%img-children x kind (lambda (y field) (visit y x field)))))))
      ;; the image's own objects: indexes 0... (an object in two globals
      ;; keeps its first index)
      (dotimes (i (length base))
        (let ((x (svref base i)))
          (unless (or (fixnump x) (characterp x) (gethash (%img-key x) *img-ids*))
            (setf (gethash (%img-key x) *img-ids*) i))))
      (dotimes (i (length base))
        (let ((x (svref base i)))
          (visit x :root)))
      (walk)
      (let ((roots (vector mutables snaps function nil)))
        (setf (gethash mutables roots-names) "the mutable globals of the image"
              (gethash snaps roots-names) "the globals of the modules"
              (gethash function roots-names) "the function to call")
        (dotimes (i (length snaps))
          (setf (gethash (svref snaps i) roots-names) i))
        (visit roots :root)
        (walk)
        ;; (the tables to rehash are themselves reached)
        (setf (svref roots 3) (coerce rehash 'simple-vector))
        (visit (svref roots 3) roots 3)
        (walk)
        ;; indexes of the heap objects, rank by rank
        (let ((next (length base)) (order nil))
          (dotimes (r 5)
            (dolist (x (nreverse (svref ranks r)))
              (setf (gethash (%img-key x) *img-ids*) next)
              (incf next)
              (push x order)))
          (setq order (nreverse order))
          (setq *img-buf* (%make-limbs 1000000) *img-fill* 0)
          (%img-halves #x494D #x4C43)       ; magic "CLMI"
          (%img-word 1)                     ; version
          (%img-word (length base))
          (%img-word (- next (length base)))
          (%img-word fillable)
          (dolist (x order) (%img-write-object x))
          ;; the records of mutable fields: the objects of the image, then
          ;; those of the heap
          (let ((written 0))
            (flet ((record (x)
                     (when (%img-write-fields x) (incf written))))
              (let ((seen (make-hash-table :test 'eq)))
                (dotimes (i (length base))
                  (let ((x (svref base i)))
                    (unless (or (fixnump x) (characterp x) (gethash (%img-key x) seen))
                      (setf (gethash (%img-key x) seen) t)
                      (record x)))))
              (dolist (x order) (record x)))
            (unless (= written fillable)
              (error "SAVE-IMAGE: ~D records written, ~D expected" written fillable)))
          (%img-ref roots)
          ;; (strings go to the host through the memory: the name first)
          (let ((save (js:global "__clysm_save_image"))
                (name (js:to-js (namestring (merge-pathnames file)))))
            (%img-to-memory *img-buf* *img-fill*)
            (js:funcall save name (* 4 *img-fill*)))
          (values (- next (length base)) (* 4 *img-fill*)))))))

(defun %img-write-object (x)
  "The kind of X and its immutable contents."
  (let ((kind (%img-kind x)))
    (%img-word (position kind +img-kinds+))
    (case kind
      ((:cons :cell :package :mdarray))
      (:vector (%img-word (%svlength x)))
      (:string8
       (let ((n (%string-length x)))
         (%img-word n)
         (do ((i 0 (+ i 4))) ((>= i n))
           (flet ((c (k) (if (< (+ i k) n) (%char-code (%schar x (+ i k))) 0)))
             (%img-halves (+ (c 2) (* 256 (c 3))) (+ (c 0) (* 256 (c 1))))))))
      (:string32
       (let ((n (%string-length x)))
         (%img-word n)
         (dotimes (i n) (%img-word (%char-code (%schar x i))))))
      (:limbs
       (let ((n (%llen x)))
         (%img-word n)
         (%img-room n)
         (%img-put-limbs *img-buf* *img-fill* x)
         (setq *img-fill* (+ *img-fill* n))))
      (:instance (%img-ref (%instance-slots x)) (%img-word (%instance-raw-hash x)))
      (:symbol (%img-ref (symbol-name x)) (%img-word (%symbol-hash x)))
      (:closure
       (dotimes (i 6)
         (let ((code (%closure-code x i)))
           (when (< code 0) (error "SAVE-IMAGE: a function without its code: ~S" x))
           (%img-word code)))
       (%img-ref (%closure-env x))
       (%img-word (%closure-code x 6))
       (%img-ref (%closure-raw-klayout x)))
      (:double (%img-room 2) (%img-put-double *img-buf* *img-fill* x) (setq *img-fill* (+ *img-fill* 2)))
      (:single (%img-room 1) (%img-put-single *img-buf* *img-fill* x) (setq *img-fill* (+ *img-fill* 1)))
      (:bignum
       (%img-word (%bignum-raw-sign x))
       (%img-ref (%bignum-raw-limbs x))
       (%img-room 2) (%img-put-bignum-value *img-buf* *img-fill* x) (setq *img-fill* (+ *img-fill* 2)))
      (:ratio (%img-ref (%ratio-num x)) (%img-ref (%ratio-den x)))
      (:complex (%img-ref (%complex-real x)) (%img-ref (%complex-imag x)))
      (:complex-d
       (%img-room 4)
       (%img-put-double *img-buf* *img-fill* (%cd-real x))
       (%img-put-double *img-buf* (+ *img-fill* 2) (%cd-imag x))
       (setq *img-fill* (+ *img-fill* 4)))
      (:complex-s
       (%img-room 2)
       (%img-put-single *img-buf* *img-fill* (%cs-real x))
       (%img-put-single *img-buf* (+ *img-fill* 1) (%cs-imag x))
       (setq *img-fill* (+ *img-fill* 2)))
      (:storage
       (let ((n (%storage-length x)))
         (%img-word (%storage-kind x))
         (%img-word n)
         (dotimes (i n)
           (let ((e (%simple-ref x i)))
             (unless (or (fixnump e) (characterp e))
               (error "SAVE-IMAGE: a specialized array of ~S cannot be saved yet" (type-of e)))
             (%img-ref e)))))
      (:fsbuf (%img-ref (%fsbuf-raw-octets x)) (%img-word (%fsbuf-pos x)) (%img-word (%fsbuf-fill x))))))

(defun %img-write-fields (x)
  "A record of the mutable fields of X (true if X has some)."
  (let ((kind (%img-kind x)))
    (when (member kind '(:cons :vector :symbol :closure :instance :cell :package :mdarray))
      (setq *img-current* x)
      (%img-ref x)
      (case kind
        (:cons (%img-ref (%car x)) (%img-ref (%cdr x)))
        (:vector (dotimes (i (%svlength x)) (%img-ref (%svref x i))))
        (:symbol (%img-ref (%symbol-raw-package x)) (%img-ref (%img-symbol-value x))
         (%img-ref (%symbol-raw-function x)) (%img-ref (%symbol-raw-plist x))
         (%img-word (%symbol-flags x)))
        (:closure (%img-ref (%closure-name x)))
        (:instance (%img-ref (%instance-layout x)) (%img-ref (%instance-slots x)))
        (:cell (%img-ref (%cell-value x)))
        (:package (dotimes (i 8) (%img-ref (%package-ref x i))))
        (:mdarray (dotimes (i 5) (%img-ref (%mdarray-field x i)))
         (%img-word (%mdarray-field x 5)) (%img-word (%mdarray-field x 6))))
      t)))

(defun %image-resume (roots)
  "Called by the host (image_resume) in an image started from a heap image."
  (setq *image-restored* t)
  (dolist (e (reverse *host-variables*))
    (setf (symbol-value (car e)) (funcall (cdr e))))
  (let ((rehash (svref roots 3)))
    (dotimes (i (length rehash)) (%ht-rehash (svref rehash i))))
  ;; as MAIN (eval.lisp) at the start of an image
  (setq *posix-argv* (%command-line-arguments))
  (%init-default-pathname)
  (%init-host-features)
  (mapc #'funcall *restore-image-hooks*)
  (%call-as-main-thread (svref roots 2)))
