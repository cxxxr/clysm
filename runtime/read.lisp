;;;; read.lisp — the Lisp reader

(defvar *read-base* 10)
(defvar *read-eval* t)
(defvar *read-suppress* nil)

(defstruct (readtable (:constructor %make-readtable) (:predicate readtablep))
  (macros (make-hash-table))          ; char -> (function . non-terminating-p)
  (dispatch (make-hash-table))        ; disp-char -> hash sub-char -> function
  (case :upcase))

(defvar *readtable* nil)
(defvar *standard-readtable* nil)


(defun copy-readtable (&optional (from *readtable*) to)
  (let* ((from (or from *standard-readtable*))
         (to (or to (%make-readtable))))
    (clrhash (readtable-macros to))
    (maphash (lambda (k v) (setf (gethash k (readtable-macros to)) v)) (readtable-macros from))
    (clrhash (readtable-dispatch to))
    (maphash (lambda (k v)
               (let ((h (make-hash-table)))
                 (maphash (lambda (k2 v2) (setf (gethash k2 h) v2)) v)
                 (setf (gethash k (readtable-dispatch to)) h)))
             (readtable-dispatch from))
    (setf (readtable-case to) (readtable-case from))
    to))

(defun set-macro-character (char function &optional non-terminating-p (rt *readtable*))
  (setf (gethash char (readtable-macros rt)) (cons function non-terminating-p))
  t)

(defun get-macro-character (char &optional (rt *readtable*))
  (let ((e (gethash char (readtable-macros (or rt *standard-readtable*)))))
    (if e (values (car e) (cdr e)) (values nil nil))))

(defun make-dispatch-macro-character (char &optional non-terminating-p (rt *readtable*))
  (setf (gethash char (readtable-dispatch rt)) (make-hash-table))
  (set-macro-character char (lambda (stream c) (%read-dispatch-macro stream c)) non-terminating-p rt)
  t)

(defun set-dispatch-macro-character (disp sub function &optional (rt *readtable*))
  (setf (gethash (char-upcase sub) (gethash disp (readtable-dispatch rt))) function)
  t)

(defun get-dispatch-macro-character (disp sub &optional (rt *readtable*))
  (let ((h (gethash disp (readtable-dispatch (or rt *standard-readtable*)))))
    (and h (gethash (char-upcase sub) h))))

(defun set-syntax-from-char (to from &optional (to-rt *readtable*) (from-rt *standard-readtable*))
  (let ((e (gethash from (readtable-macros from-rt))))
    (if e (setf (gethash to (readtable-macros to-rt)) e)
        (remhash to (readtable-macros to-rt))))
  t)

;;; ------------------------------------------------------------
;;; reader errors
;;; ------------------------------------------------------------

(defun %reader-error (stream fmt &rest args)
  (error 'reader-error :stream stream :format-control fmt :format-arguments args))

;;; ------------------------------------------------------------
;;; character classes
;;; ------------------------------------------------------------

(defun %whitespace[2]p (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)))

(defun %terminating-char-p (c)
  (or (%whitespace[2]p c)
      (let ((e (gethash c (readtable-macros *readtable*))))
        (and e (not (cdr e))))))

;;; ------------------------------------------------------------
;;; main entry points
;;; ------------------------------------------------------------

(defvar %eof-marker (make-symbol "EOF"))
(defvar %close-paren-marker (make-symbol "CLOSE-PAREN"))
(defvar %dot-marker (make-symbol "DOT"))

(defvar *read-labels* nil "alist (n . (placeholder . object)) for #n= / #n#")

(defun read-preserving-whitespace (&optional stream (eof-error-p t) eof-value recursive-p)
  (let* ((s (%input-stream stream))
         (x (if recursive-p
                (%read-object s)
                (let ((*read-labels* nil)) (%read-object s)))))
    (cond ((eq x %eof-marker)
           (if eof-error-p (error 'end-of-file :stream s) eof-value))
          ((eq x %close-paren-marker) (%reader-error s "unmatched close parenthesis"))
          (t x))))

(defun read (&optional stream (eof-error-p t) eof-value recursive-p)
  (let* ((s (%input-stream stream))
         (x (read-preserving-whitespace s eof-error-p %eof-marker recursive-p)))
    (if (eq x %eof-marker)
        eof-value
        (progn
          (unless recursive-p
            ;; consume one whitespace character following the object
            (let ((c (%stream-read-char s)))
              (unless (or (eq c :eof) (%whitespace[2]p c)) (unread-char c s))))
          x))))

(defun read-from-string (string &optional (eof-error-p t) eof-value &key (start 0) end preserve-whitespace)
  (declare (ignore preserve-whitespace))
  (let ((s (make-string-input-stream string start end)))
    (values (read s eof-error-p eof-value) (stream-index s))))

(defun %skip-whitespace (s)
  (loop
    (let ((c (%stream-read-char s)))
      (cond ((eq c :eof) (return :eof))
            ((%whitespace[2]p c))
            (t (unread-char c s) (return c))))))

(defun %read-object (s)
  "Read one object; returns %eof-marker at end of file."
  (loop
    (let ((c (%stream-read-char s)))
      (when (eq c :eof) (return %eof-marker))
      (unless (%whitespace[2]p c)
        (let ((m (gethash c (readtable-macros *readtable*))))
          (if m
              (let ((vals (multiple-value-list (funcall (car m) s c))))
                (when vals (return (car vals))))
              (progn (unread-char c s)
                     (return (%read-token-object s)))))))))

(defun %read-object-required (s)
  (let ((x (%read-object s)))
    (when (eq x %eof-marker) (error 'end-of-file :stream s))
    (when (eq x %close-paren-marker) (%reader-error s "unexpected )"))
    x))

;;; ------------------------------------------------------------
;;; tokens
;;; ------------------------------------------------------------

(defun %read-token-chars (s)
  "Returns (values string escaped-p escaped-mask) where mask is a list of escaped indices."
  (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t))
        (escaped nil) (mask nil))
    (loop
      (let ((c (%stream-read-char s)))
        (cond ((eq c :eof) (return))
              ((char= c #\\)
               (let ((n (%stream-read-char s)))
                 (when (eq n :eof) (error 'end-of-file :stream s))
                 (push (length buf) mask)
                 (vector-push-extend n buf) (setq escaped t)))
              ((char= c #\|)
               (setq escaped t)
               (loop
                 (let ((n (%stream-read-char s)))
                   (cond ((eq n :eof) (error 'end-of-file :stream s))
                         ((char= n #\|) (return))
                         ((char= n #\\) (push (length buf) mask) (vector-push-extend (%stream-read-char s) buf))
                         (t (push (length buf) mask) (vector-push-extend n buf))))))
              ((%terminating-char-p c) (unread-char c s) (return))
              (t (vector-push-extend c buf)))))
    (values (subseq buf 0) escaped mask)))

(defun %case-convert (str mask)
  (let ((mode (readtable-case *readtable*)))
    (if (eq mode :preserve) str
        (let ((r (copy-seq str)))
          (if (eq mode :invert)
              (let ((has-upper nil) (has-lower nil))
                (dotimes (i (length r))
                  (unless (member i mask)
                    (when (upper-case-p (char r i)) (setq has-upper t))
                    (when (lower-case-p (char r i)) (setq has-lower t))))
                (when (not (and has-upper has-lower))
                  (dotimes (i (length r))
                    (unless (member i mask)
                      (setf (char r i) (if has-upper (char-downcase (char r i)) (char-upcase (char r i))))))))
              (dotimes (i (length r))
                (unless (member i mask)
                  (setf (char r i) (if (eq mode :downcase) (char-downcase (char r i)) (char-upcase (char r i)))))))
          r))))

(defun %read-token-object (s)
  (multiple-value-bind (str escaped mask) (%read-token-chars s)
    (cond (*read-suppress* nil)
          ((and (not escaped) (string= str ".")) %dot-marker)
          (t
           (let ((n (and (not escaped) (%parse-number-token str *read-base*))))
             (if n (car n)
                 (%intern-token (%case-convert str mask) mask s)))))))

(defun %intern-token (str mask s)
  (let ((colon (do ((i 0 (+ i 1))) ((>= i (length str)) nil)
                 (when (and (char= (char str i) #\:) (not (member i mask))) (return i)))))
    (cond ((null colon) (values (intern str *package*)))
          ((= colon 0)
           (values (intern (subseq str (if (and (> (length str) 1) (char= (char str 1) #\:)) 2 1))
                           *keyword-package*)))
          (t (let* ((pname (subseq str 0 colon))
                    (double (and (< (+ colon 1) (length str)) (char= (char str (+ colon 1)) #\:)))
                    (name (subseq str (+ colon (if double 2 1))))
                    (pkg (find-package pname)))
               (unless pkg (%reader-error s "package ~A not found" pname))
               (if double
                   (values (intern name pkg))
                   (multiple-value-bind (sym status) (find-symbol name pkg)
                     (cond ((eq status :external) sym)
                           ((eq pkg *keyword-package*) (values (intern name pkg)))
                           ((null status)
                            (%reader-error s "symbol ~S not found in the ~A package" name pname))
                           (t (%reader-error s "symbol ~S is not external in the ~A package" name pname))))))))))

(defun %parse-number-token (str base)
  "Returns (list number) or NIL."
  (let ((len (length str)) (i 0) (sign 1))
    (when (= len 0) (return-from %parse-number-token nil))
    (when (member (char str 0) '(#\+ #\-))
      (when (char= (char str 0) #\-) (setq sign -1))
      (setq i 1))
    (when (>= i len) (return-from %parse-number-token nil))
    ;; integer in *read-base*, optional trailing dot (decimal)
    (let ((end (if (char= (char str (- len 1)) #\.) (- len 1) len)))
      (when (and (> end i) (every-index (lambda (c) (digit-char-p c (if (< end len) 10 base))) str i end))
        (return-from %parse-number-token
          (list (* sign (%digits-value str i end (if (< end len) 10 base)))))))
    ;; ratio
    (let ((slash (position #\/ str)))
      (when slash
        (if (and (> slash i) (< (+ slash 1) len)
                 (every-index (lambda (c) (digit-char-p c base)) str i slash)
                 (every-index (lambda (c) (digit-char-p c base)) str (+ slash 1) len))
            (let ((d (%digits-value str (+ slash 1) len base)))
              (return-from %parse-number-token
                (if (zerop d) nil (list (/ (* sign (%digits-value str i slash base)) d)))))
            (return-from %parse-number-token nil))))
    ;; float
    (let ((j i) (int-digits 0) (frac-digits 0) (marker nil) (expo 0))
      (do () ((not (and (< j len) (digit-char-p (char str j))))) (setq j (+ j 1) int-digits (+ int-digits 1)))
      (when (and (< j len) (char= (char str j) #\.))
        (setq j (+ j 1))
        (do () ((not (and (< j len) (digit-char-p (char str j))))) (setq j (+ j 1) frac-digits (+ frac-digits 1))))
      (when (= (+ int-digits frac-digits) 0) (return-from %parse-number-token nil))
      (let ((mant-end j))
        (when (and (< j len) (member (char-upcase (char str j)) '(#\E #\S #\F #\D #\L)))
          (setq marker (char-upcase (char str j)))
          (let ((k (+ j 1)) (esign 1))
            (when (and (< k len) (member (char str k) '(#\+ #\-)))
              (when (char= (char str k) #\-) (setq esign -1))
              (setq k (+ k 1)))
            (unless (and (< k len) (every-index #'digit-char-p str k len))
              (return-from %parse-number-token nil))
            (setq expo (* esign (%digits-value str k len 10)))
            (setq j len)))
        (unless (= j len) (return-from %parse-number-token nil))
        (when (and (= frac-digits 0) (null marker)) (return-from %parse-number-token nil))
        (let* ((digits (remove #\. (subseq str i mant-end)))
               (exact (* sign (%digits-value digits 0 (length digits) 10)
                         (expt 10 (- expo frac-digits))))
               (type (case marker
                       ((#\D #\L) 'double-float)
                       ((#\F #\S) 'single-float)
                       (t *read-default-float-format*))))
          (list (if (and (zerop exact) (= sign -1))
                    (if (eq type 'double-float) -0.0d0 -0.0)
                    (if (member type '(double-float long-float)) (%to-double exact) (%to-single exact)))))))))

(defun every-index (pred str start end)
  (do ((i start (+ i 1))) ((>= i end) t)
    (unless (funcall pred (char str i)) (return nil))))

(defun %digits-value (str start end base)
  (let ((v 0))
    (do ((i start (+ i 1))) ((>= i end) v)
      (setq v (+ (* v base) (digit-char-p (char str i) base))))))

;;; ------------------------------------------------------------
;;; standard macro characters
;;; ------------------------------------------------------------

(defun %read-list (s close)
  (let ((items nil) (tail nil))
    (loop
      (let ((c (%skip-whitespace s)))
        (when (eq c :eof) (error 'end-of-file :stream s))
        (when (char= c close)
          (%stream-read-char s)
          (let ((l (nreverse items)))
            (when tail (if l (setf (cdr (last l)) (car tail)) (%reader-error s "nothing before . in list")))
            (return l)))
        (let ((x (%read-object s)))
          (cond ((eq x %eof-marker) (error 'end-of-file :stream s))
                ((eq x %close-paren-marker)
                 (return (let ((l (nreverse items))) (when tail (setf (cdr (last l)) (car tail))) l)))
                ((eq x %dot-marker)
                 (when (null items) (%reader-error s "nothing before . in list"))
                 (setq tail (list (%read-object-required s))))
                (t (when tail (%reader-error s "more than one object after . in list"))
                   (push x items))))))))

(defun %read-string-literal (s close)
  (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (let ((c (%stream-read-char s)))
        (cond ((eq c :eof) (error 'end-of-file :stream s))
              ((char= c close) (return (subseq buf 0)))
              ((char= c #\\) (vector-push-extend (%stream-read-char s) buf))
              (t (vector-push-extend c buf)))))))

(defun %read-dispatch-macro (s disp)
  (let ((n nil) (c nil))
    (loop
      (setq c (%stream-read-char s))
      (when (eq c :eof) (error 'end-of-file :stream s))
      (if (digit-char-p c)
          (setq n (+ (* (or n 0) 10) (digit-char-p c)))
          (return)))
    (let* ((table (gethash disp (readtable-dispatch *readtable*)))
           (fn (and table (gethash (char-upcase c) table))))
      (unless fn (%reader-error s "no dispatch function defined for #~A" c))
      (funcall fn s c n))))

(defun %feature-match-p (f)
  (cond ((symbolp f) (not (null (member (symbol-name f) *features* :key #'symbol-name :test #'string=))))
        ((not (consp f)) nil)
        ((string= (symbol-name (car f)) "AND") (every #'%feature-match-p (cdr f)))
        ((string= (symbol-name (car f)) "OR") (some #'%feature-match-p (cdr f)))
        ((string= (symbol-name (car f)) "NOT") (not (%feature-match-p (cadr f))))
        (t nil)))

(defun %qq-mark (sym x) (list sym x))

(defun %init-standard-readtable ()
  (let ((rt (%make-readtable)))
    (setq *readtable* rt)
    (set-macro-character #\( (lambda (s c) (declare (ignore c)) (%read-list s #\))))
    (set-macro-character #\) (lambda (s c) (declare (ignore s c)) %close-paren-marker))
    (set-macro-character #\' (lambda (s c) (declare (ignore c)) (list 'quote (%read-object-required s))))
    (set-macro-character #\; (lambda (s c) (declare (ignore c))
                               (loop (let ((ch (%stream-read-char s)))
                                       (when (or (eq ch :eof) (char= ch #\Newline)) (return))))
                               (values)))
    (set-macro-character #\" (lambda (s c) (%read-string-literal s c)))
    (set-macro-character #\` (lambda (s c) (declare (ignore c)) (list 'quasiquote (%read-object-required s))))
    (set-macro-character #\, (lambda (s c) (declare (ignore c))
                               (let ((n (%stream-read-char s)))
                                 (cond ((eq n :eof) (error 'end-of-file :stream s))
                                       ((or (char= n #\@) (char= n #\.))
                                        (list 'unquote-splicing (%read-object-required s)))
                                       (t (unread-char n s) (list 'unquote (%read-object-required s)))))))
    (make-dispatch-macro-character #\# t)
    (set-dispatch-macro-character #\# #\' (lambda (s c n) (declare (ignore c n)) (list 'function (%read-object-required s))))
    (set-dispatch-macro-character #\# #\( (lambda (s c n)
                                            (declare (ignore c))
                                            (let ((l (%read-list s #\))))
                                              (if (and n (> n (length l)))
                                                  (let ((v (make-array n :initial-element (car (last l)))))
                                                    (replace v l) v)
                                                  (coerce l 'simple-vector)))))
    (set-dispatch-macro-character #\# #\\
      (lambda (s c n)
        (declare (ignore c n))
        (let ((first (%stream-read-char s)))
          (let ((next (%stream-read-char s)))
            (if (or (eq next :eof) (%terminating-char-p next))
                (progn (unless (eq next :eof) (unread-char next s)) first)
                (progn
                  (unread-char next s)
                  (let* ((rest (%read-token-chars s))
                         (name (concatenate 'string (string first) rest)))
                    (or (name-char name)
                        (%reader-error s "unrecognized character name: ~A" name)))))))))
    (set-dispatch-macro-character #\# #\: (lambda (s c n) (declare (ignore c n))
                                            (make-symbol (%case-convert (%read-token-chars s) nil))))
    (set-dispatch-macro-character #\# #\. (lambda (s c n) (declare (ignore c n))
                                            (let ((form (%read-object-required s)))
                                              (if *read-eval* (eval form)
                                                  (%reader-error s "*READ-EVAL* is NIL")))))
    (set-dispatch-macro-character #\# #\| (lambda (s c n) (declare (ignore c n))
                                            (let ((depth 1) (prev nil))
                                              (loop
                                                (let ((ch (%stream-read-char s)))
                                                  (when (eq ch :eof) (error 'end-of-file :stream s))
                                                  (cond ((and (eql prev #\|) (char= ch #\#))
                                                         (setq depth (- depth 1) ch nil)
                                                         (when (= depth 0) (return)))
                                                        ((and (eql prev #\#) (char= ch #\|))
                                                         (setq depth (+ depth 1) ch nil)))
                                                  (setq prev ch))))
                                            (values)))
    (flet ((radix (base) (lambda (s c n) (declare (ignore c n))
                           (let ((*read-base* base)) (%read-object-required s)))))
      (set-dispatch-macro-character #\# #\X (radix 16))
      (set-dispatch-macro-character #\# #\B (radix 2))
      (set-dispatch-macro-character #\# #\O (radix 8)))
    (set-dispatch-macro-character #\# #\R (lambda (s c n) (declare (ignore c))
                                            (let ((*read-base* n)) (%read-object-required s))))
    (set-dispatch-macro-character #\# #\* (lambda (s c n) (declare (ignore c n))
                                            (let ((tok (%read-token-chars s)))
                                              (coerce (map 'list #'digit-char-p tok) 'bit-vector))))
    (set-dispatch-macro-character #\# #\C (lambda (s c n) (declare (ignore c n))
                                            (let ((l (%read-object-required s))) (complex (car l) (cadr l)))))
    (set-dispatch-macro-character #\# #\A (lambda (s c n) (declare (ignore c))
                                            (let ((contents (%read-object-required s)))
                                              (make-array (%array-dims-from n contents) :initial-contents contents))))
    (set-dispatch-macro-character #\# #\= (lambda (s c n) (declare (ignore c)) (%read-label-define s n)))
    (set-dispatch-macro-character #\# #\# (lambda (s c n) (declare (ignore c)) (%read-label-ref s n)))
    (set-dispatch-macro-character #\# #\P (lambda (s c n) (declare (ignore c n)) (pathname (%read-object-required s))))
    (set-dispatch-macro-character #\# #\S (lambda (s c n) (declare (ignore c n))
                                            (let ((l (%read-object-required s)))
                                              (apply (intern (concatenate 'string "MAKE-" (symbol-name (car l)))
                                                             (symbol-package (car l)))
                                                     (cdr l)))))
    (flet ((conditional (want)
             (lambda (s c n) (declare (ignore c n))
               (let ((feature (let ((*package* *keyword-package*)) (%read-object-required s))))
                 (if (eq (%feature-match-p feature) want)
                     (values)
                     (progn (let ((*read-suppress* t)) (%read-object-required s)) (values)))))))
      (set-dispatch-macro-character #\# #\+ (conditional t))
      (set-dispatch-macro-character #\# #\- (conditional nil)))
    (setq *standard-readtable* rt)
    (setq *readtable* (copy-readtable rt))
    rt))

(defun %read-label-define (s n)
  (let* ((placeholder (list '%read-placeholder n))
         (entry (cons n (cons placeholder placeholder))))
    (push entry *read-labels*)
    (let ((obj (%read-object-required s)))
      (setf (cddr entry) obj)
      (%read-label-patch obj placeholder obj (make-hash-table :test 'eq)))))

(defun %read-label-patch (tree placeholder obj seen)
  "Replace PLACEHOLDER by OBJ inside TREE (which may be circular)."
  (cond ((eq tree placeholder) obj)
        ((gethash tree seen) tree)
        ((consp tree)
         (setf (gethash tree seen) t)
         (setf (car tree) (%read-label-patch (car tree) placeholder obj seen))
         (setf (cdr tree) (%read-label-patch (cdr tree) placeholder obj seen))
         tree)
        ((and (simple-vector-p tree))
         (setf (gethash tree seen) t)
         (dotimes (i (length tree) tree)
           (setf (svref tree i) (%read-label-patch (svref tree i) placeholder obj seen))))
        (t tree)))

(defun %read-label-ref (s n)
  (let ((e (assoc n *read-labels*)))
    (unless e (%reader-error s "undefined label #~D#" n))
    (cddr e)))

(defun %array-dims-from (rank contents)
  (if (or (null rank) (= rank 0)) nil
      (cons (length contents) (%array-dims-from (- rank 1) (if (> (length contents) 0) (elt contents 0) nil)))))

(%init-standard-readtable)

(defun read-delimited-list (char &optional stream recursive-p)
  (declare (ignore recursive-p))
  (%read-list (%input-stream stream) char))

;;; ------------------------------------------------------------
;;; quasiquote (runtime expansion, same algorithm as the compiler)
;;; ------------------------------------------------------------

(defun %qq-expand (x depth)
  (cond ((consp x)
         (cond ((eq (car x) 'quasiquote)
                (list 'cons (list 'quote 'quasiquote) (%qq-expand (cdr x) (+ depth 1))))
               ((or (eq (car x) 'unquote) (eq (car x) 'unquote-splicing))
                (cond ((> depth 0) (list 'cons (list 'quote (car x)) (%qq-expand (cdr x) (- depth 1))))
                      ((eq (car x) 'unquote) (cadr x))
                      (t (error "Illegal ,@ position"))))
               (t (list 'append (%qq-expand-list (car x) depth) (%qq-expand (cdr x) depth)))))
        ((simple-vector-p x) (list 'coerce (%qq-expand (coerce x 'list) depth) ''simple-vector))
        ((or (symbolp x) (consp x)) (list 'quote x))
        (t x)))

(defun %qq-expand-list (x depth)
  (cond ((consp x)
         (cond ((eq (car x) 'quasiquote)
                (list 'list (list 'cons (list 'quote 'quasiquote) (%qq-expand (cdr x) (+ depth 1)))))
               ((or (eq (car x) 'unquote) (eq (car x) 'unquote-splicing))
                (cond ((> depth 0) (list 'list (list 'cons (list 'quote (car x)) (%qq-expand (cdr x) (- depth 1)))))
                      ((eq (car x) 'unquote) (cons 'list (cdr x)))
                      (t (cons 'append (cdr x)))))
               (t (list 'list (list 'append (%qq-expand-list (car x) depth) (%qq-expand (cdr x) depth))))))
        (t (list 'list (%qq-expand x depth)))))

(%set-macro-function 'quasiquote (lambda (form env) (declare (ignore env)) (%qq-expand (cadr form) 0)))
