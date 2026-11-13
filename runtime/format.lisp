;;;; format.lisp — FORMAT and FORMATTER (CLHS 22.3)
;;;;
;;;; A control string is tokenized into literal strings and directive
;;;; structures, then built into a tree (compound directives ~[ ~{ ~( ~< hold
;;;; their clauses).  There are two users of the tree:
;;;;
;;;; * the interpreter (%FMT-RUN) walks the tree with the arguments as a list;
;;;;   every directive returns the remaining arguments.  Trees are cached per
;;;;   control string.  ~^ throws the remaining arguments to the innermost
;;;;   enclosing construct.
;;;; * the compiler (%FMTC-..., at the end of the file) turns the tree of a
;;;;   constant control string into Lisp code at compile time: the compiler
;;;;   macro of FORMAT and the macro FORMATTER use it.
;;;;
;;;; Both call the same functions for the directives (%FMT-INTEGER,
;;;; %FMT-FIXED, %FMT-OP-A ...), so interpreted and compiled control strings
;;;; print the same.

;;; The parser is also used at compile time.
(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct (%fdir (:constructor %make-fdir (char colon at params start end)))
  char colon at params start end
  name                                  ; ~/name/: the function symbol name string
  clauses seps close)                   ; compound directives

(defvar *fmt-control* nil)

(defun %fmt-error (fmt &rest args)
  (error 'simple-error
         :format-control "Error in FORMAT: ~?~@[~%  control string: ~S~]"
         :format-arguments (list fmt args (and (stringp *fmt-control*) *fmt-control*))))

;;; ------------------------------------------------------------
;;; tokenizer
;;; ------------------------------------------------------------

(defun %fmt-known-char-p (c)
  (find c "ASDBOXRPCFEG$%&|~TW*?([{<^_I;)]}>/"))

(defun %fmt-parse-directive (s start)
  "START is the position of ~.  Returns (values directive next-index)."
  (let ((i (+ start 1)) (n (length s)) (params nil) (colon nil) (at nil))
    (flet ((need () (when (>= i n) (%fmt-error "String ended before directive was found"))))
      (loop
        (need)
        (let ((c (char s i)) (val nil) (got nil))
          (cond ((or (digit-char-p c)
                     (and (or (char= c #\-) (char= c #\+)) (< (+ i 1) n) (digit-char-p (char s (+ i 1)))))
                 (let ((j (+ i 1)))
                   (loop (unless (and (< j n) (digit-char-p (char s j))) (return)) (setq j (+ j 1)))
                   (setq val (parse-integer s :start i :end j) got t i j)))
                ((char= c #\')
                 (when (>= (+ i 1) n) (%fmt-error "String ended after quote"))
                 (setq val (char s (+ i 1)) got t i (+ i 2)))
                ((char-equal c #\V) (setq val :arg got t i (+ i 1)))
                ((char= c #\#) (setq val :remaining got t i (+ i 1))))
          (need)
          (if (char= (char s i) #\,)
              (progn (push val params) (setq i (+ i 1)))
              (progn (when (or got params) (push val params)) (return)))))
      (loop
        (need)
        (let ((c (char s i)))
          (cond ((char= c #\:) (when colon (%fmt-error "too many colons supplied")) (setq colon t i (+ i 1)))
                ((char= c #\@) (when at (%fmt-error "too many at-signs supplied")) (setq at t i (+ i 1)))
                (t (return)))))
      (need)
      (let* ((c (char s i))
             (uc (if (char= c #\Newline) c (char-upcase c)))
             (dir (%make-fdir uc colon at (nreverse params) start (+ i 1))))
        (unless (or (char= uc #\Newline) (%fmt-known-char-p uc))
          (%fmt-error "unknown directive ~~~C" c))
        (if (char= uc #\/)
            (let ((close (position #\/ s :start (+ i 1))))
              (unless close (%fmt-error "no matching closing slash"))
              (setf (%fdir-name dir) (subseq s (+ i 1) close)
                    (%fdir-end dir) (+ close 1))
              (values dir (+ close 1)))
            (values dir (+ i 1)))))))

(defun %fmt-tokenize (s)
  (let ((i 0) (n (length s)) (out nil))
    (loop
      (let ((p (position #\~ s :start i)))
        (when (null p)
          (when (< i n) (push (subseq s i n) out))
          (return))
        (when (> p i) (push (subseq s i p) out))
        (multiple-value-bind (dir next) (%fmt-parse-directive s p)
          (push dir out)
          (setq i next)
          (when (and (eql (%fdir-char dir) #\Newline) (not (%fdir-colon dir)))
            (loop (unless (and (< i n) (member (char s i) '(#\Space #\Tab #\Page #\Return))) (return))
                  (setq i (+ i 1)))))))
    (nreverse out)))

(defun %fmt-closer (c)
  (case c (#\[ #\]) (#\{ #\}) (#\( #\)) (#\< #\>)))

(defun %fmt-build-until (tokens close)
  "Returns (values clauses seps close-directive remaining-tokens)."
  (let ((clauses nil) (cur nil) (seps nil))
    (loop
      (when (null tokens)
        (when close (%fmt-error "no matching ~~~C" close))
        (return (values (list (nreverse cur)) nil nil nil)))
      (let ((tok (pop tokens)))
        (if (stringp tok)
            (push tok cur)
            (let ((c (%fdir-char tok)))
              (cond ((and close (char= c close))
                     (return (values (nreverse (cons (nreverse cur) clauses)) (nreverse seps) tok tokens)))
                    ((char= c #\;)
                     (unless (member close '(#\] #\>))
                       (%fmt-error "~~; not contained within either ~~[...~~] or ~~<...~~>"))
                     (push (nreverse cur) clauses)
                     (setq cur nil)
                     (push tok seps))
                    ((member c '(#\] #\} #\) #\>))
                     (%fmt-error "no matching opening directive for ~~~C" c))
                    ((member c '(#\[ #\{ #\( #\<))
                     (multiple-value-bind (cl sp cd rest) (%fmt-build-until tokens (%fmt-closer c))
                       (setf (%fdir-clauses tok) cl (%fdir-seps tok) sp (%fdir-close tok) cd)
                       (setq tokens rest)
                       (push tok cur)))
                    (t (push tok cur)))))))))

(defun %fmt-logical-block-p (d)
  (and (eql (%fdir-char d) #\<) (%fdir-colon (%fdir-close d))))

(defun %fmt-pretty-directive-p (d)
  (or (member (%fdir-char d) '(#\W #\_ #\I))
      (and (eql (%fdir-char d) #\T) (%fdir-colon d))
      (%fmt-logical-block-p d)))

(defun %fmt-check-tree (nodes)
  "Returns (values pretty-p justify-with-semi-p); signals errors for pretty
printing directives inside ~<...~> justification."
  (let ((pretty nil) (semi nil))
    (dolist (n nodes)
      (unless (stringp n)
        (when (%fmt-pretty-directive-p n) (setq pretty t))
        (when (%fdir-clauses n)
          (let ((justify (and (eql (%fdir-char n) #\<) (not (%fmt-logical-block-p n)))))
            (when (and justify (%fdir-seps n) (%fdir-colon (first (%fdir-seps n))))
              (setq semi t))
            (dolist (cl (%fdir-clauses n))
              (multiple-value-bind (p s) (%fmt-check-tree cl)
                (when (and justify p)
                  (%fmt-error "pretty printing directives are not allowed inside ~~<...~~>"))
                (when p (setq pretty t))
                (when s (setq semi t))))))))
    (values pretty semi)))

(defun %fmt-build (tokens)
  (let ((tree (car (%fmt-build-until tokens nil))))
    (multiple-value-bind (pretty semi) (%fmt-check-tree tree)
      (when (and pretty semi)
        (%fmt-error "~~<...~~:;...~~> cannot be used with pretty printing directives")))
    tree))

(defun %fmt-parse (control)
  "The tree of the control string CONTROL (a simple string)."
  (let ((*fmt-control* control)) (%fmt-build (%fmt-tokenize control))))

)                                       ; eval-when

(defvar *fmt-popper* nil)               ; inside ~<...~:>: checks done by pprint-pop
(defvar *fmt-outside-args* nil)         ; ~:{: the remaining sublists (for ~:^)
(defvar *fmt-colon-up-ok* nil)
(defvar *fmt-cache* nil)

;;; ------------------------------------------------------------
;;; entry points
;;; ------------------------------------------------------------

(defun %format-destination (destination)
  "The stream that FORMAT to DESTINATION writes to."
  (cond ((null destination) (%scratch-string-stream))
        ((eq destination t) *standard-output*)
        ((%streamp destination) destination)
        ((stringp destination)
         (unless (array-has-fill-pointer-p destination)
           (error 'type-error :datum destination
                              :expected-type '(and string (satisfies array-has-fill-pointer-p))))
         (%make-fill-pointer-output-stream destination))
        (t (error 'type-error :datum destination :expected-type '(or (member nil t) stream string)))))

(defun %format-result (destination stream)
  "The value of FORMAT to DESTINATION, after the output was made to STREAM."
  (if (null destination) (%scratch-string-result stream) nil))

(defun format (destination control &rest args)
  (unless (or (stringp control) (functionp control))
    (error 'type-error :datum control :expected-type '(or string function)))
  (let ((s (%format-destination destination)))
    (%format s control args)
    (%format-result destination s)))

(defun %format (stream control args &optional (orig args))
  "Interpret CONTROL (string or function); returns the unused arguments."
  (if (functionp control)
      (apply control stream args)
      (let ((*fmt-popper* nil) (*fmt-control* control))
        (catch '%fmt-up-and-out
          (%fmt-run stream (%fmt-parse-cached control) orig args)))))

;;; ------------------------------------------------------------
;;; the cache of parsed control strings
;;; ------------------------------------------------------------

(defvar *fmt-recent* (make-array 16 :initial-element nil))   ; control, copy, tree, ...
(defvar *fmt-recent-next* 0)

(defun %fmt-parse-cached (control)
  ;; recently used control strings, found by identity (and checked for changes)
  (let ((v *fmt-recent*))
    (do ((i 0 (+ i 3))) ((>= i 15))
      (when (and (eq (svref v i) control) (string= (svref v (+ i 1)) control))
        (return-from %fmt-parse-cached (svref v (+ i 2)))))
    (let ((tree (%fmt-parse-cached-1 control)) (i *fmt-recent-next*))
      (setf (svref v i) control (svref v (+ i 1)) (copy-seq control) (svref v (+ i 2)) tree)
      (setq *fmt-recent-next* (if (>= i 12) 0 (+ i 3)))
      tree)))

(defun %fmt-parse-cached-1 (control)
  (unless *fmt-cache* (setq *fmt-cache* (make-hash-table :test 'equal)))
  (let ((key (if (simple-string-p control) control (coerce control 'simple-string))))
    (or (gethash key *fmt-cache*)
        (let ((tree (%fmt-parse key)))
          (when (> (hash-table-count *fmt-cache*) 2000) (clrhash *fmt-cache*))
          (setf (gethash (copy-seq key) *fmt-cache*) tree)
          tree))))

;;; ------------------------------------------------------------
;;; the directives: functions called by the interpreter and by compiled
;;; control strings
;;; ------------------------------------------------------------

(defun %fmt-list-length (l)
  (let ((n 0)) (loop (unless (consp l) (return n)) (setq n (+ n 1) l (cdr l)))))

(defun %fmt-pretty-stream-p (stream)
  (and *print-pretty* (%pp-capture-p stream)))

(defun %fmt-spaces (stream n) (%write-chars stream #\Space n))

(defun %fmt-put (stream string)
  "Write the simple string STRING."
  (let ((n (%string-length string)))
    (unless (eql n 0) (%stream-write-simple-string stream string 0 n))))

(defun %fmt-write-field (stream string mincol colinc minpad padchar padleft)
  (unless padleft (write-string string stream))
  (%write-chars stream padchar minpad)
  (when (and mincol (> colinc 0))
    (let ((chars (+ (length string) (max minpad 0))))
      (when (< chars mincol)
        (%write-chars stream padchar (* colinc (ceiling (- mincol chars) colinc))))))
  (when padleft (write-string string stream)))

(defun %fmt-arg-error (args)
  "The arguments are exhausted (or not a list)."
  (if (null args)
      (%fmt-error "no more arguments")
      (error 'type-error :datum args :expected-type 'list)))

(defun %fmt-check-list (x)
  (unless (listp x) (error 'type-error :datum x :expected-type 'list))
  x)

(defun %fmt-int-param (v default char n)
  "The value of an integer parameter given by an argument (~V)."
  (cond ((null v) default)
        ((integerp v) v)
        (t (%fmt-error "parameter ~D of ~~~C must be an integer: ~S" (+ n 1) char v))))

(defun %fmt-char-param (v default char n)
  (cond ((null v) default)
        ((characterp v) v)
        (t (%fmt-error "parameter ~D of ~~~C must be a character: ~S" (+ n 1) char v))))

(defun %fmt-princ (arg stream)
  ;; PRINC of a string is WRITE-STRING
  (if (stringp arg) (write-string arg stream) (princ arg stream)))

(defun %fmt-princ-colon (arg stream)
  (if (null arg) (write-string "()" stream) (princ arg stream)))

(defun %fmt-prin1-colon (arg stream)
  (if (null arg) (write-string "()" stream) (prin1 arg stream)))

(defun %fmt-op-a (stream arg prin1p colon at mincol colinc minpad padchar)
  "~A and ~S with parameters."
  (%fmt-write-field stream
                    (cond ((and colon (null arg)) "()")
                          (prin1p (prin1-to-string arg))
                          (t (princ-to-string arg)))
                    mincol colinc minpad padchar at))

(defun %fmt-op-w (stream arg colon at)
  (cond ((and colon at) (let ((*print-pretty* t) (*print-level* nil) (*print-length* nil))
                          (output-object arg stream)))
        (colon (let ((*print-pretty* t)) (output-object arg stream)))
        (at (let ((*print-level* nil) (*print-length* nil)) (output-object arg stream)))
        (t (output-object arg stream))))

(defun %fmt-op-r (stream arg radix colon at mincol padchar commachar interval)
  (when (and radix (not (<= 2 radix 36))) (%fmt-error "radix must be between 2 and 36: ~D" radix))
  (cond (radix (%fmt-integer stream arg radix colon at mincol padchar commachar interval))
        ((not (integerp arg)) (let ((*print-base* 10) (*print-radix* nil)) (princ arg stream)))
        ((and at colon) (%fmt-roman stream arg t))
        (at (%fmt-roman stream arg nil))
        (colon (%fmt-ordinal stream arg))
        (t (%fmt-cardinal stream arg))))

(defun %fmt-plural (stream arg at)
  (if at
      (write-string (if (eql arg 1) "y" "ies") stream)
      (unless (eql arg 1) (write-char #\s stream))))

(defun %fmt-previous-arg (orig args)
  "The argument before ARGS in ORIG (~:P)."
  (let ((pos (- (%fmt-list-length orig) (%fmt-list-length args) 1)))
    (when (< pos 0) (%fmt-error "no previous argument"))
    (nth pos orig)))

(defun %fmt-op-c (stream c colon at)
  (unless (characterp c) (error 'type-error :datum c :expected-type 'character))
  (cond (colon (let ((n (and (or (not (graphic-char-p c)) (char= c #\Space)) (char-name c))))
                 (if n (write-string n stream) (write-char c stream))))
        (at (let ((*print-escape* t) (*print-readably* nil)) (prin1 c stream)))
        (t (%stream-write-char stream c))))

(defun %fmt-newlines (stream n) (dotimes (k n) (terpri stream)))

(defun %fmt-fresh-lines (stream n)
  (when (> n 0) (fresh-line stream) (dotimes (k (- n 1)) (terpri stream))))

(defun %fmt-skip-args (args n)
  "~n*: the arguments after skipping N."
  (dotimes (k n)
    (if (consp args) (setq args (cdr args)) (%fmt-arg-error args)))
  args)

(defun %fmt-goto-arg (orig n)
  "~n@*"
  (if (<= 0 n (%fmt-list-length orig))
      (nthcdr n orig)
      (%fmt-error "~~@*: index ~D out of bounds" n)))

(defun %fmt-backup-args (orig args n)
  "~n:*"
  (let ((pos (- (%fmt-list-length orig) (%fmt-list-length args) n)))
    (if (<= 0 pos (%fmt-list-length orig))
        (nthcdr pos orig)
        (%fmt-error "~~:*: cannot back up ~D arguments" n))))

(defun %fmt-check-control (control what)
  (unless (or (stringp control) (functionp control))
    (%fmt-error "~A argument must be a string or function: ~S" what control))
  control)

(defun %fmt-indirect (stream control sub)
  "~?"
  (%fmt-check-control control "~?")
  (unless (listp sub) (%fmt-error "~~? argument must be a list: ~S" sub))
  (%format stream control sub))

(defun %fmt-indirect-at (stream control args orig)
  "~@?: returns the remaining arguments."
  (%fmt-check-control control "~?")
  (%format stream control args orig))

(defun %fmt-bad-index (index)
  (%fmt-error "~~[ argument must be an integer: ~S" index))

(defun %fmt-up-test (a1 a2 a3)
  "The test of ~^ with parameters."
  (cond (a3 (and (realp a1) (realp a2) (realp a3) (<= a1 a2 a3)))
        (a2 (eql a1 a2))
        (t (eql a1 0))))

(defun %fmt-slash-symbol (name)
  (let* ((colon-pos (position #\: name))
         (pkg-name (if colon-pos (string-upcase (subseq name 0 colon-pos)) "COMMON-LISP-USER"))
         (sym-name (string-upcase
                    (if colon-pos
                        (subseq name (if (and (< (+ colon-pos 1) (length name))
                                              (char= (char name (+ colon-pos 1)) #\:))
                                         (+ colon-pos 2) (+ colon-pos 1)))
                        name)))
         (pkg (find-package pkg-name)))
    (unless pkg (%fmt-error "no package named ~S" pkg-name))
    (intern sym-name pkg)))

;;; tabs

(defun %fmt-relative-tab (stream colrel colinc)
  (if (%fmt-pretty-stream-p stream)
      (pprint-tab :line-relative colrel colinc stream)
      (let* ((cur (%stream-column stream))
             (spaces (if (and cur (> colinc 0))
                         (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
                         colrel)))
        (%fmt-spaces stream spaces))))

(defun %fmt-absolute-tab (stream colnum colinc)
  (if (%fmt-pretty-stream-p stream)
      (pprint-tab :line colnum colinc stream)
      (let ((cur (%stream-column stream)))
        (cond ((null cur) (write-string "  " stream))
              ((< cur colnum) (%fmt-spaces stream (- colnum cur)))
              ((> colinc 0) (%fmt-spaces stream (- colinc (rem (- cur colnum) colinc))))))))

;;; ~( ... ~): the text is collected in a string stream, then converted

(defun %fmt-case-flush (stream tmp colon at)
  "Write the text collected in the scratch stream TMP to STREAM in the case
that the modifiers of ~( ask for."
  (let ((s (%scratch-string-result tmp)))
    (write-string (cond ((and colon at) (string-upcase s))
                        (colon (string-capitalize s))
                        (at (let ((d (string-downcase s))
                                  (p (position-if #'alphanumericp s)))
                              (when (and p (alpha-char-p (char d p)))
                                (setf (char d p) (char-upcase (char d p))))
                              d))
                        (t (string-downcase s)))
                  stream)))

;;; ------------------------------------------------------------
;;; interpreter
;;; ------------------------------------------------------------

(defun %fmt-run (stream nodes orig args)
  (dolist (node nodes args)
    (if (stringp node)
        (write-string node stream)
        (setq args (%fmt-directive stream node orig args)))))

(defun %fmt-eval-params (dir raw args max)
  "The values of the parameters RAW of the directive DIR: (values list
remaining-arguments)."
  (when (> (%fmt-list-length raw) max)
    (%fmt-error "too many parameters for ~~~C (at most ~D)" (%fdir-char dir) max))
  (let ((ps nil))
    (dolist (p raw)
      (push (case p
              (:arg (when *fmt-popper* (funcall *fmt-popper*))
               (if (consp args) (pop args) (%fmt-arg-error args)))
              (:remaining (%fmt-list-length args))
              (t p))
            ps))
    (values (nreverse ps) args)))

(defun %fmt-directive (stream dir orig args)
  (let ((ps nil) (char (%fdir-char dir)) (colon (%fdir-colon dir)) (at (%fdir-at dir)))
    (macrolet ((next ()
                 '(progn (when *fmt-popper* (funcall *fmt-popper*))
                         (if (consp args) (pop args) (%fmt-arg-error args))))
               (params (max)
                 `(let ((raw (%fdir-params dir)))
                    (when raw
                      (multiple-value-setq (ps args) (%fmt-eval-params dir raw args ,max)))))
               (int (n default) `(%fmt-int-param (nth ,n ps) ,default char ,n))
               (chr (n default) `(%fmt-char-param (nth ,n ps) ,default char ,n))
               (no-modifiers (what)
                 `(progn (when colon (%fmt-error "~A: colon modifier not allowed" ,what))
                         (when at (%fmt-error "~A: at-sign modifier not allowed" ,what)))))
      (case char
        ((#\A #\S)
         (params 4)
         (let ((arg (next)))
           (if (and (null ps) (not (and colon (null arg))))
               (if (char= char #\A) (princ arg stream) (prin1 arg stream))
               (%fmt-op-a stream arg (char= char #\S) colon at
                          (int 0 0) (int 1 1) (int 2 0) (chr 3 #\Space)))))
        (#\W
         (params 0)
         (%fmt-op-w stream (next) colon at))
        ((#\D #\B #\O #\X)
         (params 4)
         (%fmt-integer stream (next) (case char (#\D 10) (#\B 2) (#\O 8) (t 16))
                       colon at (int 0 0) (chr 1 #\Space) (chr 2 #\,) (int 3 3)))
        (#\R
         (params 5)
         (let ((radix (int 0 nil)))
           (when (and radix (not (<= 2 radix 36))) (%fmt-error "radix must be between 2 and 36: ~D" radix))
           (let ((mincol (int 1 0)) (padchar (chr 2 #\Space)) (commachar (chr 3 #\,)) (interval (int 4 3)))
             (%fmt-op-r stream (next) radix colon at mincol padchar commachar interval))))
        (#\P
         (params 0)
         (%fmt-plural stream (if colon (%fmt-previous-arg orig args) (next)) at))
        (#\C
         (params 0)
         (%fmt-op-c stream (next) colon at))
        (#\F
         (params 5)
         (let* ((w (int 0 nil)) (d (int 1 nil)) (k (int 2 nil)) (ovf (chr 3 nil)) (pad (chr 4 #\Space)))
           (when colon (%fmt-error "~~F: colon modifier not allowed"))
           (%fmt-fixed stream (next) w d k ovf pad at)))
        ((#\E #\G)
         (params 7)
         (let* ((w (int 0 nil)) (d (int 1 nil)) (e (int 2 nil)) (k (int 3 nil))
                (ovf (chr 4 nil)) (pad (chr 5 #\Space)) (marker (chr 6 nil)))
           (when colon (%fmt-error "colon modifier not allowed"))
           (if (char= char #\E)
               (%fmt-exponential stream (next) w d e k ovf pad marker at)
               (%fmt-general stream (next) w d e k ovf pad marker at))))
        (#\$
         (params 4)
         (let* ((d (int 0 2)) (n (int 1 1)) (w (int 2 0)) (pad (chr 3 #\Space)))
           (%fmt-dollar stream (next) d n w pad colon at)))
        (#\%
         (params 1) (no-modifiers "~%")
         (%fmt-newlines stream (int 0 1)))
        (#\&
         (params 1) (no-modifiers "~&")
         (%fmt-fresh-lines stream (int 0 1)))
        (#\|
         (params 1) (no-modifiers "~|")
         (dotimes (k (int 0 1)) (write-char (code-char 12) stream)))
        (#\~
         (params 1) (no-modifiers "~~")
         (dotimes (k (int 0 1)) (write-char #\~ stream)))
        (#\Newline
         (params 0)
         (when (and colon at) (%fmt-error "~~<newline>: cannot use both modifiers"))
         (when at (terpri stream)))
        (#\T
         (params 2)
         (if colon
             (pprint-tab (if at :section-relative :section) (int 0 1) (int 1 1) stream)
             (if at
                 (%fmt-relative-tab stream (int 0 1) (int 1 1))
                 (%fmt-absolute-tab stream (int 0 1) (int 1 1)))))
        (#\*
         (params 1)
         (cond ((and colon at) (%fmt-error "~~*: cannot use both modifiers"))
               (at (setq args (%fmt-goto-arg orig (int 0 0))))
               (colon (setq args (%fmt-backup-args orig args (int 0 1))))
               (t (dotimes (k (int 0 1)) (next)))))
        (#\?
         (params 0)
         (when colon (%fmt-error "~~?: colon modifier not allowed"))
         (let ((control (%fmt-check-control (next) "~?")))
           (if at
               (setq args (%fmt-indirect-at stream control args orig))
               (%fmt-indirect stream control (next)))))
        (#\( (params 0)
         (let ((tmp (%scratch-string-stream)))
           (unwind-protect
                (setq args (%fmt-run tmp (car (%fdir-clauses dir)) orig args))
             (%fmt-case-flush stream tmp colon at))))
        (#\[ (params 1)
         (let ((clauses (%fdir-clauses dir)) (seps (%fdir-seps dir)))
           (dolist (s (butlast seps))
             (when (%fdir-colon s) (%fmt-error "only the last ~~; in ~~[ may have a colon")))
           (cond ((and colon at) (%fmt-error "~~[: cannot use both modifiers"))
                 (at (when (cdr clauses) (%fmt-error "~~@[ must have exactly one clause"))
                     (when ps (%fmt-error "~~@[ takes no parameters"))
                     (let* ((prev args) (arg (next)))
                       (when arg
                         (setq args (%fmt-run stream (car clauses) orig prev)))))
                 (colon (unless (= (length clauses) 2) (%fmt-error "~~:[ must have exactly two clauses"))
                        (when ps (%fmt-error "~~:[ takes no parameters"))
                        (setq args (%fmt-run stream (if (next) (cadr clauses) (car clauses)) orig args)))
                 (t (let* ((index (or (int 0 nil) (next)))
                           (default (and seps (%fdir-colon (car (last seps)))))
                           (nclauses (if default (- (length clauses) 1) (length clauses)))
                           (clause (cond ((not (integerp index)) (%fmt-bad-index index))
                                         ((and (>= index 0) (< index nclauses)) (nth index clauses))
                                         (default (car (last clauses)))
                                         (t nil))))
                      (when clause (setq args (%fmt-run stream clause orig args))))))))
        (#\{ (params 1)
         (let* ((max-count (int 0 nil))
                (indirect (and (null (car (%fdir-clauses dir))) (null (cdr (%fdir-clauses dir)))))
                (insides (if indirect (next) (car (%fdir-clauses dir))))
                (closed-with-colon (%fdir-colon (%fdir-close dir))))
           (when indirect (%fmt-check-control insides "~{~}"))
           (if at
               (setq args (%fmt-iterate stream closed-with-colon orig args colon max-count insides indirect))
               (let ((arg (next)))
                 (unless (listp arg) (error 'type-error :datum arg :expected-type 'list))
                 (let ((*fmt-popper* nil))
                   (%fmt-iterate stream closed-with-colon arg arg colon max-count insides indirect))))))
        (#\<
         (if (%fdir-colon (%fdir-close dir))
             (progn
               (when (%fdir-params dir) (%fmt-error "no parameters can be supplied with ~~<...~~:>"))
               (if at
                   (progn (%fmt-logical-block stream dir orig args t) (setq args nil))
                   (%fmt-logical-block stream dir orig (next) nil)))
             (progn
               (params 4)
               (when (%fdir-at (%fdir-close dir)) (%fmt-error "@ modifier not allowed in ~~>"))
               (setq args (%fmt-justify stream dir orig args colon at
                                        (int 0 0) (int 1 1) (int 2 0) (chr 3 #\Space))))))
        (#\^
         (params 3)
         (when at (%fmt-error "~~^: at-sign modifier not allowed"))
         (when (and colon (not *fmt-colon-up-ok*)) (%fmt-error "~~:^ used outside a ~~:{...~~} construct"))
         (let ((a1 (nth 0 ps)) (a2 (nth 1 ps)) (a3 (nth 2 ps)))
           (when (cond ((or a1 a2 a3) (%fmt-up-test a1 a2 a3))
                       (colon (null *fmt-outside-args*))
                       (t (null args)))
             (if colon
                 (throw '%fmt-up-up-and-out *fmt-outside-args*)
                 (throw '%fmt-up-and-out args)))))
        (#\_
         (params 0)
         (pprint-newline (cond ((and colon at) :mandatory) (colon :fill) (at :miser) (t :linear)) stream))
        (#\I
         (params 1)
         (when at (%fmt-error "~~I: at-sign modifier not allowed"))
         (pprint-indent (if colon :current :block) (int 0 0) stream))
        (#\/
         (params 1000)
         (let ((sym (%fmt-slash-symbol (%fdir-name dir))))
           (apply sym stream (next) colon at ps)))
        (t (%fmt-error "directive ~~~C is not allowed here" char))))
    args))

;;; ~{ ... ~} with a control string given at run time, or interpreted

(defun %fmt-iterate (stream closed-with-colon orig args colon max-count insides indirect)
  "Runs the iteration over ARGS; returns the remaining ARGS."
  (let* ((*fmt-colon-up-ok* colon))
    (labels ((guts (orig args)
               (if indirect
                   (%format stream insides args orig)
                   (%fmt-run stream insides orig args)))
             (bind-args (orig args)
               (if colon
                   (let ((arg (if (consp args) (pop args) (%fmt-arg-error args))))
                     (unless (listp arg) (error 'type-error :datum arg :expected-type 'list))
                     (let ((*fmt-popper* nil) (*fmt-outside-args* args))
                       (catch '%fmt-up-and-out (guts arg arg)))
                     args)
                   (guts orig args)))
             (do-loop (orig args)
               (catch (if colon '%fmt-up-up-and-out '%fmt-up-and-out)
                 (loop
                   (when (and (not closed-with-colon) (null args)) (return))
                   (when (and max-count (< (setq max-count (- max-count 1)) 0)) (return))
                   (setq args (bind-args orig args))
                   (when (and closed-with-colon (null args)) (return)))
                 args)))
      (do-loop orig args))))

;;; ~< ... ~:> logical blocks

(defun %fmt-clause-string (clause what)
  (dolist (x clause)
    (unless (stringp x) (%fmt-error "~A of ~~<...~~:> cannot contain format directives" what)))
  (apply #'concatenate 'string clause))

(defun %fmt-fill-literal (s)
  "Split literal S, inserting a fill newline after each group of blanks."
  (let ((out nil) (pos 0) (end (length s)))
    (loop
      (let ((blank (position #\Space s :start pos)))
        (when (null blank)
          (when (< pos end) (push (subseq s pos) out))
          (return))
        (let ((non-blank (or (position #\Space s :start blank :test #'char/=) end)))
          (push (subseq s pos non-blank) out)
          (push (%make-fdir #\_ t nil nil 0 0) out)
          (setq pos non-blank)
          (when (= pos end) (return)))))
    (nreverse out)))

(defun %fmt-add-fill (nodes)
  (let ((out nil) (prev nil))
    (dolist (n nodes (nreverse out))
      (cond ((stringp n)
             (if (and prev (eql (%fdir-char prev) #\Newline))
                 (let ((nb (position #\Space n :test #'char/=)))
                   (if nb
                       (progn (push (subseq n 0 nb) out)
                              (dolist (x (%fmt-fill-literal (subseq n nb))) (push x out)))
                       (push n out)))
                 (dolist (x (%fmt-fill-literal n)) (push x out))))
            ((%fdir-clauses n)
             (let ((copy (copy-%fdir n)))
               (setf (%fdir-clauses copy) (mapcar #'%fmt-add-fill (%fdir-clauses n)))
               (push copy out)))
            (t (push n out)))
      (setq prev (if (stringp n) nil n)))))

(defun %fmt-logical-block (stream dir orig arg at)
  (let* ((clauses (%fdir-clauses dir)) (seps (%fdir-seps dir))
         (colon (%fdir-colon dir))
         (n (length clauses))
         (prefix (if (>= n 2) (%fmt-clause-string (first clauses) "prefix") (if colon "(" "")))
         (per-line (and (>= n 2) (%fdir-at (first seps))))
         (body (if (>= n 2) (second clauses) (first clauses)))
         (suffix (if (= n 3) (%fmt-clause-string (third clauses) "suffix") (if colon ")" ""))))
    (when (> n 3) (%fmt-error "too many segments for ~~<...~~:>"))
    (when (%fdir-at (%fdir-close dir)) (setq body (%fmt-add-fill body)))
    (progn
      (if per-line
          (pprint-logical-block (stream arg :per-line-prefix prefix :suffix suffix)
            (let ((*fmt-popper* (lambda () (pprint-pop))))
              (catch '%fmt-up-and-out
                (%fmt-run stream body (if at orig arg) arg))))
          (pprint-logical-block (stream arg :prefix prefix :suffix suffix)
            (let ((*fmt-popper* (lambda () (pprint-pop))))
              (catch '%fmt-up-and-out
                (%fmt-run stream body (if at orig arg) arg)))))
      nil)))

;;; ~< ... ~> justification

(defun %fmt-justify (stream dir orig args colon at mincol colinc minpad padchar)
  (let* ((clauses (%fdir-clauses dir))
         (first-sep (car (%fdir-seps dir)))
         (newline-string nil) (strings nil) (extra-space 0) (line-len 0))
    (setq args
          (catch '%fmt-up-and-out
            (when (and first-sep (%fdir-colon first-sep))
              (let* ((raw (%fdir-params first-sep))
                     (vals (mapcar (lambda (p)
                                     (case p
                                       (:arg (when *fmt-popper* (funcall *fmt-popper*))
                                        (cond ((consp args) (pop args))
                     ((null args) (%fmt-error "no more arguments"))
                     (t (error 'type-error :datum args :expected-type 'list))))
                                       (:remaining (%fmt-list-length args))
                                       (t p)))
                                   raw)))
                (when (> (length vals) 2) (%fmt-error "too many parameters for ~~:;"))
                (let ((tmp (make-string-output-stream)))
                  (setq args (%fmt-run tmp (pop clauses) orig args))
                  (setq newline-string (get-output-stream-string tmp)))
                (setq extra-space (or (first vals) 0)
                      line-len (or (second vals) *print-right-margin* 72))))
            (dolist (clause clauses)
              (let ((tmp (make-string-output-stream)))
                (setq args (%fmt-run tmp clause orig args))
                (push (get-output-stream-string tmp) strings)))
            args))
    (%fmt-justification stream newline-string extra-space line-len (nreverse strings)
                        colon at mincol colinc minpad padchar)
    args))

(defun %fmt-justification (stream newline-prefix extra-space line-len strings
                           pad-left pad-right mincol colinc minpad padchar)
  (let* ((num-gaps (+ (- (length strings) 1) (if pad-left 1 0) (if pad-right 1 0)))
         (num-gaps (max num-gaps 0))
         (chars (+ (* num-gaps minpad) (let ((s 0)) (dolist (x strings s) (setq s (+ s (length x)))))))
         (length (if (and (> chars mincol) (> colinc 0))
                     (+ mincol (* (ceiling (- chars mincol) colinc) colinc))
                     (max mincol chars)))
         (padding (+ (- length chars) (* num-gaps minpad))))
    (when (and newline-prefix
               (> (+ (or (%stream-column stream) 0) length extra-space) line-len))
      (write-string newline-prefix stream))
    (flet ((do-padding ()
             (let ((pad-len (if (<= num-gaps 0) padding (truncate padding num-gaps))))
               (setq padding (- padding pad-len) num-gaps (- num-gaps 1))
               (dotimes (i pad-len) (write-char padchar stream)))))
      (when (or pad-left (and (not pad-right) (null (cdr strings))))
        (do-padding))
      (when strings
        (write-string (car strings) stream)
        (dolist (s (cdr strings))
          (do-padding)
          (write-string s stream)))
      (when pad-right (do-padding)))))

;;; ------------------------------------------------------------
;;; integers
;;; ------------------------------------------------------------

(defun %fmt-add-commas (s comma interval)
  (let ((r nil) (n 0))
    (do ((i (- (length s) 1) (- i 1))) ((< i 0))
      (when (and (> n 0) (> interval 0) (= (mod n interval) 0)) (push comma r))
      (push (char s i) r)
      (setq n (+ n 1)))
    (coerce r 'string)))

(defun %fmt-comma-digits (buf start end comma interval sign)
  "A fresh string of the digits START..END of BUF in groups of INTERVAL
separated by COMMA, after the character SIGN (or NIL)."
  (declare (fixnum start end interval))
  (let* ((nd (- end start))
         (commas (if (> interval 0) (%fxtruncate (- nd 1) interval) 0))
         (len (+ nd commas (if sign 1 0)))
         (r (%make-string len #\0))
         (o len) (n 0))
    (declare (fixnum nd commas len o n))
    (do ((i (- end 1) (- i 1))) ((< i start))
      (declare (fixnum i))
      (when (and (> n 0) (> interval 0) (eql (%fxrem n interval) 0))
        (setq o (- o 1))
        (%set-schar r o comma))
      (setq o (- o 1))
      (%set-schar r o (%schar buf i))
      (setq n (+ n 1)))
    (when sign (%set-schar r 0 sign))
    r))

(defun %fmt-integer (stream arg base colon at mincol padchar commachar interval)
  (if (integerp arg)
      (let* ((buf *%digit-buffer*) (end (%string-length buf))
             (i (and (fixnump interval) (%integer-digits arg base buf)))
             (sign (cond ((< arg 0) #\-) (at #\+) (t nil))))
        (cond ((null i)
               ;; an integer too large for the digit buffer
               (let* ((s (%integer-to-string (abs arg) base))
                      (s (if colon (%fmt-add-commas s commachar interval) s))
                      (s (if sign (concatenate 'string (string sign) s) s)))
                 (%fmt-write-field stream s mincol 1 0 padchar t)))
              (colon
               (%fmt-write-field stream (%fmt-comma-digits buf i end commachar interval sign)
                                 mincol 1 0 padchar t))
              (t
               (when sign (setq i (- i 1)) (%set-schar buf i sign))
               (let ((pad (- mincol (- end i))))
                 (cond ((<= pad 0) (%stream-write-scratch stream buf i end))
                       ((<= pad i)
                        ;; the padding fits in front of the digits in the buffer
                        (do ((k (- i pad) (+ k 1))) ((>= k i))
                          (%set-schar buf k padchar))
                        (%stream-write-scratch stream buf (- i pad) end))
                       (t (%fmt-write-field stream (%simple-substring buf i end) mincol 1 0 padchar t)))))))
      (let ((*print-base* base) (*print-radix* nil) (*print-escape* nil) (*print-readably* nil))
        (princ arg stream))))

(defun %fmt-decimal (arg stream)
  (if (integerp arg)
      (%write-integer-digits arg 10 stream)
      (%fmt-integer stream arg 10 nil nil 0 #\Space #\, 3)))

(defparameter *fmt-ones* #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
                            "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
                            "eighteen" "nineteen"))
(defparameter *fmt-tens* #("" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))
(defparameter *fmt-illions* #("" " thousand" " million" " billion" " trillion" " quadrillion"
                               " quintillion" " sextillion" " septillion" " octillion" " nonillion"
                               " decillion" " undecillion" " duodecillion" " tredecillion"
                               " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
                               " octodecillion" " novemdecillion" " vigintillion"))

(defun %fmt-cardinal-string (n)
  (cond ((< n 0) (concatenate 'string "negative " (%fmt-cardinal-string (- n))))
        ((< n 20) (svref *fmt-ones* n))
        ((< n 100) (let ((tens (svref *fmt-tens* (floor n 10))) (ones (mod n 10)))
                     (if (= ones 0) tens (concatenate 'string tens "-" (svref *fmt-ones* ones)))))
        ((< n 1000) (let ((h (floor n 100)) (r (mod n 100)))
                      (concatenate 'string (svref *fmt-ones* h) " hundred"
                                   (if (= r 0) "" (concatenate 'string " " (%fmt-cardinal-string r))))))
        (t (when (>= n (expt 10 (* 3 (length *fmt-illions*))))
             (%fmt-error "number too large to print in English: ~:D" n))
           (let ((parts nil) (i 0))
             (do () ((= n 0))
               (let ((chunk (mod n 1000)))
                 (unless (= chunk 0)
                   (push (concatenate 'string (%fmt-cardinal-string chunk) (svref *fmt-illions* i)) parts)))
               (setq n (floor n 1000) i (+ i 1)))
             (let ((r (car parts)))
               (dolist (p (cdr parts) r) (setq r (concatenate 'string r " " p))))))))

(defun %fmt-cardinal (stream n) (write-string (%fmt-cardinal-string n) stream))

(defun %fmt-ordinal (stream n)
  (let* ((s (%fmt-cardinal-string n))
         (irregular '(("one" . "first") ("two" . "second") ("three" . "third") ("five" . "fifth")
                      ("eight" . "eighth") ("nine" . "ninth") ("twelve" . "twelfth"))))
    (let* ((sp (position-if (lambda (c) (member c '(#\Space #\-))) s :from-end t))
           (last-word (if sp (subseq s (+ sp 1)) s))
           (prefix (if sp (subseq s 0 (+ sp 1)) ""))
           (e (assoc last-word irregular :test #'string=)))
      (write-string prefix stream)
      (write-string (cond (e (cdr e))
                          ((char= (char last-word (- (length last-word) 1)) #\y)
                           (concatenate 'string (subseq last-word 0 (- (length last-word) 1)) "ieth"))
                          (t (concatenate 'string last-word "th")))
                    stream))))

(defun %fmt-roman (stream n old)
  (unless (and (integerp n) (if old (< 0 n 5000) (< 0 n 4000)))
    (%fmt-error "number out of range for Roman numerals: ~S" n))
  (let ((table (if old
                   '((1000 . "M") (500 . "D") (100 . "C") (50 . "L") (10 . "X") (5 . "V") (1 . "I"))
                   '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD") (100 . "C") (90 . "XC")
                     (50 . "L") (40 . "XL") (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I")))))
    (dolist (e table)
      (do () ((< n (car e))) (write-string (cdr e) stream) (setq n (- n (car e)))))))

;;; ------------------------------------------------------------
;;; floats
;;;
;;; The digits come from the shortest digit string that identifies the float
;;; (as the printer uses).  When a directive asks for fewer digits, the value
;;; is rounded at that position, halves up, as the exact binary value says.
;;; The exact value and the shortest digits lie on the same side of every
;;; rounding boundary unless the shortest digits are the boundary itself
;;; (they end in a 5 just after the last digit kept): only then the exact
;;; rational arithmetic decides.
;;; ------------------------------------------------------------

(defun %fmt-nonfinite-p (x)
  (and (floatp x) (let ((d (%to-double x))) (or (%double-nan-p d) (%float-infinite-p d)))))

(defun %fmt-float-arg (x)
  "Rationals are printed as single floats (or double floats if too large)."
  (if (rationalp x)
      (let ((f (coerce x 'single-float)))
        (if (%fmt-nonfinite-p f) (coerce x 'double-float) f))
      x))

(defun %fmt-zeros (n) (%make-string (if (> n 0) n 0) #\0))

(defun %fmt-cat (a b)
  "A fresh string of the simple strings A and B."
  (let* ((la (%string-length a)) (lb (%string-length b)) (r (%make-string (+ la lb) #\0)))
    (%string-copy r 0 a 0 la)
    (%string-copy r la b 0 lb)
    r))

(defun %fmt-round (r p)
  "Round the nonnegative rational R * 10^P to an integer, halves up."
  (floor (+ (* r (expt 10 p)) 1/2)))

(defun %fmt-round-digits (digits n c)
  "The digit string of the integer made of the first C of the N digits in
DIGITS, rounded by the digits that follow (halves up); NIL when the digits
that follow are exactly a half.  C is less than N."
  (declare (fixnum n c))
  (cond ((< c 0) "0")
        ((and (eq (%schar digits c) #\5) (eql (+ c 1) n)) nil)
        ((char< (%schar digits c) #\5)
         (if (eql c 0) "0" (%simple-substring digits 0 c)))
        (t
         ;; add one to the digits kept
         (let ((i (- c 1)))
           (declare (fixnum i))
           (do () ((or (< i 0) (not (eq (%schar digits i) #\9)))) (setq i (- i 1)))
           (if (< i 0)
               ;; all nines: 1 and C zeros
               (let ((r (%make-string (+ c 1) #\0)))
                 (%set-schar r 0 #\1)
                 r)
               (let ((r (%make-string c #\0)))
                 (%string-copy r 0 digits 0 i)
                 (%set-schar r i (code-char (+ (char-code (%schar digits i)) 1)))
                 r))))))

(defun %fmt-fixed-parts (x k d)
  "Digits of |X| * 10^K: (values integer-part fraction-part); integer part \"\"
when zero.  D is the number of fraction digits, or NIL for the shortest."
  (if (zerop x)
      (values "" (if d (%fmt-zeros d) ""))
      (let ((digits (%make-string 24 #\0)))
        (multiple-value-bind (n e) (%float-digits-into x digits)
          (declare (fixnum n e))
          (let* ((e (+ e k))
                 (sfrac (max 0 (- n e))))
            (if (or (null d) (>= d sfrac))
                (let ((int (cond ((<= e 0) "")
                                 ((>= e n) (%fmt-cat (%simple-substring digits 0 n) (%fmt-zeros (- e n))))
                                 (t (%simple-substring digits 0 e))))
                      (frac (cond ((<= e 0) (%fmt-cat (%fmt-zeros (- e)) (%simple-substring digits 0 n)))
                                  ((>= e n) "")
                                  (t (%simple-substring digits e n)))))
                  (values int (if (and d (> d (length frac)))
                                  (%fmt-cat frac (%fmt-zeros (- d (length frac))))
                                  frac)))
                (let ((s (or (and (fixnump d) (fixnump e) (%fmt-round-digits digits n (+ e d)))
                             (%integer-to-string (%fmt-round (* (rational x) (expt 10 k)) d) 10))))
                  (when (< (length s) (+ d 1))
                    (setq s (%fmt-cat (%fmt-zeros (- (+ d 1) (length s))) s)))
                  (let ((int (string-left-trim "0" (subseq s 0 (- (length s) d)))))
                    (values int (subseq s (- (length s) d)))))))))))

(defun %fmt-strip-trailing-zeros (s)
  (string-right-trim "0" s))

(defun %fmt-number-as-string (x)
  (let ((*print-base* 10) (*print-radix* nil)) (princ-to-string x)))

(defun %fmt-fixed (stream x w d k ovf pad at)
  (cond ((not (realp x))
         (%fmt-write-field stream (%fmt-number-as-string x) w 1 0 #\Space nil))
        (t (let ((x (%fmt-float-arg x)))
             (if (%fmt-nonfinite-p x)
                 (%fmt-write-field stream (prin1-to-string x) w 1 0 #\Space t)
                 (%fmt-fixed-aux stream x w d k ovf pad at))))))

(defun %fmt-fixed-aux (stream x w d k ovf pad at)
  (let* ((k (or k 0))
         (neg (minusp (float-sign x)))
         (ax (abs x))
         (sign (cond (neg "-") (at "+") (t ""))))
    (multiple-value-bind (int frac)
        (cond (d (%fmt-fixed-parts ax k d))
              (w (multiple-value-bind (i0 f0) (%fmt-fixed-parts ax k nil)
                   (let ((avail (max 0 (- w (length sign) 1 (length i0)))))
                     (if (<= (length f0) avail)
                         (values i0 f0)
                         (multiple-value-bind (i1 f1) (%fmt-fixed-parts ax k avail)
                           (values i1 (%fmt-strip-trailing-zeros f1)))))))
              (t (%fmt-fixed-parts ax k nil)))
      (when (and (null d) (eql (length frac) 0)) (setq frac "0"))
      (when (and (eql (length int) 0)
                 (or (null w) (eql (length frac) 0) (<= (+ (length sign) 2 (length frac)) w)))
        (setq int "0"))
      (let ((len (+ (length sign) (length int) 1 (length frac))))
        (if (and w ovf (> len w))
            (%write-chars stream ovf w)
            (progn (when w (%write-chars stream pad (- w len)))
                   (%fmt-put stream sign)
                   (%fmt-put stream int)
                   (%stream-write-char stream #\.)
                   (%fmt-put stream frac)))))))

(defun %fmt-exp-marker (x)
  (let ((m (%float-exponent-marker x)))
    (if m (char m 0) #\e)))

(defun %fmt-sig-digits (ax digits expo n)
  "N significant digits of AX (= 0.DIGITS * 10^EXPO): (values digit-string expo)."
  (if (>= n (length digits))
      (values (%fmt-cat digits (%fmt-zeros (- n (length digits)))) expo)
      (let ((s (or (and (fixnump n) (%fmt-round-digits digits (length digits) n))
                   (%integer-to-string (%fmt-round (rational ax) (- n expo)) 10))))
        (if (> (length s) n)
            (values (subseq s 0 n) (+ expo 1))
            (values (%fmt-cat (%fmt-zeros (- n (length s))) s) expo)))))

(defun %fmt-exponential (stream x w d e k ovf pad marker at)
  (cond ((not (realp x))
         (%fmt-write-field stream (%fmt-number-as-string x) w 1 0 #\Space nil))
        (t (let ((x (%fmt-float-arg x)))
             (if (%fmt-nonfinite-p x)
                 (%fmt-write-field stream (prin1-to-string x) w 1 0 #\Space t)
                 (%fmt-exp-aux stream x w d e k ovf pad marker at))))))

(defun %fmt-exp-mantissa (digits k)
  "Split significant DIGITS for scale K: (values int frac)."
  (if (> k 0)
      (let ((digits (if (< (length digits) k)
                        (%fmt-cat digits (%fmt-zeros (- k (length digits))))
                        digits)))
        (values (subseq digits 0 k) (subseq digits k)))
      (values "" (%fmt-cat (%fmt-zeros (- k)) digits))))

(defun %fmt-exp-string (ex e)
  "The exponent EX with its sign, with at least E digits."
  (let ((s (%integer-to-string (abs ex) 10)))
    (%fmt-cat (if (minusp ex) "-" "+")
              (if (and e (< (length s) e)) (%fmt-cat (%fmt-zeros (- e (length s))) s) s))))

(defun %fmt-exp-aux (stream x w d e k ovf pad marker at)
  (let* ((k (or k 1))
         (neg (minusp (float-sign x)))
         (ax (abs x))
         (sign (cond (neg "-") (at "+") (t "")))
         (marker (or marker (%fmt-exp-marker x)))
         (sdigits "0") (sexpo k))
    (unless (zerop ax)
      (multiple-value-setq (sdigits sexpo) (%float-digits-and-exponent ax)))
    (let (digits expo int frac)
      (cond
        (d (let ((n (if (> k 0) (+ d 1) (+ d k))))
             (if (zerop ax)
                 (setq digits (%fmt-zeros (max n 1)) expo k)
                 (multiple-value-setq (digits expo) (%fmt-sig-digits ax sdigits sexpo (max n 1)))))
           (multiple-value-setq (int frac) (%fmt-exp-mantissa digits k))
           (when (and (<= k 0) (> (length frac) d)) (setq frac (subseq frac 0 d))))
        (t
         (setq digits sdigits expo sexpo)
         (multiple-value-setq (int frac) (%fmt-exp-mantissa digits k))
         (when w
           (let* ((explen (+ 1 (length (%fmt-exp-string (- expo k) e))))
                  (avail (max 0 (- w (length sign) (max (length int) (if (> k 0) 0 0)) 1 explen))))
             (when (> (length frac) avail)
               (let ((n (if (> k 0) (+ k avail) (+ avail k))))
                 (multiple-value-setq (digits expo) (%fmt-sig-digits ax sdigits sexpo (max n 1)))
                 (multiple-value-setq (int frac) (%fmt-exp-mantissa digits k))
                 (setq frac (%fmt-strip-trailing-zeros frac))))))
         (when (and (> k 0) (eql (length frac) 0))
           (when (or (null w)
                     (<= (+ (length sign) (length int) 2 1 (length (%fmt-exp-string (- expo k) e))) w))
             (setq frac "0")))))
      (let* ((ex (- expo k))
             (es (%fmt-exp-string ex e)))
        (when (and (eql (length int) 0)
                   (or (null w) (<= (+ (length sign) 2 (length frac) 1 (length es)) w)))
          (setq int "0"))
        (let ((len (+ (length sign) (length int) 1 (length frac) 1 (length es))))
          (if (and ovf (or (and w (> len w))
                           (and e (> (- (length es) 1) e))))
              (%write-chars stream ovf (or w len))
              (progn (when w (%write-chars stream pad (- w len)))
                     (%fmt-put stream sign)
                     (%fmt-put stream int)
                     (%stream-write-char stream #\.)
                     (%fmt-put stream frac)
                     (%stream-write-char stream marker)
                     (%fmt-put stream es))))))))

(defun %fmt-general (stream x w d e k ovf pad marker at)
  (cond ((not (realp x))
         (%fmt-write-field stream (%fmt-number-as-string x) w 1 0 #\Space nil))
        (t (let ((x (%fmt-float-arg x)))
             (if (%fmt-nonfinite-p x)
                 (%fmt-write-field stream (prin1-to-string x) w 1 0 #\Space t)
                 (multiple-value-bind (digits n)
                     (if (zerop x) (values "0" 0) (%float-digits-and-exponent (abs x)))
                   (let* ((ee (if e (+ e 2) 4))
                          (ww (if w (- w ee) nil))
                          (q (length digits))
                          (d (or d (max q (min n 7))))
                          (dd (- d n)))
                     (if (<= 0 dd d)
                         (progn (%fmt-fixed-aux stream x ww dd nil ovf pad at)
                                (%fmt-spaces stream ee))
                         (%fmt-exp-aux stream x w d e k ovf pad marker at)))))))))

(defun %fmt-dollar (stream x d n w pad colon at)
  (if (not (realp x))
      (%fmt-write-field stream (%fmt-number-as-string x) w 1 0 #\Space nil)
      (let ((x (%fmt-float-arg x)))
        (if (%fmt-nonfinite-p x)
            (%fmt-write-field stream (prin1-to-string x) w 1 0 #\Space t)
            (let* ((neg (minusp (float-sign x)))
                   (sign (cond (neg "-") (at "+") (t ""))))
              (multiple-value-bind (int frac) (%fmt-fixed-parts (abs x) 0 d)
                (when (< (length int) n)
                  (setq int (%fmt-cat (%fmt-zeros (- n (length int))) int)))
                (let ((padn (- w (length sign) (length int) 1 (length frac))))
                  (if colon
                      (progn (%fmt-put stream sign)
                             (%write-chars stream pad padn))
                      (progn (%write-chars stream pad padn)
                             (%fmt-put stream sign)))
                  (%fmt-put stream int)
                  (%stream-write-char stream #\.)
                  (%fmt-put stream frac))))))))

;;; ------------------------------------------------------------
;;; the compiler: a constant control string becomes Lisp code
;;;
;;; %FMTC-EXPAND returns the forms that perform the output of a control
;;; string, or NIL when the string has to be interpreted: it has a syntax
;;; error (which is signalled when the FORMAT call runs), or uses ~<...~>.
;;;
;;; The arguments of a FORMAT call are variables.  As long as the position
;;; in the argument list is known at compile time ("static" mode,
;;; *FMTC-POS* is the index of the next argument), a directive takes its
;;; argument from the variable.  When the position depends on the run of
;;; the program (~@{, ~* with a parameter that is an argument, clauses of ~[
;;; that take different numbers of arguments ...), the rest of the string is
;;; compiled in "dynamic" mode: the arguments are in a list, as in the
;;; interpreter, and so are the elements inside ~{...~}.
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *fmtc-stream* nil)              ; the variable of the stream
(defvar *fmtc-vars* nil)                ; static mode: the variables of all arguments
(defvar *fmtc-pos* nil)                 ; static mode: index of the next argument; NIL: dynamic mode
(defvar *fmtc-args* nil)                ; dynamic mode: the variable of the remaining arguments
(defvar *fmtc-orig* nil)                ; dynamic mode: the variable of all arguments
(defvar *fmtc-up* nil)                  ; the block that ~^ leaves
(defvar *fmtc-up-up* nil)               ; inside ~:{: the block that ~:^ leaves
(defvar *fmtc-outer* nil)               ; inside ~:{: the variable of the remaining sublists

(defun %fmtc-fail ()
  "The control string is left to the interpreter."
  (throw '%fmtc-fail nil))

(defun %fmtc-dynamic ()
  "Static mode cannot go on: the directive is compiled again in dynamic mode."
  (if *fmtc-pos* (throw '%fmtc-dynamic nil) (%fmtc-fail)))

(defun %fmtc-next ()
  "A form for the next argument."
  (if *fmtc-pos*
      (if (< *fmtc-pos* (length *fmtc-vars*))
          (prog1 (nth *fmtc-pos* *fmtc-vars*) (setq *fmtc-pos* (+ *fmtc-pos* 1)))
          (%fmtc-dynamic))               ; no more arguments: the error is signalled at run time
      `(progn (unless (consp ,*fmtc-args*) (%fmt-arg-error ,*fmtc-args*))
              (pop ,*fmtc-args*))))

(defun %fmtc-params (dir specs)
  "The parameters of DIR; SPECS is a list of (kind default) with kind :int,
:char or :any.  Returns (values bindings value-forms): parameters that are
arguments (V) or the number of remaining arguments (#) are bound to
variables, in order."
  (let ((raw (%fdir-params dir)) (bindings nil) (vals nil) (n 0))
    (when (> (length raw) (length specs)) (%fmtc-fail))
    (dolist (spec specs)
      (let ((p (nth n raw)) (kind (car spec)) (default (cadr spec)))
        (cond ((null p) (push default vals))
              ((eq p :arg)
               (let ((g (gensym "P")) (arg (%fmtc-next)))
                 (push (list g (case kind
                                 (:int `(%fmt-int-param ,arg ,default ,(%fdir-char dir) ,n))
                                 (:char `(%fmt-char-param ,arg ,default ,(%fdir-char dir) ,n))
                                 (t arg)))
                       bindings)
                 (push g vals)))
              ((eq p :remaining)
               (when (eq kind :char) (%fmtc-fail))
               (if *fmtc-pos*
                   (push (- (length *fmtc-vars*) *fmtc-pos*) vals)
                   (let ((g (gensym "P")))
                     (push (list g `(%fmt-list-length ,*fmtc-args*)) bindings)
                     (push g vals))))
              ((eq kind :int) (unless (integerp p) (%fmtc-fail)) (push p vals))
              ((eq kind :char) (unless (characterp p) (%fmtc-fail)) (push p vals))
              (t (push p vals))))
      (setq n (+ n 1)))
    (values (nreverse bindings) (nreverse vals))))

(defun %fmtc-with (bindings form)
  (if bindings (list `(let* ,bindings ,form)) (list form)))

(defun %fmtc-with-arg (bindings fn)
  "The forms of a directive that takes one argument after its parameters;
FN makes the form from the form of the argument."
  (let ((arg (%fmtc-next)))
    (if (or (null bindings) (symbolp arg))
        (%fmtc-with bindings (funcall fn arg))
        (let ((g (gensym "ARG")))
          (list `(let* (,@bindings (,g ,arg)) ,(funcall fn g)))))))

(defun %fmtc-seq (nodes)
  "The forms for the list NODES of strings and directives."
  (let ((forms nil))
    (dolist (node nodes (nreverse forms))
      (dolist (f (%fmtc-node node)) (push f forms)))))

(defun %fmtc-top (nodes)
  "The forms for the whole control string, starting in static mode."
  (let ((forms nil))
    (do ((l nodes (cdr l))) ((null l) (nreverse forms))
      (let* ((pos *fmtc-pos*)
             (r (catch '%fmtc-dynamic (cons t (%fmtc-node (car l))))))
        (if r
            (dolist (f (cdr r)) (push f forms))
            ;; this directive and the following ones in dynamic mode
            (let ((orig (gensym "ORIG")) (args (gensym "ARGS")))
              (setq *fmtc-pos* nil *fmtc-args* args *fmtc-orig* orig)
              (push `(let* ((,orig (list ,@*fmtc-vars*)) (,args (nthcdr ,pos ,orig)))
                       (declare (ignorable ,orig))
                       ,@(%fmtc-seq l))
                    forms)
              (return (nreverse forms))))))))

(defun %fmtc-clauses (clauses)
  "The forms of each clause in CLAUSES, all compiled from the same argument
position.  In static mode they must all take the same number of arguments."
  (if *fmtc-pos*
      (let ((start *fmtc-pos*) (end nil) (r nil))
        (dolist (c clauses)
          (setq *fmtc-pos* start)
          (push (%fmtc-seq c) r)
          (cond ((null end) (setq end *fmtc-pos*))
                ((/= end *fmtc-pos*) (%fmtc-dynamic))))
        (setq *fmtc-pos* (or end start))
        (nreverse r))
      (mapcar #'%fmtc-seq clauses)))

(defun %fmtc-indirection-p (nodes)
  "True when NODES contain ~? or ~{~}, whose control string is not known."
  (dolist (n nodes nil)
    (unless (stringp n)
      (when (or (eql (%fdir-char n) #\?)
                (and (eql (%fdir-char n) #\{) (null (car (%fdir-clauses n))) (null (cdr (%fdir-clauses n))))
                (dolist (c (%fdir-clauses n) nil)
                  (when (%fmtc-indirection-p c) (return t))))
        (return t)))))

(defun %fmtc-literal (string)
  (let ((s *fmtc-stream*))
    (if (= (length string) 1)
        (list `(%stream-write-char ,s ,(char string 0)))
        (list `(%fmt-put ,s ,string)))))

(defun %fmtc-node (node)
  "The list of forms for one string or directive."
  (if (stringp node)
      (%fmtc-literal (coerce node 'simple-string))
      (%fmtc-directive node)))

(defun %fmtc-directive (dir)
  (let ((s *fmtc-stream*) (char (%fdir-char dir)) (colon (%fdir-colon dir)) (at (%fdir-at dir)))
    (case char
      ((#\A #\S)
       (if (null (%fdir-params dir))
           (%fmtc-with-arg nil (lambda (arg)
                                 (if (char= char #\A)
                                     (if colon `(%fmt-princ-colon ,arg ,s) `(%fmt-princ ,arg ,s))
                                     (if colon `(%fmt-prin1-colon ,arg ,s) `(prin1 ,arg ,s)))))
           (multiple-value-bind (b v) (%fmtc-params dir '((:int 0) (:int 1) (:int 0) (:char #\Space)))
             (%fmtc-with-arg b (lambda (arg) `(%fmt-op-a ,s ,arg ,(char= char #\S) ,colon ,at ,@v))))))
      (#\W
       (%fmtc-params dir '())
       (%fmtc-with-arg nil (lambda (arg) `(%fmt-op-w ,s ,arg ,colon ,at))))
      ((#\D #\B #\O #\X)
       (let ((base (case char (#\D 10) (#\B 2) (#\O 8) (t 16))))
         (multiple-value-bind (b v) (%fmtc-params dir '((:int 0) (:char #\Space) (:char #\,) (:int 3)))
           (%fmtc-with-arg b (lambda (arg)
                               (if (and (null (%fdir-params dir)) (not colon) (not at) (= base 10))
                                   `(%fmt-decimal ,arg ,s)
                                   `(%fmt-integer ,s ,arg ,base ,colon ,at ,@v)))))))
      (#\R
       (multiple-value-bind (b v)
           (%fmtc-params dir '((:int nil) (:int 0) (:char #\Space) (:char #\,) (:int 3)))
         (when (and (integerp (car v)) (not (<= 2 (car v) 36))) (%fmtc-fail))
         (%fmtc-with-arg b (lambda (arg) `(%fmt-op-r ,s ,arg ,(car v) ,colon ,at ,@(cdr v))))))
      (#\P
       (%fmtc-params dir '())
       (if colon
           (list `(%fmt-plural ,s
                               ,(if *fmtc-pos*
                                    (if (> *fmtc-pos* 0) (nth (- *fmtc-pos* 1) *fmtc-vars*) (%fmtc-fail))
                                    `(%fmt-previous-arg ,*fmtc-orig* ,*fmtc-args*))
                               ,at))
           (%fmtc-with-arg nil (lambda (arg) `(%fmt-plural ,s ,arg ,at)))))
      (#\C
       (%fmtc-params dir '())
       (%fmtc-with-arg nil (lambda (arg) `(%fmt-op-c ,s ,arg ,colon ,at))))
      (#\F
       (when colon (%fmtc-fail))
       (multiple-value-bind (b v)
           (%fmtc-params dir '((:int nil) (:int nil) (:int nil) (:char nil) (:char #\Space)))
         (%fmtc-with-arg b (lambda (arg) `(%fmt-fixed ,s ,arg ,@v ,at)))))
      ((#\E #\G)
       (when colon (%fmtc-fail))
       (multiple-value-bind (b v)
           (%fmtc-params dir '((:int nil) (:int nil) (:int nil) (:int nil) (:char nil) (:char #\Space) (:char nil)))
         (%fmtc-with-arg b (lambda (arg)
                             `(,(if (char= char #\E) '%fmt-exponential '%fmt-general) ,s ,arg ,@v ,at)))))
      (#\$
       (multiple-value-bind (b v) (%fmtc-params dir '((:int 2) (:int 1) (:int 0) (:char #\Space)))
         (%fmtc-with-arg b (lambda (arg) `(%fmt-dollar ,s ,arg ,@v ,colon ,at)))))
      (#\%
       (when (or colon at) (%fmtc-fail))
       (multiple-value-bind (b v) (%fmtc-params dir '((:int 1)))
         (%fmtc-with b (if (eql (car v) 1) `(terpri ,s) `(%fmt-newlines ,s ,(car v))))))
      (#\&
       (when (or colon at) (%fmtc-fail))
       (multiple-value-bind (b v) (%fmtc-params dir '((:int 1)))
         (%fmtc-with b (if (eql (car v) 1) `(fresh-line ,s) `(%fmt-fresh-lines ,s ,(car v))))))
      ((#\| #\~)
       (when (or colon at) (%fmtc-fail))
       (multiple-value-bind (b v) (%fmtc-params dir '((:int 1)))
         (let ((c (if (char= char #\|) (code-char 12) #\~)))
           (%fmtc-with b (if (eql (car v) 1) `(%stream-write-char ,s ,c) `(%write-chars ,s ,c ,(car v)))))))
      (#\Newline
       (%fmtc-params dir '())
       (when (and colon at) (%fmtc-fail))
       (if at (list `(terpri ,s)) nil))
      (#\T
       (multiple-value-bind (b v) (%fmtc-params dir '((:int 1) (:int 1)))
         (%fmtc-with b (cond (colon `(pprint-tab ,(if at :section-relative :section) ,@v ,s))
                             (at `(%fmt-relative-tab ,s ,@v))
                             (t `(%fmt-absolute-tab ,s ,@v))))))
      (#\* (%fmtc-jump dir colon at))
      (#\?
       (%fmtc-params dir '())
       (when colon (%fmtc-fail))
       (if at
           (progn
             (when *fmtc-pos* (%fmtc-dynamic))
             (let ((c (gensym "CONTROL")))
               (list `(let ((,c ,(%fmtc-next)))
                        (setq ,*fmtc-args* (%fmt-indirect-at ,s ,c ,*fmtc-args* ,*fmtc-orig*))))))
           (let* ((c (gensym "CONTROL")) (a (gensym "ARGS"))
                  (cform (%fmtc-next)) (aform (%fmtc-next)))
             (list `(let* ((,c ,cform) (,a ,aform)) (%fmt-indirect ,s ,c ,a))))))
      (#\(
       (%fmtc-params dir '())
       (let* ((tmp (gensym "TMP"))
              (body (let ((*fmtc-stream* tmp)) (%fmtc-seq (car (%fdir-clauses dir))))))
         (list `(let ((,tmp (%scratch-string-stream)))
                  (unwind-protect (progn ,@body)
                    (%fmt-case-flush ,s ,tmp ,colon ,at))))))
      (#\[ (%fmtc-conditional dir colon at))
      (#\{ (%fmtc-iteration dir colon at))
      (#\^
       (when at (%fmtc-fail))
       (when (and colon (null *fmtc-up-up*)) (%fmtc-fail))
       (let ((exit (if colon `(return-from ,*fmtc-up-up* nil) `(return-from ,*fmtc-up* nil))))
         (multiple-value-bind (b v) (%fmtc-params dir '((:any nil) (:any nil) (:any nil)))
           ;; the test without parameters (also used when the parameters,
           ;; given as arguments, are all NIL): no arguments remain
           (let ((none (cond (colon `(null ,*fmtc-outer*))
                             (*fmtc-pos* (if (< *fmtc-pos* (length *fmtc-vars*)) nil t))
                             (t `(null ,*fmtc-args*)))))
             (cond ((notevery #'null v)
                    (%fmtc-with b `(when (if (or ,@v) (%fmt-up-test ,@v) ,none) ,exit)))
                   ((null none) nil)
                   ((eq none t) (list exit))
                   (t (list `(when ,none ,exit))))))))
      (#\_
       (%fmtc-params dir '())
       (list `(pprint-newline ,(cond ((and colon at) :mandatory) (colon :fill) (at :miser) (t :linear)) ,s)))
      (#\I
       (when at (%fmtc-fail))
       (multiple-value-bind (b v) (%fmtc-params dir '((:int 0)))
         (%fmtc-with b `(pprint-indent ,(if colon :current :block) ,(car v) ,s))))
      (#\/
       (multiple-value-bind (b v)
           (%fmtc-params dir (mapcar (lambda (p) (declare (ignore p)) '(:any nil)) (%fdir-params dir)))
         (let ((f (gensym "F")) (a (gensym "ARG")))
           (list `(let* (,@b (,f (%fmt-slash-symbol ,(%fdir-name dir))) (,a ,(%fmtc-next)))
                    (funcall ,f ,s ,a ,colon ,at ,@v))))))
      (t (%fmtc-fail)))))

(defun %fmtc-jump (dir colon at)
  "~*"
  (when (and colon at) (%fmtc-fail))
  (let ((p (car (%fdir-params dir))))
    (when (cdr (%fdir-params dir)) (%fmtc-fail))
    (when (and *fmtc-pos* (or (eq p :arg) (eq p :remaining)))
      ;; # is a constant in static mode
      (if (eq p :remaining)
          (setq p (- (length *fmtc-vars*) *fmtc-pos*))
          (%fmtc-dynamic)))
    (if *fmtc-pos*
        (let* ((n (cond ((null p) (if at 0 1))
                        ((integerp p) p)
                        (t (%fmtc-fail))))
               (new (cond (at n) (colon (- *fmtc-pos* n)) (t (+ *fmtc-pos* n)))))
          ;; a jump outside the arguments is an error, signalled at run time
          (unless (and (>= n 0) (<= 0 new (length *fmtc-vars*))) (%fmtc-dynamic))
          (setq *fmtc-pos* new)
          nil)
        (multiple-value-bind (b v) (%fmtc-params dir (list (list :int (if at 0 1))))
          (let ((args *fmtc-args*) (orig *fmtc-orig*))
            (%fmtc-with b (cond (at `(setq ,args (%fmt-goto-arg ,orig ,(car v))))
                                (colon `(setq ,args (%fmt-backup-args ,orig ,args ,(car v))))
                                (t `(setq ,args (%fmt-skip-args ,args ,(car v)))))))))))

(defun %fmtc-conditional (dir colon at)
  "~[...~]"
  (let ((clauses (%fdir-clauses dir)) (seps (%fdir-seps dir)) (s *fmtc-stream*))
    (declare (ignorable s))
    (dolist (sep (butlast seps))
      (when (%fdir-colon sep) (%fmtc-fail)))
    (dolist (sep seps)
      (when (%fdir-params sep) (%fmtc-fail)))
    (cond ((and colon at) (%fmtc-fail))
          (at
           (when (or (cdr clauses) (%fdir-params dir)) (%fmtc-fail))
           (if *fmtc-pos*
               ;; the clause sees the tested argument again
               (let* ((arg (%fmtc-next)) (after *fmtc-pos*))
                 (setq *fmtc-pos* (- after 1))
                 (let ((body (%fmtc-seq (car clauses))))
                   (unless (= *fmtc-pos* after) (%fmtc-dynamic))
                   (list `(when ,arg ,@body))))
               (let ((args *fmtc-args*))
                 (list `(progn
                          (unless (consp ,args) (%fmt-arg-error ,args))
                          (if (car ,args)
                              (progn ,@(%fmtc-seq (car clauses)))
                              (setq ,args (cdr ,args))))))))
          (colon
           (when (or (/= (length clauses) 2) (%fdir-params dir)) (%fmtc-fail))
           (let* ((arg (%fmtc-next))
                  (bodies (%fmtc-clauses clauses)))
             (list `(if ,arg (progn ,@(cadr bodies)) (progn ,@(car bodies))))))
          (t
           (let ((p (car (%fdir-params dir))) (i (gensym "INDEX")))
             (when (cdr (%fdir-params dir)) (%fmtc-fail))
             ;; an index parameter that is an argument may be NIL: then the
             ;; next argument is the index
             (when (and (eq p :arg) *fmtc-pos*) (%fmtc-dynamic))
             (let* ((index (cond ((null p) (%fmtc-next))
                                 ((eq p :arg)
                                  (let ((v (%fmtc-next)))
                                    `(or (%fmt-int-param ,v nil #\[ 0) ,(%fmtc-next))))
                                 ((eq p :remaining)
                                  (if *fmtc-pos*
                                      (- (length *fmtc-vars*) *fmtc-pos*)
                                      `(%fmt-list-length ,*fmtc-args*)))
                                 ((integerp p) p)
                                 (t (%fmtc-fail))))
                    (default (and seps (%fdir-colon (car (last seps)))))
                    ;; without a default clause, an index outside the
                    ;; clauses selects an empty one
                    (bodies (%fmtc-clauses (if default clauses (append clauses (list nil)))))
                    (n 0))
               (list `(let ((,i ,index))
                        (unless (integerp ,i) (%fmt-bad-index ,i))
                        (case ,i
                          ,@(mapcar (lambda (body) (prog1 `(,n ,@body) (setq n (+ n 1))))
                                    (butlast bodies))
                          (t ,@(car (last bodies))))))))))))

(defun %fmtc-iteration (dir colon at)
  "~{...~}"
  (let* ((s *fmtc-stream*)
         (clauses (%fdir-clauses dir))
         (body (car clauses))
         (indirect (and (null body) (null (cdr clauses))))
         (closed-with-colon (%fdir-colon (%fdir-close dir))))
    (when (cdr clauses) (%fmtc-fail))
    (when (and at *fmtc-pos*) (%fmtc-dynamic))
    (when (and (or colon *fmtc-up-up*) (or indirect (%fmtc-indirection-p body)))
      ;; ~:^ in a control string given at run time needs the interpreter's bindings
      (%fmtc-fail))
    (multiple-value-bind (b v) (%fmtc-params dir '((:int nil)))
      (let* ((max (car v))
             (count (gensym "COUNT"))
             (count-check (cond ((null max) nil)
                                ((integerp max) `((when (< (setq ,count (- ,count 1)) 0) (return))))
                                (t `((when (and ,count (< (setq ,count (- ,count 1)) 0)) (return)))))))
        (if indirect
            (let ((ins (gensym "CONTROL")) (insform (%fmtc-next)))
              (if at
                  (list `(let* (,@b (,ins (%fmt-check-control ,insform "~{~}")))
                           (setq ,*fmtc-args*
                                 (%fmt-iterate ,s ,closed-with-colon ,*fmtc-orig* ,*fmtc-args*
                                               ,colon ,max ,ins t))))
                  (let ((l (gensym "LIST")) (lform (%fmtc-next)))
                    (list `(let* (,@b (,ins (%fmt-check-control ,insform "~{~}"))
                                  (,l (%fmt-check-list ,lform)))
                             (%fmt-iterate ,s ,closed-with-colon ,l ,l ,colon ,max ,ins t))))))
            (let* ((up (gensym "UP")) (up-up (gensym "UP-UP"))
                   (l (gensym "LIST")) (orig (gensym "ORIG"))
                   ;; the list that the loop consumes: the argument, or the
                   ;; remaining arguments themselves (~@{)
                   (outer (if at *fmtc-args* (gensym "LISTS")))
                   (outer-form (if at nil (%fmtc-next)))
                   (body-forms
                     (if colon
                         (let ((*fmtc-pos* nil) (*fmtc-args* l) (*fmtc-orig* orig)
                               (*fmtc-up* up) (*fmtc-up-up* up-up) (*fmtc-outer* outer))
                           (%fmtc-seq body))
                         (if at
                             (let ((*fmtc-up* up) (*fmtc-up-up* nil) (*fmtc-outer* nil))
                               (%fmtc-seq body))
                             (let ((*fmtc-pos* nil) (*fmtc-args* outer) (*fmtc-orig* orig)
                                   (*fmtc-up* up) (*fmtc-up-up* nil) (*fmtc-outer* nil))
                               (%fmtc-seq body)))))
                   (step (if colon
                             `((let* ((,l (%fmt-check-list
                                           (progn (unless (consp ,outer) (%fmt-arg-error ,outer))
                                                  (pop ,outer))))
                                      (,orig ,l))
                                 (declare (ignorable ,orig))
                                 (block ,up ,@body-forms)))
                             body-forms))
                   (loop-form
                     `(block ,(if colon up-up up)
                        (loop
                          ,@(unless closed-with-colon `((when (null ,outer) (return))))
                          ,@count-check
                          ,@step
                          ,@(when closed-with-colon `((when (null ,outer) (return))))))))
              (list `(let* (,@b
                            ,@(unless at `((,outer (%fmt-check-list ,outer-form))))
                            ,@(unless (or at colon) `((,orig ,outer)))
                            ,@(when max `((,count ,max))))
                       ,@(unless (or at colon) `((declare (ignorable ,orig))))
                       ,loop-form))))))))

(defun %fmtc-expand (control vars stream top dynamic)
  "The forms that write the control string CONTROL to the stream in the
variable STREAM, or :FAIL.  VARS are the variables of the arguments; with
DYNAMIC, VARS is instead the variable of the list of arguments.  TOP is the
name of a block around the forms."
  (let ((r (catch '%fmtc-fail
             (let ((tree (handler-case (%fmt-parse (coerce control 'simple-string))
                           (error () (%fmtc-fail)))))
               (let ((*fmtc-stream* stream) (*fmtc-up* top) (*fmtc-up-up* nil) (*fmtc-outer* nil))
                 (cons t (if dynamic
                             (let ((*fmtc-vars* nil) (*fmtc-pos* nil) (*fmtc-args* vars) (*fmtc-orig* dynamic))
                               (%fmtc-seq tree))
                             (let ((*fmtc-vars* vars) (*fmtc-pos* 0) (*fmtc-args* nil) (*fmtc-orig* nil))
                               (%fmtc-top tree)))))))))
    (if r (cdr r) :fail)))

)                                       ; eval-when

(defmacro formatter (control)
  (let* ((s (gensym "STREAM")) (a (gensym "ARGS")) (orig (gensym "ORIG")) (top (gensym "TOP"))
         (forms (if (stringp control) (%fmtc-expand control a s top orig) :fail)))
    (if (eq forms :fail)
        `(lambda (,s &rest ,a) (%format ,s ,control ,a))
        `(lambda (,s &rest ,a)
           (let ((,orig ,a))
             (declare (ignorable ,orig))
             (block ,top ,@forms)
             ,a)))))

(define-compiler-macro format (&whole form destination control &rest args)
  (if (not (stringp control))
      form
      (let* ((s (gensym "STREAM")) (top (gensym "TOP")) (d (gensym "DESTINATION"))
             (vars (mapcar (lambda (a) (declare (ignore a)) (gensym "A")) args))
             (forms (%fmtc-expand control vars s top nil)))
        (cond ((eq forms :fail) form)
              ((null destination)
               `(let* (,@(mapcar #'list vars args) (,s (%scratch-string-stream)))
                  (declare (ignorable ,@vars))
                  (block ,top ,@forms)
                  (%scratch-string-result ,s)))
              ((eq destination t)
               `(let* (,@(mapcar #'list vars args) (,s *standard-output*))
                  (declare (ignorable ,@vars))
                  (block ,top ,@forms)
                  nil))
              (t
               `(let* ((,d ,destination) ,@(mapcar #'list vars args) (,s (%format-destination ,d)))
                  (declare (ignorable ,@vars))
                  (block ,top ,@forms)
                  (%format-result ,d ,s)))))))
