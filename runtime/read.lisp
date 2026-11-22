;;;; read.lisp — the Lisp reader (CLHS 2)
;;;;
;;;; A readtable maps characters to syntax types (CLHS 2.1.4); constituent
;;;; traits (CLHS 2.1.4.2) belong to the characters themselves.  The reader
;;;; algorithm follows CLHS 2.2: whitespace is skipped, macro characters call
;;;; their reader macro functions, and everything else accumulates a token
;;;; (with single and multiple escapes) that is interpreted as a number, the
;;;; consing dot, or a symbol (CLHS 2.3).

(defvar *read-base* 10)
(defvar *read-eval* t)
(defvar *batch-read-ahead* nil
  "True while %LOAD-BATCH reads a form before the forms before it have run:
#. then throws to %BATCH-READ-AHEAD instead of evaluating its form.")
(defvar *read-suppress* nil)

(defstruct (readtable (:constructor %make-readtable) (:predicate readtablep) (:copier nil)
                      (:conc-name %rt-))
  (ascii (make-array 128 :initial-element :constituent)) ; syntax types of char codes < 128
  (syntax (make-hash-table))          ; syntax types of other characters (absent = constituent)
  (macros (make-hash-table))          ; char -> reader macro function
  (dispatch (make-hash-table))        ; dispatching char -> hash table sub-char -> function
  (case :upcase)
  ;; the entries of MACROS for char codes < 128, filled in when looked up:
  ;; NIL (not looked up yet), :NONE (no reader macro) or the function
  (mcache (make-array 128 :initial-element nil)))

(defmethod print-object ((rt readtable) stream)
  (print-unreadable-object (rt stream :type t :identity t)))

(defvar *readtable* nil)
(defvar *standard-readtable* nil)

(defun %check-readtable (rt)
  (unless (readtablep rt) (%type-error rt 'readtable))
  rt)

(defun readtable-case (rt) (%rt-case (%check-readtable rt)))
(defun (setf readtable-case) (mode rt)
  (%note-env-change)
  (%check-readtable rt)
  (unless (member mode '(:upcase :downcase :preserve :invert))
    (%type-error mode '(member :upcase :downcase :preserve :invert)))
  (setf (%rt-case rt) mode))

(defun %syntax (c rt)
  (let ((code (char-code c)))
    (if (< code 128)
        (svref (%rt-ascii rt) code)
        (gethash c (%rt-syntax rt) :constituent))))

;; the syntax type of an ASCII character read from the table at the call
;; site (in the reader's loops, which ask it of every character)
(defmacro %syntax-fast (c rt)
  (let ((ch (gensym "C")) (r (gensym "RT")) (code (gensym "CODE")))
    `(let* ((,ch ,c) (,r ,rt) (,code (char-code ,ch)))
       (if (%fx< ,code 128)
           (%svref (%rt-ascii ,r) ,code)
           (%syntax ,ch ,r)))))

(defun %set-syntax (c rt type)
  (let ((code (char-code c)))
    (if (< code 128)
        (setf (svref (%rt-ascii rt) code) type)
        (if (eq type :constituent)
            (remhash c (%rt-syntax rt))
            (setf (gethash c (%rt-syntax rt)) type)))))

(defun %rt-macro (c rt)
  "The reader macro function of the character C in the readtable RT, or NIL."
  (let ((code (char-code c)))
    (if (< code 128)
        (let* ((cache (%rt-mcache rt)) (e (svref cache code)))
          (cond ((null e)
                 (let ((f (gethash c (%rt-macros rt))))
                   (setf (svref cache code) (or f :none))
                   f))
                ((eq e :none) nil)
                (t e)))
        (values (gethash c (%rt-macros rt))))))

(defun %rt-flush-macro-cache (rt)
  "To be called when the reader macros of RT change."
  (fill (%rt-mcache rt) nil))

;;; The fast paths of the reader read the characters of a simple string
;;; between an index and an end: those of a string input stream over a
;;; simple string without an unread character, or the text of a character
;;; input file of ASCII octets (its position in octets is the index).
(defun %rd-file-text (s)
  "The text of S when it is an open character input file of ASCII octets,
without an unread character (see FILE-STREAM-TEXT), else NIL."
  (when (and (%instancep s) (eq (%instance-layout s) (load-time-value *file-stream-layout*))
             (%fsbufp (%instance-ref s +stream-fast-slot+))
             (eq (%instance-ref s +stream-direction-slot+) :input))
    (let ((tx (file-stream-text s)))
      (cond ((%simple-string-p tx) tx)
            ((eq tx :none) nil)
            (t (let* ((buf (%instance-ref s +stream-fast-slot+)) (fill (%fsbuf-fill buf))
                      (octets (%fsbuf-octets buf)))
                 (declare (fixnum fill))
                 (if (and (< fill 67108864)
                          (dotimes (i fill t)
                            (when (%fx>= (%bytes-ref octets i) 128) (return nil))))
                     (setf (file-stream-text s) (%utf8-decode octets 0 fill))
                     (progn (setf (file-stream-text s) :none) nil))))))))

(defmacro %rd-buffer (s)
  "The simple string the fast paths read S from, or NIL."
  (let ((st (gensym "S")) (b (gensym "B")))
    `(let ((,st ,s))
       (if (%string-in-p ,st)
           (and (null (%instance-ref ,st +stream-unread-slot+))
                (let ((,b (%instance-ref ,st +stream-buffer-slot+))) (and (%simple-string-p ,b) ,b)))
           (%rd-file-text ,st)))))
;; RDF: the $fsbuf of the file whose text %RD-BUFFER gave, or NIL for a
;; string stream (a variable bound where these are used)
(defmacro %rd-fsbuf (s buf)
  `(and ,buf (not (%string-in-p ,s)) (%instance-ref ,s +stream-fast-slot+)))
(defmacro %rd-index (s)
  `(if rdf (%fsbuf-pos rdf) (%instance-ref ,s +stream-index-slot+)))
(defmacro %rd-end (s)
  `(if rdf (%fsbuf-fill rdf) (%instance-ref ,s +stream-end-slot+)))
(defmacro %rd-set-index (s i)
  `(if rdf (%set-fsbuf-pos rdf ,i) (%set-instance-ref ,s +stream-index-slot+ ,i)))

;;; The next character of the stream S or :eof: a string stream is read in line.
(defmacro %rd-char (s)
  (let ((st (gensym "S")) (i (gensym "I")) (buf (gensym "BUF")))
    ;; the slots of a string input stream are read without the accessors'
    ;; tests of the structure type (a STRING-STREAM is a subtype of STREAM)
    `(let ((,st ,s))
       (if (and (%string-in-p ,st) (null (%instance-ref ,st +stream-unread-slot+)))
           (let ((,i (%instance-ref ,st +stream-index-slot+)) (,buf (%instance-ref ,st +stream-buffer-slot+)))
             (if (%fx< ,i (%instance-ref ,st +stream-end-slot+))
                 (progn (%set-instance-ref ,st +stream-index-slot+ (%fx+ ,i 1))
                        (if (%simple-string-p ,buf) (%schar ,buf ,i) (char ,buf ,i)))
                 :eof))
           ;; (an ASCII character of a character input file)
           (or (%fs-fast-char ,st t) (%stream-read-char ,st))))))

(defun %rt-whitespace-p (c)
  (eq (%syntax c (or *readtable* *standard-readtable*)) :whitespace))

(defun %peek-char-skip (stream eof-error-p eof-value)
  "PEEK-CHAR with PEEK-TYPE T: the whitespace of a string input stream over a
simple string is skipped here, its ASCII characters by the table of the
readtable."
  (let ((s (cond ((null stream) *standard-input*) ((eq stream t) *terminal-io*) (t stream))))
    (when (and (%string-in-p s) (null (%instance-ref s +stream-unread-slot+))
               (%simple-string-p (%instance-ref s +stream-buffer-slot+)))
      (let ((buf (%instance-ref s +stream-buffer-slot+))
            (end (%instance-ref s +stream-end-slot+))
            (ascii (%rt-ascii (or *readtable* *standard-readtable*))))
        (declare (fixnum end))
        (do ((i (%instance-ref s +stream-index-slot+) (%fx+ i 1))) ((%fx>= i end) (%set-instance-ref s +stream-index-slot+ end))
          (declare (fixnum i))
          (let* ((c (%schar buf i)) (code (%char-code c)))
            (unless (if (%fx< code 128) (eq (%svref ascii code) :whitespace) (%rt-whitespace-p c))
              (%set-instance-ref s +stream-index-slot+ i)
              (return-from %peek-char-skip c))))))
    (locally (declare (notinline peek-char))
      (peek-char t s eof-error-p eof-value))))

(defun %rt-designator (rt)
  "NIL designates the standard readtable."
  (if (null rt) *standard-readtable* (%check-readtable rt)))

(defun %copy-hash (h)
  (let ((new (make-hash-table)))
    (maphash (lambda (k v) (setf (gethash k new) v)) h)
    new))

(defun copy-readtable (&optional (from *readtable*) to)
  (let* ((from (%rt-designator from))
         (to (if to (%check-readtable to) (%make-readtable))))
    (unless (eq from to)
      (setf (%rt-ascii to) (copy-seq (%rt-ascii from)))
      (setf (%rt-syntax to) (%copy-hash (%rt-syntax from)))
      (setf (%rt-macros to) (%copy-hash (%rt-macros from)))
      (%rt-flush-macro-cache to)
      (let ((d (make-hash-table)))
        (maphash (lambda (k v) (setf (gethash k d) (%copy-hash v))) (%rt-dispatch from))
        (setf (%rt-dispatch to) d))
      (setf (%rt-case to) (%rt-case from)))
    to))

(defun set-macro-character (char function &optional non-terminating-p (rt *readtable*))
  (%note-env-change)
  (unless (characterp char) (%type-error char 'character))
  (let ((rt (%check-readtable rt)))
    (%set-syntax char rt (if non-terminating-p :non-terminating :terminating))
    (setf (gethash char (%rt-macros rt)) function)
    (%rt-flush-macro-cache rt)
    (remhash char (%rt-dispatch rt))
    t))

(defun get-macro-character (char &optional (rt *readtable*))
  (unless (characterp char) (%type-error char 'character))
  (let* ((rt (%rt-designator rt)) (syn (%syntax char rt)))
    (if (member syn '(:terminating :non-terminating))
        (values (gethash char (%rt-macros rt)) (eq syn :non-terminating))
        (values nil nil))))

(defun make-dispatch-macro-character (char &optional non-terminating-p (rt *readtable*))
  (%note-env-change)
  (unless (characterp char) (%type-error char 'character))
  (let ((rt (%check-readtable rt)))
    (set-macro-character char #'%read-dispatch-macro non-terminating-p rt)
    (setf (gethash char (%rt-dispatch rt)) (make-hash-table))
    t))

(defun %dispatch-table (disp rt)
  (or (gethash disp (%rt-dispatch rt))
      (error 'simple-error :format-control "~S is not a dispatching macro character"
                           :format-arguments (list disp))))

(defun set-dispatch-macro-character (disp sub function &optional (rt *readtable*))
  (%note-env-change)
  (let ((table (%dispatch-table disp (%check-readtable rt))))
    (when (digit-char-p sub)
      (error 'simple-error :format-control "~S: a digit cannot be a dispatch sub-character"
                           :format-arguments (list sub)))
    (setf (gethash (char-upcase sub) table) function)
    t))

(defun get-dispatch-macro-character (disp sub &optional (rt *readtable*))
  (let ((table (%dispatch-table disp (%rt-designator rt))))
    (if (digit-char-p sub) nil (values (gethash (char-upcase sub) table)))))

(defun set-syntax-from-char (to-char from-char &optional (to-rt *readtable*) (from-rt nil))
  (%note-env-change)
  (let ((to-rt (%check-readtable to-rt)) (from-rt (%rt-designator from-rt)))
    (%set-syntax to-char to-rt (%syntax from-char from-rt))
    (let ((fn (gethash from-char (%rt-macros from-rt)))
          (table (gethash from-char (%rt-dispatch from-rt))))
      (if fn
          (setf (gethash to-char (%rt-macros to-rt)) fn)
          (remhash to-char (%rt-macros to-rt)))
      (%rt-flush-macro-cache to-rt)
      (if table
          (setf (gethash to-char (%rt-dispatch to-rt)) (%copy-hash table))
          (remhash to-char (%rt-dispatch to-rt))))
    t))

;;; ------------------------------------------------------------
;;; reader errors
;;; ------------------------------------------------------------

(defun %reader-error (stream fmt &rest args)
  (error 'reader-error :stream stream :format-control fmt :format-arguments args))

(defun %reader-eof (stream)
  (error 'end-of-file :stream stream))

;;; ------------------------------------------------------------
;;; main entry points
;;; ------------------------------------------------------------

(defvar %eof-marker (make-symbol "EOF"))
(defvar %dot-marker (make-symbol "DOT"))

(defvar *read-labels* nil "alist (n . (placeholder . object)) for #n= / #n#")
(defvar *backquote-depth* 0)

(defun %read-object1 (s allow-dot)
  "Read one object.  Returns (values object kind), kind being :object, :none (a
macro character returned no values), :dot or :eof."
  (loop
    (let ((c (%rd-char s)))
      (when (eq c :eof) (return (values nil :eof)))
      (let ((rt *readtable*))
        (unless (eq (%syntax-fast c rt) :whitespace)
          (return (%read-object-at s c rt allow-dot)))))))

(defun %read-object-at (s c rt allow-dot)
  "Read the object that starts with the character C, which was read from S and
is not whitespace in the readtable RT.  Returns the values of %READ-OBJECT1."
  (let ((fn (let ((syn (%syntax-fast c rt)))
              (and (or (eq syn :terminating) (eq syn :non-terminating))
                   ;; (the cached entry of an ASCII character read here)
                   (let ((code (char-code c)))
                     (if (%fx< code 128)
                         (let ((e (%svref (%rt-mcache rt) code)))
                           (cond ((null e) (%rt-macro c rt))
                                 ((eq e :none) nil)
                                 (t e)))
                         (%rt-macro c rt)))))))
    (if fn
        ;; the macro function returns zero values or one
        (let ((v (funcall fn s c)))
          (if (eql (%mv-count) 0) (values nil :none) (values v :object)))
        (multiple-value-bind (found sym) (%read-plain-symbol s c rt)
          (if found
              (if (eq sym %dot-marker)
                  (if allow-dot
                      (values sym :dot)
                      (%reader-error s "a token consisting only of dots cannot be read"))
                  (values sym :object))
              (let ((x (%read-token-object s c allow-dot)))
                (values x (if (eq x %dot-marker) :dot :object))))))))

(defvar %no-symbol (make-symbol "NO-SYMBOL"))
(defvar *%rd-name* (%make-string 64 #\0)
  "The scratch string of the names of the symbols %READ-PLAIN-SYMBOL reads.")

(defun %read-plain-symbol (s c rt)
  "When the token starting with C (just read from S) is an ordinary symbol
name in a string stream — a letter followed by constituent letters, digits
and the characters of ASCII that are plain constituents, with one package
marker or none (PKG:NAME, PKG::NAME, :NAME: compiled files are mostly such
symbols) — read it and return T and the symbol, else return NIL with nothing
consumed."
  (let* ((rbuf (%rd-buffer s)) (rdf (%rd-fsbuf s rbuf)))
  (when (and rbuf (not *read-suppress*)
             (eq (%rt-case rt) :upcase) t
             ;; in bases above ten letters are digits
             (<= *read-base* 10))
    (let ((code0 (char-code c)))
      ;; a decimal fixnum: digits only
      (when (and (<= 48 code0 57) (eql *read-base* 10) (eq (svref (%rt-ascii rt) code0) :constituent))
        (let* ((buf rbuf) (i0 (%rd-index s)) (i i0) (end (%rd-end s)) (v (- code0 48)))
          (declare (fixnum i0 i end v))
          (do () ((or (>= i end) (> (- i i0) 6)))   ; at most 8 digits: a fixnum
            (let ((code (char-code (%schar buf i))))
              (if (and (<= 48 code 57) (eq (svref (%rt-ascii rt) code) :constituent))
                  (setq v (+ (* v 10) (- code 48)) i (+ i 1))
                  (return))))
          (when (or (>= i end) (member (%syntax (%schar buf i) rt) '(:whitespace :terminating)))
            (%rd-set-index s i)
            (return-from %read-plain-symbol (values t v)))))
      ;; the consing dot: a dot alone
      (when (and (eql code0 46) (eq (svref (%rt-ascii rt) 46) :constituent))
        (let ((i (%rd-index s)) (buf rbuf))
          (when (or (>= i (%rd-end s)) (member (%syntax (%schar buf i) rt) '(:whitespace :terminating)))
            (return-from %read-plain-symbol (values t %dot-marker)))))
      ;; other decimal numbers: a sign, a decimal point, a float
      (when (and (or (<= 48 code0 57) (eql code0 43) (eql code0 45) (eql code0 46))
                 (eql *read-base* 10))
        (multiple-value-bind (found x) (%read-plain-number s rt rbuf rdf)
          (when found (return-from %read-plain-symbol (values t x)))))
      ;; a symbol: a token of constituents that is not a number (it begins
      ;; with a letter, or has no decimal digit and is not only dots)
      (when (and (<= 33 code0 126) (eq (svref (%rt-ascii rt) code0) :constituent))
        (let* ((buf rbuf) (i0 (%rd-index s)) (i i0) (end (%rd-end s))
               (ascii (%rt-ascii rt))
               ;; the package marker: the index after it of the name (as
               ;; that of the buffer), and whether it was doubled
               (colon (if (eql code0 58) -1 nil)) (name-start i0) (double nil)
               ;; a decimal digit before any package marker
               (digit (<= 48 code0 57)))
          (declare (fixnum i0 i end name-start))
          (do () ((>= i end))
            (let ((code (char-code (%schar buf i))))
              (when (and (<= 48 code 57) (null colon)) (setq digit t))
              (cond ((and (eql code 58) (null colon))
                     ;; PKG:NAME or PKG::NAME (the package name read so far)
                     (setq colon i)
                     (when (and (< (+ i 1) end) (eql (char-code (%schar buf (+ i 1))) 58))
                       (setq double t i (+ i 1)))
                     (setq i (+ i 1) name-start i))
                    ((and (<= 33 code 126) (/= code 58) (eq (svref ascii code) :constituent))
                     (setq i (+ i 1)))
                    (t (return)))))
          ;; the token must end at a character that ends tokens (not an
          ;; escape), and a package marker must have a name after it
          (when (and (or (>= i end)
                         (member (%syntax (%schar buf i) rt) '(:whitespace :terminating)))
                     (or (null colon) (> i name-start))
                     (not (and (eql colon -1) double))
                     (or colon (<= 65 code0 90) (<= 97 code0 122)
                         (and (not digit)
                              (not (and (eql code0 46)
                                        (do ((k i0 (+ k 1))) ((>= k i) t)
                                          (declare (fixnum k))
                                          (unless (eql (%schar buf k) #\.) (return nil))))))))
            (let ((pkg nil) (name-len 0))
              (declare (fixnum name-len))
              (flet ((upcased (from to first)
                       ;; the characters FROM..TO of the buffer upcased, after
                       ;; FIRST if any, into the scratch string; their number
                       (let* ((k0 (if first 1 0)) (n (+ k0 (- to from))) (str *%rd-name*))
                         (declare (fixnum k0 n))
                         (when (> n (%string-length str))
                           (setq str (%make-string (max n (* 2 (%string-length str))) #\0) *%rd-name* str))
                         (when first (%set-schar str 0 (char-upcase first)))
                         (do ((k from (+ k 1)) (j k0 (+ j 1))) ((>= k to) n)
                           (declare (fixnum k j))
                           (let ((code (char-code (%schar buf k))))
                             (%set-schar str j (if (<= 97 code 122) (code-char (- code 32)) (%schar buf k)))))))
                     (name () (%simple-substring *%rd-name* 0 name-len)))
                (cond
                  ((null colon)
                   (setq name-len (upcased i0 i c) pkg *package*))
                  ((eql colon -1)
                   (setq name-len (upcased name-start i nil) pkg *keyword-package*))
                  (t
                   ;; PKG:NAME, PKG::NAME; what is not found is left to the
                   ;; reader of tokens, which signals the errors
                   (let ((n (upcased i0 colon c)))
                     (setq pkg (if *package-local-nicknames*
                                   (find-package (%simple-substring *%rd-name* 0 n))
                                   (multiple-value-bind (p found)
                                       (%region-gethash *package-table* *%rd-name* n
                                                        (%string-hash-range *%rd-name* 0 n))
                                     (if (eq found :unknown) (find-package (%simple-substring *%rd-name* 0 n)) p)))))
                   (unless pkg (return-from %read-plain-symbol nil))
                   (setq name-len (upcased name-start i nil))))
                (multiple-value-bind (sym status) (%region-find-symbol *%rd-name* name-len pkg)
                  (when (eq status :unknown)
                    (multiple-value-setq (sym status) (find-symbol (name) pkg)))
                  (let ((result
                          (cond ((or (null colon) (eql colon -1) double)
                                 (if status sym (values (intern (name) pkg))))
                                ((eq status :external) sym)
                                ((or (eq pkg *keyword-package*) *fasl-syntax*)
                                 (if status sym (values (intern (name) pkg))))
                                (t %no-symbol))))
                    (unless (eq result %no-symbol)
                      (%rd-set-index s i)
                      (values t result)))))))))))))

(defun %read-plain-number (s rt rbuf rdf)
  "The token whose first character was just read from the string stream S,
when it is a decimal integer or ratio, or a float of an exponent of at most
three digits that %DECIMAL-FLOAT rounds, made only of constituent
characters: returns T and the number, else NIL with nothing more consumed."
  (let* ((buf rbuf) (end (%rd-end s)) (start (- (%rd-index s) 1)) (i start)
         (ascii (%rt-ascii rt))
         (neg nil) (ni 0) (nf 0) (dot nil) (marker 0) (expo 0) (eneg nil) (ne 0)
         (mstart 0) (mend 0) (small 0))
    (declare (fixnum end start i ni nf marker expo ne mstart mend small))
    (macrolet ((peek () `(if (< i end) (char-code (%schar buf i)) -1))
               (check-token-end ()
                 ;; the token must end at I, and be made of constituents only
                 `(progn
                    (unless (or (>= i end) (member (%syntax (%schar buf i) rt) '(:whitespace :terminating)))
                      (return-from %read-plain-number nil))
                    (do ((k start (+ k 1))) ((>= k i))
                      (declare (fixnum k))
                      (unless (eq (svref ascii (char-code (%schar buf k))) :constituent)
                        (return-from %read-plain-number nil))))))
      (when (< start 0) (return-from %read-plain-number nil))
      (let ((code (peek)))
        (cond ((eql code 45) (setq neg t i (+ i 1)))
              ((eql code 43) (setq i (+ i 1)))))
      (setq mstart i)
      (loop
        (let ((code (peek)))
          (unless (<= 48 code 57) (return))
          (when (< ni 8) (setq small (+ (* small 10) (- code 48))))
          (setq ni (+ ni 1) i (+ i 1))))
      ;; a ratio
      (when (and (eql (peek) 47) (> ni 0))
        (setq i (+ i 1))
        (let ((dstart i) (nd 0) (dsmall 0))
          (declare (fixnum dstart nd dsmall))
          (loop
            (let ((code (peek)))
              (unless (<= 48 code 57) (return))
              (when (< nd 8) (setq dsmall (+ (* dsmall 10) (- code 48))))
              (setq nd (+ nd 1) i (+ i 1))))
          (when (eql nd 0) (return-from %read-plain-number nil))
          (check-token-end)
          (let ((den (if (<= nd 8) dsmall (%decimal-integer buf dstart i nil))))
            ;; (a zero denominator: the reader of tokens signals the error)
            (when (eql den 0) (return-from %read-plain-number nil))
            (let ((num (if (<= ni 8) small (%decimal-integer buf mstart (+ mstart ni) nil))))
              (%rd-set-index s i)
              (return-from %read-plain-number (values t (/ (if neg (- num) num) den)))))))
      (when (eql (peek) 46)
        (setq dot t i (+ i 1))
        (loop
          (unless (<= 48 (peek) 57) (return))
          (setq nf (+ nf 1) i (+ i 1))))
      (setq mend i)
      ;; an exponent: a marker, a sign, one to three digits
      (let ((code (peek)))
        (when (and (> (+ ni nf) 0)
                   (member code '(69 101 83 115 70 102 68 100 76 108)))
          (setq marker (if (>= code 97) (- code 32) code) i (+ i 1))
          (let ((code (peek)))
            (cond ((eql code 45) (setq eneg t i (+ i 1)))
                  ((eql code 43) (setq i (+ i 1)))))
          (loop
            (let ((code (peek)))
              (unless (<= 48 code 57) (return))
              (setq expo (+ (* expo 10) (- code 48)) ne (+ ne 1) i (+ i 1))
              (when (> ne 3) (return-from %read-plain-number nil))))
          (when (eql ne 0) (return-from %read-plain-number nil))))
      (check-token-end)
      (cond ((and (eql marker 0) (eql nf 0))
             ;; an integer, possibly with a decimal point at the end
             (cond ((eql ni 0) nil)
                   ((<= ni 8)
                    (%rd-set-index s i)
                    (values t (if neg (- small) small)))
                   (t (%rd-set-index s i)
                      (values t (%decimal-integer buf mstart (+ mstart ni) neg)))))
            ((and (> (+ ni nf) 0) (or (> nf 0) (not (eql marker 0))))
             ;; the nearest float, unless the helper cannot tell
             (let* ((type (cond ((or (eql marker 68) (eql marker 76)) 'double-float)
                                ((or (eql marker 70) (eql marker 83)) 'single-float)
                                (t *read-default-float-format*)))
                    (x (%decimal-float buf mstart mend
                                       (+ (* 4 (- (if eneg (- expo) expo) nf))
                                          (if neg 2 0)
                                          (if (member type '(double-float long-float)) 0 1)))))
               (when x
                 (%rd-set-index s i)
                 (values t x))))
            (t nil)))))

(defun %read-top (s eof-error-p eof-value)
  (loop
    (multiple-value-bind (x kind) (%read-object1 s nil)
      (case kind
        (:eof (return (if eof-error-p (%reader-eof s) eof-value)))
        (:object (return x))))))

(defun %read-required (s)
  "Read an object inside another one (end of file is an error)."
  (%read-top s t nil))

(defun %read-entry (stream eof-error-p eof-value recursive-p preserve)
  (let* ((s (%input-stream stream))
         (x (if recursive-p
                (%read-top s eof-error-p %eof-marker)
                (let ((*read-labels* nil) (*backquote-depth* 0))
                  (%read-top s eof-error-p %eof-marker)))))
    (if (eq x %eof-marker)
        eof-value
        (progn
          (unless (or preserve recursive-p)
            ;; consume one whitespace character following the object
            (if (%rd-buffer s)
                (let* ((buf (%rd-buffer s)) (rdf (%rd-fsbuf s buf)) (i (%rd-index s)))
                  (when (%fx< i (%rd-end s))
                    (when (eq (%syntax (%schar buf i)
                                       (or *readtable* *standard-readtable*))
                              :whitespace)
                      (%rd-set-index s (%fx+ i 1)))))
                (let ((c (%stream-peek-char s)))
                  (when (and (characterp c) (%rt-whitespace-p c)) (%stream-read-char s)))))
          x))))

(defun read (&optional stream (eof-error-p t) eof-value recursive-p)
  (%read-entry stream eof-error-p eof-value recursive-p nil))

(defun read-preserving-whitespace (&optional stream (eof-error-p t) eof-value recursive-p)
  (%read-entry stream eof-error-p eof-value recursive-p t))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace)
  (let* ((s (make-string-input-stream string start end))
         (x (%read-entry s eof-error-p eof-value nil preserve-whitespace)))
    (values x (- (stream-index s) (if (stream-unread s) 1 0)))))

(defun read-delimited-list (char &optional stream recursive-p)
  (let ((s (%input-stream stream)))
    (if recursive-p
        (%read-delimited s char)
        (let ((*read-labels* nil) (*backquote-depth* 0)) (%read-delimited s char)))))

(defun %read-delimited (s close)
  (let ((items nil))
    (loop
      (let ((c (%read-char-after-whitespace s)))
        (when (eq c :eof) (%reader-eof s))
        (when (char= c close)
          (return (if *read-suppress* nil (nreverse items))))
        (multiple-value-bind (x kind) (%read-object-at s c *readtable* nil)
          (case kind
            (:eof (%reader-eof s))
            (:object (push x items))))))))

(defun %read-char-after-whitespace (s)
  "Skip whitespace; returns the next character, which is consumed, or :eof."
  (loop
    (let ((c (%rd-char s)))
      (when (or (eq c :eof) (not (eq (%syntax-fast c *readtable*) :whitespace)))
        (return c)))))

(defun %skip-whitespace (s)
  "Skip whitespace; returns the next character (not consumed) or :eof."
  (let ((rt *readtable*))
    (loop
      (let ((c (%stream-peek-char s)))
        (cond ((eq c :eof) (return :eof))
              ((eq (%syntax c rt) :whitespace) (%stream-read-char s))
              (t (return c)))))))

;;; ------------------------------------------------------------
;;; tokens
;;; ------------------------------------------------------------

(defvar *token-buf* (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t))
(defvar *token-esc* (make-array 64 :fill-pointer 0 :adjustable t))

(defun %invalid-constituent-p (c)
  (member (char-code c) '(8 9 10 12 13 32 127)))

(defun %token-push (c escaped)
  (vector-push-extend c *token-buf*)
  (vector-push-extend escaped *token-esc*))

(defvar %token-last-escape -1)  ; token position where the last escape began

(defun %read-token (s first)
  "Accumulate a token whose first character FIRST has been read already.
Fills *TOKEN-BUF* / *TOKEN-ESC*; returns true when some character was escaped."
  (let ((rt *readtable*) (escaped nil) (c first) (firstp t))
    (%set-mdarray-fill *token-buf* 0) (%set-mdarray-fill *token-esc* 0)
    (setq %token-last-escape -1)
    (loop
      (when (eq c :eof) (return))
      (let ((syn (%syntax-fast c rt)))
        (case syn
          (:single-escape
           (let ((n (%stream-read-char s)))
             (when (eq n :eof) (%reader-eof s))
             (setq %token-last-escape (fill-pointer *token-buf*))
             (%token-push n t) (setq escaped t)))
          (:multiple-escape
           (setq escaped t %token-last-escape (fill-pointer *token-buf*))
           (loop
             (let ((n (%stream-read-char s)))
               (when (eq n :eof) (%reader-eof s))
               (case (%syntax n rt)
                 (:single-escape
                  (let ((m (%stream-read-char s)))
                    (when (eq m :eof) (%reader-eof s))
                    (%token-push m t)))
                 (:multiple-escape (return))
                 (t (%token-push n t))))))
          (t
           (if (or firstp (eq syn :constituent) (eq syn :non-terminating))
               (progn
                 (when (and (%invalid-constituent-p c) (not *read-suppress*))
                   (%reader-error s "invalid constituent character ~S" c))
                 (%token-push c nil))
               (progn (%stream-unread-char s c) (return))))))
      (setq firstp nil)
      ;; a run of ordinary constituents read straight from a string stream
      (when (%rd-buffer s)
        (let* ((buf (%rd-buffer s)) (rdf (%rd-fsbuf s buf)) (i0 (%rd-index s)) (i i0) (end (%rd-end s))
               (ascii (%rt-ascii rt)))
          (do () ((>= i end))
            (let ((code (char-code (%schar buf i))))
              (if (and (<= 33 code 126) (eq (svref ascii code) :constituent))
                  (setq i (+ i 1))
                  (return))))
          (when (> i i0)
            (%string-buffer-append *token-buf* buf i0 i)
            (let* ((esc *token-esc*) (fp (%mdarray-fill esc)) (data (%mdarray-data esc)) (k (- i i0)))
              (if (and (%simple-vector-p data) (<= (+ fp k) (%svlength data)))
                  (progn (dotimes (j k) (%set-svref data (+ fp j) nil))
                         (%set-mdarray-fill esc (+ fp k)))
                  (dotimes (j k) (vector-push-extend nil esc))))
            (%rd-set-index s i))))
      (setq c (%stream-read-char s)))
    escaped))

(defun %read-token-after (s)
  "Read a token starting at the next character of S (for #: #* ...).  The
token is empty when the next character ends a token."
  (let ((c (%stream-peek-char s)))
    (if (or (eq c :eof)
            (member (%syntax c *readtable*) '(:whitespace :terminating)))
        (progn (%set-mdarray-fill *token-buf* 0) (%set-mdarray-fill *token-esc* 0) nil)
        (%read-token s (%stream-read-char s)))))

(defun %token-case-converted ()
  "The current token as a string, converted by the readtable case."
  (let* ((buf *token-buf*) (esc *token-esc*) (n (%mdarray-fill buf))
         (r (make-string n)) (mode (%rt-case *readtable*)))
    (if (and (%mdarray-p buf) (eql (%mdarray-offset buf) 0) (%simple-string-p (%mdarray-data buf)))
        (%string-copy r 0 (%mdarray-data buf) 0 n)
        (dotimes (i n) (setf (char r i) (char buf i))))
    (case mode
      (:upcase
       (if (< %token-last-escape 0)
           ;; no escaped characters
           (dotimes (i n)
             (let ((code (char-code (char r i))))
               (cond ((< code 97))
                     ((<= code 122) (setf (char r i) (code-char (- code 32))))
                     ((> code 127) (setf (char r i) (char-upcase (char r i)))))))
           (dotimes (i n) (unless (aref esc i) (setf (char r i) (char-upcase (char r i)))))))
      (:downcase (dotimes (i n) (unless (aref esc i) (setf (char r i) (char-downcase (char r i))))))
      (:invert
       (let ((upper nil) (lower nil))
         (dotimes (i n)
           (unless (aref esc i)
             (when (upper-case-p (char r i)) (setq upper t))
             (when (lower-case-p (char r i)) (setq lower t))))
         (unless (and upper lower)
           (dotimes (i n)
             (unless (aref esc i)
               (setf (char r i) (if upper (char-downcase (char r i)) (char-upcase (char r i))))))))))
    r))

(defun %read-token-object (s first allow-dot)
  (let ((escaped (%read-token s first)))
    (if *read-suppress*
        nil
        (%interpret-token s escaped allow-dot))))

(defun %interpret-token (s escaped allow-dot)
  (let* ((n (%mdarray-fill *token-buf*)))
    (when (and (not escaped) (> n 0) (char= (char *token-buf* 0) #\.)
               (every (lambda (c) (char= c #\.)) *token-buf*))
      (if (and (= n 1) allow-dot)
          (return-from %interpret-token %dot-marker)
          (%reader-error s "a token consisting only of dots cannot be read")))
    (let ((str (%token-case-converted)))
      (unless (or escaped
                  ;; a number cannot begin with a letter that is not a digit
                  (let ((c (char-code (char str 0))))
                    (and (<= *read-base* 10) (or (<= 65 c 90) (<= 97 c 122)))))
        (let ((num (%parse-number-token str *read-base* s)))
          (when num (return-from %interpret-token (car num)))))
      (%intern-token str *token-esc* s))))

(defun %intern-token (str esc s)
  (let* ((n (length str))
         (colon (dotimes (i n nil)
                  (when (and (char= (char str i) #\:) (not (aref esc i))) (return i))))
         (double (and colon (< (+ colon 1) n) (char= (char str (+ colon 1)) #\:)
                      (not (aref esc (+ colon 1)))))
         (name-start (and colon (+ colon (if double 2 1)))))
    (when colon
      (do ((i name-start (+ i 1))) ((>= i n))
        (when (and (char= (char str i) #\:) (not (aref esc i)))
          (%reader-error s "too many package markers in ~S" str))))
    (cond ((null colon) (values (intern str *package*)))
          ((= colon 0) (values (intern (subseq str name-start) *keyword-package*)))
          ((and (= name-start n) (< %token-last-escape name-start)) (%reader-error s "no symbol name after the package marker in ~S" str))
          (t (let* ((pname (subseq str 0 colon))
                    (name (subseq str name-start))
                    (pkg (find-package pname)))
               (unless pkg (%reader-error s "package ~A not found" pname))
               (if double
                   (values (intern name pkg))
                   (multiple-value-bind (sym status) (find-symbol name pkg)
                     (cond ((eq status :external) sym)
                           ((eq pkg *keyword-package*) (values (intern name pkg)))
                           ;; a compiled file names the symbols that were external
                           ;; when it was compiled (they may be exported later in it)
                           (*fasl-syntax* (values (intern name pkg)))
                           ((null status)
                            (%reader-error s "symbol ~S not found in the ~A package" name pname))
                           (t (%reader-error s "symbol ~S is not external in the ~A package"
                                             name pname))))))))))

(defun %parse-number-token (str base &optional s)
  "Returns (list number) or NIL."
  (let ((len (length str)) (i 0) (sign 1))
    (when (= len 0) (return-from %parse-number-token nil))
    ;; a number has a decimal digit (letters are digits only above base 10)
    (when (and (<= base 10)
               (dotimes (k len t)
                 (let ((code (char-code (char str k)))) (when (and (>= code 48) (<= code 57)) (return nil)))))
      (return-from %parse-number-token nil))
    (when (member (char str 0) '(#\+ #\-))
      (when (char= (char str 0) #\-) (setq sign -1))
      (setq i 1))
    (when (>= i len) (return-from %parse-number-token nil))
    ;; integer in *read-base*, optional trailing dot (decimal)
    (let ((end (if (char= (char str (- len 1)) #\.) (- len 1) len)))
      (when (and (> end i) (%digits-p str i end (if (< end len) 10 base)))
        (return-from %parse-number-token
          (list (if (and (or (< end len) (eql base 10)) (%simple-string-p str))
                    (%decimal-integer str i end (eql sign -1))
                    (* sign (%digits-value str i end (if (< end len) 10 base))))))))
    ;; ratio
    (let ((slash (position #\/ str)))
      (when slash
        (if (and (> slash i) (< (+ slash 1) len)
                 (%digits-p str i slash base)
                 (%digits-p str (+ slash 1) len base))
            (let ((d (%digits-value str (+ slash 1) len base)))
              (when (zerop d)
                (if s
                    (%reader-error s "division by zero in ~S" str)
                    (return-from %parse-number-token nil)))
              (return-from %parse-number-token
                (list (/ (* sign (%digits-value str i slash base)) d))))
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
        (let* ((type (case marker
                       ((#\D #\L) 'double-float)
                       ((#\F #\S) 'single-float)
                       (t *read-default-float-format*)))
               (e10 (- expo frac-digits))
               ;; the nearest float, unless the helper cannot tell
               (fast (and (fixnump e10) (< -100000000 e10 100000000) (%simple-string-p str)
                          (%decimal-float str i mant-end
                                          (+ (* 4 e10) (if (= sign -1) 2 0)
                                             (if (member type '(double-float long-float)) 0 1))))))
          (if fast
              (list fast)
              (let* ((digits (remove #\. (subseq str i mant-end)))
                     (exact (* sign (%digits-value digits 0 (length digits) 10)
                               (expt 10 e10))))
                (let ((x (if (and (zerop exact) (= sign -1))
                             (if (member type '(double-float long-float)) -0.0d0 -0.0)
                             (if (member type '(double-float long-float)) (%to-double exact) (%to-single exact)))))
                  ;; beyond the largest float: an error, as in SBCL
                  (when (and s (> (abs x) (if (%single-float-p x)
                                              most-positive-single-float
                                              most-positive-double-float)))
                    (%reader-error s "floating-point overflow reading ~S" str))
                  (list x)))))))))

(defun %digits-p (str start end base)
  "True when the characters START..END of STR are all digits in BASE."
  (do ((i start (+ i 1))) ((>= i end) t)
    (unless (digit-char-p (char str i) base) (return nil))))

(defun every-index (pred str start end)
  (do ((i start (+ i 1))) ((>= i end) t)
    (unless (funcall pred (char str i)) (return nil))))

(defun %digits-value (str start end base)
  (if (and (fixnump base) (<= 2 base 36))
      ;; groups of digits that fit in a fixnum (SMALL, of K digits)
      (let ((value nil) (small 0) (k 0) (kmax (%fixnum-digit-capacity base)))
        (declare (fixnum small k kmax))
        (do ((i start (+ i 1)))
            ((>= i end) (if value (+ (* value (%fixnum-power base k)) small) small))
          (when (eql k kmax)
            (setq value (if value (+ (* value (%fixnum-power base k)) small) small) small 0 k 0))
          (setq small (+ (* small base) (digit-char-p (char str i) base)) k (+ k 1))))
      (let ((v 0))
        (do ((i start (+ i 1))) ((>= i end) v)
          (setq v (+ (* v base) (digit-char-p (char str i) base)))))))

;;; ------------------------------------------------------------
;;; standard macro characters
;;; ------------------------------------------------------------

(defun %read-list (s close allow-dot)
  "Read objects up to the character CLOSE; a list (NIL when reading is suppressed)."
  (let ((items nil) (last nil) (tail nil) (after-dot nil))
    (loop
      (let ((c (%read-char-after-whitespace s)))
        (when (eq c :eof) (%reader-eof s))
        (when (char= c close)
          (when (and after-dot (null tail) (not *read-suppress*))
            (%reader-error s "nothing after . in list"))
          (return (if *read-suppress*
                      nil
                      (progn
                        (when tail (setf (cdr last) (car tail)))
                        items))))
        (multiple-value-bind (x kind) (%read-object-at s c *readtable* allow-dot)
          (case kind
            (:eof (%reader-eof s))
            (:none)
            (:dot
             (unless *read-suppress*
               (when (or (null items) after-dot) (%reader-error s "misplaced . in list")))
             (setq after-dot t))
            (:object
             (cond ((not after-dot)
                    ;; the list is made in order
                    (let ((cell (list x)))
                      (if last (setf (cdr last) cell) (setq items cell))
                      (setq last cell)))
                   (*read-suppress*)
                   (tail (%reader-error s "more than one object after . in list"))
                   (t (setq tail (list x)))))))))))

(defun %read-string-literal (s close)
  (let ((rt *readtable*))
    ;; in a string stream: the characters up to CLOSE, when no escape is among them
    (when (%rd-buffer s)
      (let* ((buf (%rd-buffer s)) (rdf (%rd-fsbuf s buf)) (i0 (%rd-index s)) (end (%rd-end s)) (i i0)
             (ascii (%rt-ascii rt))
             ;; after an escape: the characters so far, their number, and
             ;; where the run of characters not yet in OUT starts
             (out nil) (n 0) (run i0))
        (declare (fixnum i0 end i n run))
        (macrolet ((flush ()
                     ;; the run RUN..I appended to OUT
                     `(let ((k (- i run)))
                        (declare (fixnum k))
                        (when (> (+ n k 1) (%string-length out))
                          (let ((new (%make-string (+ (* 2 (%string-length out)) k 1) #\Space)))
                            (%string-copy new 0 out 0 n)
                            (setq out new)))
                        (%string-copy out n buf run k)
                        (setq n (+ n k)))))
          (loop
            (when (>= i end) (return))
            (let* ((c (%schar buf i)) (code (char-code c)))
              (cond ((eq c close)
                     (%rd-set-index s (+ i 1))
                     (return-from %read-string-literal
                       (cond (*read-suppress* nil)
                             (out (flush) (%simple-substring out 0 n))
                             (t (%simple-substring buf i0 i)))))
                    ((eq (if (< code 128) (svref ascii code) (%syntax c rt)) :single-escape)
                     ;; the escaped character is taken as it is
                     (when (>= (+ i 1) end) (return))
                     (unless out (setq out (%make-string (+ (- i i0) 32) #\Space)))
                     (flush)
                     (%set-schar out n (%schar buf (+ i 1)))
                     (setq n (+ n 1) i (+ i 2) run i))
                    (t (setq i (+ i 1)))))))))
    (let ((buf (%make-string 32 #\Space)) (n 0))
      (declare (fixnum n))
      (macrolet ((add (c)
                   `(progn
                      (when (>= n (%string-length buf))
                        (let ((new (%make-string (* 2 n) #\Space)))
                          (%string-copy new 0 buf 0 n)
                          (setq buf new)))
                      (%set-schar buf n ,c)
                      (setq n (+ n 1)))))
        (loop
          (let ((c (%stream-read-char s)))
            (cond ((eq c :eof) (%reader-eof s))
                  ((char= c close) (return (if *read-suppress* nil (%simple-substring buf 0 n))))
                  ((eq (%syntax c rt) :single-escape)
                   (let ((m (%stream-read-char s)))
                     (when (eq m :eof) (%reader-eof s))
                     (add m)))
                  (t (add c)))))))))

(defun %read-dispatch-macro (s disp)
  (let ((n nil) (c nil))
    (loop
      (setq c (%rd-char s))
      (when (eq c :eof) (%reader-eof s))
      (if (digit-char-p c)
          (setq n (+ (* (or n 0) 10) (digit-char-p c)))
          (return)))
    (let* ((table (gethash disp (%rt-dispatch *readtable*)))
           (fn (and table (gethash (char-upcase c) table))))
      (unless fn (%reader-error s "no dispatch function defined for #~A" c))
      (funcall fn s c n))))

(defun %feature-match-p (f)
  (cond ((symbolp f) (not (null (member f *features*))))
        ((not (consp f)) (error 'simple-error :format-control "bad feature expression ~S"
                                              :format-arguments (list f)))
        ((not (symbolp (car f))) nil)
        ((string= (symbol-name (car f)) "AND") (every #'%feature-match-p (cdr f)))
        ((string= (symbol-name (car f)) "OR") (some #'%feature-match-p (cdr f)))
        ((string= (symbol-name (car f)) "NOT") (not (%feature-match-p (cadr f))))
        (t (error 'simple-error :format-control "bad feature expression ~S"
                                :format-arguments (list f)))))

(defun %sharp-backslash (s c n)
  (declare (ignore c n))
  ;; #\x of one character in a string stream
  (when (and (%rd-buffer s) (not *read-suppress*))
    (let* ((buf (%rd-buffer s)) (rdf (%rd-fsbuf s buf)) (i (%rd-index s))
          (end (%rd-end s)))
      (declare (fixnum i end))
      (when (and (< i end)
                 (or (>= (+ i 1) end)
                     (member (%syntax (%schar buf (+ i 1)) *readtable*) '(:whitespace :terminating))))
        (%rd-set-index s (+ i 1))
        (return-from %sharp-backslash (%schar buf i)))))
  (let ((first (%stream-read-char s)))
    (when (eq first :eof) (%reader-eof s))
    (let ((next (%stream-peek-char s)))
      (if (or (eq next :eof) (member (%syntax next *readtable*) '(:whitespace :terminating)))
          (if *read-suppress* nil first)
          (progn
            (%read-token s (%stream-read-char s))
            (if *read-suppress*
                nil
                (let ((name (concatenate 'string (string first) (subseq *token-buf* 0))))
                  (or (name-char name)
                      (%reader-error s "unrecognized character name: ~A" name)))))))))

(defun %sharp-left-paren (s c n)
  (declare (ignore c))
  (let ((l (%read-list s #\) nil)))
    (cond (*read-suppress* nil)
          ((null n) (coerce l 'simple-vector))
          ((> (length l) n) (%reader-error s "too many elements in #~D(...)" n))
          ((and (null l) (> n 0)) (%reader-error s "no elements in #~D()" n))
          (t (let ((v (make-array n :initial-element (car (last l)))))
               (replace v l)
               v)))))

(defun %sharp-asterisk (s c n)
  (declare (ignore c))
  (let ((escaped (%read-token-after s)))
    (if *read-suppress*
        nil
        (let* ((tok (subseq *token-buf* 0)) (len (length tok)))
          (when (or escaped (notevery (lambda (ch) (or (char= ch #\0) (char= ch #\1))) tok))
            (%reader-error s "illegal bit vector syntax #*~A" tok))
          (when n
            (when (> len n) (%reader-error s "too many bits in #~D*~A" n tok))
            (when (and (= len 0) (> n 0)) (%reader-error s "no bits in #~D*" n)))
          (let ((v (make-array (or n len) :element-type 'bit
                                          :initial-element (if (> len 0) (digit-char-p (char tok (- len 1))) 0))))
            (dotimes (i len v) (setf (sbit v i) (digit-char-p (char tok i)))))))))

(defun %sharp-colon (s c n)
  (declare (ignore c n))
  (%read-token-after s)
  (if *read-suppress*
      nil
      (let ((name (%token-case-converted)))
        (dotimes (i (length name))
          (when (and (char= (char name i) #\:) (not (aref *token-esc* i)))
            (%reader-error s "package marker in uninterned symbol #:~A" name)))
        (make-symbol name))))

(defun %sharp-radix (s base)
  (if *read-suppress*
      (progn (%read-required s) nil)
      (progn
        (unless (and (integerp base) (<= 2 base 36))
          (%reader-error s "illegal radix ~S" base))
        (let ((x (let ((*read-base* base)) (%read-required s))))
          (unless (rationalp x) (%reader-error s "#~DR: ~S is not a rational" base x))
          x))))

(defun %sharp-a (s c n)
  (declare (ignore c))
  (let ((contents (%read-required s)))
    (cond (*read-suppress* nil)
          ;; SBCL's readable arrays: #A((dimensions) element-type . contents)
          ((and (null n) (consp contents) (listp (car contents)) (consp (cdr contents))
                (every (lambda (d) (typep d '(integer 0))) (car contents)))
           (let ((dims (car contents)) (type (cadr contents)) (elements (cddr contents)))
             (if (null dims)
                 (make-array nil :element-type type :initial-element elements)
                 (make-array dims :element-type type :initial-contents elements))))
          ((null n) (%reader-error s "#A needs a rank"))
          ((= n 0) (make-array nil :initial-element contents))
          (t (let ((dims nil) (x contents))
               (dotimes (i n)
                 (unless (typep x 'sequence) (%reader-error s "#~DA: ~S is not a sequence" n x))
                 (push (length x) dims)
                 (setq x (if (> (length x) 0) (elt x 0) nil)))
               (make-array (nreverse dims) :initial-contents contents))))))

(defun %sharp-s (s c n)
  (declare (ignore c n))
  (let ((l (%read-required s)))
    (if *read-suppress*
        nil
        (progn
          (unless (and (consp l) (symbolp (car l)))
            (%reader-error s "bad #S syntax ~S" l))
          (let* ((name (car l))
                 (ctor (or (get name '%struct-constructor)
                           (find-symbol (concatenate 'string "MAKE-" (symbol-name name))
                                        (symbol-package name)))))
            (unless (and ctor (fboundp ctor))
              (%reader-error s "#S: no structure constructor for ~S" name))
            (apply ctor
                   (do ((args (cdr l) (cddr args)) (r nil))
                       ((null args) (nreverse r))
                     (push (intern (string (car args)) *keyword-package*) r)
                     (push (cadr args) r))))))))

(defun %sharp-c (s c n)
  (declare (ignore c n))
  (let ((l (%read-required s)))
    (if *read-suppress*
        nil
        (progn
          (unless (and (consp l) (consp (cdr l)) (null (cddr l)) (realp (car l)) (realp (cadr l)))
            (%reader-error s "bad #C syntax ~S" l))
          (complex (car l) (cadr l))))))

(defun %sharp-conditional (s want)
  (if *read-suppress*
      (progn (%read-required s) (%read-required s) (values))
      (let ((feature (let ((*package* *keyword-package*)) (%read-required s))))
        (if (eq (not (null (%feature-match-p feature))) want)
            (%read-required s)
            (progn (let ((*read-suppress* t)) (%read-required s)) (values))))))

(defun %sharp-block-comment (s c n)
  (declare (ignore c n))
  (let ((depth 1) (prev nil))
    (loop
      (let ((ch (%stream-read-char s)))
        (when (eq ch :eof) (%reader-eof s))
        (cond ((and (eql prev #\|) (char= ch #\#))
               (setq depth (- depth 1) ch nil)
               (when (= depth 0) (return)))
              ((and (eql prev #\#) (char= ch #\|))
               (setq depth (+ depth 1) ch nil)))
        (setq prev ch))))
  (values))

(defun %init-standard-readtable ()
  (let ((rt (%make-readtable)))
    (setq *readtable* rt)
    (dolist (code '(9 10 12 13 32)) (%set-syntax (code-char code) rt :whitespace))
    (%set-syntax #\\ rt :single-escape)
    (%set-syntax #\| rt :multiple-escape)
    (set-macro-character #\( (lambda (s c) (declare (ignore c)) (%read-list s #\) t)))
    (set-macro-character #\) (lambda (s c) (declare (ignore c)) (%reader-error s "unmatched close parenthesis")))
    (set-macro-character #\' (lambda (s c) (declare (ignore c))
                               (let ((x (%read-required s))) (if *read-suppress* nil (list 'quote x)))))
    (set-macro-character #\; (lambda (s c) (declare (ignore c))
                               (loop (let ((ch (%stream-read-char s)))
                                       (when (or (eq ch :eof) (char= ch #\Newline)) (return))))
                               (values)))
    (set-macro-character #\" (lambda (s c) (%read-string-literal s c)))
    (set-macro-character #\` (lambda (s c) (declare (ignore c))
                               (let ((x (let ((*backquote-depth* (+ *backquote-depth* 1)))
                                          (%read-required s))))
                                 (if *read-suppress* nil (list 'quasiquote x)))))
    (set-macro-character #\, (lambda (s c) (declare (ignore c))
                               (when (and (<= *backquote-depth* 0) (not *read-suppress*))
                                 (%reader-error s "comma not inside a backquote"))
                               (let* ((n (%stream-peek-char s))
                                      (op (cond ((eq n :eof) (%reader-eof s))
                                                ((or (char= n #\@) (char= n #\.))
                                                 (%stream-read-char s) 'unquote-splicing)
                                                (t 'unquote)))
                                      (x (let ((*backquote-depth* (- *backquote-depth* 1)))
                                           (%read-required s))))
                                 (if *read-suppress* nil (list op x)))))
    (make-dispatch-macro-character #\# t)
    (set-dispatch-macro-character #\# #\' (lambda (s c n) (declare (ignore c n))
                                            (let ((x (%read-required s)))
                                              (if *read-suppress* nil (list 'function x)))))
    (set-dispatch-macro-character #\# #\( #'%sharp-left-paren)
    (set-dispatch-macro-character #\# #\\ #'%sharp-backslash)
    (set-dispatch-macro-character #\# #\: #'%sharp-colon)
    (set-dispatch-macro-character #\# #\. (lambda (s c n) (declare (ignore c n))
                                            (let ((form (%read-required s)))
                                              (cond (*read-suppress* nil)
                                                    ;; (read ahead by %LOAD-BATCH: read again later)
                                                    (*batch-read-ahead* (throw '%batch-read-ahead nil))
                                                    (*read-eval* (eval form))
                                                    (t (%reader-error s "#. while *READ-EVAL* is NIL"))))))
    (set-dispatch-macro-character #\# #\| #'%sharp-block-comment)
    (set-dispatch-macro-character #\# #\X (lambda (s c n) (declare (ignore c n)) (%sharp-radix s 16)))
    (set-dispatch-macro-character #\# #\B (lambda (s c n) (declare (ignore c n)) (%sharp-radix s 2)))
    (set-dispatch-macro-character #\# #\O (lambda (s c n) (declare (ignore c n)) (%sharp-radix s 8)))
    (set-dispatch-macro-character #\# #\R (lambda (s c n) (declare (ignore c)) (%sharp-radix s n)))
    (set-dispatch-macro-character #\# #\* #'%sharp-asterisk)
    (set-dispatch-macro-character #\# #\C #'%sharp-c)
    (set-dispatch-macro-character #\# #\A #'%sharp-a)
    (set-dispatch-macro-character #\# #\= (lambda (s c n) (declare (ignore c)) (%read-label-define s n)))
    (set-dispatch-macro-character #\# #\# (lambda (s c n) (declare (ignore c)) (%read-label-ref s n)))
    (set-dispatch-macro-character #\# #\P (lambda (s c n) (declare (ignore c n))
                                            (let ((x (%read-required s)))
                                              (cond (*read-suppress* nil)
                                                    ((stringp x) (parse-namestring x))
                                                    (t (%reader-error s "#P needs a string: ~S" x))))))
    (set-dispatch-macro-character #\# #\S #'%sharp-s)
    (set-dispatch-macro-character #\# #\+ (lambda (s c n) (declare (ignore c n)) (%sharp-conditional s t)))
    (set-dispatch-macro-character #\# #\- (lambda (s c n) (declare (ignore c n)) (%sharp-conditional s nil)))
    (dolist (ch (list #\< #\) #\Space #\Tab #\Newline #\Page #\Return))
      (set-dispatch-macro-character #\# ch (lambda (s c n) (declare (ignore n))
                                             (%reader-error s "#~A cannot be read" c))))
    (setq *standard-readtable* rt)
    ;; The printer needs the standard syntax, not the reader macro functions.
    (let ((syntax (%make-readtable :ascii (copy-seq (%rt-ascii rt)))))
      (maphash (lambda (char function)
                 (when function (setf (gethash char (%rt-macros syntax)) t)))
               (%rt-macros rt))
      (setq *print-default-readtable* syntax))
    (setq *readtable* (copy-readtable rt))
    rt))

(defun %read-label-define (s n)
  (cond (*read-suppress* (%read-required s))
        ((null n) (%reader-error s "#= needs a label number"))
        ((assoc n *read-labels*) (%reader-error s "label #~D= defined twice" n))
        (t
         (let* ((placeholder (list '%read-placeholder n))
                (entry (cons n (cons placeholder placeholder))))
           (push entry *read-labels*)
           (let ((obj (%read-required s)))
             (setf (cddr entry) obj)
             (%read-label-patch obj placeholder obj (make-hash-table :test 'eq)))))))

(defun %read-label-patch (tree placeholder obj seen)
  "Replace PLACEHOLDER by OBJ inside TREE (which may be circular)."
  (cond ((eq tree placeholder) obj)
        ((gethash tree seen) tree)
        ((consp tree)
         (setf (gethash tree seen) t)
         (setf (car tree) (%read-label-patch (car tree) placeholder obj seen))
         (setf (cdr tree) (%read-label-patch (cdr tree) placeholder obj seen))
         tree)
        ((and (arrayp tree) (not (stringp tree)) (not (bit-vector-p tree)))
         (setf (gethash tree seen) t)
         (dotimes (i (array-total-size tree) tree)
           (setf (row-major-aref tree i)
                 (%read-label-patch (row-major-aref tree i) placeholder obj seen))))
        (t tree)))

(defun %read-label-ref (s n)
  (cond (*read-suppress* nil)
        ((null n) (%reader-error s "## needs a label number"))
        (t (let ((e (assoc n *read-labels*)))
             (unless e (%reader-error s "undefined label #~D#" n))
             (cddr e)))))

(%init-standard-readtable)

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
