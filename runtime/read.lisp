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
(defvar *read-suppress* nil)

(defstruct (readtable (:constructor %make-readtable) (:predicate readtablep) (:copier nil)
                      (:conc-name %rt-)
                      (:print-function (lambda (rt stream depth)
                                         (declare (ignore depth))
                                         (print-unreadable-object (rt stream :type t :identity t)))))
  (ascii (make-array 128 :initial-element :constituent)) ; syntax types of char codes < 128
  (syntax (make-hash-table))          ; syntax types of other characters (absent = constituent)
  (macros (make-hash-table))          ; char -> reader macro function
  (dispatch (make-hash-table))        ; dispatching char -> hash table sub-char -> function
  (case :upcase))

(defvar *readtable* nil)
(defvar *standard-readtable* nil)

(defun %check-readtable (rt)
  (unless (readtablep rt) (%type-error rt 'readtable))
  rt)

(defun readtable-case (rt) (%rt-case (%check-readtable rt)))
(defun (setf readtable-case) (mode rt)
  (%check-readtable rt)
  (unless (member mode '(:upcase :downcase :preserve :invert))
    (%type-error mode '(member :upcase :downcase :preserve :invert)))
  (setf (%rt-case rt) mode))

(defun %syntax (c rt)
  (let ((code (char-code c)))
    (if (< code 128)
        (svref (%rt-ascii rt) code)
        (gethash c (%rt-syntax rt) :constituent))))

(defun %set-syntax (c rt type)
  (let ((code (char-code c)))
    (if (< code 128)
        (setf (svref (%rt-ascii rt) code) type)
        (if (eq type :constituent)
            (remhash c (%rt-syntax rt))
            (setf (gethash c (%rt-syntax rt)) type)))))

(defun %rt-whitespace-p (c)
  (eq (%syntax c (or *readtable* *standard-readtable*)) :whitespace))

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
      (let ((d (make-hash-table)))
        (maphash (lambda (k v) (setf (gethash k d) (%copy-hash v))) (%rt-dispatch from))
        (setf (%rt-dispatch to) d))
      (setf (%rt-case to) (%rt-case from)))
    to))

(defun set-macro-character (char function &optional non-terminating-p (rt *readtable*))
  (unless (characterp char) (%type-error char 'character))
  (let ((rt (%check-readtable rt)))
    (%set-syntax char rt (if non-terminating-p :non-terminating :terminating))
    (setf (gethash char (%rt-macros rt)) function)
    (remhash char (%rt-dispatch rt))
    t))

(defun get-macro-character (char &optional (rt *readtable*))
  (unless (characterp char) (%type-error char 'character))
  (let* ((rt (%rt-designator rt)) (syn (%syntax char rt)))
    (if (member syn '(:terminating :non-terminating))
        (values (gethash char (%rt-macros rt)) (eq syn :non-terminating))
        (values nil nil))))

(defun make-dispatch-macro-character (char &optional non-terminating-p (rt *readtable*))
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
  (let ((to-rt (%check-readtable to-rt)) (from-rt (%rt-designator from-rt)))
    (%set-syntax to-char to-rt (%syntax from-char from-rt))
    (let ((fn (gethash from-char (%rt-macros from-rt)))
          (table (gethash from-char (%rt-dispatch from-rt))))
      (if fn
          (setf (gethash to-char (%rt-macros to-rt)) fn)
          (remhash to-char (%rt-macros to-rt)))
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
  (let ((rt *readtable*))
    (loop
      (let ((c (%stream-read-char s)))
        (when (eq c :eof) (return (values nil :eof)))
        (let ((syn (%syntax c rt)))
          (cond ((eq syn :whitespace))
                ((and (or (eq syn :terminating) (eq syn :non-terminating))
                      (gethash c (%rt-macros rt)))
                 (let ((vals (multiple-value-list (funcall (gethash c (%rt-macros rt)) s c))))
                   (return (if vals (values (car vals) :object) (values nil :none)))))
                (t
                 (let ((x (%read-token-object s c allow-dot)))
                   (return (values x (if (eq x %dot-marker) :dot :object)))))))))))

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
            (let ((c (%stream-peek-char s)))
              (when (and (characterp c) (%rt-whitespace-p c)) (%stream-read-char s))))
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
      (let ((c (%skip-whitespace s)))
        (when (eq c :eof) (%reader-eof s))
        (when (char= c close)
          (%stream-read-char s)
          (return (if *read-suppress* nil (nreverse items))))
        (multiple-value-bind (x kind) (%read-object1 s nil)
          (case kind
            (:eof (%reader-eof s))
            (:object (push x items))))))))

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

(defun %read-token (s first)
  "Accumulate a token whose first character FIRST has been read already.
Fills *TOKEN-BUF* / *TOKEN-ESC*; returns true when some character was escaped."
  (let ((rt *readtable*) (escaped nil) (c first) (firstp t))
    (setf (fill-pointer *token-buf*) 0 (fill-pointer *token-esc*) 0)
    (loop
      (when (eq c :eof) (return))
      (let ((syn (%syntax c rt)))
        (case syn
          (:single-escape
           (let ((n (%stream-read-char s)))
             (when (eq n :eof) (%reader-eof s))
             (%token-push n t) (setq escaped t)))
          (:multiple-escape
           (setq escaped t)
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
      (setq c (%stream-read-char s)))
    escaped))

(defun %read-token-after (s)
  "Read a token starting at the next character of S (for #: #* ...).  The
token is empty when the next character ends a token."
  (let ((c (%stream-peek-char s)))
    (if (or (eq c :eof)
            (member (%syntax c *readtable*) '(:whitespace :terminating)))
        (progn (setf (fill-pointer *token-buf*) 0 (fill-pointer *token-esc*) 0) nil)
        (%read-token s (%stream-read-char s)))))

(defun %token-case-converted ()
  "The current token as a string, converted by the readtable case."
  (let* ((buf *token-buf*) (esc *token-esc*) (n (fill-pointer buf))
         (r (make-string n)) (mode (%rt-case *readtable*)))
    (dotimes (i n) (setf (char r i) (char buf i)))
    (case mode
      (:upcase (dotimes (i n) (unless (aref esc i) (setf (char r i) (char-upcase (char r i))))))
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
  (let* ((n (fill-pointer *token-buf*)))
    (when (and (not escaped) (> n 0) (every (lambda (c) (char= c #\.)) *token-buf*))
      (if (and (= n 1) allow-dot)
          (return-from %interpret-token %dot-marker)
          (%reader-error s "a token consisting only of dots cannot be read")))
    (let ((str (%token-case-converted)))
      (unless escaped
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
          ((= name-start n) (%reader-error s "no symbol name after the package marker in ~S" str))
          (t (let* ((pname (subseq str 0 colon))
                    (name (subseq str name-start))
                    (pkg (find-package pname)))
               (unless pkg (%reader-error s "package ~A not found" pname))
               (if double
                   (values (intern name pkg))
                   (multiple-value-bind (sym status) (find-symbol name pkg)
                     (cond ((eq status :external) sym)
                           ((eq pkg *keyword-package*) (values (intern name pkg)))
                           ((null status)
                            (%reader-error s "symbol ~S not found in the ~A package" name pname))
                           (t (%reader-error s "symbol ~S is not external in the ~A package"
                                             name pname))))))))))

(defun %parse-number-token (str base &optional s)
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
        (let* ((digits (remove #\. (subseq str i mant-end)))
               (exact (* sign (%digits-value digits 0 (length digits) 10)
                         (expt 10 (- expo frac-digits))))
               (type (case marker
                       ((#\D #\L) 'double-float)
                       ((#\F #\S) 'single-float)
                       (t *read-default-float-format*))))
          (list (if (and (zerop exact) (= sign -1))
                    (if (member type '(double-float long-float)) -0.0d0 -0.0)
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

(defun %read-list (s close allow-dot)
  "Read objects up to the character CLOSE; a list (NIL when reading is suppressed)."
  (let ((items nil) (tail nil) (after-dot nil))
    (loop
      (let ((c (%skip-whitespace s)))
        (when (eq c :eof) (%reader-eof s))
        (when (char= c close)
          (%stream-read-char s)
          (when (and after-dot (null tail) (not *read-suppress*))
            (%reader-error s "nothing after . in list"))
          (return (if *read-suppress*
                      nil
                      (let ((l (nreverse items)))
                        (when tail (setf (cdr (last l)) (car tail)))
                        l))))
        (multiple-value-bind (x kind) (%read-object1 s allow-dot)
          (case kind
            (:eof (%reader-eof s))
            (:none)
            (:dot
             (unless *read-suppress*
               (when (or (null items) after-dot) (%reader-error s "misplaced . in list")))
             (setq after-dot t))
            (:object
             (cond ((not after-dot) (push x items))
                   (*read-suppress*)
                   (tail (%reader-error s "more than one object after . in list"))
                   (t (setq tail (list x)))))))))))

(defun %read-string-literal (s close)
  (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t))
        (rt *readtable*))
    (loop
      (let ((c (%stream-read-char s)))
        (cond ((eq c :eof) (%reader-eof s))
              ((char= c close) (return (if *read-suppress* nil (subseq buf 0))))
              ((eq (%syntax c rt) :single-escape)
               (let ((n (%stream-read-char s)))
                 (when (eq n :eof) (%reader-eof s))
                 (vector-push-extend n buf)))
              (t (vector-push-extend c buf)))))))

(defun %read-dispatch-macro (s disp)
  (let ((n nil) (c nil))
    (loop
      (setq c (%stream-read-char s))
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
