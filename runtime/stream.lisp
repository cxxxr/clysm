;;;; stream.lisp — streams
;;;;
;;;; File streams keep the whole file contents in memory as octets (read when
;;;; the file is opened, written back by FINISH-OUTPUT and CLOSE), which gives
;;;; random access (FILE-POSITION), :IO streams and binary streams of any byte
;;;; size.  Character files are UTF-8; binary elements of N bits take
;;;; ceiling(N/8) octets, little endian.

(defstruct (stream (:constructor %make-plain-stream) (:predicate %streamp))
  kind              ; :fd-out :fd-in :file :string-out :string-in :broadcast :two-way :echo :concatenated :synonym :pp
  fd
  buffer            ; output: adjustable string ; string-in: the string
  (index 0)         ; string-in position
  end               ; string-in end
  (column 0)
  unread            ; pushed back char
  streams           ; component streams
  symbol            ; synonym stream symbol
  (open t)
  pathname
  (direction :output)
  (elt-type 'character)
  (line 1))

(defstruct (file-stream (:include stream) (:constructor %make-file-stream))
  octets            ; simple (unsigned-byte 8) vector holding the file contents
  (fill 0)          ; number of valid octets
  (pos 0)           ; current position in octets
  (esize 0)         ; octets per element: 0 for character streams
  signed            ; binary elements are signed
  dirty             ; contents changed since the last write to the host
  native            ; host file name
  (external-format :default))
(defstruct (string-stream (:include stream) (:constructor %make-string-stream)))
(defstruct (broadcast-stream (:include stream) (:constructor %make-broadcast-stream)))
(defstruct (two-way-stream (:include stream) (:constructor %make-two-way-stream)))
(defstruct (echo-stream (:include stream) (:constructor %make-echo-stream)))
(defstruct (concatenated-stream (:include stream) (:constructor %make-concatenated-stream)))
(defstruct (synonym-stream (:include stream) (:constructor %make-synonym-stream)))

(defun %make-stream (&rest args &key kind &allow-other-keys)
  (apply (case kind
           ((:fd-out :fd-in :file) #'%make-file-stream)
           ((:string-out :string-in) #'%make-string-stream)
           (:broadcast #'%make-broadcast-stream)
           (:two-way #'%make-two-way-stream)
           (:echo #'%make-echo-stream)
           (:concatenated #'%make-concatenated-stream)
           (:synonym #'%make-synonym-stream)
           (t #'%make-plain-stream))
         args))

(defun streamp (x) (%streamp x))

(defun %check-stream (s)
  (unless (%streamp s) (%type-error s 'stream))
  s)

(defun %synonym-target (s) (symbol-value (stream-symbol s)))

(defun input-stream-p (s)
  (%check-stream s)
  (case (stream-kind s)
    ((:fd-in :string-in :two-way :echo :concatenated) t)
    (:file (if (member (stream-direction s) '(:input :io)) t nil))
    (:synonym (input-stream-p (%synonym-target s)))
    (t nil)))
(defun output-stream-p (s)
  (%check-stream s)
  (case (stream-kind s)
    ((:fd-out :string-out :broadcast :two-way :echo :pp) t)
    (:file (if (member (stream-direction s) '(:output :io)) t nil))
    (:synonym (output-stream-p (%synonym-target s)))
    (t nil)))
(defun open-stream-p (s) (%check-stream s) (stream-open s))
(defvar *interactive* nil "true when the REPL debugger should be used")
(defun interactive-stream-p (s)
  (%check-stream s)
  (and *interactive*
       (case (stream-kind s)
         (:fd-in t)
         (:two-way (interactive-stream-p (car (stream-streams s))))
         (:synonym (interactive-stream-p (%synonym-target s)))
         (t nil))))

(defun stream-element-type (s)
  (%check-stream s)
  (case (stream-kind s)
    (:synonym (stream-element-type (%synonym-target s)))
    (:broadcast (let ((l (last (stream-streams s))))
                  (if l (stream-element-type (car l)) t)))
    ((:two-way :echo)
     (let ((in (stream-element-type (car (stream-streams s))))
           (out (stream-element-type (cadr (stream-streams s)))))
       (if (equal in out) in `(and ,in ,out))))
    (:concatenated (let ((l (stream-streams s)))
                     (if l (stream-element-type (car l)) nil)))
    (t (stream-elt-type s))))

(defun %make-fd-output-stream (fd) (%make-stream :kind :fd-out :fd fd))
(defun %make-fd-input-stream (fd) (%make-stream :kind :fd-in :fd fd :direction :input))

(defvar *stdout* (%make-fd-output-stream 1))
(defvar *stderr* (%make-fd-output-stream 2))
(defvar *stdin* (%make-fd-input-stream 0))
(defvar *standard-output* *stdout*)
(defvar *error-output* *stderr*)
(defvar *standard-input* *stdin*)
(defvar *terminal-io* (%make-stream :kind :two-way :streams (list *stdin* *stdout*)))
(defvar *trace-output* *stdout*)
(defvar *debug-io* *terminal-io*)
(defvar *query-io* *terminal-io*)

(defun %output-stream (designator)
  (cond ((null designator) *standard-output*)
        ((eq designator t) *terminal-io*)
        ((%streamp designator) designator)
        (t (%type-error designator '(or stream (member nil t))))))

(defun %input-stream (designator)
  (cond ((null designator) *standard-input*)
        ((eq designator t) *terminal-io*)
        ((%streamp designator) designator)
        (t (%type-error designator '(or stream (member nil t))))))

(defun make-string-output-stream (&key (element-type 'character))
  (%make-stream :kind :string-out :elt-type element-type
                :buffer (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))

(defun get-output-stream-string (s)
  (unless (and (%streamp s) (eq (stream-kind s) :string-out))
    (%type-error s 'string-stream))
  (let* ((buf (stream-buffer s)) (r (subseq buf 0)))
    (setf (fill-pointer buf) 0)
    (setf (stream-column s) 0)
    r))

(defun make-string-input-stream (string &optional (start 0) end)
  (unless (stringp string) (%type-error string 'string))
  (let ((len (length string)))
    (unless (and (integerp start) (<= 0 start len)) (%type-error start `(integer 0 ,len)))
    (unless (or (null end) (and (integerp end) (<= start end len)))
      (%type-error end `(or null (integer ,start ,len))))
    (%make-stream :kind :string-in :buffer string :index start :end (or end len)
                  :direction :input)))

(defun %check-output-stream (x)
  (unless (and (%streamp x) (output-stream-p x)) (%type-error x '(satisfies output-stream-p)))
  x)
(defun %check-input-stream (x)
  (unless (and (%streamp x) (input-stream-p x)) (%type-error x '(satisfies input-stream-p)))
  x)

(defun make-broadcast-stream (&rest streams)
  (dolist (x streams) (%check-output-stream x))
  (%make-stream :kind :broadcast :streams streams))
(defun broadcast-stream-streams (s)
  (unless (broadcast-stream-p s) (%type-error s 'broadcast-stream))
  (copy-list (stream-streams s)))
(defun make-two-way-stream (in out)
  (%check-input-stream in) (%check-output-stream out)
  (%make-stream :kind :two-way :streams (list in out)))
(defun two-way-stream-input-stream (s)
  (unless (two-way-stream-p s) (%type-error s 'two-way-stream))
  (car (stream-streams s)))
(defun two-way-stream-output-stream (s)
  (unless (two-way-stream-p s) (%type-error s 'two-way-stream))
  (cadr (stream-streams s)))
(defun make-echo-stream (in out)
  (%check-input-stream in) (%check-output-stream out)
  (%make-stream :kind :echo :streams (list in out)))
(defun echo-stream-input-stream (s)
  (unless (echo-stream-p s) (%type-error s 'echo-stream))
  (car (stream-streams s)))
(defun echo-stream-output-stream (s)
  (unless (echo-stream-p s) (%type-error s 'echo-stream))
  (cadr (stream-streams s)))
(defun make-concatenated-stream (&rest streams)
  (dolist (x streams) (%check-input-stream x))
  (%make-stream :kind :concatenated :streams streams :direction :input))
(defun concatenated-stream-streams (s)
  (unless (concatenated-stream-p s) (%type-error s 'concatenated-stream))
  (copy-list (stream-streams s)))
(defun make-synonym-stream (symbol)
  (unless (symbolp symbol) (%type-error symbol 'symbol))
  (%make-stream :kind :synonym :symbol symbol))
(defun synonym-stream-symbol (s)
  (unless (synonym-stream-p s) (%type-error s 'synonym-stream))
  (stream-symbol s))

;;; ------------------------------------------------------------
;;; file stream octet buffer
;;; ------------------------------------------------------------

(defun %fs-ensure (s n)
  "Make room for N octets."
  (let ((v (file-stream-octets s)))
    (when (> n (length v))
      (let ((new (make-array (max n (* 2 (length v)) 64) :element-type '(unsigned-byte 8))))
        (replace new v :end2 (file-stream-fill s))
        (setf (file-stream-octets s) new)))))

(defun %fs-put-octet (s b)
  (let ((p (file-stream-pos s)))
    (when (>= p (length (file-stream-octets s))) (%fs-ensure s (+ p 1)))
    (setf (aref (file-stream-octets s) p) b)
    (setf (file-stream-pos s) (+ p 1))
    (when (> (+ p 1) (file-stream-fill s)) (setf (file-stream-fill s) (+ p 1)))
    (setf (file-stream-dirty s) t)))

(defun %fs-get-octet (s)
  "The next octet, or NIL at end of file."
  (let ((p (file-stream-pos s)))
    (if (>= p (file-stream-fill s))
        nil
        (progn (setf (file-stream-pos s) (+ p 1))
               (aref (file-stream-octets s) p)))))

(defun %fs-decode-char (s advance)
  "Decode the UTF-8 character at the current position (:eof at end)."
  (let* ((v (file-stream-octets s)) (fill (file-stream-fill s)) (p (file-stream-pos s)))
    (if (>= p fill)
        :eof
        (let ((b (aref v p)))
          (if (< b #x80)
              (progn (when advance (setf (file-stream-pos s) (+ p 1))) (code-char b))
              (let* ((n (cond ((>= b #xF0) 3) ((>= b #xE0) 2) ((>= b #xC0) 1) (t 0)))
                     (code (logand b (ash #x3F (- n)))))
                (dotimes (i n)
                  (let ((q (+ p 1 i)))
                    (setq code (logior (ash code 6)
                                       (if (< q fill) (logand (aref v q) #x3F) 0)))))
                (when advance (setf (file-stream-pos s) (min fill (+ p 1 n))))
                (or (code-char code) (code-char #xFFFD))))))))

(defun %utf8-length (code)
  (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))

(defun %fs-encode-char (s c)
  (let ((code (char-code c)))
    (if (< code #x80)
        (%fs-put-octet s code)
        (let ((n (%utf8-length code)))
          (%fs-put-octet s (logior (case n (2 #xC0) (3 #xE0) (t #xF0))
                                   (ash code (* -6 (- n 1)))))
          (do ((k (- n 2) (- k 1))) ((< k 0))
            (%fs-put-octet s (logior #x80 (logand (ash code (* -6 k)) #x3F))))))))

(defun %fs-read-element (s)
  "Read one binary element (an integer) or NIL at end of file."
  (let ((n (file-stream-esize s)) (p (file-stream-pos s)))
    (if (> (+ p n) (file-stream-fill s))
        nil
        (let ((v (file-stream-octets s)) (x 0))
          (dotimes (i n) (setq x (logior x (ash (aref v (+ p i)) (* 8 i)))))
          (setf (file-stream-pos s) (+ p n))
          (if (and (file-stream-signed s) (logbitp (- (* 8 n) 1) x))
              (- x (ash 1 (* 8 n)))
              x)))))

(defun %fs-write-element (s x)
  (let ((n (file-stream-esize s)))
    (dotimes (i n) (%fs-put-octet s (ldb (byte 8 (* 8 i)) x)))))

(defun %fs-flush (s)
  "Write the contents of an output file stream to the host file."
  (when (and (file-stream-dirty s) (member (stream-direction s) '(:output :io)))
    (%host-write-octets (file-stream-native s) (file-stream-octets s) (file-stream-fill s))
    (setf (file-stream-dirty s) nil)))

(defun %host-name-buffer (string)
  (dotimes (i (length string)) (%put-char-buf (char-code (char string i)))))

(defun %host-write-octets (name octets n)
  (dotimes (i n) (%put-char-buf (aref octets i)))
  (%put-char-buf -1)
  (%host-name-buffer name)
  (>= (%file-open 2) 0))

(defun %host-read-octets (name)
  "The contents of file NAME as an octet vector, or NIL."
  (%host-name-buffer name)
  (let ((n (%file-open 3)))
    (if (< n 0)
        nil
        (let ((v (make-array n :element-type '(unsigned-byte 8))))
          (dotimes (i n v) (setf (aref v i) (char-code (%host-char i))))))))

;;; ------------------------------------------------------------
;;; output
;;; ------------------------------------------------------------

(defun %stream-write-char (s c)
  (case (stream-kind s)
    (:fd-out (%write-char c (stream-fd s)))
    (:string-out (vector-push-extend c (stream-buffer s)))
    (:file (unless (and (stream-open s) (member (stream-direction s) '(:output :io))
                        (eql (file-stream-esize s) 0))
             (%not-output-stream s 'character))
     (%fs-encode-char s c))
    (:broadcast (dolist (x (stream-streams s)) (%stream-write-char x c)))
    ((:two-way :echo) (%stream-write-char (cadr (stream-streams s)) c))
    (:synonym (%stream-write-char (%synonym-target s) c))
    (:pp (%pp-capture-write-char s c))
    (t (%not-output-stream s 'character)))
  (if (char= c #\Newline)
      (setf (stream-column s) 0)
      (setf (stream-column s) (+ (stream-column s) 1)))
  c)

(defun %not-output-stream (s what)
  (if (and (%streamp s) (not (stream-open s)))
      (error 'stream-error :stream s)
      (error 'simple-type-error :datum s :expected-type 'stream
                                :format-control "~S is not a ~(~A~) output stream"
                                :format-arguments (list s what))))

(defun %not-input-stream (s what)
  (if (and (%streamp s) (not (stream-open s)))
      (error 'stream-error :stream s)
      (error 'simple-type-error :datum s :expected-type 'stream
                                :format-control "~S is not a ~(~A~) input stream"
                                :format-arguments (list s what))))

(defun write-char (c &optional stream)
  (unless (characterp c) (%type-error c 'character))
  (%stream-write-char (%output-stream stream) c))

(defun %check-seq-bounds (seq start end)
  (let ((len (length seq)))
    (unless (and (integerp start) (<= 0 start)) (%type-error start 'unsigned-byte))
    (unless (or (null end) (and (integerp end) (<= 0 end))) (%type-error end '(or null unsigned-byte)))
    (let ((end (or end len)))
      (unless (<= start end len)
        (error 'simple-error :format-control "Bad bounding indices ~S, ~S for a sequence of length ~S"
                             :format-arguments (list start end len)))
      end)))

(defun write-string (string &optional stream &key (start 0) end)
  (unless (stringp string) (%type-error string 'string))
  (let ((s (%output-stream stream)) (end (%check-seq-bounds string start end)))
    (if (and (%simple-string-p string) (eq (stream-kind s) :fd-out))
        (do ((i start (+ i 1))) ((>= i end))
          (%stream-write-char s (%schar string i)))
        (do ((i start (+ i 1))) ((>= i end))
          (%stream-write-char s (char string i))))
    string))

(defun write-line (string &optional stream &key (start 0) end)
  (write-string string stream :start start :end end)
  (write-char #\Newline stream)
  string)

(defun terpri (&optional stream) (write-char #\Newline stream) nil)

(defun %stream-column (s)
  (case (stream-kind s)
    (:synonym (%stream-column (%synonym-target s)))
    ((:two-way :echo) (%stream-column (cadr (stream-streams s))))
    (t (stream-column s))))

(defun fresh-line (&optional stream)
  (let ((s (%output-stream stream)))
    (if (eql (%stream-column s) 0) nil (progn (write-char #\Newline s) t))))

(defun %finish-output (s)
  (case (stream-kind s)
    (:file (%fs-flush s))
    ((:fd-out) (%flush))
    (:broadcast (mapc #'%finish-output (stream-streams s)))
    ((:two-way :echo) (%finish-output (cadr (stream-streams s))))
    (:synonym (%finish-output (%synonym-target s)))))

(defun finish-output (&optional stream) (%finish-output (%output-stream stream)) nil)
(defun force-output (&optional stream) (%finish-output (%output-stream stream)) nil)
(defun clear-output (&optional stream) (%output-stream stream) nil)

(defun %stream-write-byte (s b)
  (case (stream-kind s)
    (:file (unless (and (stream-open s) (member (stream-direction s) '(:output :io))
                        (> (file-stream-esize s) 0))
             (%not-output-stream s 'binary))
     (%fs-write-element s b))
    (:broadcast (dolist (x (stream-streams s)) (%stream-write-byte x b)))
    ((:two-way :echo) (%stream-write-byte (cadr (stream-streams s)) b))
    (:synonym (%stream-write-byte (%synonym-target s) b))
    (t (%not-output-stream s 'binary)))
  b)

(defun write-byte (byte stream)
  (unless (integerp byte) (%type-error byte 'integer))
  (%check-stream stream)
  (%stream-write-byte stream byte))

;;; ------------------------------------------------------------
;;; input
;;; ------------------------------------------------------------

(defun %stream-read-char (s)
  "Returns a character or :eof."
  (let ((u (stream-unread s)))
    (if u
        (progn (setf (stream-unread s) nil) u)
        (case (stream-kind s)
          (:string-in (let ((i (stream-index s)))
                        (if (>= i (stream-end s)) :eof
                            (progn (setf (stream-index s) (+ i 1))
                                   (char (stream-buffer s) i)))))
          (:file (unless (and (eql (file-stream-esize s) 0) (stream-open s)
                              (member (stream-direction s) '(:input :io)))
                   (%not-input-stream s 'character))
           (%fs-decode-char s t))
          (:fd-in (let ((code (%read-char)))
                    (if (< code 0) :eof (code-char code))))
          (:two-way (%stream-read-char (car (stream-streams s))))
          (:echo (let ((c (%stream-read-char (car (stream-streams s)))))
                   (unless (eq c :eof) (%stream-write-char (cadr (stream-streams s)) c))
                   c))
          (:concatenated
           (loop
             (when (null (stream-streams s)) (return :eof))
             (let ((c (%stream-read-char (car (stream-streams s)))))
               (if (eq c :eof) (pop (stream-streams s)) (return c)))))
          (:synonym (%stream-read-char (%synonym-target s)))
          (t (%not-input-stream s 'character))))))

(defun %stream-peek-char (s)
  "The next character (or :eof) without consuming it (and without echoing)."
  (let ((u (stream-unread s)))
    (if u
        u
        (case (stream-kind s)
          (:string-in (let ((i (stream-index s)))
                        (if (>= i (stream-end s)) :eof (char (stream-buffer s) i))))
          (:file (unless (and (eql (file-stream-esize s) 0) (stream-open s)
                              (member (stream-direction s) '(:input :io)))
                   (%not-input-stream s 'character))
           (%fs-decode-char s nil))
          ((:two-way :echo) (%stream-peek-char (car (stream-streams s))))
          (:concatenated
           (loop
             (when (null (stream-streams s)) (return :eof))
             (let ((c (%stream-peek-char (car (stream-streams s)))))
               (if (eq c :eof) (pop (stream-streams s)) (return c)))))
          (:synonym (%stream-peek-char (%synonym-target s)))
          (t (let ((c (%stream-read-char s)))
               (unless (eq c :eof) (setf (stream-unread s) c))
               c))))))

(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let* ((s (%input-stream stream)) (c (%stream-read-char s)))
    (if (eq c :eof)
        (if eof-error-p (error 'end-of-file :stream s) eof-value)
        c)))

(defun %stream-unread-char (s c)
  (case (stream-kind s)
    (:synonym (%stream-unread-char (%synonym-target s) c))
    (:two-way (%stream-unread-char (car (stream-streams s)) c))
    (:concatenated (if (stream-streams s)
                       (%stream-unread-char (car (stream-streams s)) c)
                       (setf (stream-unread s) c)))
    (:string-in (if (and (> (stream-index s) 0)
                         (eql (char (stream-buffer s) (- (stream-index s) 1)) c))
                    (setf (stream-index s) (- (stream-index s) 1))
                    (setf (stream-unread s) c)))
    (:file (let ((n (%utf8-length (char-code c))))
             (if (>= (file-stream-pos s) n)
                 (setf (file-stream-pos s) (- (file-stream-pos s) n))
                 (setf (stream-unread s) c))))
    (t (setf (stream-unread s) c))))

(defun unread-char (c &optional stream)
  (%stream-unread-char (%input-stream stream) c)
  nil)

(defun peek-char (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((s (%input-stream stream)))
    (unless (or (null peek-type) (eq peek-type t) (characterp peek-type))
      (%type-error peek-type '(or character boolean)))
    (loop
      (let ((c (%stream-peek-char s)))
        (cond ((eq c :eof) (return (if eof-error-p (error 'end-of-file :stream s) eof-value)))
              ((or (null peek-type)
                   (and (eq peek-type t) (not (%rt-whitespace-p c)))
                   (and (characterp peek-type) (char= c peek-type)))
               (return c))
              (t (%stream-read-char s)))))))

(defun read-char-no-hang (&optional stream (eof-error-p t) eof-value recursive-p)
  (read-char stream eof-error-p eof-value recursive-p))

(defun %stream-listen (s)
  (case (stream-kind s)
    (:fd-in (not (null (stream-unread s))))
    (:synonym (%stream-listen (%synonym-target s)))
    ((:two-way :echo) (or (not (null (stream-unread s))) (%stream-listen (car (stream-streams s)))))
    (:concatenated
     (loop
       (when (null (stream-streams s)) (return nil))
       (if (%stream-listen (car (stream-streams s)))
           (return t)
           (if (eq (%stream-peek-char (car (stream-streams s))) :eof)
               (pop (stream-streams s))
               (return nil)))))
    (:file (and (stream-open s) (member (stream-direction s) '(:input :io))
                (if (eql (file-stream-esize s) 0)
                    (not (eq (%stream-peek-char s) :eof))
                    (< (file-stream-pos s) (file-stream-fill s)))))
    (t (not (eq (%stream-peek-char s) :eof)))))

(defun listen (&optional stream)
  (%stream-listen (%input-stream stream)))

(defun clear-input (&optional stream)
  (let ((s (%input-stream stream)))
    (when (eq (stream-kind s) :fd-in) (setf (stream-unread s) nil))
    nil))

(defun read-line (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((s (%input-stream stream))
        (buf (make-array 80 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (let ((c (%stream-read-char s)))
        (cond ((eq c :eof)
               (return (if (= (length buf) 0)
                           (if eof-error-p (error 'end-of-file :stream s) (values eof-value t))
                           (values (subseq buf 0) t))))
              ((char= c #\Newline) (return (values (subseq buf 0) nil)))
              (t (vector-push-extend c buf)))))))

(defun %stream-read-byte (s)
  "An integer or :eof."
  (case (stream-kind s)
    (:file (unless (and (stream-open s) (member (stream-direction s) '(:input :io))
                        (> (file-stream-esize s) 0))
             (%not-input-stream s 'binary))
     (or (%fs-read-element s) :eof))
    (:two-way (%stream-read-byte (car (stream-streams s))))
    (:echo (let ((b (%stream-read-byte (car (stream-streams s)))))
             (unless (eq b :eof) (%stream-write-byte (cadr (stream-streams s)) b))
             b))
    (:concatenated
     (loop
       (when (null (stream-streams s)) (return :eof))
       (let ((b (%stream-read-byte (car (stream-streams s)))))
         (if (eq b :eof) (pop (stream-streams s)) (return b)))))
    (:synonym (%stream-read-byte (%synonym-target s)))
    (t (%not-input-stream s 'binary))))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (%check-stream stream)
  (let ((b (%stream-read-byte stream)))
    (if (eq b :eof)
        (if eof-error-p (error 'end-of-file :stream stream) eof-value)
        b)))

(defun %binary-stream-p (s input)
  "True when the stream S transfers integers rather than characters."
  (case (stream-kind s)
    (:file (> (file-stream-esize s) 0))
    ((:two-way :echo) (%binary-stream-p (if input (car (stream-streams s)) (cadr (stream-streams s))) input))
    (:concatenated (and (stream-streams s) (%binary-stream-p (car (stream-streams s)) input)))
    (:broadcast (and (stream-streams s) (%binary-stream-p (car (last (stream-streams s))) input)))
    (:synonym (%binary-stream-p (%synonym-target s) input))
    (t nil)))

(defun read-sequence (seq stream &key (start 0) end)
  (unless (typep seq 'sequence) (%type-error seq 'sequence))
  (%check-stream stream)
  (let ((end (%check-seq-bounds seq start end))
        (binary (%binary-stream-p stream t))
        (i start))
    (if (listp seq)
        (let ((tail (nthcdr start seq)))
          (loop
            (when (>= i end) (return i))
            (let ((x (if binary (%stream-read-byte stream) (%stream-read-char stream))))
              (when (eq x :eof) (return i))
              (setf (car tail) x)
              (setq tail (cdr tail) i (+ i 1)))))
        (loop
          (when (>= i end) (return i))
          (let ((x (if binary (%stream-read-byte stream) (%stream-read-char stream))))
            (when (eq x :eof) (return i))
            (setf (aref seq i) x)
            (setq i (+ i 1)))))))

(defun write-sequence (seq stream &key (start 0) end)
  (unless (typep seq 'sequence) (%type-error seq 'sequence))
  (let* ((s (%output-stream stream))
         (end (%check-seq-bounds seq start end))
         (binary (%binary-stream-p s nil)))
    (if (listp seq)
        (let ((tail (nthcdr start seq)))
          (do ((i start (+ i 1))) ((>= i end))
            (if binary (write-byte (car tail) s) (write-char (car tail) s))
            (setq tail (cdr tail))))
        (do ((i start (+ i 1))) ((>= i end))
          (let ((x (aref seq i)))
            (if binary (write-byte x s) (write-char x s)))))
    seq))

;;; ------------------------------------------------------------
;;; files
;;; ------------------------------------------------------------

(defun %binary-element-spec (et)
  "For a binary element type, (values bits signed); NIL for character types."
  (cond ((member et '(character base-char standard-char extended-char :default)) nil)
        ((eq et 'unsigned-byte) (values 8 nil))
        ((eq et 'signed-byte) (values 8 t))
        ((eq et 'bit) (values 1 nil))
        ((and (consp et) (eq (car et) 'unsigned-byte) (integerp (cadr et))) (values (cadr et) nil))
        ((and (consp et) (eq (car et) 'signed-byte) (integerp (cadr et))) (values (cadr et) t))
        ((and (consp et) (eq (car et) 'mod) (integerp (cadr et)))
         (values (max 1 (integer-length (- (cadr et) 1))) nil))
        ((subtypep et 'character) nil)
        ((subtypep et 'integer)
         (let ((n 1))
           (loop
             (when (subtypep et `(unsigned-byte ,n)) (return (values n nil)))
             (when (subtypep et `(signed-byte ,n)) (return (values n t)))
             (setq n (if (< n 64) (+ n 1) (* n 2)))
             (when (> n 100000) (error "OPEN: unsupported element type ~S" et)))))
        (t (error "OPEN: unsupported element type ~S" et))))

(defun %file-error (pathname fmt &rest args)
  (error 'file-error :pathname pathname :format-control fmt :format-arguments args))

(defun open (filename &key (direction :input) (element-type 'character)
                           (if-exists nil iep) (if-does-not-exist nil idnep)
                           (external-format :default))
  (let* ((path (if (%streamp filename) (pathname filename) (pathname filename)))
         (path (merge-pathnames path)))
    (unless (member direction '(:input :output :io :probe))
      (%type-error direction '(member :input :output :io :probe)))
    (when (wild-pathname-p path)
      (%file-error path "can't open the wild pathname ~A" path))
    (let* ((native (%native-name path))
           (exists (%file-exists-p native))
           (if-exists (if iep if-exists :new-version))
           (if-does-not-exist
             (if idnep if-does-not-exist
                 (cond ((or (eq direction :input) (eq direction :probe)) (if (eq direction :input) :error nil))
                       ((member if-exists '(:overwrite :append)) :error)
                       (t :create)))))
      (multiple-value-bind (bits signed) (%binary-element-spec element-type)
        (let ((octets nil) (pos 0))
          (cond
            ((not exists)
             (case if-does-not-exist
               (:error (return-from open
                         (%file-error path "can't open ~A: the file does not exist" native)))
               (:create (unless (%host-write-octets native (make-array 0 :element-type '(unsigned-byte 8)) 0)
                          (%file-error path "can't create ~A" native)))
               ((nil) (return-from open nil))
               (t (%type-error if-does-not-exist '(member :error :create nil))))
             (setq octets (make-array 0 :element-type '(unsigned-byte 8))))
            ((member direction '(:output :io))
             (case if-exists
               (:error (return-from open
                         (%file-error path "can't open ~A: the file already exists" native)))
               ((nil) (return-from open nil))
               ((:new-version :supersede :rename-and-delete)
                (setq octets (make-array 0 :element-type '(unsigned-byte 8))))
               (:rename
                (%host-rename-file native (concatenate 'string native ".bak"))
                (setq octets (make-array 0 :element-type '(unsigned-byte 8))))
               ((:overwrite :append)
                (setq octets (%host-read-octets native))
                (unless octets (%file-error path "can't open ~A" native))
                (when (eq if-exists :append) (setq pos (length octets))))
               (t (%type-error if-exists '(member :error :new-version :rename :rename-and-delete
                                           :overwrite :append :supersede nil)))))
            ((eq direction :input)
             (setq octets (%host-read-octets native))
             (unless octets (%file-error path "can't open ~A" native)))
            (t (setq octets (make-array 0 :element-type '(unsigned-byte 8)))))
          (let ((s (%make-stream :kind :file :pathname path :direction direction
                                 :elt-type (if bits
                                                   (list (if signed 'signed-byte 'unsigned-byte) bits)
                                                   'character)
                                 :octets octets :fill (length octets) :pos pos
                                 :esize (if bits (ceiling bits 8) 0) :signed signed
                                 :native native :external-format external-format
                                 :dirty (and (member direction '(:output :io))
                                             (not (member if-exists '(:overwrite :append)))
                                             t))))
            (when (eq direction :probe) (setf (stream-open s) nil))
            s))))))

(defun close (stream &key abort)
  (%check-stream stream)
  (when (stream-open stream)
    (when (and (eq (stream-kind stream) :file) (not abort))
      (%fs-flush stream))
    (setf (stream-open stream) nil))
  t)

(defun %file-stream-designator (s)
  (case (stream-kind s)
    (:synonym (%file-stream-designator (%synonym-target s)))
    (t s)))

(defun file-length (s)
  (%check-stream s)
  (case (stream-kind s)
    (:file (let ((n (file-stream-esize s)))
             (if (eql n 0) (file-stream-fill s) (floor (file-stream-fill s) n))))
    (:broadcast (let ((l (last (stream-streams s)))) (if l (file-length (car l)) 0)))
    (:synonym (file-length (%synonym-target s)))
    ((:fd-in :fd-out) nil)
    (t (%type-error s 'file-stream))))

(defun file-position (s &optional (pos nil posp))
  (%check-stream s)
  (if posp
      (progn
        (unless (or (member pos '(:start :end)) (and (integerp pos) (>= pos 0)))
          (%type-error pos '(or (member :start :end) unsigned-byte)))
        (case (stream-kind s)
          (:file (let* ((n (max 1 (file-stream-esize s)))
                        (len (floor (file-stream-fill s) n))
                        (p (case pos (:start 0) (:end len) (t pos))))
                   (setf (stream-unread s) nil)
                   (if (<= p len)
                       (progn (setf (file-stream-pos s) (* p n)) t)
                       nil)))
          (:string-in (let ((p (case pos (:start 0) (:end (stream-end s)) (t pos))))
                        (setf (stream-unread s) nil)
                        (if (<= p (stream-end s)) (progn (setf (stream-index s) p) t) nil)))
          (:string-out (let* ((buf (stream-buffer s))
                              (p (case pos (:start 0) (:end (fill-pointer buf)) (t pos))))
                         (if (<= p (fill-pointer buf)) (progn (setf (fill-pointer buf) p) t) nil)))
          (:synonym (file-position (%synonym-target s) pos))
          (:broadcast (let ((l (last (stream-streams s)))) (if l (file-position (car l) pos) nil)))
          (t nil)))
      (case (stream-kind s)
        (:file (let ((n (file-stream-esize s)) (p (file-stream-pos s)))
                 (if (eql n 0)
                     (if (stream-unread s) (max 0 (- p 1)) p)
                     (floor p n))))
        (:string-in (- (stream-index s) (if (stream-unread s) 1 0)))
        (:string-out (fill-pointer (stream-buffer s)))
        (:synonym (file-position (%synonym-target s)))
        (:broadcast (let ((l (last (stream-streams s)))) (if l (file-position (car l)) 0)))
        (t nil))))

(defun file-string-length (s x)
  (%check-stream s)
  (case (stream-kind s)
    (:broadcast (let ((l (last (stream-streams s)))) (if l (file-string-length (car l) x) 1)))
    (:synonym (file-string-length (%synonym-target s) x))
    (t (cond ((characterp x) (%utf8-length (char-code x)))
             ((stringp x) (let ((n 0)) (dotimes (i (length x) n) (incf n (%utf8-length (char-code (char x i)))))))
             (t (%type-error x '(or string character)))))))

(defun stream-external-format (s)
  (%check-stream s)
  (case (stream-kind s)
    (:file (file-stream-external-format s))
    (:broadcast (let ((l (last (stream-streams s)))) (if l (stream-external-format (car l)) :default)))
    (:synonym (stream-external-format (%synonym-target s)))
    (t :default)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %split-declarations (body)
    "(values declarations forms)"
    (let ((decls nil))
      (loop
        (if (and (consp body) (consp (car body)) (eq (caar body) 'declare))
            (progn (push (car body) decls) (setq body (cdr body)))
            (return (values (nreverse decls) body)))))))

(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (decls forms) (%split-declarations body)
    `(let ((,var ,stream))
       ,@decls
       (unwind-protect (progn ,@forms) (close ,var)))))

(defmacro with-open-file ((var filename &rest options) &body body)
  (let ((abortp (gensym "ABORTP")))
    (multiple-value-bind (decls forms) (%split-declarations body)
      `(let ((,var (open ,filename ,@options)) (,abortp t))
         ,@decls
         (unwind-protect (multiple-value-prog1 (progn ,@forms) (setq ,abortp nil))
           (when ,var (close ,var :abort ,abortp)))))))

(defmacro with-output-to-string ((var &optional string &key (element-type ''character)) &body body)
  (multiple-value-bind (decls forms) (%split-declarations body)
    (let ((et (gensym "ET")))
      (if string
          `(let ((,et ,element-type))
             (declare (ignore ,et))
             (let ((,var (%make-stream :kind :string-out :buffer ,string)))
               ,@decls
               ,@forms))
          `(let ((,et ,element-type))
             (let ((,var (make-string-output-stream :element-type ,et)))
               ,@decls
               (progn ,@forms)
               (get-output-stream-string ,var)))))))

(defmacro with-input-from-string ((var string &key (start 0) end index) &body body)
  (multiple-value-bind (decls forms) (%split-declarations body)
    `(let ((,var (make-string-input-stream ,string ,start ,end)))
       ,@decls
       (multiple-value-prog1 (progn ,@forms)
         ,@(when index `((setf ,index (stream-index ,var))))))))
