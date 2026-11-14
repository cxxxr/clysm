;;;; stream.lisp — streams
;;;;
;;;; File streams keep the whole file contents in memory as octets (read when
;;;; the file is opened, written back by FINISH-OUTPUT and CLOSE), which gives
;;;; random access (FILE-POSITION), :IO streams and binary streams of any byte
;;;; size.  Character files are UTF-8; binary elements of N bits take
;;;; ceiling(N/8) octets, little endian.

(defstruct (stream (:constructor %make-plain-stream) (:constructor %make-pp-stream (kind buffer))
                   (:predicate %streamp))
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
  (line 1)
  ;; :FILE for an open character input file (READ-CHAR reads its ASCII
  ;; characters inline at the call site); NIL once it is closed
  (fast nil)
  ;; :FILE for an open character output file (WRITE-STRING and WRITE-CHAR
  ;; write into its octets at once)
  (fast-out nil))

(defun %make-fs-cursor (pos fill)
  (let ((c (make-array 2 :element-type '(signed-byte 32))))
    (setf (aref c 0) pos (aref c 1) fill)
    c))

(defstruct (file-stream (:include stream) (:constructor %make-file-stream))
  octets            ; simple (unsigned-byte 8) vector holding the file contents
  ;; a (signed-byte 32) vector: the current position in OCTETS and the number
  ;; of valid octets (FILE-STREAM-POS, FILE-STREAM-FILL): integers that the
  ;; inline READ-CHAR reads and writes without boxes
  (cursor (%make-fs-cursor 0 0))
  (esize 0)         ; octets per element: 0 for character streams
  signed            ; binary elements are signed
  dirty             ; contents changed since the last write to the host
  native            ; host file name
  created           ; the file was created by OPEN (CLOSE :ABORT T deletes it)
  (external-format :default))
(defmacro %fs-cursor (s) `(the (simple-array (signed-byte 32) (2)) (file-stream-cursor ,s)))
(defun file-stream-pos (s) (%cursor-ref (file-stream-cursor s) 0))
(defun (setf file-stream-pos) (v s) (%cursor-set (file-stream-cursor s) 0 v))
(defun file-stream-fill (s) (%cursor-ref (file-stream-cursor s) 1))
(defun (setf file-stream-fill) (v s) (%cursor-set (file-stream-cursor s) 1 v))
;;; String streams.  A :STRING-OUT stream (MAKE-STRING-OUTPUT-STREAM) owns a
;;; simple string BUFFER with INDEX characters in it; its column is computed
;;; when asked for: COLUMN is the column at buffer position END.  A :STRING-FP
;;; stream writes to a user's string with a fill pointer (WITH-OUTPUT-TO-STRING
;;; with a string, FORMAT to a string).  A :STRING-IN stream reads BUFFER from
;;; INDEX to END.
(defstruct (string-stream (:include stream) (:constructor %make-string-stream)
                          (:constructor %make-sstream (kind buffer index end direction))))
(defstruct (broadcast-stream (:include stream) (:constructor %make-broadcast-stream)))
(defstruct (two-way-stream (:include stream) (:constructor %make-two-way-stream)))
(defstruct (echo-stream (:include stream) (:constructor %make-echo-stream)))
(defstruct (concatenated-stream (:include stream) (:constructor %make-concatenated-stream)))
(defstruct (synonym-stream (:include stream) (:constructor %make-synonym-stream)))
;;; a binary TCP connection (host sockets, see %TCP-CONNECT)
(defstruct (socket-stream (:include stream) (:constructor %make-socket-stream))
  id
  (in (make-array 65536 :element-type '(unsigned-byte 8)))
  (in-pos 0) (in-fill 0) in-eof
  (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))

;;; Slots of streams read without the accessors (fast paths below)
(defconstant +stream-kind-slot+ 0)
(defconstant +stream-buffer-slot+ 2)
(defconstant +stream-index-slot+ 3)
(defconstant +stream-end-slot+ 4)
(defconstant +stream-unread-slot+ 6)
(defconstant +stream-open-slot+ 9)
(defconstant +stream-direction-slot+ 11)
(defconstant +stream-column-slot+ 5)
(defconstant +stream-fast-slot+ 14)
(defconstant +stream-fast-out-slot+ 15)
(defconstant +fs-octets-slot+ 16)
(defconstant +fs-cursor-slot+ 17)
(defconstant +fs-esize-slot+ 18)
(defconstant +fs-dirty-slot+ 20)
(defmacro %sl-cursor (sl) `(the (simple-array (signed-byte 32) (2)) (%svref ,sl +fs-cursor-slot+)))

(defvar *file-stream-layout*
  (let ((s (%make-file-stream :kind :file :buffer 'b :index 'i :end 'e :unread 'u :open 'o
                              :direction 'd :octets 'oc :cursor 'cu :esize 'es :fast 'fa
                              :column 'co :fast-out 'fo :dirty 'di)))
    (unless (and (eq (%instance-ref s +stream-kind-slot+) :file)
                 (eq (%instance-ref s +stream-buffer-slot+) 'b)
                 (eq (%instance-ref s +stream-index-slot+) 'i)
                 (eq (%instance-ref s +stream-end-slot+) 'e)
                 (eq (%instance-ref s +stream-unread-slot+) 'u)
                 (eq (%instance-ref s +stream-open-slot+) 'o)
                 (eq (%instance-ref s +stream-direction-slot+) 'd)
                 (eq (%instance-ref s +stream-fast-slot+) 'fa)
                 (eq (%instance-ref s +stream-column-slot+) 'co)
                 (eq (%instance-ref s +stream-fast-out-slot+) 'fo)
                 (eq (%instance-ref s +fs-dirty-slot+) 'di)
                 (eq (%instance-ref s +fs-octets-slot+) 'oc)
                 (eq (%instance-ref s +fs-cursor-slot+) 'cu)
                 (eq (%instance-ref s +fs-esize-slot+) 'es))
      (error "stream.lisp: the slot indices of FILE-STREAM changed"))
    (%instance-layout s)))

(defvar *string-stream-layout*
  (%instance-layout (%make-sstream :string-in "" 0 0 :input)))

(defmacro %fast-out-file-p (s)
  `(and (%instancep ,s) (eq (%instance-layout ,s) (load-time-value *file-stream-layout*))
        (eq (%instance-ref ,s +stream-fast-out-slot+) :file)))

(defun %make-stream (&rest args &key kind &allow-other-keys)
  (apply (case kind
           ((:fd-out :fd-in :file) #'%make-file-stream)
           ((:string-out :string-in :string-fp) #'%make-string-stream)
           (:broadcast #'%make-broadcast-stream)
           (:two-way #'%make-two-way-stream)
           (:echo #'%make-echo-stream)
           (:concatenated #'%make-concatenated-stream)
           (:synonym #'%make-synonym-stream)
           (:socket #'%make-socket-stream)
           (t #'%make-plain-stream))
         args))

(defun streamp (x) (%streamp x))

(defun %check-stream (s)
  (unless (%streamp s) (%type-error s 'stream))
  s)

(defun %synonym-target (s) (symbol-value (stream-symbol s)))

(defun %native-input-stream-p (s)
  (%check-stream s)
  (case (stream-kind s)
    (:gray (input-stream-p s))
    ((:fd-in :string-in :two-way :echo :concatenated :socket) t)
    (:file (if (member (stream-direction s) '(:input :io)) t nil))
    (:synonym (input-stream-p (%synonym-target s)))
    (t nil)))
(defun %native-output-stream-p (s)
  (%check-stream s)
  (case (stream-kind s)
    (:gray (output-stream-p s))
    ((:fd-out :string-out :string-fp :broadcast :two-way :echo :pp :socket) t)
    (:file (if (member (stream-direction s) '(:output :io)) t nil))
    (:synonym (output-stream-p (%synonym-target s)))
    (t nil)))
(defun %native-open-stream-p (s) (%check-stream s) (stream-open s))
(defvar *interactive* nil "true when the REPL debugger should be used")
(defun interactive-stream-p (s)
  (%check-stream s)
  (and *interactive*
       (case (stream-kind s)
         (:fd-in t)
         (:gray nil)
         (:two-way (interactive-stream-p (car (stream-streams s))))
         (:synonym (interactive-stream-p (%synonym-target s)))
         (t nil))))

(defun %native-stream-element-type (s)
  (%check-stream s)
  (case (stream-kind s)
    (:gray (stream-element-type s))
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

;;; ------------------------------------------------------------
;;; string streams
;;; ------------------------------------------------------------

(defun %make-string-out ()
  (%make-sstream :string-out (%make-string 40 #\Space) 0 0 :output))

(defun make-string-output-stream (&key (element-type 'character))
  (let ((s (%make-string-out)))
    (unless (eq element-type 'character) (setf (stream-elt-type s) element-type))
    s))

(define-compiler-macro make-string-output-stream (&whole form &rest args)
  (if args form '(%make-string-out)))

(defun %make-fill-pointer-output-stream (string)
  "An output stream that appends to STRING, which has a fill pointer."
  (%make-sstream :string-fp string 0 0 :output))

(defun %string-out-grow (s need)
  "Make room for NEED more characters in the :string-out stream S; returns the buffer."
  (let* ((buf (stream-buffer s)) (fill (stream-index s)) (cap (%string-length buf)))
    (declare (fixnum fill cap need))
    (if (<= (+ fill need) cap)
        buf
        ;; four times the size while the buffer is small: fewer copies
        (let ((new (%make-string (max (+ fill need) (if (< cap 65536) (* 4 cap) (* 2 cap))) #\Space)))
          (%string-copy new 0 buf 0 fill)
          (setf (stream-buffer s) new)
          new))))

(defun %string-out-column (s)
  "The column of the :string-out stream S, brought up to date."
  (let* ((buf (stream-buffer s)) (fill (stream-index s)) (sync (stream-end s)))
    (declare (fixnum fill sync))
    (when (< sync fill)
      (let ((nl (do ((i (- fill 1) (- i 1))) ((< i sync) -1)
                  (declare (fixnum i))
                  (when (eq (%schar buf i) #\Newline) (return i)))))
        (declare (fixnum nl))
        (setf (stream-column s) (if (< nl 0) (+ (stream-column s) (- fill sync)) (- fill nl 1)))
        (setf (stream-end s) fill)))
    (stream-column s)))

(defun %string-out-reset (s fill)
  "Set the length of the contents of the :string-out stream S."
  (let ((buf (stream-buffer s)))
    (setf (stream-index s) fill (stream-end s) fill)
    (setf (stream-column s)
          (do ((i (- fill 1) (- i 1))) ((< i 0) fill)
            (when (eq (%schar buf i) #\Newline) (return (- fill i 1)))))))

(defun %string-out-contents (s)
  "A fresh string of the contents of the :string-out stream S, which is emptied."
  (let* ((n (stream-index s)) (r (%make-string n #\Space)))
    (%string-copy r 0 (stream-buffer s) 0 n)
    (setf (stream-index s) 0 (stream-end s) 0 (stream-column s) 0)
    r))

(defun get-output-stream-string (s)
  (unless (%streamp s) (%type-error s 'string-stream))
  (case (stream-kind s)
    (:string-out
     (if (and (null (stream-elt-type s)) (eql (stream-index s) 0))
         (make-array 0 :element-type nil)   ; a string of element type NIL
         (%string-out-contents s)))
    (:string-fp
     (let* ((buf (stream-buffer s)) (r (subseq buf 0)))
       (setf (fill-pointer buf) 0)
       (setf (stream-column s) 0)
       r))
    (t (%type-error s 'string-stream))))

;;; A string output stream for the duration of one call (FORMAT NIL,
;;; PRINC-TO-STRING ...): one stream is kept for reuse.  When the output
;;; function is left by a non-local exit, the stream is simply not put back.
(defvar *%scratch-string-stream* nil)

(defun %scratch-string-stream ()
  (let ((s *%scratch-string-stream*))
    (if s
        (progn (setq *%scratch-string-stream* nil) s)
        (%make-string-out))))

(defun %scratch-string-result (s)
  "The contents of the scratch stream S, which is given back."
  (let ((r (%string-out-contents s)))
    (when (<= (%string-length (stream-buffer s)) 4096)
      (setq *%scratch-string-stream* s))
    r))

(defun make-string-input-stream (string &optional (start 0) end)
  (unless (stringp string) (%type-error string 'string))
  (let ((len (length string)))
    (unless (and (integerp start) (<= 0 start len)) (%type-error start `(integer 0 ,len)))
    (unless (or (null end) (and (integerp end) (<= start end len)))
      (%type-error end `(or null (integer ,start ,len))))
    (let ((s (%make-sstream :string-in string start (or end len) :input)))
      ;; (READ-CHAR and PEEK-CHAR read a simple string inline: %FS-FAST-CHAR)
      (when (%simple-string-p string) (setf (stream-fast s) :string))
      s)))

(defun %output-stream-object-p (x) (and (%streamp x) (output-stream-p x)))
(defun %input-stream-object-p (x) (and (%streamp x) (input-stream-p x)))
(defun %check-output-stream (x)
  (unless (%output-stream-object-p x) (%type-error x '(satisfies %output-stream-object-p)))
  x)
(defun %check-input-stream (x)
  (unless (%input-stream-object-p x) (%type-error x '(satisfies %input-stream-object-p)))
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

;;; The octets of a file stream are a simple (unsigned-byte 8) vector made by
;;; the stream functions, accessed with the %BYTES- primitives.

(defun %fs-ensure (s n)
  "Make room for N octets."
  (let* ((v (file-stream-octets s)) (len (%bytes-length v)))
    (declare (fixnum len))
    (when (> n len)
      (let ((new (%make-bytes (max n (* 2 len) 64))) (fill (file-stream-fill s)))
        (declare (fixnum fill))
        (unless (%storage-copy new 0 v 0 fill)     ; array.copy
          (do ((i 0 (+ i 1))) ((>= i fill))
            (declare (fixnum i))
            (%set-bytes-ref new i (%bytes-ref v i))))
        (setf (file-stream-octets s) new)))))

(defun %fs-put-octet (s b)
  (let ((p (file-stream-pos s)))
    (declare (fixnum p))
    (when (>= p (%bytes-length (file-stream-octets s))) (%fs-ensure s (+ p 1)))
    (%set-bytes-ref (file-stream-octets s) p (logand b 255))
    (setf (file-stream-pos s) (+ p 1))
    (when (> (+ p 1) (the fixnum (file-stream-fill s))) (setf (file-stream-fill s) (+ p 1)))
    (setf (file-stream-dirty s) t)))

(defun %fs-get-octet (s)
  "The next octet, or NIL at end of file."
  (let ((p (file-stream-pos s)))
    (if (>= p (file-stream-fill s))
        nil
        (progn (setf (file-stream-pos s) (+ p 1))
               (%bytes-ref (file-stream-octets s) p)))))

(defun %fs-decode-char (s advance)
  "Decode the UTF-8 character at the current position (:eof at end)."
  (let* ((v (file-stream-octets s)) (fill (file-stream-fill s)) (p (file-stream-pos s)))
    (declare (fixnum fill p))
    (if (>= p fill)
        :eof
        (let ((b (%bytes-ref v p)))
          (declare (fixnum b))
          (if (< b #x80)
              (progn (when advance (setf (file-stream-pos s) (+ p 1))) (code-char b))
              (let* ((n (cond ((>= b #xF0) 3) ((>= b #xE0) 2) ((>= b #xC0) 1) (t 0)))
                     (code (logand b (ash #x3F (- n)))))
                (dotimes (i n)
                  (let ((q (+ p 1 i)))
                    (setq code (logior (ash code 6)
                                       (if (< q fill) (logand (%bytes-ref v q) #x3F) 0)))))
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

(defun %fs-write-chars (s string start end)
  "Write the characters START..END of the simple string STRING to the
character file stream S, encoded in UTF-8, at once."
  (declare (fixnum start end))
  (let ((pos (file-stream-pos s)))
    (declare (fixnum pos))
    (%fs-ensure s (+ pos (* 4 (- end start))))
    (setq pos (%utf8-encode-into string start end (file-stream-octets s) pos))
    (setf (file-stream-pos s) pos)
    (when (> pos (the fixnum (file-stream-fill s))) (setf (file-stream-fill s) pos))
    (setf (file-stream-dirty s) t)))

(defun %fs-read-element (s)
  "Read one binary element (an integer) or NIL at end of file."
  (let ((n (file-stream-esize s)) (p (file-stream-pos s)))
    (if (> (+ p n) (file-stream-fill s))
        nil
        (let ((v (file-stream-octets s)) (x 0))
          (dotimes (i n) (setq x (logior x (ash (%bytes-ref v (+ p i)) (* 8 i)))))
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
  "Write the first N octets of OCTETS to the file NAME; true on success."
  (let ((fs (%host-fs)))
    (if fs
        ;; at once, through the memory (runtime/js.lisp)
        (%fs-write-octets fs name octets n)
        (progn
          (if (%bytes-p octets)
              (dotimes (i n) (%put-char-buf (%bytes-ref octets i)))
              (dotimes (i n) (%put-char-buf (aref octets i))))
          (%put-char-buf -1)
          (%host-name-buffer name)
          (>= (%file-open 2) 0)))))

(defun %host-read-octets (name)
  "The contents of file NAME as an octet vector, or NIL."
  (let ((fs (%host-fs)))
    (if fs
        (%fs-read-octets fs name)
        (progn
          (%host-name-buffer name)
          (let ((n (%file-open 3)))
            (if (< n 0)
                nil
                (let ((v (%make-bytes n)))
                  (dotimes (i n v) (%set-bytes-ref v i (char-code (%host-char i)))))))))))

;;; ------------------------------------------------------------
;;; output
;;; ------------------------------------------------------------

(defun %stream-write-char (s c)
  ;; an ASCII character to a character output file: one octet
  (when (and (%fast-out-file-p s) (%fx< (char-code c) 128))
    (let* ((sl (%instance-slots s)) (cur (%sl-cursor sl)) (pos (%cursor-ref cur 0))
           (octets (%svref sl +fs-octets-slot+)))
      (when (%fx>= pos (%bytes-length octets))
        (%fs-ensure s (%fx+ pos 1))
        (setq octets (%svref sl +fs-octets-slot+)))
      (%set-bytes-ref octets pos (char-code c))
      (%cursor-set cur 0 (%fx+ pos 1))
      (when (%fx> (%fx+ pos 1) (%cursor-ref cur 1)) (%cursor-set cur 1 (%fx+ pos 1)))
      (%set-svref sl +fs-dirty-slot+ t)
      (%set-svref sl +stream-column-slot+
                  (if (eql c #\Newline) 0 (%fx+ (%svref sl +stream-column-slot+) 1)))
      (return-from %stream-write-char c)))
  (let ((kind (stream-kind s)))
    (if (eq kind :string-out)
        ;; the column of a :string-out stream is computed on demand
        (let ((buf (stream-buffer s)) (i (stream-index s)))
          (declare (fixnum i))
          (when (>= i (%string-length buf)) (setq buf (%string-out-grow s 1)))
          (%set-schar buf i c)
          (setf (stream-index s) (+ i 1))
          c)
        (%stream-write-char-1 s kind c))))

(defun %stream-write-char-1 (s kind c)
  (case kind
    (:gray (return-from %stream-write-char-1 (stream-write-char s c)))
    (:fd-out (%write-char c (stream-fd s)))
    (:string-fp (vector-push-extend c (stream-buffer s)))
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

(defun %write-spaces (stream n)
  (%write-chars stream #\Space n))

(defun %write-chars (stream c n)
  "Write the character C N times."
  (when (> n 0)
    (if (eq (stream-kind stream) :string-out)
        (let* ((fill (stream-index stream)) (buf (%string-out-grow stream n)))
          (declare (fixnum fill n))
          (do ((i fill (+ i 1)) (e (+ fill n))) ((>= i e) (setf (stream-index stream) e))
            (declare (fixnum i e))
            (%set-schar buf i c)))
        (dotimes (i n) (%stream-write-char stream c)))))

(defun %check-seq-bounds (seq start end)
  (let ((len (length seq)))
    (unless (and (integerp start) (<= 0 start)) (%type-error start 'unsigned-byte))
    (unless (or (null end) (and (integerp end) (<= 0 end))) (%type-error end '(or null unsigned-byte)))
    (let ((end (or end len)))
      (unless (<= start end len)
        (error 'simple-error :format-control "Bad bounding indices ~S, ~S for a sequence of length ~S"
                             :format-arguments (list start end len)))
      end)))

(defun %string-out-write (s string start end)
  "Append the characters START..END of the simple string STRING to the
:string-out stream S."
  (declare (fixnum start end))
  (let* ((n (- end start)) (fill (stream-index s)) (buf (stream-buffer s)))
    (declare (fixnum n fill))
    (when (> (+ fill n) (%string-length buf)) (setq buf (%string-out-grow s n)))
    (%string-copy buf fill string start n)
    (setf (stream-index s) (+ fill n))
    string))

(defun %fs-fast-write-string (s string start end)
  "Write the characters START..END of the simple string STRING to S, an open
character output file (its FAST-OUT slot is :FILE), and update its column."
  (let* ((sl (%instance-slots s)) (cur (%sl-cursor sl)) (pos (%cursor-ref cur 0)) (n (%fx- end start)))
    (when (%fx> (%fx+ pos (%fx* 4 n)) (%bytes-length (%svref sl +fs-octets-slot+)))
      (%fs-ensure s (%fx+ pos (%fx* 4 n))))
    (let ((new (%utf8-encode-into string start end (%svref sl +fs-octets-slot+) pos)))
      (%cursor-set cur 0 new)
      (when (%fx> new (%cursor-ref cur 1)) (%cursor-set cur 1 new))
      (%set-svref sl +fs-dirty-slot+ t))
    (let ((nl (%string-last-newline string start end)))
      (%set-svref sl +stream-column-slot+
                  (if (%fx< nl 0) (%fx+ (%svref sl +stream-column-slot+) n) (%fx- end (%fx+ nl 1)))))
    string))

(defun %write-string-2 (string stream)
  (let ((s (if (or (null stream) (eq stream t)) (%output-stream stream) stream)))
    (when (and (%simple-string-p string) (%fast-out-file-p s))
      (return-from %write-string-2 (%fs-fast-write-string s string 0 (%string-length string))))
    (if (and (%simple-string-p string) (%streamp s))
        (if (eq (stream-kind s) :string-out)
            (%string-out-write s string 0 (%string-length string))
            (progn (%stream-write-simple-string s string 0 (%string-length string)) string))
        (write-string string stream))))

(defun %write-string-4 (string stream start end)
  "WRITE-STRING with its keyword arguments as positional ones."
  (unless (stringp string) (%type-error string 'string))
  (let ((s (%output-stream stream)) (end (%check-seq-bounds string start end)))
    (cond ((%simple-string-p string) (%stream-write-simple-string s string start end))
          ((eq (stream-kind s) :gray) (stream-write-string s string start end))
          ((eq (stream-kind s) :broadcast)
           (dolist (x (stream-streams s)) (%write-string-4 string x start end)))
          (t (do ((i start (+ i 1))) ((>= i end))
               (%stream-write-char s (char string i)))))
    string))

(define-compiler-macro write-string (&whole form string &optional (stream nil streamp) &rest keys)
  (cond ((null keys) `(%write-string-2 ,string ,(if streamp stream nil)))
        ;; literal :START and :END: no keyword list
        ((and (member (length keys) '(2 4))
              (member (car keys) '(:start :end))
              (or (null (cddr keys))
                  (and (member (caddr keys) '(:start :end)) (not (eq (car keys) (caddr keys))))))
         (let* ((str (gensym "STRING")) (st (gensym "STREAM")) (a (gensym "A")) (b (gensym "B"))
                (first-start (eq (car keys) :start)))
           `(let* ((,str ,string) (,st ,stream) (,a ,(cadr keys))
                   ,@(when (cddr keys) `((,b ,(cadddr keys)))))
              (%write-string-4 ,str ,st
                               ,(if first-start a (if (cddr keys) b 0))
                               ,(if first-start (if (cddr keys) b nil) a)))))
        (t form)))

(defun %stream-note-string-column (s string start end)
  "Update the column of S after the characters START..END of STRING were written."
  (declare (fixnum start end))
  (let ((nl (%string-last-newline string start end)))
    (declare (fixnum nl))
    (setf (stream-column s) (if (< nl 0) (+ (stream-column s) (- end start)) (- end nl 1)))))

(defun %stream-write-simple-string (s string start end)
  "Write the characters START..END of the simple string STRING to the stream S."
  (declare (fixnum start end))
  (when (< start end)
    (case (stream-kind s)
      (:string-out (%string-out-write s string start end))
      (:gray (stream-write-string s string start end))
      (:string-fp
       (if (%string-buffer-append (stream-buffer s) string start end)
           (%stream-note-string-column s string start end)
           (do ((i start (+ i 1))) ((>= i end))
             (declare (fixnum i))
             (%stream-write-char s (%schar string i)))))
      (:fd-out
       (let ((fd (stream-fd s)))
         (do ((i start (+ i 1))) ((>= i end))
           (declare (fixnum i))
           (%write-char (%schar string i) fd)))
       (%stream-note-string-column s string start end))
      (:file
       (unless (and (stream-open s) (member (stream-direction s) '(:output :io))
                    (eql (file-stream-esize s) 0))
         (%not-output-stream s 'character))
       (%fs-write-chars s string start end)
       (%stream-note-string-column s string start end))
      ;; each component stream gets the whole string
      (:broadcast (dolist (x (stream-streams s)) (%stream-write-simple-string x string start end)))
      ((:two-way :echo)
       (%stream-write-simple-string (cadr (stream-streams s)) string start end)
       (%stream-note-string-column s string start end))
      (:synonym
       (%stream-write-simple-string (%synonym-target s) string start end)
       (%stream-note-string-column s string start end))
      (:pp
       (if (do ((i start (+ i 1))) ((>= i end) t)
             (declare (fixnum i))
             (when (eq (%schar string i) #\Newline) (return nil)))
           (progn (%pp-capture-write-string (stream-buffer s) string start end)
                  (setf (stream-column s) (+ (stream-column s) (- end start))))
           (do ((i start (+ i 1))) ((>= i end))
             (declare (fixnum i))
             (%stream-write-char s (%schar string i)))))
      (t (do ((i start (+ i 1))) ((>= i end))
           (declare (fixnum i))
           (%stream-write-char s (%schar string i))))))
  nil)

(defun write-string (string &optional stream &key (start 0) end)
  (%write-string-4 string stream start end))

;; without keyword arguments: no keyword list to parse
(define-compiler-macro write-line (&whole form string &optional (stream nil) &rest keys)
  (if keys
      form
      (let ((str (gensym "STRING")) (st (gensym "STREAM")))
        `(let ((,str ,string) (,st ,stream))
           (%write-string-2 ,str ,st)
           (write-char #\Newline ,st)
           ,str))))

(defun write-line (string &optional stream &key (start 0) end)
  (write-string string stream :start start :end end)
  (write-char #\Newline stream)
  string)

(defun terpri (&optional stream)
  (let ((s (%output-stream stream)))
    (if (eq (stream-kind s) :gray) (stream-terpri s) (%stream-write-char s #\Newline)))
  nil)

(defun %stream-column (s)
  (case (stream-kind s)
    (:gray (stream-line-column s))
    (:synonym (%stream-column (%synonym-target s)))
    ((:two-way :echo) (%stream-column (cadr (stream-streams s))))
    (:pp (%pp-stream-column s))
    (:string-out (%string-out-column s))
    ;; the column of the last component stream, as the values of the other
    ;; operations on a broadcast stream
    (:broadcast (let ((l (last (stream-streams s)))) (if l (%stream-column (car l)) 0)))
    (t (stream-column s))))

(defun fresh-line (&optional stream)
  (let ((s (%output-stream stream)))
    (when (eq (stream-kind s) :gray)
      (return-from fresh-line (stream-fresh-line s)))
    (if (eql (%stream-column s) 0) nil (progn (write-char #\Newline s) t))))

(defun %finish-output (s)
  (case (stream-kind s)
    (:gray (stream-finish-output s))
    (:file (%fs-flush s))
    ((:fd-out) (%flush))
    (:socket (%socket-flush s))
    (:broadcast (mapc #'%finish-output (stream-streams s)))
    ((:two-way :echo) (%finish-output (cadr (stream-streams s))))
    (:synonym (%finish-output (%synonym-target s)))))

(defun finish-output (&optional stream) (%finish-output (%output-stream stream)) nil)
(defun force-output (&optional stream)
  ;; FORCE-OUTPUT only initiates the output (CLHS 21.2): a file stream keeps
  ;; its contents in memory and writes the whole file back, so that is left
  ;; to FINISH-OUTPUT and CLOSE (writing on every FORCE-OUTPUT is quadratic)
  (let ((s (%output-stream stream)))
    (case (stream-kind s)
      (:file nil)
      (:gray (stream-force-output s))
      (t (%finish-output s))))
  nil)
(defun clear-output (&optional stream)
  (let ((s (%output-stream stream)))
    (when (eq (stream-kind s) :gray) (stream-clear-output s)))
  nil)

(defun %stream-write-byte (s b)
  (case (stream-kind s)
    (:gray (stream-write-byte s b))
    (:file (unless (and (stream-open s) (member (stream-direction s) '(:output :io))
                        (> (file-stream-esize s) 0))
             (%not-output-stream s 'binary))
     (%fs-write-element s b))
    (:socket (unless (stream-open s) (%not-output-stream s 'binary))
     (vector-push-extend (logand b 255) (socket-stream-out s))
     (when (>= (length (socket-stream-out s)) 65536) (%socket-flush s)))
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

(defun %fs-refresh-fast (s)
  "Mark the file stream S for READ-CHAR inline if it is an open character
input file without an unread character."
  (setf (stream-fast s)
        (and (null (stream-unread s)) (stream-open s) (eql (file-stream-esize s) 0)
             (member (stream-direction s) '(:input :io))
             :file)))

(defmacro %character-input-file-p (s)
  "True when the file stream S is open for character input."
  `(and (eql (file-stream-esize ,s) 0) (stream-open ,s)
        (let ((d (stream-direction ,s))) (or (eq d :input) (eq d :io)))))

(defun %stream-read-char (s)
  "Returns a character or :eof."
  (when (eq (stream-kind s) :gray)
    (return-from %stream-read-char (stream-read-char s)))
  (let ((u (stream-unread s)))
    (if u
        (progn (setf (stream-unread s) nil)
               (case (stream-kind s)
                 (:file (%fs-refresh-fast s))
                 (:string-in (when (and (stream-open s) (%simple-string-p (stream-buffer s)))
                               (setf (stream-fast s) :string))))
               u)
        (case (stream-kind s)
          (:string-in (let ((i (stream-index s)) (buf (stream-buffer s)))
                        (if (>= i (stream-end s)) :eof
                            (progn (setf (stream-index s) (+ i 1))
                                   (if (%simple-string-p buf) (%schar buf i) (char buf i))))))
          (:file (unless (%character-input-file-p s) (%not-input-stream s 'character))
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
  (when (eq (stream-kind s) :gray)
    (return-from %stream-peek-char (stream-peek-char s)))
  (let ((u (stream-unread s)))
    (if u
        u
        (case (stream-kind s)
          (:string-in (let ((i (stream-index s)) (buf (stream-buffer s)))
                        (if (>= i (stream-end s)) :eof
                            (if (%simple-string-p buf) (%schar buf i) (char buf i)))))
          (:file (unless (%character-input-file-p s) (%not-input-stream s 'character))
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
  (let ((s (%input-stream stream)))
    (if (and (eq (stream-kind s) :string-in) (null (stream-unread s)))
        (let ((i (stream-index s)) (buf (stream-buffer s)))
          (declare (fixnum i))
          (if (< i (the fixnum (stream-end s)))
              (progn (setf (stream-index s) (+ i 1))
                     (if (%simple-string-p buf) (%schar buf i) (char buf i)))
              (if eof-error-p (error 'end-of-file :stream s) eof-value)))
        (let ((c (if (and (eq (stream-kind s) :file) (null (stream-unread s))
                          (%character-input-file-p s))
                     ;; a character file: ASCII is taken straight from the octets
                     (let ((p (file-stream-pos s)))
                       (declare (fixnum p))
                       (if (< p (the fixnum (file-stream-fill s)))
                           (let ((b (%bytes-ref (file-stream-octets s) p)))
                             (declare (fixnum b))
                             (if (< b #x80)
                                 (progn (setf (file-stream-pos s) (+ p 1)) (code-char b))
                                 (%fs-decode-char s t)))
                           :eof))
                     (%stream-read-char s))))
          (if (eq c :eof)
              (if eof-error-p (error 'end-of-file :stream s) eof-value)
              c)))))

;;; READ-CHAR and PEEK-CHAR with up to three arguments call these functions
;;; of fixed arguments (compiler macros below).  A character input file and
;;; a string input stream are read here without the accessors' type checks:
;;; one test of the instance's layout, then its slots by their indices
;;; (checked against the accessors when this file is loaded).

(defmacro %fast-input-case (s file string other)
  "FILE when S is an open character input file without an unread character,
STRING when it is a string input stream without one, else OTHER."
  `(if (and (%instancep ,s) (null (%instance-ref ,s +stream-unread-slot+)))
       (let ((layout (%instance-layout ,s)))
         (cond ((and (eq layout *file-stream-layout*)
                     (%instance-ref ,s +stream-open-slot+)
                     (eql (%instance-ref ,s +fs-esize-slot+) 0)
                     (let ((d (%instance-ref ,s +stream-direction-slot+))) (or (eq d :input) (eq d :io))))
                ,file)
               ((and (eq layout *string-stream-layout*)
                     (eq (%instance-ref ,s +stream-kind-slot+) :string-in))
                ,string)
               (t ,other)))
       ,other))

(defun %read-char-3 (stream eof-error-p eof-value)
  (let ((s (cond ((null stream) *standard-input*) ((eq stream t) *terminal-io*) (t stream))))
    (%fast-input-case s
      (let ((p (file-stream-pos s)))
        (declare (fixnum p))
        (if (< p (the fixnum (file-stream-fill s)))
            (let ((b (%bytes-ref (%instance-ref s +fs-octets-slot+) p)))
              (declare (fixnum b))
              (if (< b #x80)
                  (progn (setf (file-stream-pos s) (+ p 1)) (code-char b))
                  (read-char s eof-error-p eof-value)))
            (if eof-error-p (error 'end-of-file :stream s) eof-value)))
      (let ((i (%instance-ref s +stream-index-slot+)) (buf (%instance-ref s +stream-buffer-slot+)))
        (declare (fixnum i))
        (if (< i (the fixnum (%instance-ref s +stream-end-slot+)))
            (progn (%set-instance-ref s +stream-index-slot+ (+ i 1))
                   (if (%simple-string-p buf) (%schar buf i) (char buf i)))
            (if eof-error-p (error 'end-of-file :stream s) eof-value)))
      (read-char s eof-error-p eof-value))))

(defun %peek-char-3 (stream eof-error-p eof-value)
  "PEEK-CHAR with PEEK-TYPE NIL."
  (let ((s (cond ((null stream) *standard-input*) ((eq stream t) *terminal-io*) (t stream))))
    (%fast-input-case s
      (let ((p (file-stream-pos s)))
        (declare (fixnum p))
        (if (< p (the fixnum (file-stream-fill s)))
            (let ((b (%bytes-ref (%instance-ref s +fs-octets-slot+) p)))
              (declare (fixnum b))
              (if (< b #x80) (code-char b) (peek-char nil s eof-error-p eof-value)))
            (if eof-error-p (error 'end-of-file :stream s) eof-value)))
      (let ((i (%instance-ref s +stream-index-slot+)) (buf (%instance-ref s +stream-buffer-slot+)))
        (declare (fixnum i))
        (if (< i (the fixnum (%instance-ref s +stream-end-slot+)))
            (if (%simple-string-p buf) (%schar buf i) (char buf i))
            (if eof-error-p (error 'end-of-file :stream s) eof-value)))
      (peek-char nil s eof-error-p eof-value))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %inline-file-char-expansion (stream eof-error-p eof-value advance fallback)
    "READ-CHAR (ADVANCE) or PEEK-CHAR of STREAM at the call site: the next
octet of a character input file when it is ASCII, else the call FALLBACK."
    (let ((s (gensym "S")) (e (gensym "E")) (v (gensym "V")) (p (gensym "P")) (b (gensym "B"))
          (sl (gensym "SLOTS")))
      (declare (ignore p b sl))
      `(let ((,s ,stream) (,e ,eof-error-p) (,v ,eof-value))
         (or (%fs-fast-char ,s ,advance)
             (,fallback ,s ,e ,v))))))

(define-compiler-macro read-char (&whole form &optional (stream nil) (eof-error-p t) eof-value
                                         (recursive-p nil recursive-p-p))
  (if (and recursive-p-p (not (constantp recursive-p)))
      form
      (%inline-file-char-expansion stream eof-error-p eof-value t '%read-char-3)))

(define-compiler-macro peek-char (&whole form &optional (peek-type nil) (stream nil) (eof-error-p t) eof-value
                                         (recursive-p nil recursive-p-p))
  (if (or (not (null peek-type)) (and recursive-p-p (not (constantp recursive-p))))
      form
      (%inline-file-char-expansion stream eof-error-p eof-value nil '%peek-char-3)))

(defun %read-line-3 (stream eof-error-p eof-value)
  "READ-LINE of up to three arguments; a character input file is read here."
  (let ((s (cond ((null stream) *standard-input*) ((eq stream t) *terminal-io*) (t stream))))
    (if (and (%instancep s) (eq (%instance-layout s) *file-stream-layout*)
             (eq (%instance-ref s +stream-fast-slot+) :file))
        (let* ((sl (%instance-slots s)) (cur (%sl-cursor sl)) (p (%cursor-ref cur 0)) (fill (%cursor-ref cur 1)))
          (if (%fx>= p fill)
              (if eof-error-p (error 'end-of-file :stream s) (values eof-value t))
              (let* ((v (%svref sl +fs-octets-slot+)) (e (%bytes-find v 10 p fill)))
                (%cursor-set cur 0 (if (%fx< e fill) (%fx+ e 1) e))
                (values (%utf8-decode v p e) (%fx>= e fill)))))
        (read-line s eof-error-p eof-value))))

(define-compiler-macro read-line (&whole form &optional (stream nil) (eof-error-p t) eof-value
                                         (recursive-p nil recursive-p-p))
  (if (and recursive-p-p (not (constantp recursive-p)))
      form
      `(%read-line-3 ,stream ,eof-error-p ,eof-value)))

(defun %stream-unread-char (s c)
  (case (stream-kind s)
    (:gray (stream-unread-char s c))
    (:synonym (%stream-unread-char (%synonym-target s) c))
    (:two-way (%stream-unread-char (car (stream-streams s)) c))
    (:concatenated (if (stream-streams s)
                       (%stream-unread-char (car (stream-streams s)) c)
                       (setf (stream-unread s) c)))
    (:string-in (if (and (> (stream-index s) 0)
                         (eql (char (stream-buffer s) (- (stream-index s) 1)) c))
                    (setf (stream-index s) (- (stream-index s) 1))
                    (setf (stream-unread s) c (stream-fast s) nil)))
    (:file (let ((n (%utf8-length (char-code c))))
             (if (>= (file-stream-pos s) n)
                 (setf (file-stream-pos s) (- (file-stream-pos s) n))
                 ;; (READ-CHAR inline reads the octets only without one)
                 (setf (stream-unread s) c (stream-fast s) nil))))
    (t (setf (stream-unread s) c))))

(defun unread-char (c &optional stream)
  (%stream-unread-char (%input-stream stream) c)
  nil)

(defun peek-char (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((s (%input-stream stream)))
    (when (null peek-type)
      (let ((c (%stream-peek-char s)))
        (return-from peek-char
          (if (eq c :eof)
              (if eof-error-p (error 'end-of-file :stream s) eof-value)
              c))))
    (unless (or (eq peek-type t) (characterp peek-type))
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
  (let ((s (%input-stream stream)))
    (if (eq (stream-kind s) :gray)
        (let ((c (stream-read-char-no-hang s)))
          (if (eq c :eof)
              (if eof-error-p (error 'end-of-file :stream s) eof-value)
              c))
        (read-char s eof-error-p eof-value recursive-p))))

(defun %stream-listen (s)
  (case (stream-kind s)
    (:gray (stream-listen s))
    (:fd-in (not (null (stream-unread s))))
    (:socket (%socket-listen s))
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
    (case (stream-kind s)
      (:fd-in (setf (stream-unread s) nil))
      (:gray (stream-clear-input s)))
    nil))

(defun %simple-substring (string start end)
  "A fresh simple string of the characters START..END of the simple string STRING."
  (declare (fixnum start end))
  (let ((r (%make-string (- end start) #\Space)))
    (%string-copy r 0 string start (- end start))
    r))

(defun %file-read-line (s)
  "READ-LINE for a character file stream at a character boundary: the line
and whether the newline was missing, or :eof."
  (let* ((v (file-stream-octets s)) (fill (file-stream-fill s)) (p (file-stream-pos s))
         (e p))
    (declare (fixnum fill p e))
    (if (>= p fill)
        :eof
        (progn
          ;; the end of the line; UTF-8 has no octet 10 inside a character
          (do () ((>= e fill))
            (let ((b (%bytes-ref v e)))
              (declare (fixnum b))
              (when (eql b 10) (return))
              (setq e (+ e 1))))
          (let ((line (%utf8-decode v p e)))
            (setf (file-stream-pos s) (if (< e fill) (+ e 1) e))
            (values line (>= e fill)))))))

(defun read-line (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let* ((s (%input-stream stream)) (kind (stream-kind s)))
    (cond
      ((eq kind :gray)
       (multiple-value-bind (line missing-newline-p) (stream-read-line s)
         (if (and missing-newline-p (or (eq line :eof) (= (length line) 0)))
             (if eof-error-p (error 'end-of-file :stream s) (values eof-value t))
             (values line missing-newline-p))))
      ;; a string stream: the line is a part of the string
      ((and (eq kind :string-in) (null (stream-unread s)) (%simple-string-p (stream-buffer s)))
       (let* ((buf (stream-buffer s)) (i (stream-index s)) (end (stream-end s)) (e i))
         (declare (fixnum i end e))
         (if (>= i end)
             (if eof-error-p (error 'end-of-file :stream s) (values eof-value t))
             (progn
               (do () ((or (>= e end) (eq (%schar buf e) #\Newline))) (setq e (+ e 1)))
               (setf (stream-index s) (if (< e end) (+ e 1) e))
               (values (%simple-substring buf i e) (>= e end))))))
      ((and (eq kind :file) (null (stream-unread s)) (eql (file-stream-esize s) 0) (stream-open s)
            (member (stream-direction s) '(:input :io)))
       (multiple-value-bind (line missing-newline-p) (%file-read-line s)
         (if (eq line :eof)
             (if eof-error-p (error 'end-of-file :stream s) (values eof-value t))
             (values line missing-newline-p))))
      (t
       (let ((buf (%make-string 80 #\Space)) (n 0))
         (declare (fixnum n))
         (loop
           (let ((c (%stream-read-char s)))
             (cond ((eq c :eof)
                    (return (if (= n 0)
                                (if eof-error-p (error 'end-of-file :stream s) (values eof-value t))
                                (values (%simple-substring buf 0 n) t))))
                   ((char= c #\Newline) (return (values (%simple-substring buf 0 n) nil)))
                   (t (when (>= n (%string-length buf))
                        (let ((new (%make-string (* 2 n) #\Space)))
                          (%string-copy new 0 buf 0 n)
                          (setq buf new)))
                      (%set-schar buf n c)
                      (setq n (+ n 1)))))))))))

(defun %stream-read-byte (s)
  "An integer or :eof."
  (case (stream-kind s)
    (:gray (stream-read-byte s))
    (:file (unless (and (stream-open s) (member (stream-direction s) '(:input :io))
                        (> (file-stream-esize s) 0))
             (%not-input-stream s 'binary))
     (or (%fs-read-element s) :eof))
    (:socket (%socket-read-byte s))
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
    (:gray (let ((et (stream-element-type s))) (and et (not (subtypep et 'character)))))
    (:file (> (file-stream-esize s) 0))
    (:socket t)
    ((:two-way :echo) (%binary-stream-p (if input (car (stream-streams s)) (cadr (stream-streams s))) input))
    (:concatenated (and (stream-streams s) (%binary-stream-p (car (stream-streams s)) input)))
    (:broadcast (and (stream-streams s) (%binary-stream-p (car (last (stream-streams s))) input)))
    (:synonym (%binary-stream-p (%synonym-target s) input))
    (t nil)))

(defun read-sequence (seq stream &key (start 0) end)
  (unless (typep seq 'sequence) (%type-error seq 'sequence))
  (%check-stream stream)
  (when (eq (stream-kind stream) :gray)
    (return-from read-sequence
      (stream-read-sequence stream seq start (%check-seq-bounds seq start end))))
  (when (and (eq (stream-kind stream) :socket) (vectorp seq))
    (return-from read-sequence (%socket-read-sequence seq stream start (%check-seq-bounds seq start end))))
  (let ((end (%check-seq-bounds seq start end))
        (binary (%binary-stream-p stream t))
        (i start))
    ;; a simple string from a string stream or a character file, octets from
    ;; a file of octets: without a dispatch per element
    (when (and (null (stream-unread stream)) (fixnump start))
      (let ((kind (stream-kind stream)))
        (cond ((and (%simple-string-p seq) (eq kind :string-in) (%simple-string-p (stream-buffer stream)))
               (let* ((pos (stream-index stream))
                      (n (min (- end start) (- (stream-end stream) pos))))
                 (%string-copy seq start (stream-buffer stream) pos n)
                 (setf (stream-index stream) (+ pos n))
                 (return-from read-sequence (+ start n))))
              ((and (%simple-string-p seq) (eq kind :file) (%character-input-file-p stream))
               (loop
                 (when (>= i end) (return-from read-sequence i))
                 (let ((c (%fs-decode-char stream t)))
                   (when (eq c :eof) (return-from read-sequence i))
                   (%set-schar seq i c)
                   (setq i (+ i 1)))))
              ((and (%bytes-p seq) (eq kind :file) (eql (file-stream-esize stream) 1)
                    (not (file-stream-signed stream)) (stream-open stream)
                    (member (stream-direction stream) '(:input :io)))
               (let* ((pos (file-stream-pos stream)) (octets (file-stream-octets stream))
                      (n (min (- end start) (- (file-stream-fill stream) pos))))
                 (declare (fixnum pos n))
                 (dotimes (k n) (%set-bytes-ref seq (+ start k) (%bytes-ref octets (+ pos k))))
                 (setf (file-stream-pos stream) (+ pos n))
                 (return-from read-sequence (+ start n)))))))
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
    (when (eq (stream-kind s) :gray)
      (stream-write-sequence s seq start end)
      (return-from write-sequence seq))
    (when (and (eq (stream-kind s) :socket) (vectorp seq))
      (unless (stream-open s) (%not-output-stream s 'binary))
      (let ((out (socket-stream-out s)))
        (do ((i start (+ i 1))) ((>= i end))
          (vector-push-extend (logand (aref seq i) 255) out))
        (when (>= (length out) 65536) (%socket-flush s)))
      (return-from write-sequence seq))
    ;; a string to a character stream is WRITE-STRING; octets to a file of octets
    (cond ((and (stringp seq) (not binary))
           (write-string seq s :start start :end end)
           (return-from write-sequence seq))
          ((and (%bytes-p seq) (eq (stream-kind s) :file) (eql (file-stream-esize s) 1)
                (stream-open s) (member (stream-direction s) '(:output :io)))
           (%fs-ensure s (+ (file-stream-pos s) (- end start)))
           (do ((i start (+ i 1))) ((>= i end))
             (%fs-put-octet s (%bytes-ref seq i)))
           (return-from write-sequence seq)))
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
           ;; an input file is read at once: reading it tells whether it exists
           (input-octets (and (eq direction :input) (%host-read-octets native)))
           (exists (or input-octets (%file-exists-p native)))
           (if-exists (if iep if-exists :new-version))
           (if-does-not-exist
             (if idnep if-does-not-exist
                 (cond ((or (eq direction :input) (eq direction :probe)) (if (eq direction :input) :error nil))
                       ((member if-exists '(:overwrite :append)) :error)
                       (t :create)))))
      (multiple-value-bind (bits signed) (%binary-element-spec element-type)
        (let ((octets nil) (pos 0) (created nil))
          (cond
            ((not exists)
             (when (eq if-does-not-exist :create) (setq created t))
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
             (setq octets input-octets)
             (unless octets (%file-error path "can't open ~A" native)))
            (t (setq octets (make-array 0 :element-type '(unsigned-byte 8)))))
          (let ((s (%make-file-stream :kind :file :pathname path :direction direction
                                 :elt-type (if bits
                                                   (list (if signed 'signed-byte 'unsigned-byte) bits)
                                                   'character)
                                 :octets octets :cursor (%make-fs-cursor pos (length octets))
                                 :esize (if bits (ceiling bits 8) 0) :signed signed
                                 :native native :external-format external-format
                                 :created created
                                 :fast (and (not bits) (member direction '(:input :io)) :file)
                                 :fast-out (and (not bits) (member direction '(:output :io)) :file)
                                 :dirty (and (member direction '(:output :io))
                                             (not (member if-exists '(:overwrite :append)))
                                             t))))
            (when (eq direction :probe) (setf (stream-open s) nil (stream-fast s) nil (stream-fast-out s) nil))
            s))))))

(defun %native-close (stream &key abort)
  (%check-stream stream)
  (when (stream-open stream)
    (when (eq (stream-kind stream) :file)
      (if abort
          (when (file-stream-created stream)
            (ignore-errors (delete-file (stream-pathname stream))))
          (%fs-flush stream)))
    (when (eq (stream-kind stream) :socket)
      (unless abort (ignore-errors (%socket-flush stream)))
      (ignore-errors (%tcp-close (socket-stream-id stream))))
    (setf (stream-open stream) nil (stream-fast stream) nil (stream-fast-out stream) nil))
  t)

;;; ------------------------------------------------------------
;;; TCP streams
;;; ------------------------------------------------------------

(defun %socket-flush (s)
  (let ((out (socket-stream-out s)))
    (when (> (length out) 0)
      (%tcp-write (socket-stream-id s) out)
      (setf (fill-pointer out) 0))))

(defun %socket-read-byte (s)
  (unless (stream-open s) (%not-input-stream s 'binary))
  (when (>= (socket-stream-in-pos s) (socket-stream-in-fill s))
    (if (socket-stream-in-eof s)
        (return-from %socket-read-byte :eof)
        (progn
          ;; send what was written before waiting for the answer
          (%socket-flush s)
          (let ((n (%tcp-read (socket-stream-id s) (socket-stream-in s))))
            (setf (socket-stream-in-pos s) 0 (socket-stream-in-fill s) n)
            (when (= n 0)
              (setf (socket-stream-in-eof s) t)
              (return-from %socket-read-byte :eof))))))
  (prog1 (aref (socket-stream-in s) (socket-stream-in-pos s))
    (setf (socket-stream-in-pos s) (+ (socket-stream-in-pos s) 1))))

(defun open-tcp-stream (host port)
  "A bidirectional binary stream of (UNSIGNED-BYTE 8) connected to HOST:PORT."
  (%make-stream :kind :socket :id (%tcp-connect host port) :direction :io
                :elt-type '(unsigned-byte 8)))

(defun open-local-stream (path)
  "A bidirectional binary stream of (UNSIGNED-BYTE 8) connected to the Unix
domain socket PATH."
  (%make-stream :kind :socket :id (%local-connect path) :direction :io
                :elt-type '(unsigned-byte 8)))

(defun %socket-listen (s)
  (and (stream-open s)
       (or (< (socket-stream-in-pos s) (socket-stream-in-fill s))
           (and (not (socket-stream-in-eof s))
                (progn (%socket-flush s) (%tcp-available-p (socket-stream-id s)))
                (not (eq (%socket-peek-byte s) :eof))))))

(defun %socket-read-sequence (seq s start end)
  (let ((i start))
    (loop
      (when (>= i end) (return i))
      (when (>= (socket-stream-in-pos s) (socket-stream-in-fill s))
        ;; refill the buffer (waits for at least one octet)
        (let ((b (%socket-read-byte s)))
          (when (eq b :eof) (return i))
          (setf (socket-stream-in-pos s) (- (socket-stream-in-pos s) 1))))
      (let* ((pos (socket-stream-in-pos s))
             (n (min (- end i) (- (socket-stream-in-fill s) pos)))
             (in (socket-stream-in s)))
        (dotimes (k n) (setf (aref seq (+ i k)) (aref in (+ pos k))))
        (setf (socket-stream-in-pos s) (+ pos n))
        (setq i (+ i n))))))

(defun %socket-peek-byte (s)
  (let ((b (%socket-read-byte s)))
    (unless (eq b :eof) (setf (socket-stream-in-pos s) (- (socket-stream-in-pos s) 1)))
    b))

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
  (when (eq (stream-kind s) :gray)
    (return-from file-position
      (if posp
          (funcall #'(setf stream-file-position) pos s)
          (stream-file-position s))))
  (if posp
      (progn
        (unless (or (member pos '(:start :end)) (and (integerp pos) (>= pos 0)))
          (%type-error pos '(or (member :start :end) unsigned-byte)))
        (case (stream-kind s)
          (:file (let* ((n (max 1 (file-stream-esize s)))
                        (len (floor (file-stream-fill s) n))
                        (p (case pos (:start 0) (:end len) (t pos))))
                   (setf (stream-unread s) nil)
                   (%fs-refresh-fast s)
                   (if (<= p len)
                       (progn (setf (file-stream-pos s) (* p n)) t)
                       nil)))
          (:string-in (let ((p (case pos (:start 0) (:end (stream-end s)) (t pos))))
                        (setf (stream-unread s) nil)
                        (when (and (stream-open s) (%simple-string-p (stream-buffer s)))
                          (setf (stream-fast s) :string))
                        (if (<= p (stream-end s)) (progn (setf (stream-index s) p) t) nil)))
          (:string-out (let ((p (case pos (:start 0) (:end (stream-index s)) (t pos))))
                         (if (<= p (stream-index s)) (progn (%string-out-reset s p) t) nil)))
          (:string-fp (let* ((buf (stream-buffer s))
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
        (:string-out (stream-index s))
        (:string-fp (fill-pointer (stream-buffer s)))
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
             (let ((,var (%make-fill-pointer-output-stream ,string)))
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
