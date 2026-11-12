;;;; stream.lisp — streams

(defstruct (stream (:constructor %make-plain-stream) (:predicate %streamp))
  kind              ; :fd-out :fd-in :string-out :string-in :broadcast :two-way :echo :concatenated :synonym :file-out
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
  (element-type 'character)
  (line 1))

(defstruct (file-stream (:include stream) (:constructor %make-file-stream)))
(defstruct (string-stream (:include stream) (:constructor %make-string-stream)))
(defstruct (broadcast-stream (:include stream) (:constructor %make-broadcast-stream)))
(defstruct (two-way-stream (:include stream) (:constructor %make-two-way-stream)))
(defstruct (echo-stream (:include stream) (:constructor %make-echo-stream)))
(defstruct (concatenated-stream (:include stream) (:constructor %make-concatenated-stream)))
(defstruct (synonym-stream (:include stream) (:constructor %make-synonym-stream)))

(defun %make-stream (&rest args &key kind pathname &allow-other-keys)
  (apply (case kind
           ((:fd-out :fd-in :file-out) #'%make-file-stream)
           ((:string-out :string-in) (if pathname #'%make-file-stream #'%make-string-stream))
           (:broadcast #'%make-broadcast-stream)
           (:two-way #'%make-two-way-stream)
           (:echo #'%make-echo-stream)
           (:concatenated #'%make-concatenated-stream)
           (:synonym #'%make-synonym-stream)
           (t #'%make-plain-stream))
         args))

(defun streamp (x) (%streamp x))
(defun input-stream-p (s)
  (case (stream-kind s)
    ((:fd-in :string-in :two-way :echo :concatenated) t)
    (:synonym (input-stream-p (symbol-value (stream-symbol s))))
    (t nil)))
(defun output-stream-p (s)
  (case (stream-kind s)
    ((:fd-out :string-out :broadcast :two-way :echo :file-out :pp) t)
    (:synonym (output-stream-p (symbol-value (stream-symbol s))))
    (t nil)))
(defun open-stream-p (s) (stream-open s))
(defvar *interactive* nil "true when the REPL debugger should be used")
(defun interactive-stream-p (s)
  (and *interactive*
       (case (stream-kind s)
         (:fd-in t)
         (:two-way (interactive-stream-p (car (stream-streams s))))
         (:synonym (interactive-stream-p (symbol-value (stream-symbol s))))
         (t nil))))

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
        (t (%type-error designator 'stream))))

(defun %input-stream (designator)
  (cond ((null designator) *standard-input*)
        ((eq designator t) *terminal-io*)
        ((%streamp designator) designator)
        (t (%type-error designator 'stream))))

(defun make-string-output-stream (&key element-type)
  (declare (ignore element-type))
  (%make-stream :kind :string-out
                :buffer (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))

(defun get-output-stream-string (s)
  (let* ((buf (stream-buffer s)) (r (subseq buf 0)))
    (setf (fill-pointer buf) 0)
    (setf (stream-column s) 0)
    r))

(defun make-string-input-stream (string &optional (start 0) end)
  (%make-stream :kind :string-in :buffer string :index start :end (or end (length string))
                :direction :input))

(defun make-broadcast-stream (&rest streams) (%make-stream :kind :broadcast :streams streams))
(defun broadcast-stream-streams (s) (stream-streams s))
(defun make-two-way-stream (in out) (%make-stream :kind :two-way :streams (list in out)))
(defun two-way-stream-input-stream (s) (car (stream-streams s)))
(defun two-way-stream-output-stream (s) (cadr (stream-streams s)))
(defun make-echo-stream (in out) (%make-stream :kind :echo :streams (list in out)))
(defun echo-stream-input-stream (s) (car (stream-streams s)))
(defun echo-stream-output-stream (s) (cadr (stream-streams s)))
(defun make-concatenated-stream (&rest streams) (%make-stream :kind :concatenated :streams streams :direction :input))
(defun concatenated-stream-streams (s) (stream-streams s))
(defun make-synonym-stream (symbol) (%make-stream :kind :synonym :symbol symbol))
(defun synonym-stream-symbol (s) (stream-symbol s))

;;; ------------------------------------------------------------
;;; output
;;; ------------------------------------------------------------

(defun %stream-write-char (s c)
  (case (stream-kind s)
    (:fd-out (%write-char c (stream-fd s)))
    ((:string-out :file-out) (vector-push-extend c (stream-buffer s)))
    (:broadcast (dolist (x (stream-streams s)) (%stream-write-char x c)))
    ((:two-way :echo) (%stream-write-char (cadr (stream-streams s)) c))
    (:synonym (%stream-write-char (symbol-value (stream-symbol s)) c))
    (:pp (%pp-capture-write-char s c))
    (t (error "~S is not an output stream" s)))
  (if (char= c #\Newline)
      (setf (stream-column s) 0)
      (setf (stream-column s) (+ (stream-column s) 1)))
  c)

(defun write-char (c &optional stream)
  (%stream-write-char (%output-stream stream) c))

(defun write-string (string &optional stream &key (start 0) end)
  (let ((s (%output-stream stream)) (end (or end (length string))))
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
    (:synonym (%stream-column (symbol-value (stream-symbol s))))
    ((:two-way :echo) (%stream-column (cadr (stream-streams s))))
    (t (stream-column s))))

(defun fresh-line (&optional stream)
  (let ((s (%output-stream stream)))
    (if (eql (%stream-column s) 0) nil (progn (write-char #\Newline s) t))))

(defun finish-output (&optional stream) (declare (ignore stream)) (%flush) nil)
(defun force-output (&optional stream) (declare (ignore stream)) (%flush) nil)
(defun clear-output (&optional stream) (declare (ignore stream)) nil)

(defun write-byte (byte stream)
  (%stream-write-char stream (code-char byte))
  byte)

;;; ------------------------------------------------------------
;;; input
;;; ------------------------------------------------------------

(defun %stream-read-char (s)
  "Returns a character or :eof."
  (let ((u (stream-unread s)))
    (if u
        (progn (setf (stream-unread s) nil) u)
        (case (stream-kind s)
          (:fd-in (let ((code (%read-char)))
                    (if (< code 0) :eof (code-char code))))
          (:string-in (let ((i (stream-index s)))
                        (if (>= i (stream-end s)) :eof
                            (progn (setf (stream-index s) (+ i 1))
                                   (char (stream-buffer s) i)))))
          (:two-way (%stream-read-char (car (stream-streams s))))
          (:echo (let ((c (%stream-read-char (car (stream-streams s)))))
                   (unless (eq c :eof) (%stream-write-char (cadr (stream-streams s)) c))
                   c))
          (:concatenated
           (loop
             (when (null (stream-streams s)) (return :eof))
             (let ((c (%stream-read-char (car (stream-streams s)))))
               (if (eq c :eof) (pop (stream-streams s)) (return c)))))
          (:synonym (%stream-read-char (symbol-value (stream-symbol s))))
          (t (error "~S is not an input stream" s))))))

(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let* ((s (%input-stream stream)) (c (%stream-read-char s)))
    (if (eq c :eof)
        (if eof-error-p (error 'end-of-file :stream s) eof-value)
        c)))

(defun unread-char (c &optional stream)
  (let ((s (%input-stream stream)))
    (case (stream-kind s)
      (:synonym (unread-char c (symbol-value (stream-symbol s))))
      (:two-way (unread-char c (car (stream-streams s))))
      (t (setf (stream-unread s) c)))
    nil))

(defun peek-char (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((s (%input-stream stream)))
    (loop
      (let ((c (%stream-read-char s)))
        (cond ((eq c :eof) (return (if eof-error-p (error 'end-of-file :stream s) eof-value)))
              ((or (null peek-type)
                   (and (eq peek-type t) (not (%whitespace-char-p c)))
                   (and (characterp peek-type) (char= c peek-type)))
               (unread-char c s)
               (return c)))))))

(defun read-char-no-hang (&optional stream (eof-error-p t) eof-value recursive-p)
  (read-char stream eof-error-p eof-value recursive-p))

(defun listen (&optional stream)
  (let ((s (%input-stream stream)))
    (case (stream-kind s)
      (:string-in (or (stream-unread s) (< (stream-index s) (stream-end s))))
      (t (not (null (stream-unread s)))))))

(defun clear-input (&optional stream) (declare (ignore stream)) nil)

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

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (let ((c (%stream-read-char stream)))
    (if (eq c :eof)
        (if eof-error-p (error 'end-of-file :stream stream) eof-value)
        (char-code c))))

(defun read-sequence (seq stream &key (start 0) end)
  (let ((end (or end (length seq))) (i start))
    (loop
      (when (>= i end) (return i))
      (let ((c (%stream-read-char stream)))
        (when (eq c :eof) (return i))
        (setf (elt seq i) (if (subtypep (stream-element-type stream) 'character) c (char-code c)))
        (setq i (+ i 1))))))

(defun write-sequence (seq stream &key (start 0) end)
  (let ((end (or end (length seq))))
    (do ((i start (+ i 1))) ((>= i end) seq)
      (let ((x (elt seq i)))
        (if (characterp x) (write-char x stream) (write-byte x stream))))))

;;; ------------------------------------------------------------
;;; files
;;; ------------------------------------------------------------

(defun %host-name-buffer (string)
  (dotimes (i (length string)) (%put-char-buf (char-code (char string i)))))

(defun open (filename &key (direction :input) (element-type 'character) if-exists
                           (if-does-not-exist nil idnep) external-format)
  (declare (ignore if-exists external-format))
  (let ((name (%native-name filename)))
    (case direction
      (:input
       (%host-name-buffer name)
       (let ((n (%file-open 0)))
         (if (< n 0)
             (if (and idnep (null if-does-not-exist))
                 nil
                 (error 'file-error :pathname filename :format-control "can't open file ~A"
                                    :format-arguments (list name)))
             (let ((s (make-string n)))
               (dotimes (i n) (%set-schar s i (%host-char i)))
               (%make-stream :kind :string-in :buffer s :index 0 :end n :direction :input
                             :pathname (merge-pathnames filename) :element-type element-type)))))
      ((:output :io)
       (%make-stream :kind :file-out :pathname (merge-pathnames filename)
                     :buffer (make-array 256 :element-type 'character :fill-pointer 0 :adjustable t)
                     :element-type element-type))
      (:probe (probe-file filename))
      (t (error "OPEN: unsupported direction ~S" direction)))))

(defun %binary-element-type-p (et)
  (and et (not (member et '(character base-char standard-char)))
       (or (member et '(unsigned-byte signed-byte bit))
           (and (consp et) (member (car et) '(unsigned-byte signed-byte mod))))))

(defun close (stream &key abort)
  (when (and (eq (stream-kind stream) :file-out) (stream-open stream) (not abort))
    (%host-name-buffer (stream-buffer stream))
    (%put-char-buf -1)
    (%host-name-buffer (%native-name (stream-pathname stream)))
    (%file-open (if (%binary-element-type-p (stream-element-type stream)) 2 1)))
  (setf (stream-open stream) nil)
  t)

(defun file-length (s)
  (if (eq (stream-kind s) :string-in) (stream-end s) (length (stream-buffer s))))
(defun file-position (s &optional pos)
  (if pos
      (progn (setf (stream-index s) pos) t)
      (if (eq (stream-kind s) :string-in) (stream-index s) (length (stream-buffer s)))))
(defun file-string-length (s x) (declare (ignore s)) (if (characterp x) 1 (length x)))
(defun stream-external-format (s) (declare (ignore s)) :utf-8)

(defmacro with-open-stream ((var stream) &body body)
  `(let ((,var ,stream))
     (unwind-protect (progn ,@body) (close ,var))))

(defmacro with-open-file ((var filename &rest options) &body body)
  `(let ((,var (open ,filename ,@options)))
     (unwind-protect (progn ,@body) (when ,var (close ,var)))))

(defmacro with-output-to-string ((var &optional string &key element-type) &body body)
  (declare (ignore element-type))
  (if string
      `(let ((,var (%make-stream :kind :string-out :buffer ,string)))
         ,@body)
      `(let ((,var (make-string-output-stream)))
         ,@body
         (get-output-stream-string ,var))))

(defmacro with-input-from-string ((var string &key (start 0) end index) &body body)
  `(let ((,var (make-string-input-stream ,string ,start ,end)))
     (multiple-value-prog1 (progn ,@body)
       ,@(when index `((setf ,index (stream-index ,var)))))))
