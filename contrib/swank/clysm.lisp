;;;; contrib/swank/clysm.lisp — the SWANK backend for Clysm
;;;;
;;;; Added to the system SWANK of Quicklisp's SLIME by contrib/fixups.lisp.
;;;; Clysm has no threads: the server serves one connection at a time, in
;;;; the thread that called CREATE-SERVER (communication style NIL).

(in-package swank/backend)

;;; Forms evaluated by SWANK (the REPL, C-x C-e, C-c C-c) are compiled to
;;; WebAssembly like those typed at Clysm's own REPL; CL:EVAL is Clysm's
;;; interpreter.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '#:eval :swank))

(defvar *polling* nil "True while a request is processed by an interrupt check.")

(defun swank::eval (form)
  ;; (an evaluation in the debugger of an interrupt can be interrupted too)
  (let ((*polling* nil))
    (sys::%eval-toplevel form)))

;;;; MOP

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (s :swank-mop)
    (let ((real (or (find-symbol (string s) :clysm-mop)
                    (let ((cl (find-symbol (string s) :cl)))
                      (and cl (eq (symbol-package cl) (find-package :cl)) cl)))))
      (when (and real (not (eq real s)))
        (unintern s :swank-mop)
        (import real :swank-mop)
        (export real :swank-mop)))))

(unless (fboundp 'swank-mop:slot-definition-documentation)
  (defun swank-mop:slot-definition-documentation (slot)
    (documentation slot t)))

(unless (fboundp 'swank-mop:generic-function-declarations)
  (defun swank-mop:generic-function-declarations (gf)
    (declare (ignore gf))
    nil))

(defimplementation gray-package-name ()
  "GRAY")

;;;; TCP server

(defimplementation preferred-communication-style ()
  nil)

(defimplementation create-socket (host port &key backlog)
  (declare (ignore backlog))
  ;; "localhost" may be only ::1 for Node.js; Emacs connects to 127.0.0.1
  (sys::open-tcp-server (if (equal host "localhost") "127.0.0.1" host) port))

(defimplementation local-port (socket)
  (sys::tcp-server-port socket))

(defimplementation close-socket (socket)
  (sys::close-tcp-server socket))

(defimplementation accept-connection (socket &key external-format buffering timeout)
  (declare (ignore buffering timeout))
  (when external-format
    (error "Clysm's SWANK server has only binary connections (external format ~S)"
           external-format))
  (sys::accept-tcp-stream socket))

(defun queued-request-socket ()
  "The socket of the current connection when a request is in its queue."
  (let ((c (and (boundp 'swank::*emacs-connection*)
                (symbol-value 'swank::*emacs-connection*))))
    (and c
         (typep c 'swank::singlethreaded-connection)
         (some (lambda (e) (and (consp e) (member (car e) '(:emacs-rex :emacs-channel-send))))
               (funcall 'swank::sconn.event-queue c))
         (funcall 'swank::connection.socket-io c))))

(defimplementation wait-for-input (streams &optional timeout)
  ;; only the sockets are waited for: Clysm cannot wait for the terminal and
  ;; a socket at the same time
  (let ((sockets (remove-if-not #'sys::socket-stream-p streams)))
    ;; a request read from the socket while another was waited for (by the
    ;; debugger) and left in the queue of the connection: the connection's
    ;; socket is ready, else the request waits for the next one
    (unless timeout
      (let ((socket (queued-request-socket)))
        (when (member socket streams)
          (return-from wait-for-input (list socket)))))
    (loop
      (let ((ready (remove-if-not (lambda (s)
                                    (or (ignore-errors (listen s))
                                        ;; at end of file: reading ends the connection
                                        (and (sys::socket-stream-p s)
                                             (or (not (open-stream-p s))
                                                 (sys::socket-stream-in-eof s)))))
                                  streams)))
        (when ready (return ready)))
      (when timeout (return nil))
      (cond ((= (length sockets) 1)
             ;; at end of file the stream is "ready": reading it ends the
             ;; connection
             (sys::socket-wait-input (first sockets))
             (return sockets))
            (t (sleep 0.05))))))

;;;; Interrupts (C-c C-c in Emacs)
;;;
;;; Code compiled in the image after a connection is made checks for
;;; interrupts (sys::*interrupt-checks*): every so often POLL-FOR-INTERRUPT
;;; looks whether Emacs sent something during an evaluation and processes
;;; the requests, as with communication style :SIGIO, so :EMACS-INTERRUPT
;;; enters the debugger (SIMPLE-BREAK) where the computation was.

(defvar *last-data-seq* nil)

(defun current-connection ()
  (and (boundp 'swank::*emacs-connection*)
       (symbol-value 'swank::*emacs-connection*)))

(defun poll-for-interrupt ()
  (unless *polling*
    (let ((c (current-connection)))
      (when (and c
                 (boundp 'swank::*slime-interrupts-enabled*)
                 (symbol-value 'swank::*slime-interrupts-enabled*)
                 (typep c 'swank::singlethreaded-connection))
        (let ((socket (funcall 'swank::connection.socket-io c))
              (seq (sys::%tcp-data-seq)))
          ;; a call of the host only when data arrived since the last
          ;; check, or input is buffered
          (when (and (sys::socket-stream-p socket)
                     (open-stream-p socket)
                     (or (not (eql seq *last-data-seq*))
                         (< (sys::socket-stream-in-pos socket) (sys::socket-stream-in-fill socket))))
            (setq *last-data-seq* seq)
            (when (listen socket)
              (let ((*polling* t))
                (funcall 'swank::invoke-or-queue-interrupt
                         (lambda () (funcall 'swank::handle-requests c t)))))))))))

(defimplementation emacs-connected ()
  (setq sys::*interrupt-poll-function* 'poll-for-interrupt
        sys::*interrupt-checks* t))

;;;; Unix

(defimplementation getpid ()
  (or (ignore-errors (let ((process (js:global "process")))
                       (and process (js:get process "pid"))))
      0))

(defimplementation lisp-implementation-type-name ()
  "clysm")

(defimplementation quit-lisp ()
  (sys::%exit 0))

(defimplementation command-line-args ()
  (ignore-errors (uiop:raw-command-line-arguments)))

(defimplementation default-directory ()
  (namestring (sys::%getcwd)))

(defimplementation set-default-directory (directory)
  (let ((dir (uiop:ensure-directory-pathname directory)))
    (sys::%chdir dir)
    (setf *default-pathname-defaults* (pathname (sys::%getcwd)))
    (default-directory)))

;;;; Compilation

(defvar *buffer-name* nil)
(defvar *buffer-position* 0)

(defun compiler-condition-location ()
  (cond (*buffer-name*
         (make-location (list :buffer *buffer-name*)
                        (list :offset *buffer-position* 0)))
        (*compile-file-truename*
         (make-location (list :file (namestring *compile-file-truename*))
                        (list :position 1)))
        (t (list :error "No location"))))

(defun signal-compiler-condition (condition severity)
  (signal 'compiler-condition
          :original-condition condition
          :severity severity
          :message (princ-to-string condition)
          :location (compiler-condition-location)))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((style-warning (lambda (c) (signal-compiler-condition c :style-warning)))
                 (warning (lambda (c)
                            (unless (typep c 'style-warning)
                              (signal-compiler-condition c :warning))))
                 (reader-error (lambda (c) (signal-compiler-condition c :read-error)))
                 (error (lambda (c) (signal-compiler-condition c :error))))
    (funcall function)))

(defvar *buffer-definitions* (make-hash-table :test 'equal)
  "Definitions compiled from Emacs buffers (C-c C-c): name -> ((dspec buffer
file offset) ...).")

(defun note-buffer-definition (form buffer file offset)
  (let ((dspec (sys::%definition-spec form)))
    (when dspec
      (let ((name (second dspec)))
        (setf (gethash name *buffer-definitions*)
              (cons (list dspec buffer file offset)
                    (remove dspec (gethash name *buffer-definitions*)
                            :key #'car :test #'equal)))))))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore line column policy))
  (let ((*buffer-name* buffer)
        (*buffer-position* position)
        (failed nil))
    (handler-bind ((error (lambda (c) (declare (ignore c)) (setq failed t))))
      (with-compilation-hooks ()
        (with-input-from-string (in string)
          (loop
            (let ((form (read in nil in)))
              (when (eq form in) (return))
              (note-buffer-definition form buffer filename position)
              (sys::%eval-toplevel form))))))
    (not failed)))

(defimplementation swank-compile-file (input-file output-file load-p external-format
                                       &key policy)
  (declare (ignore external-format policy))
  (with-compilation-hooks ()
    (multiple-value-bind (output warnings-p failure-p)
        (compile-file input-file :output-file output-file)
      (values output warnings-p
              (or failure-p
                  (and load-p output (not (load output))))))))

(defimplementation find-external-format (coding-system)
  (when (member coding-system '("utf-8" "utf-8-unix" "iso-latin-1-unix" "iso-8859-1")
                :test #'equal)
    :default))

;;;; Documentation

(defimplementation arglist (name)
  (cond ((functionp name)
         (or (ignore-errors (arglist (function-name name))) :not-available))
        ((not (valid-function-name-p name)) :not-available)
        ((and (symbolp name) (special-operator-p name))
         (special-operator-arglist name))
        ((and (symbolp name) (macro-function name))
         (multiple-value-bind (indicator ll tail)
             (get-properties (symbol-plist name) '(sys::%macro-lambda-list))
           (declare (ignore indicator))
           (cond (tail ll)
                 ((get name 'sys::%arglist))
                 (t :not-available))))
        ((fboundp name)
         (let ((f (fdefinition name))
               (sym (sys::%function-name-symbol name)))
           (cond ((typep f 'generic-function)
                  (swank-mop:generic-function-lambda-list f))
                 ((get sym 'sys::%arglist) (get sym 'sys::%arglist))
                 ((get sym 'sys::%lambda-source) (third (get sym 'sys::%lambda-source)))
                 (t :not-available))))
        (t :not-available)))

(defun special-operator-arglist (name)
  (case name
    ((progn) '(&rest forms))
    ((if) '(test then &optional else))
    ((let let*) '((&rest bindings) &body body))
    ((flet labels macrolet) '((&rest definitions) &body body))
    ((symbol-macrolet) '((&rest macrobindings) &body body))
    ((block) '(name &rest forms))
    ((return-from) '(name &optional value))
    ((tagbody) '(&rest statements))
    ((go) '(tag))
    ((catch) '(tag &body body))
    ((throw) '(tag result))
    ((unwind-protect) '(protected &body cleanup))
    ((setq) '(&rest pairs))
    ((quote function) '(thing))
    ((the) '(type form))
    ((locally) '(&body body))
    ((eval-when) '((&rest situations) &body body))
    ((load-time-value) '(form &optional read-only-p))
    ((multiple-value-call) '(function &rest forms))
    ((multiple-value-prog1) '(first-form &rest forms))
    ((progv) '(symbols values &body body))
    (t :not-available)))

(defimplementation function-name (function)
  (nth-value 2 (function-lambda-expression function)))

(defimplementation type-specifier-p (symbol)
  (or (and (find-class symbol nil) t)
      (and (ignore-errors (typep nil symbol) t) t)))

(defun doc (name type)
  (ignore-errors (documentation name type)))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((add (key value) (setf result (list* key value result))))
      (when (boundp symbol)
        (add :variable (or (doc symbol 'variable) :not-documented)))
      (cond ((special-operator-p symbol)
             (add :special-operator (or (doc symbol 'function) :not-documented)))
            ((macro-function symbol)
             (add :macro (or (doc symbol 'function) :not-documented)))
            ((and (fboundp symbol) (typep (fdefinition symbol) 'generic-function))
             (add :generic-function (or (doc symbol 'function) :not-documented)))
            ((fboundp symbol)
             (add :function (or (doc symbol 'function) :not-documented))))
      (when (ignore-errors (fboundp `(setf ,symbol)))
        (add :setf (or (doc `(setf ,symbol) 'function) :not-documented)))
      (when (find-class symbol nil)
        (add :class (or (doc symbol 'type) :not-documented))))
    result))

(defimplementation describe-definition (name type)
  (case type
    ((:variable :function :macro :special-operator :generic-function :setf)
     (describe name))
    (:class (describe (find-class name)))
    (t (describe name))))

;;;; Definitions (M-.)

(defun octet-offset-to-char-position (file offset)
  "The position of the character at octet OFFSET of the UTF-8 FILE (1-based,
as Emacs counts)."
  (or (ignore-errors
       (with-open-file (in file :element-type '(unsigned-byte 8))
         (let ((chars 0))
           (dotimes (i offset (1+ chars))
             (let ((b (read-byte in nil nil)))
               (unless b (return (1+ chars)))
               ;; a byte that is not a continuation byte begins a character
               (unless (= (logand b #xC0) #x80) (incf chars)))))))
      (1+ offset)))

(defun snippet-at (file offset)
  (ignore-errors
   (with-open-file (in file :element-type '(unsigned-byte 8))
     (file-position in offset)
     (let* ((octets (make-array 200 :element-type '(unsigned-byte 8)))
            (n (read-sequence octets in))
            (end (or (position 10 octets :end n) n)))
       ;; a whole line, decoded (cut before a partial character)
       (loop while (and (> end 0) (< end n) (= (logand (aref octets end) #xC0) #x80))
             do (decf end))
       (utf8-to-string (subseq octets 0 end))))))

(defun form-start (file offset)
  "The octet offset of the form at or after OFFSET of FILE (whitespace and
line comments skipped)."
  (or (ignore-errors
       (with-open-file (in file :element-type '(unsigned-byte 8))
         (file-position in offset)
         (loop
           (let ((b (read-byte in nil nil)))
             (cond ((null b) (return offset))
                   ((member b '(32 9 10 13 12)) (incf offset))
                   ((= b 59)            ; ;
                    (incf offset)
                    (loop (let ((b (read-byte in nil nil)))
                            (unless b (return))
                            (incf offset)
                            (when (= b 10) (return)))))
                   (t (return offset)))))))
      offset))

(defun file-location (file offset)
  (setq offset (form-start file offset))
  (if (probe-file file)
      (make-location (list :file file)
                     (list :position (octet-offset-to-char-position file offset))
                     (let ((s (snippet-at file offset)))
                       (if s (list :snippet s) nil)))
      (list :error (format nil "File ~A does not exist" file))))

(defimplementation find-definitions (name)
  (let ((results '()))
    (loop for (dspec buffer file offset) in (gethash name *buffer-definitions*)
          do (push (list dspec
                         (make-location (if file
                                            (list :buffer-and-file buffer file)
                                            (list :buffer buffer))
                                        (list :offset offset 0)))
                   results))
    (loop for (dspec file offset) in (sys::%definition-sources name)
          unless (find dspec results :key #'car :test #'equal)
            do (push (list dspec (file-location file offset)) results))
    (nreverse results)))

(defimplementation find-source-location (object)
  (let ((name (typecase object
                (function (function-name object))
                (class (class-name object))
                (generic-function (swank-mop:generic-function-name object)))))
    (or (and name (second (first (find-definitions name))))
        (list :error (format nil "No source location for ~S" object)))))

;;;; Debugging

(defvar *backtrace* '()
  "The frames of the debugger: the names of the functions on the stack,
innermost first.")

(defparameter *debugger-frames*
  '("CALL-WITH-DEBUGGING-ENVIRONMENT" "DEBUG-IN-EMACS" "INVOKE-SLIME-DEBUGGER"
    "SWANK-DEBUGGER-HOOK" "INVOKE-DEBUGGER" "%INVOKE-DEBUGGER" "ERROR" "CERROR" "SIGNAL"
    "WARN" "BREAK" "%BREAK" "%SIGNAL*" "%ERROR*")
  "Functions of the debugger itself, left out at the top of backtraces.")

(defun frame-name-matches (name candidates)
  (let ((short (subseq name (1+ (or (position #\: name :from-end t) -1)))))
    (member short candidates :test #'string-equal)))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* ((names (sys::%backtrace-names 200))
         (last-debugger (position-if (lambda (n) (frame-name-matches n *debugger-frames*))
                                     names :from-end t)))
    (let ((*backtrace* (if last-debugger (nthcdr (1+ last-debugger) names) names)))
      (funcall debugger-loop-fn))))

(defimplementation compute-backtrace (start end)
  (subseq *backtrace* (min start (length *backtrace*))
          (and end (min end (length *backtrace*)))))

(defimplementation print-frame (frame stream)
  (write-string frame stream))

(defimplementation frame-locals (index)
  (declare (ignore index))
  '())

(defimplementation frame-var-value (frame var)
  (declare (ignore frame var))
  nil)

(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  '())

(defimplementation frame-source-location (index)
  ;; a frame is named by the name of its function's symbol, without the
  ;; package: the definition of a symbol of that name
  (let ((frame (nth index *backtrace*)))
    (or (and frame
             (loop for symbol in (find-all-symbols frame)
                   for location = (second (first (find-definitions symbol)))
                   when location return location))
        (list :error (format nil "No source location for frame ~D" index)))))

(defimplementation eval-in-frame (form frame-number)
  (declare (ignore frame-number))
  (swank::eval form))

(defimplementation frame-restartable-p (frame)
  (declare (ignore frame))
  nil)

(defimplementation condition-extras (condition)
  (declare (ignore condition))
  '())

;;;; Inspector, hash tables

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weakness :key args))

(defimplementation make-weak-value-hash-table (&rest args)
  (apply #'make-hash-table :weakness :value args))

(defimplementation hash-table-weakness (hashtable)
  (sys::hash-table-weakness hashtable))

;;;; Packages

(defimplementation package-local-nicknames (package)
  (sys::package-local-nicknames package))
