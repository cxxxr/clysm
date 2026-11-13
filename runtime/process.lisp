;;;; process.lisp — subprocesses (Node.js hosts)
;;;;
;;;; %RUN-PROGRAM-SYNC runs a program and waits for it (node:child_process
;;;; spawnSync).  %LAUNCH-PROGRAM starts one and gives Lisp streams for its
;;;; standard input and output: the processes are kept by the manager of
;;;; host/process.mjs.  In the Node.js host (a worker) a call of the manager
;;;; blocks until the main thread answers (__clysm_proc_sync); in a library
;;;; (host/lib.mjs) the manager is in the same thread (__clysm_proc) and a
;;;; read that has to wait for the process waits with JS:AWAIT, so it is
;;;; done in a function of JS:ASYNC-FUNCTION.  UIOP:RUN-PROGRAM and
;;;; UIOP:LAUNCH-PROGRAM are built on these (third_party/asdf).

(defvar *child-process-module* nil)

(defun %child-process-module ()
  (or *child-process-module*
      (setq *child-process-module*
            (let ((process (%node-process)))
              (or (and process
                       (ignore-errors (js:call process "getBuiltinModule" "node:child_process")))
                  (error "Subprocesses are not available on this host"))))))

(defun %program-argument (x)
  (if (pathnamep x) (%native-name x) (princ-to-string x)))

(defun %js-command (command)
  "COMMAND as the host takes it: a string (for the shell) or an array of the
program and its arguments."
  (if (stringp command)
      command
      (coerce (mapcar #'%program-argument command) 'vector)))

(defun %js-environment (environment)
  "ENVIRONMENT (an alist of names and values, or a list of \"NAME=value\"
strings) as a JavaScript object, or NIL."
  (when environment
    (let ((o (js:object)))
      (dolist (e environment o)
        (if (consp e)
            (js:set o (string (car e)) (princ-to-string (cdr e)))
            (let ((p (position #\= e)))
              (js:set o (subseq e 0 p) (if p (subseq e (+ p 1)) ""))))))))

(defun %stdio-name (mode)
  (ecase mode
    ((:pipe :stream) "pipe")
    ((:inherit :interactive) "inherit")
    ((nil :ignore) "ignore")))

(defun %run-program-sync (command &key input directory environment (output :pipe) (error-output :pipe))
  "Run COMMAND (a string for the shell, or a list of the program and its
arguments) and wait until it exits.  INPUT is a string given to its standard
input, :INTERACTIVE (this process's) or NIL (none).  OUTPUT and
ERROR-OUTPUT are :PIPE (returned as strings), :INTERACTIVE or NIL.
Returns the output, the error output and the exit code (128 plus the signal
number if a signal ended it)."
  (let* ((cp (%child-process-module))
         (options (js:object "maxBuffer" 1073741824
                             "encoding" "utf8"
                             "stdio" (vector (cond ((stringp input) "pipe")
                                                   ((eq input :interactive) "inherit")
                                                   (t "ignore"))
                                             (%stdio-name output)
                                             (%stdio-name error-output))))
         (env (%js-environment environment)))
    (when (stringp input) (js:set options "input" input))
    (when directory (js:set options "cwd" (%native-name (pathname directory))))
    (when env
      (js:set options "env" (js:funcall (js:eval "(e) => ({ ...process.env, ...e })") env)))
    (let ((r (if (stringp command)
                 (progn (js:set options "shell" t)
                        (js:call cp "spawnSync" command options))
                 (let ((c (%js-command command)))
                   (js:call cp "spawnSync" (aref c 0) (subseq c 1) options)))))
      (let ((err (js:get r "error")))
        (when err
          (error "Cannot run ~S: ~A" command (js:get err "message"))))
      (let ((status (js:get r "status")) (signal (js:get r "signal")))
        (values (or (js:get r "stdout") (and (eq output :pipe) ""))
                (or (js:get r "stderr") (and (eq error-output :pipe) ""))
                (or status (+ 128 (%signal-number signal))))))))

(defun %signal-number (name)
  (let ((pos (position name '("SIGHUP" "SIGINT" "SIGQUIT" "SIGILL" "SIGTRAP" "SIGABRT" "SIGBUS"
                              "SIGFPE" "SIGKILL" "SIGUSR1" "SIGSEGV" "SIGUSR2" "SIGPIPE"
                              "SIGALRM" "SIGTERM")
                       :test #'equal)))
    (if pos (+ pos 1) 0)))

;;; The process manager

(defun %process-manager-call (op &rest args)
  (let ((sync (%js-get (%js-global) (%js-string "__clysm_proc_sync"))))
    (if (eql (%js-typeof sync) 5)
        (apply #'js:call sync op args)
        (let ((manager (%js-get (%js-global) (%js-string "__clysm_proc"))))
          (unless (eql (%js-typeof manager) 5)
            (error "Subprocesses with streams are not available on this host"))
          (let ((r (apply #'js:call manager op args)))
            (if (and (js:js-value-p r) (eq (js:typeof (js:get r "then")) :function))
                (if (js:can-await-p)
                    (js:await r)
                    (error "The process has not written what is read yet, and this computation cannot wait for it (it is not in a function of JS:ASYNC-FUNCTION)"))
                r))))))

(defstruct (%process (:constructor %make-process (id command)))
  id command exit-code signal)

(defclass %process-input (fundamental-character-input-stream)
  ((process :initarg :process)
   (fd :initarg :fd)
   (buffer :initform "")
   (index :initform 0)
   (eof :initform nil))
  (:documentation "The characters that a process writes to its output or error output."))

(defclass %process-binary-input (fundamental-binary-input-stream)
  ((process :initarg :process)
   (fd :initarg :fd)
   (buffer :initform (%make-bytes 0))
   (index :initform 0)
   (eof :initform nil)))

(defclass %process-output (fundamental-character-output-stream)
  ((process :initarg :process)
   (buffer :initform (make-string-output-stream))
   (column :initform 0))
  (:documentation "The standard input of a process."))

(defun %process-fill (s)
  "Read the next chunk of the process output into the buffer of S; false at its end."
  (with-slots (process fd buffer index eof) s
    (if eof
        nil
        (let ((chunk (if (typep s '%process-input)
                         (%process-manager-call "readText" (%process-id process) fd 65536)
                         (let ((o (%process-manager-call "read" (%process-id process) fd 65536)))
                           (and o (js:to-octets o))))))
          (if (null chunk)
              (progn (setf eof t) nil)
              (progn (setf buffer chunk index 0) t))))))

(defmethod stream-read-char ((s %process-input))
  (with-slots (buffer index) s
    (loop
      (when (< index (length buffer))
        (return (prog1 (char buffer index) (incf index))))
      (unless (%process-fill s) (return :eof)))))

(defmethod stream-unread-char ((s %process-input) character)
  (declare (ignore character))
  (with-slots (index) s (when (> index 0) (decf index)))
  nil)

(defmethod stream-read-line ((s %process-input))
  (with-slots (buffer index) s
    (let ((out nil))
      (loop
        (when (>= index (length buffer))
          (unless (%process-fill s)
            (return (values (if out (apply #'concatenate 'string (nreverse out)) "")
                            t))))
        (let ((nl (position #\Newline buffer :start index)))
          (if nl
              (let ((part (subseq buffer index nl)))
                (setf index (+ nl 1))
                (return (values (if out (apply #'concatenate 'string (nreverse (cons part out))) part)
                                nil)))
              (progn (push (subseq buffer index) out)
                     (setf index (length buffer)))))))))

(defmethod stream-listen ((s %process-input))
  (with-slots (process fd buffer index eof) s
    (or (< index (length buffer))
        (and (not eof) (%process-manager-call "available" (%process-id process) fd)))))

(defmethod stream-read-byte ((s %process-binary-input))
  (with-slots (buffer index) s
    (loop
      (when (< index (length buffer))
        (return (prog1 (aref buffer index) (incf index))))
      (unless (%process-fill s) (return :eof)))))

(defmethod stream-element-type ((s %process-binary-input)) '(unsigned-byte 8))

(defun %process-send (s)
  (with-slots (process buffer) s
    (let ((text (get-output-stream-string buffer)))
      (when (> (length text) 0)
        (%process-manager-call "write" (%process-id process) text)))))

(defmethod stream-write-char ((s %process-output) character)
  (with-slots (buffer column) s
    (write-char character buffer)
    (setf column (if (char= character #\Newline) 0 (+ column 1))))
  character)

(defmethod stream-write-string ((s %process-output) string &optional (start 0) end)
  (let ((end (or end (length string))))
    (with-slots (buffer column) s
      (write-string string buffer :start start :end end)
      (let ((nl (position #\Newline string :start start :end end :from-end t)))
        (setf column (if nl (- end nl 1) (+ column (- end start))))))
    string))

(defmethod stream-line-column ((s %process-output)) (slot-value s 'column))
(defmethod stream-finish-output ((s %process-output)) (%process-send s) nil)
(defmethod stream-force-output ((s %process-output)) (%process-send s) nil)

(defmethod close ((s %process-output) &key abort)
  (declare (ignore abort))
  (when (open-stream-p s)
    (%process-send s)
    (%process-manager-call "closeInput" (%process-id (slot-value s 'process))))
  (call-next-method))

(defun %stdio-spec (x direction)
  "The stdio entry for the host: \"pipe\", \"inherit\", \"ignore\", or the
file descriptor of the file X (a pathname or a string) opened for DIRECTION."
  (cond ((member x '(:stream)) "pipe")
        ((eq x :interactive) "inherit")
        ((null x) "ignore")
        ((or (stringp x) (pathnamep x))
         (let ((fs (%host-fs)))
           (js:call fs "openSync" (%native-name (pathname x))
                    (ecase direction (:input "r") (:output "w") (:append "a")))))
        (t (error "~S is not a stream specifier for a process here" x))))

(defun %launch-program (command &key input output error-output directory environment
                                     (element-type 'character) (if-output-exists :supersede)
                                     (if-error-output-exists :supersede))
  "Start COMMAND (a string for the shell, or a list of the program and its
arguments) without waiting.  INPUT, OUTPUT and ERROR-OUTPUT are :STREAM (a
Lisp stream, see %PROCESS-STREAMS), :INTERACTIVE, NIL or a file; ERROR-OUTPUT
may be :OUTPUT.  Returns the process and its input, output and error output
streams (or NIL)."
  (let* ((options (js:object))
         (out-spec (%stdio-spec output (if (eq if-output-exists :append) :append :output)))
         (err-spec (if (eq error-output :output)
                       "stdout"
                       (%stdio-spec error-output (if (eq if-error-output-exists :append) :append :output)))))
    (js:set options "stdin" (%stdio-spec input :input))
    (js:set options "stdout" out-spec)
    (js:set options "stderr" err-spec)
    (when directory (js:set options "cwd" (%native-name (pathname directory))))
    (let ((env (%js-environment environment))) (when env (js:set options "env" env)))
    (let* ((id (%process-manager-call "spawn" (%js-command command) options))
           (process (%make-process id command))
           (binary (not (subtypep element-type 'character)))
           (input-class (if binary '%process-binary-input '%process-input)))
      (values process
              (and (eq input :stream) (make-instance '%process-output :process process))
              (and (eq output :stream) (make-instance input-class :process process :fd 1))
              (and (eq error-output :stream) (make-instance input-class :process process :fd 2))))))

(defun %process-wait (process)
  "Wait until PROCESS exits; return its exit code (128 plus the number of the
signal that ended it) and the signal number or NIL."
  (unless (%process-exit-code process)
    (let ((r (%process-manager-call "wait" (%process-id process))))
      (let ((code (js:get r 0)) (signal (js:get r 1)))
        (setf (%process-exit-code process) (if signal (+ 128 (%signal-number signal)) code)
              (%process-signal process) (and signal (%signal-number signal))))))
  (values (%process-exit-code process) (%process-signal process)))

(defun %process-status (process)
  "The exit code of PROCESS, or NIL while it runs."
  (or (%process-exit-code process)
      (let ((r (%process-manager-call "status" (%process-id process))))
        (and r (values (%process-wait process))))))

(defun %process-pid (process) (%process-manager-call "pid" (%process-id process)))

(defun %process-kill (process &optional urgent)
  (%process-manager-call "kill" (%process-id process) (if urgent "SIGKILL" "SIGTERM"))
  t)
