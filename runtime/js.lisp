;;;; js.lisp — JavaScript interoperability
;;;;
;;;; JavaScript values are externrefs; they cannot live in Lisp data
;;;; structures directly, so they are wrapped in $foreign structs (spec:
;;;; 外部オブジェクトを管理するための専用のラッパー構造体).

(defpackage "JS"
  (:use)
  (:export "GLOBAL" "GET" "SET" "CALL" "NEW" "FUNCALL" "EVAL" "TYPEOF" "LOG"
           "TO-LISP" "TO-JS" "ARRAY" "OBJECT" "JS-VALUE-P"
           "AWAIT" "ASYNC-FUNCTION" "CAN-AWAIT-P"))

(defun js:js-value-p (x) (%foreignp x))

(defun %js-string (s)
  (dotimes (i (length s)) (%put-char-buf (char-code (char s i))))
  (%js-take-string))

(defun js:to-js (x)
  (cond ((%foreignp x) x)
        ((null x) (%js-special 0))
        ((eq x t) (%js-bool 1))
        ((eq x :false) (%js-bool 0))
        ((eq x :null) (%js-special 0))
        ((eq x :undefined) (%js-special 1))
        ((realp x) (%js-number (%to-double x)))
        ((stringp x) (%js-string x))
        ((characterp x) (%js-string (string x)))
        ((keywordp x) (%js-string (string-downcase (symbol-name x))))
        ((functionp x) (%js-wrap-fn x))
        ((or (vectorp x) (consp x))
         (let ((a (%js-array)))
           (map nil (lambda (e) (%js-push a (js:to-js e))) x)
           a))
        (t (%js-wrap-lisp x))))

(defun js:to-lisp (o)
  (if (not (%foreignp o))
      o
      (case (%js-typeof o)
        ((0 1) nil)
        (2 (%js-truthy o))
        (3 (let ((d (%js-to-number o)))
             (if (and (not (%double-nan-p d)) (%d= d (%dtrunc d))
                      (%d< (%dabs d) 9.007199254740992d15))
                 (%float-truncate-to-integer d)
                 d)))
        (4 (let* ((n (%js-str-len o)) (s (make-string n)))
             (dotimes (i n s) (setf (char s i) (%js-str-char o i)))))
        (9 (%js-unwrap-lisp o))
        (t o))))

(defun %js-args (args)
  (let ((a (%js-array)))
    (dolist (x args a) (%js-push a (js:to-js x)))))

(defun js:global (&optional name)
  (if name (js:get (%js-global) name) (%js-global)))

(defun js:get (object key)
  (js:to-lisp (%js-get (js:to-js object) (js:to-js key))))

(defun js:set (object key value)
  (%js-set (js:to-js object) (js:to-js key) (js:to-js value))
  value)

(defun %js-result (r)
  "Signal a Lisp error for a JavaScript exception boxed by the host's apply."
  (when (eql (%js-typeof r) 5)
    (let ((msg (%js-get r (%js-string "__clysm_js_error"))))
      (when (eql (%js-typeof msg) 4)
        (error 'simple-error :format-control "JavaScript error: ~A"
                             :format-arguments (list (js:to-lisp msg))))))
  (js:to-lisp r))

(defun js:call (object method &rest args)
  (let ((o (js:to-js object)))
    (%js-result (%js-apply (%js-get o (js:to-js method)) o (%js-args args)))))

(defun js:funcall (function &rest args)
  (%js-result (%js-apply (js:to-js function) (%js-special 1) (%js-args args))))

(defun js:new (constructor &rest args)
  (%js-result (%js-new (js:to-js constructor) (%js-args args))))

(defun js:eval (code) (js:funcall (js:global "eval") code))

(defun js:typeof (x)
  (if (%foreignp x)
      (svref #(:undefined :null :boolean :number :string :object :function :bigint :symbol :lisp)
             (%js-typeof x))
      :lisp))

(defun js:array (&rest items) (js:to-js (coerce items 'vector)))

(defun js:object (&rest plist)
  (let ((o (js:new (js:global "Object"))))
    (do ((l plist (cddr l))) ((null l) o)
      (js:set o (car l) (cadr l)))))

(defun js:log (&rest args)
  (apply #'js:call (js:global "console") "log" args)
  nil)

;;; Waiting for promises (JavaScript Promise Integration).  A function made
;;; by JS:ASYNC-FUNCTION, when JavaScript calls it, runs on a stack of its
;;; own and returns a promise; inside it, JS:AWAIT suspends that stack until
;;; a promise is settled, while the page goes on (and may call other Lisp
;;; functions).  Only one computation waits at a time, and only one that
;;; JavaScript called from its event loop: a stack that is suspended must
;;; not have another Lisp computation below it, whose dynamic bindings
;;; would be undone in the wrong order.

(defvar *js-running* nil "True while Lisp code called from JavaScript runs.")
(defvar *js-top* nil "True in a callback that JavaScript called from its event loop.")
;; the program itself (its toplevel forms) runs until the first task of the
;; event loop
(defvar *js-starting* (list t))
(let ((set-timeout (js:global "setTimeout")))
  (if (%foreignp set-timeout)
      (js:funcall set-timeout (lambda () (setf (car *js-starting*) nil) nil) 0)
      (setf (car *js-starting*) nil)))

(defvar *js-can-await* nil "True in a function of JS:ASYNC-FUNCTION, unless JavaScript is in between.")
(defvar *js-awaiting* nil "True while a computation waits in JS:AWAIT.")
(defvar *js-suspending* :unknown "Whether the JavaScript engine can suspend WebAssembly.")

;;; called from JavaScript (exported as lisp_apply) when a Lisp function
;;; wrapped by TO-JS is invoked
(defun %js-callback (closure args)
  (let* ((f (%js-unwrap-lisp closure))
         (n (js:to-lisp (%js-get args (%js-string "length"))))
         (lisp-args (let ((r nil))
                      (dotimes (i n (nreverse r))
                        (push (js:to-lisp (%js-get args (%js-number (%to-double i)))) r))))
         ;; whether no other Lisp computation is below this call (then a
         ;; function of JS:ASYNC-FUNCTION may wait)
         (*js-top* (not (or *js-running* (car *js-starting*))))
         (*js-running* t)
         (*js-can-await* nil))
    (js:to-js (apply f lisp-args))))

(defun %js-suspending-p ()
  (when (eq *js-suspending* :unknown)
    (setq *js-suspending*
          (let ((wasm (js:global "WebAssembly")))
            (and (%foreignp (js:get wasm "Suspending"))
                 (%foreignp (js:get wasm "promising"))
                 t))))
  *js-suspending*)

(defun js:async-function (function)
  "A JavaScript function that calls FUNCTION and returns a promise;
FUNCTION may wait with JS:AWAIT.  (Where the JavaScript engine cannot
suspend WebAssembly, it is an ordinary function, and JS:CAN-AWAIT-P false.)"
  (%js-wrap-async-fn (lambda (&rest args)
                       (let ((*js-can-await* *js-top*))
                         (apply function args)))))

(defun js:can-await-p ()
  "True if JS:AWAIT can be called here."
  (and *js-can-await* (not *js-awaiting*) (%js-suspending-p)))

(defun js:await (promise)
  "Wait until the JavaScript PROMISE is settled; return its value, or
signal an error if it is rejected.  Meanwhile JavaScript runs, and may call
Lisp functions, which see the dynamic bindings of the waiting computation."
  (unless (js:can-await-p)
    (error "JS:AWAIT cannot wait here: not in a function of JS:ASYNC-FUNCTION called from the event loop, or already waiting"))
  (%js-result (let ((*js-awaiting* t))
                (%js-await (js:to-js promise)))))

(defun %foreign-type-name (x) (js:typeof x))

;;; Tracking instances, so that the instances of a redefined class can be
;;; updated (see %UPDATE-OBSOLETE-INSTANCES in class0.lisp).  The instances
;;; are held by JavaScript WeakRefs: tracking does not keep them alive.
(defvar *instance-tracker* nil)

(defun %instance-tracker ()
  (or *instance-tracker*
      (setq *instance-tracker*
            (js:eval "({
  refs: new WeakMap(),
  add(cls, obj) {
    let e = this.refs.get(cls);
    if (!e) this.refs.set(cls, e = { list: [], limit: 64 });
    e.list.push(new WeakRef(obj));
    if (e.list.length > e.limit) {
      e.list = e.list.filter((r) => r.deref() !== undefined);
      e.limit = Math.max(64, e.list.length * 2);
    }
  },
  live(cls) {
    const e = this.refs.get(cls);
    return e ? e.list.map((r) => r.deref()).filter((o) => o !== undefined) : [];
  },
})"))))

(defun track-instances (&optional (on t))
  "While ON, the instances made by ALLOCATE-INSTANCE are tracked: when a class
is redefined with different slots, its living instances get the new slots
and UPDATE-INSTANCE-FOR-REDEFINED-CLASS is called for them (at the
redefinition, not when they are next accessed).  Instances made while
tracking is off keep their old slot vectors.  It costs a call of JavaScript
for every instance, so it is meant for development."
  (if on
      (let* ((tracker (js:to-js (%instance-tracker)))
             (add (%js-get tracker (%js-string "add"))))
        (setq *track-instance-hook*
              (lambda (class instance)
                (let ((args (%js-array)))
                  (%js-push args (%js-wrap-lisp class))
                  (%js-push args (%js-wrap-lisp instance))
                  (%js-apply add tracker args)))
              *live-instances-hook*
              (lambda (class)
                (let* ((array (js:call tracker "live" class))
                       (result nil))
                  (dotimes (i (js:get array "length") result)
                    (push (js:get array i) result))))))
      (setq *track-instance-hook* nil))
  on)

;;; called by hosts (exported as eval_string): read and evaluate all forms in
;;; a JS string, return the printed values as a JS string
(defun %host-eval-string (src)
  (let ((text (js:to-lisp src)) (out (make-string-output-stream))
        (*js-running* t))
    (with-input-from-string (s text)
      (handler-case
          (loop
            (let ((form (read s nil %eof-marker)))
              (when (eq form %eof-marker) (return))
              (let ((vals (multiple-value-list
                           (if *native-eval-hook* (funcall *native-eval-hook* form) (eval form)))))
                (setq /// // // / / vals *** ** ** * * (car vals))
                (if vals
                    (dolist (v vals) (prin1 v out) (terpri out))
                    (format out "; No value~%")))))
        (error (c) (format out "; Error: ~A~%" c))))
    (js:to-js (get-output-stream-string out))))

(defun %init-default-pathname ()
  "Set *DEFAULT-PATHNAME-DEFAULTS* to the host's working directory (Node.js)."
  (let ((cwd (ignore-errors
              (let ((process (js:global "process")))
                (and (js:js-value-p process) (js:call process "cwd"))))))
    (when (stringp cwd)
      (setq *default-pathname-defaults* (pathname (concatenate 'string cwd "/"))))))

;;; ------------------------------------------------------------
;;; Host operating system interface (Node.js); used by UIOP/ASDF
;;; ------------------------------------------------------------

(defun %node-process ()
  (let ((p (ignore-errors (js:global "process"))))
    (and (js:js-value-p p) p)))

(defun %init-host-features ()
  "Add the host operating system to *FEATURES* (CLHS 24.1.2.1.1)."
  (let* ((process (%node-process))
         (platform (and process (ignore-errors (js:get process "platform")))))
    (when (stringp platform)
      (dolist (f (cond ((string= platform "linux") '(:unix :linux))
                       ((string= platform "darwin") '(:unix :darwin :bsd))
                       ((member platform '("freebsd" "openbsd" "netbsd") :test #'string=)
                        (list :unix :bsd (intern (string-upcase platform) :keyword)))
                       ((string= platform "win32") '(:windows :win32))
                       (t '(:unix))))
        (pushnew f *features*)))))

(defun %getenv (name)
  "The value of the host environment variable NAME, or NIL."
  (let ((process (%node-process)))
    (and process
         (let ((v (ignore-errors (js:get (js:get process "env") name))))
           (and (stringp v) v)))))

(defun %setenv (name value)
  "Set the host environment variable NAME to VALUE (a string), or unset it
when VALUE is NIL."
  (let ((process (%node-process)))
    (when process
      (if value
          (js:set (js:get process "env") name value)
          (js:funcall (js:eval "(o, k) => { delete o[k]; }") (js:get process "env") name))))
  value)

(defvar *%cwd* nil
  "The working directory set by %CHDIR where the host cannot change its own
(a worker thread of Node.js), or NIL.")

(defun %getcwd ()
  "The host's working directory, as a string ending in a slash."
  (let* ((process (%node-process))
         (cwd (or *%cwd* (and process (ignore-errors (js:call process "cwd"))))))
    (if (stringp cwd)
        (if (and (> (length cwd) 0) (char= (char cwd (1- (length cwd))) #\/))
            cwd
            (concatenate 'string cwd "/"))
        (namestring *default-pathname-defaults*))))

(defun %chdir (directory)
  (let ((process (%node-process))
        (name (namestring directory)))
    (when process
      ;; process.chdir() throws in a worker: the directory is remembered
      (handler-case (progn (js:call process "chdir" name) (setq *%cwd* nil))
        (error () (setq *%cwd* name))))
    t))

(defun %delete-directory (directory)
  "Delete the empty directory DIRECTORY."
  (let ((fs (%host-fs)))
    (unless fs (error 'file-error :pathname directory))
    (handler-case (js:call fs "rmdirSync" (%native-name (%fs-pathname directory)))
      (error () (error 'file-error :pathname directory)))
    t))

;;; ------------------------------------------------------------
;;; TCP connections (Node.js host; synchronous, see host/run.mjs)
;;; ------------------------------------------------------------

(defun %host-net ()
  (let ((n (ignore-errors (js:global "__clysm_net"))))
    (if (js:js-value-p n) n (error "TCP connections are not available on this host"))))

(defun %tcp-connect (host port)
  "Open a TCP connection to HOST (a string) and PORT; returns a connection id."
  (handler-case (js:call (%host-net) "connect" (string host) port)
    (error (e) (error 'simple-error :format-control "cannot connect to ~A:~D: ~A"
                                    :format-arguments (list host port e)))))

(defun %tcp-write (id octets &key (start 0) end)
  "Write the octets of the vector OCTETS (from START to END) to connection ID."
  (let* ((end (or end (length octets)))
         (s (make-string (- end start))))
    (do ((i start (+ i 1))) ((>= i end))
      (setf (char s (- i start)) (code-char (aref octets i))))
    (js:call (%host-net) "write" id s)
    octets))

(defun %tcp-read (id buffer &key (start 0) end)
  "Read available octets from connection ID into BUFFER (blocking until at
least one arrives); returns the index after the last octet stored, which is
START at end of file."
  (let* ((end (or end (length buffer)))
         (s (js:call (%host-net) "read" id (- end start)))
         (n (length s)))
    (dotimes (i n)
      (setf (aref buffer (+ start i)) (char-code (char s i))))
    (+ start n)))

(defun %tcp-available-p (id)
  "True when a read of connection ID would not wait."
  (js:call (%host-net) "available" id))

(defun %local-connect (path)
  "Open a connection to the Unix domain socket PATH; returns a connection id."
  (handler-case (js:call (%host-net) "connect" (namestring path) nil)
    (error (e) (error 'simple-error :format-control "cannot connect to ~A: ~A"
                                    :format-arguments (list path e)))))

(defun %tcp-close (id)
  (js:call (%host-net) "close" id)
  t)

;;; ------------------------------------------------------------
;;; Backtraces as Lisp data: the Lisp frames appear in the JavaScript
;;; stack with the names of the wasm name section (as in the host's
;;; backtrace import, host/common.mjs).
;;; ------------------------------------------------------------

(defun %backtrace-names (&optional (count 60))
  "The names of the functions on the stack, innermost first."
  (let* ((error-class (ignore-errors (js:global "Error")))
         (stack (and (js:js-value-p error-class)
                     (let ((old (js:get error-class "stackTraceLimit")))
                       (js:set error-class "stackTraceLimit" (+ count 10))
                       (unwind-protect (js:get (js:new error-class) "stack")
                         (js:set error-class "stackTraceLimit" old)))))
         (names nil))
    (when (stringp stack)
      (with-input-from-string (in stack)
        (loop for line = (read-line in nil nil)
              while line
              do (let* ((at (search "at " line))
                        (wasm (search " (wasm:" line)))
                   (when (and at wasm (< at wasm))
                     (let* ((name (subseq line (+ at 3) wasm))
                            (hash (position #\# name :from-end t)))
                       (when hash (setq name (subseq name 0 hash)))
                       (cond ((string= name "toplevel") (push "<toplevel>" names) (loop-finish))
                             ((or (zerop (length name)) (char= (char name 0) #\:) (char= (char name 0) #\()
                                  (member name '("NEW" "%BACKTRACE-NAMES" "%PRINT-BACKTRACE" "PRINT-BACKTRACE"
                                                 "RAW-PRINT-BACKTRACE" "ERROR" "%ERROR*" "SIGNAL" "%SIGNAL*"
                                                 "%TYPE-ERROR" "%COERCE-TO-CONDITION" "INVOKE-DEBUGGER")
                                          :test #'string=)))
                             (t (push name names)))))))))
    (let ((names (nreverse names)))
      (if (> (length names) count) (subseq names 0 count) names))))

(defun %print-backtrace (stream &optional count)
  "Print the functions on the stack to STREAM (for UIOP:PRINT-BACKTRACE)."
  (let ((i 0))
    (dolist (name (%backtrace-names (or count 60)))
      (format stream "~&~D: ~A~%" i name)
      (incf i))))
