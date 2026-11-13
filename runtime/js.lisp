;;;; js.lisp — JavaScript interoperability
;;;;
;;;; JavaScript values are externrefs; they cannot live in Lisp data
;;;; structures directly, so they are wrapped in $foreign structs (spec:
;;;; 外部オブジェクトを管理するための専用のラッパー構造体).

(defpackage "JS"
  (:use)
  (:export "GLOBAL" "GET" "SET" "CALL" "NEW" "FUNCALL" "EVAL" "TYPEOF" "LOG"
           "TO-LISP" "TO-JS" "ARRAY" "OBJECT" "JS-VALUE-P"
           "AWAIT" "ASYNC-FUNCTION" "CAN-AWAIT-P" "ASYNC" "AWAIT-ALL"
           "JS-ERROR" "JS-ERROR-VALUE" "JS-ERROR-NAME" "JS-ERROR-MESSAGE" "EXPORT"
           "MODULE" "REQUIRE" "IMPORT" "TO-OCTETS" "FROM-OCTETS"))

(defun js:js-value-p (x) (%foreignp x))

(defun %js-string (s)
  "The JavaScript string of the Lisp string S (all characters at once,
through the memory)."
  (let ((s (if (simple-string-p s) s (coerce s 'simple-string))))
    (%js-str-from-mem (%mem-put-string s 0 (length s)))))

(defun %js-lisp-string (o)
  "The Lisp string of the JavaScript string O."
  (%mem-get-string (%js-str-to-mem o)))

(defun js:to-octets (x)
  "A new (unsigned-byte 8) vector of the octets of the JavaScript Uint8Array
(or Buffer, ArrayBuffer, typed array) X."
  (let ((n (%js-octets-to-mem (js:to-js x))))
    (when (< n 0)
      (error 'type-error :datum x :expected-type '(or js-uint8array js-arraybuffer)))
    (%mem-get-octets n)))

(defun js:from-octets (octets &key (start 0) end)
  "A new JavaScript Uint8Array of the octets of the (unsigned-byte 8)
vector OCTETS from START to END."
  (let ((end (or end (length octets))))
    (if (%bytes-p octets)
        (%js-octets-from-mem (%mem-put-octets octets start end))
        (let ((v (%make-bytes (- end start))))
          (dotimes (i (- end start)) (%set-bytes-ref v i (aref octets (+ start i))))
          (%js-octets-from-mem (%mem-put-octets v 0 (- end start)))))))

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
        (4 (%js-lisp-string o))
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

(define-condition js:js-error (error)
  ((value :initarg :value :reader js:js-error-value)
   (name :initarg :name :initform nil :reader js:js-error-name)
   (message :initarg :message :initform "" :reader js:js-error-message))
  (:report (lambda (condition stream)
             (format stream "JavaScript error: ~@[~A: ~]~A"
                     (js:js-error-name condition) (js:js-error-message condition))))
  (:documentation "A JavaScript exception thrown by a function called through
the JS package, or the rejection of a promise awaited by JS:AWAIT.  VALUE is
what was thrown (usually an Error), NAME and MESSAGE its name and message."))

(defun %js-result (r)
  "Signal a Lisp error for a JavaScript exception boxed by the host's apply:
the original condition if it is the Error of a Lisp error (a Lisp function
called from JavaScript), otherwise a JS:JS-ERROR."
  (when (eql (%js-typeof r) 5)
    (let ((msg (%js-get r (%js-string "__clysm_js_error"))))
      (when (eql (%js-typeof msg) 4)
        (let* ((value (%js-get r (%js-string "value")))
               (condition (and (member (%js-typeof value) '(5 6))
                               (%js-get value (%js-string "lispCondition")))))
          (if (and condition (eql (%js-typeof condition) 9))
              (error (%js-unwrap-lisp condition))
              (error 'js:js-error
                     :value (js:to-lisp value)
                     :name (and (member (%js-typeof value) '(5 6))
                                (let ((name (%js-get value (%js-string "name"))))
                                  (and (eql (%js-typeof name) 4) (js:to-lisp name))))
                     :message (js:to-lisp msg)))))))
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

;;; Calls from JavaScript, and waiting for promises (JavaScript Promise
;;; Integration).
;;;
;;; A Lisp function that JavaScript calls (made by TO-JS) runs in the
;;; dynamic environment of whatever Lisp code is below it.  If it signals an
;;; error that no handler of its own takes, the call throws a JavaScript
;;; Error (see %JS-FAILURE), unless a debugger hook is set; a Lisp caller of
;;; JS:CALL further out gets the original condition back.
;;;
;;; A function made by JS:ASYNC-FUNCTION returns a promise and is a
;;; computation of its own, like a thread: it has a binding stack of its
;;; own, and while it waits in JS:AWAIT its dynamic bindings are taken out of
;;; the symbols (they get the values from outside it) and put back when it
;;; goes on.  So any number of computations can wait at the same time, and
;;; code that runs meanwhile sees the global values.  A computation sees the
;;; bindings of the code that called it until it first waits.

(defvar *js-running* nil "True while Lisp code called from JavaScript runs.")
(defvar *js-can-await* nil "True in a function of JS:ASYNC-FUNCTION, unless JavaScript is in between.")
(defvar *js-computation* nil
  "The computation (of JS:ASYNC-FUNCTION) that runs: a vector of its binding
stack, the binding stack and depth outside it, and its own depth while it waits.")
(defvar *js-suspending* :unknown "Whether the JavaScript engine can suspend WebAssembly.")

(defvar *js-make-failure*
  (js:eval "(message, type, condition) => {
  const old = Error.stackTraceLimit; Error.stackTraceLimit = 100;
  const error = new Error(message);
  Error.stackTraceLimit = old;
  // the frames from the function that signalled the condition: without the
  // ones of this function, SIGNAL or ERROR and the handler; the Lisp frames
  // are named after their functions (without the numbers that tell apart
  // functions of the same name)
  const lines = String(error.stack).split('\\n');
  let first = 1;
  for (let i = 1; i < lines.length; i++) {
    if (/^\\s*at (ERROR|%ERROR\\*|CERROR|SIGNAL|%SIGNAL\\*|%SIGNAL-CONDITION|%TYPE-ERROR|%CHECK-TYPE-ERROR|INVOKE-DEBUGGER)[#( ]/.test(lines[i])) first = i + 1;
  }
  if (first === 1) {
    while (first < lines.length && !/\\(wasm:/.test(lines[first])) first++;
  }
  error.stack = ['LispError: ' + message]
    .concat(lines.slice(first).map((l) => l.replace(/at ([^ ]+?)#\\d+((?:\\/[a-z]+)?) \\(/, 'at $1$2 (')))
    .join('\\n');
  error.name = 'LispError';
  error.lispType = type;
  error.lispCondition = condition;
  return { __clysm_failure__: error };
}")
  "Makes the JavaScript Error for a Lisp condition (its stack has the Lisp frames
of the place where it was signalled), in the object that tells the host to throw it.")

(defun %js-failure (condition)
  (js:funcall *js-make-failure*
              (handler-case (princ-to-string condition)
                (error () "<error while printing the condition>"))
              (handler-case (prin1-to-string (type-of condition))
                (error () "ERROR"))
              (%js-wrap-lisp condition)))

(defun %js-run (closure args async)
  (let* ((f (%js-unwrap-lisp closure))
         (n (js:to-lisp (%js-get args (%js-string "length"))))
         (lisp-args (let ((r nil))
                      (dotimes (i n (nreverse r))
                        (push (js:to-lisp (%js-get args (%js-number (%to-double i)))) r))))
         (*js-running* t)
         (*js-can-await* async))
    (block run
      (handler-bind ((error (lambda (c)
                              (unless (or *debugger-hook* *invoke-debugger-hook*)
                                (return-from run (%js-failure c))))))
        (js:to-js (apply f lisp-args))))))

;;; called from JavaScript (exported as lisp_apply) when a Lisp function
;;; wrapped by TO-JS is invoked
(defun %js-callback (closure args)
  (%js-run closure args nil))

;;; called from JavaScript (exported as lisp_apply_async, through
;;; WebAssembly.promising) for a function of JS:ASYNC-FUNCTION: the
;;; computation gets a binding stack of its own
(defun %js-async-callback (closure args)
  (let ((computation (vector nil (%bstack) (%bsp) 0)))
    (%set-bstack (make-array 64))
    (%set-bsp 0)
    (unwind-protect
         (let ((*js-computation* computation))
           (%js-run closure args t))
      (%unbind-to 0)
      (%set-bstack (svref computation 1))
      (%set-bsp (svref computation 2)))))

(defun %js-recover (bsp)
  "Called by the host after an exception that Lisp code cannot catch (the
stack exhausted, a trap) went out of a call from JavaScript, which skipped
the code that undoes dynamic bindings.  BSP is the depth of the binding stack
before a synchronous call, or -1 after a computation of JS:ASYNC-FUNCTION
died: its bindings are undone and the binding stack outside it installed."
  (if (>= bsp 0)
      (%unbind-to bsp)
      (let ((computation *js-computation*))
        (%unbind-to 0)
        (when computation
          (%set-bstack (svref computation 1))
          (%set-bsp (svref computation 2)))))
  nil)

(defun %suspend-computation (computation)
  (let ((stack (%bstack)) (top (%bsp)))
    (setf (svref computation 0) stack (svref computation 3) top)
    (%swap-bindings stack top nil)
    (%set-bstack (svref computation 1))
    (%set-bsp (svref computation 2))))

(defun %resume-computation (computation)
  (setf (svref computation 1) (%bstack) (svref computation 2) (%bsp))
  (%swap-bindings (svref computation 0) (svref computation 3) t)
  (%set-bstack (svref computation 0))
  (%set-bsp (svref computation 3)))

(defun %js-suspending-p ()
  (when (eq *js-suspending* :unknown)
    (setq *js-suspending*
          (let ((wasm (js:global "WebAssembly")))
            (and (%foreignp (js:get wasm "Suspending"))
                 (%foreignp (js:get wasm "promising"))
                 t))))
  *js-suspending*)

(defun js:async-function (function)
  "A JavaScript function that calls FUNCTION and returns a promise of its
value (rejected with a JavaScript Error if FUNCTION signals an error it does
not handle).  FUNCTION runs as a computation of its own and may wait with
JS:AWAIT.  (Where the JavaScript engine cannot suspend WebAssembly, it
cannot wait: JS:CAN-AWAIT-P is false.)"
  (%js-wrap-async-fn function))

(defun js:can-await-p ()
  "True if JS:AWAIT can be called here."
  (and *js-can-await* *js-computation* (%js-suspending-p) t))

(defvar *js-async-value* (list '%async-value)
  "The marker of the value of a computation of JS:ASYNC: (marker . value).")

(defun %js-async-value (x)
  (if (and (consp x) (eq (car x) *js-async-value*)) (cdr x) x))

(defun js:await (promise)
  "Wait until the JavaScript PROMISE is settled; return its value, or
signal an error if it is rejected.  Meanwhile JavaScript runs, and may call
Lisp functions or start or resume other computations."
  (unless (js:can-await-p)
    (error "JS:AWAIT cannot wait here: not in a function of JS:ASYNC-FUNCTION (or JavaScript is in between)"))
  (let ((computation *js-computation*)
        (promise (js:to-js promise)))
    (%suspend-computation computation)
    (let ((result (%js-await promise)))
      (%resume-computation computation)
      (%js-async-value (%js-result result)))))

(defun js:async (function &rest args)
  "Start FUNCTION (applied to ARGS) as a computation of its own that may
wait with JS:AWAIT, and return the promise of its value at once.  JS:AWAIT
and JS:AWAIT-ALL of the promise return the Lisp value itself (not converted
to JavaScript)."
  (js:funcall (js:async-function
               (lambda () (%js-wrap-lisp (cons *js-async-value* (apply function args)))))))

(defun js:await-all (promises)
  "Wait for every promise of the list PROMISES; return the list of their values.
Signals an error if one is rejected."
  (let ((all (js:call (js:global "Promise") "all" (coerce promises 'vector))))
    (let ((array (js:await all)))
      (loop for i below (js:get array "length")
            collect (%js-async-value (js:get array i))))))

;;; Files through the host's file system (node:fs), at once (runtime/stream.lisp)

(defun %fs-write-octets (fs name octets n)
  (handler-case (progn (js:call fs "writeFileSync" name (js:from-octets octets :end n)) t)
    (error () nil)))

(defun %fs-read-octets (fs name)
  (handler-case (js:to-octets (js:call fs "readFileSync" name))
    (error () nil)))

;;; JavaScript modules

(defun js:module (name)
  "The JavaScript module that the host was given as NAME (the modules option
of host/lib.mjs).  Signals an error if there is none."
  (let ((m (%js-get (%js-host 0) (js:to-js name))))
    (when (member (%js-typeof m) '(0 1))
      (error "No JavaScript module ~S was given to the host" name))
    m))

(defun js:require (specifier)
  "The JavaScript module SPECIFIER loaded with Node.js's require(), found
from the working directory (an ES module too, if it does not wait at its
toplevel).  Not available in a browser."
  (let ((require (%js-host 2)))
    (unless (eql (%js-typeof require) 6)
      (error "JS:REQUIRE is not available on this host"))
    (js:funcall require specifier)))

(defun js:import (specifier)
  "The JavaScript module SPECIFIER loaded with import(): waits for it with
JS:AWAIT (so it is called where JS:CAN-AWAIT-P is true)."
  (js:await (js:funcall (%js-host 1) specifier)))

;;; Libraries: a module loaded by host/lib.mjs exports Lisp functions to
;;; JavaScript by name.  The host puts the object of the exports in
;;; globalThis.__clysm_exports while the module's toplevel forms run.

(defvar *js-exports* nil "The JavaScript object of the functions exported by JS:EXPORT.")

(defun %js-exports ()
  (or *js-exports*
      (let ((o (%js-get (%js-global) (%js-string "__clysm_exports"))))
        (setq *js-exports*
              (if (eql (%js-typeof o) 5)
                  o
                  ;; not loaded as a library: exports go to an object of their own
                  (let ((o (js:new (js:global "Object"))))
                    (js:set (js:global) "__clysm_exports" o)
                    o))))))

(defun js:export (name function &key async type)
  "Export FUNCTION to JavaScript as NAME (a string) in the exports of the
library (host/lib.mjs).  JavaScript calls it with its arguments converted by
JS:TO-LISP and gets its value converted by JS:TO-JS; an error that it does
not handle is thrown as a JavaScript Error.  With ASYNC, it returns a promise
and may wait with JS:AWAIT.  TYPE is its TypeScript type for the generated
.d.ts (by default (...args: any[]) => any, or a Promise of any)."
  (let ((f (if async (js:async-function function) (js:to-js function))))
    (when type (js:set f "clysmType" type))
    (when async (js:set f "clysmAsync" t))
    (js:set (%js-exports) name f)
    name))

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
      (setq *default-pathname-defaults*
            (pathname (concatenate 'string cwd (if *windows-paths* "\\" "/")))))))

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
    (when (equal platform "win32") (setq *windows-paths* t))
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
    (when (and (stringp cwd) *windows-paths*) (setq cwd (substitute #\/ #\\ cwd)))
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
