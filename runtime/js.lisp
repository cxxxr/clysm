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
           "MODULE" "REQUIRE" "IMPORT" "TO-OCTETS" "FROM-OCTETS"
           "THROW" "DEFINE-CLASS" "DECLARE-TYPES" "REEXPORT" "LIBRARY-OPTIONS"
           "GET-RAW" "CALL-RAW" "FUNCALL-RAW" "RAW-FUNCTION" "AWAIT-RAW" "JS-ERROR-THROWN"
           "LIBRARY-LOCATION" "ENTRYPOINT" "ENTRY-CONTEXT"))

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
   (message :initarg :message :initform "" :reader js:js-error-message)
   ;; what was thrown, as JavaScript has it (not converted: undefined, null
   ;; and false are told apart), or NIL when not known
   (thrown :initarg :thrown :initform nil :reader js:js-error-thrown))
  (:documentation "A JavaScript exception thrown by a function called through
the JS package, the rejection of a promise awaited by JS:AWAIT, or the value
given to JS:THROW.  VALUE is what was thrown (usually an Error), NAME and
MESSAGE its name and message; JS:JS-ERROR-THROWN is what was thrown as
JavaScript has it (undefined, null and false told apart), or NIL when it is
not known.  When no Lisp handler takes it, the call from
JavaScript that it leaves throws what was thrown, as it was."))
(%define-condition-message js:js-error (c)
  "JavaScript error: ~@[~A: ~]~A" (%condition-slot c 'name) (%condition-slot c 'message))

(defun %js-result (r)
  "The value R of a call, converted (JS:TO-LISP), after %JS-RESULT-RAW."
  (js:to-lisp (%js-result-raw r)))

(defun %js-result-raw (r)
  "Signal a Lisp error for a JavaScript exception boxed by the host's apply:
the original condition if it is the Error of a Lisp error (a Lisp function
called from JavaScript), otherwise a JS:JS-ERROR.  Otherwise R, unconverted."
  (when (eql (%js-typeof r) 5)
    (let ((msg (%js-get r (%js-string "__clysm_js_error"))))
      (when (eql (%js-typeof msg) 4)
        (let* ((value (%js-get r (%js-string "value")))
               ;; The host only exposes a condition originating in this
               ;; instance. Foreign LispErrors remain JavaScript exceptions.
               (condition (%js-get r (%js-string "lispCondition"))))
          (if (and condition (eql (%js-typeof condition) 9))
              (error (%js-unwrap-lisp condition))
              (error 'js:js-error
                     :value (js:to-lisp value)
                     :thrown value
                     :name (and (member (%js-typeof value) '(5 6))
                                (let ((name (%js-get value (%js-string "name"))))
                                  (and (eql (%js-typeof name) 4) (js:to-lisp name))))
                     :message (js:to-lisp msg)))))))
  r)

(defun js:call (object method &rest args)
  (let ((o (js:to-js object)))
    (%js-result (%js-apply (%js-get o (js:to-js method)) o (%js-args args)))))

(defun js:funcall (function &rest args)
  (%js-result (%js-apply (js:to-js function) (%js-special 1) (%js-args args))))

;;; The same, giving the value as JavaScript has it (not converted by
;;; JS:TO-LISP): a value of JavaScript (JS:JS-VALUE-P), for which JS:TYPEOF
;;; tells undefined, null and false apart, and a string that stays
;;; JavaScript's (a long text handed on, or read a part of, without being
;;; copied into Lisp).

(defun js:get-raw (object key)
  "The property KEY of OBJECT, as JavaScript has it (see JS:GET)."
  (%js-get (js:to-js object) (js:to-js key)))

(defun js:call-raw (object method &rest args)
  "JS:CALL, giving the value as JavaScript has it."
  (let ((o (js:to-js object)))
    (%js-result-raw (%js-apply (%js-get o (js:to-js method)) o (%js-args args)))))

(defun js:funcall-raw (function &rest args)
  "JS:FUNCALL, giving the value as JavaScript has it."
  (%js-result-raw (%js-apply (js:to-js function) (%js-special 1) (%js-args args))))

;;; A name written as a literal string (a property, a method, a global) is a
;;; string of Lisp, and made a string of JavaScript at every call: reading a
;;; property by its name cost six times what it costs with the name already
;;; JavaScript's.  Compiled code makes such a name JavaScript's once, where it
;;; is written, when it first comes there.  The JavaScript string is kept in
;;; a cell (string . JavaScript string), one for each name, not as the value
;;; of the LOAD-TIME-VALUE itself: a heap image cannot hold JavaScript
;;; objects, so SAVE-IMAGE empties the cells (%JS-FORGET-NAMES) and the image
;;; started from the file makes each name again when it first uses it.
(defvar *js-name-cells* nil "string -> (string . JavaScript string or NIL)")

(defun %js-name-cell (string)
  ;; (a LOAD-TIME-VALUE of the image may come before the DEFVAR)
  (let ((table (or (and (boundp '*js-name-cells*) *js-name-cells*)
                   (setq *js-name-cells* (make-hash-table :test 'equal)))))
    (or (gethash string table)
        (setf (gethash string table) (cons string nil)))))

(defun %js-name-fill (cell)
  (setf (cdr cell) (%js-string (car cell))))

(defun %js-forget-names ()
  (when *js-name-cells*
    (maphash (lambda (string cell) (declare (ignore string)) (setf (cdr cell) nil)) *js-name-cells*)))

(declaim (not-copied %js-cell-name))
(defun %js-cell-name (cell)
  "The JavaScript string of CELL (%JS-NAME-CELL), made at its first use."
  (or (cdr cell) (%js-name-fill cell)))

(defmacro %js-literal-name (string)
  ;; (one direct call where the name is used: the code of the cell's test
  ;; and of making the string, copied into each JS:GET of a literal name,
  ;; was most of it)
  `(%js-cell-name (load-time-value (%js-name-cell ,string))))

(declaim (not-copied %js-get-named %js-get-raw-named %js-set-named))
(defun %js-get-named (object cell) (js:get object (%js-cell-name cell)))
(defun %js-get-raw-named (object cell) (js:get-raw object (%js-cell-name cell)))
(defun %js-set-named (object cell value) (js:set object (%js-cell-name cell) value))

;; (a literal name: one direct call, of the object and the name's cell)
(define-compiler-macro js:get (&whole form object key)
  (if (stringp key) `(%js-get-named ,object (load-time-value (%js-name-cell ,key))) form))

(define-compiler-macro js:set (&whole form object key value)
  (if (stringp key) `(%js-set-named ,object (load-time-value (%js-name-cell ,key)) ,value) form))

(define-compiler-macro js:call (&whole form object method &rest args)
  (if (stringp method) `(js:call ,object (%js-literal-name ,method) ,@args) form))

(define-compiler-macro js:get-raw (&whole form object key)
  (if (stringp key) `(%js-get-raw-named ,object (load-time-value (%js-name-cell ,key))) form))

(define-compiler-macro js:call-raw (&whole form object method &rest args)
  (if (stringp method) `(js:call-raw ,object (%js-literal-name ,method) ,@args) form))

(define-compiler-macro js:global (&whole form &optional name)
  (if (stringp name) `(js:global (%js-literal-name ,name)) form))

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

;;; Variables whose values are objects of the host: SAVE-IMAGE (image.lisp)
;;; does not save their values, which an image started from a heap image
;;; makes again with their forms.
(defvar *host-variables* nil "(symbol . function making its value), latest first")

(defun %note-host-variable (name fn)
  (let ((e (assoc name *host-variables*)))
    (if e (setf (cdr e) fn) (push (cons name fn) *host-variables*))
    name))

(defmacro %defvar-host (name form &optional (doc nil docp))
  "DEFVAR of a variable whose value, made by FORM, is an object of the host
(a heap image does not save it; an image started from one evaluates FORM again)."
  `(progn (defvar ,name ,form ,@(when docp (list doc)))
          (%note-host-variable ',name (lambda () ,form))))

(%note-host-variable '*track-instance-hook* (lambda () nil))
(%defvar-host *js-suspending* :unknown "Whether the JavaScript engine can suspend WebAssembly.")

(defun %js-helper (name)
  "The function NAME of JavaScript that the runtime calls, of the host
\(host/jsffi.mjs): none is made from text by eval, which a page's
Content-Security-Policy forbids."
  (js:get (js:global "__clysm_runtime") name))

(%defvar-host *js-make-failure*
  (%js-host 5)
  "Makes the JavaScript Error for a Lisp condition (its stack has the Lisp frames
of the place where it was signalled), in the object that tells the host to throw it.")

(defun %js-failure (condition)
  ;; (a JavaScript exception that went through Lisp leaves it as it came)
  (let ((thrown (and (typep condition 'js:js-error) (%condition-slot condition 'thrown))))
    (when thrown
      (return-from %js-failure (js:object "__clysm_failure__" thrown))))
  (js:funcall *js-make-failure*
              (%condition-message condition)
              (%condition-type-string condition)
              (%js-wrap-lisp condition)))

(defvar *js-raw-functions* nil
  "The functions given their arguments as JavaScript has them (JS:EXPORT's
:RAW): an EQ table of them, made with the first.")

(defun js:raw-function (function)
  "A function that calls FUNCTION, to give JavaScript (an argument of
JS:CALL, a listener, a callback): JavaScript gives it its arguments as
JavaScript has them, not converted by JS:TO-LISP.  FUNCTION itself, given to
JavaScript, still has its arguments converted."
  (%js-raw-function function))

(defun %js-raw-function (function)
  "A function of its own that calls FUNCTION, and that JavaScript gives its
arguments unconverted (FUNCTION itself, exported or given elsewhere, is not)."
  (let ((raw (lambda (&rest arguments) (apply function arguments))))
    (setf (gethash raw (or *js-raw-functions*
                           (setq *js-raw-functions* (make-hash-table :test 'eq))))
          t)
    raw))

(defun %js-run (closure args async)
  (let* ((f (%js-unwrap-lisp closure))
         (raw (and *js-raw-functions* (gethash f *js-raw-functions*)))
         (n (js:to-lisp (%js-get args (%js-string "length"))))
         (lisp-args (let ((r nil))
                      (dotimes (i n (nreverse r))
                        (let ((a (%js-get args (%js-number (%to-double i)))))
                          (push (if raw a (js:to-lisp a)) r)))))
         (*js-running* t)
         (*js-can-await* async))
    (block run
      ;; an error that ERROR signals (not one given to SIGNAL, which returns
      ;; when no handler takes it) becomes the JavaScript error, before the
      ;; handlers of the Lisp code that called the JavaScript code; unless a
      ;; REPL, a script or a thread inside takes it (*DEBUGGER-FALLBACK*
      ;; bound again), or a debugger of the program's own
      (let* ((fallback (lambda (c) (return-from run (%js-failure c))))
             (*debugger-fallback* fallback))
        (handler-bind ((error (lambda (c)
                                (when (and (eq c *erroring*) (eq *debugger-fallback* fallback)
                                           (not (or *debugger-hook* *invoke-debugger-hook*)))
                                  (funcall fallback c)))))
          (js:to-js (apply f lisp-args)))))))

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
         ;; (handler frames of its own: the code outside goes on with
         ;; its frames while this computation waits)
         (let* ((*js-computation* computation)
                (*handler-frames* (%linked-handler-frames))
                (*handler-clusters* 1)
                (*restart-frames* (%linked-restart-frames))
                (*restart-clusters* 1))
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
  (%js-await-settled promise nil))

(defun js:await-raw (promise)
  "JS:AWAIT, giving the value as JavaScript has it (see JS:GET-RAW); the value
of a computation of JS:ASYNC is its Lisp value all the same."
  (%js-await-settled promise t))

(defun %js-await-settled (promise raw)
  (unless (js:can-await-p)
    (if (%js-suspending-p)
        (error "JS:AWAIT cannot wait here: not in a function of JS:ASYNC-FUNCTION (or JavaScript is in between)")
        ;; (Node.js 22 comes here: the module loads and runs, and nothing can wait)
        (error "JS:AWAIT cannot wait: this JavaScript engine cannot suspend WebAssembly (it has no JavaScript Promise Integration: Node.js 24, Chrome 137 or later)")))
  (let ((computation *js-computation*)
        (promise (js:to-js promise)))
    (%suspend-computation computation)
    (let ((result (%js-await promise)))
      (%resume-computation computation)
      (if raw
          (let ((value (%js-result-raw result)))
            ;; (the value of a computation of JS:ASYNC: as JS:AWAIT gives it; another Lisp
            ;; object stays as JavaScript has it)
            (if (eql (%js-typeof value) 9)
                (let ((lisp (js:to-lisp value)))
                  (if (and (consp lisp) (eq (car lisp) *js-async-value*)) (cdr lisp) value))
                value))
          (%js-async-value (%js-result result))))))

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
  (declare (ignore fs))
  ;; read by the host into the memory, then copied into a Lisp vector
  (let ((n (%js-read-file-mem (%js-string name))))
    (and (>= n 0) (%mem-get-octets n))))

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

(%defvar-host *js-exports* nil "The JavaScript object of the functions exported by JS:EXPORT.")

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

(%defvar-host *js-library* nil
  "What a library says of itself for the files bin/clysm library writes: its
exports, the types they name, its classes and how it is loaded.")

(defun %js-library ()
  (or *js-library*
      (let ((o (%js-get (%js-global) (%js-string "__clysm_library"))))
        (setq *js-library*
              (if (eql (%js-typeof o) 5)
                  o
                  ;; not loaded as a library: the same in an object of its own
                  (let ((o (js:object "exports" (js:object) "declarations" (js:array)
                                      "classes" (js:object) "options" (js:object))))
                    (js:set (js:global) "__clysm_library" o)
                    o))))))

(defun js:export (name value &key async type entry raw)
  "Export VALUE to JavaScript as NAME (a string) in the exports of the
library (host/lib.mjs).  A function is called with its arguments converted
by JS:TO-LISP and gives its value converted by JS:TO-JS; an error that it
does not handle is thrown as a JavaScript Error (a JS:JS-ERROR as what was
thrown).  With ASYNC, it returns a promise and may wait with JS:AWAIT.
Another value is converted by JS:TO-JS once: a constant, or a class of
JS:DEFINE-CLASS.  TYPE is its TypeScript type for the generated .d.ts (by
default (...args: any[]) => any, or a Promise of any, for a function).
ENTRY names the module of the library that exports it (ENTRY.mjs beside
NAME.mjs), by default the library's own (NAME.mjs).  With RAW, a function
is given its arguments as JavaScript has them, not converted: values of
JavaScript (JS:JS-VALUE-P) for which JS:TYPEOF tells undefined, null and
false apart, and a long string that Lisp reads a part of (JS:CALL of its
methods) without having it copied; JS:TO-LISP converts one."
  (when (and raw (functionp value)) (setq value (%js-raw-function value)))
  (let ((v (cond (async (js:async-function value))
                 (t (js:to-js value)))))
    (when (and type (functionp value)) (js:set v "clysmType" type))
    (when async (js:set v "clysmAsync" t))
    (js:set (js:get (%js-library) "exports") name
            (js:object "type" (or type :undefined) "async" (if async t :false)
                       "entry" (or entry :undefined)))
    (js:set (%js-exports) name v)
    name))

(defun %js-throw-with-message (value fallback)
  (unless (stringp fallback) (%type-error fallback 'string))
  (let* ((value (js:to-js value))
         (object (member (%js-typeof value) '(5 6)))
         (name (and object (js:get value "name")))
         (message (and object (js:get value "message"))))
    (error 'js:js-error :value (js:to-lisp value) :thrown value
                        :name (and (stringp name) name)
                        :message (if (stringp message) message fallback))))

(defun js:throw (value &key (message nil message-p))
  "Throw the JavaScript VALUE (an Error, usually) from the call of an export
or of a callback that Lisp runs: the call of JavaScript gets it as it is.  In
Lisp it is a JS:JS-ERROR, which a handler can take.  MESSAGE, when supplied,
is a string to use when VALUE has no string message property, instead of
printing VALUE.  Such a call need not retain the Lisp printer."
  (if message-p
      (%js-throw-with-message value message)
      (let* ((value (js:to-js value))
             (object (member (%js-typeof value) '(5 6)))
             (name (and object (js:get value "name")))
             (message (and object (js:get value "message"))))
        (error 'js:js-error :value (js:to-lisp value) :thrown value
                            :name (and (stringp name) name)
                            :message (if (stringp message) message (princ-to-string (js:to-lisp value)))))))

(define-compiler-macro js:throw (&whole form value &rest options)
  (if (and (= (length options) 2) (eq (first options) :message))
      `(%js-throw-with-message ,value ,(second options))
      form))

(defun js:library-location ()
  "The URL (a string) of the file the library's module was loaded from
\(host/lib.mjs: a path or a URL, as a file: URL), or NIL when it was given as
bytes or a module (an embedded library) and no `location` with them.  What
ships beside the module is found from it."
  (let ((location (js:get (%js-library) "location")))
    (and (stringp location) location)))

(defun %js-entry-expression (value)
  (cond ((consp value) (map 'vector #'%js-entry-expression value))
        ((and (keywordp value) (not (member value '(:true :false :null))))
         (string-downcase (symbol-name value)))
        (t value)))

(defun js:entrypoint (function &key entry ((:when condition) nil condition-p)
                                  imports environment shebang errors checks load-errors)
  "Register a synchronous initializer for a named entry of a lazy library.
It runs on importing that generated entry, not while generating it. The
function receives its entry context (url, environment and imported modules).
WHEN is a declarative predicate over globals, evaluated before loading Wasm.
IMPORTS lists (module-specifier export-name) pairs; \"*\" means the namespace.
ENVIRONMENT lists process.env names to capture in generated source, allowing
a bundler to replace them. SHEBANG emits a Node executable header. ERRORS may
be :STDERR to report a synchronous startup failure and set process.exitCode.
CHECKS lists (predicate message) pairs, checked before loading the library
when WHEN holds. Messages are strings or (:CONCAT ...), with (:GLOBAL ...)
parts converted to strings. LOAD-ERRORS lists (error-code message) pairs to
translate only failures of loading the library, not the initializer itself."
  (unless (functionp function) (%type-error function 'function))
  (unless (member errors '(nil :stderr))
    (error "JS:ENTRYPOINT: ERRORS is NIL or :STDERR"))
  (let* ((library (%js-library))
         (entries (or (js:get library "entrypoints")
                      (js:set library "entrypoints" (js:array)))))
    (js:call entries "push"
             (js:object "entry" entry "run" function
                        "condition" (if condition-p (%js-entry-expression condition) t)
                        "imports" (map 'vector (lambda (spec) (coerce spec 'vector)) imports)
                        "environment" (coerce environment 'vector)
                        "shebang" (if shebang t :false)
                        "errors" (if errors "stderr" :null)
                        "checks" (map 'vector #'%js-entry-expression checks)
                        "loadErrors" (map 'vector (lambda (spec) (coerce spec 'vector)) load-errors))))
  nil)

(defun js:entry-context (entry)
  "The generated entry's context, or NIL if that entry has not been imported.
Registering a context does not load a lazy library. Its URL names the entry
or its containing bundle, independently of JS:LIBRARY-LOCATION's Wasm URL."
  (let ((contexts (js:get (%js-library) "entryContexts")))
    (and contexts (js:get contexts entry))))

(defun js:declare-types (text &key entry)
  "Add TEXT, TypeScript declarations (interfaces, types), to the .d.ts of the
module ENTRY of the library (its own by default; see JS:EXPORT)."
  (js:call (js:get (%js-library) "declarations") "push"
           (js:object "text" text "entry" (or entry :undefined)))
  nil)

(defun js:reexport (names &key from entry type-only)
  "Re-export NAMES from the JavaScript module FROM in the generated module
ENTRY, without changing the library's runtime exports. NAMES is a string
or a list/vector of strings. FROM is the module specifier as written in an
import (including its extension). TYPE-ONLY writes only the .d.ts export.
A facade can thus preserve a package's public surface while its functions
and types belong to several generated entries or external modules."
  (let* ((library (%js-library))
         (items (or (js:get library "reexports")
                    (js:set library "reexports" (js:array)))))
    (js:call items "push"
             (js:object "names" (if (stringp names) (vector names) (coerce names 'vector))
                        "from" from "entry" entry "typeOnly" (if type-only t :false))))
  nil)

(defun js:library-options (&key lazy (output :output) (extension ".mjs") embed compression locations)
  "How the library is loaded by the modules bin/clysm library writes.  With
LAZY, importing a module of the library does not load it: the first call of
an export, or the first use of one of its classes, does, in the calling
thread and at once (the constants are written into the modules).  OUTPUT is
where what Lisp writes to the standard output goes: :OUTPUT (the standard
output of the process) or :ERROR (its standard error, for a program whose
standard output is a protocol).  EXTENSION is that of the modules: \".js\"
for a package whose package.json says \"type\": \"module\", where TypeScript
finds NAME.d.ts for an import of NAME.js (for NAME.mjs it looks for
NAME.d.mts).  With EMBED, the modules hold the module's bytes, and load it
with the host of host/web/lib.mjs, which imports no module of Node.js: a
library for a page, a browser's worker or Electron's renderer (and Node.js
too), which has no files, processes or command line.  :OUTPUT :ERROR is then
console.error.  COMPRESSION may be :GZIP or :XZ with EMBED: the bytes are
compressed before encoding, and decompressed synchronously by the bundled
web loader. :XZ needs the xz command when generating the library; its tiny
Wasm decoder is embedded too, so running it needs no external files.
LOCATIONS, for a non-embedded library, is a list/vector of nonempty module
paths relative to the generated loader. The first existing file is loaded;
when none exists, loading the first path reports the ordinary file error.
A lazy library searches only on first use, and USELIBRARY overrides it."
  (unless (member compression '(nil :gzip :xz))
    (error "JS:LIBRARY-OPTIONS: COMPRESSION is NIL, :GZIP or :XZ"))
  (when (and compression (not embed))
    (error "JS:LIBRARY-OPTIONS: COMPRESSION requires EMBED"))
  (unless (and (or (listp locations) (and (vectorp locations) (not (stringp locations))))
               (every (lambda (path) (and (stringp path) (plusp (length path)))) locations))
    (error "JS:LIBRARY-OPTIONS: LOCATIONS is a list or vector of nonempty strings"))
  (when (and embed (plusp (length locations)))
    (error "JS:LIBRARY-OPTIONS: LOCATIONS requires a non-embedded library"))
  (let ((options (js:get (%js-library) "options")))
    (js:set options "locations" (coerce locations 'vector))
    (js:set options "compression" (if compression (string-downcase (symbol-name compression)) :null))
    (js:set options "embed" (if embed t :false))
    (js:set options "extension" extension)
    (js:set options "lazy" (if lazy t :false))
    (js:set options "output" (string-downcase (symbol-name output))))
  nil)

(defun %js-plist-object (plist what)
  "A JavaScript object of the names and Lisp functions of PLIST, for WHAT."
  (let ((o (js:object)))
    (do ((l plist (cddr l))) ((null l) o)
      (unless (and (stringp (car l)) (functionp (cadr l)))
        (error "~A: ~S is not a name (a string) and a function" what (list (car l) (cadr l))))
      (js:set o (car l) (cadr l)))))

(defun js:define-class (name &key extends super-arguments constructor methods getters
                                   static type raw)
  "A JavaScript class NAME (a string), whose constructor and methods are Lisp
functions, and which a library exports with JS:EXPORT.  EXTENDS names the
class it extends: a global (\"Error\") or a class of JS:DEFINE-CLASS
defined before.  SUPER-ARGUMENTS are the positions of the arguments of the
constructor given to the constructor of that class (for an Error, the one of
its message).  CONSTRUCTOR is called with the new object and the arguments,
after it.  METHODS and GETTERS are plists of names and functions called with
the object and the arguments; STATIC a plist of the class's own functions.
TYPE is its TypeScript declaration for the .d.ts (\"declare class NAME
...\"; by default one that says nothing of its members).  With RAW, the
functions are given their arguments unconverted (see JS:EXPORT)."
  (when raw
    (flet ((raw (f) (if (functionp f) (%js-raw-function f) f))
           (raw-plist (plist) (loop for (n f) on plist by #'cddr collect n collect (if (functionp f) (%js-raw-function f) f))))
      (setq constructor (raw constructor)
            methods (raw-plist methods) getters (raw-plist getters) static (raw-plist static))))
  (let ((spec (js:object "name" name "extends" (or extends :null)
                         "superArguments" (if super-arguments (coerce super-arguments 'vector) :null)
                         "methods" (coerce (loop for (n) on methods by #'cddr collect n) 'vector)
                         "getters" (coerce (loop for (n) on getters by #'cddr collect n) 'vector)
                         "static" (coerce (loop for (n) on static by #'cddr collect n) 'vector)
                         "type" (or type :undefined)))
        (implementation (js:object "init" (or constructor :undefined)
                                   "methods" (%js-plist-object methods "JS:DEFINE-CLASS's methods")
                                   "getters" (%js-plist-object getters "JS:DEFINE-CLASS's getters")
                                   "static" (%js-plist-object static "JS:DEFINE-CLASS's static functions"))))
    (js:set (js:get (%js-library) "classes") name spec)
    (js:funcall (js:global "__clysm_define_class") spec implementation (%js-library))))

(defun %foreign-type-name (x) (js:typeof x))

;;; Tracking instances, so that the instances of a redefined class can be
;;; updated (see %UPDATE-OBSOLETE-INSTANCES in class0.lisp).  The instances
;;; are held by JavaScript WeakRefs: tracking does not keep them alive.
(%defvar-host *instance-tracker* nil)

(defun %instance-tracker ()
  (or *instance-tracker*
      (setq *instance-tracker*
            (js:funcall (%js-helper "newInstanceTracker")))))

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
              *live-instances-updater* #'%update-obsolete-instances
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

(defun %host-no-eval-string (src)
  "eval_string of a shaken program compiled without --eval-string."
  (declare (ignore src))
  (js:to-js "; Error: this module has no evaluator: compile it with --eval-string
"))

(%defvar-host *windows-paths*
  (let ((process (ignore-errors (js:global "process"))))
    (and (js:js-value-p process) (equal (ignore-errors (js:get process "platform")) "win32")))
  "True on a Windows host (Node.js's process.platform is win32): in
namestrings a backslash is a slash, \"C:\" is the device \"C\", and
\"//server/share\" (or \"\\\\server\\share\") the device of a UNC name.")

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
          (js:funcall (%js-helper "deleteProperty") (js:get process "env") name))))
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
  (handler-case (if (%can-wait-p)
                    (%await-promise (js:call (%host-net) "connectAsync" (string host) port) :connection)
                    (js:call (%host-net) "connect" (string host) port))
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
         (s (if (%can-wait-p)
                ;; the other threads run while it waits
                (%await-promise (js:call (%host-net) "readAsync" id (- end start)) :input)
                (js:call (%host-net) "read" id (- end start))))
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

(defun %tcp-listen (host port)
  "Listen for TCP connections on HOST (a string, or NIL for all interfaces)
and PORT (0 for any free port); returns the server id and the port."
  (let ((reply (handler-case (js:call (%host-net) "listen" (and host (string host)) port)
                 (error (e) (error 'simple-error :format-control "cannot listen on ~A:~D: ~A"
                                                 :format-arguments (list (or host "*") port e))))))
    (let ((space (position #\Space reply)))
      (values (parse-integer reply :end space) (parse-integer reply :start (1+ space))))))

(defun %tcp-accept (server)
  "Wait for a connection to the server SERVER; returns a connection id."
  (if (%can-wait-p)
      (%await-promise (js:call (%host-net) "acceptAsync" server) :connection)
      (js:call (%host-net) "accept" server)))

(defun %tcp-data-seq ()
  "A number that changes when data arrives on any connection (without a
call of the host's main thread)."
  (js:call (%host-net) "dataSeq"))

(defun %tcp-close-server (server)
  (js:call (%host-net) "closeServer" server)
  t)

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
    ;; (a library keeps the names of its functions in a file of its own:
    ;; the host names the frames from it)
    (let ((name-frames (ignore-errors (%js-host 4))))
      (when (and (stringp stack) (js:js-value-p name-frames))
        (let ((named (ignore-errors (js:funcall name-frames stack))))
          (when (stringp named) (setq stack named)))))
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
