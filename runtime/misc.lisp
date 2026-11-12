;;;; misc.lisp — random numbers, time, environment

;;; ------------------------------------------------------------
;;; random
;;; ------------------------------------------------------------

(defstruct (random-state (:constructor %make-random-state (s0 s1)))
  s0 s1)

(defvar *random-state* (%make-random-state 123456789 362436069))

;;; printed readably as #.(%MAKE-RANDOM-STATE s0 s1)
(defmethod print-object ((rs random-state) stream)
  (if *print-readably*
      (progn
        (unless *read-eval* (error 'print-not-readable :object rs))
        (write-string "#." stream)
        (let ((*print-base* 10) (*print-radix* nil))
          (write (list '%make-random-state (random-state-s0 rs) (random-state-s1 rs)) :stream stream)))
      (call-next-method))
  rs)

(defun make-random-state (&optional state)
  (cond ((null state) (%make-random-state (random-state-s0 *random-state*) (random-state-s1 *random-state*)))
        ((eq state t) (%make-random-state (logand (get-universal-time) 268435455) 362436069))
        (t (%make-random-state (random-state-s0 state) (random-state-s1 state)))))

(defun %random-bits (state)
  ;; 28-bit xorshift-style generator on fixnums
  (let ((x (random-state-s0 state)) (y (random-state-s1 state)))
    (setq x (logand (logxor x (ash x 7)) 268435455))
    (setq x (logxor x (ash x -9)))
    (setq x (logand (logxor x (ash x 8)) 268435455))
    (setq y (logand (+ (* y 69069) 1) 268435455))
    (setf (random-state-s0 state) x (random-state-s1 state) y)
    (%fxlogand (%fxlogxor x y) 268435455)))

(defun random (limit &optional (state *random-state*))
  (cond ((and (integerp limit) (plusp limit))
         (if (<= limit 268435456)
             (mod (%random-bits state) limit)
             (let ((r 0) (bits 0) (n (integer-length limit)))
               (do () ((>= bits (+ n 28)))
                 (setq r (+ (ash r 28) (%random-bits state)) bits (+ bits 28)))
               (mod r limit))))
        ((and (floatp limit) (plusp limit))
         (* limit (/ (float (%random-bits state) limit) (float 268435456 limit))))
        (t (%type-error limit (quote (or (integer 1) (float (0.0))))))))

;;; ------------------------------------------------------------
;;; time
;;; ------------------------------------------------------------

(defconstant internal-time-units-per-second 1000)
(defconstant +unix-epoch-offset+ 2208988800)

(defun %now-ms () (%host-time 0))
(defun get-universal-time ()
  (+ +unix-epoch-offset+ (floor (%float-truncate-to-integer (%now-ms)) 1000)))
(defun get-internal-real-time () (%float-truncate-to-integer (%host-time 1)))
(defun get-internal-run-time () (%float-truncate-to-integer (%host-time 1)))

(defun %leap-year-p (y) (and (= (mod y 4) 0) (or (/= (mod y 100) 0) (= (mod y 400) 0))))
(defun %days-in-month (m y)
  (if (= m 2) (if (%leap-year-p y) 29 28) (svref #(0 31 28 31 30 31 30 31 31 30 31 30 31) m)))

(defun decode-universal-time (ut &optional (time-zone 0))
  (let* ((ut (- ut (* time-zone 3600)))
         (secs (mod ut 86400)) (days (floor ut 86400))
         (year 1900) (month 1))
    (loop
      (let ((ylen (if (%leap-year-p year) 366 365)))
        (if (>= days ylen) (setq days (- days ylen) year (+ year 1)) (return))))
    (loop
      (let ((mlen (%days-in-month month year)))
        (if (>= days mlen) (setq days (- days mlen) month (+ month 1)) (return))))
    (values (mod secs 60) (mod (floor secs 60) 60) (floor secs 3600) (+ days 1) month year
            (mod (floor ut 86400) 7) nil time-zone)))

(defun encode-universal-time (sec min hour date month year &optional (time-zone 0))
  (let ((days 0))
    (do ((y 1900 (+ y 1))) ((>= y year)) (setq days (+ days (if (%leap-year-p y) 366 365))))
    (do ((m 1 (+ m 1))) ((>= m month)) (setq days (+ days (%days-in-month m year))))
    (+ (* (+ days (- date 1)) 86400) (* hour 3600) (* min 60) sec (* time-zone 3600))))

(defun get-decoded-time () (decode-universal-time (get-universal-time)))

(defun sleep (seconds)
  (let ((end (+ (get-internal-real-time) (* seconds 1000))))
    (loop (when (>= (get-internal-real-time) end) (return nil)))))

(defmacro time (form)
  (let ((start (gensym)))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 ,form
         (format *trace-output* "~&Evaluation took:~%  ~,3F seconds of real time~%"
                 (/ (- (get-internal-real-time) ,start) 1000.0))))))

;;; pathnames: see pathname.lisp

;;; ------------------------------------------------------------
;;; environment
;;; ------------------------------------------------------------

(defun lisp-implementation-type () "Clysm")
(defun lisp-implementation-version () "0.1.0")
(defun machine-type () "WebAssembly")
(defun machine-version () "WasmGC")
(defun machine-instance () "wasm")
(defun software-type () "WebAssembly host")
(defun software-version () "")
(defun short-site-name () "")
(defun long-site-name () "")
(defvar *features* '(:clysm :common-lisp :ansi-cl :wasm :ieee-floating-point))
(defvar *modules* nil)
(defun provide (name) (pushnew (string name) *modules* :test #'string=) t)
(defvar *module-provider-functions* '(%module-provide-contrib)
  "Functions called by REQUIRE (without pathnames) with the module name; one
that returns true has provided the module.  ASDF adds its own provider.")

(defun %clysm-root ()
  "The directory of the Clysm installation, from the host (or NIL)."
  (let ((root (ignore-errors (js:get (js:global) "__clysm_root"))))
    (and (stringp root) (pathname (if (char= (char root (1- (length root))) #\/)
                                      root
                                      (concatenate 'string root "/"))))))

(defun %unicode-names ()
  (when (null *%unicode-names*)
    (setq *%unicode-names* :none)
    ;; %CLYSM-ROOT is NIL when the host does not tell where Clysm is installed
    (let* ((root (%clysm-root))
           (file (and root (merge-pathnames "data/unicode-names.txt" root))))
      (when (and file (probe-file file))
        (let ((by-code (make-hash-table)) (by-name (make-hash-table :test 'equalp)))
          (with-open-file (in file)
            (loop
              (let ((line (read-line in nil nil)))
                (unless line (return))
                (let* ((sp (position #\Space line))
                       (code (parse-integer line :end sp :radix 16))
                       (name (subseq line (+ sp 1))))
                  (setf (gethash code by-code) name
                        (gethash name by-name) code)))))
          (setq *%unicode-names* (cons by-code by-name))))))
  (if (consp *%unicode-names*) *%unicode-names* nil))

(defun %load-bundled (relative)
  (let* ((root (%clysm-root))
         (file (and root (merge-pathnames relative root))))
    (when (and file (probe-file file))
      (load file)
      t)))

(defun %quicklisp-setup-file ()
  (let ((home (%getenv "QUICKLISP_HOME")))
    (probe-file (merge-pathnames "setup.lisp"
                                 (if home
                                     (pathname (if (char= (char home (1- (length home))) #\/)
                                                   home (concatenate 'string home "/")))
                                     (merge-pathnames "quicklisp/" (user-homedir-pathname)))))))

(defun %module-provide-contrib (name)
  "Modules shipped with Clysm: ASDF and UIOP (third_party/asdf/asdf.lisp),
QL-CLYSM (Quicklisp support, contrib/ql-clysm.lisp) and QUICKLISP (loads
$QUICKLISP_HOME/setup.lisp, by default ~/quicklisp/setup.lisp, with that
support)."
  (let ((name (string-upcase name)))
    (cond ((member name '("ASDF" "UIOP") :test #'string=)
           (when (%load-bundled "third_party/asdf/asdf.lisp")
             (provide "ASDF") (provide "UIOP")
             (%load-bundled "contrib/asdf-hooks.lisp")
             t))
          ((string= name "QL-CLYSM")
           (when (%load-bundled "contrib/ql-clysm.lisp")
             (provide "QL-CLYSM")
             t))
          ((string= name "QUICKLISP")
           (let ((setup (%quicklisp-setup-file)))
             (when setup
               (require "ASDF")
               (load setup)         ; the ASDF hook adds QL-CLYSM
               (provide "QUICKLISP")
               t))))))

(defun require (name &optional pathnames)
  (let ((name (string name)))
    (unless (member name *modules* :test #'string=)
      (cond (pathnames
             (dolist (p (if (listp pathnames) pathnames (list pathnames)))
               (load p)))
            ((dolist (f *module-provider-functions* nil)
               (when (funcall f name) (return t))))
            (t (error "REQUIRE: don't know how to load module ~S" name))))
    nil))

(deftype string-designator () '(or string symbol character))

;;; Documentation strings live in a table keyed by (object doc-type); a
;;; function or class has one string for doc-types T/FUNCTION resp. T/TYPE.
(defvar %documentation-table (make-hash-table :test 'equal))
(defun %doc-key (x doc-type)
  (cond ((and (functionp x) (member doc-type '(t function))) (list x 'function))
        ((and (%class-p x) (member doc-type '(t type))) (list x 'type))
        (t (list x doc-type))))
(defgeneric documentation (x doc-type))
(defmethod documentation (x doc-type)
  (if (symbolp x)
      (%symbol-doc x doc-type)
      (values (gethash (%doc-key x doc-type) %documentation-table))))
(defgeneric (setf documentation) (new-value x doc-type))
(defmethod (setf documentation) (new-value x doc-type)
  (cond
    ((symbolp x) (%set-symbol-doc x doc-type new-value))
    (new-value
      (setf (gethash (%doc-key x doc-type) %documentation-table) new-value))
    (t (remhash (%doc-key x doc-type) %documentation-table)))
  new-value)
(defun %set-documentation (x doc-type new-value)
  (funcall (fdefinition '(setf documentation)) new-value x doc-type))

(defgeneric describe-object (object stream))
(defmethod describe-object (object stream)
  (format stream "~&~S~%  [~S]~%" object (type-of object))
  (when (symbolp object)
    (when (boundp object) (format stream "~&  Its value is ~S~%" (symbol-value object)))
    (when (fboundp object)
      (format stream "~&  It is ~A~%" (cond ((special-operator-p object) "a special operator")
                                            ((macro-function object) "a macro")
                                            (t "a function"))))
    (when (symbol-package object)
      (format stream "~&  It is in package ~A~%" (package-name (symbol-package object))))))
(defun describe (object &optional stream)
  (let ((s (%output-stream stream)))
    (describe-object object s)
    (values)))
(defun inspect (object) (describe object))
(defun room (&optional (x :default))
  (declare (ignore x))
  (format t "~&Memory is managed by the WebAssembly host's garbage collector;~%~
             no allocation statistics are available.~%")
  (values))
(defun ed (&optional x) (declare (ignore x)) nil)
(defun dribble (&optional x) (declare (ignore x)) nil)
(defun apropos-list (string &optional package)
  (let ((r nil))
    (dolist (p (if package (list (find-package package)) (list-all-packages)) r)
      (dolist (s (%package-symbol-list p :present))
        (when (search (string-upcase string) (symbol-name s)) (pushnew s r))))))
(defun apropos (string &optional package)
  (dolist (s (apropos-list string package)) (format t "~S~%" s))
  (values))
;;; TRACE replaces the function definition with a wrapper that reports calls
;;; and returns on *TRACE-OUTPUT*.  A generic function's wrapper shares its
;;; metaobject, so methods added later are still found.
(defvar %traced-functions nil "list of (name original wrapper)")
(defvar %trace-depth 0)

(defun %trace-entry-current-p (e)
  (and (fboundp (car e)) (eq (fdefinition (car e)) (third e))))

(defun %traced-names ()
  (setq %traced-functions (remove-if-not #'%trace-entry-current-p %traced-functions))
  (mapcar #'car %traced-functions))

(defun %trace-1 (name)
  (cond ((not (fboundp name)) (warn "~S is not a function; not traced" name))
        ((and (symbolp name) (or (special-operator-p name) (macro-function name)))
         (warn "~S is not a function; not traced" name))
        ((member name (%traced-names) :test #'equal)
         (warn "~S is already traced" name))
        (t
         (let* ((original (fdefinition name))
                (wrapper (lambda (&rest args)
                           (let ((depth %trace-depth))
                             (format *trace-output* "~&~V@T~D: ~S~%" (* 2 depth) depth (cons name args))
                             (let ((vals (let ((%trace-depth (+ depth 1)))
                                           (multiple-value-list (apply original args)))))
                               (format *trace-output* "~&~V@T~D: ~S returned~{ ~S~}~%"
                                       (* 2 depth) depth name vals)
                               (values-list vals))))))
           (%set-closure-name wrapper (or (%gf-info original) name))
           (%set-fdefinition name wrapper)
           (push (list name original wrapper) %traced-functions))))
  name)

(defun %untrace-1 (name)
  (let ((e (find name %traced-functions :key #'car :test #'equal)))
    (if e
        (progn (when (%trace-entry-current-p e) (%set-fdefinition name (second e)))
               (setq %traced-functions (remove e %traced-functions)))
        (warn "~S is not traced" name))
    name))

(defmacro trace (&rest names)
  (if names
      `(mapcar #'%trace-1 ',names)
      `(%traced-names)))
(defmacro untrace (&rest names)
  (if names
      `(mapcar #'%untrace-1 ',names)
      `(mapcar #'%untrace-1 (%traced-names))))
(defmacro step (form) form)
(defun gc (&key full) (declare (ignore full)) nil)
(defun function-lambda-expression (f) (values nil t (%closure-name f)))
(defun upgraded-complex-part-type (type &optional env) (declare (ignore env)) type)
(defun arithmetic-error-operation* (c) (arithmetic-error-operation c))
