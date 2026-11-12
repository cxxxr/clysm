;;;; misc.lisp — random numbers, time, environment

;;; ------------------------------------------------------------
;;; random
;;; ------------------------------------------------------------

(defstruct (random-state (:constructor %make-random-state (s0 s1)))
  s0 s1)

(defvar *random-state* (%make-random-state 123456789 362436069))

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
        (t (error "RANDOM: invalid limit ~S" limit))))

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
(defun require (name &optional paths)
  (let ((name (string name)))
    (unless (member name *modules* :test #'string=)
      (when (null paths)
        (error 'simple-error :format-control "REQUIRE: don't know how to load the module ~A"
                             :format-arguments (list name)))
      (dolist (p (if (listp paths) paths (list paths)))
        (load p)))
    nil))

(defun documentation (x doc-type) (declare (ignore doc-type)) (get (if (symbolp x) x 'nil) '%documentation))
(defun %set-documentation (value x doc-type) (declare (ignore doc-type))
  (when (symbolp x) (%put x '%documentation value)) value)

(defgeneric describe-object (object stream))
(defmethod describe-object (object stream)
  (format stream "~&~S~%  [~A]~%" object (type-of object)))
(defun describe (object &optional stream)
  (let ((s (%output-stream stream)))
    (describe-object object s)
    (values)))
(defun inspect (object) (describe object))
(defun room (&optional x) (declare (ignore x)) (values))
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
(defmacro trace (&rest names) `',names)
(defmacro untrace (&rest names) `',names)
(defmacro step (form) form)
(defun gc (&key full) (declare (ignore full)) nil)
(defun function-lambda-expression (f) (values nil t (%closure-name f)))
(defun upgraded-array-element-type (type &optional env) (declare (ignore env)) (%normalize-element-type type))
(defun upgraded-complex-part-type (type &optional env) (declare (ignore env)) type)
(defun arithmetic-error-operation* (c) (arithmetic-error-operation c))
