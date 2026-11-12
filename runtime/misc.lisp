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

;;; ------------------------------------------------------------
;;; pathnames (minimal, namestring based)
;;; ------------------------------------------------------------

(defstruct (pathname (:constructor %make-pathname (host device directory name type version))
                     (:predicate pathnamep))
  host device directory name type version)

(defun %parse-namestring (s)
  (let* ((slash (position #\/ s :from-end t))
         (dir-part (if slash (subseq s 0 (+ slash 1)) nil))
         (file (if slash (subseq s (+ slash 1)) s))
         (dot (position #\. file :from-end t))
         (name (if (and dot (> dot 0)) (subseq file 0 dot) (if (string= file "") nil file)))
         (type (if (and dot (> dot 0)) (subseq file (+ dot 1)) nil))
         (directory (when dir-part
                      (let ((parts nil) (start (if (char= (char dir-part 0) #\/) 1 0)))
                        (do ((i start (+ i 1))) ((>= i (length dir-part)))
                          (when (char= (char dir-part i) #\/)
                            (push (subseq dir-part start i) parts)
                            (setq start (+ i 1))))
                        (cons (if (char= (char dir-part 0) #\/) :absolute :relative)
                              (remove "" (nreverse parts) :test #'string=))))))
    (%make-pathname nil nil directory name type nil)))

(defun pathname (x)
  (cond ((pathnamep x) x)
        ((stringp x) (%parse-namestring x))
        ((%streamp x) (or (stream-pathname x) (%parse-namestring "")))
        (t (%type-error x 'pathname-designator))))

(defun namestring (x)
  (let ((p (pathname x)))
    (with-output-to-string (s)
      (let ((dir (pathname-directory p)))
        (when dir
          (when (eq (car dir) :absolute) (write-char #\/ s))
          (dolist (d (cdr dir)) (write-string (if (eq d :up) ".." (string d)) s) (write-char #\/ s))))
      (when (pathname-name p) (write-string (pathname-name p) s))
      (when (pathname-type p) (write-char #\. s) (write-string (pathname-type p) s)))))

(defun file-namestring (x)
  (let ((p (pathname x)))
    (concatenate 'string (or (pathname-name p) "") (if (pathname-type p) "." "") (or (pathname-type p) ""))))
(defun directory-namestring (x)
  (namestring (make-pathname :directory (pathname-directory (pathname x)))))
(defun enough-namestring (x &optional defaults) (declare (ignore defaults)) (namestring x))
(defun host-namestring (x) (declare (ignore x)) "")

(defun make-pathname (&key host device directory name type version defaults case)
  (declare (ignore case))
  (let ((d (if defaults (pathname defaults) nil)))
    (%make-pathname host device
                    (if (stringp directory) (list :absolute directory)
                        (or directory (and d (pathname-directory d))))
                    (or name (and d (pathname-name d)))
                    (or type (and d (pathname-type d)))
                    version)))

(defun merge-pathnames (path &optional (defaults *default-pathname-defaults*) default-version)
  (declare (ignore default-version))
  (let ((p (pathname path)) (d (pathname defaults)))
    (%make-pathname nil nil
                    (let ((pd (pathname-directory p)))
                      (cond ((null pd) (pathname-directory d))
                            ((eq (car pd) :absolute) pd)
                            (t (append (or (pathname-directory d) (list :relative)) (cdr pd)))))
                    (or (pathname-name p) (pathname-name d))
                    (or (pathname-type p) (pathname-type d))
                    nil)))

(defvar *default-pathname-defaults* (%make-pathname nil nil nil nil nil nil))

(defun probe-file (x)
  (let ((s (open x :direction :input :if-does-not-exist nil)))
    (if s (progn (close s) (pathname x)) nil)))
(defun truename (x) (pathname x))
(defun wild-pathname-p (p &optional field) (declare (ignore p field)) nil)
(defun pathname-match-p (p w) (declare (ignore p w)) nil)
(defun parse-namestring (thing &optional host defaults &key (start 0) end junk-allowed)
  (declare (ignore host defaults junk-allowed))
  (values (pathname (subseq (string thing) start end)) (or end (length (string thing)))))
(defun user-homedir-pathname (&optional host) (declare (ignore host)) (pathname "/"))
(defun delete-file (x) (declare (ignore x)) t)
(defun rename-file (a b) (declare (ignore a)) (pathname b))
(defun ensure-directories-exist (p &key verbose) (declare (ignore verbose)) (values p nil))
(defun directory (p &key) (declare (ignore p)) nil)
(defun file-write-date (p) (declare (ignore p)) nil)
(defun file-author (p) (declare (ignore p)) nil)
(defun pathname-device* (p) (pathname-device p))
(defun logical-pathname (x) (pathname x))
(defun translate-pathname (s f to &key) (declare (ignore f to)) (pathname s))
(defun translate-logical-pathname (p &key) (pathname p))
(defun logical-pathname-translations (h) (declare (ignore h)) nil)
(defun %set-lpt (h v) (declare (ignore h)) v)
(defun load-logical-pathname-translations (h) (declare (ignore h)) nil)
(defun compile-file-pathname (p &key output-file) (declare (ignore output-file)) (pathname p))

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
(defun require (name &optional paths) (declare (ignore paths)) (provide name))

(defun documentation (x doc-type) (declare (ignore doc-type)) (get (if (symbolp x) x 'nil) '%documentation))
(defun %set-documentation (value x doc-type) (declare (ignore doc-type))
  (when (symbolp x) (%put x '%documentation value)) value)

(defun describe (object &optional stream)
  (let ((s (%output-stream stream)))
    (format s "~&~S~%  [~A]~%" object (type-of object))
    (values)))
(defun describe-object (object stream) (describe object stream))
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
