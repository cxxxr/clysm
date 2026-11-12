;;;; reader.lisp — host-side reader for target source code.
;;;;
;;;; Target packages are mirrored by host packages.  The target COMMON-LISP and
;;;; KEYWORD packages are the host ones; every other target package FOO is a host
;;;; package named "CLYSM-T/FOO" (except SYSTEM -> CLYSM-SYS and
;;;; COMMON-LISP-USER -> CLYSM-USER).  Backquote is read into
;;;; (sys:quasiquote x) / (sys:unquote x) / (sys:unquote-splicing x) markers.

(in-package :clysm)

(defvar *target-packages* (make-hash-table :test 'equal)
  "target package name or nickname -> host package")
(defvar *host->target-name* (make-hash-table :test 'eq)
  "host package -> target package primary name")
(defvar *target-nicknames* (make-hash-table :test 'equal)
  "target primary name -> list of nicknames")

(defun register-target-package (name host-pkg &optional nicknames)
  (setf (gethash name *target-packages*) host-pkg
        (gethash host-pkg *host->target-name*) name
        (gethash name *target-nicknames*) nicknames)
  (dolist (n nicknames) (setf (gethash n *target-packages*) host-pkg))
  host-pkg)

(defun find-target-package (name &optional (errorp t))
  (let ((name (if (packagep name) (return-from find-target-package name) (string name))))
    (or (gethash name *target-packages*)
        (and errorp (error "Clysm reader: unknown package ~A" name)))))

(defvar *live-packages* nil "in live mode, packages map to themselves")

(defun target-package-name (host-pkg)
  (or (gethash host-pkg *host->target-name*)
      (and *live-packages* (package-name host-pkg))
      (error "host package ~A is not a target package" host-pkg)))

(defun ensure-host-package-for (name)
  (or (find-target-package name nil)
      (let ((hp (or (find-package (format nil "CLYSM-T/~A" name))
                    (make-package (format nil "CLYSM-T/~A" name) :use nil))))
        (register-target-package name hp))))

(defun init-target-packages ()
  (clrhash *target-packages*) (clrhash *host->target-name*) (clrhash *target-nicknames*)
  (register-target-package "COMMON-LISP" (find-package :cl) '("CL"))
  (register-target-package "KEYWORD" (find-package :keyword) '())
  (register-target-package "SYSTEM" (find-package :clysm-sys) '("SYS"))
  (register-target-package "COMMON-LISP-USER"
                           (or (find-package :clysm-user) (make-package :clysm-user :use '(:cl)))
                           '("CL-USER")))

(init-target-packages)

;;; ------------------------------------------------------------
;;; Reader
;;; ------------------------------------------------------------

(defvar *target-features* '(:clysm :common-lisp :ansi-cl :wasm :ieee-floating-point))
(defvar *read-package* (find-package :clysm-user))

(define-condition reader-eof (error) ())

(defparameter +qq+ 'clysm-sys::quasiquote)
(defparameter +uq+ 'clysm-sys::unquote)
(defparameter +uqs+ 'clysm-sys::unquote-splicing)

(defun whitespacep (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)))
(defun terminatingp (c)
  (or (whitespacep c) (member c '(#\( #\) #\' #\" #\; #\` #\,))))

(defun skip-ws (s)
  (loop
    (let ((c (peek-char nil s nil nil)))
      (cond ((null c) (return))
            ((whitespacep c) (read-char s))
            ((char= c #\;) (loop for ch = (read-char s nil nil) until (or (null ch) (char= ch #\Newline))))
            (t (return))))))

(defun skip-block-comment (s)
  (let ((depth 1))
    (loop
      (let ((c (read-char s)))
        (cond ((and (char= c #\|) (eql (peek-char nil s nil nil) #\#))
               (read-char s) (decf depth) (when (zerop depth) (return)))
              ((and (char= c #\#) (eql (peek-char nil s nil nil) #\|))
               (read-char s) (incf depth)))))))

(defvar *dot-marker* (make-symbol "DOT"))
(defvar *close-marker* (make-symbol "CLOSE"))

(defun target-read (s &optional (eof-error-p t) eof-value)
  (loop
    (skip-ws s)
    (let ((c (peek-char nil s nil nil)))
      (when (null c)
        (if eof-error-p (error 'reader-eof) (return eof-value)))
      (multiple-value-bind (obj valuep) (read-dispatch s)
        (when valuep
          (when (eq obj *close-marker*) (error "Clysm reader: unexpected )"))
          (return obj))))))

(defun read-dispatch (s)
  "Returns (values obj t) or (values nil nil) when nothing was read (comment)."
  (let ((c (read-char s)))
    (case c
      (#\( (values (read-list-rest s) t))
      (#\) (values *close-marker* t))
      (#\' (values (list 'quote (target-read s)) t))
      (#\` (values (list +qq+ (target-read s)) t))
      (#\, (if (member (peek-char nil s) '(#\@ #\.))
               (progn (read-char s) (values (list +uqs+ (target-read s)) t))
               (values (list +uq+ (target-read s)) t)))
      (#\" (values (read-string-literal s) t))
      (#\; (unread-char c s) (skip-ws s) (values nil nil))
      (#\# (read-sharp s))
      (t (unread-char c s) (values (read-token s) t)))))

(defun read-list-rest (s)
  (let ((items nil) (tail nil))
    (loop
      (skip-ws s)
      (let ((c (peek-char nil s)))
        (cond ((char= c #\)) (read-char s) (return (let ((l (nreverse items))) (when tail (setf (cdr (last l)) (car tail))) l)))
              (t (multiple-value-bind (obj valuep) (read-dispatch s)
                   (when valuep
                     (cond ((eq obj *dot-marker*)
                            (setf tail (list (target-read s)))
                            (skip-ws s)
                            (unless (char= (read-char s) #\)) (error "Clysm reader: bad dotted list"))
                            (let ((l (nreverse items))) (setf (cdr (last l)) (car tail)) (return l)))
                           ((eq obj *close-marker*)
                            (return (nreverse items)))
                           (t (push obj items)))))))))))

(defun read-string-literal (s)
  (with-output-to-string (o)
    (loop for c = (read-char s)
          until (char= c #\")
          do (if (char= c #\\) (write-char (read-char s) o) (write-char c o)))))

(defparameter *char-names*
  '(("SPACE" . #\Space) ("NEWLINE" . #\Newline) ("TAB" . #\Tab) ("RETURN" . #\Return)
    ("LINEFEED" . #\Linefeed) ("PAGE" . #\Page) ("BACKSPACE" . #\Backspace)
    ("RUBOUT" . #\Rubout) ("DEL" . #\Rubout) ("NUL" . #\Nul) ("NULL" . #\Nul)
    ("ESCAPE" . #\Esc) ("ESC" . #\Esc) ("BELL" . #\Bel) ("BEL" . #\Bel)))

(defun read-raw-token (s)
  "Read a token; returns (values string escapedp package-marker-info)."
  (let ((chars nil) (escaped nil) (escaped-positions nil) (i 0))
    (loop
      (let ((c (peek-char nil s nil nil)))
        (cond ((or (null c) (terminatingp c)) (return))
              ((char= c #\\) (read-char s) (push (read-char s) chars) (push i escaped-positions)
               (incf i) (setf escaped t))
              ((char= c #\|) (read-char s) (setf escaped t)
               (loop for ch = (read-char s)
                     until (char= ch #\|)
                     do (when (char= ch #\\) (setf ch (read-char s)))
                        (push ch chars) (push i escaped-positions) (incf i)))
              (t (read-char s) (push c chars) (incf i)))))
    (values (coerce (nreverse chars) 'string) escaped escaped-positions)))

(defun read-token (s)
  (multiple-value-bind (str escaped escpos) (read-raw-token s)
    (when (and (not escaped) (string= str ".")) (return-from read-token *dot-marker*))
    (unless escaped
      (let ((n (parse-number-token str)))
        (when n (return-from read-token n))))
    ;; case conversion of unescaped chars
    (let* ((up (with-output-to-string (o)
                 (loop for c across str for i from 0
                       do (write-char (if (member i escpos) c (char-upcase c)) o))))
           (colon (loop for i from 0 below (length up)
                        when (and (char= (char up i) #\:) (not (member i escpos))) return i)))
      (cond ((null colon) (intern up *read-package*))
            ((zerop colon) (intern (subseq up (if (and (> (length up) 1) (char= (char up 1) #\:)) 2 1))
                                   :keyword))
            (t (let* ((pkgname (subseq up 0 colon))
                      (double (and (< (1+ colon) (length up)) (char= (char up (1+ colon)) #\:)))
                      (name (subseq up (+ colon (if double 2 1))))
                      (pkg (find-target-package pkgname)))
                 (if double
                     (intern name pkg)
                     (multiple-value-bind (sym status) (find-symbol name pkg)
                       (cond ((eq status :external) sym)
                             ((eq pkg (find-package :keyword)) (intern name pkg))
                             ;; be lenient: allow pkg:sym for internal symbols too
                             (t (intern name pkg)))))))))))

(defun parse-number-token (str)
  (let ((len (length str)))
    (when (zerop len) (return-from parse-number-token nil))
    ;; integer: [+-]digits[.]
    (let ((i 0) (sign 1))
      (when (member (char str 0) '(#\+ #\-))
        (when (char= (char str 0) #\-) (setf sign -1))
        (setf i 1))
      (when (>= i len) (return-from parse-number-token nil))
      ;; integer
      (let ((end (if (char= (char str (1- len)) #\.) (1- len) len)))
        (when (and (> end i) (every #'digit-char-p (subseq str i end)))
          (return-from parse-number-token (* sign (parse-integer str :start i :end end)))))
      ;; ratio
      (let ((slash (position #\/ str)))
        (when slash
          (let ((a (subseq str i slash)) (b (subseq str (1+ slash))))
            (when (and (plusp (length a)) (plusp (length b))
                       (every #'digit-char-p a) (every #'digit-char-p b))
              (return-from parse-number-token
                (* sign (/ (parse-integer a) (parse-integer b))))))
          (return-from parse-number-token nil)))
      ;; float: digits [. digits] [exp]
      (let ((j i) (int-digits 0) (frac-digits 0) (expo 0) (marker nil))
        (loop while (and (< j len) (digit-char-p (char str j))) do (incf j) (incf int-digits))
        (when (and (< j len) (char= (char str j) #\.))
          (incf j)
          (loop while (and (< j len) (digit-char-p (char str j))) do (incf j) (incf frac-digits)))
        (when (zerop (+ int-digits frac-digits)) (return-from parse-number-token nil))
        (when (and (< j len) (member (char-upcase (char str j)) '(#\E #\S #\F #\D #\L)))
          (setf marker (char-upcase (char str j)))
          (incf j)
          (let ((k j))
            (when (and (< k len) (member (char str k) '(#\+ #\-))) (incf k))
            (unless (and (< k len) (every #'digit-char-p (subseq str k)))
              (return-from parse-number-token nil))
            (setf expo (parse-integer str :start j))
            (setf j len)))
        (unless (= j len) (return-from parse-number-token nil))
        (when (and (zerop frac-digits) (null marker)) (return-from parse-number-token nil))
        ;; exact computation then convert
        (let* ((dot (position #\. str :start i))
               (mant-end (or (position-if (lambda (c) (member (char-upcase c) '(#\E #\S #\F #\D #\L))) str :start i) len))
               (digits (remove #\. (subseq str i mant-end)))
               (scale (if dot (- mant-end dot 1) 0))
               (exact (* sign (parse-integer digits) (expt 10 (- expo scale))))
               (type (case marker ((#\D #\L) 'double-float) (t 'single-float))))
          (if (and (zerop exact) (= sign -1))
              (if (eq type 'double-float) -0d0 -0f0)
              (coerce exact type)))))))

(defun read-sharp (s)
  (let ((numarg nil))
    (loop while (digit-char-p (peek-char nil s))
          do (setf numarg (+ (* (or numarg 0) 10) (digit-char-p (read-char s)))))
    (let ((c (read-char s)))
      (case (char-downcase c)
        (#\' (values (list 'function (target-read s)) t))
        (#\( (values (coerce (read-list-rest s) 'simple-vector) t))
        (#\\ (let ((first (read-char s)))
               (if (terminatingp (or (peek-char nil s nil nil) #\Space))
                   (values first t)
                   (let ((rest (read-raw-token s)))
                     (let* ((name (concatenate 'string (string first) rest))
                            (entry (assoc name *char-names* :test #'string-equal)))
                       (values (cond (entry (cdr entry))
                                     ((= (length name) 1) (char name 0))
                                     ((and (> (length name) 2) (char-equal (char name 0) #\U)
                                           (char= (char name 1) #\+))
                                      (code-char (parse-integer name :start 2 :radix 16)))
                                     ((name-char name))
                                     (t (error "Clysm reader: unknown character name ~A" name)))
                               t))))))
        (#\: (values (make-symbol (string-upcase (read-raw-token s))) t))
        (#\| (skip-block-comment s) (values nil nil))
        (#\. (values (eval (target-read s)) t))
        ((#\+ #\-)
         (let* ((feature (let ((*read-package* (find-package :keyword))) (target-read s)))
                (ok (feature-match-p feature)))
           (when (char= c #\-) (setf ok (not ok)))
           (if ok
               (values nil nil)
               (progn (let ((*read-suppress* t)) (target-read s)) (values nil nil)))))
        (#\x (values (parse-integer (read-raw-token s) :radix 16) t))
        (#\b (values (parse-integer (read-raw-token s) :radix 2) t))
        (#\o (values (parse-integer (read-raw-token s) :radix 8) t))
        (#\r (values (parse-integer (read-raw-token s) :radix numarg) t))
        (#\* (let ((tok (read-raw-token s)))
               (values (coerce (map 'list #'digit-char-p tok) 'simple-bit-vector) t)))
        (#\c (let ((l (target-read s))) (values (complex (first l) (second l)) t)))
        (#\a (let ((contents (target-read s)))
               (values (make-array (array-dims-from numarg contents) :initial-contents contents) t)))
        (#\p (values (pathname (target-read s)) t))
        (t (error "Clysm reader: unsupported dispatch #~A" c))))))

(defun array-dims-from (rank contents)
  (if (zerop rank) nil
      (cons (length contents) (array-dims-from (1- rank) (if (plusp (length contents)) (elt contents 0) nil)))))

(defun feature-match-p (f)
  (cond ((symbolp f) (member f *target-features* :test #'string=))
        ((eq (first f) :and) (every #'feature-match-p (rest f)))
        ((eq (first f) :or) (some #'feature-match-p (rest f)))
        ((eq (first f) :not) (not (feature-match-p (second f))))
        (t nil)))

(defun read-all-forms (stream)
  (let ((forms nil))
    (loop
      (let ((f (target-read stream nil stream)))
        (when (eq f stream) (return (nreverse forms)))
        (push f forms)))))

;;; ------------------------------------------------------------
;;; Quasiquote expansion (Bawden's algorithm + simplification)
;;; ------------------------------------------------------------

(defun qq-expand (x depth)
  (cond ((consp x)
         (cond ((eq (car x) +qq+)
                (list 'cons (list 'quote +qq+) (qq-expand (cdr x) (1+ depth))))
               ((or (eq (car x) +uq+) (eq (car x) +uqs+))
                (cond ((> depth 0)
                       (list 'cons (list 'quote (car x)) (qq-expand (cdr x) (1- depth))))
                      ((and (eq (car x) +uq+) (consp (cdr x)) (null (cddr x)))
                       (cadr x))
                      (t (error "Illegal unquote-splicing position: ~S" x))))
               (t (qq-append (qq-expand-list (car x) depth) (qq-expand (cdr x) depth)))))
        ((simple-vector-p x)
         (list 'coerce (qq-expand (coerce x 'list) depth) ''simple-vector))
        ((or (symbolp x) (consp x)) (list 'quote x))
        (t x)))

(defun qq-expand-list (x depth)
  (cond ((consp x)
         (cond ((eq (car x) +qq+)
                (list 'list (list 'cons (list 'quote +qq+) (qq-expand (cdr x) (1+ depth)))))
               ((or (eq (car x) +uq+) (eq (car x) +uqs+))
                (cond ((> depth 0)
                       (list 'list (list 'cons (list 'quote (car x)) (qq-expand (cdr x) (1- depth)))))
                      ((eq (car x) +uq+) (cons 'list (cdr x)))
                      (t (cons :splice (cdr x)))))
               (t (list 'list (qq-append (qq-expand-list (car x) depth) (qq-expand (cdr x) depth))))))
        (t (list 'list (qq-expand x depth)))))

(defun qq-constant-p (form)
  (or (and (consp form) (eq (car form) 'quote))
      (and (atom form) (not (symbolp form)))
      (null form) (eq form t) (keywordp form)))

(defun qq-constant-value (form)
  (if (consp form) (second form) form))

(defun qq-append (head tail)
  "HEAD is (list . forms) or (:splice . forms); TAIL is a form."
  (cond ((eq (car head) :splice)
         ;; (append spliced tail)
         (let ((forms (cdr head)))
           (if (and (qq-constant-p tail) (null (qq-constant-value tail)))
               (if (= (length forms) 1) (first forms) `(append ,@forms))
               `(append ,@forms ,tail))))
        (t
         (let ((items (cdr head)))
           (cond ((and (every #'qq-constant-p items) (qq-constant-p tail))
                  (list 'quote (append (mapcar #'qq-constant-value items) (qq-constant-value tail))))
                 ((and (qq-constant-p tail) (null (qq-constant-value tail)))
                  `(list ,@items))
                 ((and (consp tail) (eq (car tail) 'list))
                  `(list ,@items ,@(cdr tail)))
                 ((and (consp tail) (eq (car tail) 'list*))
                  `(list* ,@items ,@(cdr tail)))
                 ((and (consp tail) (eq (car tail) 'quote) (consp (second tail))
                       (null (cdr (last (second tail)))) (< (length (second tail)) 4))
                  `(list ,@items ,@(mapcar (lambda (x) (list 'quote x)) (second tail))))
                 ((= (length items) 1) `(cons ,(first items) ,tail))
                 (t `(list* ,@items ,tail)))))))

;;; host-side macro so that host-evaluated code (macro bodies) understands backquote.
;;; When the compiler is itself compiled by Clysm, the macro is installed at
;;; load time instead (a DEFMACRO would also redefine it in the building host).
#-clysm (defmacro clysm-sys::quasiquote (x) (qq-expand x 0))
#+clysm (setf (macro-function 'clysm-sys::quasiquote)
              (lambda (form env) (declare (ignore env)) (qq-expand (second form) 0)))
