;;;; pprint.lisp — the pretty printer
;;;;
;;;; Objects are converted to a document tree of logical blocks; the layout
;;;; follows XP's rules: a block that fits is printed flat, otherwise linear
;;;; newlines break, and fill newlines break when the following section does
;;;; not fit (or the preceding section was broken).

(defconstant +pp-width+ 80)

;;; doc block: #(:block prefix suffix items indent width)
(defun %pp-block (prefix suffix items indent)
  (let ((b (vector :block prefix suffix items indent 0)))
    (setf (svref b 5) (%pp-items-width items (+ (length prefix) (length suffix))))
    b))

(defun %pp-items-width (items base)
  (let ((w base))
    (dolist (it items w)
      (cond ((stringp it) (setq w (+ w (length it))))
            ((simple-vector-p it) (setq w (+ w (svref it 5))))
            ((eq it :mandatory) (setq w (+ w 10000)))))))

(defun %pp-width (it)
  (cond ((stringp it) (length it))
        ((simple-vector-p it) (svref it 5))
        (t 0)))

(defun %pp-flat-string (x)
  (let ((*print-pretty* nil))
    (with-output-to-string (s) (%write-object-1 x s))))

;;; ------------------------------------------------------------
;;; building documents
;;; ------------------------------------------------------------

(defparameter *pp-body-forms*
  '((lambda . 1) (let . 1) (let* . 1) (when . 1) (unless . 1) (dolist . 1) (dotimes . 1)
    (flet . 1) (labels . 1) (macrolet . 1) (symbol-macrolet . 1) (block . 1) (catch . 1)
    (unwind-protect . 1) (progn . 0) (tagbody . 0) (locally . 0) (eval-when . 1)
    (defun . 2) (defmacro . 2) (multiple-value-bind . 2) (destructuring-bind . 2)
    (with-open-file . 1) (with-output-to-string . 1) (with-input-from-string . 1)
    (with-slots . 2) (handler-bind . 1) (prog1 . 1) (prog2 . 2) (defstruct . 1)
    (defclass . 3) (defgeneric . 2) (define-condition . 3) (do-symbols . 1)
    (loop . 0)))

(defvar *pp-level* 0)

(defun %pp-doc (x)
  (cond ((and (consp x) (not (and *circle-table* (gethash x *circle-table*))))
         (%pp-list-doc x))
        ((and (vectorp x) (not (stringp x)) (not (bit-vector-p x)) *print-array*
              (not (and *circle-table* (gethash x *circle-table*))))
         (%pp-vector-doc x))
        (t (%pp-flat-string x))))

(defun %pp-elements (list)
  "Docs for the elements of LIST honoring *print-length*; returns (values docs dotted-tail-or-nil truncated)"
  (let ((docs nil) (n 0) (l list))
    (loop
      (when (atom l) (return (values (nreverse docs) l nil)))
      (when (and *print-length* (>= n *print-length*)) (return (values (nreverse docs) nil t)))
      (when (and (> n 0) *circle-table* (gethash l *circle-table*))
        (return (values (nreverse docs) l nil)))
      (push (%pp-doc (car l)) docs)
      (setq n (+ n 1) l (cdr l)))))

(defun %pp-join (docs sep-kind)
  (let ((items nil) (first t))
    (dolist (d docs (nreverse items))
      (unless first (push " " items) (push sep-kind items))
      (push d items)
      (setq first nil))))

(defun %pp-list-doc (x)
  (cond ((and (eq (car x) 'quote) (consp (cdr x)) (null (cddr x)))
         (%pp-block "'" "" (list (%pp-doc (cadr x))) 1))
        ((and (eq (car x) 'function) (consp (cdr x)) (null (cddr x)))
         (%pp-block "#'" "" (list (%pp-doc (cadr x))) 2))
        ((and *print-level* (>= *pp-level* *print-level*)) "#")
        (t
         (let ((*pp-level* (+ *pp-level* 1)))
           (multiple-value-bind (docs tail truncated) (%pp-elements x)
             (let* ((op (car x))
                    (style (and (symbolp op) (not truncated) (null tail)
                                (cond ((eq op 'if) :if)
                                      ((eq op 'cond) :cond)
                                      ((member op '(let let*)) :let)
                                      ((member op '(flet labels macrolet)) :flet)
                                      ((member op '(setq psetq setf psetf)) :setq)
                                      ((assoc op *pp-body-forms*) :body)
                                      (t nil))))
                    (items
                      (case style
                        (:if (if (cdr docs)
                                 (append (list (car docs) " " (cadr docs))
                                         (let ((r nil)) (dolist (d (cddr docs) (nreverse r))
                                                          (push " " r) (push :mandatory r) (push d r))))
                                 docs))
                        (:let (append (list (car docs))
                                      (when (cdr docs) (list " " (cadr docs)))
                                      (let ((r nil)) (dolist (d (cddr docs) (nreverse r))
                                                       (push " " r) (push :mandatory r) (push d r)))))
                        (:flet (append (list (car docs))
                                       (when (cdr docs)
                                         (list " " (if (listp (cadr x))
                                                       (%pp-block "(" ")"
                                                                  (%pp-join (mapcar #'%pp-local-def-doc (cadr x)) :mandatory)
                                                                  1)
                                                       (cadr docs))))
                                       (let ((r nil)) (dolist (d (cddr docs) (nreverse r))
                                                        (push " " r) (push :mandatory r) (push d r)))))
                        (:setq (let ((r (list (car docs))) (i 0))
                                 (dolist (d (cdr docs))
                                   (push " " r)
                                   (when (and (> i 0) (evenp i)) (push :mandatory r))
                                   (push d r)
                                   (setq i (+ i 1)))
                                 (nreverse r)))
                        (:cond (append (list (car docs))
                                       (when (cdr docs) (cons " " (%pp-join (cdr docs) :fill)))))
                        (:body (let* ((n (cdr (assoc op *pp-body-forms*)))
                                      (head (list (car docs))) (rest (cdr docs)))
                                 (dotimes (i n)
                                   (when rest (setq head (append head (list " " (pop rest))))))
                                 (append head
                                         (let ((r nil)) (dolist (d rest (nreverse r))
                                                          (push " " r) (push :linear r) (push d r))))))
                        (t (%pp-join docs :fill))))
                    (items (append items
                                   (cond (truncated (list " " :fill "..."))
                                         (tail (list " " :fill "." " " (%pp-doc tail)))
                                         (t nil))))
                    (indent (case style
                              (:if 4)
                              (:cond (+ 2 (length (%pp-flat-string op))))
                              ((:body :let :flet) 2)
                              (:setq (+ 2 (length (%pp-flat-string op))))
                              (t 1))))
               (%pp-block "(" ")" items indent)))))))

(defun %pp-local-def-doc (def)
  "A local function definition (name lambda-list . body) inside FLET/LABELS."
  (if (and (consp def) (consp (cdr def)))
      (let ((*pp-level* (+ *pp-level* 1)))
        (%pp-block "(" ")"
                   (append (list (%pp-doc (car def)) " " (%pp-doc (cadr def)))
                           (let ((r nil)) (dolist (b (cddr def) (nreverse r))
                                            (push " " r) (push :mandatory r) (push (%pp-doc b) r))))
                   2))
      (%pp-doc def)))

(defun %pp-vector-doc (v)
  (if (and *print-level* (>= *pp-level* *print-level*)) "#"
      (let ((*pp-level* (+ *pp-level* 1)) (docs nil) (n (length v)) (truncated nil))
        (dotimes (i n)
          (when (and *print-length* (>= i *print-length*)) (setq truncated t) (return))
          (push (%pp-doc (aref v i)) docs))
        (%pp-block "#(" ")" (append (%pp-join (nreverse docs) :fill)
                                    (when truncated (list " " :fill "...")))
                   2))))

;;; ------------------------------------------------------------
;;; layout
;;; ------------------------------------------------------------

(defvar *pp-pending-spaces* 0)
(defvar *pp-multiline* nil)

(defun %pp-emit (string stream)
  (when (> (length string) 0)
    (let ((trailing 0) (n (length string)))
      (do () ((not (and (< trailing n) (char= (char string (- n trailing 1)) #\Space))))
        (setq trailing (+ trailing 1)))
      (if (= trailing n)
          (setq *pp-pending-spaces* (+ *pp-pending-spaces* n))
          (progn
            (dotimes (i *pp-pending-spaces*) (write-char #\Space stream))
            (setq *pp-pending-spaces* trailing)
            (write-string string stream :end (- n trailing)))))))

(defun %pp-flush-spaces (stream)
  (dotimes (i *pp-pending-spaces*) (write-char #\Space stream))
  (setq *pp-pending-spaces* 0))

(defun %pp-print-flat (it stream)
  (cond ((stringp it) (%pp-emit it stream))
        ((simple-vector-p it)
         (%pp-emit (svref it 1) stream)
         (dolist (x (svref it 3)) (%pp-print-flat x stream))
         (%pp-emit (svref it 2) stream))
        ((eq it :mandatory) (%pp-break stream 0))
        (t nil)))

(defun %pp-break (stream col)
  (setq *pp-pending-spaces* 0)
  (write-char #\Newline stream)
  (dotimes (i col) (write-char #\Space stream))
  (setq *pp-multiline* t)
  col)

(defun %pp-section-width (items suffix-width trailing)
  "Width of ITEMS up to the next newline marker; if the end is reached,
include SUFFIX-WIDTH and TRAILING."
  (let ((w 0))
    (dolist (it items (+ w suffix-width trailing))
      (when (member it '(:fill :linear :mandatory :miser)) (return w))
      (setq w (+ w (%pp-width it))))))

(defun %pp-layout (it stream col trailing)
  "Print IT at column COL; returns the new column."
  (cond ((stringp it) (%pp-emit it stream) (+ col (length it)))
        ((not (simple-vector-p it)) col)
        ((<= (+ col (svref it 5) trailing) +pp-width+)
         (%pp-print-flat it stream)
         (+ col (svref it 5)))
        (t
         (let* ((prefix (svref it 1)) (suffix (svref it 2))
                (indent-col (+ col (svref it 4)))
                (items (svref it 3))
                (section-multi nil))
           (%pp-emit prefix stream)
           (setq col (+ col (length prefix)))
           (do ((l items (cdr l))) ((null l))
             (let ((x (car l)))
               (cond ((stringp x) (%pp-emit x stream) (setq col (+ col (length x))))
                     ((simple-vector-p x)
                      (let ((*pp-multiline* nil))
                        (setq col (%pp-layout x stream col
                                              (%pp-section-width (cdr l) (length suffix) trailing)))
                        (when *pp-multiline* (setq section-multi t))))
                     ((member x '(:linear :mandatory))
                      (setq col (%pp-break stream indent-col) section-multi nil))
                     ((eq x :fill)
                      (let ((next (%pp-section-width (cdr l) (length suffix) trailing)))
                        (when (or section-multi (> (+ col next) +pp-width+))
                          (setq col (%pp-break stream indent-col) section-multi nil))))
                     (t nil))))
           (%pp-emit suffix stream)
           (when section-multi (setq *pp-multiline* t))
           (+ col (length suffix))))))

(defun %pretty-write (x stream)
  (let ((doc (%pp-doc x))
        (*pp-pending-spaces* 0)
        (*pp-multiline* nil))
    (%pp-layout doc stream (%stream-column stream) 0)
    (%pp-flush-spaces stream)))

;;; ------------------------------------------------------------
;;; user-level pretty printing interface (simplified)
;;; ------------------------------------------------------------

(defun pprint-newline (kind &optional stream)
  (declare (ignore kind stream))
  nil)
(defun pprint-indent (relative-to n &optional stream)
  (declare (ignore relative-to n stream))
  nil)
(defun pprint-tab (kind colnum colinc &optional stream)
  (declare (ignore kind colnum colinc stream))
  nil)

(defmacro pprint-logical-block ((stream-symbol object &key prefix per-line-prefix suffix) &body body)
  (let ((obj (gensym "OBJ")) (s (if (member stream-symbol '(t nil)) '*standard-output* stream-symbol)))
    `(let* ((,s (%output-stream ,(if (eq stream-symbol t) t s)))
            (,obj ,object))
       (when ,(or prefix per-line-prefix) (write-string ,(or prefix per-line-prefix) ,s))
       (block %pprint-block
         (macrolet ((pprint-pop () '(if (consp ,obj) (pop ,obj) (progn (when ,obj (write-string ". " ,s) (prin1 ,obj ,s)) (return-from %pprint-block nil))))
                    (pprint-exit-if-list-exhausted () '(when (null ,obj) (return-from %pprint-block nil))))
           ,@body))
       (when ,suffix (write-string ,suffix ,s))
       nil)))

(defun pprint-fill (stream list &optional (colon t) at)
  (declare (ignore at))
  (let ((s (%output-stream stream)))
    (if (and colon (listp list))
        (let ((*print-pretty* t)) (%write-object list s))
        (%write-object list s))))
(defun pprint-linear (stream list &optional (colon t) at) (pprint-fill stream list colon at))
(defun pprint-tabular (stream list &optional (colon t) at tabsize)
  (declare (ignore tabsize))
  (pprint-fill stream list colon at))

(defun copy-pprint-dispatch (&optional table) (declare (ignore table)) nil)
(defun pprint-dispatch (object &optional table) (declare (ignore object table)) (values nil nil))
(defun set-pprint-dispatch (type function &optional priority table)
  (declare (ignore type function priority table))
  nil)
