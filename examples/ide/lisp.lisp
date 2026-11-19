;;;; lisp.lisp — what the IDE knows of Lisp: evaluation, the debugger,
;;;; interrupts, completion, argument lists, definitions and the inspector
;;;;
;;;; The IDE evaluates in its own image (it is built with the compiler,
;;;; tools/build-repl.lisp): a form is compiled to WebAssembly, like at
;;;; Clysm's REPL.  An evaluation is a computation of its own (JS:ASYNC), and
;;;; the code it compiles checks for interrupts (SYS::*INTERRUPT-CHECKS*):
;;;; at a check, now and then, it lets the page run (JS:AWAIT), so that the
;;;; IDE is shown and works while a long computation runs, and the button
;;;; "中断" can stop it.  An error, or an interrupt, waits in the debugger of
;;;; the IDE for a restart to be chosen (UI:WAIT-FOR-ANSWER).
;;;;
;;;; Nothing here draws: ide.lisp shows the log, the debugger and the inspector.

(defpackage "CLYSM-IDE"
  (:use "CL" "UI"))

(in-package "CLYSM-IDE")

;;; ------------------------------------------------------------------
;;; The log of the REPL: entries, which ide.lisp shows

(defmodel entry ()
  ((kind :initarg :kind :reader entry-kind)          ; :input :output :values :error :note :warning
   (text :initarg :text :initform "" :accessor entry-text)
   (objects :initarg :objects :initform nil :reader entry-objects)
   (package :initarg :package :initform nil :reader entry-package)))

(defstate *log* '() "The entries of the REPL, the latest first.")
(defparameter +log-limit+ 400 "The number of entries kept.")
(defparameter +output-limit+ 200000 "The characters kept of one output.")

(defun add-entry (kind &rest initargs)
  (flush-output)
  (let ((entry (apply #'make-instance 'entry :kind kind initargs)))
    (setf *log* (let ((log (cons entry *log*)))
                  (if (> (length log) +log-limit+) (subseq log 0 +log-limit+) log)))
    entry))

(defun note (kind control &rest args)
  "Add a line of KIND (:note, :error, :warning) to the log."
  (add-entry kind :text (apply #'format nil control args)))

(defun clear-log ()
  (flush-output)
  (setf *log* '()))

;;; The output of an evaluation goes to an entry :output, in pieces: what
;;; is written is gathered in a buffer, and put into the entry when the page
;;; can show it (FLUSH-OUTPUT).

(defvar *output-entry* nil)
(defvar *output-buffer* (make-string-output-stream))
(defvar *output-pending* nil)

(defun flush-output ()
  (when *output-pending*
    (setf *output-pending* nil)
    (let ((text (get-output-stream-string *output-buffer*)))
      (if (and *output-entry* (eq *output-entry* (first *log*)))
          (let ((new (concatenate 'string (entry-text *output-entry*) text)))
            (setf (entry-text *output-entry*)
                  (if (> (length new) +output-limit+)
                      (concatenate 'string "…" (subseq new (- (length new) +output-limit+)))
                      new)))
          (let ((entry (make-instance 'entry :kind :output :text text)))
            (setf *output-entry* entry)
            (setf *log* (cons entry *log*)))))))

(defclass ide-output (gray:fundamental-character-output-stream)
  ((column :initform 0 :accessor output-column)))

(defmethod gray:stream-write-char ((stream ide-output) char)
  (write-char char *output-buffer*)
  (setf (output-column stream) (if (char= char #\Newline) 0 (1+ (output-column stream))))
  (setf *output-pending* t)
  char)

(defmethod gray:stream-write-string ((stream ide-output) string &optional (start 0) end)
  (let ((end (or end (length string))))
    (write-string string *output-buffer* :start start :end end)
    (let ((newline (position #\Newline string :start start :end end :from-end t)))
      (setf (output-column stream)
            (if newline (- end newline 1) (+ (output-column stream) (- end start)))))
    (setf *output-pending* t))
  string)

(defmethod gray:stream-line-column ((stream ide-output)) (output-column stream))
(defmethod gray:stream-finish-output ((stream ide-output)) (flush-output) nil)
(defmethod gray:stream-force-output ((stream ide-output)) (flush-output) nil)

(defvar *ide-output* (make-instance 'ide-output))

;;; ------------------------------------------------------------------
;;; Evaluation

(defstate *busy* nil "While an evaluation runs, what it evaluates (a string).")
(defstate *repl-package* "CL-USER" "The package of the REPL.")
(defstate *debuggers* '() "The debuggers waiting for a restart, the innermost first.")
(defvar *interrupt-requested* nil)
(defvar *last-yield* 0)
(defvar *evaluation-depth* 0)
(defvar *outer-restarts* '() "The restarts of outside the evaluation, not offered in its debugger.")

(defun package-named (name)
  (or (and name (find-package name)) (find-package "CL-USER")))

(defun short-package-name (package)
  (let ((name (package-name package)))
    (cond ((string= name "COMMON-LISP-USER") "CL-USER")
          ((string= name "COMMON-LISP") "CL")
          (t (or (find-if (lambda (n) (< (length n) (length name))) (package-nicknames package))
                 name)))))

(defun timeout-promise (ms)
  (js:new (js:global "Promise")
          (lambda (resolve &rest more)
            (declare (ignore more))
            (js:call (js:global) "setTimeout" resolve ms)
            nil)))

(defun poll ()
  "Called by the code of an evaluation every so often: let the page run now
and then, and stop if the user asked to."
  (let ((now (get-internal-real-time)))
    (when (> (- now *last-yield*) 40)
      (flush-output)
      (when (js:can-await-p)
        (js:await (timeout-promise 0)))
      (setf *last-yield* (get-internal-real-time))))
  (when *interrupt-requested*
    (setf *interrupt-requested* nil)
    (with-simple-restart (continue "評価を続ける")
      (invoke-debugger (make-condition 'simple-condition :format-control "中断しました")))))

(defun interrupt-evaluation ()
  (when *busy* (setf *interrupt-requested* t)))

;;; The debugger

(defstruct debugger condition restarts backtrace answer level)

(defparameter +debugger-frames+
  '("INVOKE-DEBUGGER" "%INVOKE-DEBUGGER" "ERROR" "CERROR" "SIGNAL" "WARN" "%SIGNAL*" "%ERROR*"
    "IDE-DEBUGGER" "POLL" "%INTERRUPT-POLL" "SIGNAL-ERROR"))

(defparameter +evaluator-frames+
  '("EVALUATE-FORMS" "%EVAL-TOPLEVEL" "RUN-EVALUATION"))

(defun frame-short-name (name)
  (subseq name (1+ (or (position #\: name :from-end t) -1))))

(defun backtrace ()
  "The names of the functions on the stack, of the evaluation only."
  (let* ((names (ignore-errors (sys::%backtrace-names 200)))
         (last (position-if (lambda (n) (member (frame-short-name n) +debugger-frames+ :test #'string-equal))
                            names :from-end t))
         (names (if last (nthcdr (1+ last) names) names))
         (end (position-if (lambda (n) (member (frame-short-name n) +evaluator-frames+ :test #'string-equal))
                           names)))
    (if end (subseq names 0 end) names)))

(defun ide-debugger (condition &optional hook)
  "Show CONDITION with its restarts, and invoke the one the user chooses."
  (declare (ignore hook))
  (flush-output)
  (let ((restarts (remove-if (lambda (r) (member r *outer-restarts*)) (compute-restarts condition))))
    (if (not (js:can-await-p))
        ;; nobody can answer: give up the evaluation
        (progn (note :error "~A" condition)
               (invoke-restart (find-restart 'abort condition)))
        (let* ((debugger (make-debugger :condition condition :restarts restarts
                                        :backtrace (backtrace) :level (1+ (length *debuggers*))))
               (choice (wait-for-answer
                        (lambda (answer)
                          (setf (debugger-answer debugger) answer)
                          (push debugger *debuggers*)))))
          (setf *debuggers* (remove debugger *debuggers*))
          (if choice
              (invoke-restart-interactively choice)
              (invoke-restart (find-restart 'abort condition)))))))

(defun choose-restart* (debugger restart)
  (let ((answer (debugger-answer debugger)))
    (when answer (funcall answer restart))))

(defun condition-type-name (condition)
  (string-downcase (princ-to-string (type-of condition))))

;;; Warnings of the compiler are notes in the log

(defun note-warning (warning)
  (note :warning "; ~:[警告~;注意~]: ~A" (typep warning 'style-warning) warning)
  (muffle-warning warning))

;;; Running forms

(defvar *definition-hook* nil
  "Called with the name, the kind and the form of each definition evaluated
(and the offset in its text), to remember where it is.")

(defun definition-name (form)
  "If FORM defines a named thing, its name and the kind (\"defun\"...)."
  (when (and (consp form) (symbolp (car form)) (consp (cdr form)))
    (let ((op (symbol-name (car form))))
      (when (and (> (length op) 3) (string= op "DEF" :end1 3))
        (let ((name (second form)))
          (cond ((symbolp name) (values name (string-downcase op)))
                ((and (consp name) (eq (car name) 'setf)) (values (second name) (string-downcase op)))
                ((and (consp name) (symbolp (car name))) (values (car name) (string-downcase op)))))))))

(defun evaluate-forms (source &key echo-values (offset 0) on-definition)
  "Read and evaluate the forms of the string SOURCE in *PACKAGE*; return the
values of the last."
  (let ((values '()))
    (with-input-from-string (in source)
      (loop
        (let* ((start (sys-form-start in))
               (form (read in nil in)))
          (when (eq form in) (return))
          (setf values (multiple-value-list (sys::%eval-toplevel form)))
          (when on-definition
            (multiple-value-bind (name kind) (definition-name form)
              (when name (funcall on-definition name kind (+ offset start)))))
          (when echo-values
            (setf /// // // / / values *** ** ** * * (first values))))))
    values))

(defun sys-form-start (stream)
  "The position of the next form of STREAM (after blanks and comments)."
  (loop
    (let ((c (peek-char nil stream nil nil)))
      (cond ((null c) (return (file-position stream)))
            ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (read-char stream))
            ((char= c #\;) (read-line stream nil))
            (t (return (file-position stream)))))))

(defun run-evaluation (thunk &key label package)
  "Call THUNK as an evaluation: with the IDE's output, debugger and
interrupts.  Return its values, or NIL if it was given up."
  (let ((*package* (package-named package))
        (*standard-output* *ide-output*)
        (*error-output* *ide-output*)
        (*trace-output* *ide-output*)
        (*standard-input* (make-string-input-stream ""))
        (*debugger-hook* nil)
        (sys::*invoke-debugger-hook* #'ide-debugger)
        (sys::*interrupt-checks* t)
        (sys::*interrupt-poll-function* #'poll)
        (*evaluation-depth* (1+ *evaluation-depth*))
        (*outer-restarts* (compute-restarts)))
    (setf *busy* label
          *interrupt-requested* nil
          *last-yield* (get-internal-real-time))
    ;; (an evaluation in the REPL while the debugger waits is one inside another)
    (unwind-protect
         (restart-case
             (handler-bind ((warning #'note-warning)
                            (serious-condition (lambda (c) (invoke-debugger c))))
               (values (funcall thunk) t))
           (abort ()
             :report "評価をやめる"
             (note :note "; 評価をやめました")
             (values nil nil)))
      (flush-output)
      (fresh-output-line)
      (setf *busy* (and *debuggers* "デバッガ")))))

(defun fresh-output-line ()
  (setf (output-column *ide-output*) 0))

(defun start-evaluation (function)
  "Run FUNCTION as a computation of its own (it may wait); if it dies (the
stack exhausted), say so."
  (let ((promise (js:async function)))
    (js:call promise "catch"
             (lambda (error)
               (setf *busy* (and *debuggers* "デバッガ"))
               (flush-output)
               (let ((message (ignore-errors (js:get error "message"))))
                 (note :error "; 評価が止まりました: ~A"
                       (if (and (stringp message) (search "stack" message))
                           "スタックが尽きました（再帰が深すぎます）"
                           (or message "JavaScript の例外"))))
               nil))
    promise))

(defun evaluate-in-repl (source)
  "Evaluate the input SOURCE of the REPL."
  (add-entry :input :text source :package *repl-package*)
  (start-evaluation
   (lambda ()
     (multiple-value-bind (values ok)
         (run-evaluation (lambda ()
                           (prog1 (evaluate-forms source :echo-values t)
                             (setf *repl-package* (short-package-name *package*))))
                         :label (one-line source) :package *repl-package*)
       (when ok (add-entry :values :objects values :package *repl-package*))))))

(defun evaluate-source (source &key package label on-definition (offset 0) on-done (show-values t))
  "Evaluate SOURCE, the text of a buffer, in PACKAGE (a name)."
  (add-entry :eval :text label :package package)
  (start-evaluation
   (lambda ()
     (multiple-value-bind (values ok)
         (run-evaluation (lambda () (evaluate-forms source :offset offset :on-definition on-definition))
                         :label label :package package)
       (when (and ok show-values) (add-entry :values :objects values :text label :package package))
       (when on-done (funcall on-done values ok))))))

(defun one-line (string &optional (max 60))
  (let* ((s (string-trim '(#\Space #\Newline #\Tab) string))
         (newline (position #\Newline s))
         (s (if newline (concatenate 'string (subseq s 0 newline) " …") s)))
    (if (> (length s) max) (concatenate 'string (subseq s 0 max) "…") s)))

(defun print-value (object &optional package)
  "OBJECT printed for the log, as read in PACKAGE (a name)."
  (let ((*print-length* 200) (*print-level* 12) (*print-circle* t)
        (*package* (package-named (or package *repl-package*))))
    (handler-case (prin1-to-string object)
      (error () (format nil "#<~A を表示できません>" (type-of object))))))

;;; ------------------------------------------------------------------
;;; Input that is complete

(defun complete-input-p (source)
  "True unless SOURCE ends inside a form."
  (let ((*read-eval* nil) (*read-suppress* t))
    (with-input-from-string (s source)
      (handler-case (loop (when (eq (read s nil s) s) (return t)))
        (end-of-file () nil)
        (error () t)))))

;;; ------------------------------------------------------------------
;;; Symbols: completion, argument lists, documentation

(defun symbol-char-p (char)
  (not (or (member char '(#\( #\) #\' #\` #\, #\" #\; #\Space #\Tab #\Newline #\#))
           (char= char #\|))))

(defun parse-symbol-name (string package)
  "The package part (or NIL), whether it is internal (::), and the name part of STRING."
  (declare (ignore package))
  (let ((colon (position #\: string)))
    (cond ((null colon) (values nil nil string))
          ((zerop colon) (values "KEYWORD" nil (string-left-trim ":" string)))
          (t (let ((internal (and (< (1+ colon) (length string)) (char= (char string (1+ colon)) #\:))))
               (values (subseq string 0 colon) internal
                       (subseq string (+ colon (if internal 2 1)))))))))

(defun find-symbol* (string package)
  "The symbol STRING names in PACKAGE (as the reader would read it), or NIL."
  (multiple-value-bind (package-part internal name) (parse-symbol-name string package)
    (declare (ignore internal))
    (let ((home (if package-part (find-package (string-upcase package-part)) package)))
      (when home
        (multiple-value-bind (symbol status) (find-symbol (string-upcase name) home)
          (and status symbol))))))

(defun completions (prefix package &key (limit 200))
  "The completions of the symbol name PREFIX in PACKAGE: a list of strings."
  (multiple-value-bind (package-part internal name) (parse-symbol-name prefix package)
    (let* ((up (string-upcase name))
           (home (if package-part (find-package (string-upcase package-part)) package))
           (results '())
           (seen (make-hash-table :test 'equal)))
      (when home
        (flet ((consider (symbol)
                 (let ((n (symbol-name symbol)))
                   (when (and (>= (length n) (length up)) (string= up n :end2 (length up))
                              (not (gethash n seen)))
                     (setf (gethash n seen) t)
                     (push symbol results)))))
          (cond ((null package-part) (do-symbols (s home) (consider s)))
                (internal (do-symbols (s home) (when (eq (symbol-package s) home) (consider s))))
                (t (do-external-symbols (s home) (consider s))))))
      (let ((names (mapcar (lambda (s)
                             (let ((n (string-downcase (symbol-name s))))
                               (cond ((string= (or package-part "") "KEYWORD") (concatenate 'string ":" n))
                                     (package-part (concatenate 'string package-part (if internal "::" ":") n))
                                     (t n))))
                           results)))
        (let ((sorted (sort names (lambda (a b) (or (< (length a) (length b))
                                                    (and (= (length a) (length b)) (string< a b)))))))
          (if (> (length sorted) limit) (subseq sorted 0 limit) sorted))))))

(defun special-operator-arglist (name)
  (case name
    ((progn) '(&rest forms))
    ((if) '(test then &optional else))
    ((let let*) '((&rest bindings) &body body))
    ((flet labels macrolet) '((&rest definitions) &body body))
    ((block) '(name &rest forms))
    ((return-from) '(name &optional value))
    ((setq) '(&rest pairs))
    ((quote function) '(thing))
    ((the) '(type form))
    ((unwind-protect) '(protected &body cleanup))
    ((catch) '(tag &body body))
    ((throw) '(tag result))
    ((tagbody) '(&rest statements))
    ((go) '(tag))
    ((eval-when) '((&rest situations) &body body))
    ((multiple-value-call) '(function &rest forms))
    ((multiple-value-prog1) '(first-form &rest forms))
    ((locally) '(&body body))
    ((symbol-macrolet) '((&rest bindings) &body body))
    ((progv) '(symbols values &body body))
    ((load-time-value) '(form &optional read-only-p))
    (t :unknown)))

(defun arglist (symbol)
  "The lambda list of the function or macro SYMBOL, or :UNKNOWN."
  (cond ((special-operator-p symbol) (special-operator-arglist symbol))
        ((macro-function symbol)
         (multiple-value-bind (indicator ll tail)
             (get-properties (symbol-plist symbol) '(sys::%macro-lambda-list))
           (declare (ignore indicator))
           (cond (tail ll)
                 ((get symbol 'sys::%arglist))
                 (t :unknown))))
        ((fboundp symbol)
         (let ((f (fdefinition symbol)))
           (cond ((typep f 'generic-function)
                  (or (ignore-errors (funcall (find-symbol "GENERIC-FUNCTION-LAMBDA-LIST" "CLYSM-MOP") f))
                      :unknown))
                 ((get symbol 'sys::%arglist))
                 ((get symbol 'sys::%lambda-source) (third (get symbol 'sys::%lambda-source)))
                 (t :unknown))))
        (t :unknown)))

(defun arg-text (x)
  "X of a lambda list, without packages."
  (typecase x
    (null "()")
    (keyword (format nil ":~(~A~)" (symbol-name x)))
    (symbol (string-downcase (symbol-name x)))
    (cons (with-output-to-string (out)
            (write-char #\( out)
            (loop for tail = x then (cdr tail)
                  while (consp tail)
                  do (write-string (arg-text (car tail)) out)
                     (when (cdr tail) (write-char #\Space out))
                  finally (when tail (format out ". ~A" (arg-text tail))))
            (write-char #\) out)))
    (t (prin1-to-string x))))

(defun arglist-string (symbol &optional (argument -1))
  "\"(name args...)\" for the status line, with the ARGUMENT-th argument (from
0) in [ ], or NIL if the argument list is not known."
  (let ((list (arglist symbol)))
    (unless (eq list :unknown)
      (let ((*print-case* :downcase) (*print-pretty* nil)
            (*package* (or (symbol-package symbol) *package*))
            (parts '()) (i 0) (mode :positional) (marked nil))
        (loop for tail = (if (listp list) list (list list)) then (cdr tail)
              while (consp tail)
              do (let ((arg (car tail)))
                   (cond ((member arg lambda-list-keywords)
                          (setf mode (case arg ((&rest &body) :rest) ((&key &aux &allow-other-keys) :key) (t mode)))
                          (push (arg-text arg) parts))
                         (t
                          (let ((text (arg-text arg)))
                            (push (if (and (not marked) (>= argument 0)
                                           (or (and (eq mode :positional) (= i argument))
                                               (and (eq mode :rest) (>= argument i))))
                                      (progn (setf marked t) (format nil "[~A]" text))
                                      text)
                                  parts)
                            (when (eq mode :positional) (incf i))))))
              finally (when (and tail (atom tail)) (push (format nil ". ~A" (arg-text tail)) parts)))
        (format nil "(~(~A~)~{ ~A~})" (symbol-name symbol) (nreverse parts))))))

(defun describe-to-string (symbol)
  (with-output-to-string (out)
    (handler-case (describe symbol out)
      (error (e) (format out "~A" e)))))

;;; ------------------------------------------------------------------
;;; The inspector: the parts of an object, ((label . value) ...)

(defun mop (name) (find-symbol name "CLYSM-MOP"))

(defun object-slots (object)
  (let ((class (class-of object)))
    (ignore-errors
     (loop for slot in (funcall (mop "CLASS-SLOTS") class)
           collect (let ((name (funcall (mop "SLOT-DEFINITION-NAME") slot)))
                     (cons (string-downcase (symbol-name name))
                           (if (slot-boundp object name) (slot-value object name) '|#<unbound>|)))))))

(defun inspect-parts (object)
  "A description of OBJECT and its parts: (title . ((label . value)...))."
  (let ((title (format nil "~A" (string-downcase (princ-to-string (type-of object))))))
    (cons title
          (typecase object
            (null '())
            (cons (if (and (listp (cdr (last object))) (ignore-errors (list-length object)))
                      (loop for x in object for i from 0 below 500 collect (cons (princ-to-string i) x))
                      (list (cons "car" (car object)) (cons "cdr" (cdr object)))))
            (string (list (cons "長さ" (length object))))
            (vector (loop for x across object for i from 0 below 500 collect (cons (princ-to-string i) x)))
            (array (list (cons "次元" (array-dimensions object))))
            (hash-table
             (append (list (cons "test" (hash-table-test object)) (cons "件数" (hash-table-count object)))
                     (let ((parts '()) (i 0))
                       (block nil
                         (maphash (lambda (k v)
                                    (when (> (incf i) 500) (return))
                                    (push (cons (print-value k) v) parts))
                                  object))
                       (nreverse parts))))
            (symbol
             (remove nil
                     (list (cons "名前" (symbol-name object))
                           (cons "パッケージ" (symbol-package object))
                           (and (boundp object) (cons "値" (symbol-value object)))
                           (and (fboundp object) (cons "関数" (fdefinition object)))
                           (and (symbol-plist object) (cons "属性リスト" (symbol-plist object))))))
            (package
             (list (cons "名前" (package-name object))
                   (cons "別名" (package-nicknames object))
                   (cons "use" (package-use-list object))))
            (function
             (list (cons "名前" (ignore-errors (nth-value 2 (function-lambda-expression object))))))
            (number (list (cons "型" (type-of object))))
            (character (list (cons "コード" (char-code object)) (cons "名前" (char-name object))))
            (t (object-slots object))))))

;;; ------------------------------------------------------------------
;;; Definitions: where a name is defined

(defvar *definitions* (make-hash-table :test 'eq)
  "symbol -> ((kind file offset) ...): the definitions evaluated in the IDE.")

(defun remember-definition (symbol kind file offset)
  (setf (gethash symbol *definitions*)
        (cons (list kind file offset)
              (remove-if (lambda (d) (and (equal (first d) kind) (equal (second d) file)))
                         (gethash symbol *definitions*)))))

(defun definition-places (symbol)
  "((kind file offset)...) of SYMBOL: those evaluated in the IDE, then those of the image."
  (append (gethash symbol *definitions*)
          (loop for (dspec file position) in (ignore-errors (sys::%definition-sources symbol))
                when file
                  collect (list (string-downcase (princ-to-string (if (consp dspec) (car dspec) dspec)))
                                (namestring file) position))))
