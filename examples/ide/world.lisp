;;;; world.lisp — the world: where Clysm IDE evaluates
;;;;
;;;; A world is the IDE's own image started again in a Web Worker
;;;; (contrib/world/worker.js boots it with the page's compiled module).
;;;; The image finds that it has no page (no `document`) and serves the IDE
;;;; (SERVE-WORLD) instead of showing it.  What the user evaluates runs here,
;;;; in another heap and another thread of the computer: a computation that
;;;; breaks the image, or one that never ends, leaves the IDE as it is, and
;;;; the IDE can kill the worker and start another world whatever it does.
;;;;
;;;; The IDE and the world talk with messages of the worker: objects
;;;; {type: "ide", text}, the TEXT an S-expression of strings, numbers,
;;;; keywords and lists (read with *READ-EVAL* off).  To the world:
;;;;
;;;;   (:eval :id N :source S :package P :label L :repl BOOL :offset O :file F)
;;;;   (:interrupt)
;;;;   (:restart :level N :index I)        I NIL: give up
;;;;   (:request :id N :op OP :args (...)) answered by (:reply :id N :value V)
;;;;
;;;; To the IDE:
;;;;
;;;;   (:ready)
;;;;   (:output TEXT)
;;;;   (:note :kind K :text TEXT)          K :warning, :note
;;;;   (:defined :name QUALIFIED :kind K :file F :offset O)
;;;;   (:debugger :level N :type T :message M :restarts (LABEL...) :abort I
;;;;              :backtrace (NAME...) :condition ID)
;;;;   (:debugger-end :level N)
;;;;   (:done :id N :ok BOOL :values ((PRINTED ID)...) :package P :crashed MESSAGE)
;;;;
;;;; An object of the world is known to the IDE by an ID (OBJECT-ID), which
;;;; :inspect and the debugger's condition use.
;;;;
;;;; How an evaluation is interrupted: its code is compiled with interrupt
;;;; checks (SYS::*INTERRUPT-CHECKS*), and at a check, every 40 ms, POLL
;;;; waits for the worker's event loop (JS:AWAIT): the messages of the IDE
;;;; are received then (:interrupt sets a flag that the next check sees),
;;;; and the output is sent.

(defpackage "CLYSM-IDE"
  (:use "CL" "UI"))

(in-package "CLYSM-IDE")

;;; ------------------------------------------------------------------
;;; Messages

(defun encode (form)
  (let ((*package* (find-package "KEYWORD"))
        (*print-pretty* nil) (*print-readably* nil) (*print-circle* nil)
        (*print-length* nil) (*print-level* nil) (*print-escape* t))
    (prin1-to-string form)))

(defun decode (text)
  "The S-expression of a message: only strings, numbers, keywords and lists
are read from it."
  (let ((*package* (find-package "KEYWORD"))
        (*read-eval* nil))
    (read-from-string text)))

(defun send-to-page (form)
  (js:call (js:global "self") "postMessage" (js:object "type" "ide" "text" (encode form))))

;;; ------------------------------------------------------------------
;;; Objects known to the IDE

(defvar *objects* (make-hash-table) "id -> an object of the world shown in the IDE")
(defvar *object-ids* (make-hash-table :test 'eq) "object -> id")
(defvar *next-object-id* 0)
(defparameter +object-limit+ 20000)

(defun object-id (object)
  (or (gethash object *object-ids*)
      (progn
        (when (>= (hash-table-count *objects*) +object-limit+)
          (clrhash *objects*) (clrhash *object-ids*))
        (let ((id (incf *next-object-id*)))
          (setf (gethash id *objects*) object
                (gethash object *object-ids*) id)
          id))))

(defun find-object (id)
  (multiple-value-bind (object found) (gethash id *objects*)
    (unless found (error "その値はもうありません"))
    object))

;;; ------------------------------------------------------------------
;;; Packages and printing

(defun package-named (name)
  (or (and name (find-package name)) (find-package "CL-USER")))

(defun short-package-name (package)
  (let ((name (package-name package)))
    (cond ((string= name "COMMON-LISP-USER") "CL-USER")
          ((string= name "COMMON-LISP") "CL")
          (t (or (find-if (lambda (n) (< (length n) (length name))) (package-nicknames package))
                 name)))))

(defun print-value (object &optional package)
  "OBJECT printed for the IDE, as read in PACKAGE (a name)."
  (let ((*print-length* 200) (*print-level* 12) (*print-circle* t) (*print-pretty* t)
        (*print-right-margin* 100)
        (*package* (package-named package)))
    (handler-case (prin1-to-string object)
      (error () (format nil "#<~A を表示できません>" (type-of object))))))

(defun one-line (string &optional (max 60))
  (let* ((s (string-trim '(#\Space #\Newline #\Tab) string))
         (newline (position #\Newline s))
         (s (if newline (concatenate 'string (subseq s 0 newline) " …") s)))
    (if (> (length s) max) (concatenate 'string (subseq s 0 max) "…") s)))

(defun qualified-name (symbol)
  (let ((package (symbol-package symbol)))
    (if package
        (format nil "~A::~A" (package-name package) (symbol-name symbol))
        (format nil "#:~A" (symbol-name symbol)))))

;;; ------------------------------------------------------------------
;;; Output: gathered, and sent when the worker's event loop runs

(defvar *output-buffer* (make-string-output-stream))
(defvar *output-pending* nil)

(defun flush-output ()
  (when *output-pending*
    (setf *output-pending* nil)
    (send-to-page (list :output (get-output-stream-string *output-buffer*)))))

(defclass world-output (gray:fundamental-character-output-stream)
  ((column :initform 0 :accessor output-column)))

(defmethod gray:stream-write-char ((stream world-output) char)
  (write-char char *output-buffer*)
  (setf (output-column stream) (if (char= char #\Newline) 0 (1+ (output-column stream))))
  (setf *output-pending* t)
  char)

(defmethod gray:stream-write-string ((stream world-output) string &optional (start 0) end)
  (let ((end (or end (length string))))
    (write-string string *output-buffer* :start start :end end)
    (let ((newline (position #\Newline string :start start :end end :from-end t)))
      (setf (output-column stream)
            (if newline (- end newline 1) (+ (output-column stream) (- end start)))))
    (setf *output-pending* t))
  string)

(defmethod gray:stream-line-column ((stream world-output)) (output-column stream))
(defmethod gray:stream-finish-output ((stream world-output)) (flush-output) nil)
(defmethod gray:stream-force-output ((stream world-output)) (flush-output) nil)

(defvar *world-output* (make-instance 'world-output))

(defun note (kind control &rest args)
  (flush-output)
  (send-to-page (list :note :kind kind :text (apply #'format nil control args))))

;;; ------------------------------------------------------------------
;;; Interrupts

(defvar *interrupt-requested* nil)
(defvar *last-yield* 0)

(defun timeout-promise (ms)
  (js:new (js:global "Promise")
          (lambda (resolve &rest more)
            (declare (ignore more))
            (js:call (js:global) "setTimeout" resolve ms)
            nil)))

(defun poll ()
  "Called by the code of an evaluation every so often: let the worker's event
loop run now and then, and stop if the IDE asked to."
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

;;; ------------------------------------------------------------------
;;; The debugger: the IDE shows it, and answers with the restart to invoke

(defvar *outer-restarts* '() "The restarts of outside the evaluation, not offered in its debugger.")
(defvar *debugger-level* 0)
(defvar *waiting* (make-hash-table) "level -> function of the answer of the IDE")

(defparameter +debugger-frames+
  '("INVOKE-DEBUGGER" "%INVOKE-DEBUGGER" "ERROR" "CERROR" "SIGNAL" "WARN" "%SIGNAL*" "%ERROR*"
    "WORLD-DEBUGGER" "POLL" "%INTERRUPT-POLL" "SIGNAL-ERROR"))

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

(defun wait-for-restart (level)
  "Wait until the IDE answers the debugger LEVEL; the index of the restart, or NIL."
  (let* ((answer nil)
         (promise (js:new (js:global "Promise")
                          (lambda (resolve &rest more)
                            (declare (ignore more))
                            (setf (gethash level *waiting*)
                                  (lambda (index)
                                    (setf answer index)
                                    (js:funcall resolve nil)))
                            nil))))
    (js:await promise)
    (remhash level *waiting*)
    answer))

(defun world-debugger (condition &optional hook)
  "Show CONDITION in the IDE with its restarts; invoke the one chosen there."
  (declare (ignore hook))
  (flush-output)
  (let* ((restarts (remove-if (lambda (r) (member r *outer-restarts*)) (compute-restarts condition)))
         (abort (find-restart 'abort condition)))
    (if (not (js:can-await-p))
        (progn (note :error "~A" condition) (invoke-restart abort))
        (let* ((level (incf *debugger-level*))
               (index (progn
                        (send-to-page
                         (list :debugger :level level
                                         :type (string-downcase (princ-to-string (type-of condition)))
                                         :message (or (ignore-errors (princ-to-string condition))
                                                      "（表示できない状態）")
                                         :restarts (mapcar (lambda (r)
                                                             (or (ignore-errors (princ-to-string r))
                                                                 (string (restart-name r))))
                                                           restarts)
                                         :abort (position abort restarts)
                                         :backtrace (backtrace)
                                         :condition (object-id condition)))
                        (wait-for-restart level))))
          (send-to-page (list :debugger-end :level level))
          (let ((restart (and (integerp index) (nth index restarts))))
            (if restart
                (invoke-restart-interactively restart)
                (invoke-restart abort)))))))

(defun answer-debugger (level index)
  (let ((answer (gethash level *waiting*)))
    (when answer (funcall answer index))))

;;; ------------------------------------------------------------------
;;; Evaluation

(defun note-warning (warning)
  (note :warning "; ~:[警告~;注意~]: ~A" (typep warning 'style-warning) warning)
  (muffle-warning warning))

(defun definition-name (form)
  "If FORM defines a named thing, its name and the kind (\"defun\"...)."
  (when (and (consp form) (symbolp (car form)) (consp (cdr form)))
    (let ((op (symbol-name (car form))))
      (when (and (> (length op) 3) (string= op "DEF" :end1 3))
        (let ((name (second form)))
          (cond ((symbolp name) (values name (string-downcase op)))
                ((and (consp name) (eq (car name) 'setf)) (values (second name) (string-downcase op)))
                ((and (consp name) (symbolp (car name))) (values (car name) (string-downcase op)))))))))

(defun form-start (stream)
  "The position of the next form of STREAM (after blanks and comments)."
  (loop
    (let ((c (peek-char nil stream nil nil)))
      (cond ((null c) (return (file-position stream)))
            ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (read-char stream))
            ((char= c #\;) (read-line stream nil))
            (t (return (file-position stream)))))))

(defun evaluate-forms (source &key repl (offset 0) file)
  "Read and evaluate the forms of the string SOURCE in *PACKAGE*; return the
values of the last."
  (let ((values '()))
    (with-input-from-string (in source)
      (loop
        (let* ((start (form-start in))
               (form (read in nil in)))
          (when (eq form in) (return))
          (setf values (multiple-value-list (sys::%eval-toplevel form)))
          (when file
            (multiple-value-bind (name kind) (definition-name form)
              (when name
                (send-to-page (list :defined :name (qualified-name name) :kind kind
                                             :file file :offset (+ offset start))))))
          (when repl
            (setf /// // // / / values *** ** ** * * (first values))))))
    values))

(defun run-evaluation (thunk package)
  "Call THUNK with the world's output, debugger and interrupts; its values
and T, or NIL and NIL if it was given up."
  (let ((*package* (package-named package))
        (*standard-output* *world-output*)
        (*error-output* *world-output*)
        (*trace-output* *world-output*)
        (*standard-input* (make-string-input-stream ""))
        (*debugger-hook* nil)
        (sys::*invoke-debugger-hook* #'world-debugger)
        (sys::*interrupt-checks* t)
        (sys::*interrupt-poll-function* #'poll)
        (*outer-restarts* (compute-restarts)))
    (setf *interrupt-requested* nil
          *last-yield* (get-internal-real-time))
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
      (setf (output-column *world-output*) 0))))

(defun evaluate (&key id source package repl (offset 0) file label)
  "The message :eval: evaluate SOURCE as a computation of its own."
  (declare (ignore label))
  (let ((promise
          (js:async
           (lambda ()
             (let ((new-package package))
               (multiple-value-bind (values ok)
                   (run-evaluation (lambda ()
                                     (prog1 (evaluate-forms source :repl repl :offset offset :file file)
                                       (when repl (setf new-package (short-package-name *package*)))))
                                   package)
                 (send-to-page
                  (list :done :id id :ok ok :package new-package
                              :values (and ok (mapcar (lambda (v)
                                                        (list (print-value v new-package) (object-id v)))
                                                      values))))))))))
    ;; the stack exhausted: the computation died
    (js:call promise "catch"
             (lambda (error)
               (flush-output)
               (let ((message (ignore-errors (js:get error "message"))))
                 (send-to-page
                  (list :done :id id :ok nil :package package
                              :crashed (if (and (stringp message) (search "stack" message))
                                           "スタックが尽きました（再帰が深すぎます）"
                                           (if (stringp message) message "JavaScript の例外")))))
               nil))))

;;; ------------------------------------------------------------------
;;; Symbols: completion, argument lists, documentation

(defun symbol-char-p (char)
  (not (or (member char '(#\( #\) #\' #\` #\, #\" #\; #\Space #\Tab #\Newline #\#))
           (char= char #\|))))

(defun parse-symbol-name (string)
  "The package part (or NIL), whether it is internal (::), and the name part of STRING."
  (let ((colon (position #\: string)))
    (cond ((null colon) (values nil nil string))
          ((zerop colon) (values "KEYWORD" nil (string-left-trim ":" string)))
          (t (let ((internal (and (< (1+ colon) (length string)) (char= (char string (1+ colon)) #\:))))
               (values (subseq string 0 colon) internal
                       (subseq string (+ colon (if internal 2 1)))))))))

(defun find-symbol* (string package)
  "The symbol STRING names in PACKAGE (as the reader would read it), or NIL."
  (multiple-value-bind (package-part internal name) (parse-symbol-name string)
    (declare (ignore internal))
    (let ((home (if package-part (find-package (string-upcase package-part)) package)))
      (when home
        (multiple-value-bind (symbol status) (find-symbol (string-upcase name) home)
          (and status symbol))))))

(defun completions (prefix package &key (limit 200))
  "The completions of the symbol name PREFIX in PACKAGE: a list of strings."
  (multiple-value-bind (package-part internal name) (parse-symbol-name prefix)
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
      (let ((parts '()) (i 0) (mode :positional) (marked nil))
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

;;; ------------------------------------------------------------------
;;; The inspector: the parts of an object

(defun mop (name) (find-symbol name "CLYSM-MOP"))

(defun object-slots (object)
  (let ((class (class-of object)))
    (ignore-errors
     (loop for slot in (funcall (mop "CLASS-SLOTS") class)
           collect (let ((name (funcall (mop "SLOT-DEFINITION-NAME") slot)))
                     (cons (string-downcase (symbol-name name))
                           (if (slot-boundp object name) (slot-value object name) '|#<unbound>|)))))))

(defun object-parts (object)
  "((label . value)...) of OBJECT."
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
    (number (list (cons "型" (type-of object))))
    (character (list (cons "コード" (char-code object)) (cons "名前" (char-name object))))
    (function '())
    (t (object-slots object))))

(defun inspect-description (id)
  "(:id :title :printed :parts ((label printed id)...)) of the object ID."
  (let ((object (find-object id)))
    (list :id id
          :title (string-downcase (princ-to-string (type-of object)))
          :printed (one-line (print-value object) 400)
          :parts (loop for (label . value) in (object-parts object)
                       collect (list label (one-line (print-value value) 120) (object-id value))))))

;;; ------------------------------------------------------------------
;;; Requests of the IDE

(defun symbol-named (name package)
  (find-symbol* name (package-named package)))

(defun answer (op args)
  (ecase op
    (:complete (destructuring-bind (prefix package) args
                 (completions prefix (package-named package))))
    (:arglist (destructuring-bind (name package index) args
                (let ((symbol (symbol-named name package)))
                  (and symbol (or (fboundp symbol) (special-operator-p symbol))
                       (arglist-string symbol index)))))
    (:describe (destructuring-bind (name package) args
                 (let ((symbol (symbol-named name package)))
                   (if symbol
                       (with-output-to-string (out)
                         (handler-case (describe symbol out)
                           (error (e) (format out "~A" e))))
                       (format nil "~A は知られていません" name)))))
    (:macroexpand (destructuring-bind (source package) args
                    (let ((*package* (package-named package)))
                      (handler-case
                          (let ((*print-case* :downcase) (*print-pretty* t))
                            (prin1-to-string (macroexpand-1 (read-from-string source))))
                        (error (e) (format nil "; エラー: ~A" e))))))
    (:inspect (inspect-description (first args)))
    (:definition (destructuring-bind (name package) args
                   (let ((symbol (symbol-named name package)))
                     (when symbol
                       (list (qualified-name symbol)
                             (loop for (dspec file position) in (ignore-errors (sys::%definition-sources symbol))
                                   when file
                                     collect (list (string-downcase (princ-to-string (if (consp dspec) (car dspec) dspec)))
                                                   (namestring file) position)))))))
    (:chdir (let ((root (first args)))
              (ignore-errors
               (let ((process (js:global "process")))
                 (when (js:js-value-p process) (js:call process "chdir" root))))
              (setf *default-pathname-defaults* (pathname root))
              t))
    (:forget (clrhash *objects*) (clrhash *object-ids*) t)))

(defun receive-from-page (form)
  (case (first form)
    (:eval (apply #'evaluate (rest form)))
    (:interrupt (setf *interrupt-requested* t))
    (:restart (destructuring-bind (&key level index) (rest form)
                (answer-debugger level index)))
    (:request (destructuring-bind (&key id op args) (rest form)
                (send-to-page
                 (list :reply :id id
                              :value (handler-case (answer op args)
                                       (error (e) (list :error (princ-to-string e))))))))))

(defun world-p ()
  "True in a world: an image in a worker, without a page."
  (let ((document (ignore-errors (js:global "document"))))
    (not (and document (js:js-value-p document)))))

(defun serve-world ()
  "In a worker: receive the messages of the IDE, and tell it that the world is ready."
  (js:call (js:global "self") "addEventListener" "message"
           (lambda (event)
             (let ((data (js:get event "data")))
               (when (equal (js:get data "type") "ide")
                 (handler-case (receive-from-page (decode (js:get data "text")))
                   (error (e)
                     (js:call (js:global "console") "error" (format nil "world: ~A" e))))))
             nil))
  (send-to-page '(:ready)))
