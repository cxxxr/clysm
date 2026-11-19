;;;; client.lisp — the IDE's side of the world (world.lisp)
;;;;
;;;; The IDE starts a world (a worker with its own image), sends it what to
;;;; evaluate and what to ask, and keeps what comes back: the log of the
;;;; REPL, whether an evaluation runs, the debuggers waiting for a restart.
;;;; The objects of the world are known here by their printed text and an id.

(in-package "CLYSM-IDE")

;;; ------------------------------------------------------------------
;;; The log of the REPL: entries, which ide.lisp shows

(defmodel entry ()
  ((kind :initarg :kind :reader entry-kind)          ; :input :eval :output :values :error :note :warning
   (text :initarg :text :initform "" :accessor entry-text)
   (objects :initarg :objects :initform nil :reader entry-objects) ; ((printed id)...)
   (package :initarg :package :initform nil :reader entry-package)))

(defstate *log* '() "The entries of the REPL, the latest first.")
(defparameter +log-limit+ 400 "The number of entries kept.")
(defparameter +output-limit+ 200000 "The characters kept of one output.")

(defun add-entry (kind &rest initargs)
  (let ((entry (apply #'make-instance 'entry :kind kind initargs)))
    (setf *log* (let ((log (cons entry *log*)))
                  (if (> (length log) +log-limit+) (subseq log 0 +log-limit+) log)))
    entry))

(defun add-output (text)
  "TEXT written by an evaluation: at the end of the last output, if it is the last entry."
  (let ((last (first *log*)))
    (if (and last (eq (entry-kind last) :output))
        (let ((new (concatenate 'string (entry-text last) text)))
          (setf (entry-text last)
                (if (> (length new) +output-limit+)
                    (concatenate 'string "…" (subseq new (- (length new) +output-limit+)))
                    new)))
        (add-entry :output :text text))))

(defun clear-log ()
  (setf *log* '())
  (world-request :forget '() nil))

;;; ------------------------------------------------------------------
;;; The world

(defstate *world-state* :none "The state of the world: :starting, :ready, :failed or :none.")
(defstate *world-seconds* nil "How long the world took to start.")
(defvar *worker* nil)
(defvar *world-queue* '() "The messages to send when the world is ready, the latest first.")
(defvar *world-started* 0)
(defvar *world-hooks* '() "Functions called when a world is ready.")

(defstate *busy* nil "While an evaluation runs, what it evaluates (a string).")
(defstate *message* nil "The message of the status line.")
(defstate *repl-package* "CL-USER" "The package of the REPL.")
(defstate *debuggers* '() "The debuggers waiting for a restart (plists), the innermost first.")

(defvar *next-id* 0)
(defvar *evaluations* (make-hash-table) "id -> (label . function of the end) of the evaluations running")
(defvar *requests* (make-hash-table) "id -> function of the answer")

(defun send-to-world (form)
  (if (and *worker* (eq *world-state* :ready))
      (js:call *worker* "postMessage" (js:object "type" "ide" "text" (encode form)))
      (push form *world-queue*)))

(defun start-world ()
  "Start a world: the image of this page in a worker."
  (let ((worker (js:new (js:global "Worker") "world-worker.js")))
    (setf *worker* worker
          *world-state* :starting
          *world-seconds* nil
          *world-started* (get-internal-real-time))
    (js:call worker "addEventListener" "message"
             (lambda (event)
               (when (eq worker *worker*)
                 (handler-case (receive-from-world (js:get event "data"))
                   (error (e) (js:call (js:global "console") "error" (format nil "IDE: ~A" e)))))
               nil))
    (js:call worker "addEventListener" "error"
             (lambda (event)
               (when (eq worker *worker*)
                 (world-died (or (ignore-errors (js:get event "message")) "world のワーカーが止まりました")))
               nil))
    (js:call worker "postMessage"
             (js:object "type" "boot"
                        "module" (js:global "__clysm_module")
                        "root" (js:global "__clysm_root")
                        "args" (vector)))
    worker))

(defun world-died (message)
  (setf *world-state* :failed)
  (forget-evaluations)
  (add-entry :error :text (format nil "; world が止まりました: ~A" message)))

(defun forget-evaluations ()
  "The evaluations and requests of a world that is gone end."
  (setf *busy* nil *debuggers* '())
  (clrhash *evaluations*)
  (clrhash *requests*)
  (setf *world-queue* '()))

(defun restart-world ()
  "Kill the world, whatever it does, and start another."
  (when *worker*
    (js:call *worker* "terminate")
    (setf *worker* nil))
  (forget-evaluations)
  (setf *repl-package* "CL-USER")
  (add-entry :note :text "; world を終わらせ、新しい world を始めます。定義は評価し直してください。")
  (start-world))

(defun world-ready-p () (eq *world-state* :ready))

(defun receive-from-world (data)
  (let ((type (js:get data "type")))
    (cond ((equal type "ide") (world-message (decode (js:get data "text"))))
          ;; what the image writes to its standard output
          ((equal type "output") (js:call (js:global "console") "log" (format nil "[world] ~A" (js:get data "text"))))
          ((equal type "failed") (world-died (js:get data "message"))))))

(defun world-message (form)
  (case (first form)
    (:ready
     (setf *world-state* :ready
           *world-seconds* (/ (- (get-internal-real-time) *world-started*) 1000.0))
     (let ((queue (reverse *world-queue*)))
       (setf *world-queue* '())
       (dolist (hook *world-hooks*) (funcall hook))
       (mapc #'send-to-world queue)))
    (:output (add-output (second form)))
    (:note (destructuring-bind (&key kind text) (rest form)
             (add-entry kind :text text)))
    (:defined (destructuring-bind (&key name kind file offset) (rest form)
                (remember-definition name kind file offset)))
    (:debugger (push (rest form) *debuggers*))
    (:debugger-end (let ((level (getf (rest form) :level)))
                     (setf *debuggers* (remove level *debuggers* :key (lambda (d) (getf d :level))))))
    (:done (destructuring-bind (&key id ok values package crashed) (rest form)
             (let ((evaluation (gethash id *evaluations*)))
               (remhash id *evaluations*)
               (when crashed (add-entry :error :text (format nil "; 評価が止まりました: ~A" crashed)))
               (when ok (add-entry :values :objects values :package package))
               (setf *busy* (let ((others (loop for e being the hash-values of *evaluations* return (car e))))
                              others))
               (when (and evaluation (cdr evaluation))
                 (funcall (cdr evaluation) ok values package)))))
    (:reply (destructuring-bind (&key id value) (rest form)
              (let ((continuation (gethash id *requests*)))
                (remhash id *requests*)
                (when continuation
                  (if (and (consp value) (eq (first value) :error))
                      (setf *message* (second value))
                      (funcall continuation value))))))))

;;; ------------------------------------------------------------------
;;; Evaluating and asking

(defun busy-p () (and *busy* (null *debuggers*)))

(defun evaluate-in-world (source &key package label repl (offset 0) file on-done)
  "Evaluate SOURCE in the world; ON-DONE is called with OK, the values ((printed id)...)
and the package."
  (let ((id (incf *next-id*)))
    (setf (gethash id *evaluations*) (cons label on-done))
    (setf *busy* label)
    (send-to-world (list :eval :id id :source source :package package :label label
                               :repl repl :offset offset :file file))
    id))

(defun evaluate-in-repl (source)
  (add-entry :input :text source :package *repl-package*)
  (evaluate-in-world source :package *repl-package* :label (one-line source) :repl t
                            :on-done (lambda (ok values package)
                                       (declare (ignore values))
                                       (when ok (setf *repl-package* package)))))

(defun evaluate-source (source &key package label file (offset 0) on-done)
  "Evaluate SOURCE, a text of a buffer, in PACKAGE (a name)."
  (add-entry :eval :text label :package package)
  (evaluate-in-world source :package package :label label :file file :offset offset :on-done on-done))

(defun world-request (op args continuation)
  "Ask the world; CONTINUATION is called with the answer (if the world answers)."
  (let ((id (incf *next-id*)))
    (when continuation (setf (gethash id *requests*) continuation))
    (send-to-world (list :request :id id :op op :args args))
    id))

(defun interrupt-evaluation ()
  (when *busy* (send-to-world '(:interrupt))))

(defun choose-restart* (debugger index)
  (send-to-world (list :restart :level (getf debugger :level) :index index)))

(defun abort-debugger (debugger)
  (choose-restart* debugger (getf debugger :abort)))

;;; ------------------------------------------------------------------
;;; Definitions evaluated in the IDE: where they are in the files

(defvar *definitions* (make-hash-table :test 'equal)
  "qualified name -> ((kind file offset) ...)")

(defun remember-definition (name kind file offset)
  (setf (gethash name *definitions*)
        (cons (list kind file offset)
              (remove-if (lambda (d) (and (equal (first d) kind) (equal (second d) file)))
                         (gethash name *definitions*)))))

;;; ------------------------------------------------------------------
;;; Text

(defun complete-input-p (source)
  "True unless SOURCE ends inside a form."
  (let ((*read-eval* nil) (*read-suppress* t))
    (with-input-from-string (s source)
      (handler-case (loop (when (eq (read s nil s) s) (return t)))
        (end-of-file () nil)
        (error () t)))))
