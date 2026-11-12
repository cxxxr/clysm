;;;; js.lisp — JavaScript interoperability
;;;;
;;;; JavaScript values are externrefs; they cannot live in Lisp data
;;;; structures directly, so they are wrapped in $foreign structs (spec:
;;;; 外部オブジェクトを管理するための専用のラッパー構造体).

(defpackage "JS"
  (:use)
  (:export "GLOBAL" "GET" "SET" "CALL" "NEW" "FUNCALL" "EVAL" "TYPEOF" "LOG"
           "TO-LISP" "TO-JS" "ARRAY" "OBJECT" "JS-VALUE-P"))

(defun js:js-value-p (x) (%foreignp x))

(defun %js-string (s)
  (dotimes (i (length s)) (%put-char-buf (char-code (char s i))))
  (%js-take-string))

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
        (4 (let* ((n (%js-str-len o)) (s (make-string n)))
             (dotimes (i n s) (setf (char s i) (%js-str-char o i)))))
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

(defun js:call (object method &rest args)
  (let ((o (js:to-js object)))
    (js:to-lisp (%js-apply (%js-get o (js:to-js method)) o (%js-args args)))))

(defun js:funcall (function &rest args)
  (js:to-lisp (%js-apply (js:to-js function) (%js-special 1) (%js-args args))))

(defun js:new (constructor &rest args)
  (js:to-lisp (%js-new (js:to-js constructor) (%js-args args))))

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

;;; called from JavaScript (exported as lisp_apply) when a Lisp function
;;; wrapped by TO-JS is invoked
(defun %js-callback (closure args)
  (let* ((f (%js-unwrap-lisp closure))
         (n (js:to-lisp (%js-get args (%js-string "length"))))
         (lisp-args (let ((r nil))
                      (dotimes (i n (nreverse r))
                        (push (js:to-lisp (%js-get args (%js-number (%to-double i)))) r)))))
    (js:to-js (apply f lisp-args))))

(defun %foreign-type-name (x) (js:typeof x))

;;; called by hosts (exported as eval_string): read and evaluate all forms in
;;; a JS string, return the printed values as a JS string
(defun %host-eval-string (src)
  (let ((text (js:to-lisp src)) (out (make-string-output-stream)))
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
      (setq *default-pathname-defaults* (pathname (concatenate 'string cwd "/"))))))
