;;;; live.lisp — compiling code into a running image ("live" modules)
;;;;
;;;; When the compiler is embedded in a Clysm image, COMPILE and the REPL can
;;;; turn forms into new WebAssembly modules that are linked with the running
;;;; image:
;;;;  * the base types form an identical rec group, so struct/array/function
;;;;    types are shared between the modules;
;;;;  * the multiple-value buffer, binding stack and exception tag are imported
;;;;    from the image;
;;;;  * symbols and literal objects are passed in at instantiation as a vector
;;;;    of the live objects themselves.

(in-package :clysm)

(defmacro with-fresh-codegen-state (() &body body)
  "Bind all per-module code generation state, so that compilations can nest
(e.g. a macro expander being compiled while a module is being built)."
  `(let ((*toplevel-lams* nil)
         (*current-toplevel* nil)
         (*toplevel-nodes* nil)
         (*symbol-globals* nil)
         (*symbol-global-table* (make-hash-table :test 'eq))
         (*const-globals* nil)
         (*const-table* (make-hash-table :test 'eql))
         (*const-string-table* (make-hash-table :test 'equal))
         (*closure-globals* nil)
         (*misc-globals* nil)
         (*pending-lams* nil)
         (*compiled-lams* (make-hash-table :test 'eq))
         (*extra-types* (make-hash-table :test 'equal))
         (*global-defuns* (make-hash-table :test 'eq))
         (*called-functions* (make-hash-table :test 'eq))
         (*runtime-consts* nil)
         (*live-constants* nil)
         (*helpers* *helpers*)
         (*current-lam* nil)
         (*macros-ready* t))
     ,@body))

(defun live-compile-forms (forms)
  "Compile FORMS into a module linked with the running image.
Returns (values bytes constants-vector)."
  (let ((*live-mode* t)
        (*live-packages* t)
        (*read-package* *package*)
        (*warnings* nil)
        (*error-output* (make-broadcast-stream)))
    (with-fresh-codegen-state ()
      (let ((env (make-cenv)))
        (dolist (f forms) (process-toplevel f env)))
      (when *current-toplevel*
        ;; keep the value of the last form
        (setf (lam-body *current-toplevel*) `(:progn (:const nil) ,@(reverse *toplevel-nodes*)))
        (push *current-toplevel* *toplevel-lams*)
        (setf *current-toplevel* nil *toplevel-nodes* nil))
      (build-live-module))))

(defun build-live-module ()
  (let ((*m* (make-wasm-module)))
    (loop for (name def) in *type-defs* do (add-type *m* name def))
    (setf (wasm-module-base-types *m*) (length *type-defs*))
    (loop for (name type mod field) in *imports* do (add-import-func *m* name type mod field))
    (add-import-global *m* :mv-count :i32 t "clysm" "mv_count")
    (add-import-global *m* :mv-buf '(:ref :vector) nil "clysm" "mv_buf")
    (add-import-global *m* :bstack '(:ref :vector) t "clysm" "bstack")
    (add-import-global *m* :bsp :i32 t "clysm" "bsp")
    (add-import-tag *m* :lisp-throw :exn "clysm" "lisp_throw")
    (sym-global nil) (sym-global t)
    (define-standard-helpers)
    (define-argerr-helpers)
    (define-checked-casts)
    (let ((tops (reverse *toplevel-lams*)))
      (dolist (l tops) (ensure-lam-compiled l) (static-closure-global l))
      (compile-pending-lams)
      (let ((*fs* (make-fstate :lam nil :nparams 0)))
        (dolist (h (reverse *helpers*))
          (destructuring-bind (name type locals body-fn) h
            (let ((body (directify-helper-calls
                         (loop for item in (funcall body-fn)
                               if (and (consp item) (eq (car item) :progn-marker))
                                 append (cdr item)
                               else collect item))))
              (add-func *m* name type
                        :locals (mapcar #'second locals)
                        :local-names (mapcar #'first locals)
                        :body (remove nil body))))))
      (compile-pending-lams)
      ;; globals: symbols, live constants and closures are filled in by RUN
      (let* ((syms (reverse *symbol-globals*))
             (consts (reverse *live-constants*))
             (closures (reverse *closure-globals*))
             (objects (append syms (mapcar #'cdr consts))))
        (dolist (s syms)
          (add-global *m* (list :sym s) '(:ref :null :symbol) t '((:ref.null :symbol)))
          (setf (gethash (list :sym s) (wasm-module-nonnull-globals *m*)) t))
        (dolist (c consts)
          (add-global *m* (car c) :eqref t '((:ref.null :eq))))
        (dolist (g closures)
          (add-global *m* (first g) '(:ref :null :closure) t '((:ref.null :closure)))
          (setf (gethash (first g) (wasm-module-nonnull-globals *m*)) t))
        (dolist (g (reverse *misc-globals*))
          (destructuring-bind (name type mutable init) g
            (add-global *m* name type mutable init)))
        (add-func *m* :run :x-e :export "run"
                  :locals '((:ref :null :vector) :eqref) :local-names '(v result)
                  :body `((:local.get 0) :any.convert_extern (:ref.cast! :vector) (:local.set v)
                          ,@(loop for s in syms for i from 0
                                  append `((:local.get v) (:i32.const ,i) (:array.get :vector)
                                           (:ref.cast :symbol) (:global.set (:sym ,s))))
                          ,@(loop for c in consts for i from (length syms)
                                  append `((:local.get v) (:i32.const ,i) (:array.get :vector)
                                           (:global.set ,(car c))))
                          ,@(loop for g in closures
                                  append `(,@(third g) (:global.set ,(first g))))
                          ,@(loop for l in tops
                                  append `((:global.get ,(static-closure-global l))
                                           (:call (:main ,(lam-id l))) (:local.set result)))
                          (:local.get result)))
        ;; a module made at run time only needs the helpers it calls
        (remove-unreachable-functions *m*)
        (values (encode-module *m*) (coerce objects 'simple-vector))))))
