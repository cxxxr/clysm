;;;; tools/dump-func.lisp — print the Wasm instructions of compiled functions
;;;; usage: sbcl --script tools/dump-func.lisp FILE.lisp NAME...
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)
(defvar *dump-module* nil)
(sb-int:encapsulate 'clysm-wasm:encode-module 'dump
                    (lambda (f m &rest r) (setq *dump-module* m) (apply f m r)))
(let* ((args (cdr sb-ext:*posix-argv*)) (file (first args)) (names (rest args)))
  (let ((*error-output* (make-broadcast-stream)))
    (compile-program (list file) :output "/dev/null"
                                 :runtime-dir (merge-pathnames "../runtime/" *load-truename*)))
  (loop for f across (clysm-wasm::wasm-module-funcs *dump-module*)
        for disp = (format nil "~A" (or (clysm-wasm::wasm-func-display f) (wasm-func-name f)))
        when (some (lambda (n) (or (string-equal n disp) (eql 0 (search (format nil "~A#" n) disp :test (function char-equal))))) names)
          do (let ((*print-right-margin* 120) (*print-case* :downcase))
               (format t "~&;;; ~A  locals ~S~%" disp (wasm-func-locals f))
               (dolist (i (wasm-func-body f)) (format t "  ~S~%" i)))))
