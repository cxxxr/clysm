;;;; contrib/fixups.lisp — Clysm fixups of Quicklisp systems (see asdf-hooks.lisp)

(in-package :asdf)

(defvar *clysm-contrib-directory* (merge-pathnames "contrib/" (sys::%clysm-root)))

;;; 3bz reads files with mmap through CFFI except on Mezzano and ABCL; Clysm
;;; uses the portable stubs like those.
(define-clysm-fixup "3bz" (s)
  (clysm-drop-dependencies s "cffi" "mmap")
  (clysm-enable-component s '("io-nommap") t)
  (clysm-patch-component s '("io-nommap")
    "#- (or abcl mezzano)
(error \"this code assume mezzano/abcl, patches welcome if some other OS needs it\")"
    "")
  (clysm-enable-component s '("io-mmap") nil))

;;; pngload: the same, without mmap
(define-clysm-fixup "pngload" (s)
  (clysm-drop-dependencies s "cffi" "mmap")
  (clysm-enable-component s '("source-noffi") t)
  (clysm-enable-component s '("source-ffi") nil))

;;; Drei uses SWANK for its Lisp syntax unless :CLIM-WITHOUT-SWANK; there is
;;; no SWANK backend for Clysm.
(define-clysm-fixup "drei-mcclim" (s)
  (pushnew :clim-without-swank *features*)
  (clysm-drop-dependencies s "swank"))

;;; Babel: the concrete mappings of all its encodings are made by one form
;;; (INSTANTIATE-CONCRETE-MAPPINGS), whose code (that of the function made
;;; for the toplevel form: 300 KB, 7000 locals) the engine took most of a
;;; second to compile at every load.  Made encoding by encoding, each
;;; encoding's code is in a function of its own.
(define-clysm-fixup "babel" (s)
  (clysm-patch-component s '("src" "strings")
    "(defparameter *string-vector-mappings*
  (instantiate-concrete-mappings
   ;; :optimize ((speed 3) (safety 0) (debug 0) (compilation-speed 0))
   :octet-seq-setter ub-set
   :octet-seq-getter ub-get
   :octet-seq-type (simple-array (unsigned-byte 8) (*))
   :code-point-seq-setter string-set
   :code-point-seq-getter string-get
   :code-point-seq-type simple-unicode-string))"
    "(defparameter *string-vector-mappings* (make-hash-table :test 'eq))

(macrolet ((instantiate-each ()
             (let ((encodings '()))
               (maphash (lambda (name mapping)
                          (declare (ignore mapping))
                          (push name encodings))
                        babel-encodings::*abstract-mappings*)
               `(progn
                  ,@(loop for encoding in (reverse encodings)
                          collect `(maphash (lambda (name mapping)
                                              (setf (gethash name *string-vector-mappings*) mapping))
                                            (instantiate-concrete-mappings
                                             :encodings (,encoding)
                                             :octet-seq-setter ub-set
                                             :octet-seq-getter ub-get
                                             :octet-seq-type (simple-array (unsigned-byte 8) (*))
                                             :code-point-seq-setter string-set
                                             :code-point-seq-getter string-get
                                             :code-point-seq-type simple-unicode-string)))))))
  (instantiate-each))"))

;;; McCLIM
(define-clysm-fixup "clim-core/system" (s)
  (clysm-patch-component s '("utilities" "utilities")
    "  #+clasp (ext:getenv string)
  #-(or abcl ecl excl cmu scl clisp sbcl openmcl lispworks clasp)"
    "  #+clasp (ext:getenv string)
  #+clysm (uiop:getenv string)
  #-(or abcl ecl excl cmu scl clisp sbcl openmcl lispworks clasp clysm)"))

;;; CLX: X server connections (Unix domain or TCP sockets of the host),
;;; environment variables and host names
(define-clysm-fixup "clx" (s)
  (clysm-patch-component s '("dependent")
    "#-(or CMU sbcl ecl clisp clasp)
(defun open-x-stream (host display protocol)"
    "#+clysm
(defun open-x-stream (host display protocol)
  (declare (ignore protocol)
           (type (integer 0) display))
  (let ((path (unix-socket-path-from-host host display)))
    (if path
        (sys::open-local-stream path)
        (sys::open-tcp-stream host (+ *x-tcp-port* display)))))

#-(or CMU sbcl ecl clisp clasp clysm)
(defun open-x-stream (host display protocol)"
    "#-(or CMU sbcl ecl clisp clasp)
(defun host-address (host &optional (family :internet))"
    "#+clysm
(defun host-address (host &optional (family :internet))
  (declare (type stringable host)
           (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (unless (member family '(:internet 0))
    (error \"Host ~S has no ~S address\" host family))
  (let ((name (string host)))
    (cons :internet
          (if (string-equal name \"localhost\")
              (list 127 0 0 1)
              (let ((parts (loop for start = 0 then (1+ dot)
                                 for dot = (position #\\. name :start start)
                                 collect (parse-integer name :start start :end dot :junk-allowed t)
                                 while dot)))
                (if (and (= (length parts) 4) (every (lambda (p) (and p (<= 0 p 255))) parts))
                    parts
                    (error \"Unknown host ~S (only numeric addresses are supported)\" host)))))))

#-(or CMU sbcl ecl clisp clasp clysm)
(defun host-address (host &optional (family :internet))"
    "  #-(or sbcl CMU ecl clisp clasp) (progn name nil))"
    "  #+clysm (sys::%getenv name)
  #-(or sbcl CMU ecl clisp clasp clysm) (progn name nil))"
    "  #+(or cmu sbcl ecl clasp) (machine-instance)"
    "  #+(or cmu sbcl ecl clasp clysm) (machine-instance)"
    "  #-(or cmu sbcl ecl clisp clasp) (error \"get-host-name not implemented\"))"
    "  #-(or cmu sbcl ecl clisp clasp clysm) (error \"get-host-name not implemented\"))"
    ;; angles are compared in the protocol's precision
    "#+(or cmu sbcl clisp ecl clasp)
(progn

;;; This overrides"
    "#+(or cmu sbcl clisp ecl clasp clysm)
(progn

;;; This overrides"))

;;; micros (Lem's SWANK): the backend for Clysm (contrib/micros/clysm.lisp)
(define-clysm-fixup "micros" (s)
  (clysm-add-source-file s '("sbcl") "clysm"
                         (merge-pathnames "micros/clysm.lisp" *clysm-contrib-directory*)
                         :before "gray"))

;;; Trivia's optimizer declaration needs DEFINE-DECLARATION of CLtL2
;;; (trivial-cltl2), which Clysm does not have; it takes the global
;;; optimizer instead, as on the implementations without it.
(define-clysm-fixup "trivia.level2" (s)
  (clysm-drop-dependencies s "trivial-cltl2")
  (clysm-patch-component s '("impl")
    "#-(or abcl clasp ecl mezzano clisp cmu
      lispworks5 lispworks6 lispworks7.0) ; hcl"
    "#-(or abcl clasp ecl mezzano clisp cmu clysm
      lispworks5 lispworks6 lispworks7.0) ; hcl"
    "(funcall (or #-(or abcl clasp ecl mezzano clisp cmu
"
    "(funcall (or #-(or abcl clasp ecl mezzano clisp cmu clysm
"))
;;; SWANK (SLIME's server): the backend for Clysm (contrib/swank/clysm.lisp),
;;; and Clysm in the loader's lists of implementations.
(define-clysm-fixup "swank" (s)
  (clysm-patch-component s '("swank-loader")
    "  #+mezzano '((swank mezzano) (swank gray))"
    "  #+mezzano '((swank mezzano) (swank gray))
  #+clysm '((swank clysm) (swank gray))"
    ":armedbear :gcl :ecl :scl :mkcl :clasp :mezzano))"
    ":armedbear :gcl :ecl :scl :mkcl :clasp :mezzano :clysm))"
    ":java-1.4 :java-1.5 :java-1.6 :java-1.7))"
    ":java-1.4 :java-1.5 :java-1.6 :java-1.7 :wasm))"
    "  #+armedbear (lisp-implementation-version)"
    "  #+armedbear (lisp-implementation-version)
  #+clysm (lisp-implementation-version)")
  ;; With communication style NIL, the requests that arrive while requests
  ;; are processed are processed with the I/O redirection of the connection
  ;; of the time the processing began: those that follow SWANK-REPL:CREATE-REPL
  ;; would write to the process's own streams.
  (clysm-patch-component s '("swank")
    "         ((:emacs-rex &rest args) (apply #'eval-for-emacs args))"
    "         ((:emacs-rex &rest args)
          (with-io-redirection (*emacs-connection*)
            (apply #'eval-for-emacs args)))")
  (clysm-add-source-file s '("backend") "clysm"
                         (merge-pathnames "swank/clysm.lisp" *clysm-contrib-directory*)
                         :before "gray"))
