;;;; contrib/fixups.lisp — Clysm fixups of Quicklisp systems (see asdf-hooks.lisp)

(in-package :asdf)

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
