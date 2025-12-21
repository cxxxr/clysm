;;;; wasm-emit.lisp - Wasm binary emission
;;;; Reference: WebAssembly Binary Format specification

(in-package #:clysm/backend/wasm-emit)

;;; Module header constants
(defconstant +wasm-magic+ #x6d736100)  ; "\0asm" in little-endian
(defconstant +wasm-version+ 1)

(defun emit-module-header ()
  "Emit the Wasm module header: magic number and version.
   Returns an 8-byte vector."
  (let ((header (make-array 8 :element-type '(unsigned-byte 8))))
    ;; Magic number: \0asm (0x00 0x61 0x73 0x6d)
    (setf (aref header 0) #x00)
    (setf (aref header 1) #x61)
    (setf (aref header 2) #x73)
    (setf (aref header 3) #x6d)
    ;; Version: 1 (little-endian 32-bit)
    (setf (aref header 4) #x01)
    (setf (aref header 5) #x00)
    (setf (aref header 6) #x00)
    (setf (aref header 7) #x00)
    header))

(defun emit-empty-module (&key output)
  "Emit a minimal valid Wasm module (header only, no sections).
   If OUTPUT is provided, writes to file and returns pathname.
   Otherwise returns byte vector."
  (let ((bytes (emit-module-header)))
    (if output
        (progn
          (with-open-file (stream output
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
            (write-sequence bytes stream))
          (pathname output))
        bytes)))

(defun emit-module (sections &key output)
  "Emit a complete Wasm module with sections.
   Sections must be provided in ascending ID order."
  (validate-section-order sections)
  (let* ((header (emit-module-header))
         (encoded-sections (mapcar #'encode-section sections))
         (total-size (+ (length header)
                        (reduce #'+ encoded-sections :key #'length)))
         (bytes (make-array total-size :element-type '(unsigned-byte 8)))
         (offset 0))
    ;; Copy header
    (loop for i from 0 below (length header)
          do (setf (aref bytes (+ offset i)) (aref header i)))
    (incf offset (length header))
    ;; Copy sections
    (dolist (section encoded-sections)
      (loop for i from 0 below (length section)
            do (setf (aref bytes (+ offset i)) (aref section i)))
      (incf offset (length section)))
    ;; Output
    (if output
        (progn
          (with-open-file (stream output
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
            (write-sequence bytes stream))
          (pathname output))
        bytes)))
