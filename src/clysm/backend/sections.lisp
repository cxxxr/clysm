;;;; sections.lisp - Wasm binary section structure
;;;; Reference: WebAssembly Binary Format - Section definitions

(in-package #:clysm/backend/sections)

;;; Section IDs (must be emitted in ascending order)
(defconstant +section-id-custom+ 0)
(defconstant +section-id-type+ 1)
(defconstant +section-id-import+ 2)
(defconstant +section-id-function+ 3)
(defconstant +section-id-table+ 4)
(defconstant +section-id-memory+ 5)
(defconstant +section-id-global+ 6)
(defconstant +section-id-export+ 7)
(defconstant +section-id-start+ 8)
(defconstant +section-id-element+ 9)
(defconstant +section-id-code+ 10)
(defconstant +section-id-data+ 11)
(defconstant +section-id-data-count+ 12)
(defconstant +section-id-tag+ 13)

(defstruct section
  "A Wasm binary section."
  (id 0 :type (unsigned-byte 8))
  (content #() :type (vector (unsigned-byte 8))))

(defun encode-section (section)
  "Encode a section to bytes: ID + size(LEB128) + content."
  (let* ((content (section-content section))
         (size-bytes (encode-unsigned-leb128 (length content)))
         (result (make-array (+ 1 (length size-bytes) (length content))
                             :element-type '(unsigned-byte 8))))
    ;; Section ID
    (setf (aref result 0) (section-id section))
    ;; Content size (LEB128)
    (loop for i from 0 below (length size-bytes)
          do (setf (aref result (1+ i)) (aref size-bytes i)))
    ;; Content
    (loop for i from 0 below (length content)
          do (setf (aref result (+ 1 (length size-bytes) i)) (aref content i)))
    result))

(defun validate-section-order (sections)
  "Validate that sections are in ascending ID order.
   Returns T if valid, signals error otherwise."
  (loop for (prev curr) on sections
        while curr
        do (when (>= (section-id prev) (section-id curr))
             (error "Section order violation: ID ~D appears after ID ~D"
                    (section-id curr) (section-id prev))))
  t)

(defun encode-vec (items encoder)
  "Encode a vector of items with count prefix."
  (let ((count-bytes (encode-unsigned-leb128 (length items)))
        (encoded-items (mapcar encoder items)))
    (apply #'concatenate '(vector (unsigned-byte 8))
           count-bytes
           encoded-items)))

(defun encode-name (name)
  "Encode a UTF-8 name as length-prefixed byte sequence."
  (let* ((bytes (babel:string-to-octets name :encoding :utf-8))
         (len-bytes (encode-unsigned-leb128 (length bytes))))
    (concatenate '(vector (unsigned-byte 8)) len-bytes bytes)))
