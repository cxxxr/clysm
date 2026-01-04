;;;; backend/sections.lisp - Wasm Section Emitters
;;;;
;;;; Each Wasm section has a specific ID and structure.
;;;; This file provides functions to emit each section type.

(in-package #:clysm)

;;; ============================================================
;;; Section IDs
;;; ============================================================

(defparameter *section-ids*
  '((:custom . 0)
    (:type . 1)
    (:import . 2)
    (:function . 3)
    (:table . 4)
    (:memory . 5)
    (:global . 6)
    (:export . 7)
    (:start . 8)
    (:element . 9)
    (:code . 10)
    (:data . 11)
    (:data-count . 12)
    (:tag . 13))
  "Mapping from section names to their IDs.")

;;; ============================================================
;;; Section Wrapper
;;; ============================================================

(defun wrap-section (section-id content)
  "Wrap section content with ID and size prefix."
  (let ((content-bytes (if (listp content) content (coerce content 'list))))
    (append (list section-id)
            (encode-uleb128 (length content-bytes))
            content-bytes)))

(defun emit-section (section-name content)
  "Emit a section with the given name and content."
  (let ((id (cdr (assoc section-name *section-ids*))))
    (unless id
      (error "Unknown section: ~S" section-name))
    (wrap-section id content)))

;;; ============================================================
;;; Type Section (ID = 1)
;;; ============================================================

(defun emit-type-section (types)
  "Emit the type section from a list of wasm-type structs.
Returns the section bytes including header."
  (when (null types)
    (return-from emit-type-section nil))
  (let ((content (encode-vector #'encode-type-definition types)))
    (emit-section :type content)))

;;; ============================================================
;;; Import Section (ID = 2)
;;; ============================================================

(defparameter *import-kind-encoding*
  '((:func . #x00)
    (:table . #x01)
    (:memory . #x02)
    (:global . #x03)))

(defun encode-import (import)
  "Encode a single import entry."
  (append (encode-name (wasm-import-module import))
          (encode-name (wasm-import-name import))
          (list (cdr (assoc (wasm-import-kind import) *import-kind-encoding*)))
          (ecase (wasm-import-kind import)
            (:func (encode-uleb128 (wasm-import-desc import)))
            (:table (error "Table import not yet implemented"))
            (:memory (encode-limits (wasm-import-desc import)))
            (:global (error "Global import not yet implemented")))))

(defun emit-import-section (imports)
  "Emit the import section."
  (when (null imports)
    (return-from emit-import-section nil))
  (let ((content (encode-vector #'encode-import imports)))
    (emit-section :import content)))

;;; ============================================================
;;; Function Section (ID = 3)
;;; ============================================================

(defun emit-function-section (funcs)
  "Emit the function section (type indices only).
The actual code is in the code section."
  (when (null funcs)
    (return-from emit-function-section nil))
  (let ((content (encode-vector
                  (lambda (f) (encode-uleb128 (wasm-func-type-index f)))
                  funcs)))
    (emit-section :function content)))

;;; ============================================================
;;; Table Section (ID = 4)
;;; ============================================================

(defun encode-table (table)
  "Encode a table definition."
  (append (encode-valtype (wasm-table-reftype table))
          (encode-limits (wasm-table-limits table))))

(defun emit-table-section (tables)
  "Emit the table section."
  (when (null tables)
    (return-from emit-table-section nil))
  (let ((content (encode-vector #'encode-table tables)))
    (emit-section :table content)))

;;; ============================================================
;;; Memory Section (ID = 5)
;;; ============================================================

(defun encode-memory (memory)
  "Encode a memory definition."
  (encode-limits (wasm-memory-limits memory)))

(defun emit-memory-section (memories)
  "Emit the memory section."
  (when (null memories)
    (return-from emit-memory-section nil))
  (let ((content (encode-vector #'encode-memory memories)))
    (emit-section :memory content)))

;;; ============================================================
;;; Global Section (ID = 6)
;;; ============================================================

(defun encode-global (global)
  "Encode a global definition."
  (append (encode-valtype (wasm-global-type global))
          (list (if (wasm-global-mutable global) #x01 #x00))
          (wasm-global-init global)))

(defun emit-global-section (globals)
  "Emit the global section."
  (when (null globals)
    (return-from emit-global-section nil))
  (let ((content (encode-vector #'encode-global globals)))
    (emit-section :global content)))

;;; ============================================================
;;; Export Section (ID = 7)
;;; ============================================================

(defun emit-export-section (exports)
  "Emit the export section."
  (when (null exports)
    (return-from emit-export-section nil))
  (let ((content (encode-vector #'encode-export exports)))
    (emit-section :export content)))

;;; ============================================================
;;; Start Section (ID = 8)
;;; ============================================================

(defun emit-start-section (start-func-index)
  "Emit the start section."
  (when (null start-func-index)
    (return-from emit-start-section nil))
  (emit-section :start (encode-uleb128 start-func-index)))

;;; ============================================================
;;; Code Section (ID = 10)
;;; ============================================================

(defun encode-locals (locals)
  "Encode local variable declarations.
LOCALS is a list of (count . valtype) pairs."
  (encode-vector
   (lambda (local)
     (append (encode-uleb128 (car local))
             (encode-valtype (cdr local))))
   locals))

(defun encode-func-body (func)
  "Encode a function body (locals + code)."
  (let* ((locals-bytes (encode-locals (wasm-func-locals func)))
         (code-bytes (wasm-func-body func))
         (body (append locals-bytes code-bytes)))
    ;; Wrap with size prefix
    (append (encode-uleb128 (length body))
            body)))

(defun emit-code-section (funcs)
  "Emit the code section."
  (when (null funcs)
    (return-from emit-code-section nil))
  (let ((content (encode-vector #'encode-func-body funcs)))
    (emit-section :code content)))

;;; ============================================================
;;; Data Section (ID = 11)
;;; ============================================================

(defun emit-data-section (data-segments)
  "Emit the data section."
  (when (null data-segments)
    (return-from emit-data-section nil))
  ;; Data segments are more complex; implement as needed
  (error "Data section not yet implemented"))

;;; ============================================================
;;; Custom Section (ID = 0)
;;; ============================================================

(defun emit-custom-section (name content)
  "Emit a custom section with NAME and CONTENT."
  (let ((section-content (append (encode-name name)
                                 (if (listp content)
                                     content
                                     (coerce content 'list)))))
    (emit-section :custom section-content)))

(defun emit-name-section (module)
  "Emit the 'name' custom section for debugging."
  ;; Simplified: just function names for now
  (let* ((func-names
          (loop for func in (wasm-module-funcs module)
                for i from (length (wasm-module-imports module))
                when (wasm-func-name func)
                  collect (cons i (wasm-func-name func))))
         (content
          (when func-names
            (append
             ;; Subsection 1: function names
             (list 1)  ; subsection id
             (let ((name-map
                    (encode-vector
                     (lambda (pair)
                       (append (encode-uleb128 (car pair))
                               (encode-name (cdr pair))))
                     func-names)))
               (append (encode-uleb128 (length name-map))
                       name-map))))))
    (when content
      (emit-custom-section "name" content))))
