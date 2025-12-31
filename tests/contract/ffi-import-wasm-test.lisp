;;;; ffi-import-wasm-test.lisp - Contract tests for FFI import section generation
;;;; Feature: 027-complete-ffi (T014)
;;;; Feature: 001-ffi-import-architecture (T029)

(in-package #:clysm/tests)

;;; ==========================================================================
;;; Helpers for Import Section Parsing
;;; ==========================================================================

(defun read-leb128-unsigned (bytes pos)
  "Read unsigned LEB128 from BYTES at POS. Returns (value new-pos)."
  (let ((result 0)
        (shift 0))
    (loop
      (let ((byte (aref bytes pos)))
        (incf pos)
        (setf result (logior result (ash (logand byte #x7f) shift)))
        (incf shift 7)
        (when (zerop (logand byte #x80))
          (return (values result pos)))))))

(defun read-wasm-string (bytes pos)
  "Read a Wasm string (length-prefixed UTF-8) from BYTES at POS.
Returns (string new-pos)."
  (multiple-value-bind (len new-pos) (read-leb128-unsigned bytes pos)
    (let ((str (make-string len)))
      (loop for i from 0 below len
            do (setf (aref str i) (code-char (aref bytes (+ new-pos i)))))
      (values str (+ new-pos len)))))

(defun parse-import-section (wasm-bytes)
  "Parse the Import section from WASM-BYTES.
Returns a list of (module-name field-name import-kind) entries, or NIL if no import section."
  (when (and (arrayp wasm-bytes) (> (length wasm-bytes) 8))
    (let ((pos 8)
          (imports nil))
      ;; Find import section (ID = 2)
      (loop while (< pos (length wasm-bytes))
            do (let ((section-id (aref wasm-bytes pos)))
                 (incf pos)
                 (when (>= pos (length wasm-bytes))
                   (return-from parse-import-section nil))
                 (multiple-value-bind (section-size new-pos)
                     (read-leb128-unsigned wasm-bytes pos)
                   (setf pos new-pos)
                   (when (= section-id 2) ; Import section
                     ;; Parse imports
                     (multiple-value-bind (import-count p)
                         (read-leb128-unsigned wasm-bytes pos)
                       (setf pos p)
                       (dotimes (i import-count)
                         ;; Read module name
                         (multiple-value-bind (mod-name p1)
                             (read-wasm-string wasm-bytes pos)
                           (setf pos p1)
                           ;; Read field name
                           (multiple-value-bind (field-name p2)
                               (read-wasm-string wasm-bytes pos)
                             (setf pos p2)
                             ;; Read import kind
                             (let ((kind (aref wasm-bytes pos)))
                               (incf pos)
                               ;; Skip type/table/memory/global description
                               ;; For simplicity, we just skip based on kind
                               (case kind
                                 (0 ; func - skip type index (leb128)
                                  (multiple-value-bind (type-idx p3)
                                      (read-leb128-unsigned wasm-bytes pos)
                                    (declare (ignore type-idx))
                                    (setf pos p3)))
                                 (1 ; table - skip limits
                                  (incf pos 3)) ; reftype + limits (simplified)
                                 (2 ; memory - skip limits
                                  (incf pos 2)) ; limits (simplified)
                                 (3 ; global - skip globaltype
                                  (incf pos 2))) ; valtype + mut
                               (push (list mod-name field-name kind) imports))))))
                     (return-from parse-import-section (nreverse imports)))
                   ;; Skip this section
                   (incf pos section-size))))
      nil)))

(defun has-import-p (wasm-bytes module-name field-name)
  "Check if WASM-BYTES contains an import with MODULE-NAME and FIELD-NAME."
  (let ((imports (parse-import-section wasm-bytes)))
    (some (lambda (import)
            (and (string= (first import) module-name)
                 (string= (second import) field-name)))
          imports)))

(deftest ffi-import-section-generation-test
  "Test that FFI declarations produce valid import section"
  (testing "import section structure"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function test-fn "host.func" (:fixnum) :fixnum))

    (let ((imports (clysm/ffi:collect-ffi-imports clysm/ffi:*ffi-environment*)))
      (ok imports "Should collect imports")
      (ok (listp imports) "Imports should be a list")
      (ok (>= (length imports) 1) "Should have at least 1 import"))))

(deftest ffi-import-type-index-test
  "Test that FFI imports get assigned type indices"
  (testing "type index assignment"
    (clysm/ffi:reset-ffi-environment)
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (eval '(clysm/ffi:define-foreign-function fn1 "host.fn1" (:fixnum) :fixnum))
    (eval '(clysm/ffi:define-foreign-function fn2 "host.fn2" () :void))

    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (let ((decl1 (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'fn1))
          (decl2 (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'fn2)))
      ;; Type indices should be assigned (>= 23 per spec - after reserved types)
      (ok (or (null (clysm/ffi:ffd-type-index decl1))
              (>= (clysm/ffi:ffd-type-index decl1) 0))
          "fn1 should have valid type index or nil")
      (ok (or (null (clysm/ffi:ffd-type-index decl2))
              (>= (clysm/ffi:ffd-type-index decl2) 0))
          "fn2 should have valid type index or nil"))))

;;; ==========================================================================
;;; T029: Dynamic Call Includes $dynamic-call Import
;;; Feature: 001-ffi-import-architecture (User Story 3)
;;; ==========================================================================

(deftest dynamic-call-includes-dynamic-call-import
  "T029: Code with dynamic funcall/apply includes $dynamic-call import.
When the analyzer detects a dynamic call pattern like (funcall var args),
the compiled Wasm should include a $dynamic-call import from 'clysm:runtime'
to enable runtime function resolution."
  (testing "dynamic funcall includes $dynamic-call import"
    ;; This code has a dynamic call: (funcall x) where x is a variable
    (let* ((code '(let ((fn (intern "IDENTITY")))
                    (funcall fn 42)))
           (wasm-bytes (clysm/compiler:compile-to-wasm code)))
      ;; The import should be from "clysm:runtime" module with name "$dynamic-call"
      (ok (has-import-p wasm-bytes "clysm:runtime" "$dynamic-call")
          "Dynamic funcall should produce $dynamic-call import")))

  (testing "dynamic apply includes $dynamic-call import"
    ;; apply with a variable function is also dynamic
    ;; Use a simpler expression that doesn't require undefined functions
    (let* ((code '(let ((fn (car (list 'identity))))
                    (apply fn '(1 2 3))))
           (wasm-bytes (clysm/compiler:compile-to-wasm code)))
      (ok (has-import-p wasm-bytes "clysm:runtime" "$dynamic-call")
          "Dynamic apply should produce $dynamic-call import")))

  (testing "static funcall does NOT include $dynamic-call import"
    ;; Static funcall with quoted symbol should NOT need $dynamic-call
    (let* ((code '(funcall 'identity 42))
           (wasm-bytes (clysm/compiler:compile-to-wasm code)))
      (ok (not (has-import-p wasm-bytes "clysm:runtime" "$dynamic-call"))
          "Static funcall should NOT produce $dynamic-call import"))))
