;;;; primitive-dispatch-test.lisp - Unit tests for primitive dispatch
;;;;
;;;; Tests for the hash-table driven primitive dispatch mechanism.
;;;; Part of 001-primitive-dispatch-table feature.

(defpackage #:clysm/tests/primitive-dispatch
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler/codegen/primitive-dispatch
                #:register-primitive-compiler
                #:unregister-primitive-compiler
                #:primitive-compiler-entry
                #:primitive-registered-p
                #:list-registered-primitives
                #:dispatch-primitive
                #:clear-primitive-tables
                #:primitive-entry
                #:make-primitive-entry
                #:primitive-entry-p
                #:primitive-entry-compiler-fn
                #:primitive-entry-arity
                #:primitive-entry-flags
                #:*primitive-symbol-table*
                #:*primitive-string-table*))

(in-package #:clysm/tests/primitive-dispatch)

;;; Test helpers

(defun test-compiler-fn (op args env)
  "Test compiler function that returns a simple marker."
  (declare (ignore op args env))
  '(:test-compiled))

(defun test-compiler-fn-2 (op args env)
  "Alternative test compiler function."
  (declare (ignore op args env))
  '(:test-compiled-2))

;;; ==========================================================================
;;; User Story 1: Registration API Tests
;;; ==========================================================================

(deftest register-primitive-creates-entry
  "T012: register-primitive-compiler creates entry in symbol table"
  (clear-primitive-tables)
  (let ((entry (register-primitive-compiler 'test-op #'test-compiler-fn :arity 2)))
    (ok (not (null entry)) "Entry should be returned")
    (ok (primitive-entry-p entry) "Should return a primitive-entry struct")
    (ok (gethash 'test-op *primitive-symbol-table*) "Should be in symbol table")
    (ok (eql (primitive-entry-arity entry) 2) "Arity should be 2")
    (ok (functionp (primitive-entry-compiler-fn entry)) "Compiler-fn should be a function")))

(deftest register-with-string-name-creates-both
  "T013: register-primitive-compiler with :string-name creates entry in both tables"
  (clear-primitive-tables)
  (register-primitive-compiler 'test-op-2 #'test-compiler-fn
                               :string-name "TEST-OP-2")
  (ok (gethash 'test-op-2 *primitive-symbol-table*) "Should be in symbol table")
  (ok (gethash "TEST-OP-2" *primitive-string-table*) "Should be in string table"))

(deftest re-registration-replaces-entry
  "T014: re-registration replaces existing entry"
  (clear-primitive-tables)
  (register-primitive-compiler 'test-op #'test-compiler-fn :arity 2)
  (register-primitive-compiler 'test-op #'test-compiler-fn-2 :arity 3)
  (let ((entry (gethash 'test-op *primitive-symbol-table*)))
    (ok (eql (primitive-entry-arity entry) 3) "Arity should be updated to 3")
    (ok (eq (primitive-entry-compiler-fn entry) #'test-compiler-fn-2)
        "Compiler function should be updated")))

(deftest primitive-registered-p-returns-correct-value
  "T015: primitive-registered-p returns T for registered, NIL for unregistered"
  (clear-primitive-tables)
  (register-primitive-compiler 'registered-op #'test-compiler-fn)
  (ok (primitive-registered-p 'registered-op) "Should return T for registered")
  (ok (not (primitive-registered-p 'unregistered-op)) "Should return NIL for unregistered"))

(deftest primitive-compiler-entry-returns-entry-or-nil
  "T016: primitive-compiler-entry returns entry struct or NIL"
  (clear-primitive-tables)
  (register-primitive-compiler 'existing-op #'test-compiler-fn)
  (ok (primitive-entry-p (primitive-compiler-entry 'existing-op))
      "Should return entry for registered primitive")
  (ok (null (primitive-compiler-entry 'nonexistent-op))
      "Should return NIL for unregistered primitive"))

(deftest list-registered-primitives-returns-all
  "T017: list-registered-primitives returns all registered symbols"
  (clear-primitive-tables)
  (register-primitive-compiler 'op-a #'test-compiler-fn)
  (register-primitive-compiler 'op-b #'test-compiler-fn)
  (register-primitive-compiler 'op-c #'test-compiler-fn :string-name "OP-C")
  (let ((symbols (list-registered-primitives :table :symbol))
        (strings (list-registered-primitives :table :string))
        (all (list-registered-primitives :table :all)))
    (ok (= (length symbols) 3) "Should have 3 symbol entries")
    (ok (= (length strings) 1) "Should have 1 string entry")
    (ok (= (length all) 4) "Should have 4 total entries")))

;;; ==========================================================================
;;; User Story 2: Symbol Lookup Dispatch Tests
;;; ==========================================================================

(deftest dispatch-returns-instructions-for-registered
  "T026: dispatch-primitive returns instructions for registered symbol"
  (clear-primitive-tables)
  (register-primitive-compiler 'test-dispatch #'test-compiler-fn)
  (let ((result (dispatch-primitive 'test-dispatch '(1 2) nil)))
    (ok (equal result '(:test-compiled)) "Should return compiled instructions")))

(deftest dispatch-returns-nil-for-unregistered
  "T027: dispatch-primitive returns NIL for unregistered symbol"
  (clear-primitive-tables)
  (let ((result (dispatch-primitive 'unknown-op '(1 2) nil)))
    (ok (null result) "Should return NIL for unregistered")))

;;; ==========================================================================
;;; User Story 3: String Lookup Tests
;;; ==========================================================================

(deftest dispatch-finds-string-entry-when-symbol-missing
  "T041: dispatch-primitive finds string-table entry when symbol-table lookup fails"
  (clear-primitive-tables)
  ;; Register only in string table by using a gensym and adding to string table directly
  (let ((entry (make-primitive-entry :compiler-fn #'test-compiler-fn)))
    (setf (gethash "SPECIAL-OP" *primitive-string-table*) entry))
  ;; Create a symbol with the same name but different identity
  (let* ((pkg (or (find-package :clysm/tests/primitive-dispatch)
                  (make-package :clysm/tests/primitive-dispatch)))
         (sym (intern "SPECIAL-OP" pkg)))
    (let ((result (dispatch-primitive sym '() nil)))
      (ok (equal result '(:test-compiled))
          "Should find via string lookup when symbol lookup fails"))))

(deftest symbol-lookup-takes-precedence
  "T042: symbol-table lookup takes precedence over string-table"
  (clear-primitive-tables)
  ;; Register with both symbol and string, but different compilers
  (register-primitive-compiler 'precedence-test #'test-compiler-fn)
  (let ((string-entry (make-primitive-entry :compiler-fn #'test-compiler-fn-2)))
    (setf (gethash "PRECEDENCE-TEST" *primitive-string-table*) string-entry))
  (let ((result (dispatch-primitive 'precedence-test '() nil)))
    (ok (equal result '(:test-compiled))
        "Should use symbol-table entry, not string-table")))

;;; ==========================================================================
;;; Unregister Tests
;;; ==========================================================================

(deftest unregister-removes-entry
  "unregister-primitive-compiler removes entry from symbol table"
  (clear-primitive-tables)
  (register-primitive-compiler 'to-remove #'test-compiler-fn)
  (ok (primitive-registered-p 'to-remove) "Should be registered initially")
  (unregister-primitive-compiler 'to-remove)
  (ok (not (primitive-registered-p 'to-remove)) "Should be removed"))

(deftest unregister-with-string-name-removes-both
  "unregister-primitive-compiler with :string-name removes from both tables"
  (clear-primitive-tables)
  (register-primitive-compiler 'dual-entry #'test-compiler-fn
                               :string-name "DUAL-ENTRY")
  (ok (gethash 'dual-entry *primitive-symbol-table*) "Should be in symbol table")
  (ok (gethash "DUAL-ENTRY" *primitive-string-table*) "Should be in string table")
  (unregister-primitive-compiler 'dual-entry :string-name "DUAL-ENTRY")
  (ok (not (gethash 'dual-entry *primitive-symbol-table*)) "Should be removed from symbol table")
  (ok (not (gethash "DUAL-ENTRY" *primitive-string-table*)) "Should be removed from string table"))

;;; ==========================================================================
;;; Edge Cases
;;; ==========================================================================

(deftest empty-table-dispatch-returns-nil
  "Dispatch on empty table returns NIL"
  (clear-primitive-tables)
  (ok (null (dispatch-primitive '+ '(1 2) nil))
      "Should return NIL when tables are empty"))

(deftest special-character-primitive-names
  "Primitives with special characters work correctly"
  (clear-primitive-tables)
  (register-primitive-compiler '1+ #'test-compiler-fn)
  (register-primitive-compiler '1- #'test-compiler-fn)
  (ok (primitive-registered-p '1+) "1+ should be registered")
  (ok (primitive-registered-p '1-) "1- should be registered")
  (ok (dispatch-primitive '1+ '(5) nil) "1+ should dispatch"))

(deftest variadic-arity-works
  "Primitives with nil arity (variadic) work correctly"
  (clear-primitive-tables)
  (register-primitive-compiler 'variadic-op #'test-compiler-fn :arity nil)
  (let ((entry (primitive-compiler-entry 'variadic-op)))
    (ok (null (primitive-entry-arity entry)) "Arity should be nil for variadic")))
