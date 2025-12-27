;;;; typecase-wasm-test.lisp - Contract tests for typecase Wasm output
;;;; Feature: 030-typecase-macros

(in-package #:clysm/tests/contract/typecase-wasm)

;;; ============================================================
;;; T020: Typecase Wasm Output Validation
;;; ============================================================

(deftest typecase-wasm-expansion-structure
  "Test that typecase expansion has valid structure for Wasm compilation"
  (let* ((form '(typecase x
                  (integer 1)
                  (symbol 2)
                  (otherwise 3)))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form)))
    ;; Verify structure is compilable
    (ok (listp expanded)
        "Expansion is a list")
    (ok (eq (car expanded) 'let)
        "Expansion is a LET form")
    ;; Verify bindings
    (let ((bindings (second expanded)))
      (ok (listp bindings)
          "Bindings is a list")
      (ok (= 1 (length bindings))
          "Single binding for keyform"))))

(deftest typecase-predicate-forms-valid
  "Test that generated predicate forms are valid"
  ;; Test all supported atomic types produce valid predicates
  (dolist (type '(integer symbol cons null list number float
                  character function string))
    (let ((pred-form (clysm/lib/macros:type-specifier-to-predicate type 'x)))
      (ok (listp pred-form)
          (format nil "~A produces a list form" type))
      (ok (symbolp (car pred-form))
          (format nil "~A predicate is a function call" type)))))

(deftest typecase-compound-types-valid
  "Test that compound type specifiers produce valid forms"
  ;; (or integer symbol)
  (let ((or-form (clysm/lib/macros:type-specifier-to-predicate '(or integer symbol) 'x)))
    (ok (eq (car or-form) 'or)
        "OR compound produces OR form")
    (ok (= 3 (length or-form))
        "OR form has correct length"))
  ;; (and number (satisfies evenp))
  (let ((and-form (clysm/lib/macros:type-specifier-to-predicate '(and number (satisfies evenp)) 'x)))
    (ok (eq (car and-form) 'and)
        "AND compound produces AND form"))
  ;; (not null)
  (let ((not-form (clysm/lib/macros:type-specifier-to-predicate '(not null) 'x)))
    (ok (eq (car not-form) 'not)
        "NOT compound produces NOT form"))
  ;; (member :a :b :c)
  (let ((member-form (clysm/lib/macros:type-specifier-to-predicate '(member :a :b :c) 'x)))
    (ok (eq (car member-form) 'or)
        "MEMBER compound produces OR of EQL forms")))

(deftest typecase-nested-if-structure
  "Test that typecase produces correct nested if structure"
  (let* ((form '(typecase val
                  (integer "int")
                  (symbol "sym")
                  (cons "cons")))
         (expander (clysm/lib/macros:make-typecase-expander))
         (expanded (funcall expander form))
         (body (third expanded)))  ; Body of the let
    (ok (eq (car body) 'if)
        "Body starts with IF")
    (let ((else-branch (fourth body)))
      (ok (eq (car else-branch) 'if)
          "Else branch is another IF"))))

;;; ============================================================
;;; T033: Etypecase Wasm Output Validation
;;; ============================================================

(deftest etypecase-wasm-expansion-structure
  "Test that etypecase expansion has valid structure for Wasm compilation"
  (let* ((form '(etypecase x
                  (integer 1)
                  (symbol 2)))
         (expander (clysm/lib/macros:make-etypecase-expander))
         (expanded (funcall expander form)))
    ;; Verify structure is compilable
    (ok (listp expanded)
        "Expansion is a list")
    (ok (eq (car expanded) 'let)
        "Expansion is a LET form")))

(deftest etypecase-error-clause-present
  "Test that etypecase expansion includes error signaling"
  (let* ((form '(etypecase x
                  (integer 1)
                  (symbol 2)))
         (expander (clysm/lib/macros:make-etypecase-expander))
         (expanded (funcall expander form)))
    ;; Flatten and check for error
    (labels ((flatten (tree)
               (cond ((null tree) nil)
                     ((atom tree) (list tree))
                     (t (append (flatten (car tree))
                                (flatten (cdr tree)))))))
      (ok (member 'error (flatten expanded))
          "Expansion contains ERROR call")
      (ok (member 'type-error (flatten expanded))
          "Expansion contains TYPE-ERROR"))))
