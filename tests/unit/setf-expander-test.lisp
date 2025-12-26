;;;; setf-expander-test.lisp - Unit tests for setf expanders
;;;; Feature 028: Setf Macros and Generalized References

(defpackage #:clysm/tests/unit/setf-expander-test
  (:use #:cl #:rove)
  (:import-from #:clysm/lib/setf-expanders
                #:setf-expander-registry
                #:make-setf-expander-registry
                #:register-setf-expander
                #:get-setf-expander
                #:get-setf-expansion*
                #:*global-setf-expander-registry*
                #:install-standard-setf-expanders
                #:make-car-setf-expander
                #:make-cdr-setf-expander
                #:make-nth-setf-expander
                #:make-aref-setf-expander
                #:make-gethash-setf-expander
                #:make-symbol-value-setf-expander))

(in-package #:clysm/tests/unit/setf-expander-test)

;;; ============================================================
;;; Phase 2: Foundational Tests (T007-T008)
;;; ============================================================

(deftest test-setf-expander-registry-creation ()
  "Test setf-expander-registry creation and lookup (T007)."
  (testing "Registry creation"
    (let ((registry (make-setf-expander-registry)))
      (ok (typep registry 'setf-expander-registry)
          "Registry is correct type")))
  (testing "Registry lookup before registration"
    (let ((registry (make-setf-expander-registry)))
      (ok (null (get-setf-expander registry 'nonexistent))
          "Lookup returns nil for unknown accessor")))
  (testing "Registry registration and lookup"
    (let ((registry (make-setf-expander-registry))
          (expander (lambda (form env)
                      (declare (ignore form env))
                      (values nil nil nil nil nil))))
      (register-setf-expander registry 'test-accessor expander)
      (ok (eq expander (get-setf-expander registry 'test-accessor))
          "Retrieved expander matches registered"))))

(deftest test-get-setf-expansion-protocol ()
  "Test get-setf-expansion* returns five values (T008)."
  (testing "Simple variable expansion protocol"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* 'x)
      (ok (listp temps) "temps is a list")
      (ok (listp vals) "vals is a list")
      (ok (listp stores) "stores is a list")
      (ok (not (null stores)) "stores is non-empty")
      (ok (consp store-form) "store-form is a form")
      (ok (not (null access-form)) "access-form is present")))
  (testing "CAR expansion protocol"
    (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-expansion* '(car x))
      (ok (= (length temps) (length vals))
          "temps and vals have same length")
      (ok (= 1 (length stores)) "One store variable")
      (ok (consp store-form) "store-form is a form")
      (ok (consp access-form) "access-form is a form"))))

;;; ============================================================
;;; Phase 3: User Story 1 - Standard Expander Tests (T019-T024)
;;; ============================================================

(deftest test-car-setf-expander ()
  "Test CAR setf expander (T019)."
  (let ((expander (make-car-setf-expander)))
    (multiple-value-bind (temps vals stores store-form access-form)
        (funcall expander '(car x) nil)
      (ok (= 1 (length temps)) "One temp for cons")
      (ok (= 1 (length vals)) "One val")
      (ok (eq (first vals) 'x) "Val is the cons form")
      (ok (= 1 (length stores)) "One store")
      (ok (member 'rplaca (flatten store-form))
          "Store form uses rplaca")
      (ok (eq (first access-form) 'car)
          "Access form uses car"))))

(deftest test-cdr-setf-expander ()
  "Test CDR setf expander (T020)."
  (let ((expander (make-cdr-setf-expander)))
    (multiple-value-bind (temps vals stores store-form access-form)
        (funcall expander '(cdr x) nil)
      (ok (= 1 (length temps)) "One temp for cons")
      (ok (= 1 (length vals)) "One val")
      (ok (= 1 (length stores)) "One store")
      (ok (member 'rplacd (flatten store-form))
          "Store form uses rplacd")
      (ok (eq (first access-form) 'cdr)
          "Access form uses cdr"))))

(deftest test-nth-setf-expander ()
  "Test NTH setf expander (T021)."
  (let ((expander (make-nth-setf-expander)))
    (multiple-value-bind (temps vals stores store-form access-form)
        (funcall expander '(nth 2 list) nil)
      (ok (= 2 (length temps)) "Two temps (index and list)")
      (ok (= 2 (length vals)) "Two vals")
      (ok (= 1 (length stores)) "One store")
      (ok (eq (first access-form) 'nth)
          "Access form uses nth"))))

(deftest test-aref-setf-expander ()
  "Test AREF setf expander (T022)."
  (let ((expander (make-aref-setf-expander)))
    (testing "Single index"
      (multiple-value-bind (temps vals stores store-form access-form)
          (funcall expander '(aref arr 0) nil)
        (ok (= 2 (length temps)) "Two temps (array and index)")
        (ok (= 2 (length vals)) "Two vals")
        (ok (= 1 (length stores)) "One store")
        (ok (eq (first access-form) 'aref)
            "Access form uses aref")))
    (testing "Multiple indices"
      (multiple-value-bind (temps vals stores store-form access-form)
          (funcall expander '(aref arr 0 1 2) nil)
        (ok (= 4 (length temps)) "Four temps (array and 3 indices)")
        (ok (= 4 (length vals)) "Four vals")))))

(deftest test-gethash-setf-expander ()
  "Test GETHASH setf expander (T023)."
  (let ((expander (make-gethash-setf-expander)))
    (testing "Without default"
      (multiple-value-bind (temps vals stores store-form access-form)
          (funcall expander '(gethash key ht) nil)
        (ok (= 2 (length temps)) "Two temps (key and hash-table)")
        (ok (= 2 (length vals)) "Two vals")
        (ok (= 1 (length stores)) "One store")
        (ok (eq (first access-form) 'gethash)
            "Access form uses gethash")))
    (testing "With default"
      (multiple-value-bind (temps vals stores store-form access-form)
          (funcall expander '(gethash key ht default) nil)
        (ok (= 3 (length temps)) "Three temps (key, ht, default)")
        (ok (= 3 (length vals)) "Three vals")))))

(deftest test-symbol-value-setf-expander ()
  "Test SYMBOL-VALUE setf expander (T024)."
  (let ((expander (make-symbol-value-setf-expander)))
    (multiple-value-bind (temps vals stores store-form access-form)
        (funcall expander '(symbol-value sym) nil)
      (ok (= 1 (length temps)) "One temp for symbol")
      (ok (= 1 (length vals)) "One val")
      (ok (= 1 (length stores)) "One store")
      (ok (member 'set (flatten store-form))
          "Store form uses set")
      (ok (eq (first access-form) 'symbol-value)
          "Access form uses symbol-value"))))

;;; ============================================================
;;; Phase 8: User Story 6 - define-setf-expander Tests (T089, T091-T092)
;;; ============================================================

(deftest test-define-setf-expander-registration ()
  "Test define-setf-expander* registers expander in global registry (T089)."
  (testing "Expander is callable after registration"
    ;; The standard expanders are already registered
    (let ((car-expander (get-setf-expander *global-setf-expander-registry* 'car)))
      (ok (functionp car-expander) "CAR expander is a function")
      (multiple-value-bind (temps vals stores store-form access-form)
          (funcall car-expander '(car x) nil)
        (ok (listp temps) "Returns temps list")
        (ok (listp stores) "Returns stores list")))))

(deftest test-defsetf-short-form ()
  "Test defsetf* short form creates correct expander (T091)."
  (testing "Short form expander"
    ;; Test the short form expander factory
    (let ((expander (clysm/lib/setf-expanders:make-short-form-setf-expander 'my-accessor 'my-setter)))
      (ok (functionp expander) "Short form creates a function")
      (multiple-value-bind (temps vals stores store-form access-form)
          (funcall expander '(my-accessor arg1 arg2) nil)
        (ok (= 2 (length temps)) "Two temps for two args")
        (ok (= 2 (length vals)) "Two vals")
        (ok (= 1 (length stores)) "One store")
        (ok (eq (first store-form) 'my-setter) "Store form uses setter")
        (ok (eq (first access-form) 'my-accessor) "Access form uses accessor")))))

(deftest test-defsetf-long-form ()
  "Test defsetf* long form creates correct expander (T092)."
  (testing "Long form expander"
    ;; Test the long form expander factory
    (let ((expander (clysm/lib/setf-expanders:make-long-form-setf-expander
                     'my-reader
                     '(seq start)  ; lambda-list
                     '(new-val)    ; store-vars
                     '(`(my-writer ,seq ,start ,new-val)))))  ; body
      (ok (functionp expander) "Long form creates a function")
      (multiple-value-bind (temps vals stores store-form access-form)
          (funcall expander '(my-reader s 0) nil)
        (ok (= 2 (length temps)) "Two temps for seq and start")
        (ok (= 2 (length vals)) "Two vals")
        (ok (= 1 (length stores)) "One store")
        (ok (eq (first access-form) 'my-reader) "Access form uses accessor")))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun flatten (tree)
  "Flatten a tree into a list."
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (append (flatten (car tree))
               (flatten (cdr tree))))))
