;;;; clos-test.lisp - CLOS unit tests (Phase 10 - T207)
(in-package #:clysm/tests/unit/clos)

;;; Class Metaobject Tests (T207)

(deftest class-metaobject-structure
  (testing "standard-class has required slots"
    (let ((class (clysm/clos/mop:make-standard-class
                  :name 'point
                  :superclasses nil
                  :slots '((x :initarg :x) (y :initarg :y))
                  :precedence-list nil)))
      (ok (eq 'point (clysm/clos/mop:class-name class)))
      (ok (null (clysm/clos/mop:class-superclasses class)))
      (ok (= 2 (length (clysm/clos/mop:class-slots class))))))

  (testing "class with superclass"
    (let* ((parent (clysm/clos/mop:make-standard-class
                    :name 'shape
                    :superclasses nil
                    :slots nil))
           (child (clysm/clos/mop:make-standard-class
                   :name 'circle
                   :superclasses (list parent)
                   :slots '((radius :initarg :radius)))))
      (ok (= 1 (length (clysm/clos/mop:class-superclasses child))))
      (ok (eq 'shape (clysm/clos/mop:class-name
                      (first (clysm/clos/mop:class-superclasses child))))))))

;;; Class Registry Tests

(deftest class-registry
  (testing "find-class returns registered class"
    (let ((class (clysm/clos/mop:make-standard-class
                  :name 'test-class
                  :superclasses nil
                  :slots nil)))
      (clysm/clos/mop:register-class 'test-class class)
      (ok (eq class (clysm/clos/mop:find-class* 'test-class)))))

  (testing "find-class returns nil for unknown class"
    (ok (null (clysm/clos/mop:find-class* 'nonexistent-class-12345 nil)))))

;;; Class Precedence List Tests

(deftest class-precedence-list
  (testing "single class has itself in cpl"
    (let ((class (clysm/clos/mop:make-standard-class
                  :name 'simple
                  :superclasses nil
                  :slots nil)))
      (clysm/clos/mop:compute-class-precedence-list class)
      (ok (member class (clysm/clos/mop:class-precedence-list class)))))

  (testing "subclass comes before superclass in cpl"
    (let* ((parent (clysm/clos/mop:make-standard-class
                    :name 'parent
                    :superclasses nil
                    :slots nil))
           (child (clysm/clos/mop:make-standard-class
                   :name 'child
                   :superclasses (list parent)
                   :slots nil)))
      (clysm/clos/mop:compute-class-precedence-list parent)
      (clysm/clos/mop:compute-class-precedence-list child)
      (let ((cpl (clysm/clos/mop:class-precedence-list child)))
        (ok (< (position child cpl) (position parent cpl)))))))

;;; Slot Definition Tests

(deftest slot-definition
  (testing "slot-definition has name"
    (let ((slot (clysm/clos/mop:make-slot-definition
                 :name 'x
                 :initarg :x
                 :initform nil
                 :accessor 'x)))
      (ok (eq 'x (clysm/clos/mop:slot-definition-name slot)))))

  (testing "slot-definition has initarg"
    (let ((slot (clysm/clos/mop:make-slot-definition
                 :name 'x
                 :initarg :x)))
      (ok (eq :x (clysm/clos/mop:slot-definition-initarg slot)))))

  (testing "slot-definition has initform"
    (let ((slot (clysm/clos/mop:make-slot-definition
                 :name 'x
                 :initform 0)))
      (ok (eql 0 (clysm/clos/mop:slot-definition-initform slot))))))

;;; Instance Structure Tests

(deftest instance-structure
  (testing "instance has class reference"
    (let* ((class (clysm/clos/mop:make-standard-class
                   :name 'point
                   :superclasses nil
                   :slots nil))
           (instance (clysm/clos/mop:make-instance-struct class 2)))
      (ok (eq class (clysm/clos/mop:instance-class instance)))))

  (testing "instance has slot storage"
    (let* ((class (clysm/clos/mop:make-standard-class
                   :name 'point
                   :superclasses nil
                   :slots nil))
           (instance (clysm/clos/mop:make-instance-struct class 2)))
      (ok (= 2 (length (clysm/clos/mop:instance-slots instance)))))))

