;;;; clos-test.lisp - CLOS integration tests (Phase 10 - T208-T214)
(in-package #:clysm/tests/integration/clos)

;;; defclass Tests (T208)

(deftest defclass-parsing
  (testing "parse simple defclass"
    (let ((result (clysm/clos/defclass:parse-defclass
                   '(defclass point ()
                     ((x :initarg :x)
                      (y :initarg :y))))))
      (ok result)
      (ok (eq 'point (clysm/clos/defclass:defclass-result-name result)))
      (ok (= 2 (length (clysm/clos/defclass:defclass-result-slots result))))))

  (testing "parse defclass with superclass"
    (let ((result (clysm/clos/defclass:parse-defclass
                   '(defclass colored-point (point)
                     ((color :initarg :color))))))
      (ok result)
      (ok (equal '(point) (clysm/clos/defclass:defclass-result-superclasses result)))))

  (testing "parse slot with accessor"
    (let ((result (clysm/clos/defclass:parse-defclass
                   '(defclass point ()
                     ((x :initarg :x :accessor point-x))))))
      (let ((slot (first (clysm/clos/defclass:defclass-result-slots result))))
        (ok (eq 'point-x (clysm/clos/defclass:slot-spec-accessor slot))))))

  (testing "parse slot with initform"
    (let ((result (clysm/clos/defclass:parse-defclass
                   '(defclass counter ()
                     ((count :initform 0))))))
      (let ((slot (first (clysm/clos/defclass:defclass-result-slots result))))
        (ok (eql 0 (clysm/clos/defclass:slot-spec-initform slot)))))))

;;; make-instance Tests (T209)

(deftest make-instance-basic
  (testing "make-instance creates instance"
    ;; First define a class
    (clysm/clos/defclass:define-class*
     '(defclass test-point ()
       ((x :initarg :x)
        (y :initarg :y))))
    (let ((instance (clysm/clos/instance:make-instance* 'test-point :x 3 :y 4)))
      (ok instance)
      (ok (clysm/clos/mop:instance-p instance))))

  (testing "make-instance with initargs"
    (clysm/clos/defclass:define-class*
     '(defclass test-point2 ()
       ((x :initarg :x)
        (y :initarg :y))))
    (let ((instance (clysm/clos/instance:make-instance* 'test-point2 :x 10 :y 20)))
      (ok (= 10 (clysm/clos/slot-access:slot-value* instance 'x)))
      (ok (= 20 (clysm/clos/slot-access:slot-value* instance 'y)))))

  (testing "make-instance with initform"
    (clysm/clos/defclass:define-class*
     '(defclass test-counter ()
       ((count :initform 0))))
    (let ((instance (clysm/clos/instance:make-instance* 'test-counter)))
      (ok (= 0 (clysm/clos/slot-access:slot-value* instance 'count))))))

;;; Slot Access Tests (T210)

(deftest slot-access
  (testing "slot-value reads slot"
    (clysm/clos/defclass:define-class*
     '(defclass slot-test ()
       ((value :initarg :value))))
    (let ((instance (clysm/clos/instance:make-instance* 'slot-test :value 42)))
      (ok (= 42 (clysm/clos/slot-access:slot-value* instance 'value)))))

  (testing "setf slot-value writes slot"
    (clysm/clos/defclass:define-class*
     '(defclass slot-test2 ()
       ((value :initarg :value))))
    (let ((instance (clysm/clos/instance:make-instance* 'slot-test2 :value 0)))
      (clysm/clos/slot-access:set-slot-value* instance 'value 100)
      (ok (= 100 (clysm/clos/slot-access:slot-value* instance 'value))))))

;;; Generic Function Tests (T211)

(deftest generic-function-basic
  (testing "defgeneric creates generic function"
    (let ((gf (clysm/clos/generic:defgeneric* 'test-gf '(x))))
      (ok gf)
      (ok (clysm/clos/generic:generic-function-p gf))))

  (testing "generic function has name"
    (let ((gf (clysm/clos/generic:defgeneric* 'named-gf '(x y))))
      (ok (eq 'named-gf (clysm/clos/generic:gf-name gf)))))

  (testing "generic function has lambda-list"
    (let ((gf (clysm/clos/generic:defgeneric* 'lambda-gf '(a b c))))
      (ok (equal '(a b c) (clysm/clos/generic:gf-lambda-list gf))))))

;;; defmethod Tests (T212)

(deftest defmethod-parsing
  (testing "parse simple defmethod"
    (let ((result (clysm/clos/defmethod:parse-defmethod
                   '(defmethod speak ((obj animal))
                     "woof"))))
      (ok result)
      (ok (eq 'speak (clysm/clos/defmethod:method-result-name result)))))

  (testing "parse defmethod with specializer"
    (let ((result (clysm/clos/defmethod:parse-defmethod
                   '(defmethod area ((shape circle))
                     (* pi (expt (radius shape) 2))))))
      (let ((specializers (clysm/clos/defmethod:method-result-specializers result)))
        (ok (equal '(circle) specializers)))))

  (testing "parse defmethod with qualifier"
    (let ((result (clysm/clos/defmethod:parse-defmethod
                   '(defmethod speak :before ((obj animal))
                     (format t "~A says: " (name obj))))))
      (ok (eq :before (clysm/clos/defmethod:method-result-qualifier result))))))

;;; Method Dispatch Tests (T213)

(deftest method-dispatch
  (testing "dispatch calls most specific method"
    ;; Define classes
    (clysm/clos/defclass:define-class*
     '(defclass dispatch-animal () ()))
    (clysm/clos/defclass:define-class*
     '(defclass dispatch-dog (dispatch-animal) ()))
    ;; Define generic function
    (let ((gf (clysm/clos/generic:defgeneric* 'dispatch-speak '(obj))))
      ;; Add methods
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(dispatch-animal)
        :function (lambda (obj) (declare (ignore obj)) "animal sound")))
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(dispatch-dog)
        :function (lambda (obj) (declare (ignore obj)) "woof")))
      ;; Test dispatch
      (let ((dog (clysm/clos/instance:make-instance* 'dispatch-dog)))
        (ok (equal "woof" (clysm/clos/dispatch:dispatch gf dog)))))))

;;; Method Combination Tests (T214)

(deftest method-combination
  (testing ":before method runs before primary"
    (clysm/clos/defclass:define-class*
     '(defclass comb-test () ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'comb-speak '(obj)))
          (log nil))
      ;; Primary method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(comb-test)
        :function (lambda (obj) (declare (ignore obj)) (push :primary log))))
      ;; Before method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(comb-test)
        :qualifier :before
        :function (lambda (obj) (declare (ignore obj)) (push :before log))))
      ;; Execute
      (let ((instance (clysm/clos/instance:make-instance* 'comb-test)))
        (clysm/clos/dispatch:dispatch gf instance))
      ;; :before should be first in reverse order
      (ok (eq :before (second log)))
      (ok (eq :primary (first log)))))

  (testing ":after method runs after primary"
    (clysm/clos/defclass:define-class*
     '(defclass comb-test2 () ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'comb-speak2 '(obj)))
          (log nil))
      ;; Primary method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(comb-test2)
        :function (lambda (obj) (declare (ignore obj)) (push :primary log))))
      ;; After method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(comb-test2)
        :qualifier :after
        :function (lambda (obj) (declare (ignore obj)) (push :after log))))
      ;; Execute
      (let ((instance (clysm/clos/instance:make-instance* 'comb-test2)))
        (clysm/clos/dispatch:dispatch gf instance))
      ;; :after should be last in reverse order
      (ok (eq :after (first log)))
      (ok (eq :primary (second log)))))

  (testing "call-next-method invokes next method"
    (clysm/clos/defclass:define-class*
     '(defclass cnm-parent () ()))
    (clysm/clos/defclass:define-class*
     '(defclass cnm-child (cnm-parent) ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'cnm-greet '(obj))))
      ;; Parent method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(cnm-parent)
        :function (lambda (obj) (declare (ignore obj)) "parent")))
      ;; Child method calls next method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(cnm-child)
        :function (lambda (obj)
                    (declare (ignore obj))
                    (concatenate 'string "child-" (clysm/clos/combination:call-next-method*)))))
      (let ((child (clysm/clos/instance:make-instance* 'cnm-child)))
        (ok (equal "child-parent" (clysm/clos/dispatch:dispatch gf child)))))))

;;; Acceptance Test (T244)

(deftest clos-acceptance
  (testing "(make-instance 'point :x 3 :y 4) returns instance"
    (clysm/clos/defclass:define-class*
     '(defclass point ()
       ((x :initarg :x :accessor point-x)
        (y :initarg :y :accessor point-y))))
    (let ((p (clysm/clos/instance:make-instance* 'point :x 3 :y 4)))
      (ok p)
      (ok (clysm/clos/mop:instance-p p))
      (ok (= 3 (clysm/clos/slot-access:slot-value* p 'x)))
      (ok (= 4 (clysm/clos/slot-access:slot-value* p 'y))))))

