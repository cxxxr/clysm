;;;; clos-edge-test.lisp - CLOS edge case tests (T262-T264)
(in-package #:clysm/tests/integration/clos-edge)

;;; Method ordering tests (T263)

(deftest method-ordering
  (testing ":before methods run most-specific-first"
    (clysm/clos/defclass:define-class*
     '(defclass order-parent () ()))
    (clysm/clos/defclass:define-class*
     '(defclass order-child (order-parent) ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'order-test '(obj)))
          (log nil))
      ;; Parent :before
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(order-parent)
        :qualifier :before
        :function (lambda (obj) (declare (ignore obj)) (push :parent-before log))))
      ;; Child :before
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(order-child)
        :qualifier :before
        :function (lambda (obj) (declare (ignore obj)) (push :child-before log))))
      ;; Primary
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(order-parent)
        :function (lambda (obj) (declare (ignore obj)) (push :primary log))))
      ;; Execute
      (clysm/clos/dispatch:dispatch gf (clysm/clos/instance:make-instance* 'order-child))
      ;; Child :before should run first (most specific)
      (ok (eq :child-before (third log)))))

  (testing ":after methods run least-specific-first"
    (clysm/clos/defclass:define-class*
     '(defclass after-parent () ()))
    (clysm/clos/defclass:define-class*
     '(defclass after-child (after-parent) ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'after-test '(obj)))
          (log nil))
      ;; Parent :after
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(after-parent)
        :qualifier :after
        :function (lambda (obj) (declare (ignore obj)) (push :parent-after log))))
      ;; Child :after
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(after-child)
        :qualifier :after
        :function (lambda (obj) (declare (ignore obj)) (push :child-after log))))
      ;; Primary
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(after-parent)
        :function (lambda (obj) (declare (ignore obj)) (push :primary log))))
      ;; Execute
      (clysm/clos/dispatch:dispatch gf (clysm/clos/instance:make-instance* 'after-child))
      ;; Parent :after should run first (least specific)
      (ok (eq :parent-after (second log)))
      (ok (eq :child-after (first log))))))

;;; call-next-method edge cases (T264)

(deftest call-next-method-edge-cases
  (testing "call-next-method with no next method signals error"
    (clysm/clos/defclass:define-class*
     '(defclass cnm-edge () ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'cnm-edge-test '(obj))))
      ;; Single primary method that calls next-method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(cnm-edge)
        :function (lambda (obj)
                    (declare (ignore obj))
                    ;; This should error - no next method
                    (clysm/clos/combination:call-next-method*))))
      (ok (signals (clysm/clos/dispatch:dispatch gf
                     (clysm/clos/instance:make-instance* 'cnm-edge))))))

  (testing "next-method-p returns nil when no next method"
    (clysm/clos/defclass:define-class*
     '(defclass nmp-test () ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'nmp-test '(obj)))
          (has-next nil))
      ;; Single primary method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(nmp-test)
        :function (lambda (obj)
                    (declare (ignore obj))
                    (setf has-next (clysm/clos/combination:next-method-p*)))))
      (clysm/clos/dispatch:dispatch gf (clysm/clos/instance:make-instance* 'nmp-test))
      (ok (null has-next))))

  (testing "next-method-p returns t when there is a next method"
    (clysm/clos/defclass:define-class*
     '(defclass nmp-parent () ()))
    (clysm/clos/defclass:define-class*
     '(defclass nmp-child (nmp-parent) ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'nmp-test2 '(obj)))
          (has-next nil))
      ;; Parent method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(nmp-parent)
        :function (lambda (obj) (declare (ignore obj)) :parent)))
      ;; Child method checks for next
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(nmp-child)
        :function (lambda (obj)
                    (declare (ignore obj))
                    (setf has-next (clysm/clos/combination:next-method-p*))
                    :child)))
      (clysm/clos/dispatch:dispatch gf (clysm/clos/instance:make-instance* 'nmp-child))
      (ok has-next))))

;;; :around method edge cases

(deftest around-method-edge-cases
  (testing ":around can skip primary method"
    (clysm/clos/defclass:define-class*
     '(defclass around-skip () ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'around-skip-test '(obj)))
          (primary-called nil))
      ;; Primary
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(around-skip)
        :function (lambda (obj) (declare (ignore obj)) (setf primary-called t) :primary)))
      ;; Around that doesn't call-next-method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(around-skip)
        :qualifier :around
        :function (lambda (obj) (declare (ignore obj)) :around-only)))
      (let ((result (clysm/clos/dispatch:dispatch gf
                      (clysm/clos/instance:make-instance* 'around-skip))))
        (ok (eq :around-only result))
        (ok (null primary-called)))))

  (testing ":around wraps :before and :after"
    (clysm/clos/defclass:define-class*
     '(defclass around-wrap () ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'around-wrap-test '(obj)))
          (log nil))
      ;; Before
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(around-wrap)
        :qualifier :before
        :function (lambda (obj) (declare (ignore obj)) (push :before log))))
      ;; Primary
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(around-wrap)
        :function (lambda (obj) (declare (ignore obj)) (push :primary log))))
      ;; After
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(around-wrap)
        :qualifier :after
        :function (lambda (obj) (declare (ignore obj)) (push :after log))))
      ;; Around calls next-method
      (clysm/clos/defmethod:add-method*
       gf
       (clysm/clos/defmethod:make-method*
        :specializers '(around-wrap)
        :qualifier :around
        :function (lambda (obj)
                    (declare (ignore obj))
                    (push :around-start log)
                    (clysm/clos/combination:call-next-method*)
                    (push :around-end log))))
      (clysm/clos/dispatch:dispatch gf (clysm/clos/instance:make-instance* 'around-wrap))
      ;; Verify order: around-start, before, primary, after, around-end
      (ok (equal '(:around-end :after :primary :before :around-start) log)))))

;;; No applicable method edge case

(deftest no-applicable-method
  (testing "dispatch with no matching method signals error"
    (clysm/clos/defclass:define-class*
     '(defclass no-method-class () ()))
    (let ((gf (clysm/clos/generic:defgeneric* 'no-method-test '(obj))))
      ;; No methods added
      (ok (signals (clysm/clos/dispatch:dispatch gf
                     (clysm/clos/instance:make-instance* 'no-method-class)))))))

;;; Slot access edge cases

(deftest slot-access-edge-cases
  (testing "slot-value on nonexistent slot signals error"
    (clysm/clos/defclass:define-class*
     '(defclass slot-edge-class ()
       ((existing-slot :initform 0))))
    (let ((instance (clysm/clos/instance:make-instance* 'slot-edge-class)))
      (ok (signals (clysm/clos/slot-access:slot-value* instance 'nonexistent-slot-xyz))))))

