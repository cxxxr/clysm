;;;; sections-test.lisp - Wasm section structure tests

(in-package #:clysm/tests/contract/sections)

(deftest test-section-ids
  (ok (= 0 +section-id-custom+))
  (ok (= 1 +section-id-type+))
  (ok (= 3 +section-id-function+))
  (ok (= 10 +section-id-code+))
  (ok (= 13 +section-id-tag+)))

(deftest test-make-section
  (let ((section (make-section :id 1 :content #())))
    (ok (= 1 (section-id section)))
    (ok (equalp #() (section-content section)))))

(deftest test-encode-section
  (let* ((section (make-section :id 1 :content #(1 2 3)))
         (encoded (encode-section section)))
    ;; Section ID
    (ok (= 1 (aref encoded 0)))
    ;; Size (3 in LEB128)
    (ok (= 3 (aref encoded 1)))
    ;; Content
    (ok (= 1 (aref encoded 2)))
    (ok (= 2 (aref encoded 3)))
    (ok (= 3 (aref encoded 4)))))

(deftest test-section-order-valid
  (let ((sections (list (make-section :id 1 :content #())
                        (make-section :id 3 :content #())
                        (make-section :id 10 :content #()))))
    (ok (validate-section-order sections))))

(deftest test-section-order-invalid
  (let ((sections (list (make-section :id 3 :content #())
                        (make-section :id 1 :content #()))))
    (ok (signals (validate-section-order sections)))))
