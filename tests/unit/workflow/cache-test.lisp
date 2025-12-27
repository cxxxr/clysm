;;;; cache-test.lisp - Unit tests for compilation cache
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; T035: Unit test for cache load/save

(defpackage #:clysm/tests/unit/workflow/cache-test
  (:use #:cl #:rove #:clysm/workflow))

(in-package #:clysm/tests/unit/workflow/cache-test)

;;; ============================================================
;;; T035: Unit tests for cache operations
;;; ============================================================

(defun temp-cache-dir ()
  "Create a temporary cache directory for testing."
  (let ((dir (uiop:merge-pathnames*
              (format nil "clysm-cache-test-~A/" (random 100000))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun cleanup-cache-dir (dir)
  "Clean up temporary cache directory."
  (when (uiop:directory-exists-p dir)
    (uiop:delete-directory-tree dir :validate t)))

(deftest create-empty-cache-test
  "Test creating an empty cache."
  (let ((cache (create-empty-cache "/tmp/project")))
    (testing "version is current"
      (ok (= (compilation-cache-version cache) clysm/workflow::*cache-version*)))
    (testing "modules hash table is empty"
      (ok (= (hash-table-count (compilation-cache-modules cache)) 0)))
    (testing "project-root is set"
      (ok (search "/tmp/project" (compilation-cache-project-root cache))))))

(deftest save-and-load-cache-test
  "Test saving and loading cache."
  (let* ((cache-dir (temp-cache-dir))
         (cache (create-empty-cache (uiop:getcwd))))
    (unwind-protect
         (progn
           ;; Add a module
           (setf (gethash "test.lisp" (compilation-cache-modules cache))
                 (make-cached-module :path "test.lisp"
                                     :mtime 12345
                                     :hash "abc123"
                                     :compiled-size 100))

           ;; Save
           (save-cache cache cache-dir)

           ;; Load
           (let ((loaded (load-cache cache-dir (uiop:getcwd))))
             (testing "loaded cache is not nil"
               (ok loaded))
             (when loaded
               (testing "loaded cache has correct version"
                 (ok (= (compilation-cache-version loaded) 1)))
               (testing "loaded cache has module"
                 (ok (gethash "test.lisp" (compilation-cache-modules loaded)))))))

      ;; Cleanup
      (cleanup-cache-dir cache-dir))))

(deftest find-dirty-modules-test
  "Test finding dirty modules."
  (let* ((cache (create-empty-cache "/tmp"))
         (mod1 (make-source-module :relative-path "a.lisp" :mtime 100))
         (mod2 (make-source-module :relative-path "b.lisp" :mtime 200)))

    ;; Add cached module with same mtime
    (setf (gethash "a.lisp" (compilation-cache-modules cache))
          (make-cached-module :path "a.lisp" :mtime 100))

    (testing "unchanged module is not dirty"
      (let ((dirty (find-dirty-modules (list mod1) cache)))
        (ok (null dirty))))

    (testing "changed module is dirty"
      (let ((dirty (find-dirty-modules (list mod2) cache)))
        (ok (= (length dirty) 1))))))

(deftest prune-deleted-files-test
  "Test pruning deleted files from cache."
  (let ((cache (create-empty-cache "/tmp")))
    ;; Add modules to cache
    (setf (gethash "a.lisp" (compilation-cache-modules cache))
          (make-cached-module :path "a.lisp"))
    (setf (gethash "b.lisp" (compilation-cache-modules cache))
          (make-cached-module :path "b.lisp"))

    ;; Only a.lisp exists now
    (let* ((mod1 (make-source-module :relative-path "a.lisp"))
           (pruned (prune-deleted-files cache (list mod1))))
      (testing "one file was pruned"
        (ok (= pruned 1)))
      (testing "a.lisp still in cache"
        (ok (gethash "a.lisp" (compilation-cache-modules cache))))
      (testing "b.lisp removed from cache"
        (ok (null (gethash "b.lisp" (compilation-cache-modules cache))))))))
