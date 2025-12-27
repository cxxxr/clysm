;;;; cache.lisp - Compilation cache persistence
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Implements cache load/save and dirty module detection

(in-package #:clysm/workflow)

;;; ============================================================
;;; Cache file format (S-expression)
;;; ============================================================
;;;
;;; File: .clysm-cache/compilation-cache.sexp
;;; Format:
;;; (:version 1
;;;  :timestamp "2025-12-27T12:00:00Z"
;;;  :project-root "/home/user/project"
;;;  :modules
;;;  (("src/foo.lisp" :mtime 12345 :hash "abc..." :compiled-size 1234)
;;;   ("src/bar.lisp" :mtime 12346 :hash "def..." :compiled-size 2345))
;;;  :dependency-order ("src/foo.lisp" "src/bar.lisp"))

(defparameter *cache-version* 1
  "Current cache format version.")

(defparameter *cache-filename* "compilation-cache.sexp"
  "Name of the cache file within the cache directory.")

;;; ============================================================
;;; T043: load-cache - Load cache from disk
;;; ============================================================

(defun load-cache (cache-dir &optional (project-root (uiop:getcwd)))
  "Load compilation cache from CACHE-DIR.
   Returns a compilation-cache struct, or NIL if cache doesn't exist
   or is invalid/incompatible.

   PROJECT-ROOT is used to validate the cache belongs to this project."
  (let ((cache-path (uiop:merge-pathnames* *cache-filename* cache-dir)))
    (unless (file-exists-p cache-path)
      (return-from load-cache nil))

    (handler-case
        (let* ((data (with-open-file (stream cache-path :direction :input)
                       (read stream nil nil)))
               (version (getf data :version))
               (root (getf data :project-root)))

          ;; Validate version
          (unless (eql version *cache-version*)
            (return-from load-cache nil))

          ;; Validate project root matches
          (unless (equal root (namestring (truename project-root)))
            (return-from load-cache nil))

          ;; Build cache struct
          (let ((modules (make-hash-table :test 'equal)))
            (dolist (entry (getf data :modules))
              (let ((path (car entry))
                    (props (cdr entry)))
                (setf (gethash path modules)
                      (make-cached-module
                       :path path
                       :mtime (getf props :mtime 0)
                       :hash (getf props :hash "")
                       :compiled-size (getf props :compiled-size 0)
                       :compile-time-ms (getf props :compile-time-ms 0)))))

            (make-compilation-cache
             :version version
             :timestamp (getf data :timestamp "")
             :project-root root
             :modules modules
             :dependency-graph (getf data :dependency-order))))

      (error (e)
        (declare (ignore e))
        nil))))

;;; ============================================================
;;; T044: save-cache - Save cache to disk
;;; ============================================================

(defun save-cache (cache cache-dir)
  "Save CACHE to CACHE-DIR.
   Creates the cache directory if it doesn't exist."
  (let ((cache-path (uiop:merge-pathnames* *cache-filename* cache-dir)))
    ;; Ensure directory exists
    (ensure-directory cache-path)

    ;; Build module list for serialization
    (let ((module-list nil))
      (maphash (lambda (path cached)
                 (push (list path
                             :mtime (cached-module-mtime cached)
                             :hash (cached-module-hash cached)
                             :compiled-size (cached-module-compiled-size cached)
                             :compile-time-ms (cached-module-compile-time-ms cached))
                       module-list))
               (compilation-cache-modules cache))

      ;; Sort by path for deterministic output
      (setf module-list (sort module-list #'string< :key #'car))

      ;; Write cache file
      (with-open-file (stream cache-path :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
        (let ((*print-pretty* t)
              (*print-right-margin* 100))
          (pprint `(:version ,(compilation-cache-version cache)
                    :timestamp ,(current-iso-timestamp)
                    :project-root ,(compilation-cache-project-root cache)
                    :modules ,module-list
                    :dependency-order ,(compilation-cache-dependency-graph cache))
                  stream)
          (terpri stream))))))

(defun current-iso-timestamp ()
  "Return current time as ISO-8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

;;; ============================================================
;;; T045: find-dirty-modules - Find modules needing recompilation
;;; ============================================================

(defun find-dirty-modules (modules cache)
  "Compare MODULES against CACHE to find which need recompilation.
   Returns a list of source-module structs that are dirty.

   A module is dirty if:
   - Not in cache
   - mtime differs from cached mtime
   - hash differs from cached hash (if available)"
  (loop for mod in modules
        for path = (source-module-relative-path mod)
        for cached = (when cache
                       (gethash path (compilation-cache-modules cache)))
        when (or (null cached)
                 (/= (source-module-mtime mod) (cached-module-mtime cached))
                 (and (not (equal (source-module-hash mod) ""))
                      (not (equal (source-module-hash mod) (cached-module-hash cached)))))
        collect mod))

;;; ============================================================
;;; T046: prune-deleted-files - Remove deleted files from cache
;;; ============================================================

(defun prune-deleted-files (cache modules)
  "Remove entries from CACHE for files that no longer exist in MODULES.
   Returns the number of entries pruned."
  (let ((module-paths (make-hash-table :test 'equal))
        (pruned-count 0))

    ;; Build set of current module paths
    (dolist (mod modules)
      (setf (gethash (source-module-relative-path mod) module-paths) t))

    ;; Find and remove entries not in current modules
    (let ((to-remove nil))
      (maphash (lambda (path cached)
                 (declare (ignore cached))
                 (unless (gethash path module-paths)
                   (push path to-remove)))
               (compilation-cache-modules cache))

      (dolist (path to-remove)
        (remhash path (compilation-cache-modules cache))
        (incf pruned-count)))

    pruned-count))

(defun invalidate-module (cache path)
  "Remove PATH from CACHE, invalidating its compiled state."
  (remhash path (compilation-cache-modules cache)))

;;; ============================================================
;;; Cache update helpers
;;; ============================================================

(defun update-cache-for-module (cache mod result)
  "Update CACHE with compilation result RESULT for module MOD."
  (when (compilation-result-success-p result)
    (setf (gethash (source-module-relative-path mod)
                   (compilation-cache-modules cache))
          (make-cached-module
           :path (source-module-relative-path mod)
           :mtime (source-module-mtime mod)
           :hash (source-module-hash mod)
           :compiled-size (compilation-result-byte-count result)
           :compile-time-ms (compilation-result-compile-time-ms result)))))

(defun create-empty-cache (project-root)
  "Create a new empty compilation cache for PROJECT-ROOT."
  (make-compilation-cache
   :version *cache-version*
   :timestamp (current-iso-timestamp)
   :project-root (namestring (truename project-root))
   :modules (make-hash-table :test 'equal)
   :dependency-graph nil))
