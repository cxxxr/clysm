;;;; stage2-gen.lisp - CLI entry point for Stage 2 generation
;;;;
;;;; Part of Feature 040: Fixed-Point Verification
;;;; Generates Stage 2 binary by running Stage 1 on wasmtime
;;;;
;;;; Usage:
;;;;   sbcl --load build/stage2-gen.lisp
;;;;
;;;; Environment variables:
;;;;   STAGE1_PATH - Path to Stage 1 binary (default: dist/clysm-stage1.wasm)
;;;;   STAGE2_PATH - Output path for Stage 2 (default: dist/clysm-stage2.wasm)
;;;;   SOURCE_DIR  - Source directory (default: src/clysm/)

(in-package #:cl-user)

;;; Load dependencies
(require :asdf)
(push (truename ".") asdf:*central-registry*)

;;; Load Clysm system
(handler-case
    (progn
      (asdf:load-system :clysm)
      (format t "~&Loaded clysm system~%"))
  (error (e)
    (format *error-output* "~&Error loading clysm: ~A~%" e)
    (uiop:quit 2)))

;;; Parse environment
(defun get-env-or-default (name default)
  "Get environment variable or return default."
  (or (uiop:getenv name) default))

(defparameter *stage1-path*
  (get-env-or-default "STAGE1_PATH" "dist/clysm-stage1.wasm"))

(defparameter *stage2-path*
  (get-env-or-default "STAGE2_PATH" "dist/clysm-stage2.wasm"))

(defparameter *source-dir*
  (get-env-or-default "SOURCE_DIR" "src/clysm/"))

;;; Main entry point
(defun main ()
  "Generate Stage 2 binary."
  (format t "~&=== Stage 2 Generation ===~%")
  (format t "Stage 1: ~A~%" *stage1-path*)
  (format t "Output:  ~A~%" *stage2-path*)
  (format t "Source:  ~A~%" *source-dir*)
  (format t "~%")

  ;; Check Stage 1 exists
  (unless (probe-file *stage1-path*)
    (format *error-output* "~&ERROR: Stage 1 not found: ~A~%" *stage1-path*)
    (format *error-output* "Run: sbcl --load build/stage1-gen.lisp~%")
    (uiop:quit 3))

  ;; Check wasmtime available
  (unless (clysm/stage1:wasmtime-available-p)
    (format *error-output* "~&ERROR: wasmtime not available~%")
    (format *error-output* "Install: curl https://wasmtime.dev/install.sh -sSf | bash~%")
    (uiop:quit 3))

  ;; Generate Stage 2
  (multiple-value-bind (success-p stage2-info error-msg)
      (clysm/stage2:generate-stage2
       :stage1-path *stage1-path*
       :output-path *stage2-path*
       :source-dir *source-dir*)

    (if success-p
        (progn
          (format t "~%=== SUCCESS ===~%")
          (format t "Stage 2 generated: ~A~%"
                  (clysm/stage1:binary-info-size-bytes stage2-info))
          (uiop:quit 0))
        (progn
          (format *error-output* "~%=== PARTIAL/FAILURE ===~%")
          (when error-msg
            (format *error-output* "~A~%" error-msg))
          (when (and stage2-info
                     (probe-file *stage2-path*))
            (format t "Partial Stage 2 generated: ~A bytes~%"
                    (clysm/stage1:binary-info-size-bytes stage2-info)))
          (uiop:quit 2)))))

;;; Run
(main)
