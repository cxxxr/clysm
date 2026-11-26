;;;; tools/build-many.lisp — compile many programs with the SBCL-hosted compiler
;;;; usage: sbcl --script tools/build-many.lisp [--shake] [--thrown-exits] [-j N] [--timeout SECONDS]
;;;;          [--definition-seconds SECONDS] JOBS
;;;; JOBS is a file of lines "OUT.wasm LOG FILE...": each program is compiled
;;;; as `tools/build.lisp [--shake] [--thrown-exits] OUT.wasm FILE...` would,
;;;; its messages written to LOG.  The runtime is compiled once; each program
;;;; is then compiled in a process forked from there (N at a time, the number
;;;; of processors by default), which is what takes most of the time of a
;;;; small program.  One line "@@ OUT.wasm STATUS" is printed per program
;;;; (STATUS 0 when it was compiled, "timeout" when it took more than
;;;; SECONDS, 600 by default).  --definition-seconds is how long the host may
;;;; take to compile one definition of a program before it is compiled again
;;;; in the way that always ends (*HOST-DEFINITION-SECONDS*, 20 by default).
(require :sb-posix)
(load (merge-pathnames "../load.lisp" *load-truename*))
(in-package :clysm)

(defun build-many-split (line)
  (let ((words nil) (start nil))
    (dotimes (i (length line))
      (if (member (char line i) '(#\Space #\Tab))
          (when start (push (subseq line start i) words) (setf start nil))
          (unless start (setf start i))))
    (when start (push (subseq line start) words))
    (nreverse words)))

(defun build-many-processors ()
  (or (ignore-errors
       (with-open-file (s "/proc/cpuinfo")
         (loop for line = (read-line s nil) while line
               count (eql 0 (search "processor" line)))))
      4))

(defun build-many-run (jobs nprocs timeout)
  "Fork a process per job of JOBS ((out log . files)), NPROCS at a time.
Returns in a child, with its job; the parent prints the status of each job
and exits."
  (let ((pending jobs) (running nil))   ; running: ((pid start out) ...)
    (flet ((report (out status) (format t "@@ ~A ~A~%" out status) (finish-output)))
      (loop while (or pending running)
            do (loop while (and pending (< (length running) nprocs))
                     do (let* ((job (pop pending))
                               (pid (progn (finish-output) (finish-output *error-output*) (sb-posix:fork))))
                          (when (zerop pid)
                            (let ((fd (sb-posix:open (second job)
                                                     (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-trunc)
                                                     #o644)))
                              (sb-posix:dup2 fd 1) (sb-posix:dup2 fd 2) (sb-posix:close fd))
                            (return-from build-many-run job))
                          (push (list pid (get-internal-real-time) (first job)) running)))
               (let ((progress nil))
                 (dolist (r running)
                   (multiple-value-bind (pid status) (sb-posix:waitpid (first r) sb-posix:wnohang)
                     (cond ((and pid (plusp pid))
                            (setf running (remove r running) progress t)
                            (report (third r)
                                    (if (sb-posix:wifexited status) (sb-posix:wexitstatus status) "signal")))
                           ((> (- (get-internal-real-time) (second r))
                               (* timeout internal-time-units-per-second))
                            (sb-posix:kill (first r) sb-posix:sigkill)
                            (sb-posix:waitpid (first r) 0)
                            (setf running (remove r running) progress t)
                            (report (third r) "timeout")))))
                 (unless progress (sleep 0.02))))
      (sb-ext:exit :code 0 :abort t))))

(let ((args (cdr sb-ext:*posix-argv*)) (shake nil) (nprocs nil) (timeout 600))
  (loop (cond ((equal (first args) "--shake") (pop args) (setf shake t))
              ((equal (first args) "--thrown-exits") (pop args) (setf *thrown-exits* t))
              ((equal (first args) "-j") (pop args) (setf nprocs (parse-integer (pop args))))
              ((equal (first args) "--timeout") (pop args) (setf timeout (parse-integer (pop args))))
              ((equal (first args) "--definition-seconds") (pop args)
               (setf *host-definition-seconds* (parse-integer (pop args))))
              (t (return))))
  (let ((jobs (with-open-file (s (first args))
                (loop for line = (read-line s nil) while line
                      for words = (build-many-split line)
                      when (>= (length words) 3) collect words)))
        (job nil))
    (unless jobs (sb-ext:exit :code 0))
    (let ((*after-runtime-hook*
            (lambda (files output)
              (declare (ignore files output))
              (setf job (build-many-run jobs (or nprocs (build-many-processors)) timeout))
              (values (cddr job) (first job)))))
      ;; (in the child, with its job)
      (handler-case
          (let ((n (compile-program nil :output "unused.wasm" :shake shake
                                        :runtime-dir (merge-pathnames "../runtime/" *load-truename*))))
            (format *error-output* "~&; wrote ~A (~D bytes)~%" (first job) n))
        (error (e) (format *error-output* "~&COMPILE ERROR: ~A~%" e)
          (finish-output *error-output*) (sb-ext:exit :code 1 :abort t))))
    (finish-output) (finish-output *error-output*)
    (sb-ext:exit :code 0 :abort t)))
