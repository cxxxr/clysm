;;; tests/link/run.sh: a heap image whose compiled function has the bignum
;;; kernels' code (SIMD, 64-bit loads of the linear memory), for host/link.mjs
;;; (CL-USER::*LINK-TEST-IMAGE*: the file, defined by run.sh)
(defun big-square (n) (* (expt 3 n) (expt 3 n)))
(sys::save-image cl-user::*link-test-image*
  (lambda () (print (integer-length (big-square 4000))) (terpri)))
