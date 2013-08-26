(defpackage "SB-MPFR-TESTS"
  (:use "COMMON-LISP" "SB-RT"))

(in-package "SB-MPFR-TESTS")

(defun test ()
  (let ((sb-mpfr:*mpfr-rnd* :MPFR_RNDD))
    (sb-mpfr:with-precision 200
      (let ((u (sb-mpfr:coerce 1.0 'sb-mpfr:mpfr-float)))
        (loop for i from 1 to 100
              for v = u then (sb-mpfr:mul v i) ; :MPFR_RNDU
              collect (sb-mpfr:div u v)
                into vals
              finally (return (sb-mpfr:sum (cons u vals))))))))
