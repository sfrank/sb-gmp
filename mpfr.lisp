(defpackage :sb-mpfr (:use "COMMON-LISP" "SB-ALIEN" "SB-C-CALL")
            (:export ;; parameters
                     #:*mpfr-precision*
                     #:*mpfr-rnd*
                     #:*mpfr-base*
                     ;; bignum float operations
                     #:make-mpfr-float
                     #:mpfr-float-to-string
                     ;; random number generation
                     ;; ...
                     ;; (un)installer functions
                     ;; #:install-mpfr-funs
                     ;; #:uninstall-mpfr-funs
                     ))

(in-package :sb-mpfr)

#-win32
(sb-alien::load-shared-object "libmpfr.so")

;;; types and initialization

(define-alien-type nil
    (struct mpfrfloat
            (mpfr_prec long)
            (mpfr_sign int)
            (mpfr_exp long)
            (mpfr_d (* unsigned-long))))

(define-alien-type mpfr_rnd_enum
  (enum mpfr_rnd
        (:mpfr_rndn 0)
        (:mpfr_rndz 1)
        (:mpfr_rndu 2)
        (:mpfr_rndd 3)
        (:mpfr_rnda 4)
        (:mpfr_rndf 5)
        (:mpfr_rndna -1)))

(declaim (inline mpfr_init2
                 mpfr_clear
                 mpfr_set_d
                 mpfr_set_nan
                 mpfr_set_inf
                 mpfr_set_zero
                 mpfr_get_default_prec
                 mpfr_get_str
                 mpfr_free_str))

(define-alien-routine mpfr_init2 void
  (x (* (struct mpfrfloat)))
  (precision long))

(define-alien-routine mpfr_clear void
  (x (* (struct mpfrfloat))))

;;; conversion functions

(define-alien-routine mpfr_set void
  (x (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_ui void
  (x (* (struct mpfrfloat)))
  (op unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_si void
  (x (* (struct mpfrfloat)))
  (op long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_flt void
  (x (* (struct mpfrfloat)))
  (op float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_d void
  (x (* (struct mpfrfloat)))
  (op double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_nan void
  (x (* (struct mpfrfloat))))

(define-alien-routine mpfr_set_inf void
  (x (* (struct mpfrfloat)))
  (sign int))

(define-alien-routine mpfr_set_zero void
  (x (* (struct mpfrfloat)))
  (sign int))

(define-alien-routine mpfr_get_default_prec long)

(define-alien-routine mpfr_get_str (* char)
  (str (* char))
  (exp (* long))
  (base int)
  (n unsigned)
  (x (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_get_flt float
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_get_d double
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_free_str void
  (str (* char)))

(define-alien-routine mpfr_set_str int
  (x (* (struct mpfrfloat)))
  (str c-string)
  (base int)
  (rnd mpfr_rnd_enum))

;;; arithmetic functions

(define-alien-routine mpfr_add int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_d int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_q int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_ui_sub int
  (r (* (struct mpfrfloat)))
  (op1 unsigned-long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_si_sub int
  (r (* (struct mpfrfloat)))
  (op1 long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_d_sub int
  (r (* (struct mpfrfloat)))
  (op1 double-float)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_d int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_z_sub int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct sb-gmp::gmpint)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_q int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_d int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_q int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sqr int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_ui_div int
  (r (* (struct mpfrfloat)))
  (op1 unsigned-long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_si_div int
  (r (* (struct mpfrfloat)))
  (op1 long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_d_div int
  (r (* (struct mpfrfloat)))
  (op1 double-float)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_d int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_q int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sqrt int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sqrt_ui int
  (r (* (struct mpfrfloat)))
  (op unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_rec_sqrt int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_cbrt int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_root int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (k unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_pow int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_pow_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_pow_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_pow_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_ui_pow_ui int
  (r (* (struct mpfrfloat)))
  (op1 unsigned-long)
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_ui_pow int
  (r (* (struct mpfrfloat)))
  (op1 unsigned-long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_neg int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_abs int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_dim int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

;;; lisp interface

(defparameter *mpfr-precision* (mpfr_get_default_prec))
(defparameter *mpfr-rnd* :mpfr_rndn)
(defparameter *mpfr-base* 10)

(defstruct (mpfr-float (:constructor %make-mpfr-float))
  (ref (make-alien (struct mpfrfloat))
   :type (alien (* (struct mpfrfloat))) :read-only t))

(defun make-mpfr-float (&optional (precision *mpfr-precision*))
  (declare (optimize (speed 3) (space 3)))
  (let* ((float (%make-mpfr-float))
         (ref (mpfr-float-ref float)))
    (mpfr_init2 ref precision)
    (sb-ext:finalize float (lambda ()
                             (declare (optimize (speed 3) (space 3) (safety 0)))
                             (mpfr_clear ref)
                             (free-alien ref)))
    float))


;;; printing and reader syntax

(defmethod print-object ((obj mpfr-float) stream)
  (multiple-value-bind (str exp)
      (mpfr-float-to-string obj)
    (if *print-readably*
        (if (minusp exp)
            (format stream "#M~s" str)
            (format stream "#M\"~a@~s\"" str
                    (- exp (length str))) ; why is the returned exp value
                                          ; of mpfr_get_str so weird?!
            )
        (if (minusp exp)
            (format stream "~a" str)
            (format stream "~a.~a"
                    (subseq str 0 exp)
                    (subseq str exp))))))

(defun mpfr-float-to-string (x &optional (rnd *mpfr-rnd*))
  (with-alien ((exp long)
               (str (* char)))
    (setf exp -1)
    (setf str (mpfr_get_str NIL (addr exp) *print-base* 0 (mpfr-float-ref x) rnd))
    (multiple-value-prog1 
        (values (cast str c-string) exp)
      (mpfr_free_str str))))

(defun mpfr-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((mpfr (make-mpfr-float)))
    (mpfr_set_str (mpfr-float-ref mpfr)
                  (read stream t nil t)
                  *mpfr-base* *mpfr-rnd*)
    mpfr))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun enable-fd-syntax (readtable)
    (set-dispatch-macro-character #\# #\M #'mpfr-reader readtable)))

