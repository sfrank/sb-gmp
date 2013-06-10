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

