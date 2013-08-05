(defpackage :sb-mpfr (:use "COMMON-LISP" "SB-ALIEN" "SB-C-CALL")
            (:export ;; parameters
                     #:*mpfr-precision*
                     #:*mpfr-rnd*
                     #:*mpfr-base*
                     ;; bignum float operations
                     #:make-mpfr-float
                     #:mpfr-float-to-string
                     #:add
                     #:sub
                     #:mul
                     #:square
                     #:div
                     ;; random number generation
                     ;; ...
                     ;; (un)installer functions
                     ;; #:install-mpfr-funs
                     ;; #:uninstall-mpfr-funs
                     ))

(in-package :sb-mpfr)

(defun %load-mpfr ()
  (handler-case
      (load-shared-object #-(or win32 darwin) "libmpfr.so"
                          #+darwin "libmpfr.dylib"
                          #+win32 "mpfr.dll"
                          :dont-save t)
    (error (e)
      (warn "MPFR not loaded (~a)" e)
      (return-from %load-mpfr nil)))
  t)

(%load-mpfr)

;;; types and initialization

(define-alien-type nil
    (struct mpfrfloat
            (mpfr_prec long)
            (mpfr_sign int)
            (mpfr_exp long)
            (mpfr_d (* unsigned-long))))

(define-alien-type mpfr_rnd_enum
  (enum mpfr_rnd
        (:mpfr_rndna -1)
        (:mpfr_rndn 0)
        (:mpfr_rndz 1)
        (:mpfr_rndu 2)
        (:mpfr_rndd 3)
        (:mpfr_rnda 4)
        (:mpfr_rndf 5)))

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

;;; special functions

(defmacro define-onearg-mpfr-int (funs)
  (loop for i in funs collect `(define-alien-routine ,i int
                                 (r (* (struct mpfrfloat)))
                                 (op (* (struct mpfrfloat)))
                                 (rnd mpfr_rnd_enum))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-onearg-mpfr-int
    (mpfr_log
     mpfr_log2
     mpfr_log10
     mpfr_exp
     mpfr_exp2
     mpfr_exp10
     mpfr_cos
     mpfr_sin
     mpfr_tan
     mpfr_sec
     mpfr_csc
     mpfr_cot
     mpfr_acos
     mpfr_asin
     mpfr_atan
     mpfr_cosh
     mpfr_sinh
     mpfr_tanh
     mpfr_sech
     mpfr_csch
     mpfr_coth
     mpfr_acosh
     mpfr_asinh
     mpfr_atanh
     mpfr_log1p
     mpfr_expm1
     mpfr_eint
     mpfr_li2
     mpfr_gamma
     mpfr_lngamma
     mpfr_digamma
     mpfr_zeta
     mpfr_erf
     mpfr_erfc
     mpfr_j0
     mpfr_j1
     mpfr_y0
     mpfr_y1
     mpfr_ai))

(defmacro define-twoarg-mpfr-int (funs)
  (loop for i in funs collect `(define-alien-routine ,i int
                                 (r (* (struct mpfrfloat)))
                                 (op1 (* (struct mpfrfloat)))
                                 (op2 (* (struct mpfrfloat)))
                                 (rnd mpfr_rnd_enum))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-twoarg-mpfr-int
    (mpfr_sin_cos
     mpfr_atan2
     mpfr_sinh_cosh
     mpfr_agm
     mpfr_hypot))

;; TODO: fac_ui, zeta_ui, jn, yn, fma, fms

(defmacro define-const-mpfr-int (funs)
  (loop for i in funs collect `(define-alien-routine ,i int
                                 (r (* (struct mpfrfloat)))
                                 (rnd mpfr_rnd_enum))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-const-mpfr-int
    (mpfr_const_log2
     mpfr_const_pi
     mpfr_const_euler
     mpfr_const_catalan))

;; TODO: _sum

;;; comparison functions

(define-alien-routine mpfr_cmpabs int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat))))

(defmacro define-onearg-mpfr-bool (funs)
  (loop for i in funs collect `(define-alien-routine ,i boolean
                                 (r (* (struct mpfrfloat)))
                                 (op (* (struct mpfrfloat))))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-onearg-mpfr-bool
    (mpfr_nan_p
     mpfr_inf_p
     mpfr_number_p
     mpfr_zero_p
     mpfr_regular_p))

(define-alien-routine mpfr_sgn int
  (op (* (struct mpfrfloat))))

(defmacro define-twoarg-mpfr-bool (funs)
  (loop for i in funs collect `(define-alien-routine ,i boolean
                                 (r (* (struct mpfrfloat)))
                                 (op1 (* (struct mpfrfloat)))
                                 (op2 (* (struct mpfrfloat))))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-twoarg-mpfr-bool
    (mpfr_greater_p
     mpfr_greaterequal_p
     mpfr_less_p
     mpfr_lessequal_p
     mpfr_equal_p
     mpfr_lessgreater_p
     mpfr_unordered_p))

;;; miscellaneous functions

(defmacro define-mpfr-void (funs)
  (loop for i in funs collect `(define-alien-routine ,i void)
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-mpfr-void
    (mpfr_clear_underflow
     mpfr_clear_overflow
     mpfr_clear_divby0
     mpfr_clear_nanflag
     mpfr_clear_inexflag
     mpfr_clear_erangeflag
     mpfr_set_underflow
     mpfr_set_overflow
     mpfr_set_divby0
     mpfr_set_nanflag
     mpfr_set_inexflag
     mpfr_set_erangeflag
     mpfr_clear_flags))

(defmacro define-mpfr-bool (funs)
  (loop for i in funs collect `(define-alien-routine ,i boolean)
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-mpfr-bool
    (mpfr_underflow_p
     mpfr_overflow_p
     mpfr_divby0_p
     mpfr_nanflag_p
     mpfr_inexflag_p
     mpfr_erangeflag_p))

;;;; lisp interface

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

(defun enable-mpfr-syntax (readtable)
  (set-dispatch-macro-character #\# #\M #'mpfr-reader readtable))
(enable-mpfr-syntax *readtable*)

;;; arithmetic function

(defun add (x y)
  (if (typep x 'mpfr-float)
      (let* ((res (make-mpfr-float))
             (r (mpfr-float-ref res))
             (xr (mpfr-float-ref x))
             (i (etypecase y
                  (mpfr-float 
                   (mpfr_add r xr (mpfr-float-ref y) *mpfr-rnd*))
                  ((unsigned-byte #.sb-vm:n-word-bits)
                   (mpfr_add_ui r xr y *mpfr-rnd*))
                  ((signed-byte #.sb-vm:n-word-bits)
                   (mpfr_add_si r xr y *mpfr-rnd*))
                  (double-float
                   (mpfr_add_d r xr y *mpfr-rnd*))
                  (integer
                   (sb-gmp::with-mpz-vars ((y gy))
                     (mpfr_add_z r xr (addr gy) *mpfr-rnd*)))
                  (rational
                   (sb-gmp::with-mpq-var (y qy)
                     (mpfr_add_q r xr (addr qy) *mpfr-rnd*))))))
        (values res i))
      (etypecase y
        (mpfr-float
         (add y x)))))

(defun sub (x y)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase x
              (mpfr-float
               (let ((xr (mpfr-float-ref x)))
                 (etypecase y
                   (mpfr-float 
                    (mpfr_sub r xr (mpfr-float-ref y) *mpfr-rnd*))
                   ((unsigned-byte #.sb-vm:n-word-bits)
                    (mpfr_sub_ui r xr y *mpfr-rnd*))
                   ((signed-byte #.sb-vm:n-word-bits)
                    (mpfr_sub_si r xr y *mpfr-rnd*))
                   (double-float
                    (mpfr_sub_d r xr y *mpfr-rnd*))
                   (integer
                    (sb-gmp::with-mpz-vars ((y gy))
                      (mpfr_sub_z r xr (addr gy) *mpfr-rnd*)))
                   (rational
                    (sb-gmp::with-mpq-var (y qy)
                      (mpfr_sub_q r xr (addr qy) *mpfr-rnd*))))))
              ((unsigned-byte #.sb-vm:n-word-bits)
               (etypecase y
                 (mpfr-float 
                  (mpfr_ui_sub r x (mpfr-float-ref y) *mpfr-rnd*))))
              ((signed-byte #.sb-vm:n-word-bits)
               (etypecase y
                 (mpfr-float 
                  (mpfr_si_sub r x (mpfr-float-ref y) *mpfr-rnd*))))
              (double-float
               (etypecase y
                 (mpfr-float 
                  (mpfr_d_sub r x (mpfr-float-ref y) *mpfr-rnd*))))
              (integer
               (etypecase y
                 (mpfr-float
                  (sb-gmp::with-mpz-vars ((x gx))
                    (mpfr_z_sub r (addr gx) (mpfr-float-ref y) *mpfr-rnd*))))))))
    (values res i)))

(defun mul (x y)
  (if (typep x 'mpfr-float)
      (let* ((res (make-mpfr-float))
             (r (mpfr-float-ref res))
             (xr (mpfr-float-ref x))
             (i (etypecase y
                  (mpfr-float 
                   (mpfr_mul r xr y *mpfr-rnd*))
                  ((unsigned-byte #.sb-vm:n-word-bits)
                   (mpfr_mul_ui r xr y *mpfr-rnd*))
                  ((signed-byte #.sb-vm:n-word-bits)
                   (mpfr_mul_si r xr y *mpfr-rnd*))
                  (double-float
                   (mpfr_mul_d r xr y *mpfr-rnd*))
                  (integer
                   (sb-gmp::with-mpz-vars ((y gy))
                     (mpfr_mul_z r xr (addr gy) *mpfr-rnd*)))
                  (rational
                   (sb-gmp::with-mpq-var (y qy)
                     (mpfr_mul_q r xr (addr qy) *mpfr-rnd*))))))
        (values res i))
      (etypecase y
        (mpfr-float
         (mul y x)))))

(defun sqare (x)
  (let ((r (make-mpfr-float)))
    (values r (mpfr_sqr (mpfr-float-ref r) (mpfr-float-ref x) *mpfr-rnd*))))

(defun div (x y)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase x
              (mpfr-float
               (let ((xr (mpfr-float-ref x)))
                 (etypecase y
                   (mpfr-float 
                    (mpfr_div r xr y *mpfr-rnd*))
                   ((unsigned-byte #.sb-vm:n-word-bits)
                    (mpfr_div_ui r xr y *mpfr-rnd*))
                   ((signed-byte #.sb-vm:n-word-bits)
                    (mpfr_div_si r xr y *mpfr-rnd*))
                   (double-float
                    (mpfr_div_d r xr y *mpfr-rnd*))
                   (integer
                    (sb-gmp::with-mpz-vars ((y gy))
                      (mpfr_div_z r xr (addr gy) *mpfr-rnd*)))
                   (rational
                    (sb-gmp::with-mpq-var (y qy)
                      (mpfr_div_q r xr (addr qy) *mpfr-rnd*))))))
              ((unsigned-byte #.sb-vm:n-word-bits)
               (etypecase y
                 (mpfr-float 
                  (mpfr_ui_div r x (mpfr-float-ref y) *mpfr-rnd*))))
              ((signed-byte #.sb-vm:n-word-bits)
               (etypecase y
                 (mpfr-float 
                  (mpfr_si_div r x (mpfr-float-ref y) *mpfr-rnd*))))
              (double-float
               (etypecase y
                 (mpfr-float 
                  (mpfr_d_div r x (mpfr-float-ref y) *mpfr-rnd*)))))))
    (values res i)))
