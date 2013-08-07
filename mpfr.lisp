(defpackage :sb-mpfr (:use "COMMON-LISP" "SB-ALIEN" "SB-C-CALL")
            (:export ;; parameters
                     #:*mpfr-precision*
                     #:*mpfr-rnd*
                     #:*mpfr-base*
                     ;; arithmetic operations
                     #:make-mpfr-float
                     #:mpfr-float-to-string
                     #:add
                     #:sub
                     #:mul
                     #:square
                     #:div
                     #:sqrt
                     #:reciprocal-sqrt
                     #:cubic-root
                     #:k-root
                     #:power
                     #:negate
                     #:abs
                     #:dim
                     #:mul-2-raised
                     #:div-2-raised
                     ;; special functions
                     #:log
                     #:log2
                     #:log10
                     #:exp
                     #:exp2
                     #:exp10
                     #:cos
                     #:sin
                     #:tan
                     #:sin-cos
                     #:sec
                     #:csc
                     #:cot
                     #:acos
                     #:asin
                     #:atan
                     #:cosh
                     #:sinh
                     #:tanh
                     #:sinh-cosh
                     #:sech
                     #:csch
                     #:coth
                     #:acosh
                     #:asinh
                     #:atanh
                     #:log1p
                     #:expm1
                     #:eint
                     #:li2
                     #:gamma
                     #:lngamma
                     #:digamma
                     #:zeta
                     #:erf
                     #:erfc
                     #:j0
                     #:j1
                     #:y0
                     #:y1
                     #:ai
                     #:arithmetic-geometric-mean
                     #:hypot
                     ;; comparison functions and predicates
                     #:nan-p
                     #:infinityp
                     #:numberp
                     #:zerop
                     #:regularp
                     #:compare
                     #:compare-2exp
                     #:compare-abs
                     #:>
                     #:>=
                     #:<
                     #:<=
                     #:=
                     #:/=
                     #:unorderedp
                     ;; constants
                     #:const-log2
                     #:const-pi
                     #:const-euler
                     #:const-catalan
                     ;; (un)installer functions
                     ;; #:install-mpfr-funs
                     ;; #:uninstall-mpfr-funs
                     )
            (:shadow :sqrt
                     :abs
                     :log
                     :exp
                     :cos
                     :sin
                     :tan
                     :acos
                     :asin
                     :atan
                     :cosh
                     :sinh
                     :tanh
                     :acosh
                     :asinh
                     :atanh
                     :numberp
                     :zerop
                     :>
                     :>=
                     :<
                     :<=
                     :=
                     :/=
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

(declaim (inline mpfr_add
                 mpfr_add_ui
                 mpfr_add_si
                 mpfr_add_d
                 mpfr_add_z
                 mpfr_add_q
                 mpfr_sub
                 mpfr_ui_sub
                 mpfr_sub_ui
                 mpfr_si_sub
                 mpfr_sub_si
                 mpfr_d_sub
                 mpfr_sub_d
                 mpfr_z_sub
                 mpfr_sub_z
                 mpfr_sub_q
                 mpfr_mul
                 mpfr_mul_ui
                 mpfr_mul_si
                 mpfr_mul_d
                 mpfr_mul_z
                 mpfr_mul_q
                 mpfr_sqr
                 mpfr_div
                 mpfr_ui_div
                 mpfr_div_ui
                 mpfr_si_div
                 mpfr_div_si
                 mpfr_d_div
                 mpfr_div_d
                 mpfr_div_z
                 mpfr_div_q
                 mpfr_sqrt
                 mpfr_sqrt_ui
                 mpfr_rec_sqrt
                 mpfr_cbrt
                 mpfr_root
                 mpfr_pow
                 mpfr_pow_ui
                 mpfr_pow_si
                 mpfr_pow_z
                 mpfr_ui_pow
                 mpfr_neg
                 mpfr_abs
                 mpfr_dim
                 mpfr_mul_2ui
                 mpfr_mul_2si
                 mpfr_div_2ui
                 mpfr_div_2si))

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

(define-alien-routine mpfr_mul_2ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_2si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_2ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_2si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
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
(define-alien-routine mpfr_fac_ui int
  (r (* (struct mpfrfloat)))
  (op unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_zeta_ui int
  (r (* (struct mpfrfloat)))
  (op unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_jn int
  (r (* (struct mpfrfloat)))
  (n long)
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_yn int
  (r (* (struct mpfrfloat)))
  (n long)
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_fma int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (op3 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_fms int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (op3 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))


;;; constant initialization

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
(define-alien-routine mpfr_sum int
  (r (* (struct mpfrfloat)))
  (tab (* (struct mpfrfloat)))
  (n unsigned-long)
  (rnd mpfr_rnd_enum))


;;; comparison functions

(declaim (inline mpfr_cmp
                 mpfr_cmp_ui
                 mpfr_cmp_si
                 mpfr_cmp_d
                 mpfr_cmp_z
                 mpfr_cmp_q
                 mpfr_cmp_ui_2exp
                 mpfr_cmp_si_2exp
                 mpfr_cmpabs))

(define-alien-routine mpfr_cmp int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat))))

(define-alien-routine mpfr_cmp_ui int
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long))

(define-alien-routine mpfr_cmp_si int
  (op1 (* (struct mpfrfloat)))
  (op2 long))

(define-alien-routine mpfr_cmp_d int
  (op1 (* (struct mpfrfloat)))
  (op2 double))

(define-alien-routine mpfr_cmp_z int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint))))

(define-alien-routine mpfr_cmp_q int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat))))

(define-alien-routine mpfr_cmp_ui_2exp int
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (exp long))

(define-alien-routine mpfr_cmp_si_2exp int
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (exp long))

(define-alien-routine mpfr_cmpabs int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat))))


(defmacro define-onearg-mpfr-bool (funs)
  (loop for i in funs collect `(define-alien-routine ,i boolean
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

(declaim (inline mpfr-float-ref
                 make-mpfr-float))

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

;; TODO: check printing of negative values!!!
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

;;; arithmetic functions

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
                   (mpfr_mul r xr (mpfr-float-ref y) *mpfr-rnd*))
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
                    (mpfr_div r xr (mpfr-float-ref y) *mpfr-rnd*))
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

(defun sqrt (x)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase x
              ((unsigned-byte #.sb-vm:n-word-bits)
               (mpfr_sqrt_ui r x *mpfr-rnd*))
              (mpfr-float
               (mpfr_sqrt r (mpfr-float-ref x) *mpfr-rnd*)))))
    (values res i)))

(defun reciprocal-sqrt (x)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_rec_sqrt r (mpfr-float-ref x) *mpfr-rnd*)))
    (values res i)))

(defun cubic-root (x)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_cbrt r (mpfr-float-ref x) *mpfr-rnd*)))
    (values res i)))

(defun k-root (x k)
  (check-type k (unsigned-byte #.sb-vm:n-word-bits))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_root r (mpfr-float-ref x) k *mpfr-rnd*)))
    (values res i)))

(defun power (x y)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase x
              (mpfr-float
               (let ((xr (mpfr-float-ref x)))
                 (etypecase y
                   (mpfr-float 
                    (mpfr_pow r xr (mpfr-float-ref y) *mpfr-rnd*))
                   ((unsigned-byte #.sb-vm:n-word-bits)
                    (mpfr_pow_ui r xr y *mpfr-rnd*))
                   ((signed-byte #.sb-vm:n-word-bits)
                    (mpfr_pow_si r xr y *mpfr-rnd*))
                   (integer
                    (sb-gmp::with-mpz-vars ((y gy))
                      (mpfr_pow_z r xr (addr gy) *mpfr-rnd*))))))
              ((unsigned-byte #.sb-vm:n-word-bits)
               (etypecase y
                 (mpfr-float 
                  (mpfr_ui_pow r x (mpfr-float-ref y) *mpfr-rnd*)))))))
    (values res i)))

(defun negate (x)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_neg r (mpfr-float-ref x) *mpfr-rnd*)))
    (values res i)))

(defun abs (x)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_abs r (mpfr-float-ref x) *mpfr-rnd*)))
    (values res i)))

(defun dim (x y)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_dim r (mpfr-float-ref x) (mpfr-float-ref y) *mpfr-rnd*)))
    (values res i)))

(defun mul-2-raised (x y)
  "Compute X*(2^Y)."
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase y
              ((unsigned-byte #.sb-vm:n-word-bits)
               (mpfr_mul_2ui r (mpfr-float-ref x) y *mpfr-rnd*))
              ((signed-byte #.sb-vm:n-word-bits)
               (mpfr_mul_2si r (mpfr-float-ref x) y *mpfr-rnd*)))))
    (values res i)))

(defun div-2-raised (x y)
  "Compute X/(2^Y)."
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase y
              ((unsigned-byte #.sb-vm:n-word-bits)
               (mpfr_div_2ui r (mpfr-float-ref x) y *mpfr-rnd*))
              ((signed-byte #.sb-vm:n-word-bits)
               (mpfr_div_2si r (mpfr-float-ref x) y *mpfr-rnd*)))))
    (values res i)))

;;; special functions

(defmacro define-onearg-mpfr-funs (funs)
  (loop for (clfun mfun) in funs
        collect `(defun ,clfun (x)
                   (let* ((result (make-mpfr-float))
                          (i (,mfun (mpfr-float-ref result)
                                    (mpfr-float-ref x)
                                    *mpfr-rnd*)))
                     (values result i)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-onearg-mpfr-funs
    ((log mpfr_log)
     (log2 mpfr_log2)
     (log10 mpfr_log10)
     (exp mpfr_exp)
     (exp2 mpfr_exp2)
     (exp10 mpfr_exp10)
     (cos mpfr_cos)
     (sin mpfr_sin)
     (tan mpfr_tan)
     (sec mpfr_sec)
     (csc mpfr_csc)
     (cot mpfr_cot)
     (acos mpfr_acos)
     (asin mpfr_asin)
     (cosh mpfr_cosh)
     (sinh mpfr_sinh)
     (tanh mpfr_tanh)
     (sech mpfr_sech)
     (csch mpfr_csch)
     (coth mpfr_coth)
     (acosh mpfr_acosh)
     (asinh mpfr_asinh)
     (atanh mpfr_atanh)
     (log1p mpfr_log1p)
     (expm1 mpfr_expm1)
     (eint mpfr_eint)
     (li2 mpfr_li2)
     (gamma mpfr_gamma)
     (lngamma mpfr_lngamma)
     (digamma mpfr_digamma)
     (zeta mpfr_zeta)
     (erf mpfr_erf)
     (erfc mpfr_erfc)
     (j0 mpfr_j0)
     (j1 mpfr_j1)
     (y0 mpfr_y0)
     (y1 mpfr_y1)
     (ai mpfr_ai)))

(defun atan (y &optional x)
  (if x
      (let* ((result (make-mpfr-float))
             (i (mpfr_atan2 (mpfr-float-ref result)
                            (mpfr-float-ref y)
                            (mpfr-float-ref x)
                            *mpfr-rnd*)))
        (values result i))
      (let* ((result (make-mpfr-float))
             (i (mpfr_atan (mpfr-float-ref result)
                           (mpfr-float-ref y)
                           *mpfr-rnd*)))
        (values result i))))

(defun sin-cos (x)
  (let* ((sin (make-mpfr-float))
         (cos (make-mpfr-float))
         (i (mpfr_sin_cos (mpfr-float-ref sin)
                          (mpfr-float-ref cos)
                          (mpfr-float-ref x)
                          *mpfr-rnd*)))
    (values sin cos i)))

(defun sinh-cosh (x)
  (let* ((sin (make-mpfr-float))
         (cos (make-mpfr-float))
         (i (mpfr_sinh_cosh (mpfr-float-ref sin)
                            (mpfr-float-ref cos)
                            (mpfr-float-ref x)
                            *mpfr-rnd*)))
    (values sin cos i)))

(defun arithmetic-geometric-mean (u v)
  (let* ((result (make-mpfr-float))
         (i (mpfr_agm (mpfr-float-ref result)
                      (mpfr-float-ref u)
                      (mpfr-float-ref v)
                      *mpfr-rnd*)))
    (values result i)))

(defun hypot (x y)
  (let* ((result (make-mpfr-float))
         (i (mpfr_hypot (mpfr-float-ref result)
                        (mpfr-float-ref x)
                        (mpfr-float-ref y)
                        *mpfr-rnd*)))
    (values result i)))

;;; constant values

(defmacro define-const-mpfr-funs (funs)
  (loop for (fname mname) in funs
        collect `(defun ,fname ()
                   (let* ((result (make-mpfr-float))
                          (i (,mname (mpfr-float-ref result)
                                     *mpfr-rnd*)))
                     (values result i)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-const-mpfr-funs
    ((const-log2 mpfr_const_log2)
     (const-pi mpfr_const_pi)
     (const-euler mpfr_const_euler)
     (const-catalan mpfr_const_catalan)))

;;; comparison functions and poredicates

(defmacro define-onearg-mpfr-predicates (funs)
  (loop for (fname mname) in funs
        collect `(defun ,fname (x)
                   (,mname (mpfr-float-ref x)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-onearg-mpfr-predicates
    ((nan-p mpfr_nan_p)
     (infinityp mpfr_inf_p)
     (numberp mpfr_number_p)
     (zerop mpfr_zero_p)
     (regularp mpfr_regular_p)))

(defmacro define-twoarg-mpfr-predicates (funs)
  (loop for (fname mname) in funs
        collect `(defun ,fname (x y)
                   (,mname (mpfr-float-ref x)
                           (mpfr-float-ref y)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-twoarg-mpfr-predicates
    ((compare-abs mpfr_cmpabs)
     (> mpfr_greater_p)
     (>= mpfr_greaterequal_p)
     (< mpfr_less_p)
     (<= mpfr_lessequal_p)
     (= mpfr_equal_p)
     (/= mpfr_lessgreater_p)
     (unorderedp mpfr_unordered_p)))

(defun compare (x y)
  (if (typep x 'mpfr-float)
      (etypecase y
        (mpfr-float 
         (mpfr_cmp (mpfr-float-ref x)
                   (mpfr-float-ref y)))
        ((unsigned-byte #.sb-vm:n-word-bits)
         (mpfr_cmp_ui (mpfr-float-ref x) y))
        ((signed-byte #.sb-vm:n-word-bits)
         (mpfr_cmp_si (mpfr-float-ref x) y))
        (double-float
         (mpfr_cmp_d (mpfr-float-ref x) y))
        (integer
         (sb-gmp::with-mpz-vars ((y gy))
           (mpfr_cmp_z (mpfr-float-ref x) (addr gy))))
        (rational
         (sb-gmp::with-mpq-var (y qy)
           (mpfr_cmp_q (mpfr-float-ref x) (addr qy)))))
      (etypecase y
        (mpfr-float
         (compare y x)))))

(defun compare-2exp (x y exp)
  (if (typep x 'mpfr-float)
      (etypecase y
        ((unsigned-byte #.sb-vm:n-word-bits)
         (mpfr_cmp_ui_2exp (mpfr-float-ref x) y exp))
        ((signed-byte #.sb-vm:n-word-bits)
         (mpfr_cmp_si_2exp (mpfr-float-ref x) y exp)))
      (etypecase y
        (mpfr-float
         (compare-2exp y x exp)))))
