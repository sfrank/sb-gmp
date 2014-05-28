(defpackage "SB-GMP"
  (:use "COMMON-LISP" "SB-ALIEN" "SB-BIGNUM")
  ;; we need a few very internal symbols
  (:import-from "SB-BIGNUM"
                "%BIGNUM-0-OR-PLUSP" "%NORMALIZE-BIGNUM"
                "NEGATE-BIGNUM-IN-PLACE")
  (:export
   ;; bignum integer operations
   #:mpz-add
   #:mpz-sub
   #:mpz-mul
   #:mpz-mod
   #:mpz-mul-2exp  ; shift left
   #:mpz-cdiv
   #:mpz-fdiv
   #:mpz-fdiv-2exp ; shift right
   #:mpz-tdiv
   #:mpz-powm
   #:mpz-pow
   #:mpz-gcd
   #:mpz-lcm
   #:mpz-sqrt
   #:mpz-probably-prime-p
   #:mpz-nextprime
   #:mpz-fac
   ;; the following functions are GMP >= 5.1 only
   #:mpz-2fac
   #:mpz-mfac
   #:mpz-primorial
   ;; number theoretic functions
   #:mpz-remove
   #:mpz-bin
   #:mpz-fib2
   ;; random number generation
   #:make-gmp-rstate-mt
   #:make-gmp-rstate-lc
   #:make-gmp-rstate-lc-size
   #:rand-seed
   #:random-bitcount
   #:random-int
   ;; ratio arithmetic
   #:mpq-add
   #:mpq-sub
   #:mpq-mul
   #:mpq-div
   ;; (un)installer functions
   ; these insert/remove the runtime patch in SBCL's runtime
   #:install-gmp-funs
   #:uninstall-gmp-funs
   ; these also load/unload the shared library and setup/clear
   ; hooks to handle core saves
   #:load-gmp
   #:unload-gmp
   ;; special variables
   #:*gmp-version*
   #:*gmp-disabled*
   ))

(in-package "SB-GMP")

(defvar *gmp-disabled* nil)

(defconstant +bignum-raw-area-offset+
  (- (* sb-vm:bignum-digits-offset sb-vm:n-word-bytes)
     sb-vm:other-pointer-lowtag))

(declaim (inline bignum-data-sap data-sap-bignum))
(defun bignum-data-sap (x)
  (declare (type bignum x))
  (sb-sys:sap+ (sb-sys:int-sap (sb-kernel:get-lisp-obj-address x))
               +bignum-raw-area-offset+))

(defun data-sap-bignum (x)
  (declare (type system-area-pointer x))
  (sb-kernel:%make-lisp-obj
   (sb-sys:sap-int
    (sb-sys:sap+ x (- +bignum-raw-area-offset+)))))

;; library loading preparation

(defun try-load-shared-object (pathname)
  (handler-case
      (load-shared-object pathname :dont-save t)
    (error () nil)))

(defun %load-gmp ()
  (or (some #'try-load-shared-object
            #-(or win32 darwin) '("libgmp.so" "libgmp.so.10" "libgmp.so.3")
            #+darwin '("libgmp.dylib" "libgmp.10.dylib" libgmp.3.dylib)
            #+win32 '("libgmp.dll" "libgmp-10.dll" "libgmp-3.dll"))
      (warn "GMP not loaded.")))

(defvar *gmp-features* nil)
(defvar *gmp-version* nil)

;; We load only the library right now to avoid undefined alien
;; style warnings
(%load-gmp)



;;;; memory handling

;; we use the client side allocation scheme of GMP. Upon
;; (re)allocation we call back into SBCL and allocate an approriate
;; bignum. The returned memory range is the raw bignum buffer without
;; hader. It is therefore crucial to disable GC when calling into GMP
;; since the bignums must not be moved. Furthermore, since we also
;; potentially allocate new bignums, a simple with-pinned-objects is
;; not sufficiant since the newly allocated bignum created when
;; calling from C to Lisp are not yet known references in the program
;; context. They only become bound (or definite garbage) on return
;; from a GMP computation.

(sb-alien::define-alien-callback allocate_gmp (* t) ((size size-t))
  (declare (optimize (speed 3) (space 3))
           (type (unsigned-byte #.sb-vm:n-word-bits) size))
  ;;(format t "GMP allocate_function called; size ~S~%" size)
  (bignum-data-sap
   (%allocate-bignum
    (ceiling size sb-vm:n-word-bytes))))

(sb-alien::define-alien-callback reallocate_gmp (* t) ((buffer (* t))
                                                       (old_size size-t)
                                                       (new_size size-t))
  (declare (optimize (speed 3) (space 3))
           (type (unsigned-byte #.sb-vm:n-word-bits) old_size new_size))
  ;;(format t "GMP reallocate_function called; old size ~S, new size ~S~%"
  ;;        old_size new_size)
  (if (<= new_size old_size)
      buffer
      (let ((new (bignum-data-sap
                  (%allocate-bignum
                   (ceiling new_size
                            sb-vm:n-word-bytes)))))
        (sb-kernel:system-area-ub8-copy (alien-sap buffer)
                                        0 new 0 old_size)
        new)))

(sb-alien::define-alien-callback free_gmp void ((buffer (* t))
                                                (size size-t))
  (declare (optimize (speed 3) (space 3) (safety 0))
           (dynamic-extent buffer size)
           (ignore buffer size))
  ;;(format t "GMP free_function called~%")
  (values))

(define-alien-routine __gmp_set_memory_functions void
  (alloc (function (* t) size-t))
  (realloc (function (* t) (* t) size-t size-t))
  (free (function void (* t) size-t)))

(defun init-allocation-functions ()
  (__gmp_set_memory_functions allocate_gmp
                              reallocate_gmp
                              free_gmp))

;;;; alien GMP types

;; GMP bignum integers
(define-alien-type nil
    (struct gmpint
            (mp_alloc int)
            (mp_size int)
            (mp_d (* unsigned-long))))

;;; GMP bignum rationals
(define-alien-type nil
    (struct gmprat
            (mp_num (struct gmpint))
            (mp_den (struct gmpint))))


;;; utility functions for SBCL bignums

(declaim (inline blength bassert)
         (ftype (function (integer) (values bignum-index &optional)) blength)
         (ftype (function (integer) (values bignum &optional)) bassert))

(defun blength (a)
  (declare (optimize (speed 3) (space 3) (safety 0)))
  (etypecase a
    (fixnum 1)
    (t (%bignum-length a))))

(defun bassert (a)
  (declare (optimize (speed 3) (space 3) (safety 0)))
  (etypecase a
    (fixnum (make-small-bignum a))
    (t a)))


;;; integer interface functions
(defmacro define-twoarg-mpz-funs (funs)
  (loop for i in funs collect `(define-alien-routine ,i void
                                 (r (* (struct gmpint)))
                                 (a (* (struct gmpint))))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(defmacro define-threearg-mpz-funs (funs)
  (loop for i in funs collect `(define-alien-routine ,i void
                                 (r (* (struct gmpint)))
                                 (a (* (struct gmpint)))
                                 (b (* (struct gmpint))))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(defmacro define-fourarg-mpz-funs (funs)
  (loop for i in funs collect `(define-alien-routine ,i void
                                 (r (* (struct gmpint)))
                                 (a (* (struct gmpint)))
                                 (b (* (struct gmpint)))
                                 (c (* (struct gmpint))))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))


(define-twoarg-mpz-funs (__gmpz_sqrt
                         __gmpz_nextprime))

(define-threearg-mpz-funs (__gmpz_add
                           __gmpz_sub
                           __gmpz_mul
                           __gmpz_mod
                           __gmpz_gcd
                           __gmpz_lcm))

(define-fourarg-mpz-funs (__gmpz_cdiv_qr
                          __gmpz_fdiv_qr
                          __gmpz_tdiv_qr
                          __gmpz_powm))

(declaim (inline __gmpz_mul_2exp
                 __gmpz_fdiv_q_2exp
                 __gmpz_pow_ui
                 __gmpz_probab_prime_p
                 __gmpz_fac_ui
                 __gmpz_2fac_ui
                 __gmpz_mfac_uiui
                 __gmpz_primorial_ui
                 __gmpz_remove
                 __gmpz_bin_ui
                 __gmpz_fib2_ui))

(define-alien-routine __gmpz_mul_2exp void
  (r (* (struct gmpint)))
  (b (* (struct gmpint)))
  (e unsigned-long))

(define-alien-routine __gmpz_fdiv_q_2exp void
  (r (* (struct gmpint)))
  (b (* (struct gmpint)))
  (e unsigned-long))

(define-alien-routine __gmpz_pow_ui void
  (r (* (struct gmpint)))
  (b (* (struct gmpint)))
  (e unsigned-long))

(define-alien-routine __gmpz_probab_prime_p int
  (n (* (struct gmpint)))
  (reps int))

(define-alien-routine __gmpz_fac_ui void
  (r (* (struct gmpint)))
  (a unsigned-long))

(define-alien-routine __gmpz_2fac_ui void
  (r (* (struct gmpint)))
  (a unsigned-long))

(define-alien-routine __gmpz_mfac_uiui void
  (r (* (struct gmpint)))
  (n unsigned-long)
  (m unsigned-long))

(define-alien-routine __gmpz_primorial_ui void
  (r (* (struct gmpint)))
  (n unsigned-long))

(define-alien-routine __gmpz_remove unsigned-long
  (r (* (struct gmpint)))
  (x (* (struct gmpint)))
  (f (* (struct gmpint))))

(define-alien-routine __gmpz_bin_ui void
  (r (* (struct gmpint)))
  (n (* (struct gmpint)))
  (k unsigned-long))

(define-alien-routine __gmpz_fib2_ui void
  (r (* (struct gmpint)))
  (a (* (struct gmpint)))
  (b unsigned-long))


;; ratio functions
(defmacro define-threearg-mpq-funs (funs)
  (loop for i in funs collect `(define-alien-routine ,i void
                                 (r (* (struct gmprat)))
                                 (a (* (struct gmprat)))
                                 (b (* (struct gmprat))))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-threearg-mpq-funs (__gmpq_add
                           __gmpq_sub
                           __gmpq_mul
                           __gmpq_div))


;;;; SBCL interface

;;; utility macros for GMP mpz variable and result declaration and
;;; incarnation of associated SBCL bignums

(defmacro with-mpz-vars (pairs &body body)
  (loop for (a ga) in pairs
        for length = (gensym "LENGTH")
        for plusp = (gensym "PLUSP")
        for barg = (gensym "BARG")
        for arg = (gensym "ARG")
        collect `(,ga (struct gmpint)) into declares
        collect `(,barg (bassert ,a)) into gmpinits
        collect `(,plusp (%bignum-0-or-plusp ,barg (%bignum-length ,barg))) into gmpinits
        collect `(,arg (if ,plusp ,barg (negate-bignum ,barg nil))) into gmpinits
        collect `(,length (%bignum-length ,arg)) into gmpinits
        collect arg into vars
        collect `(setf (slot ,ga 'mp_alloc) ,length
                       (slot ,ga 'mp_size)
                       (progn ;; handle twos complements/ulong limbs mismatch
                         (when (zerop (%bignum-ref ,arg (1- ,length)))
                           (decf ,length))
                         (if ,plusp ,length (- ,length)))
                       (slot ,ga 'mp_d) (bignum-data-sap ,arg))
          into gmpvarssetup
        finally (return
                  `(with-alien ,declares
                     (let* ,gmpinits
                       (sb-sys:with-pinned-objects ,vars
                         ,@gmpvarssetup
                         ,@body))))))


;; pre-allocate the result bignum with a certain limb size
(defmacro with-allocated-results (pairs &body body)
  (loop for (gres size) in pairs
        for res = (gensym "RESULT")
        collect `(,gres (struct gmpint)) into declares
        collect `(,res (%allocate-bignum ,size))
          into resinits
        collect `(setf (slot ,gres 'mp_alloc) ,size
                       (slot ,gres 'mp_size) 0
                       (slot ,gres 'mp_d) (bignum-data-sap ,res))
          into inits
        collect `(let ((bres (%normalize-bignum
                                (data-sap-bignum (alien-sap (slot ,gres 'mp_d)))
                                (slot ,gres 'mp_alloc))))
                   (declare (sb-ext:muffle-conditions sb-ext:compiler-note)
                            (type unsigned-byte bres))
                   ;; check for negative result
                   (if (minusp (slot ,gres 'mp_size))
                       (- bres)
                       bres))
          into normlimbs
        collect res into results
        finally (return
                  `(let ,resinits
                     (sb-sys:without-gcing
                      (with-alien ,declares
                        ,@inits
                        ,@body
                        (values ,@normlimbs)))))))

;; the default case is to initially allocate one limb for the result
(defmacro with-results (resultvars &body body)
  (loop for gres in resultvars
        collect `(,gres 1) into inits
        finally (return
                  `(with-allocated-results ,inits
                     ,@body))))

;;; function definition and foreign function relationships
(defmacro defgmpfun (name args &body body)
  `(progn
     (declaim (sb-ext:maybe-inline ,name))
     (defun ,name ,args
       (declare (optimize (speed 3) (space 3))
                (type integer ,@args))
       ,@body)))


;; SBCL/GMP functions
(defgmpfun mpz-add (a b)
  (with-allocated-results ((result (1+ (max (blength a)
                                            (blength b)))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_add (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-sub (a b)
  (with-allocated-results ((result (1+ (max (blength a)
                                            (blength b)))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_sub (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-mul (a b)
  (with-allocated-results ((result (+ (blength a)
                                      (blength b))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_mul (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-mul-2exp (a b)
  (check-type b (unsigned-byte #.sb-vm:n-word-bits))
  (with-allocated-results ((result (+ (1+ (blength a))
                                      (floor b sb-vm:n-word-bits))))
    (with-mpz-vars ((a ga))
      (__gmpz_mul_2exp (addr result) (addr ga) b))))

(defgmpfun mpz-mod (a b)
  (with-allocated-results ((result (1+ (max (blength a)
                                            (blength b)))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_mod (addr result) (addr ga) (addr gb))
      (when (and (minusp (slot gb 'mp_size))
                 (/= 0 (slot result 'mp_size)))
        (__gmpz_add (addr result) (addr result) (addr gb))))))

(defgmpfun mpz-cdiv (n d)
  (let ((size (1+ (max (blength n)
                       (blength d)))))
    (with-allocated-results ((quot size)
                             (rem size))
      (with-mpz-vars ((n gn) (d gd))
        (__gmpz_cdiv_qr (addr quot) (addr rem) (addr gn) (addr gd))))))

(defgmpfun mpz-fdiv (n d)
  (let ((size (1+ (max (blength n)
                       (blength d)))))
    (with-allocated-results ((quot size)
                             (rem size))
      (with-mpz-vars ((n gn) (d gd))
        (__gmpz_fdiv_qr (addr quot) (addr rem) (addr gn) (addr gd))))))

(defgmpfun mpz-fdiv-2exp (a b)
  (check-type b (unsigned-byte #.sb-vm:n-word-bits))
  (with-allocated-results ((result (1+ (- (blength a)
                                          (floor b sb-vm:n-word-bits)))))
    (with-mpz-vars ((a ga))
      (__gmpz_fdiv_q_2exp (addr result) (addr ga) b))))

(defgmpfun mpz-tdiv (n d)
  (let ((size (max (blength n)
                   (blength d))))
    (with-allocated-results ((quot size)
                             (rem size))
      (with-mpz-vars ((n gn) (d gd))
        (__gmpz_tdiv_qr (addr quot) (addr rem) (addr gn) (addr gd))))))

(defgmpfun mpz-pow (base exp)
  (check-type exp (integer 0 #.most-positive-fixnum))
  (with-results (rop)
    (with-mpz-vars ((base gbase))
      (__gmpz_pow_ui (addr rop) (addr gbase) exp))))

(defgmpfun mpz-powm (base exp mod)
  (with-allocated-results ((rop (1+ (blength mod))))
    (with-mpz-vars ((base gbase) (exp gexp) (mod gmod))
      (__gmpz_powm (addr rop) (addr gbase) (addr gexp) (addr gmod)))))

(defgmpfun mpz-gcd (a b)
  (with-allocated-results ((result (min (blength a)
                                        (blength b))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_gcd (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-lcm (a b)
  (with-allocated-results ((result (+ (blength a)
                                      (blength b))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_lcm (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-sqrt (a)
  (with-allocated-results ((result (1+ (ceiling (blength a) 2))))
    (with-mpz-vars ((a ga))
      (__gmpz_sqrt (addr result) (addr ga)))))


;;; Functions that use GMP-side allocated integers and copy the result
;;; into a SBCL bignum at the end of the computation when the required
;;; bignum length is known.
(defun mpz-probably-prime-p (n &optional (reps 25))
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type integer n reps))
  (check-type reps fixnum)
  (with-mpz-vars ((n gn))
    (__gmpz_probab_prime_p (addr gn) reps)))

(defgmpfun mpz-nextprime (a)
  (with-results (prime)
    (with-mpz-vars ((a ga))
      (__gmpz_nextprime (addr prime) (addr ga)))))

(defgmpfun mpz-fac (n)
  (check-type n (unsigned-byte #.sb-vm:n-word-bits))
  (with-results (fac)
    (__gmpz_fac_ui (addr fac) n)))

(defgmpfun %mpz-2fac (n)
  (check-type n (unsigned-byte #.sb-vm:n-word-bits))
  (with-results (fac)
    (__gmpz_2fac_ui (addr fac) n)))

(defgmpfun %mpz-mfac (n m)
  (check-type n (unsigned-byte #.sb-vm:n-word-bits))
  (check-type m (unsigned-byte #.sb-vm:n-word-bits))
  (with-results (fac)
    (__gmpz_mfac_uiui (addr fac) n m)))

(defgmpfun %mpz-primorial (n)
  (check-type n (unsigned-byte #.sb-vm:n-word-bits))
  (with-results (r)
    (__gmpz_primorial_ui (addr r) n)))

(defun setup-5.1-stubs ()
  (macrolet ((stubify (name implementation &rest arguments)
               `(setf (fdefinition ',name)
                      (if (member :sb-gmp-5.1 *gmp-features*)
                          (fdefinition ',implementation)
                          (lambda ,arguments
                            (declare (ignore ,@arguments))
                            (error "~S is only available in GMP >= 5.1"
                                   ',name))))))
    (stubify mpz-2fac %mpz-2fac n)
    (stubify mpz-mfac %mpz-mfac n m)
    (stubify mpz-primorial %mpz-primorial n)
    (unless (member :sb-gmp-5.1 *gmp-features*)
      (setf (fdefinition 'mpz-remove) #'mpz-remove-5.1))))

(defgmpfun mpz-remove-5.1 (n f)
  (check-type f unsigned-byte
              "only handled by GMP prior to version 5.1")
  (let* ((c 0)
         (res (with-results (r)
                (with-mpz-vars ((n gn)
                                (f gf))
                  (setf c (__gmpz_remove (addr r) (addr gn) (addr gf)))
                  ;; the limb at |size|+1 is not set correctly to 0, so do it
                  ;; to not confuse %normalize-bignum
                  (when (> (slot r 'mp_alloc) (abs (slot r 'mp_size)))
                    (%bignum-set (data-sap-bignum (alien-sap (slot r 'mp_d)))
                                 (1- (slot r 'mp_alloc))
                                 0))))))
    (values res c)))


(defgmpfun mpz-remove (n f)
  (let* ((c 0)
         (res (with-results (r)
                (with-mpz-vars ((n gn)
                                (f gf))
                  (setf c (__gmpz_remove (addr r) (addr gn) (addr gf)))
                  ;; the limb at |size|+1 is not set correctly to 0, so do it
                  ;; to not confuse %normalize-bignum
                  (when (> (slot r 'mp_alloc) (abs (slot r 'mp_size)))
                    (%bignum-set (data-sap-bignum (alien-sap (slot r 'mp_d)))
                                 (1- (slot r 'mp_alloc))
                                 0))))))
    (values res c)))

(defgmpfun mpz-bin (n k)
  (check-type k (unsigned-byte #.sb-vm:n-word-bits))
  (with-results (r)
    (with-mpz-vars ((n gn))
      (__gmpz_bin_ui (addr r) (addr gn) k))))

(defgmpfun mpz-fib2 (n)
  (check-type n (unsigned-byte #.sb-vm:n-word-bits))
  (with-results (fibn fibn-1)
    (__gmpz_fib2_ui (addr fibn) (addr fibn-1) n)))


;;;; Random bignum (mpz) generation

;; internal structure of GMP radom state
(define-alien-type nil
    (struct gmprandstate
            (mp_seed (struct gmpint))
            (mp_alg int)
            (mp_algdata (* t))))

(declaim (inline __gmp_randinit_mt
                 __gmp_randinit_lc_2exp
                 __gmp_randinit_lc_2exp_size
                 __gmp_randseed
                 __gmp_randseed_ui
                 __gmpz_urandomb
                 __gmpz_urandomm))

(define-alien-routine __gmp_randinit_mt void
  (s (* (struct gmprandstate))))

(define-alien-routine __gmp_randinit_lc_2exp void
  (s (* (struct gmprandstate)))
  (a (* (struct gmpint)))
  (c unsigned-long)
  (exp unsigned-long))

(define-alien-routine __gmp_randinit_lc_2exp_size void
  (s (* (struct gmprandstate)))
  (sz unsigned-long))

(define-alien-routine __gmp_randseed void
  (s (* (struct gmprandstate)))
  (sd (* (struct gmpint))))

(define-alien-routine __gmp_randseed_ui void
  (s (* (struct gmprandstate)))
  (sd unsigned-long))

(define-alien-routine __gmpz_urandomb void
  (r (* (struct gmpint)))
  (s (* (struct gmprandstate)))
  (bcnt unsigned-long))

(define-alien-routine __gmpz_urandomm void
  (r (* (struct gmpint)))
  (s (* (struct gmprandstate)))
  (n (* (struct gmpint))))

;; GMP random state representation in SB-GMP
(defstruct (gmp-rstate (:constructor %make-gmp-rstate)
                       (:constructor %make-gmp-rstate-boa (seed rfun)))
  (seed (%allocate-bignum 1)
   :type integer)
  (rfun (sap-alien (sb-sys:int-sap 0) (* t)) :type (alien (* t))))

(declaim (sb-ext:maybe-inline make-gmp-rstate
                              make-gmp-rstate-lc
                              rand-seed
                              random-bitcount
                              random-int))

(defmacro with-rstate ((ref state) &body body)
  `(with-alien ((,ref (struct gmprandstate)))
     (setf (slot (slot ,ref 'mp_seed) 'mp_d)
           (bignum-data-sap (gmp-rstate-seed state))
           (slot ,ref 'mp_algdata)
           (gmp-rstate-rfun ,state))
     ,@body
     (setf (gmp-rstate-seed ,state)
           (data-sap-bignum (alien-sap (slot (slot ,ref 'mp_seed) 'mp_d))))))

(defun make-gmp-rstate-mt ()
  "Instantiate a state for the GMP Mersenne-Twister random number generator."
  (declare (optimize (speed 3) (space 3) (safety 0)))
  (sb-sys:without-gcing
    (with-alien ((ref (struct gmprandstate)))
      (__gmp_randinit_mt (addr ref))
      (%make-gmp-rstate-boa (data-sap-bignum (alien-sap (slot (slot ref 'mp_seed) 'mp_d)))
                            (slot ref 'mp_algdata)))))

(defun make-gmp-rstate-lc (a c m2exp)
  "Instantiate a state for the GMP linear congruential random number generator."
  (declare (optimize (speed 3) (space 3)))
  (check-type c (unsigned-byte #.sb-vm:n-word-bits))
  (check-type m2exp (unsigned-byte #.sb-vm:n-word-bits))
  (sb-sys:without-gcing
    (with-mpz-vars ((a ga))
      (with-alien ((ref (struct gmprandstate)))
        (__gmp_randinit_lc_2exp (addr ref) (addr ga) c m2exp)
        (%make-gmp-rstate-boa (data-sap-bignum (alien-sap (slot (slot ref 'mp_seed) 'mp_d)))
                              (slot ref 'mp_algdata))))))

(defun make-gmp-rstate-lc-size (size)
  "Instantiate a state for the GMP linear congruential random number generator."
  (declare (optimize (speed 3) (space 3)))
  ;; chooses A, C and M2EXP from an internal table
  ;; which is limited to entries of 0..128
  (check-type size (integer 0 128))
  (sb-sys:without-gcing
    (with-alien ((ref (struct gmprandstate)))
      (__gmp_randinit_lc_2exp_size (addr ref) size)
      (%make-gmp-rstate-boa (data-sap-bignum (alien-sap (slot (slot ref 'mp_seed) 'mp_d)))
                            (slot ref 'mp_algdata)))))

(defun rand-seed (state seed)
  "Initialize a random STATE with SEED."
  (declare (optimize (speed 3) (space 3)))
  (check-type state gmp-rstate)
  (cond
    ((typep seed '(and fixnum unsigned-byte))
     (sb-sys:without-gcing
       (with-rstate (ref state)
         (__gmp_randseed_ui (addr ref) seed))))
    ((typep seed 'unsigned-byte)
     (sb-sys:without-gcing
       (with-mpz-vars ((seed gseed))
         (with-rstate (ref state)
           (__gmp_randseed (addr ref) (addr gseed))))))
    (t
     (error "SEED must be a positive integer")))
  state)

(defun random-bitcount (state bitcount)
  "Return a random integer in the range 0..(2^bitcount - 1)."
  (declare (optimize (speed 3) (space 3))
           (type (and fixnum unsigned-byte) bitcount))
  (check-type state gmp-rstate)
  (with-allocated-results ((result (+ (ceiling bitcount sb-vm:n-word-bits) 2)))
    (with-rstate (ref state)
      (__gmpz_urandomb (addr result) (addr ref) bitcount))))

(defun random-int (state boundary)
  "Return a random integer in the range 0..(boundary - 1)."
  (declare (optimize (speed 3) (space 3)))
  (check-type state gmp-rstate)
  (let ((b (bassert boundary)))
    (with-allocated-results ((result (1+ (%bignum-length b))))
      (with-mpz-vars ((b gb))
        (with-rstate (ref state)
            (__gmpz_urandomm (addr result) (addr ref) (addr gb)))))))


;;; Rational functions
(declaim (inline %lsize))
(defun %lsize (minusp n)
  (declare (optimize (speed 3) (space 3) (safety 0)))
  "n must be a (potentially denormalized) bignum"
  (let ((length (%bignum-length n)))
    (when (zerop (%bignum-ref n (1- length)))
      (decf length))
    (if minusp (- length) length)))

(defmacro with-mpq-var (pair &body body)
  (destructuring-bind (var mpqvar) pair
    `(let* ((an (bassert (numerator ,var)))
            (ad (bassert (denominator ,var)))
            (asign (not (%bignum-0-or-plusp an (%bignum-length an)))))
       (when asign
           (setf an (negate-bignum an nil)))
       (let* ((anlen (%lsize asign an))
              (adlen (%lsize NIL ad)))
           (with-alien ((,mpqvar (struct gmprat)))
             (sb-sys:with-pinned-objects (an ad)
               (setf (slot (slot ,mpqvar 'mp_num) 'mp_size) anlen
                     (slot (slot ,mpqvar 'mp_num) 'mp_alloc) (abs anlen)
                     (slot (slot ,mpqvar 'mp_num) 'mp_d)
                     (bignum-data-sap an))
               (setf (slot (slot ,mpqvar 'mp_den) 'mp_size) adlen
                     (slot (slot ,mpqvar 'mp_den) 'mp_alloc) (abs adlen)
                     (slot (slot ,mpqvar 'mp_den) 'mp_d)
                     (bignum-data-sap ad))
               ,@body))))))

(defmacro defmpqfun (name gmpfun)
  `(progn
     (declaim (sb-ext:maybe-inline ,name))
     (defun ,name (a b)
       (declare (optimize (speed 3) (space 3) (safety 0)))
       (let ((size (+ (max (blength (numerator a))
                           (blength (denominator a)))
                      (max (blength (numerator b))
                           (blength (denominator b)))
                      3)))
         (with-alien ((r (struct gmprat)))
           (let ((num (%allocate-bignum size))
                 (den (%allocate-bignum size)))
             (sb-sys:without-gcing
               (setf (slot (slot r 'mp_num) 'mp_size) 0
                     (slot (slot r 'mp_num) 'mp_alloc) size
                     (slot (slot r 'mp_num) 'mp_d) (bignum-data-sap num))
               (setf (slot (slot r 'mp_den) 'mp_size) 0
                     (slot (slot r 'mp_den) 'mp_alloc) size
                     (slot (slot r 'mp_den) 'mp_d) (bignum-data-sap den))
               (let* ((an (bassert (numerator a)))
                      (ad (bassert (denominator a)))
                      (asign (not (%bignum-0-or-plusp an (%bignum-length an))))
                      (bn (bassert (numerator b)))
                      (bd (bassert (denominator b)))
                      (bsign (not (%bignum-0-or-plusp bn (%bignum-length bn)))))
                 (when asign
                   (setf an (negate-bignum an nil)))
                 (when bsign
                   (setf bn (negate-bignum bn nil)))
                 (let* ((anlen (%lsize asign an))
                        (adlen (%lsize NIL ad))
                        (bnlen (%lsize bsign bn))
                        (bdlen (%lsize NIL bd)))
                   (with-alien ((arga (struct gmprat))
                                (argb (struct gmprat)))
                     (setf (slot (slot arga 'mp_num) 'mp_size) anlen
                           (slot (slot arga 'mp_num) 'mp_alloc) (abs anlen)
                           (slot (slot arga 'mp_num) 'mp_d)
                           (bignum-data-sap an))
                     (setf (slot (slot arga 'mp_den) 'mp_size) adlen
                           (slot (slot arga 'mp_den) 'mp_alloc) (abs adlen)
                           (slot (slot arga 'mp_den) 'mp_d)
                           (bignum-data-sap ad))
                     (setf (slot (slot argb 'mp_num) 'mp_size) bnlen
                           (slot (slot argb 'mp_num) 'mp_alloc) (abs bnlen)
                           (slot (slot argb 'mp_num) 'mp_d)
                           (bignum-data-sap bn))
                     (setf (slot (slot argb 'mp_den) 'mp_size) bdlen
                           (slot (slot argb 'mp_den) 'mp_alloc) (abs bdlen)
                           (slot (slot argb 'mp_den) 'mp_d)
                           (bignum-data-sap bd))
                     (,gmpfun (addr r) (addr arga) (addr argb))))
                 (locally (declare (optimize (speed 1)))
                   (sb-kernel::build-ratio (if (minusp (slot (slot r 'mp_num) 'mp_size))
                                               (%normalize-bignum
                                                (negate-bignum-in-place
                                                 (data-sap-bignum
                                                  (alien-sap (slot (slot r 'mp_num) 'mp_d))))
                                                (slot (slot r 'mp_num) 'mp_alloc))
                                               (%normalize-bignum
                                                (data-sap-bignum
                                                 (alien-sap (slot (slot r 'mp_num) 'mp_d)))
                                                (slot (slot r 'mp_num) 'mp_alloc)))
                                           (%normalize-bignum
                                            (data-sap-bignum
                                             (alien-sap (slot (slot r 'mp_den) 'mp_d)))
                                            (slot (slot r 'mp_den) 'mp_alloc))))))))))))

(defmpqfun mpq-add __gmpq_add)
(defmpqfun mpq-sub __gmpq_sub)
(defmpqfun mpq-mul __gmpq_mul)
(defmpqfun mpq-div __gmpq_div)


;;;; SBCL interface and integration installation
(macrolet ((def (name original)
             (let ((special (intern (format nil "*~A-FUNCTION*" name))))
               `(progn
                  (declaim (type function ,special)
                           (inline ,name))
                  (defvar ,special (symbol-function ',original))
                  (defun ,name (&rest args)
                    (apply (load-time-value ,special t) args))))))
  (def orig-mul multiply-bignums)
  (def orig-truncate bignum-truncate)
  (def orig-gcd bignum-gcd)
  (def orig-lcm sb-kernel:two-arg-lcm)
  (def orig-isqrt isqrt)
  (def orig-two-arg-+ sb-kernel:two-arg-+)
  (def orig-two-arg-- sb-kernel:two-arg--)
  (def orig-two-arg-* sb-kernel:two-arg-*)
  (def orig-two-arg-/ sb-kernel:two-arg-/)
  (def orig-intexp sb-kernel::intexp))

;;; integers
(defun gmp-mul (a b)
  (declare (optimize (speed 3) (space 3))
           (type bignum-type a b)
           (inline mpz-mul))
  (if (or (< (min (%bignum-length a)
                  (%bignum-length b))
             6)
          *gmp-disabled*)
      (orig-mul a b)
      (mpz-mul a b)))

(defun gmp-truncate (a b)
  (declare (optimize (speed 3) (space 3))
           (type bignum-type a b)
           (inline mpz-tdiv))
  (if (or (< (min (%bignum-length a)
                  (%bignum-length b))
             3)
          *gmp-disabled*)
      (orig-truncate a b)
      (mpz-tdiv a b)))

(defun gmp-lcm (a b)
  (declare (optimize (speed 3) (space 3))
           (type integer a b)
           (inline mpz-lcm))
  (if (or (and (typep a 'fixnum)
               (typep b 'fixnum))
          *gmp-disabled*)
      (orig-lcm a b)
      (mpz-lcm a b)))

(defun gmp-isqrt (n)
  (declare (optimize (speed 3) (space 3))
           (type unsigned-byte n)
           (inline mpz-sqrt))
  (if (or (typep n 'fixnum)
          *gmp-disabled*)
      (orig-isqrt n)
      (mpz-sqrt n)))

;;; rationals
(defun gmp-two-arg-+ (x y)
  (declare (optimize (speed 3) (space 3))
           (inline mpq-add))
  (if (and (or (typep x 'ratio)
               (typep y 'ratio))
           (rationalp y)
           (rationalp x)
           (not *gmp-disabled*))
      (mpq-add x y)
      (orig-two-arg-+ x y)))

(defun gmp-two-arg-- (x y)
  (declare (optimize (speed 3) (space 3))
           (inline mpq-sub))
  (if (and (or (typep x 'ratio)
               (typep y 'ratio))
           (rationalp y)
           (rationalp x)
           (not *gmp-disabled*))
      (mpq-sub x y)
      (orig-two-arg-- x y)))

(defun gmp-two-arg-* (x y)
  (declare (optimize (speed 3) (space 3))
           (inline mpq-mul))
  (if (and (or (typep x 'ratio)
               (typep y 'ratio))
           (rationalp y)
           (rationalp x)
           (not *gmp-disabled*))
      (mpq-mul x y)
      (orig-two-arg-* x y)))

(defun gmp-two-arg-/ (x y)
  (declare (optimize (speed 3) (space 3))
           (inline mpq-div))
  (if (and (rationalp x)
           (rationalp y)
           (not (eql y 0))
           (not *gmp-disabled*))
      (mpq-div x y)
      (orig-two-arg-/ x y)))

(defun gmp-intexp (base power)
  (declare (inline mpz-mul-2exp mpz-pow))
  (check-type power (integer #.(1+ most-negative-fixnum) #.most-positive-fixnum))
  (cond
    ((or (and (integerp base)
              (< (abs power) 1000)
              (< (blength base) 4))
         *gmp-disabled*)
     (orig-intexp base power))
    (t
     (when (and sb-kernel::*intexp-maximum-exponent*
                (> (abs power) sb-kernel::*intexp-maximum-exponent*))
       (error "The absolute value of ~S exceeds ~S."
              power 'sb-kernel::*intexp-maximum-exponent*))
     (cond ((minusp power)
            (/ (the integer (gmp-intexp base (- power)))))
           ((eql base 2)
            (mpz-mul-2exp 1 power))
           ((typep base 'ratio)
            (sb-kernel::%make-ratio (gmp-intexp (numerator base) power)
                                    (gmp-intexp (denominator base) power)))
           (t
            (mpz-pow base power))))))

;;; installation
(defmacro with-package-locks-ignored (&body body)
  `(handler-bind ((sb-ext:package-lock-violation
                    (lambda (condition)
                      (declare (ignore condition))
                      (invoke-restart :ignore-all))))
     ,@body))

(defun install-gmp-funs ()
  (init-allocation-functions)
  (with-package-locks-ignored
      (macrolet ((def (destination source)
                   `(setf (fdefinition ',destination)
                          (fdefinition ',source))))
        (def sb-bignum:multiply-bignums gmp-mul)
        (def sb-bignum:bignum-truncate gmp-truncate)
        (def sb-bignum:bignum-gcd mpz-gcd)
        (def sb-kernel:two-arg-lcm gmp-lcm)
        (def sb-kernel:two-arg-+ gmp-two-arg-+)
        (def sb-kernel:two-arg-- gmp-two-arg--)
        (def sb-kernel:two-arg-* gmp-two-arg-*)
        (def sb-kernel:two-arg-/ gmp-two-arg-/)
        (def isqrt gmp-isqrt)
        (def sb-kernel::intexp gmp-intexp)))
  (values))

(defun uninstall-gmp-funs ()
  (with-package-locks-ignored
      (macrolet ((def (destination source)
                   `(setf (fdefinition ',destination)
                          ,(intern (format nil "*~A-FUNCTION*" source)))))
        (def multiply-bignums orig-mul)
        (def bignum-truncate orig-truncate)
        (def bignum-gcd orig-gcd)
        (def sb-kernel:two-arg-lcm orig-lcm)
        (def sb-kernel:two-arg-+ orig-two-arg-+)
        (def sb-kernel:two-arg-- orig-two-arg--)
        (def sb-kernel:two-arg-* orig-two-arg-*)
        (def sb-kernel:two-arg-/ orig-two-arg-/)
        (def isqrt orig-isqrt)
        (def sb-kernel::intexp orig-intexp)))
  (values))

(defun load-gmp (&key (persistently t))
  (setf *gmp-features* nil
        *gmp-version* nil
        *features* (set-difference *features* '(:sb-gmp :sb-gmp-5.0 :sb-gmp-5.1)))
  (when persistently
    (pushnew 'load-gmp sb-ext:*init-hooks*)
    (pushnew 'uninstall-gmp-funs sb-ext:*save-hooks*))
  (let ((success (%load-gmp)))
    (when success
      (setf *gmp-version* (extern-alien "__gmp_version" c-string)))
    (cond ((null *gmp-version*))
          ((string<= *gmp-version* "5.")
           (warn "SB-GMP requires at least GMP version 5.0")
           (setf success nil))
          (t
           (pushnew :sb-gmp *gmp-features*)
           (pushnew :sb-gmp-5.0 *gmp-features*)
           (when (string>= *gmp-version* "5.1")
             (pushnew :sb-gmp-5.1 *gmp-features*))
           (setf *features* (union *features* *gmp-features*))))
    (if success
        (install-gmp-funs)
        (uninstall-gmp-funs))
    (setup-5.1-stubs)
    success))

(defun unload-gmp ()
  (setf sb-ext:*init-hooks* (remove 'load-gmp sb-ext:*init-hooks*))
  (uninstall-gmp-funs)
  (setf sb-ext:*save-hooks* (remove 'uninstall-gmp-funs sb-ext:*save-hooks*))
  (values))

(load-gmp)
