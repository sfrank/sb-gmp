(defpackage :gmp-bignum (:use "COMMON-LISP" "SB-ALIEN" "SB-C-CALL"))
(in-package :gmp-bignum)

;;;; NOTE: if not annotated otherwise, all functions expect a true
;;;; SBCL bignum integer. This convention is not checked due to
;;;; optimization settings since this is a module mostly targeted at
;;;; compiler/runtime internal use. The function
;;;; SB-BIGNUM:MAKE-SMALL-BIGNUM converts a FIXNUM to a bignum for
;;;; these purposes.

(defconstant +bignum-raw-area-offset+
  (- sb-vm:other-pointer-lowtag
     sb-vm:n-word-bytes))

(defconstant +long-max+
  (1- (ash 1 (1- sb-vm:n-word-bits))))

;; tested with GMB lib 5.1
(sb-alien::load-shared-object "libgmp.so")
(defparameter *gmp-features* nil)
(progn
  (defparameter *gmp-version* (extern-alien "__gmp_version" c-string))
  (if (or (null *gmp-version*)
          (string<= *gmp-version* "5."))
      (error "SB-GMP requires at least GMP version 5.0")
      (progn
        (push :GMP5.0 *gmp-features*)
        (when (string>= *gmp-version* "5.1")
          (push :GMP5.1 *gmp-features*))
        (setf *features* (append *features* *gmp-features*) ))))

;;; types and initialization

(define-alien-type nil
    (struct gmpint
            (mp_alloc int)
            (mp_size int)
            (mp_d (* unsigned-long))))

;; Section 3.6 "Memory Management" of the GMP manual states: "mpz_t
;; and mpq_t variables never reduce their allocated space. Normally
;; this is the best policy, since it avoids frequent
;; reallocation. Applications that need to return memory to the heap
;; at some particular point can use mpz_realloc2, or clear variables
;; no longer needed."
;;
;; We can therefore allocate a bignum of sufficiant size and use the
;; space for GMP computations without the need for memory transfer
;; from C to Lisp space.

(declaim (inline z-to-bignum z-to-bignum-neg))

(defun z-to-bignum (b count)
  "Convert GMP integer in the buffer of a pre-allocated bignum."
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type sb-bignum::bignum-type b)
           (type sb-bignum::bignum-index count))
  (if (> (sb-bignum::%bignum-ref b (1- count))
         +long-max+) ; handle most signif. limb > LONG_MAX
      (sb-bignum::%normalize-bignum b (1+ count))
      (sb-bignum::%normalize-bignum b count)))

(defun z-to-bignum-neg (b count)
  "Convert to twos complement int the buffer of a pre-allocated
bignum."
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type sb-bignum::bignum-type b)
           (type sb-bignum::bignum-index count))
  (sb-bignum::%normalize-bignum 
   (sb-bignum::negate-bignum-in-place b)
   count))


;;; conversion functions that also copy from GMP to SBCL bignum space

(declaim (inline gmp-z-to-bignum gmp-z-to-bignum-neg))

(defun gmp-z-to-bignum (z b count)
  "Convert and copy a positive GMP integer into the buffer of a
pre-allocated bignum. The allocated bignum-length must be (1+ COUNT)."
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type (alien (* unsigned-long)) z)
           (type sb-bignum::bignum-type b)
           (type sb-bignum::bignum-index count))
  (dotimes (i count (if (> (sb-bignum::%bignum-ref b (1- count))
                           +long-max+) ; handle most signif. limb > LONG_MAX
                        (sb-bignum::%normalize-bignum b (1+ count))
                        (sb-bignum::%normalize-bignum b count)))
    (sb-bignum::%bignum-set b i (deref z i))))

(defun gmp-z-to-bignum-neg (z b count)
  "Convert to twos complement and copy a negative GMP integer into the
buffer of a pre-allocated bignum. The allocated bignum-length must
be (1+ COUNT)."
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type (alien (* unsigned-long)) z)
           (type sb-bignum::bignum-type b)
           (type sb-bignum::bignum-index count))
  (let ((carry 0)
        (add 1))
    (declare (type (mod 2) carry add))
    (dotimes (i count b)
      (multiple-value-bind (value carry-tmp)
          (sb-bignum::%add-with-carry 
           (sb-bignum::%lognot (deref z i)) add carry)
        (sb-bignum::%bignum-set b i value)
        (setf carry carry-tmp
              add 0)))))


;;;; rationals 

(define-alien-type nil
    (struct gmprat
            (mp_num (struct gmpint))
            (mp_den (struct gmpint))))

;;; memory initialization functions to support non-alloced results
;;; since an upper bound cannot always correctly predetermined
;;; (e.g. the memory required for the fib function exceed the number
;;; of limbs that are be determined through the infamous Phi-relation
;;; resulting in a memory access error.

;; use these for non-prealloced bignum values, but only when
;; ultimately necessary since copying back into bignum space a the end
;; of the operation is about three times slower than the shared buffer
;; approach.
(declaim (inline __gmpz_init
                 __gmpz_clear))

(define-alien-routine __gmpz_init void
  (x (* (struct gmpint))))

(define-alien-routine __gmpz_clear void
  (x (* (struct gmpint))))

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


(declaim (inline __gmpz_fac_ui
                 __gmpz_2fac_ui
                 __gmpz_primorial_ui
                 __gmpz_bin_ui
                 __gmpz_fib2_ui))

(define-alien-routine __gmpz_fac_ui void
  (r (* (struct gmpint)))
  (a unsigned-long))

#+:GMP5.1
(define-alien-routine __gmpz_2fac_ui void
  (r (* (struct gmpint)))
  (a unsigned-long))

#+:GMP5.1
(define-alien-routine __gmpz_primorial_ui void
  (r (* (struct gmpint)))
  (n unsigned-long))

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

(defmacro with-mpz-results (pairs &body body)
  (loop for (gres size) in pairs
        for res = (gensym "RESULT")
        collect `(,gres (struct gmpint)) into declares
        collect `(,res (sb-bignum:%allocate-bignum ,size))
          into resinits
        collect `(setf (slot ,gres 'mp_alloc) (sb-bignum::%bignum-length ,res)
                       (slot ,gres 'mp_size) 0
                       (slot ,gres 'mp_d) (sb-sys:int-sap
                                           (- (sb-kernel:get-lisp-obj-address ,res)
                                              +bignum-raw-area-offset+)))
          into inits
        collect `(if (minusp (slot ,gres 'mp_size)) ; check for negative result
                     (z-to-bignum-neg ,res (abs (slot ,gres 'mp_size)))
                     (z-to-bignum ,res (slot ,gres 'mp_size))) 
          into normlimbs
        collect res into results
        finally (return
                  `(let ,resinits
                     (sb-sys:with-pinned-objects ,results
                       (with-alien ,declares
                         ,@inits
                         ,@body
                         ,@normlimbs))
                     (values ,@results)))))

(defmacro with-mpz-vars (pairs &body body)
  (loop for (a ga) in pairs
        for length = (gensym "LENGTH")
        for plusp = (gensym "PLUSP")
        for arg = (gensym "ARG")
        collect `(,ga (struct gmpint)) into declares
        collect `(,length (sb-bignum::%bignum-length ,a)) into gmpinits
        collect `(,plusp (sb-bignum::%bignum-0-or-plusp ,a ,length)) into gmpinits
        collect `(,arg (if ,plusp ,a (sb-bignum::negate-bignum ,a))) into gmpinits
        collect a into vars
        collect `(setf (slot ,ga 'mp_alloc) ,length
                       (slot ,ga 'mp_size) (if ,plusp ,length (- ,length))
                       (slot ,ga 'mp_d) (sb-sys:int-sap
                                         (- (sb-kernel:get-lisp-obj-address ,arg)
                                            +bignum-raw-area-offset+)))
          into gmpvarssetup
        finally (return
                  `(with-alien ,declares
                     (let* ,gmpinits
                       (sb-sys:with-pinned-objects ,vars
                         ,@gmpvarssetup
                         ,@body))))))


(defmacro with-gmp-mpz-results (resultvars &body body)
  (loop for gres in resultvars
        for res = (gensym "RESULT")
        collect `(,gres (struct gmpint)) into declares
        collect `(__gmpz_init (addr ,gres)) into inits
        collect `(,res (sb-bignum:%allocate-bignum 
                        (1+ (abs (slot ,gres 'mp_size)))))
          into resinits
        collect `(if (minusp (slot ,gres 'mp_size)) ; check for negative result
                     (gmp-z-to-bignum-neg (slot ,gres 'mp_d) ,res (abs (slot ,gres 'mp_size)))
                     (gmp-z-to-bignum (slot ,gres 'mp_d) ,res (slot ,gres 'mp_size))) 
          into copylimbs
        collect `(__gmpz_clear (addr ,gres)) into clears
        collect res into results
        finally (return
                  `(with-alien ,declares
                     ,@inits
                     ,@body
                     (let* ,resinits
                       ;; copy GMP limbs into result bignum
                       (sb-sys:with-pinned-objects ,results
                         ,@copylimbs)
                       ,@clears
                       (values ,@results))))))

;;; function definition and foreign function relationships

(defmacro defgmpfun (name args &body body)
  `(defun ,name ,args
     (declare (optimize (speed 3) (space 3) (safety 0))
              (type sb-bignum::bignum-type ,@args))
     ,@body))

;; SBCL/GMP functions

(defgmpfun mpz-add (a b)
  (with-mpz-results ((result (1+ (max (sb-bignum::%bignum-length a)
                                      (sb-bignum::%bignum-length b)))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_add (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-sub (a b)
  (with-mpz-results ((result (1+ (max (sb-bignum::%bignum-length a)
                                      (sb-bignum::%bignum-length b)))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_sub (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-mul (a b)
  (with-mpz-results ((result (+ (sb-bignum::%bignum-length a)
                                (sb-bignum::%bignum-length b))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_mul (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-mod (a b)
  (with-mpz-results ((result (max (sb-bignum::%bignum-length a)
                                  (sb-bignum::%bignum-length b))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_mod (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-cdiv (n d)
  (let ((size (max (sb-bignum::%bignum-length n)
                   (sb-bignum::%bignum-length d))))
    (with-mpz-results ((quot size)
                       (rem size))
      (with-mpz-vars ((n gn) (d gd))
        (__gmpz_cdiv_qr (addr quot) (addr rem) (addr gn) (addr gd))))))

(defgmpfun mpz-fdiv (n d)
  (let ((size (max (sb-bignum::%bignum-length n)
                   (sb-bignum::%bignum-length d))))
    (with-mpz-results ((quot size)
                       (rem size))
      (with-mpz-vars ((n gn) (d gd))
        (__gmpz_fdiv_qr (addr quot) (addr rem) (addr gn) (addr gd))))))

(defgmpfun mpz-tdiv (n d)
  (let ((size (max (sb-bignum::%bignum-length n)
                   (sb-bignum::%bignum-length d))))
    (with-mpz-results ((quot size)
                       (rem size))
      (with-mpz-vars ((n gn) (d gd))
        (__gmpz_tdiv_qr (addr quot) (addr rem) (addr gn) (addr gd))))))

(defgmpfun mpz-powm (base exp mod)
  (with-mpz-results ((rop (sb-bignum::%bignum-length mod)))
    (with-mpz-vars ((base gbase) (exp gexp) (mod gmod))
      (__gmpz_powm (addr rop) (addr gbase) (addr gexp) (addr gmod)))))

(defgmpfun mpz-gcd (a b)
  (with-mpz-results ((result (min (sb-bignum::%bignum-length a)
                                  (sb-bignum::%bignum-length b))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_gcd (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-lcm (a b)
  (with-mpz-results ((result (+ (sb-bignum::%bignum-length a)
                                (sb-bignum::%bignum-length b))))
    (with-mpz-vars ((a ga) (b gb))
      (__gmpz_lcm (addr result) (addr ga) (addr gb)))))

(defgmpfun mpz-sqrt (a)
  (with-mpz-results ((result (ceiling (sb-bignum::%bignum-length a) 2)))
    (with-mpz-vars ((a ga))
      (__gmpz_sqrt (addr result) (addr ga)))))


;;; Functions that use GMP-side allocated integers and copy the result
;;; into a SBCL bignum at the end of the computation when the required
;;; bignum length is known.

(defun mpz-nextprime (a)
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type sb-bignum::bignum-type a))
  (with-gmp-mpz-results (prime)
    (with-mpz-vars ((a ga))
      (__gmpz_nextprime (addr prime) (addr ga)))))

(defun mpz-fac (n)
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type (unsigned-byte #.sb-vm:n-word-bits) n))
  (with-gmp-mpz-results (fac)
    (__gmpz_fac_ui (addr fac) n)))

#+:GMP5.1
(defun mpz-2fac (n)
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type (unsigned-byte #.sb-vm:n-word-bits) n))
  (with-gmp-mpz-results (fac)
    (__gmpz_2fac_ui (addr fac) n)))

#+:GMP5.1
(defun mpz-primorial (n)
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type (unsigned-byte #.sb-vm:n-word-bits) n))
  (with-gmp-mpz-results (r)
    (__gmpz_primorial_ui (addr r) n)))

(defun mpz-bin (n k)
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type sb-bignum::bignum-type n)
           (type (unsigned-byte #.sb-vm:n-word-bits) k))
  (with-gmp-mpz-results (r)
    (with-mpz-vars ((n gn))
      (__gmpz_bin_ui (addr r) (addr gn) k))))

(defun mpz-fib2 (n)
  (declare (optimize (speed 3) (space 3) (safety 0))
           (type (unsigned-byte #.sb-vm:n-word-bits) n))
  ;; (let ((size (1+ (ceiling (* n (log 1.618034 2)) 64)))))
  ;; fibonacci number magnitude in bits is assymptotic to n(log_2 phi)
  ;; This is correct for the result but appears not to be enough for GMP
  ;; during computation (memory access error), so use GMP-side allocation.    
  (with-gmp-mpz-results (fibn fibn-1)
    (__gmpz_fib2_ui (addr fibn) (addr fibn-1) n)))

;;;; Random bignum (mpz) generation

;; we do not actually use the gestalt of struct but need its size for
;; allocation purposes
(define-alien-type nil
    (struct gmprandstate
            (mp_seed (struct gmpint))
            (mp_alg int)
            (mp_algdata (* t))))

(declaim (inline __gmp_randinit_mt
                 __gmp_randseed
                 __gmp_randseed_ui
                 __gmpz_urandomb
                 __gmpz_urandomm))

(define-alien-routine __gmp_randinit_mt void
  (s (* (struct gmprandstate))))

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

(defstruct (gmp-rstate (:constructor %make-gmp-rstate))
  (ref (make-alien (struct gmprandstate))
   :type (alien (* (struct gmprandstate))) :read-only t))

(defun make-gmp-rstate ()
  "Instantiate a state for the GMP random number generator."
  (declare (optimize (speed 3) (space 3)))
  (let* ((state (%make-gmp-rstate))
         (ref (gmp-rstate-ref state)))
    (__gmp_randinit_mt ref)
    (sb-ext:finalize state (lambda () (free-alien ref)))
    state))

(defun rand-seed (state seed)
  "Initialize a random STATE with SEED."
  (declare (optimize (speed 3) (space 3) (safety 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (check-type state gmp-rstate)
  (let ((ref (gmp-rstate-ref state)))
    (cond
      ((typep seed '(unsigned-byte #.sb-vm:n-word-bits))
       (__gmp_randseed_ui ref seed))
      ((typep seed '(integer 0 *))
       (with-mpz-vars ((seed gseed))
         (__gmp_randseed ref (addr gseed))))
      (t
       (error "SEED must be a positive integer")))))

(defun random-bitcount (state bitcount)
  "Return a random integer in the range 0..(2^bitcount - 1)."
  (declare (optimize (speed 3) (space 3) (safety 0)))
  (check-type state gmp-rstate)
  (check-type bitcount (unsigned-byte #.sb-vm:n-word-bits))
  (let ((ref (gmp-rstate-ref state)))
    (with-mpz-results ((result (1+ (ceiling bitcount sb-vm:n-word-bits))))
      (__gmpz_urandomb (addr result) ref bitcount))))


(defun random-int (state boundary)
  "Return a random integer in the range 0..(boundary - 1)."
  (declare (optimize (speed 3) (space 3) (safety 0)))
  (check-type state gmp-rstate)
  (let ((b (bassert boundary))
        (ref (gmp-rstate-ref state)))
    (with-mpz-results ((result (sb-bignum::%bignum-length b)))
      (with-mpz-vars ((b gb))
        (__gmpz_urandomm (addr result) ref (addr gb))))))


;;; Rational functions

(declaim (inline blength bassert))
(defun blength (a)
  (declare (optimize (speed 3) (space 3) (safety 0)))
  (if (sb-impl::fixnump a)
      1
      (sb-bignum::%bignum-length a)))

(defun bassert (a)
  (declare (optimize (speed 3) (space 3) (safety 0)))
  (if (sb-impl::fixnump a)
      (sb-bignum:make-small-bignum a)
      a))

(defmacro defmpqfun (name gmpfun)
  `(defun ,name (a b)
     (declare (optimize (speed 3) (space 3) (safety 0))
              (sb-ext:muffle-conditions sb-ext:compiler-note))
     (let ((size (+ (max (blength (numerator a))
                         (blength (denominator a)))
                    (max (blength (numerator b))
                         (blength (denominator b))))))
       (with-alien ((r (struct gmprat)))
         (let ((num (sb-bignum::%allocate-bignum size))
               (den (sb-bignum::%allocate-bignum size)))
           (sb-sys:with-pinned-objects (num den)
             (setf (slot (slot r 'mp_num) 'mp_size) 0
                   (slot (slot r 'mp_num) 'mp_alloc) size
                   (slot (slot r 'mp_num) 'mp_d)
                   (sb-sys:int-sap
                    (- (sb-kernel:get-lisp-obj-address num)
                       +bignum-raw-area-offset+)))
             (setf (slot (slot r 'mp_den) 'mp_size) 0
                   (slot (slot r 'mp_den) 'mp_alloc) size
                   (slot (slot r 'mp_den) 'mp_d)
                   (sb-sys:int-sap
                    (- (sb-kernel:get-lisp-obj-address den)
                       +bignum-raw-area-offset+)))
             (let* ((an (bassert (numerator a)))
                    (ad (bassert (denominator a)))
                    (bn (bassert (numerator b)))
                    (bd (bassert (denominator b))))
               (let* ((anlen (sb-bignum::%bignum-length an))
                      (adlen (sb-bignum::%bignum-length ad))
                      (bnlen (sb-bignum::%bignum-length bn))
                      (bdlen (sb-bignum::%bignum-length bd)))
                 ;; handle negative numerators
                 (when (minusp an)
                   (setf anlen (- anlen))
                   (sb-bignum::negate-bignum-in-place an))
                 (when (minusp bn)
                   (setf bnlen (- bnlen))
                   (sb-bignum::negate-bignum-in-place bn))
                 (with-alien ((arga (struct gmprat))
                              (argb (struct gmprat)))
                   (sb-sys:with-pinned-objects (an ad bn bd)
                     (setf (slot (slot arga 'mp_num) 'mp_size) anlen
                           (slot (slot arga 'mp_num) 'mp_alloc) (abs anlen)
                           (slot (slot arga 'mp_num) 'mp_d)
                           (sb-sys:int-sap
                            (- (sb-kernel:get-lisp-obj-address an)
                               +bignum-raw-area-offset+)))
                     (setf (slot (slot arga 'mp_den) 'mp_size) adlen
                           (slot (slot arga 'mp_den) 'mp_alloc) (abs adlen)
                           (slot (slot arga 'mp_den) 'mp_d)
                           (sb-sys:int-sap
                            (- (sb-kernel:get-lisp-obj-address ad)
                               +bignum-raw-area-offset+)))
                     (setf (slot (slot argb 'mp_num) 'mp_size) bnlen
                           (slot (slot argb 'mp_num) 'mp_alloc) (abs bnlen)
                           (slot (slot argb 'mp_num) 'mp_d)
                           (sb-sys:int-sap
                            (- (sb-kernel:get-lisp-obj-address bn)
                               +bignum-raw-area-offset+)))
                     (setf (slot (slot argb 'mp_den) 'mp_size) bdlen
                           (slot (slot argb 'mp_den) 'mp_alloc) (abs bdlen)
                           (slot (slot argb 'mp_den) 'mp_d)
                           (sb-sys:int-sap
                            (- (sb-kernel:get-lisp-obj-address bd)
                               +bignum-raw-area-offset+)))
                     (,gmpfun (addr r) (addr arga) (addr argb)))))
               (sb-kernel::build-ratio (if (minusp (slot (slot r 'mp_num) 'mp_size))
                                           (z-to-bignum-neg num (- (slot (slot r 'mp_num) 'mp_size)))
                                           (z-to-bignum num (slot (slot r 'mp_num) 'mp_size)))
                                       (z-to-bignum den (slot (slot r 'mp_den) 'mp_size))))))))))

(defmpqfun mpq-add __gmpq_add)

(defmpqfun mpq-sub __gmpq_sub)

(defmpqfun mpq-mul __gmpq_mul)

(defmpqfun mpq-div __gmpq_div)

;;; Tests

;; test corner case of magnitude => twos-complement conversion
;; (mpz-add #x7FFFFFFFFFFFFFFF #x7FFFFFFFFFFFFFFF) => 18446744073709551614


