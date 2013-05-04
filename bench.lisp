(defpackage :sb-gmp-bench (:use "COMMON-LISP"))

(in-package :sb-gmp-bench)

(defparameter *stream* t)

(defparameter *state* nil)

(defun bench-+ () ; never
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        collect (,f i j)))))
    (loop for limbs fixnum from 2
          with gmp-win = 0
          until (or (= gmp-win 5)
                    (= limbs 80))
          do
             (loop 
               for i below 100000
               collect (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits))
                 into list-a
               collect (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits))
                 into list-b
               finally
                  (let (time1 time2)
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing 
                                          (lambda (&rest plist) 
                                            (setf time1 plist))
                                          (tstfun + list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing 
                                         (lambda (&rest plist) 
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpz-add list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbs time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))


(defun bench-* () ; limbs 5
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        collect (,f i j)))))
    (loop for limbs fixnum from 2
          with gmp-win = 0
          until (or (= gmp-win 3)
                    (= limbs 100))
          do
             (loop 
               for i below 10000
               collect (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits))
                 into list-a
               collect (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits))
                 into list-b
               finally
                  (let (time1 time2)
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing 
                                          (lambda (&rest plist) 
                                            (setf time1 plist))
                                          (tstfun * list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing 
                                         (lambda (&rest plist) 
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpz-mul list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbs time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))

(defun bench-/ () ; limbs 11 / 5
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        ;do (format t "a: ~s~%b: ~s~%" i j)
                        collect (,f i j)))))
    (loop for limbs fixnum from 4
          for limbs_b fixnum from 2
          with gmp-win = 0
          until (or (= gmp-win 3)
                    (= limbs 100))
          do
             (loop 
               for i below 100
               collect (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits))
                 into list-a
               collect (1+ (sb-gmp:random-bitcount *state* (* limbs_b sb-vm:n-word-bits)))
                 into list-b
               finally
                  (let (time1 time2)
                    (format t "bench it~%")
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing 
                                          (lambda (&rest plist) 
                                            (setf time1 plist))
                                          (tstfun truncate list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing 
                                         (lambda (&rest plist) 
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpz-tdiv list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbs time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))

(defun bench ()
  (let ((*state* (sb-gmp:make-gmp-rstate)))
    (sb-gmp:rand-seed *state* 1234)
    (bench-+)))
