(require :fiveam)

(defpackage :sb-gmp-test (:use "COMMON-LISP" :it.bese.fiveam))

(in-package :sb-gmp-test)

(def-suite sb-gmp-suite :description "Unit tests for the GMP lib interface.")

(in-suite sb-gmp-suite)

(defparameter *state* (sb-gmp:make-gmp-rstate))
(sb-gmp:rand-seed *state* 1234)

(defmacro defgenerator (name arguments &body body)
  `(defun ,name ,arguments
     (lambda () ,@body)))

(defgenerator gen-mpz (&key (limbs 5))
  (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits)))

(test mpz-add "Test the mpz-add function"
  (dotimes (i 5)
    (let ((limbs (+ (random #x1FFFF) 2)))
      (for-all ((neg-a (gen-integer :min 0 :max 1))
                (neg-b (gen-integer :min 0 :max 1))
                (a (gen-mpz :limbs limbs))
                (b (gen-mpz :limbs limbs)))
        (let ((ta (if (zerop neg-a) a (- a)))
              (tb (if (zerop neg-b) b (- b))))
          (is (= (+ ta tb)
                 (sb-gmp:mpz-add ta tb)))))))
  (is (= (+ #x7FFFFFFFFFFFFFFF #x7FFFFFFFFFFFFFFF)
         (sb-gmp:mpz-add #x7FFFFFFFFFFFFFFF #x7FFFFFFFFFFFFFFF))))

(test mpz-sub "Test the mpz-sub function"
  (dotimes (i 5)
    (let ((limbs (+ (random #x1FFFF) 2)))
      (for-all ((neg-a (gen-integer :min 0 :max 1))
                (neg-b (gen-integer :min 0 :max 1))
                (a (gen-mpz :limbs limbs))
                (b (gen-mpz :limbs limbs)))
        (let ((ta (if (zerop neg-a) a (- a)))
              (tb (if (zerop neg-b) b (- b))))
          (is (= (- ta tb)
                 (sb-gmp:mpz-sub ta tb))))))))

(test mpz-mul "Test the mpz-mul function"
  (dotimes (i 5)
    (let ((limbs (+ (random #x253F) 2)))
      (for-all ((neg-a (gen-integer :min 0 :max 1))
                (neg-b (gen-integer :min 0 :max 1))
                (a (gen-mpz :limbs limbs))
                (b (gen-mpz :limbs limbs)))
        (let ((ta (if (zerop neg-a) a (- a)))
              (tb (if (zerop neg-b) b (- b))))
          (is (= (* ta tb)
                 (sb-gmp:mpz-mul ta tb))))))))

(test mpz-truncate "Test the mpz-tdiv function"
  (dotimes (i 5)
    (let ((limbs (+ (random #x253F) 2)))
      (for-all ((neg-a (gen-integer :min 0 :max 1))
                (neg-b (gen-integer :min 0 :max 1))
                (a (gen-mpz :limbs limbs))
                (b (gen-mpz :limbs limbs)))
        (let ((ta (if (zerop neg-a) a (- a)))
              (tb (if (zerop neg-b) b (- b))))
          (multiple-value-bind (ld lr)
              (truncate ta tb)
            (multiple-value-bind (gd gr)
                (sb-gmp:mpz-tdiv ta tb)
              (is (and (= ld gd)
                       (= lr gr))))))))))

(test fixed-bugs "Tests for found bugs"
  (is (= (+ #x7FFFFFFFFFFFFFFF #x7FFFFFFFFFFFFFFF)
         (sb-gmp:mpz-add #x7FFFFFFFFFFFFFFF #x7FFFFFFFFFFFFFFF)))
  (let ((a 30951488519636377404900619671461408624764773310745985021994671444676860083493)
        (b 200662724990805535745252242839121922075))
    (multiple-value-bind (ld lr)
        (truncate a b)
      (multiple-value-bind (gd gr)
          (sb-gmp:mpz-tdiv a b)
        (is (and (= ld gd)
                 (= lr gr))))))
  (let ((a 320613729464106236061704728914573914390)
        (b -285049280629101090500613812618405407883))
    (multiple-value-bind (ld lr)
        (truncate a b)
      (multiple-value-bind (gd gr)
          (sb-gmp:mpz-tdiv a b)
        (is (and (= ld gd)
                 (= lr gr)))))))
