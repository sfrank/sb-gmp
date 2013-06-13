(defpackage #:sb-gmp-system (:use #:asdf #:cl))

(in-package #:sb-gmp-system)

(defsystem sb-gmp
  :name "SB-GMP"
  :version "0.1"
  :description "bignum calculations for SBCL using the GMP library"
  :serial t
  :components ((:module sb-gmp
                :pathname ""
                :components ((:file "gmp")))))

(defsystem sb-gmp-tests
  :depends-on (sb-rt sb-gmp)
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-gmp-tests))))
  (multiple-value-bind (soft strict pending)
      (funcall (intern "DO-TESTS" (find-package "SB-RT")))
    (declare (ignorable pending))
    (fresh-line)
    (unless strict
      (warn "ignoring expected failures in test-op"))
    (unless soft
      (error "test-op failed with unexpected failures"))))
