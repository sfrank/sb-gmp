(defsystem :sb-gmp-test
  :name "SB-GMP-TEST"
  :description "Regression tests for SB-GMP"
  :serial t
  :components ((:module sb-gmp-test
                :pathname ""
                :components ((:file "test"))))
  :depends-on (:sb-gmp :fiveam))

