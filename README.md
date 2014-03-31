SB-GMP
======

GMP integration for SBCL

This contrib enables the use of bignum computation routines from the
GMP library for SBCL internal bignum calculations.

Some functions can be transparently replaced within SBCL, namely:

 - sb-bignum:multiply-bignum
 - sb-bignum:bignum-truncate
 - sb-bignum:bignum-gcd
 - sb-kernel::two-arg-lcm
 - cl:isqrt
 - sb-kernel::intexp

and for making use of the GMP rational arithmetic:

 - sb-kernel::two-arg-+
 - sb-kernel::two-arg--
 - sb-kernel::two-arg-*
 - sb-kernel::two-arg-/

Most of the other SBCL bignum routines rely on these functions for the
heavy computational lifting.

However, SB-GMP also provides easy and transparent access to several
other functions of the GMP library and may be able to replace other
parts of the SBCL bignum machinery in the future. Refer to the GMP
documentation for the respective function specification.

The transparent SBCL integration is activated by calling

    (SB-GMP:INSTALL-GMP-FUNS)

in a similar way it can be deactivated via

    (SB-GMP:UNINSTALL-GMP-FUNS)
