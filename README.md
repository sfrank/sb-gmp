SB-GMP
======

GMP integration for SBCL

This contrib enables the use of bignum computation routines from the
GMP library for SBCL internal bignum calculations.

So far only a few functions are transparently replace for SBCL, namely:

 - sb-bignum:multiply-bignum
 - sb-bignum:bignum-truncate
 - sb-bignum:bignum-gcd
 - sb-kernel::two-arg-lcm
 - cl:isqrt

Most of the other SBCL bignum routines rely on these function for the
heavy computational lifting.

However, SB-GMP also provides easy and transparent access to several
other functions of the GMP library and may be able to replace other
parts of the SBCL bignum machinery in the future. Refer to the GMP
documentation for the respective function specification.
