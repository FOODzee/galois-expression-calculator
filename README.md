# galois-expression-calculator

A scala library for parsing mathemitical expressions with support for parentheses and variables and evaluating them in finite (Galois) field `GF(2^4)` with arbitrary reduction polinomial.

features:
+ math operators: `+`, `-`, `*`, `/`, `**`(power)
+ parentheses `( )` and comma `,`
+ variable name: `$` with valid Java variable name
+ finite field arithmetic
+ elliptic curve group arithmetic 

not yet done:
- `GF(p^m)`
- easier set up of reduction polynomial
- replaceability of field without parser/scanner modification
- elliptic curve is not parameterized (it'd better be separate class like GaloisField)

### Usage

See `test/scala/io/github/facaiy/math/expression/MathExpSuite.scala`  
and `main/scala/io/github/foodzee/math/{Main, EllipticCurve}.scala`
