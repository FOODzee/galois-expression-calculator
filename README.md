# math-expression-parser

A scala library for parsing mathemitical expressions with support for parentheses and variables and evaluating them in finite (Galois) field `GF(2^4)` with arbitrary reduction polinomial.

features:
+ math operators: `+`, `-`, `*`, `/`, `**`(power)
+ parentheses `( )` and comma `,`
+ variable name: `$` with valid Java variable name
+ finite field arithmetic

### Install

_**TODO**_

### Usage

A simple example:

_**TODO:**_ rework example below

```scala
import io.github.facaiy.math.expression.MathExp

val str = "1 + sqrt(2 * $a1) + $a2 ** 2"
val ex = MathExp.parse(str)

val variables = Map("a1" -> 2, "a2" -> 1)
val output = ex.eval(variables)
// output = 4.0

val output1 = ex.eval(Map("a1" -> 8.0, "a2" -> 2))
// output1 = 9.0
```
