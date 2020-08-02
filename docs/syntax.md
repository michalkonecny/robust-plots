# Expression Syntax

In robust plots the expression input only certain input text is accepted. Each expression input is dependent on a single variable. For function plots it is `x` and for the parametric plots it is `t`. This doc explains the syntax of expressions.

## Literal values
In the expressions literal values like `2` can be added in. The parser will evaluate decimal values like `3.4`. Note that negatives will be parsed as negatives if there is no value to the left of the operator. To circumvent this wrap the negative value with parentheses like this `(-3)`.

## Constant variables
There are constant values that can be used as operands in the expressions.
- `pi` : Pi
- `e` : e

## Unary values
Unary operators are prefix notation and depending on the function expect there operand to be wrapped in parentheses.
- `-` : Negation [`-(20) = -20`]
- `abs` : Absolute [`abs(-25) = 25`]
- `sqrt` : Square root [`sqrt(25) = 5`]
- `e^` : Natural exponential
- `log` : Natural logarithm
- `sin` : Sine 
- `cos` : Cosine
- `tan` : Tangent

## Binary operations
Most binary operations use infix notation but `min` and `max` are prefix notation.For nesting operations use parentheses. Not that variables like `pi` and `e` can can be substituted for any literal value.
- `+` : Addition [`2+3 = 5`]
- `-` : Subtraction [`5-3 = 2`]
- `/` : Division [`10/2 = 5`]
- `*` : Multiplication [`5*3 = 15`]
- `^` : Power [`2^3 = 8`]
- `min` : Minimum [`min(5,3) = 3`]
- `max` : Maximum [`max(8,4) = 8`]

## Examples
The following are examples of function plot inputs.
- `sin(15*(x^4))`
- `x*sin(4/(x^2))`
- `max(0,x*sin(4/(x^2)))`
- `1/(1+1000000*(x-0.5)^2)`
- `0.1+(10^20)*(x^2-1)-(10^20)*(x+1)*(x-1)`
- `(1+x^2)^(sin(10*x))-1`
