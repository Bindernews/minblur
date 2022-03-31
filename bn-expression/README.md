# bn-expression
This is an expression parsing and evaluation library. The expression type itself is generic
over the type of operators, making it easy to support alternate types of expressions.

The expression parser also supports function calls, partial evaulation, and some other
nice features. This was pulled out of `minblur` so it's probably not applicable to most
other projects, but feel free to use it if you want.
