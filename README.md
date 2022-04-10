# Minblur Compiler
Minblur is a compiler for a superset of "logic" programming language in the game
[Mindustry](https://github.com/Anuken/Mindustry). It helps reduce code-duplication,
making it easier to reuse the same logic in different places in your code without
extra instructions.

Try out the [online demo](https://bindernews.github.io/minblur-web-demo/) in your
browser now!

# Features
Minblur is a strict superset of Mindustry's regular "logic" text language, meaning
you can copy-paste existing code into the compiler and it'll work. Then you can
start to modify you code to use advanced features including...

## Labels
One of the major pain-points with "logic text" is that `jump` instructions use numbers
to indicate their destination, so inserting or removing code will break `jump`s.
The solution is labels. A label is a name that will be resolved into a `jump` destination.

```
start:
  read isOn cell1 0
  jump $label:start equal isOn 0
  radar player any any distance @this 1 p
  sensor isDead p @dead
  jump $label:start equal isDead true
```

## Constants
Code has lots of numbers, especially when dealing with memory cells and unit control.
Keeping track of which number means what can be challenging, especially as the project
grows and there are multiple processors involved. Is `health` at cell index 4, or 5?
Constants help solve this problem.

```
# Define a constant
.define health_index 5

# A $ indicates a constant, which will be replaced with the correct value
# by the compiler. Also # starts a comment.
read health cell1 $health_index

# You can also have constant-expressions like this: ${<expression>}
.define player_index 2
read health cell1 ${player_index + 3}

# There are some shortcuts for common constant expressions, like getting a label address.
# These take the form "$<function>:<value>". So this:
jump $label:HesDeadJim health lessThan 0
# is the same as this
jump ${label("HesDeadJim")} health lessThan 0
HesDeadJim:
```

## Macros
Code duplication is an issue, especially when dealing with multiple logic processors
that have similar code but one or two slightly different values. You could just have
a few `set` instructions at the top of each processor, but you still have to keep
them in sync and it's annoying. Macros to the rescue! Macros allow you to generate
the same code but with different constants.

```
# This macro takes one argument: controller
# Inside the macro, use $controller to reference the value
.macro shoot_control(controller)
  start:
  sensor shoot $controller @shooting
  sensor aimX $controller @shootX
  sensor aimY $controller @shootY
  set i 0
  fire_loop:
  getlink bldg i
  m! i += 1
  # Don't try to control the controller
  jump $label:fire_end equal bldg $controller
  control shoot bldg bldgAimX bldgAimY shoot 0
  fire_end:
  m! jump(fire_loop, i < @links)
  m! jump(start)
.endmacro
# Here lancer1 is the controller
shoot_control!(lancer1)
# Here salvo2 is the controller
shoot_control!(salvo2)
```

Macro arguments can be split across mutliple lines as long as they're between braces or
parenthesies. If a macro is all on one line, the braces/parenthesies may be skipped.

```
.macro dummy(arg0, arg1, arg2)
  # Does nothing
.endmacro
dummy!(
    bla,
    bla,
    bla,
)
dummy!{ abc, def,
  ghi }
# No braces or parenthesies necessary
dummy! abd, def, ghi
```

## Math Macro
One of the built-in macros is `math!` or just `m!`. It has its own syntax and is
designed to make writing math expressions, and mindustry code in general,
much easier. Some functions (e.g. `read()`) have return values and can be used
in expressions like normal. Other functions (e.g. `jump()`) have no return,
and can only be called as their own statement (see the example). In the
list of functions below a `-> result` after the function means it returns
the `result` parameter of the given instruction, and can be used in expressions
like you'd expect. Note that the math functions are usually the `op` instruction.

```
sensor bx lancer1 @x
sensor by lancer1 @y
# These are math one-liners
start:
m! {
  dx = bx - @x
  dist = len(dx, by - @y)
}
print len
# Conditional jump
m! jump(print_out, len > 20)
print " - far away"
print_out:
printflush message1
# Unconditional jump
m! jump(start)
```

### Functions
- `read(cell, index) -> result`
- `write(value, cell, index)`
- `draw(...)` - The `draw` instruction
- `print(to)` - The `print` instruction
- `drawflush()`
- `printflush()`
- `getlink(index) -> result`
- `control(...)` - The `control` instruction
- `radar(target1, target2, target3, sort, source, order) -> result` - The `radar` instruction
- `sensor(from, sense_type) -> result`
- `jump(label)` OR `jump(label, condition)` - The `jump` function has two versions, the first
  generates an "always" jump, the second generates a conditional jump.
- `max(a, b) -> result`
- `min(a, b) -> result`
- `angle(x, y) -> result`
- `len(x, y) -> result`
- `noise(x, y) -> result`
- `abs(x) -> result`
- `log(a, b) -> result`
- `log10(x) -> result`
- `floor(x) -> result`
- `ceil(x) -> result`
- `sqrt(x) -> result`
- `rand(x) -> result`
- `sin(x) -> result`
- `cos(x) -> result`
- `tan(x) -> result`
- `asin(x) -> result`
- `acos(x) -> result`
- `atan(x) -> result`

## Eval Macro
Due to how the parser works you can **not** use a constant in place of the instruction
name itself (see the example below). The easiest way around this is the `eval!` macro
which will do text-replacement and then parse that text.

```
.define INSTR set
# This will NOT work
$INSTR a 5
# This WILL work
eval!($INSTR a 5)

# This code:
.define index 1
eval!(set name${index} 4)
# Will generate this:
# set name1 4
```

## Comments
Comments start with a `#` and continue until the end of the line.
Comments starting with `#!` will be copied into the final output (though Mindustry will
remove them when code is copied into the game itself). These output comments can be
useful to denote code for different processors, if all your code is in one file.

Using the example from the [Macros](##Macros) section, adding some comments would
help make the output easier to read and make the sections more clear.

```
# This is a comment
#! Lancer code
shoot_control!(lancer1)
#! Salvo code
shoot_control!(salvo2)
#! End of code
```
