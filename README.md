# cflang – the C**** Functional programming language

An experiment in programming language design and implementation.
Basically, an esoteric functional language.

cflang is a functional programming language without the ability to declare
variables and constants. The only way of declaring a *named value* in cflang is
through function parameters.

#### C-four stars/asterisks/globs? What?

It doesn't really mean anything. You can fill in these four asterisks with
whatever you really want, be it an inside joke, be it a swear word or slur,
it can even be politically charged (I take that back, please don't).

## Syntax

### Comments

Comments are made using `--`:

```
-- this is a comment
```

### Numbers

The only primitive available in cflang is a number. Numbers can be integers or
reals:

```
1     -- an integer
3.14  -- a real
1e-10 -- a real with an exponent
```

### Identifiers

Just like in C and other programming languages from its family, identifiers
in cflang begin with any lowercase or uppercase letter or an underscore,
followed by any number of lowercase or uppercase letters, underscores, or
digits. Here are some examples of valid identifiers:

```
hello
number9
python_style
nimStyle
CsharpStyle
```

There is no de facto "idiomatic" style cflang code should adhere to, so pick
your flavor. The language doesn't have a way of splitting programs into
multiple files anyways, so you may as well do what you want.

### Math

cflang supports the following mathematical expressions, following the usual
precedence found in math notation:

```
-1     -- negation
1 + 2  -- addition
1 - 2  -- subtraction
1 * 2  -- multiplication
1 / 2  -- division

(1 + 2) * 3  -- grouping

-- comparison
3 = 2   -- 0
3 <> 2  -- 1
3 < 2   -- 0
3 <= 2  -- 0
3 > 2   -- 1
3 >= 2  -- 1

-- logic; these operators are _not_ short-circuiting!
3 = 2 or 2 = 2   -- 1
3 = 2 and 2 = 2  -- 0
~1               -- 0
```

Additionally, there are four extra operators for I/O.

```
-- print the result of 1 + 2. the ! operator evaluates to its operand
(1 + 2)!

-- print ASCII character 33 ('!'). again, !! evaluates to its operand
33!!

-- read a number from the user and pass it to sin
sin: ?

-- read an ASCII character from the user and print it out
??!!
```

Upon reaching end of input, `?` and `??` return 0.

### Functions

cflang wouldn't be a functional language if it didn't have functions, so here's
how they look:

```
|a, b, c| {
  ...
}
```

First, the parameters of the function are specified in `||`. If a function does
not accept any parameters, the list can be omitted. Then, the body is enclosed
in `{}`.

The body is a list of expressions. Each expression in the body must be
terminated with a semicolon `;`.

For instance:

```
|a| {
  a + 1;
}
```

To call a function, the `:` operator is used, followed by comma-separated
parameters:

```
myfunction: 1, 2, 3;
```

This operator is left-associative, so this:

```
myfunction : 1 : 2;
```

is read like:

```
(myfunction: 1): 2;
```

Additionally, for cflang to be a Turing-complete language, some form of
branching is needed. This is also done using the `:` operator, but on numbers:

```
(x = 2) : { 1!; }
```

Using the `:` operator on a number results in the function on its right-hand
side being called if the left-hand side is not equal to 0. The result of the
expression is 0 if the function was called, or 1 if the function wasn't called.
This allows for an extra "else" branch to be added with the use of another call
operation:

```
(x = 2)  -- if x is equal to 2
-- print 1
: { 1!; }
-- otherwise, print 2
: { 2!; }
```

Note that function calls do not evaluate to anything. Well… they have to
evaluate to *something*, and that something is 0. This value *cannot* be
changed, however. The only way to return something from a function,
is by giving the returning function a callback, like so:

```
|calculate| {
  calculate: 1, |result| {
    result!;
  };
} :
-- calculate
|x, return| {
  return: x + 2;
}
```

### Variables? Constants? How do I declare them?

The fact is, *you don't*. The only way of creating a named value is passing it
to a function via a parameter. So for instance, if you want to declare a
constant `pi`, you need to create a function *specifically* for that:

```
|pi| {
  pi!;
}
: 3.14159265
```

The same applies to function definitions. The only way to name a function is to
pass it in as a parameter. The same applies to recursive functions: the only
way of creating a recursive function is passing itself as a parameter to
itself. Here's a simple loop counting 1..10 as an example:

```
|loop| {
  loop: loop, 1;
}
: |loop, i| {
  (i <= 10) : {
    i!;
    loop: i + 1;
  };
}
```

#### Advanced: Tail call optimization

The previous loop example is subject to what is called *tail call optimization*.
This optimization allows for effectively zero-cost recursion for iteration.

For each expression, the compiler keeps track of whether an expression is the
last one in a function's body. If that's true, the expression is what we call
a *tail expression*.

All calls that are tail expressions will use a special optimized path in the
virtual machine. Instead of allocating a new stack frame for the *tail call*,
the VM will simply replace the current stack frame with the new call, without
needing to allocate any extra memory.

Hence, when writing your cflang programs, try to leverage this optimization in
recursive loops as often as you can.

### Final example

As a final example, here's a function that computes the factorial of whatever
number you input to stdin. This example demonstrates various aspects of the
language, such as how closures can be used to simplify calling recursive
functions.

```
|fac| {
  fac: ?, |x| { x!; };
}
-- fac
: |n, yield| {
  |aux| {
    aux: aux, 1, 1;
  }
  : |aux, i, result| {
    -- here, we leverage tail call optimization by inverting the condition
    -- and making the iteration call be the last call in the function
    ~(i <= n) : {
      yield: result;
    } : {
      aux: aux, i + 1, result * i;
    };
  };
}
```
