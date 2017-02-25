# Assignment notes

Author: **Simon Hellinger**

Assignment (1.24)

## Preface

There are two files, ``shapes.erl`` and ``bits.erl``, containing the two assignments.

All functions are tested with eunit. To test:

* download the files (or git clone the containing directory or project)
* run erlang in the directory containing the files
* compile the files with ``c(shapes).`` or ``c(bits).``
* test with ``eunit:test(shapes).`` or ``eunit:test(bits).``

All the major functions are also exported and can be called from the command line as usual.

## Shapes

We need to be able to calculate perimeter, area and the smallest enclosing rectangle for various shapes of our choosing, as long as triangles are among them.

My example supports the following shapes:

```erlang
{rectangle, {W, H}}     %(W ... WIDTH, H ... HEIGHT)
{circle, R}             %(R ... RADIUS)
{triangle, {A, B, C}}   %(A, B, C ... Sides of the triangle)
```

Owing to erlang's 'let it fail' strategy all other shapes will fail with an exception

### Perimeter
The perimeter is calculated as follows:

* Rectangle: 2 * W + 2 * H
* Circle: 2 * R * Pi
* Triangle: A + B + C

### Area
The area is calculated as follows:

* Rectangle: W * H
* Circle: R ^ 2 * Pi
* Triangle: Using Heron's formula, we get:
    * S = perimeter of the triangle / 2,
    * Area = Square Root of (S * (S - A) * (S - B) * (S - C))

### Enclose
The smallest enclosing rectangle is calculated by those formulas:

* Rectangle: encloses itself
* Circle: Using the diameter of the circle as W and H
* Triangle:
    * Use the longest side as base
    * Use the area formula A = (base * height) / 2 to get height = (2 * A) / base.
    * use base as W and height as H

## Bits

This exercise sees us calculate the binary representation of an integer and then sum the binary digits. For example, decimal seven is the binary 1, 1, 1, which, summed up, becomes 3. This has to be done as regular recursion and as tail recursion.

### Which is better, regular or tail recursion?

Although in this case, both variants look virtually the same, adding an accumulator variable to enable tail end recursion hurts readability. Other than that, tail end recursion usually wins the competition by allowing for better compiler optimization(i.e. stack-frame reuse).