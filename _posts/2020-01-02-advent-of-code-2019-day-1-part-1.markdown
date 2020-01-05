---
layout: post
title:  "Advent of Code 2019 - Day 1 Part 1"
category: advent-of-code
tags: aoc2019 fotran
---
Intro
-----

Welcome to my extremely late [Advent of Code][aoc] series.

Each year, I do Advent of Code with a language I've never used (or at least never used in anything serious). Previous years I've used Rust, Erlang, and Scheme. One unifying theme for this, though, is that I get about six days in and stop. Usually to do something frivolous like spend time with my family during the holidays. This year, I've decided to try to push myself through by writing about each solution. I've already completed six days of 25, so these first few will be written in retrospect.

The first few are fairly simple, so I'll concentrate more on Fortran than the problems themselves. Once we get to the meatier ones I'll focus more on the solutions than the syntax.

This year I've decided on Fortran. Initially it was interesting to me because of its history; It's [62 years old][fortran-wikipedia]. It's the precursor to most popular programming languages still in use today. Many design choices are familiar, but there are a lot of "archaic" ideas in it that the language design community has moved away from. I hope to write about some of the more interesting ones here. 

I'd like to thank the authors of the [Fortran wikibook][fortran-wikibook], which has been my primary resource in learning to solve these problems in Fortran. I've also used the [gfortran docs][gfortran-docs] for the specifics of the built-ins available.

Part One
--------

The first day is usually extremely simple. This is nice because it gives me a chance to flounder around figuring out the language, learning how to do basic things like reading files. This year is no different: The instructions are to simply take a list of numbers, apply a fixed formula (`floor(x/3) - 2`) to them, and sum them all up. So we'll learn reading files, math, and looping.


Reading Files
=============

Files are opened and assigned a `unit number` (which is essentially a file handle). All writes and reads (including from stdout/stdin) operate on a unit number.

{% highlight fortran %}
integer, parameter :: input_file = 20
open (unit=input_file, file="one.input", action="read")
{% endhighlight %}

The actual number for the unit number is entirely arbitrary. I chose 20 because that's what the wikibook examples use. The modifier `parameter` in the declaration means this will be a constant.

Looping
=======

The `do` loop is overloaded - all the loops are `do` loops. Here, we want to loop until we're out of lines in our file.

```fortran
do
  read (unit=input_file, fmt=*, end=10) current
  total = total + ((current/3) - 2)
end do

10 close(input_file)
```

The unit for the read statement is, of course, the unit number we opened before. There is some non-obvious stuff going on here too:

- `fmt=*` specifies the format of the data to read. Specifying `*` is called "list-directed" formatting, which means the list of variables afterwards determines how we'll read the data. We provide an integer (the `current`) variable, and so Fortran will try to read one integer from each line of input. We'll see more advanced `fmt` directives in the future.
- `end=10` specifies that when Fortran encounters the end of the file, it should jump to the line labelled `10`. There are more modern ways to handle this, but for now, we'll stick with a GOTO.

That's all there is for part one. Here's the full program:

```fortran
program exercise
  implicit none
  integer, parameter :: input_file = 20
  integer :: total, current

  total = 0
  open (unit=input_file, file="one.input", action="read")

  do
    read (input_file, *, end=10) current
    total = total + ((current/3) - 2)
  end do

10 close (input_file)

  print *, total

end program exercise
```


There's one more interesting thing here: `implicit none`: In Fortran, variables can be implicitly assigned a type. Variables starting with letters i through n are type `integer`, while all others are type `real`. using `implicit none` causes implicit variable definitions to be treated as errors. From what I've read, all modern fortran should use `implicit none`.

[fortran-wikipedia]: https://en.wikipedia.org/wiki/Fortran
[fortran-wikibook]: https://en.wikibooks.org/wiki/Fortran
[gfortran-docs]: https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html
[aoc]: https://adventofcode.com/
