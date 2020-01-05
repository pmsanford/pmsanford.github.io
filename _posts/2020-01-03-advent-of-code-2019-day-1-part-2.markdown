---
layout: post
title:  "AoC2019 - Day 1 Part 2: Functions"
category: advent-of-code
tags: aoc2019 fotran
---
This is another short one - this builds on the code from [Part One]({% post_url 2020-01-02-advent-of-code-2019-day-1-part-1 %}):

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

In the previous exercise, we take the list of component weights, apply a function (`floor(x/3) - 2`) to them, and sum the results. Our task in part 2 is to take into account the weight of the fuel as well - so after we get the result of the function, we apply the function again, until the weight rounds to zero. This sounds like a job for recursion. Before we can write a recursive function, though, we need to learn how to write a function at all. The general function syntax is:

```fortran
  function fuel(mass) result(ct)
    implicit none
    integer, intent(in) :: mass
    integer :: ct
  end function fuel
```

There are some interesting things here:
- We declare both argument (`mass`) and return value variable (`ct`) in the function declaration
- We declare the types of these at the beginning of the block
- In addition to a type, we declare an "intent." In this case, we plan to read from `mass` but not write to it - we explicitly declare our intent here.

It may seem a little awkward that we separately declare the name and types of the argument and return value, but remember that originally all variables had implicit types - explicit typing was introduced recently (only 40 years ago).

Ok, so the function should be pretty simple:

```fortran
  function fuel(mass) result(ct)
    implicit none
    integer, intent(in) :: mass
    integer :: ct, curr
    curr = (mass/3) - 2
    if (curr > 0) then
      ct = curr + fuel(curr) ! compile error
    else
      ct = 0
    end if
  end function fuel
```

Oh, a compiler error. It turns out that to call a function from within itself, it has to be explicitly marked as `recursive`. This is part of a theme in Fortran (at least Fortran 77 and onwards) that prefers explicit to implicit, similar to the `intent` marks and some other features we'll see later on.

Simply adding the keyword `recursive` before the function definition fixes our problem. Here's the full solution below:

```fortran
program exercise
  implicit none
  integer, parameter :: input_file = 20
  integer :: total, current

  total = 0
  open (unit=input_file, file="one.input", action="read")

  do
    read (input_file, *, end=10) current
    total = total + fuel(current)
  end do

10 close (input_file)

  print *, total

contains

  recursive function fuel(mass) result(ct)
    implicit none
    integer, intent(in) :: mass
    integer :: ct, curr
    curr = (mass/3) - 2
    if (curr > 0) then
      ct = curr + fuel(curr)
    else
      ct = 0
    end if
  end function fuel

end program exercise
```
