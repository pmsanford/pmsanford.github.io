---
layout: post
title:  "AoC2019 - Day 2 Part 2: Intcode Program Inputs"
category: advent-of-code
tags: aoc2019 fotran
---

The Problem
-----------

If you read the AoC question in addition to my last post, you might have noticed I left something out: The question asks you to provide input to the program by modifying certain values in the provided memory. I left this out mostly because I forgot about it. For the first problem, I did this by simply editing the file before feeding it into my intcode computer.

The problem description here also provides some concrete names for different parts of an intcode program. For this specific program, the input values mentioned above have names: The value we insert at __[1]__ in memory is called the `noun` and the value for __[2]__ is the `verb`. These two values are constrained to the interval [0,99], meaning values from 0 to 99, including 0 and 99. It also specifies that the output of the program is stored at __[0]__.

Now, what this part of the exercise wants from us is to find what combination of `noun` and `verb` produces a specific output. This means we have to load the program, insert the inputs in memory, run it, and check if the output is the value we want. If not, we need to load the program again (and reset it to its initial state) and try again with another set of inputs. Obviously, doing this the way we did in the first problem (editing the file by hand) would be extremely tedious. So then, we need to parametrize our program.


Code Organization
-----------------

While we could just nest all of our VM's code directly in a loop in the main program body, that's pretty messy. This is a good time to learn about ways we can organize Fortran code. Since it's pretty clear we're going to be re-using this intcode computer later on, we're going to put a bit of effort into refactoring here. 


Subroutines
===========

We read about defining functions in the second article in this series. Subroutines are very similar to functions. There are two main differences:

1. Functions _must_ return a value. Subroutines don't return values (at least in the sense of a single result).
2. Functions are invoked with the familiar `foo = bar(baz)` syntax. Subroutines are invoked by using a special keyword: `call bar(foo, baz)`. 

Here's how you'd use a function and subroutine to add two:

```fortran
function add_two(val) result(res)
  integer, intent(in) :: val
  integer :: res

  res = val + 2
end function add_two

subroutine add_two(val, res)
  integer, intent(in) :: val
  integer, intent(out) :: res
  
  res = val + 2
end subroutine add_two
```

For simple functionality like this, they're pretty similar. I don't know what the best practice is in the Fortran community, but I've been using functions where there's one output and subroutines for everything else. One thing important to note is that these `intent` modifiers are not just fancier comments - writing to an   intent(in)` variable will cause undefined behavior.
