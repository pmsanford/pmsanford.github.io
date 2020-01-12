---
layout: post
title:  "AoC2019 - Day 2 Part 1: Intcode Computer"
category: advent-of-code
tags: aoc2019 fotran
---
The Problem
-----------
Ok, we're into the interesting stuff now. The problem for this one is to build a [virtual machine](https://en.wikipedia.org/wiki/System_virtual_machine) for an elfen computer architecture called Intcode. This is exciting for me, because I've been interested in these kinds of toy architectures [for a while](https://github.com/pmsanford/sic-debug). The first part of this article is going to be a slightly expanded rehash of the description in [the problem description](https://adventofcode.com/2019/day/2) on the AoC website.

We're going to simulate a very simple computer. Here's an extremely simplified description of how processors execute a program:

1. Start by reading a number (called an __opcode__) at a specific location in memory. This is called the __entry point__.
2. Based on this number, perform a single operation. In addition to the actual operation (such as addition or multiplication), this might involve:
  * Loading one or more __arguments__ from the memory location(s) following the instruction.
  * Storing the result of the operation
3. Advance the __instruction pointer__ by a certain amount. In the current exercise, the instruction pointer is always advanced by __4__ - we'll see why in a moment.
4. Unless the opcode says otherwise, start again at step 1. This time, instead of the __entry point__, read an __opcode__ from the location referred to by the __instruction pointer__.

The operations our Intcode computer is expected to perform (and the numbers they're represented by) are:

|Opcode|Operation|Description                   | Arguments                                                                        |
|------|---------|------------------------------|----------------------------------------------------------------------------------|
|     1|      ADD| Add two numbers together     | 3: The memory locations of two numbers to add plus a location to store the result|
|     2|      MUL| Multiply two numbers together| 3: Same as above, but to multiply                                                |
|    99|     HALT| Stop processing instructions | 0: Just stop                                                                     |

The initial state of memory will be represented by a comma-separated list of numbers we'll read off the hard drive. Our __entry point__ will always be the first memory location. As in most programming languages, the first memory location is position __0__. In this and the following articles about the Intcode computer, I'll be referring to a memory location with a number in square brackets. So memory location 0 will look like this: __[0]__

So, let's take a look at the example program from Advent of Code (here broken into individual instructions): 

`1,9,10,3`
`2,3,11,0`
`99`
`30,40,50`

You might notice that there are values in memory following the `HALT` instruction - since this computer (like the computer on which you're reading this) doesn't segregate executable instructions from data, any data upon which you want to operate shares space with the processor instructions. This last triplet is just data, and won't be executed by our processor (absent any bugs).

The first instruction is broken down like this:

`1`: This is the opcode for ADD.

`9`: This is the __location of__ the first number we'll be adding. Since the first memory address is 0, this argument refers to the 10th value in memory. If we count up, we find this is `30`, the value immediately following the `HALT` instruction in the full program.

`10`: This is the address of the second argument, which is `40` (immediately following the previous one).

`3`: This is another address: The location to store the result. Notice that this is the 4th part of this instruction. We started this instruction at 0, so this memory location actually refers to itself.

So then, when our VM encounters this instruction, it should add __[9]__ (which contains the number 30) to __[10]__ (which contains the number 40) and store the result at __[3]__. After executing the first instruction, the memory should look like this:

`1,9,10,70`
`2,3,11,0`
`99`
`30,40,50`

The next instruction at __[4]__ is a multiply instruction, __[3]__ * __[11]__ stored at __[0]__ (which means it's overwriting the opcode from the previous instruction). After that we move the instruction pointer up 4 again, to __[8]__ which contains the `HALT` instruction. The final state of memory is:

`3500,9,10,70`
`2,3,11,0`
`99`
`30,40,50`

The Solution
------------

Before we get into writing the VM, let's look at a few more Fortran features we haven't seen yet.

Arrays
======

One of Fortran's fortes (heh) is array handling. This is often cited as one of the reasons it has survived so long in the scientific and academic community. Some of its features, like robust slicing, are surprisingly sophisticated. Arrays are declared and used like this:

```fortran
integer :: arr_one(6) ! a one-dimensional array of length six. One-indexed.
integer :: arr_two(2, 3) ! a two-dimensional array, two 3-element arrays
integer :: arr_three(0:5) ! a one-dimensional array of length six, zero-indexed.
integer :: arr_four(-2:3) ! yep. a one-dimensional array of length six, negative two indexed.

arr_one(1) = 5 ! Set the first element to 5 (yes, they're 1-indexed)
arr_one(0) = 1 ! Probably cause a segmentation fault when this array is deallocated

arr_one(4:6) = arr_one(1:3) ! set the values at indicies 4-6 to the values from 1-3

arr_one = 3 ! Set every element to 3
arr_one = (/1, 2, 3, 4, 5, 6/) ! Initialize every element at once

arr_two(1, 1) = 5 ! Set the first element of the first array to 5

arr_three(0) = 5 ! Set the first element to 5 (yes, you can set the indexing)

arr_four(-2) = 5 ! Set the first element to 5 (I know right)


arr_two = reshape((/1, 2, 3, 4, 5, 6/), shape(arr_two)) ! Initialize all arrays
```

One interesting tidbit about that last one: Arrays are represented in memory in column-major order. So the last line above is equivalent to:

```fortran
arr_two(1, :) = (/1, 3, 5/)
arr_two(2, :) = (/2, 4, 6/)
```

Allocation
==========

Fortran has two ways to use heap memory. Pointers, which we'll see later, and "allocatable" variables. Other than being allocated on the heap, they work just like regular versions of those variables. You declare them like this:

```fortran
integer, allocatable :: arr_one(:) ! Specify a colon for the array's size
character(len=:), allocatable :: str_one ! Specify a colon for the string's length

allocate(arr_one(5))
allocate(character(len=4) :: str_one)
```

Allocatable variables are automatically deallocated when they go out of scope. They are also guaranteed to be contiguous in memory (unlike memory allocated to pointers) which makes it more efficient to pass them around. The only downside is they can't alias existing memory. You also have to know the rank (number of dimensions) up front (but this is also true for pointers). 


CSV Parsing
===========

The first step is writing a CSV parser so we can load the initial state of our virtual machine's memory. Ok, we could write something to process some hard-coded values, but I'm doing this first. I'll end up revisiting this later on when the CSV data gets more complicated but for now I'll be using boolean arrays to help me parse them. Fortran documentation refers to these as `masks`. There are a few intrinsics that work on masks, like `COUNT`, which will tell you the number of `true`s in the mask.

My plan was, at first, to use what is a pretty cool consequence of the implicit-element-wise array assignment we saw earlier:

```fortran
integer :: my_ints(10)
logical :: my_mask(10) ! logical is the boolean type

my_ints = (/1, 2, 3, 4, 1, 2, 3, 4, 1, 2/)

my_mask = my_ints > 2

write(*,*) my_mask ! writing to the fileno * and format * writes to stdout with automatic formatting

! This prints F F T T F F T T F F
```

Cool, right? Unfortunately, strings (`character(len=10) :: str`) and character arrays (`character :: str(10)`) are not the same thing. So `my_mask = str == ','` doesn't work as hoped. Since I'd already settled on it, here's the implementation I ended up with:

```fortran
  do i = 1, sz
    cur = content(i:i)
    sepmask(i) = cur == ','
  end do

  icount = COUNT(sepmask) + 1
  allocate (integer :: instructions(icount))

  ctr = 2
  read (content, *) instructions(1)
  do i = 1, sz
    if (sepmask(i)) then
      read (content(i + 1:), *) instructions(ctr)
      ctr = ctr + 1
    end if
  end do
```

Here we're using what's called an __internal read__ to read values from the string variable `content` instead of from a file. We're looping over the mask array and for each true value, reading an integer from that index forward. Fortran knows to stop reading an integer when it encounters a non-numeric character (in this case, the comma delimiting the field), so this works.

You might be thinking "We don't need the mask at all here. What are you doing" and you'd be right. The only thing it's really being used for is the `COUNT` function, which we could easily recover from the loop. Also, this CSV parser will only work for CSVs where the values are all numbers. But this is good enough for now.


Virtual Machine
===============

Virtual Machine sounds fancy, but for this architecture the actual machinery is pretty simple. You just do these things in a loop:

1. Read the value (`op`) at the instruction pointer (`ptr`). This is initially set to the first address in memory, which is __[0]__. You might remember from above, though, that Fortran arrays start at 1 by default. This is a bit confusing, but you get used to it.
2. If the value is 99, break out of the loop (halt the machine)
3. Read the arguments at ptr+1 (`lhs` or left-hand side) and ptr+2 (`rhs` or right-hand side). Remember that these are addresses, so we have to look up the actual values to operate on based on the addresses stored here.
5. Read the destination (`dst`) at ptr+3
4. If `op` is 1, add `lhs` and `rhs` and store the value at `dst`
5. If `op` is 2, multiply `lhs` and `rhs` and store the value at `dst`
6. Increment the instruction pointer to the next instruction

So let's see how that looks in Fortran:

```fortran
  do
    if (ptr > icount) then ! We incremented the pointer past the end of the array - oops!
      write (*, *) "Program terminated abnormally"
    end if
    op = instructions(ptr)
    write (*, *) "Processing op ", op
    if (op == 99) then
      write (*, *) "Program terminated"
      exit
    end if
    ptr = ptr + 1
    lhs = instructions(ptr)
    ptr = ptr + 1
    rhs = instructions(ptr)
    ptr = ptr + 1
    dst = instructions(ptr)
    if (dst < 0 .OR. dst >= icount .OR. lhs < 0 .OR. lhs >= icount .OR. rhs < 0 .OR. rhs >= icount) then
      write (*, *) "Segmentation fault. ptr: ", ptr
      write (*, *) "O: ", op, " L: ", lhs, " R: ", rhs, " D: ", dst
      exit
    end if
    if (op == 1) then
      res = instructions(lhs + 1) + instructions(rhs + 1)
    else if (op == 2) then
      res = instructions(lhs + 1)*instructions(rhs + 1)
    else
      write (*, *) "Unknown op code ", op, " at position ", ptr
      exit
    end if
    write (*, *) "O: ", op, " L: ", lhs, " R: ", rhs, " D: ", dst, " P: ", res
    instructions(dst + 1) = res
    ptr = ptr + 1
  end do
```

You can see I've added some debug statements and error checking, like making sure dst and the pointers aren't outside the bounds of the array. You might also notice we keep adding 1 to all the addresses - this is because of Fortran's 1-indexed arrays that I mentioned above.


Full Program
============

That's pretty much it. I'll reproduce the program in full here, but they're getting a little big. In the future I'll just link to the repo with the code in it. The only thing we haven't covered here is writing out the full memory state at the end. This isn't necessary, but was nice for debugging.

```fortran
program exercise
  integer :: infile = 20, sz, icount, op, lhs, rhs, dst, res, ptr, ctr, ofile = 21
  integer, allocatable :: instructions(:)
  character(:), allocatable :: content
  logical, allocatable :: sepmask(:)
  character :: cur

  open (unit=infile, file='two.input', action='read')
  inquire (unit=infile, size=sz)
  allocate (character(sz) :: content)
  allocate (logical :: sepmask(sz))
  read (infile, '(A)') content
  close (infile)

  do i = 1, sz
    cur = content(i:i)
    sepmask(i) = cur == ','
  end do

  icount = COUNT(sepmask) + 1
  allocate (integer :: instructions(icount))

  ctr = 2
  read (content, *) instructions(1)
  do i = 1, sz
    if (sepmask(i)) then
      read (content(i + 1:), *) instructions(ctr)
      ctr = ctr + 1
    end if
  end do

  ptr = 1

  do
    if (ptr > icount) then
      write (*, *) "Program terminated abnormally"
    end if
    op = instructions(ptr)
    write (*, *) "Processing op ", op
    if (op == 99) then
      write (*, *) "Program terminated"
      exit
    end if
    ptr = ptr + 1
    lhs = instructions(ptr)
    ptr = ptr + 1
    rhs = instructions(ptr)
    ptr = ptr + 1
    dst = instructions(ptr)
    if (dst < 0 .OR. dst >= icount .OR. lhs < 0 .OR. lhs >= icount .OR. rhs < 0 .OR. rhs >= icount) then
      write (*, *) "Segmentation fault. ptr: ", ptr
      write (*, *) "O: ", op, " L: ", lhs, " R: ", rhs, " D: ", dst
      exit
    end if
    if (op == 1) then
      res = instructions(lhs + 1) + instructions(rhs + 1)
    else if (op == 2) then
      res = instructions(lhs + 1)*instructions(rhs + 1)
    else
      write (*, *) "Unknown op code ", op, " at position ", ptr
      exit
    end if
    write (*, *) "O: ", op, " L: ", lhs, " R: ", rhs, " D: ", dst, " P: ", res
    instructions(dst + 1) = res
    ptr = ptr + 1
  end do

  open (unit=ofile, file='two.output', action='write', status='replace')

  do i = 1, icount
    write (ofile, '(I0)', advance='no') instructions(i)
    if (i .NE. icount) then
      write (ofile, '(A)', advance='no') ','
    end if
  end do
  close (ofile)

  write (*, *) "Instruction count: ", icount

end program exercise
```
