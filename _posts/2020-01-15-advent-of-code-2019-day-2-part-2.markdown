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

For simple functionality like this, they're pretty similar. I don't know what the best practice is in the Fortran community, but I've been using functions where there's one output and subroutines for everything else. One thing important to note is that these `intent` modifiers are not just comments with compiler support - writing to an `intent(in)` variable will cause undefined behavior.


Modules
=======

The main way of encapsulating code in Fortran is by way of __modules__. Modules are self-contained libraries that can share both code and data. We're only going to use them as a way of organizing code for now. Modules are declared like this:

```fortran
module hello
contains
  subroutine say_hello(name)
    character(len=:) :: name

    write(*,*) "Hello, ",name
  end subroutine say_hello
end module hello
```

Pretty simple, like a namespace or package in another language. To use it from another code file:

```fortran
program main
  use hello

  call say_hello("Paul")
end program main
```

With gfortran, modules can be compiled separately to object files for later linking, or implicitly linked during compilation by providing them on the command line along with the main program file (much like a multi-file C program).


Refactor
--------

So now we have the tools, let's get started in breaking down our VM. When breaking programs down like this, I like to think through the steps we're taking in high-level terms. This way, we can factor the code out into intuitive building blocks. Here's roughly the steps our VM takes:

1. Load the program from the CSV file into memory
2. Set the noun and verb inputs
3. Initialize the VM
4. Run the processor loop:
    1. Retrieve the op code at the instruction pointer as well as the parameter addresses, if applicable
    2. Run the operation specified by the op code and store the result
5. Write out the final state of memory

Load Memory
===========

If you recall, in our last program, we just read the entire file into a big string and then created a mask to parse it into a big array of integers. For the rest of the program, those all hung around in memory. That's fine if we're running one iteration, but we're about to potentially run 100 * 100 iterations. Let's encapsulate reading memory into a function and make sure we only hang on to what we need:

```fortran
  function load_memory(filename) result(instructions)
    implicit none
    integer :: infile = 20, sz, icount, i, ctr
    character :: cur
    character(*) :: filename
    character(:), allocatable :: content
    logical, allocatable :: sepmask(:)
    integer, allocatable :: instructions(:)
    open (unit=infile, file=filename, action='read')
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
    deallocate (content)
    deallocate (sepmask)
  end function
```

This is mostly the same as before, but this time we deallocate both the string we read in and the mask we generated before returning, as well as closing the file once we're done with it. Much cleaner.


Set Inputs
==========

This is pretty simple:

```fortran
  subroutine set_inputs(instructions, noun, verb)
    implicit none
    integer, intent(inout) :: instructions(*)
    integer, intent(in) :: noun, verb
    instructions(2) = noun
    instructions(3) = verb
  end subroutine
```


Run Computer
============

It turns out that "initializing the VM" in our simple computer is just setting the instruction pointer to the first memory location __[0]__, so I've lumped that in with the next step, running the main processing loop. This is the main entry point to our intcode computer library:

```fortran

  function run_computer(input_file, output_file, noun, verb, debug) result(exit_code)
    implicit none
    character(*), intent(in) :: input_file, output_file
    integer, intent(in) :: noun, verb
    integer :: icount, op, lhs, rhs, dst_idx, ptr
    integer, allocatable :: instructions(:)
    logical :: err, term
    logical, intent(in) :: debug
    character(80) :: msg
    integer :: exit_code

    exit_code = 0

    if (debug) then
      write (*, *) "Loading instructions from ", input_file
    end if

    instructions = load_memory(input_file)
    icount = SIZE(instructions)
    ptr = 1

    if (debug) then
      write (*, *) "Setting inputs to ", noun, " ", verb
    end if

    call set_inputs(instructions, noun, verb)

    if (debug) then
      write (*, *) "Beginning execution"
    end if

    do
      if (debug) then
        write (*, *) "Parsing at position ", ptr
      end if
      call parse_op(icount, ptr, instructions, op, lhs, rhs, dst_idx, err, term, msg)
      if (term) then
        if (err) then
          if (debug) then
            write (*, *) "Parse error: ", msg
          end if
          exit_code = 1
          exit
        end if
        exit
      end if
      if (debug) then
        write (*, *) "Processing op ", op, " at ", ptr
      end if
      call process_op(instructions, op, lhs, rhs, dst_idx, err, msg, debug)
      if (err) then
        if (debug) then
          write (*, *) "Process error: ", msg
        end if
        exit_code = 2
        exit
      end if
    end do

    call write_state(icount, output_file, instructions)
  end function
```

Phew, that's a long one. A good deal of it is debug statements. All that's really going on, program wise, is calling into the various other functions and subroutines we've refactored into here in a loop. The one other new thing we've added is an `exit_code` - This is to tell the caller whether our VM terminated normally or not and, if not, which part of processing failed (right now, either parsing or processing an operation).


Parse Op
========

Here we need to take in the instruction pointer and pull out the op code, values to operate on, and destination address for the result.

```fortran
  subroutine parse_op(icount, ptr, instructions, op, lhs, rhs, dst_idx, err, term, msg)
    implicit none
    integer, intent(in) :: icount, instructions(*)
    integer, intent(inout) :: ptr
    integer, intent(out) :: op, lhs, rhs, dst_idx
    logical, intent(out) :: err, term
    character(80), intent(out) :: msg

    err = .FALSE.
    term = .FALSE.

    if (ptr > icount) then
      err = .TRUE.
      term = .TRUE.
      msg = "Program terminated abnormally"
      return
    end if

    op = instructions(ptr)

    if (op == 99) then
      ptr = ptr + 1
      term = .TRUE.
      return
    end if
    ptr = ptr + 1
    lhs = instructions(ptr)
    ptr = ptr + 1
    rhs = instructions(ptr)
    ptr = ptr + 1
    dst_idx = instructions(ptr) + 1
    if (dst_idx < 0 .OR. dst_idx >= icount .OR. lhs < 0 .OR. lhs >= icount .OR. rhs < 0 .OR. rhs >= icount) then
      err = .TRUE.
      term = .TRUE.
      write (msg, *) "Segmentation fault. ptr: ", ptr
      return
    end if
    lhs = instructions(lhs + 1)
    rhs = instructions(rhs + 1)

    ptr = ptr + 1
  end subroutine
```

You can see we're using a new annotation, `intent(inout)` - this is a variable we both read from and write to. This is so we can adjust the instruction pointer from the caller here. Since we're no longer parsing these operations directly in our main processor loop, we also need a way of telling the caller whether the program has terminated. This is what the `term` variable is for. We also have an `err` variable to indicate whether we encountered an error. So `term = true` and `err = false` means we hit a `HALT` instruction and terminated normally. If `term = true` and `err = true`, we terminated for another reason (namely, one of the addresses we tried to operate on was invalid).


Process Op
==========

This one is fairy straightforward (for now) - either add two numbers or multiply them, then save the result.

```fortran
  subroutine process_op(instructions, op, lhs, rhs, dst_idx, err, msg, debug)
    implicit none
    integer :: res
    integer, intent(in) :: op, lhs, rhs, dst_idx
    character(80), intent(out) :: msg
    logical, intent(out) :: err
    logical, intent(in) :: debug
    integer, intent(inout) :: instructions(*)
    err = .FALSE.

    if (op == 1) then
      res = lhs + rhs
    else if (op == 2) then
      res = lhs*rhs
    else
      err = .TRUE.
      write (msg, *) "Uknown op code ", op
    end if

    if (debug) then
      write (*, *) "Writing ", res, " at ", dst_idx
    end if

    instructions(dst_idx) = res
  end subroutine
```

As you can see, we have also made an allowance for returning an error message. When I was writing this I was experimenting with various ways of configurable debugging - I don't like just deleting or commenting out the debugging statements when I've gotten everything working. Neither of these systems seems quite ergonomic to me though.


Write State
===========

This is pretty much unmodified from the previous version, just converted to a subroutine:

```fortran
  subroutine write_state(icount, filename, instructions)
    implicit none
    integer :: i, ofile = 21
    integer, intent(in) :: icount
    character(*), intent(in) :: filename
    integer, intent(in) :: instructions(*)
    open (unit=ofile, file=filename, action='write', status='replace')

    do i = 1, icount
      write (ofile, '(I0)', advance='no') instructions(i)
      if (i .NE. icount) then
        write (ofile, '(A)', advance='no') ','
      end if
    end do
    close (ofile)
  end subroutine
```

One thing to mention is `advance='no'` here - you might have been able to guess, but that tells Fortran not to print newlines after writing the characters.


Solution
--------

Okay, we've got our computer nice and pretty now. What about solving the actual problem? Well, recall the signature for `run_computer`:

```fortran
  function run_computer(input_file, output_file, noun, verb, debug) result(exit_code)
```

Since this takes in a noun and a verb, all we need to do is loop from 0 to 99 for each and run this every time:

```fortran
  outer: do noun = 0, 99
    do verb = 0, 99
      exit_code = run_computer(in_file, out_file, noun, verb, debug)
      
      ! Check if output is equal to desired output
      exit outer
    end do
  end do outer
```

You can see we're using a label to be able to break out of a two-level nexted loop. Yep, that's basically a GOTO. I wonder if people graduating today know what GOTO is. Anyway, how do we check the output of our computer? Well, I'll admit, the solution I came up with is more than a little lazy:

```fortran
  function read_output(file_name) result(val)
    character(*), intent(in) :: file_name
    integer :: val, fileno = 23

    open (unit=fileno, file=file_name, action='read')

    read (fileno, *) val

    close (fileno)
  end function
```

That's right, we read in the file that the `run_computer` method wrote out. If you remember from the problem statement, the result of the program is stored in the first memory position, so we just parse the first integer from the file and return it. Hey, it works. You're not my boss. Get off my back. Here's the entire main program:

```fortran
program exercise
  use intcode
  implicit none
  integer :: exit_code
  character(50) :: in_file, out_file
  logical :: debug = .FALSE.
  integer :: noun, verb, output

  in_file = 'two.input'
  out_file = 'two.output'

  outer: do noun = 0, 99
    do verb = 0, 99
      exit_code = run_computer(in_file, out_file, noun, verb, debug)

      if (exit_code > 0) then
        write (*, *) "Abnormal termination: ", exit_code
      end if

      output = read_output(out_file)

      if (output == 19690720) then
        write (*, *) "Found correct answer with noun ", noun, " and verb ", verb
        exit outer
      end if
    end do
  end do outer

  write (*, *) "Scanned ", noun*verb, " combinations"

contains
  function read_output(file_name) result(val)
    character(*), intent(in) :: file_name
    integer :: val, fileno = 23

    open (unit=fileno, file=file_name, action='read')

    read (fileno, *) val

    close (fileno)
  end function

end program exercise
```
