
An assert unit for Turbo Pascal?
--------------------------------

After reading Steve Maquire's "Writing Solid Code,"
I wanted to apply practices in it to my own programming.
Part of it was to use assert to verify my assumptions
were not violated.  As Turbo Pascal provided no built-in
support for assert, thus I wrote one to provide this
functionality.


Table of Content

1. How to use the assert procedure
2. How to find the failed assertion under IDE
3. How to find the failed assertion under DOS command-line
4. How it works
5. Notes

1. How to use the assert procedure
----------------------------------

The assert procedure is declared in unit assert_u as:

procedure assert(blAssertExpr : boolean);

o if blAssertExpr is TRUE, assert would do nothing,
o otherwise, assert would print a message like
  "Assertion failed before xxxx:xxxx", and then
  terminate abnormally.

You use the address reported to find the failed
assertion.

Calls to the assert procedure are often placed in
blocks delineated by {$ifdef DEBUG} ... {$endif}.
While you're actively developing the program, you
would define the identifier "DEBUG" to let assert
thoroughly check your assumptions; when you are
going to ship your program, you undefine "DEBUG"
and rebuild, then no assert call would be in your
software.

Find the attached NILPTR.PAS to see how it's used.


2. How to find the failed assertion under IDE
---------------------------------------------

Before building your program, make sure your configuration
are set as follows:

o Compile/Destination: Disk
o Options/Debug Information: On

After assert reported the address, enter it at the "Error
Address" textbox of Compile/Find error.


3. How to find the failed assertion under DOS command-line
----------------------------------------------------------

Although TPC(command-line compiler of TP) has the option
to find run-time error(/F), but my test showed that what it
reported is not the failed assertion, but the statement
following it.  So I wrote a small program named "finderr"
(source included) to find the filename and line number of
the failed assertion.

As finderr depends on the detailed map file of your program,
build your program under the following setting if under IDE:

o Compile/Destination: Disk
o Options/Debug Information: On
o Options/Linker/Map file: Detailed

If you compile using the command-line compiler, remember to
include the Detailed map file option(/GD).

Take the included nilptr program for example, to find the
failed assertion, enter:

finderr nilptr.map 0000:0022

The 1st argument of finderr is the detailed map file name of
your program, and the 2nd is the address reported.

Finderr would correctly report the failed assertion:

Assertion failed at file NILPTR.PAS, line 11


4. How it works
---------------

Since procedure assert is placed in a separate unit, every
call to it would be a far call, thus the stack frame inside
assert looks like this:

        segment portion of return address   <--- bp+4
        offset portion of return address    <--- bp+2
        old bp                              <--- bp   

Unit assert_u provides assert with two inline directives,
wReturnSegment and wReturnOffset, to get the return address.
So the address reported is indeed the address following the
failed assertion.  Finderr uses this knowledge to find the
correct line number.


5. Notes
--------

I wrote this unit under TP 5.5, but I think it should work
under all versions of Turbo Pascal supporting Unit, that is,
4.0 or above.

This package may be used and distributed freely, provided it
is not changed in any way.  Uploading to bulletin boards is
encouraged. Please do not distribute any part of this package
separately.

The author shall not be liable to the user for any direct, indirect
or consequential loss arising from the use of, or inability to use,
any unit, program or file howsoever caused. No warranty is given
that the units and programs will work under all circumstances.
--adapted to compile on FPC, no further code modifications done--

Chih-Cherng Chin
elmo@iii.org.tw
