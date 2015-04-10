Testing
=======

The file `test.scm` contains a little test runner I (Stephen) wrote.  It's
pretty magical.  It works in Racket -- use Pretty Big as the language.  It no
longer works in MIT Scheme (which is a piece of shit).  Use Racket.


Creating Test Groups
--------------------

So, the test runner works on the concept that test groups are folders, and tests
are files.  Inside a test group folder (like `tests_pt1`) there is a file named
`spec.scm`.  This file contains a list.  The list has the following:

* The car is a string, either `"simple"` for simple interpreter, or `"function"`
  for function interpreter.
* The cdr is a list of pairs:
    * The car of the pair is the test number.
    * The cadr of the pair is the expected return value - a number, `true`,
    `false`, or `error`.

The test runner will run each test and compare the output to the expected
output.  If you have any questions about the test group/runner operation, ask me
or look at the existing tests.


Running Test Groups
-------------------

From the Racket command line, load `test.scm`.  If you're using Dr. Racket, and
you have `test.scm` open, that is done for you when you hit the run button.  If
you're on the command line, use `(load "test.scm")`.

Then, to run a test group, just run:

    (test "group_name")

For instance, for part 1:

    (test "tests_pt1")
