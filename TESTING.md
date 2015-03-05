Testing
=======

The file `test.scm` contains a little test runner I (Stephen) wrote.  It's
pretty magical.  It works in Racket -- use Pretty Big as the language.  It no
longer works in MIT Scheme (which is a piece of shit).  Use Racket.


Creating Test Groups
--------------------

So, the test runner works on the concept that test groups are folders, and tests
are files.  Inside a test group folder (like `tests_pt1`) there is a file named
`spec.scm`.  This file contains a list of pairs -- test number and return value.
The test runner just runs each test in that spec, and checks that it returns the
same value.

To check for errors, the test runner uses the atom `'error`.  So put that as
your return value if you expect an error.

If you have any questions about the test group/runner operation, ask me or look
at the existing tests.


Running Test Groups
-------------------

From the Racket command line, load `test.scm`.  If you're using Dr. Racket, and
you have `test.scm` open, that is done for you when you hit the run button.  If
you're on the command line, use `(load "test.scm")`.

Then, to run a test group, just run:

    (test "group_name")

For instance, for part 1:

    (test "tests_pt1")
