Testing
=======

I don't know what implementation you guys are using (Dr. Racket vs MIT vs
Guile).  I (Stephen) have both, but most of my development uses MIT scheme.  My
test harness (`test.scm`) is written for MIT Scheme (99% certain it doesn't work
on Dr. Racket).  If you want to run the tests in a completely automated way with
my harness, you gotta use MIT Scheme (it's easiest to use on Linux).

Put the tests in a subdirectory (`tests_pt#`).  Name each test `##.txt`.  Then,
create a file in the directory named `spec.scm`.  It should contain a list of
pairs.  The first value of the pair is the test number, and the second value is
the return value (use the atom `error` if an error is expected).  See the
existing spec files for examples.

Then, from the main repo directory, run this in the scheme command line:

    (load "test.scm")
    (test "tests_pt#")

And it will run each test, and let you know if it passed or failed, and a reason
it failed if it did so.
