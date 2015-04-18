# EECS 345 Project

This repository contains our EECS 345 parser project code.  All our project work
should stay in this repository, and we can just tag each revision we turn in.
So, for the first assignment, we will tag it `part1`, etc.

Contributors:
* Stephen Brennan (smb196)
* Joe Fennimore (jrf118)
* Kaan Atesoglu (aka43)

For this part on, we'll be using the
[Github Flow](https://guides.github.com/introduction/flow/).  The long and short
of it, if you've never done it, is:

    $ git pull origin master           # Update your repo.
    $ git checkout -b name-of-branch   # Create a new branch for something.
    $ emacs ...                        # Make your changes.
    $ git commit ...                   # Commit them.
    $ emacs ...                        # Continue making changes.
    $ git commit ...                   # Commit as you go, as many as you'd like.
    $ git push --set-upstream origin name-of-branch

Once you've pushed the branch to Github, you can create a pull request.  We get
to look at the code, run it ourselves, and make comments.  Once it's approved by
the other partners, we merge it in on Github.  This is a pretty standard
workflow, and it's very useful!

**PART 1 COMPLETE!**

**PART 2 COMPLETE!**

**PART 3 COMPLETE!**

**PART 4: Classes :(**

- [x] Use an environment that separates the names from the values, and have the
  values stored in reverse order, using the index of the name to look up the
  value. (This will be needed in part 5.)
- [x] Create helper functions to create a new class and instance (will be needed
  for part 5) and to access the portions of a class and instance.
- [x] All M_state and M_value functions will need to pass parameters for the
  class (the compile-time type) and instance ("this" - needed for part 5).
- [ ] Change the top level interpreter code that you used in part 3 to return a
  class instead of returning an environment.
- [ ] Change the top level interpreter code that you wrote for part 3 to expect
  static and non-static declarations for variables and functions.
- [ ] Update your code that interprets a function definition to add a new
  function to the closure that looks up the function's class in the environment.
- [ ] Create a new global level for the interpreter that reads a list of class
  definitions, and stores each class with its definition in the environment.
- [ ] Create a function that takes a variable, a class, and an instance, and
  checks if the variable is in the list of class or instance variables and
  returns its value.
- [ ] Create a function that takes a variable, and environment, a class, and an
  instance, if the variable is in the environment, look it up, otherwise look in
  the class and instance variables. (Why both this function and the one above
  it? To deal with when you have a dot and when you don't.)
- [ ] Create a pair of functions (or a single function that returns a pair) that
  takes the left hand side of a dot expression and returns the class and the
  instance of the left hand side.
- [ ] Update the code that evaluates a function call to deal with objects and
  classes. (Follow the denotational semantics sketched in lecture.)
- [ ] Update the code that interprets an assignment statement so that it looks
  for the variable in the environment, class and instance variables.
- [ ] Create a new interpret function.
