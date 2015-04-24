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

**PART 4 COMPLETE!**

*Part 5: Objects*


*To-Do: Try/Catch:*

Basically, we're going to need to create a new "throw" continuation, and keep it
in the function call context.

- [ ] Modify the context to include the throw continuation.  This includes
  creating an accessor and modifier function, and modifying `ctx-default` to
  provide a good default value for the throw continuation.
- [ ] Create Mstate functions for throw, try, catch, finally.
