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

**PART 3: Functions**

- [ ] Create `Mstate` for function declarations.
    - This involves creating a closure.
    - It needs to work in global scope, or inside another function.
- [ ] Create `Mstate` and `Mvalue` for function calls.
    - This should be substantially easier now that we're using boxes.
- [ ] Rework the overall parser to read declarations, then call `main`.
- [ ] Make sure functions with no `return` statement still end up calling the
  `return` continuation!

Note that since we have switched to boxes, we can stop worrying about doing
anything special for side effects.  They just work!
