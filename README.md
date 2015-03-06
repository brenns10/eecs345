# EECS 345 Project

This repository contains our EECS 345 parser project code.  All our project work
should stay in this repository, and we can just tag each revision we turn in.
So, for the first assignment, we will tag it `part1`, etc.

Contributors:
* Stephen Brennan (smb196)
* Joe Fennimore (jrf118)
* Kaan Atesoglu (aka43)

**PART 1 COMPLETE!**

**PART 2: Blocks, While, Break, Continue**

*Layers*: Each layer will contain a list of variables and bindings similar to
 the basic environment of part 1. The initial environment consist of a single
 layer. Each time a new block is entered, you must "cons" a new layer to the
 front of your environment (but use abstraction and give the operation a better
 name than "cons"). Each time a variable is declared, that variable's binding
 goes into the top layer. Each time a variable is accessed (either to lookup its
 binding or to change it), the search must start in the top layer and work
 down. When a block is exited, the layer must be popped off of the environment,
 deleting any variables that were declared inside the block.

To Do:
* Implement layers in interpreter.
    * **DONE** by Stephen.
* Change all Mstate functions to take return, break, continue.
    * **DONE** by Stephen.
* While: must be done tail-recursively!
    * **DONE** by Stephen.
* Break, Continue
    * **DONE** by Stephen.
* Blocks
    * **DONE** by Stephen.

**NOTE FROM STEPHEN**

So, I pretty much did all of Part 2.  I promise, I didn't intend to.  I made the
call/cc branch as a way to experiment in how we should implement the return,
break, and continue portions of the code.  As it turns out, my experimenting
worked, and that pretty much finished the project.  I'd be willing to take some
time to sit down and talk to everyone about what I did while implementing it,
and how it works now.
