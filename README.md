# EECS 345 Project

This repository contains our EECS 345 parser project code.  All our project work
should stay in this repository, and we can just tag each revision we turn in.
So, for the first assignment, we will tag it `part1`, etc.

Contributors:
* Stephen Brennan (smb196)
* Joe Fennimore (jrf118)
* Kaan Atesoglu (aka43)

**PART 1 COMPLETE!**
**PART 2**

**Required Environment changes**
*Layers*: Each layer will contain a list of variables and bindings similar to the basic environment of part 1. The initial environment consist of a single layer. Each time a new block is entered, you must "cons" a new layer to the front of your environment (but use abstraction and give the operation a better name than "cons"). Each time a variable is declared, that variable's binding goes into the top layer. Each time a variable is accessed (either to lookup its binding or to change it), the search must start in the top layer and work down. When a block is exited, the layer must be popped off of the environment, deleting any variables that were declared inside the block.


Concepts to implement and extend in Parser
* While
* Break
* Continue
* Blocks


