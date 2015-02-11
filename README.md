# EECS 345 Project

This repository contains our EECS 345 parser project code.  All our project work
should stay in this repository, and we can just tag each revision we turn in.
So, for the first assignment, we will tag it `part1`, etc.

Contributors:
* Stephen Brennan (smb196)
* Joe Fennimore (jrf118)
* Kaan Atesoglu (aka43)

Goals:
* Pick a state data structure and create functions to work with it.  **DONE**
* Implement Mvalue.
    * For numerical expressions. **DONE**
    * For boolean expressions: add boolean operators and add boolean
      literals. (Joe)
    * For return statements. **DONE**
    * For assignment statements. (Joe)
    * For if statement (should return value of executed statement body). (Joe)
* Implement Mstate.
    * For expressions. **DONE (except side effects extra credit)**
    * For assignment. **DONE**
    * For variable declaration--as seen in test #4, needs to accept an
      expression as initialization. (Kaan)
    * For if statement. (Kaan)
* Implement `(interpret)`.  **DONE**
* Implement side effects (EC).
