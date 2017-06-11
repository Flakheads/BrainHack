# BrainHack

BrainHack is a Haskell implementation of the Brain-Flak language.

## Running BrainHack

BrainHack can be run by compiling and running `BrainHack.hs`.  This can be compiled with ghc:

    $ ghc BrainHack.hs

To run a Brain-Flak program call the resulting binaries with the name of the file where the source is located and the arguments for the program.

Example:

    $ ./BrainHack source.flk 1 3 6

## Special to BrainHack

Here are some things that are present in the BrainHack interpreter but might not be present in other interpreters.

### Comments

BrainHack uses `#{...}` for comments.  BrainHack *requires* braces to be balanced inside of comments.

