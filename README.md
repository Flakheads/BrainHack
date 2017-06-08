# BrainHack

BrainHack is a Haskell implementation of the Brain-Flak language.

## Running BrainHack

Currently you can run BrainHack via ghci, using the function `brainflak`.  This function 
takes a `String` representing the source code and a `[Integer]` of input.  It will output
a `[Integer]` for output.

---

Example execution:

    $ ghci BrainHack.hs
    GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
    [1 of 1] Compiling Main             ( BrainHack.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> brainflak "({({})({}[()])}{})" [9]
    [81]
