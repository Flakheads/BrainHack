# BrainHack

BrainHack is a Haskell implementation of the Brain-Flak language.

## Running BrainHack

BrainHack can be run by compiling and running `BrainHack.hs`.  This can be compiled with ghc:

    $ ghc BrainHack.hs

To run a Brain-Flak program call the resulting binaries with the name of the file where the source is located and the arguments for the program.

Example:

    $ ./BrainHack source.flk 1 3 6

## Comparison to Rain-Flak

Here is a comparison between BrainHack and Rain-Flak (the standard Ruby interpreter)

### Speed

BrainHack is *much* faster than Rain-Flak.  Considerable testing puts the BrainHack at 50 times the speed of Rain-Flak.

### Comments

BrainHack uses `#{...}` for block comments. While in Rain-Flak `#` comments out the rest of the line.

BrainHack *requires* braces to be balanced inside of comments, while Rain-Flak does not parse its comments.

### ASCII input

BrainHack will automatically use ASCII input if you format your input as a string.  For example

    $BrainHack -e "({}())" '"String"'

BrainHack will also allow input of mixed types for example:

    $BrainHack -e "({}())" 2 '"String"'

Will have both `2` and `String` as inputs.

Rain-Flak requires a `-a` flag to be passed to turn on ASCII input, mixed input is not allowed in Rain-Flak.

### Debugging

BrainHack has a full debugger using the `@` syntax borrowed from Rain-Flak.  However BrainHack uses slightly different flags than Rain-Flak.
For a detailed comparison run both of the interpreters with the `-H` option.

Both interpreters have flags that are unique to the specific interpreter however Rain-Flak has more.

### Miniflak Optimizations

While both interpreters can run Miniflak code, BrainHack has special optimizations to make Miniflak faster when run.

### Additional Languages

BrainHack only supports Vanilla Brain-Flak and Miniflak (not all features are available with Miniflak), while Rain-Flak supports,
Vanilla, Mini-flak, Brain-Flak Classic and Brain-Flueue, all with full features.
