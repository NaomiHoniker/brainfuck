# Clojure Brainfuck Interpreter
A Brainfuck interpreter in Clojure

See https://en.wikipedia.org/wiki/Brainfuck

Brainfuck is an esoteric programming language created in 1993 by Urban Müller, and is notable for its extreme minimalism.

The language consists of only eight simple commands and an instruction pointer. While it is fully Turing complete, it is not intended for practical use, but to challenge and amuse programmers. Brainfuck simply requires one to break commands into microscopic steps.

The language's name is a reference to the slang term brainfuck, which refers to things so complicated or unusual that they exceed the limits of one's understanding. 

Specifically, read https://en.wikipedia.org/wiki/Brainfuck#Language_design for details of how Brainfuck operates

**Important** for our version of Brainfuck, we also allow '*' as a symbol, which is an alias for ','
This is because we want to be able to directly type in Brainfuck code using the bf macro, and the Clojure reader treats commas as whitespace

## Layout
You will need to add code to `src/brainfuck/engine.clj` and `src/brainfuck/core.clj`

Do not change the function `-main` nor any of the unit tests. Please use all of our existing function signatures, but feel free to add your own as well.

`src/brainfuck/utils.clj` has utility functions that will be useful for your implementation
`resources` contains several example Brainfuck programs as well as sample outputs used for testing

## Running
Once your program is implemented, you can run it by executing `lein run [-- source ...]` from the current directory.

For example
```
$ lein run
```
without any sources provided will just run some example programs defined in -main

```
$ lein run -- resources/beer.bf
```
will run the 99 bottles of beer program found in resources/beer.bf


## Testing
Test the program by executing `lein test` from this directory. 
It will test the components you are supposed to implement, along with some complete Brainfuck programs
