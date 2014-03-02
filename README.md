River
=====

A stream-based programming language


### Useful OCaml Stuff

Enables stack traces on exceptions (provided -g is set on ocamlc in the makefile)

    export OCAMLRUNPARAM=b


### Running Problem Files

    bin/river spl/p1.spl < spl/p1.input

Is the same as

    ./run 1

Or run all problem files with

    ./run all


### Debugging Features

    bin/river spl/p1.spl -printast

or

    ./run 1 -printast

Will print the AST for a source file and ignore any input.

    ./run 1 -input

Will print out the stream lists after parsing the input.
