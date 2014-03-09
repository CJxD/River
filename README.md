River
=====

A stream-based programming language for the COMP2212 Coursework

### Authors

	Sam Lavers <sl17g12@soton.ac.uk 25390805>
	Chris Watts <cw17g12@soton.ac.uk 25274384>

### Useful OCaml Stuff

Enables stack traces on exceptions (provided -g is set on ocamlc in the makefile)

    export OCAMLRUNPARAM=b


### Running Problem Files

    bin/river spl/p1.spl < spl/p1.input

Is the same as

    ./run 1

Or run all problem files with

    ./run all

Run the handin-formatted (prX.spl) files with

	./run handin
