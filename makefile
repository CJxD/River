
COMPILE=ocamlc -c
BUILD=ocamlc -o
LEX=ocamllex
YACC=ocamlyacc

OUTDIR=bin

all: build

build: lexer parser river
	$(BUILD) lexer.cmo parser.cmo river.cmo

river:
	$(COMPILE) river.ml

lexer:
	$(LEX) lexer.mll
	$(COMPILE) lexer.ml

parser:
	$(YACC) parser.mly
	$(COMPILE) parser.ml
	$(COMPILE) parser.mli