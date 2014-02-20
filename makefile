
OUTDIR=bin
OBJECTS=objects
LIBRARIES=lib

CC=ocamlc
CFLAGS=-c -I $(OBJECTS)/
LEX=ocamllex
YACC=ocamlyacc

all: build

build: setup parser lexer river
	$(CC) -o $(OUTDIR)/river $(OBJECTS)/lexer.cmo $(OBJECTS)/parser.cmo $(OBJECTS)/river.cmo

setup:
	mkdir -p $(OUTDIR) $(OBJECTS)

lib: setup
	$(CC) $(CFLAGS) -o $(OBJECTS)/streams.cmo $(LIBRARIES)/streams.ml

objects/%.cmi: setup
	$(CC) $(CFLAGS) -o $(OBJECTS)/$@ $(LIBRARIES)/$(addsuffix .ml, $(basename $@))

parser: setup lib
	$(YACC) parser.mly
	$(CC) $(CFLAGS) -o $(OBJECTS)/parser.cmi parser.mli
	$(CC) $(CFLAGS) -o $(OBJECTS)/parser.cmo parser.ml

lexer: setup lib
	$(LEX) lexer.mll
	$(CC) $(CFLAGS) -o $(OBJECTS)/lexer.cmo lexer.ml

river: setup lib
	$(CC) $(CFLAGS) -o $(OBJECTS)/river.cmo river.ml

clean:
	rm -f $(OUTDIR)/* $(OBJECTS)/*