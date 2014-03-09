
# Libraries directory

LIBDIR=lib

# Output Binary

BINARY=river

# Output Directories

BINDIR=bin
OBJDIR=obj
GENDIR=gen

# Source .ml files to include in the build (ORDER MATTERS)

SOURCES= language.ml errors.ml parser.ml lexer.ml comparison.ml math.ml input.ml streams.ml interpreter.ml river.ml

# OCaml precompiled libraries to include during linking

LIBRARIES= str.cma

# Compiler/Lexer/Parser commands

CC=ocamlc -g -I $(OBJDIR)
LEX=ocamllex
YACC=ocamlyacc

# Change .ml to .cmo in the sources list & add the object dir

OBJS= $(addprefix $(OBJDIR)/, $(SOURCES:.ml=.cmo))

# Execute a full river build

all: $(BINDIR)/$(BINARY)

# Link the interpreter

$(BINDIR)/$(BINARY): $(BINDIR) $(OBJS)
	@echo "-> Linking lexer, parser & objects"
	$(CC) -o $@ $(LIBRARIES) $(OBJS)
	chmod +x $@
	@echo "---> Done"

# Compile Module Interface

$(OBJDIR)/%.cmi: %.mli $(OBJDIR)
	@echo "-> Compiling interface: $<"
	$(CC) -o $@ -c $<

# Compile Module

$(OBJDIR)/%.cmo: %.ml $(OBJDIR)
	@echo "-> Compiling module: $<"
	$(CC) -o $@ -c $<

# Compile library module

$(OBJDIR)/%.cmo: $(LIBDIR)/%.ml $(OBJDIR)
	@echo "-> Compiling library module: $<"
	$(CC) -o $@ -c $<
	
# Compile Module (Attempt to build its Interface first, dont think this will work)
	
$(OBJDIR)/%.cmo: %.ml $(OBJDIR)/%.cmi $(OBJDIR)
	@echo "-> Compiling module: $<"
	$(CC) -o $@ -c $<

# Generate & compile the parser

$(OBJDIR)/parser.cmo $(OBJDIR)/parser.cmi: parser.mly $(GENDIR) $(OBJDIR)/language.cmo $(OBJDIR)/errors.cmo
	@echo "-> Generating parser..."
	$(YACC) -b$(GENDIR)/parser -v $<
	@echo "-> Compiling parser..."
	$(CC) -o $(OBJDIR)/parser.cmi -c $(GENDIR)/parser.mli
	$(CC) -o $(OBJDIR)/parser.cmo -c $(GENDIR)/parser.ml

# Generate & compile the lexer

$(OBJDIR)/lexer.cmo: lexer.mll $(GENDIR) $(OBJDIR)/parser.cmo $(OBJDIR)/parser.cmi
	@echo "-> Generating lexer..."
	$(LEX) -o $(GENDIR)/lexer.ml $<
	@echo "-> Compiling lexer..."
	$(CC) -o $(OBJDIR)/lexer.cmo -c $(GENDIR)/lexer.ml

# Create output directories

$(OBJDIR):
	@echo "-> Creating objects directory..." 
	@mkdir $(OBJDIR)

$(BINDIR):
	@echo "-> Creating binaries directory..."
	@mkdir $(BINDIR)

$(GENDIR):
	@echo "-> Creating generated files directory..."
	@mkdir $(GENDIR)

# Remove all generated Files

clean:
	@echo "-> Removing build directories..."
	@rm -rf $(BINDIR) $(OBJDIR) $(GENDIR)	
