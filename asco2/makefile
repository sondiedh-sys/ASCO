EXEC = cmoins
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

SOURCES = ast.ml printer.ml parser.ml lexer.ml check.ml types.ml main.ml
OBJECTS = $(SOURCES:.ml=.cmo)

all: $(EXEC)

$(EXEC): $(OBJECTS)
	$(OCAMLC) -o $(EXEC) $(OBJECTS)

lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

# Interfaces
ast.cmi: ast.mli
	$(OCAMLC) -c ast.mli
printer.cmi: printer.mli ast.cmi
	$(OCAMLC) -c printer.mli
check.cmi: check.mli ast.cmi
	$(OCAMLC) -c check.mli
types.cmi: types.mli ast.cmi
	$(OCAMLC) -c types.mli
parser.cmi: parser.mli ast.cmi
	$(OCAMLC) -c parser.mli


%.cmo: %.ml
	$(OCAMLC) -c $<


ast.cmo: ast.cmi
printer.cmo: printer.cmi ast.cmi
parser.cmo: parser.cmi ast.cmi
lexer.cmo: parser.cmi
check.cmo: check.cmi ast.cmi
types.cmo: types.cmi ast.cmi printer.cmi
main.cmo: lexer.cmo parser.cmi ast.cmi printer.cmi check.cmi types.cmi

clean:
	rm -f *.cmo *.cmi parser.mli lexer.ml parser.ml $(EXEC)

.PHONY: clean all