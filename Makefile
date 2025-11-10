OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

SOURCES = \
  ast.mli ast.ml \
  env.ml \
  check_scope.ml \
  check_types.ml \
  parser.mly \
  lexer.mll \
  main.ml

OBJS = \
  ast.cmo \
  env.cmo \
  check_scope.cmo \
  check_types.cmo \
  parser.cmo \
  lexer.cmo \
  main.cmo

all: cmoins

parser.ml parser.mli: parser.mly
	$(OCAMLYACC) $<

lexer.ml: lexer.mll
	$(OCAMLLEX) $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

ast.cmo: ast.cmi
parser.cmo: parser.cmi

cmoins: parser.ml lexer.ml $(OBJS)
	$(OCAMLC) -o cmoins $(OBJS)

clean:
	rm -f *.cmo *.cmi parser.ml parser.mli lexer.ml cmoins

.PHONY: all clean
