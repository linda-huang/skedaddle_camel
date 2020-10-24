MODULES=camel coin enemy main maze position projectile state test graphics_playground
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default:
	utop
	
build:
	ocamlbuild -use-ocamlfind main.cmo

test:
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte

clean:
	ocamlbuild -clean