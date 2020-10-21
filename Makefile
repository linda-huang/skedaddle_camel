MODULES=camel coin enemy main maze position projectile state test maindemo
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=maindemo.byte 
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg graphics

default:
	utop
	
build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

demo:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

clean:
	ocamlbuild -clean