MODULES=camel coin enemy main maze position projectile round_state authors scorer constant draw game_state
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte 
MAZE=draw_maze.byte
GRAPH=graphicsdemo.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg graphics -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,ounit2,graphics

default:
	utop
	
build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

main:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

maze:
	$(OCAMLBUILD) $(MAZE) && ./$(MAZE)

graph:
	$(OCAMLBUILD) $(GRAPH) && ./$(GRAPH)

clean:
	ocamlbuild -clean
	rm -rf *.byte
	rm -rf camels.zip
	rm -rf doc.public _coverage bisect*.coverage

bisect: clean test
	bisect-ppx-report html

docs: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

zip:
	zip camels.zip *.ml* _tags *.txt Makefile