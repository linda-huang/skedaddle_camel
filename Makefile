MODULES=camel coin enemy main maze position projectile round_state test maindemo scorer keypress_tests constant draw game_state
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte 
DEMO=maindemo.byte
MAZE=draw_maze.byte
GRAPH=graphicsdemo.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg graphics

default:
	utop
	
build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

demo:
	$(OCAMLBUILD) $(DEMO) && ./$(DEMO)

main:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

maze:
	$(OCAMLBUILD) $(MAZE) && ./$(MAZE)

graph:
	$(OCAMLBUILD) $(GRAPH) && ./$(GRAPH)

clean:
	ocamlbuild -clean
	rm -rf *.byte

zip:
	zip camels.zip *.ml* _tags *.txt Makefile