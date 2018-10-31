MODULES=opython main state parser evaluate utils
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OPYTHON=opython
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,str,oUnit,qcheck

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

run:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

all:
	$(OCAMLBUILD) opython.byte && mv opython.byte opython && alias OPython="./opython"

check:
	bash checkenv.sh
	
finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip search_src.zip *.ml* _tags Makefile .bashrc
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out
