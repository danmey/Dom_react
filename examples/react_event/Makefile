SRC=$(wildcard *.ml)
EXECS=$(SRC:.ml=.byte)
TARGETS=$(SRC:.ml=.js)
LIB=-package js_of_ocaml $(DOM_REACT)/dom_react.cma
OCAMLFLAGS=-package react -package js_of_ocaml.syntax -syntax camlp4o -I $(DOM_REACT)
OCAMLLFLAGS=
DOM_REACT=../../lib/
JS_OF_OCAML_PREFIX=$(shell ocamlfind query js_of_ocaml)

all: $(TARGETS)

%.byte: %.ml $(DOM_REACT)
	@echo "[CC]" $@
	ocamlfind ocamlc -linkpkg -o $@ $(OCAMLFLAGS) $(LIB) $<

%.js: %.byte
	js_of_ocaml $^ -pretty -noruntime $(JS_OF_OCAML_PREFIX)/runtime/runtime.js $(JS_OF_OCAML_PREFIX)/runtime/weak.js

ocamldep:
	ocamlfind ocamldep -I $(DOM_REACT) $(OCAMLFLAGS) $(SRC) 

clean:
	@echo "[CLEAN]"
	-rm -f *.cmo *.cmi $(TARGETS) $(EXECS) *_req.js

var: 
	@echo $(TARGETS)
	@echo $(EXECS)
