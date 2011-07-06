# You can get js_of_ocaml with the command 
# darcs get http://ocsigen.org/darcs/js_of_ocaml
# (you need to install darcs first: http://darcs.net/)

#You need ocaamldsort to execute `make depend`, you can download it at
#ftp://quatramaran.ens.fr/pub/ara/ocamldsort/ocamldsort-0.14.3.tar.gz

JS_OF_OCAML?=$(shell ocamlfind query js_of_ocaml)
LIB=$(JS_OF_OCAML_PATH)/lib

INC_DIR=$(sort $(dir $(wildcard */ */*/)))
INCLUDES=$(addprefix -I	,$(INC_DIR))
OCAMLFLAGS=$(INCLUDES) -package react -package js_of_ocaml.syntax -syntax camlp4o
OCAMLIFLAGS=$(INCLUDES) -package react -package js_of_ocaml -pp "cpp -traditional-cpp"
DOM_REACT=dom_react.cmo
DOM_REACT_LIB=dom_react.cma
LIBRARY=dom_react
MLS=$(shell find * -name "*.ml")
MLIS=$(shell find * -name "*.mli")
OCAMLFIND=ocamlfind
OCAMLDOC=ocamldoc

all: $(DOM_REACT_LIB)

-include .depend .ocamldsort

$(DOM_REACT_LIB):$(CMOS) $(DOM_REACT)
	$(OCAMLFIND) ocamlc -a -o $@ $^

.SECONDEXPANSION:

%.cmo: %.ml
	@echo "[CC] $@"
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLFIND) ocamlc $(OCAMLIFLAGS) -c $<

depend: ocamldsort

ocamldep:
	($(OCAMLFIND) ocamldep $(OCAMLFLAGS) $(MLS); \
	$(OCAMLFIND) ocamldep $(OCAMLIFLAGS) $(MLIS)) > .depend

ocamldsort: ocamldep
	echo CMOS=`< .depend ocamldsort -byte $(MLS) $(MLIS)` > .ocamldsort

# var:
# 	@echo $(FILES)

# doc:
# 	mkdir -p ../doc
# 	$(OCAMLFIND) $(OCAMLDOC) -stars -html -d ../doc $(OCAMLIFLAGS) goog.mli

# ocsi_doc:
# 	mkdir -p ../docdiv
# 	ocamlopt -shared -I /usr/lib/ocaml/ocamldoc -o ./odoc_ocsigen.cmxs ../tools/odoc_ocsigen.ml
# 	$(OCAMLFIND) $(OCAMLDOC) -stars -g ./odoc_ocsigen.cmxs -d ../docdiv \
# 		-t "Dom_React - API Reference" $(OCAMLIFLAGS) goog.mli

cleanall: clean
	-rm -f .depend .ocamldsort

clean:
	@echo "[CLEAN]"
	-rm -f $(DOM_REACT_LIB)
	-find . -name "*.cm[oix]" -exec rm {} \;
