LIBRARY=dom_react
DIRS=lib examples

all: lib examples

.PHONY: lib examples doc

lib:
	@echo "[MAKE] in lib"
	@(${MAKE} -C lib);

examples: lib
	@echo "[MAKE] in examples"
	@(${MAKE} -C examples);

install:
	ocamlfind install $(LIBRARY) lib/dom_react.cmi lib/dom_react.cma META

uninstall:
	ocamlfind remove $(LIBRARY)

doc:
	@(${MAKE} -C lib doc);

clean:
	@for D in $(DIRS);do\
	(echo "-> ./$$D");\
	(${MAKE} -C $$D clean);\
	done;

depend:
	@echo "[MAKE] depend in lib"
	@(${MAKE} -C lib depend);
