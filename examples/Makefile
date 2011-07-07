all: react_event calc

.PHONY: force
react_event: force
	@echo "[MAKE] in examples"
	@(${MAKE} -C react_event);

calc: force
	@echo "[MAKE] in examples"
	@(${MAKE} -C calc);

clean:
	@(${MAKE} -C react_event clean);
	@(${MAKE} -C calc clean);
