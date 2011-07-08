all: react_event calc drag_and_drop

.PHONY: force
react_event: force
	@echo "[MAKE] in examples"
	@(${MAKE} -C react_event);

calc: force
	@echo "[MAKE] in examples"
	@(${MAKE} -C calc);

drag_and_drop: force
	@echo "[MAKE] in examples"
	@(${MAKE} -C drag_and_drop);

clean:
	@(${MAKE} -C react_event clean);
	@(${MAKE} -C calc clean);
