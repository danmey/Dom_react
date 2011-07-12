all: react_event calc drag_and_drop follow

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

follow: force
	@echo "[MAKE] in examples"
	@(${MAKE} -C follow);

clean:
	@(${MAKE} -C react_event clean);
	@(${MAKE} -C calc clean);
	@(${MAKE} -C follow clean);
