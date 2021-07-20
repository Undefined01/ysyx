test:
	mill -i __.test

verilog:
	mill -i __.test.runMain top.TopMain -td ./build

help:
	mill -i __.test.runMain top.TopMain --help

emu: verilog
	make -C difftest emu

difftest: emu
	build/emu -b 0 -i ready_to_run/microbench.bin --diff ready_to_run/riscv64-nemu-interpreter-so

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: test verilog help emu clean
