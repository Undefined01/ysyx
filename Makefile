help:
	mill -i __.test.runMain TopMain --help

test:
	mill -i __.test

verilog:
	mill -i __.test.runMain TopMain -td ./build

emu: verilog
	make -C difftest emu EMU_TRACE=1

difftest: emu
	build/emu -b 0 -i ${IMG}

clean:
	-rm -rf build

.PHONY: test verilog help emu clean
