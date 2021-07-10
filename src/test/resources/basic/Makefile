.DEFAULT_GOAL := all

RV_TOOLCHIAN ?= riscv64-unknown-elf-
RV_AS ?= ${RV_TOOLCHIAN}as
RV_LD ?= ${RV_TOOLCHIAN}ld
RV_OBJCOPY ?= ${RV_TOOLCHIAN}objcopy
RV_OBJDUMP ?= ${RV_TOOLCHIAN}objdump
RV_ARCH ?= rv64i

CC ?= gcc

src_dir := src
include_dir := include
script_dir := script
build_dir := build

SRCS := $(wildcard ${src_dir}/*.s)
BINS := $(patsubst ${src_dir}/%,${build_dir}/%,${SRCS})
BINS := $(patsubst %.s,%.bin,${BINS})

${build_dir}/%.o: ${src_dir}/%.s ${include_dir}/common.h
	@echo + RV_AS $@
	@${RV_AS} -march=${RV_ARCH} -o $@ -I ${include_dir} $<

%.elf: %.o ${script_dir}/${RV_ARCH}.ld
	@${RV_LD} -T ${script_dir}/${RV_ARCH}.ld -o $@ $<

%.bin: %.elf
	@${RV_OBJCOPY} --strip-all $^ -O binary $@

.PHONY: all clean make_build_dir

make_build_dir:
	@mkdir -p ${build_dir}

all: make_build_dir \
	$(BINS) $(BINS:.bin=.elf)

clean:
	rm -r ${build_dir}
