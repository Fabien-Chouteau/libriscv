mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))

all:
	cd ada-bfd && ./configure --prefix=$(mkfile_dir)/ada-bfd/install && make all install
	gprbuild

test:
	RISCV_TARGET=../../compliance_target RISCV_PREFIX=riscv32-elf- RISCV_DEVICE=rv32ui RISCV_ISA=rv32ui make -C riscv-compliance
	RISCV_TARGET=../../compliance_target RISCV_PREFIX=riscv32-elf- RISCV_DEVICE=rv32i RISCV_ISA=rv32i make -C riscv-compliance
