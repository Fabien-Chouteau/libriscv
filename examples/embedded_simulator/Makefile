all:
	gprbuild

zfp_rv32i:
	cd runtime && gprbuild -p -P zfp_rv32i.gpr && gprinstall -v -f -p zfp_rv32i.gpr

test:
	../simulator/riscv-sim --htif embedded-riscv-sim
