
include ./Makefile.inc
ifeq ($(XLEN), 32)
  width=4
else ifeq ($(XLEN),64)
  width=8
else
  width=16
endif

default: create_hex append$(width)

.PHONY: append4
append4:
	@printf '00000297\n02028593\nf1402573\n0182a283\n00028067\n00000000\n80000000\n00000000\n' | cat - boot.hex > temp && mv temp boot.hex
	@for number in $$(seq 1 7945) ; do \
    echo "00000000" >> boot.hex; \
  done;

.PHONY: append8
append8:
	@printf '0202859300000297\n0182b283f1402573\n0000000000028067\n0000000080000000\n' | cat - boot.hex > temp && mv temp boot.hex
	@for number in $$(seq 1 8069) ; do \
    echo "0000000000000000" >> boot.hex; \
  done;

.PHONY: append16
append16:
	@printf '0182b283f14025730202859300000297\n00000000800000000000000000028067\n' | cat - boot.hex > temp && mv temp boot.hex
	@for number in $$(seq 1 8127) ; do \
    echo "0000000000000000" >> boot.hex; \
  done;

.PHONY: create_hex
create_hex:
	@dtc -O dtb -o c-class.dtb -b 0 c-class.dts
ifeq ($(XXD_VERSION),2023)
	@xxd -R never -e -c $(width) c-class.dtb config.c-class
else
	@xxd -e -c $(width) c-class.dtb config.c-class
endif
	@head -n -1 config.c-class > 1
	@mv 1 config.c-class
ifeq ($(XLEN),128)
	@cat config.c-class| awk '{print $$5 $$4 $$3 $$2}' > temp
endif
ifeq ($(XLEN),64)
	@cat config.c-class| awk '{print $$3 $$2}' > temp
endif
ifeq ($(XLEN),32)
	@cat config.c-class| awk '{print $$2}' > temp
endif
	@mv temp boot.hex


.PHONY: clean
clean:
	@rm -f c-class.dtb boot.hex config.c-class c-class.dump
