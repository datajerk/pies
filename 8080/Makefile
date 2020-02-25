SHELL := /bin/bash
AS = asl
;AS_FLAGS = -L -cpu 8080 -gnuerrors -u -x -x -U
AS_FLAGS = -L -gnuerrors -u -x -x -U
C2T = c2t
STARTADDR = 0

SRC = $(shell echo *.s)
OBJ = $(SRC:%.s=%.p)
LST = $(SRC:%.s=%.lst)
BIN = $(SRC:%.s=%.bin)
HEX = $(SRC:%.s=%.hex)
MON = $(SRC:%.s=%.mon)

all: $(BIN) $(MON) $(HEX)

clean:
	-rm -f $(BIN) $(OBJ) $(LST) $(MON) $(HEX)

%.p: %.s
	$(AS) $(AS_FLAGS) $< 
	perl -pi -e 's/.*\x0C.*//' $*.lst

%.bin: %.p
	p2bin -r \$$-\$$ $< $@

%.hex: %.p
	p2hex -r \$$-\$$ $< $@

%.mon: %.bin
	$(C2T) -l $<,$(STARTADDR) $@

test:
	(date; emu8080 -q pi.bin) | tee -a results

