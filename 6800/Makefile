SHELL := /bin/bash
AS = asl
AS_FLAGS = -L -cpu 6800 -gnuerrors -u -x -x -U
C2T = c2t
STARTADDR = 280

SRC = $(shell echo *.s)
OBJ = $(SRC:%.s=%.p)
LST = $(SRC:%.s=%.lst)
BIN = $(SRC:%.s=%.bin)
MON = $(SRC:%.s=%.mon)

all: $(BIN) $(MON)

clean:
	-rm -f $(BIN) $(OBJ) $(LST) $(MON)

%.p: %.s
	$(AS) $(AS_FLAGS) $< 
	perl -pi -e 's/.*\x0C.*//' $*.lst

%.bin: %.p
	p2bin -r \$$-\$$ $< $@

%.mon: %.bin
	$(C2T) -l $<,$(STARTADDR) $@

test:
	./emu6800 -s 280 pi.bin

