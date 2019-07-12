SHELL := /bin/bash
AS = asl
;AS_FLAGS = -L -cpu 65816 -gnuerrors -u -x -x -U
AS_FLAGS = -L -gnuerrors -u -x -x -U
C2T = c2t
STARTADDR = 900

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
	(date; emu65816 -s 900 pi.bin) | tee -a results

install:
	java -jar /Applications/AppleCommander.app/Contents/Resources/Java/AppleCommander.jar -pro140 pi.po PI
	java -jar /Applications/AppleCommander.app/Contents/Resources/Java/AppleCommander.jar -p pi.po pi BIN 0x900 <pi.bin
	java -jar /Applications/AppleCommander.app/Contents/Resources/Java/AppleCommander.jar -l pi.po 

