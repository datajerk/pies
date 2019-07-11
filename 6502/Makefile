SHELL := /bin/bash
CL = cl65
CL_FLAGS = -t none --listing --list-bytes 100
C2T = c2t
STARTADDR = 800

SRC = $(shell echo *.s)
OBJ = $(SRC:%.s=%.o)
LST = $(SRC:%.s=%.lst)
MON = $(SRC:%.s=%.mon)
BIN = $(SRC:%.s=%)
DSK	= prodos.po
APPLECMD = java -jar /Applications/AppleCommander.app/Contents/Resources/Java/AppleCommander.jar

all: $(BIN) $(MON)

clean:
	-rm -f $(BIN) $(OBJ) $(LST) $(MON)

%: %.s
	$(CL) $(CL_FLAGS) $< 

%.mon: %
	$(C2T) -l $<,$(STARTADDR) $@
	#$(C2T) -l $< $@

install: all
	for i in $(BIN); do \
		$(APPLECMD) -d $(DSK) $$i; \
		$(APPLECMD) -d dos33.dsk $$i; \
		$(APPLECMD) -cc65 $(DSK) $$i bin < $$i; \
		$(APPLECMD) -cc65 dos33.dsk $$i bin < $$i; \
	done
	$(APPLECMD) -l $(DSK)
	$(APPLECMD) -l dos33.dsk

