#
# Makefile for Doug's Salota CPU Card 5/02/2011
#

#PREFIX   = /Developer/sdcc/bin/
CC       = $(PREFIX)sdcc 
COPTS    = -c -mz80 -D__SDCC__=1
AS       = $(PREFIX)sdasz80
AOPTS    = -plosff 
LD       = $(PREFIX)sdldz80
LOPTSX   = -u --verbose --code-loc 0x100
LOPTS	 =    
RM       = del
BIOS     = 0xFC00
QUIET    = @
COPY     = cp
DEL      = rm
DELFLAGS = -f
REN      = mv
RENFLAGS = 

# CPMRELS = cpm0.rel cpmbdos.rel getchar.rel outchar.rel cprintf.rel
CPMRELS = cpm0.rel cpmbdos.rel cprintf.rel

all:    pi.com

pi.com:	pi.rel $(CPMRELS) pi.arf load Makefile
	$(QUIET)$(COPY) pi.arf pi.lnk
	$(QUIET)$(COPY) pi.arf pi.lk
	$(LD) $(LOPTS) -nf pi.lnk
	cp pi.ihx pi.hex
	./load pi

cprintf.rel:	cprintf.c
	$(CC) $(COPTS) cprintf.c

load:	load.cpp
	g++ load.cpp -o load

pi.rel:	pi.c cpmbdos.h cprintf.h Makefile
	$(CC) $(COPTS) pi.c

cpm0.rel:	cpm0.s Makefile
	$(AS) $(AOPTS) cpm0.s 

getchar.rel:	getchar.s
	$(AS) $(AOPTS) getchar.s

outchar.rel:	outchar.s Makefile
	$(AS) $(AOPTS) outchar.s

cpmbdos.rel:	cpmbdos.s Makefile
	$(AS) $(AOPTS) cpmbdos.s

install:
	cp *.com ~/Documents/devobox/cdrive
clean:
	$(QUIET)$(DEL) $(DELFLAGS) *.hex
	$(QUIET)$(DEL) $(DELFLAGS) *.ihx
	$(QUIET)$(DEL) $(DELFLAGS) *.lst
	$(QUIET)$(DEL) $(DELFLAGS) *.rel
	$(QUIET)$(DEL) $(DELFLAGS) *.lst
	$(QUIET)$(DEL) $(DELFLAGS) *.rst
	$(QUIET)$(DEL) $(DELFLAGS) *.lnk
	$(QUIET)$(DEL) $(DELFLAGS) *.lk
	$(QUIET)$(DEL) $(DELFLAGS) *.sym
	$(QUIET)$(DEL) $(DELFLAGS) *.map
	$(QUIET)$(DEL) $(DELFLAGS) *.noi
	$(QUIET)$(DEL) $(DELFLAGS) *.asm
	$(QUIET)$(DEL) $(DELFLAGS) *.com
	$(QUIET)$(DEL) $(DELFLAGS) *.ini
	$(QUIET)$(DEL) $(DELFLAGS) load

