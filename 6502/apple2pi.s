;; Apple Pie
;;
;; Egan Ford (egan@sense.net) with a lot of optimizations and pointers from
;; Anton Treuenfels (atreuenfels@earthlink.net)
;;
;; Sept, 8 2012
;;
;; Apple Pie (Pi and e) will compute up to 10,000 decimal digits of Pi or e.
;; All Apple Pie computation is base 256 and then converted to base 10 for
;; display (technically base 100, but they both display the same).  Arrays
;; of bytes are used to represent multiprecision numbers.  In the comments
;; below "array" refers to a big endian multiprecision number.
;;
;; Aug, 8 2012
;;
;; Increased range to 22950
;;


;; replace Clobbered: with Registers:


;; Apple II variables/vectors

getln1	=	$FD6F		; get line of input and store in $200
cout	=	$FDED		; character out sub
prbyte	=	$FDDA		; print a hex byte
crout	=	$FD8E		; CR out sub
rdkey	=	$FD0C		; read key
warm	=	$FF69		; back to monitor
linebuf	=	$200		; input line buffer
promptc	=	$33		; location of getln1 prompt char


;; my variables/vectors

ptr	=	$0		; $0-$1 16 bit generic pointer
ptr_mp	=	$2		; $2-$3 16 bit generic pointer
a32	=	$4		; $4-$7 32 bit number
carry_mp=	$4		; $4-$4  8 bit multiprecision carry
				;          32/16 div
dividend=	$4		; $4-$7 32 bit
remainder=	$4		; $4-$5 16 bit
quotient=	$6		; $6-$7 16 bit
divisor	=	$8		; $8-$9 16 bit
				;          backup regs
yreg	=	$A		; $A-$A  8 bit
xreg	=	$B		; $B-$B  8 bit
heapptr	=	$C		; $C-$D 16 bit start of free memory

org	=	$800		; start here
heapend	=	$BFFF		; end of usable memory
max	=	22950		; max number of digits (FYI, 9860 = 4K)
				; 22950 is max because n = n + 2 in atan will get > 32767
				; and div16 bug when divisor 16th bit high.  Need to fix.
min	=	100		; min number of digits



;; start of global macros

.include	"pimacros.m"


; Description:	Get time from clock card (if present) and store in "ptr"
; Input:	16-bit ptr to array of 4 bytes (s,m,h,100th sec)
; Calls:	gettime

.macro	time	ptr
	ldx	gotclock
	beq	:+
	lday	#ptr
	jsr	gettime
:
.endmacro

;; end of macros


;; use for DOS header, NOTE: will mess with .align statements
	;.byte  <begin,>begin,<(end-begin),>(end-begin)


;; start of real code
	.org	org
begin:
	;; check for 80 col code here
	;; clear/home screen code
	; lazy, going to hard code for now, in the future check for //e with
	;   80 col card, or //c, IIgs, or Videx, else die
	lda	#$00		; load up $36/$37
	sta	$36		; with $C300
	lda	#$C3		; i.e. pr#3
	sta	$37		; or C300G

	ldx	#35		; print 35 spaces to center title
:	lda	#' '+$80
	jsr	cout
	dex
	bne	:-
	sprint	"Apple Pi/e",$0D
	jsr	crout		; print CR
	jsr	crout		; print CR

	; check for no slot clock
	jsr	detect_noslotclock
	bcc	:+		; if carry clear, no 'no slot clock'
	lda	#1
	sta	gotclock	; set clock type to 1
	sprint	"'No Slot Clock' detected",$0D
	beq	setup		; branch to setup
:
	; check for thunderclock card
	jsr	detect_thunderclock
	cpx	#0		; slot returned in x
	beq	:+		; no thunder clock if x = 0
	lda	#2
	sta	gotclock	; set clock type to 2
	stx	clockslot	; save the card slot number
	sprint	"Thunderclock detected in slot "
	txa			; transfer slot number to A
	ora	#$B0		; convert to ASCII
	jsr	cout		; print it
	jsr	crout		; CR
:

setup:				; initial setup
	jsr	crout		; CR
	lda	#':'+$80	; update prompt char just in case user hits <-
	sta	promptc		;   and store it

start:				; prompt for pi or e
	lda	#'P'
	sta	constant
	lda	#'i'
	sta	constant+1
	lda	#-1
	sta	pie	
	sprint	"Compute [P]i or [e]? "
:	jsr	rdkey		; read one keystroke
	ora	#%10100000	; convert to lower case + $80
	cmp	#'p'+$80	; got P?
	beq	:++		;   branch two :
	cmp	#'e'+$80	; got E?
	beq	:+		;   branch one :
	bne	:-		; none of the above, go back and try again
:	inc	pie		; e,   pie = 1
	lda	#'e'		; wipe out "Pi" and replace with "e",null
	sta	constant
	lda	#0
	sta	constant+1
:	inc	pie		; pi,  pie = 0
	sprint	constant	; display selection
	jsr	crout		; print CR
	jsr	crout		; print CR

printer:			; prompt for printer code
	sprint	"Print to slot (1-7), (0 or RETURN for no printout)? "
:	jsr	rdkey		; read one keystroke
	ora	#$80		; set hi bit (it's an apple thing)
	cmp	#$8D		; check for return
	beq	:+		; jump to fake a zero on display
	cmp	#$B8		; >= ASCII 8?
	bcs	:-		; yep, try again
	cmp	#$B0		; < ASCII 0?
	bcs	:++		; nope, move on
	bcc	:-		; yep, try again
:	lda	#$B0		; fake a zero
:	jsr	cout		; display slot
	and	#%00000111	; mask off last 3 bits
	sta	prslot		; save it for later
	jsr	crout		; print CR

	;; prompt for 6502 or 6809 or both
	; eventually I'll get the 6809 version of this for Stellation 6809
	; co-processor cards

inputrange:
	jsr	crout		; print CR
	sprint	"Number of "
	sprint	constant	; Pi or e
	sprint	" digits (100-22950): "
	jsr	getln1		; get line of input, length in x
	cpx	#6		; if x >= 6 (should be int(log10(max)) + 1)
	bcs	rangeerror	;   error out and branch to rangeerror
	lda	#0		; put a null at the end of the string
	sta	linebuf,x	;   for asciitohex (plus display/printing)
	jsr	asciitohex	; returns 16-bit hex lo in a and hi in y
	bcc	noinputerror	;   if no carry set, then no asciitohex error
rangeerror:
	jsr	crout		; print CR
	sprint	"Digits must be in range 100-22950.",$0D
	beq	inputrange	; free zero from sprint
noinputerror:			; check for range error
				; last operation from asciitohex was ldy (hi)
	bne	highcheck	; check for > 22950
	cmp	#min		; if < 100
	bcc	rangeerror
highcheck:
	cpy	#>max
	bcc	rangeok		; < 22950
	bne	rangeerror	; > 22950
	cmp	#(<max)+1	; got hi bits, need to check for lo bits in a
	bcs	rangeerror	; >= lo bits
rangeok:
	sta	declen		; save declen lo (declen used for print_mp)
	sta	arraylen	; save arraylen lo
	sty	declen+1	; save declen hi
	sty	arraylen+1	; save arraylen hi
	; need to add one to range for the "3." that is left of
	; the decimal digits, i.e. 1000 digits is really 1001
	inc	arraylen	; arraylen++ (lo = lo + 1)
	bne	:+		; if rolled to 0 then
	inc	arraylen+1	; inc arraylen hi
:	; at this point we have a valid number between 100-22950 inclusive
	;
	; compute ceil(number decimal digits * 1/log(256) + 1) to compute
	; the number of base(256) digits to use
	;
	;   1st, multiply arraylen by 1/log(256) (0.6A4D)
	;   2nd, ceil (round up) the result + 1 guard digit
	;   3rd, add extra guard digits for accuracy

	; 1. arraylen/log(256)
	;
log256:	; fixed point 32-bit math product/result
	lda	#0
	sta	a32		; right of the decimal (< 0)
	sta	a32+1
	sta	a32+2		; left of the decimal (> 0)
	sta	a32+3
	ldx	#16
	bne	:++		; skip first shift
:	asl	a32		; shift ...
	rol	a32+1
	rol	a32+2
	rol	a32+3
:	asl	arraylen	; get high bit
	rol	arraylen+1
	bcc	:+		; zero bit, next
	clc			; ... and add
	lda	#$4D		; 1/log(256)
	adc	a32
	sta	a32
	lda	#$6A		; 1/log(256)
	adc	a32+1
	sta	a32+1
	bcc	:+		; no carry, no prob
	inc	a32+2		; got carry, inc next MSD
	bne	:+		; inc does not set carry, but if rolled to
	inc	a32+3		;   zero, then it's a carry, inc MSD
:	dex			; not zero
	bne	:---		;   then back to top of loop

	; 2. compute ceil and add a guard digit
	; x = 0 now (free 0!)
	;
ceil:	ldy	#2		; assumed ceil + one guard digit

	cpx	a32		; right of decimal not 0?  round up (ceil)
	bne	guards		;   truncate and keep Y = 2
	cpx	a32+1		; right of decimal not 0?  round up (ceil)
	bne	guards		;   truncate and keep Y = 2
	dey			; remove assumed ceil (prob never happend)

	; 3. if declen > 1000 ($3E8), then A = A + 2 more guard digits.
	;
guards: lda	declen		; 16-bit compare
	cmp	#<1001		; if # of decimal digits > 1000
	lda	declen+1	;   then add two more guard digits, this will
	sbc	#>1001		;   be sufficient for up to 22950 digits
	bcc	:+		; branch if declen < 1001
	iny			;   otherwise add two more guard digits
	iny			;   then transfer to A
:	tya			; A = ceil + guards
	clc			; add guard+ceil digits to /log256
	adc	a32+2		;   and save in arraylen
	sta	arraylen
	lda	#0
	adc	a32+3
	sta	arraylen+1

	; almost done
	;
	; make arraylen even to optimize Duff Device branch decisions
	;   this will make the code a bit shorter, but also a bit slower
	;   on the order of 1/1000ths of seconds or less total time 50% of the
	;   time, IOW not going to worry about it.
	;
	lda	arraylen	; load LSB arraylen
	lsr			; shift LSBit to carry
	bcc	:+		; is even, then skip
	inc	arraylen	; is odd, then plus one to make even
	bne	:+		; arraylen rolled to zero?
	inc	arraylen+1	; got zero, inc arraylen+1
:
	; compute arrayend, used by loops that start from end
	;
	lda	arraylen+1	; arrayend = arraylen - 1
	sta	arrayend+1
	lda	arraylen
	sta	arrayend
	bne	:+		; check if arraylen (lo) is zero, if zero, 
	dec	arrayend+1	;   then it will rollback to $FF, so dec (hi)
:	dec	arrayend	; dec (lo)

	; finally done with entry and setup, time for some Pie!!!

initheap:			; setup heap pointer
	lday	#heap		; This is the start of dynamically allocated
	stay	heapptr		; memory.  This needs to be done each time Pi
				; or e is computed to free up old arrays.
	
alloc_pointers:			; allocate all arrays (we need 4 for Pi)
	alloc	mp_a,arraylen	; allocate mp_a with size arraylen
	alloc	mp_b,arraylen	;   error on failure and die
	alloc	mp_x,arraylen
	alloc	mp_y,arraylen

				; if printout, enable printer
	lda	prslot		; load prslot value (0-7)
	beq	:+		; if zero no print
	ora	#$C0		; setup redir by storing
	sta	$37		; $C<slot>00 in $36/$37
	lda	#$00
	sta	$36
:
header:				; print header
	jsr	crout		; print CR
	sprint	"Computing "
	sprint	constant
	sprint	" to "
	sprint	linebuf
	sprint	" digits..."

	time	starttime	; if clock detected, store start time
cnton	1
	lda	pie		; check pie flag for pi = 0. e = 1
	beq	:+		; if zero branch to jsr pi
	jsr	e		;   else jsr e
	bcs	:++		; carry set after e run
:	jsr	pi
:	jsr	crout
	jsr	crout
	mprint	mp_a		; print it (note mp_a hosed)
	jsr	crout		; print CR
cntoff	1
	time	endtime		; if clock detected, start end time
	jsr	printtime	;   and print the wallclock time
	jsr	crout
				; if printout, disable printer
	lda	prslot		; load prslot value (0-7)
	beq	:+		; if zero no print
	lda	#$00		; hard coded load of $C300 in $36/$37
	sta	$36		;  i.e. pr#3
	lda	#$C3
	sta	$37
:
	sprint	"More Pie? "	; who doesn't want more Pie?
:	jsr	rdkey		; read a single keystroke
	ora	#%10100000	; convert to lower case + $80
	cmp	#'n'+$80	; check for N or 
	beq	:++
	cmp	#'y'+$80	; Y
	beq	:+
	bne	:-		; if not N/Y retry.
:	sprint	"Yes"		; print "Yes" if y
	jsr	crout
	jsr	crout
	jmp	start		; start over with all prompts
:	sprint	"No"		; print "No" if n
	jsr	crout		; print CR
	rts			;  and exit


;; end of main code, start of non-zp allocations

prslot:	.byte	$0		; Printer slot
pie:	.byte	$0		; Pi or e flag, pi = 0, e = 1
constant:			; Pi or e text
	.byte	$0, $0, $0
gotclock:
	.byte	$0		; clock type
clockslot:
	.byte	$0		; clock slot
starttime:
	.byte	$0, $0, $0, $0	; start time (s,m,h,1/100 s)
endtime:
	.byte	$0, $0, $0, $0	; end time (s,m,h,1/100 s)
arraylen:
	.byte	$0, $0		; length of base 256 array
arrayend:
	.byte	$0, $0		; last byte of array
declen:	.byte	$0, $0		; decimal length of digits
mp_a:	.byte	$0, $0		; mp pointer
mp_b:	.byte	$0, $0		; mp pointer
mp_x:	.byte	$0, $0		; mp pointer
mp_y:	.byte	$0, $0		; mp pointer


;; start of subs

; Description: Compute pi using the Gregory expansion of Machin's arctan
;              formula and save in array (mp_a) (hardcoded--bad).
;
;   pi = 4 * (4 *      atan(1/5)           -           atan(1/239)          )
;
;
;   __      /   / 1     1      1       \       / 1       1        1        \\
;   || = 4 | 4 |  - - ---- + ---- - ... |  -  | --- - ------ + ------ - ... ||
;          |   |  5      3      5       |     | 239        3        5       ||
;           \   \     3x5    5x5       /       \      3x239    5x239       //
;
;
; Input:       None
;
; Output:      (mp_a) = pi = 4 * (4 * atan(1/5) - atan(1/239))
;
; Clobbered:   A, X, Y
;
; Calls:       matan/atan_mp, masl/asl_mp, msub/sub_mp

pi:	matan	mp_a,#5		; a = atan(1/5)
	masl	mp_a		; a = a * 4
	masl	mp_a
	matan	mp_b,#239	; b = atan(1/239)
	msub	mp_a,mp_b	; a = a - b
	masl	mp_a		; a = a * 4
	masl	mp_a
	rts


; Description: Compute e using the infinite series:
;
;               oo
;              =====
;              \      1
;      e   =    >    --  = 1 + 1/1! + 1/2! + 1/3! + 1/4! + ...
;              /     k!
;              =====
;              k = 0
;
;              and save in array (mp_a) (hardcoded--bad).
;
; Input:       None
;
; Output:      (mp_a) = e = 1 + 1/1! + 1/2! + 1/3! + 1/4! + ...
;
; Clobbered:   A, X, Y
;
; Calls:       mset/set_mp, mdiv/div_mp, mdiv16/div16_mp, madd/add
;
; Globals:     Uses n (16-bit) from atan_mp, mp_a, mp_x
;
; C Algorithm:
;
; int n = 1;
; setbig(A, 1, 0);
; setbig(X, 1, 0);
; while(divbig(X, n++))         // while(dividend != 0)
;     addbig(A, X);

e:	lda	#0		; n = 0
	sta	n
	sta	n+1
	mset	mp_a,#1		; mp_a = 1
	mset	mp_x,#1		; mp_x = 1
e_mp_loop:
	inc	n		; n = n + 1
	bne	:+		; n not zero
	inc	n+1		; n rolled to 0, inc n (hi)
:	ldx	n+1		; if n < 256 then div else div16
	bne	:+		; >= 256 use 16 bit div
	mdiv	mp_x,n		; < 256 use 8 bit div
	bcs	e_mp_end	; dividend = 0, done
	bcc	:++		; still working on it
:	mdiv16	mp_x,n		; div16
	bcs	e_mp_end	; dividend = 0, done
:	madd	mp_a,mp_x	; a = a + x
	jmp	e_mp_loop
e_mp_end:
	rts


; Description: Compute arctan(1/N) using the Gregory expansion of Machin's
;              arctan formula and save in array (ptr_a).
;
;
;                              / 1     1      1       \
;              arctan(1/N) =  |  - - ---- + ---- - ... |
;                             |  N      3      5       |
;                              \     3xN    5xN       /
;
;
; Input:       Y/A (hi/lo) pointer to array
;              X = N (8 bit)
;
; Output:      Y/A (hi/lo) pointer to array = arctan(1/N)
;
; Clobbered:   A, X, Y
;
; Calls:       mset/set_mp, mdiv/div_mp, madd/add_mp, msub/sub_mp,
;              mdiv16/div16_mp, mcopy/copy_mp
;
; Globals:     mp_x (16-bit), mp_y (16-bit)
;
; C Algorithm:
;
; void atanbig(bignum A, unsigned short x)
; {
;     bignum X, Y;
;     unsigned short n = 1;
; 
;     setbig(X, 1, 0);
;     divbig(X, x);
;     copybig(A, X);
;     x *= x;
;     while (1) {
;         n += 2;
;         divbig(X, x);
;         copybig(Y, X);
;         if (!divbig(Y, n))      // dividend = 0
;             break;
;         if (n & 2)
;             subbig(A, Y);
;         else
;             addbig(A, Y);
;     }
; }
;
; Locals (not really):

regx:	.byte	$0
x2:	.byte	$0		; x^2 if x < 16
n:	.byte	$0, $0
ptr_a:	.byte	$0, $0
squares:
	.byte	0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
	.byte	121, 144, 169, 196, 225

atan_mp:
	sta	ptr_a		; store ptr lo from A
	sty	ptr_a+1		; store ptr hi from Y
	stx	regx		; save x

	lda	#0		; n = 1
	sta	n
	sta	n+1
	inc	n		; n = 1 little endian

	mset	mp_x,#1		; mp_x = 1
	mdiv	mp_x,regx	; mp_x /= regx
	mcopy	ptr_a,mp_x	; ptr_a = mp_x

	ldx	regx		; square x if x < 16
	stx	x2
	cpx	#16
	bcs	atan_mp_loop
	lda	squares,x	; load from squares table
	sta	x2		;   and save

atan_mp_loop:			; main loop
				; n = n + 2
	inc	n		; n now even
	bne	:+		; n not zero
	inc	n+1		; n rolled to 0, inc n (hi)
:	inc	n		; n to be odd, save to inc (cannot roll from
				;   even to odd)
	ldx	regx		; x = x / regx ^ 2
	cpx	#16
	bcc	:+		; x already x*x, one div required
	mdiv	mp_x,x2		; x >= 16, then do two div
:	mdiv	mp_x,x2		;   (faster than 16 bit div)

	mcopy	mp_y,mp_x	; y = x

	ldx	n+1		; if n < 256 then div else div16
	bne	:+		; >= 256 use 16 bit div
	mdiv	mp_y,n		; < 256 use 8 bit div
	bcs	atan_mp_end	; dividend = 0, done
	bcc	:++		; still working on it
:	mdiv16	mp_y,n		; div16
	bcs	atan_mp_end	; dividend = 0, done

:	lda	n		; add or sub
	and	#2
	beq	:+		; add it
	msub	ptr_a,mp_y	; a = a - y 
	jmp	atan_mp_loop
:	madd	ptr_a,mp_y	; a = a + y 
	jmp	atan_mp_loop
atan_mp_end:
	rts


; Description: Multiprecision add:  a = a + b
;
; Input:       Y/A (hi/lo) pointer to array (a)
;              ptr_mp set by macro to point to array (b)
;
; Output:      a = a + b
;
; Clobbered:   A, X, Y
;
; Globals:     arraylen (16-bit), arrayend (16-bit), zp ptr (16-bit),
;              zp ptr_mp (16-bit)

add_mp:
	sta	ptr		; store ptr lo from A
	tya
	clc
	adc	arrayend+1	; add number of pages since we have to go
	sta	ptr+1		;   backwards for add/sub/asl
	lda	ptr_mp+1
	clc
	adc	arrayend+1	; add number of pages since we have to go
	sta	ptr_mp+1	;   backwards for add/sub/asl
	ldx	arrayend+1	; full pages
	ldy	arrayend	; partial
	clc			; clear carry for many adds
	tya
	and	#%10		; Duff's Device.   We know that arraylen is
	beq	@1		;   even (arrayend odd), so just need to
	bne	@3		;   check for 2nd bit (odd or even # of pairs).
:	dex
	dec	ptr+1		; previous page of 256
	dec	ptr_mp+1	; previous page of 256
:	dey
@3:	lda	(ptr),y		; a	; do bytes 3,4,...,255
	adc	(ptr_mp),y	; + b
	sta	(ptr),y		; a = a + b
	dey
@2:	lda	(ptr),y		; a	; do bytes 2,4,...,254
	adc	(ptr_mp),y	; + b
	sta	(ptr),y		; a = a + b
	dey
@1:	lda	(ptr),y		; a	; do bytes 1,5,...,253
	adc	(ptr_mp),y	; + b
	sta	(ptr),y		; a = a + b
	dey
@0:	lda	(ptr),y		; a	; do bytes 0,4,...,252
	adc	(ptr_mp),y	; + b
	sta	(ptr),y		; a = a + b
	tya
	bne	:-
	txa
	bne	:--
	rts


; Description: Multiprecision subtract:  a = a - b
;
; Input:       Y/A (hi/lo) pointer to array (a)
;              ptr_mp set by macro to point to array (b)
;
; Output:      a = a - b
;
; Clobbered:   A, X, Y
;
; Globals:     arraylen (16-bit), arrayend (16-bit), zp ptr (16-bit),
;              zp ptr_mp (16-bit)

sub_mp:
	sta	ptr		; store ptr lo from A
	tya
	clc
	adc	arrayend+1	; add number of pages since we have to go
	sta	ptr+1		;   backwards for add/sub/asl
	lda	ptr_mp+1
	clc
	adc	arrayend+1	; add number of pages since we have to go
	sta	ptr_mp+1	;   backwards for add/sub/asl
	ldx	arrayend+1	; full pages
	ldy	arrayend	; partial
	sec			; sec carry for many subs
	tya
	and	#%10		; Duff's Device.   We know that arraylen is
	beq	@1		;   even (arrayend odd), so just need to
	bne	@3		;   check for 2nd bit (odd or even # of pairs).
:	dex
	dec	ptr+1		; previous page of 256
	dec	ptr_mp+1	; previous page of 256
:	dey
@3:	lda	(ptr),y		; a	; do bytes 3,7,...,255
	sbc	(ptr_mp),y	; - b
	sta	(ptr),y		; a = a - b
	dey
@2:	lda	(ptr),y		; a	; do bytes 2,6,...,254
	sbc	(ptr_mp),y	; - b
	sta	(ptr),y		; a = a - b
	dey
@1:	lda	(ptr),y		; a	; do bytes 1,5,...,253
	sbc	(ptr_mp),y	; - b
	sta	(ptr),y		; a = a - b
	dey
@0:	lda	(ptr),y		; a	; do bytes 0,4,...,252
	sbc	(ptr_mp),y	; - b
	sta	(ptr),y		; a = a - b
	tya
	bne	:-
	txa
	bne	:--
	rts


; Description: Multiprecision shift left:  a = a * 2
;
; Input:       Y/A (hi/lo) pointer to array (a)
;
; Output:      a = a * 2
;
; Clobbered:   A, X, Y
;
; Globals:     arraylen (16-bit), arrayend (16-bit), zp ptr (16-bit),

asl_mp:
	sta	ptr		; store ptr lo from A
	tya
	clc
	adc	arrayend+1	; add number of pages since we have to go
	sta	ptr+1		;   backwards for add/sub/asl
	ldx	arrayend+1	; full pages
	ldy	arrayend	; partial
	clc			; clear carry for many rols
	tya
	and	#%10		; Duff's Device.   We know that arraylen is
	beq	@1		;   even (arrayend odd), so just need to
	bne	@3		;   check for 2nd bit (odd or even # of pairs).
:	dex
	dec	ptr+1		; previous page of 256
:	dey
@3:	lda	(ptr),y		; do bytes 3,7,...,255
	rol
	sta	(ptr),y
	dey
@2:	lda	(ptr),y		; do bytes 2,6,...,254
	rol
	sta	(ptr),y
	dey
@1:	lda	(ptr),y		; do bytes 1,5,...,253
	rol
	sta	(ptr),y
	dey
@0:	lda	(ptr),y		; do bytes 0,4,...,252
	rol
	sta	(ptr),y
	tya
	bne	:-
	txa
	bne	:--
	rts


; Description: Multiprecision transfer:  a = b
;
; Input:       Y/A (hi/lo) pointer to array (b)
;              ptr_mp set by macro to point to array (a)
;
; Output:      a = b
;
; Clobbered:   A, X, Y
;
; Globals:     arraylen (16-bit), zp ptr (16-bit), zp ptr_mp (16-bit)

copy_mp:
	sta	ptr		; store ptr lo from A
	sty	ptr+1		; store ptr hi from Y
	ldx	arraylen+1	; full pages
	ldy	arraylen	; partial pages
	beq	@0		;   if not process full pages only
	inx			; bump up x +1 for the partial page
	clc			; adjust to point below source start
	tya			; - ex: ptr = $xx00, arraylen = 1
	adc	ptr		; we want ptr = $(xx-1)01, y = $ff (255)
	sta	ptr
	bcs	:+
	dec	ptr+1
:	clc			; adjust to point below destination start
	tya			; - ex: ptr_mp = $xxff, arraylen = 2
	adc	ptr_mp		; we want ptr_mp = $xx01, y = $fe (254)
	sta	ptr_mp
	bcs	:+
	dec	ptr_mp+1
:	tya			; adjust y index value, y = -y
	eor	#$ff		; one's complement
	tay
	iny			; two's complement
	and	#%10		; Duff's Device.   We know that arraylen is
	beq	@2		;   even, so just need to check for 2nd bit
				;   (odd or even # of pairs).
				;   Fall through to @0.
				; NOTE: A = Y - 1, checking A, not Y
				; if A & %11 = 1, then Y%3 = 2
				; if A & %11 = 3, then Y%3 = 0
@0:	lda	(ptr),y		; move bytes 0,4,...,252
	sta	(ptr_mp),y
	iny
@1:	lda	(ptr),y		; move bytes 1,5,...,253
	sta	(ptr_mp),y
	iny
@2:	lda	(ptr),y		; move bytes 2,6,...,254
	sta	(ptr_mp),y
	iny
@3:	lda	(ptr),y		; move bytes 3,7,...,255
	sta	(ptr_mp),y
	iny			; page done ?
	bne	@0		; b: no
	inc	ptr+1		; next page
	inc	ptr_mp+1
	dex			; full page left ?
	bne	@0		; b:yes
	rts 


; Description: Skip leading zeros (used by div_mp and div16_mp)
;
; Input:       Y/A (hi/lo) pointer to array (b)
;
; Output:      None
;
; Clobbered:   A, X, Y
;
; Globals:     arraylen (16-bit), zp ptr (16-bit)

skipzeros:
	sta	ptr		; store ptr lo from A
	sty	ptr+1		; store ptr hi from Y
	ldx	arraylen+1	; full pages
	ldy	arraylen	; partial pages ?
	beq	@0		; b:no - process full pages only
	inx			; bump up x +1 for the partial page
	clc			; adjust to point below source start
	tya			; - ex: ptr = $xx00, arraylen = 1
	adc	ptr		; we want ptr = $(xx-1)01, y = $ff (255)
	sta	ptr
	bcs	:+
	dec	ptr+1
:	tya			; adjust y index value, y = -y
	eor	#$ff		; one's complement
	tay
	iny			; two's complement
	and	#%10		; Duff's Device.   We know that arraylen is
	beq	@2		;   even, so just need to check for 2nd bit
				;   (odd or even # of pairs).
				;   Fall through to @0.
				; NOTE: A = Y - 1, checking A, not Y
				; if A & %11 = 1, then Y%3 = 2
				; if A & %11 = 3, then Y%3 = 0
@0:	lda	(ptr),y		; check	0,4,...,252
	bne	@5
	iny
@1:	lda	(ptr),y		; check 1,5,...,253
	bne	@4
	iny
@2:	lda	(ptr),y		; check 2,6,...,254
	bne	@5
	iny
@3:	lda	(ptr),y		; check 3,7,...,255
	bne	@4
	iny
	bne	@0
	inc	ptr+1		; next page
	dex			; is there another page ?
	bne	@0		; b:yes
	sec			; flag: all cells are zero
	rts
@4:	dey			; make Y even
@5:	clc			; flag: (ptr),y -> first non-zero cell
	rts


; Description: 16-bit/8-bit divide based on:
;              http://6502org.wikidot.com/software-math-intdiv		
;
; Input:       divisor (8-bit), dividend (16-bit) hi: X, lo: dividend
;              
; Output:      divisor (8-bit) unchanged,
;              X: remainder (8-bit),
;              dividend (8-bit): quotient
;
; Clobbered:   A, X

.macro	div8			
cnton 60

	txa			; remainder
	asl	dividend
	.repeat 8		; unroll 8 times for speed
	rol
	bcs	:+
	cmp	divisor
	bcc	:++
:	sbc	divisor
	sec
:	rol	dividend
	.endrepeat
	tax			; remainder

cntoff 60
.endmacro


; Description: Divide mp array by 8-bit number.
;
; Input:       Y(hi)/A(lo) pointer to mp
;              X = dividend/denominator
;
; Output:      (ptr) = (ptr)/x
;
; Clobbered:   A, X, Y
;
; Globals:     zp ptr (16-bit), arraylen (16-bit), dividend (8-bit),
;              zp xreg (8-bit), 
;
; Calls:       skipzeros, div8(macro)
;
; C Algorithm:
;
; short divbig(number, x)
; bignum number;
; unsigned short x;
; {
;     dword result;
;     short j = 0;
;     unsigned short rest = 0;
; 
;     while (number[j] == 0 && j < MAXSIZE)
;         j++;
;     if (j == MAXSIZE)
;         return (0);
;     while (j < MAXSIZE) {
;         result.w.lo = number[j];
;         result.w.hi = rest;
;         number[j] = result.L / x;
;         rest = result.L % x;
;         j++;
;     }
;     return (1);
; }

div_mp:
	stx	divisor		; save divisor
	jsr	skipzeros	; skip leading zeros for speed
	bcc	:+		; carry clear?  continue
	rts			;   else all zeros, return to caller
:	stx	xreg		; need x reg for speed (carry)
	ldx	#0
	tya
	and	#%11
	beq	@0		; 00 & 11 = 0
	lsr			; 00 => 0,0; 01 => 0,1; 10 => 1,0; 11 => 1,1
	beq	@1		; 01 => 0,1
	;bcc	@2		; 1,0
	bcc	:+		; long branch
	;bcs	@3		; 1,1
	jmp	@3		; long branch
:	jmp	@2
@0:	lda	(ptr),y		; div byte 0,4,...,252
	sta	dividend
	div8
	lda	dividend
	sta	(ptr),y
	iny
@1:	lda	(ptr),y		; div byte 1,5,...,253
	sta	dividend
	div8
	lda	dividend
	sta	(ptr),y
	iny
@2:	lda	(ptr),y		; div byte 2,6,...,254
	sta	dividend
	div8
	lda	dividend
	sta	(ptr),y
	iny
@3:	lda	(ptr),y		; div byte 3,7,...,255
	sta	dividend
	div8
	lda	dividend
	sta	(ptr),y
	iny			; page done ?
	beq	:+
	jmp	@0
:	dec	xreg		; full page left ?
	beq	:+
	inc	ptr+1		; next page
	jmp	@0
:	clc			; non zero result
	rts


; Description: 32-bit/16-bit divide based on:
;              Apple II firmware (Steve Wozniak) and
;              http://www.txbobsc.com/aal/1983/aal8303.html#a5, and
;              optimized by Anton Treuenfels.
;
; Input:       divisor (16-bit big endian), dividend (32-bit big endian)
;              
; Output:      divisor (16-bit big endian) unchanged,
;              dividend (32-bit big endian) hi: remainder
;              dividend (32-bit big endian) lo: quotient
;
; Clobbered:   A, X, Y

.macro	div16
	ldx	dividend+1	; remainder
	ldy	dividend+0	; remainder
	.repeat 16		; unroll 16 times for speed
	asl	dividend+3	; 5
	rol	dividend+2	; 10
	txa			; 12
	rol			; 14
	tax			; 16
	tya			; 18
	rol			; 20
	tay			; 22
	cpx	divisor+1	; 25
	sbc	divisor+0	; 28
	bcc	:+		; 30/31 (no subtraction)
	tay			; 32
	txa			; 34
	sbc	divisor+1	; 37
	tax			; 39
	inc	dividend+3	; 44 (subtraction)
:
	.endrepeat
	stx	dividend+1	; remainder
	sty	dividend+0	; remainder
.endmacro


; Description: Divide mp array by 16-bit number.
;
; Input:       Y(hi)/A(lo) pointer to mp
;              divisor (16-bit) set by macro
;
; Output:      (ptr) = (ptr)/divisor
;
; Clobbered:   A, X, Y
;
; Globals:     zp ptr (16-bit), arraylen (16-bit), dividend (32-bit),
;              zp xreg (8-bit), divisor (16-bit)
;
; Calls:       skipzeros, div16(macro), div16_long(macro)
;
; C Algorithm: see div_mp

.macro	div16_long
	lda	(ptr),y		; load/store LSB of dividend
	sta	dividend+2
	sty	yreg		; save y for updating array
	iny
	lda	(ptr),y		; load/store LSB-1 of dividend
	sta	dividend+3
	div16			; need x and y reg
	ldy	yreg		; restore y
	lda	quotient+0	; load/store quotient MSB to array
	sta	(ptr),y
	iny
	lda	quotient+1	; load/store quotient LSB to array
	sta	(ptr),y
	iny
.endmacro

div16_mp:
	jsr	skipzeros	; skip leading zeros for speed
	bcc	:+		; carry clear?  continue
	rts			;   else all zeros, return to caller
:	lda	#0		; clear remainder/carry
	sta	dividend+0	; MSB (big endian)
	sta	dividend+1	; MSB-1
	stx	xreg		; macro div16 needs x reg
	tya
	lsr			; even number of non-zero
	and	#%11		; however there can be an odd number of pairs
	beq	@0		; 00 & 11 = 0
	lsr			; 00 => 0,0; 01 => 0,1; 10 => 1,0; 11 => 1,1
	;beq	@1		; 01 => 0,1
	bne	:+
	jmp	@1
:	;bcc	@2		; 1,0
	bcc	:+		; long branch
	;bcs	@3		; 1,1
	jmp	@3		; long branch
:	jmp	@2
@0:	div16_long
@1:	div16_long
@2:	div16_long
@3:	div16_long
	beq	:+		; done?
	jmp	@0
:	dec	xreg
	beq	:+
	inc	ptr+1		; next page
	jmp	@0
:	clc			; non zero result
	rts


; Description: print mp base 10/100
;
; Input:       Y(hi)/A(lo) pointer to array
;
; Output:      mp base 10/100 out to screen
;
; Clobbered:   A, X, Y, input array
;
; Globals:     zero page: ptr (16-bit) , ptr_mp (16-bit), carry_mp (8-bit),
;              arraylen (16-bit)
;
; Tables:      512 byte mult100_lo/hi: and 100 byte bintobcd:
;
; Locals:

dlength:
	.byte	$0, $0
ptr_save:
	.byte	$0
five:	.byte	$5		; counts the # of base 100 digits printed
seven:	.byte	$7		; counts the # of sets of 10 digits printed

print_mp:
	sta	ptr		; store ptr lo from A
	sty	ptr+1		; store ptr hi from Y
	tya
	clc
	adc	arrayend+1	; add number of pages since we have to go
	sta	ptr_save	;   backwards for multiply

	lda	declen+1	; converting to base 100, so dlength=dlength/2
	lsr
	sta	dlength+1
	lda	declen
	ror
	sta	dlength

	lda	#5		; reset five and seven
	sta	five		; used for pretty output
	lda	#7
	sta	seven
				; print left of decimal
	ldy	#0		; get first digit in array (big endian)
	lda	(ptr),y
	tax			; now convert from hex to dec
	lda	bintobcd,x	; table lookup
	cmp	#10		; if less than 10 skip leading zero
	bcc	:+
	jsr	prbyte		; print it, only A gets hosed, safe to call
	jmp	period
:	clc			; no leading zero code
	adc	#$B0		;   convert single digit to ascii
	jsr	cout		;   print it
period:	lda	#'.'+$80	; print decimal point (fixed point math)
	jsr	cout		;   print it

print_mp1:			; loop through rest of decimal digits
	ldy	#0		; array[0] = 0
	lda	#0
	sta	(ptr),y
	sta	carry_mp	; clear multiprecision carry

	lda	ptr		; ptr_mp to get hosed, must be restored
	sta	ptr_mp
	lda	ptr_save	; add number of pages since we have to go
	sta	ptr_mp+1	;   backwards for multiply (right -> left)

	; multi array x 100
	; loop from LSB to MSB
	; 16-bit product = array[i] * 100 + carry_mp;
	; array[i] = product lo
	; carry_mp = product hi
	;
	; C Algorithm:
	;
	;    while (j >= 0) {
        ;	result.L = (long) number[j] * 100 + carry;
        ;	number[j] = result.w.lo;
        ;	carry = result.w.hi;
        ;	j--;
    	;    }

	.macro	mult100
	lda	(ptr_mp),y	; get it
	tax
	lda	mult100_lo,x	; get product lo
	clc
	adc	carry_mp	; add carry_mp to it
	sta	(ptr_mp),y	;   and save it
	lda	mult100_hi,x	; get product hi
	adc	#0		;   add carry if present
	sta	carry_mp	;   and save it
	.endmacro

	ldx	arrayend+1	; full pages
	stx	xreg
	ldy	arrayend	; partial
	tya
	and	#%10		; Duff's Device.   We know that arraylen is
	beq	@1		;   even (arrayend odd), so just need to
	bne	@3		;   check for 2nd bit (odd or even # of pairs).
:	dec	xreg
	dec	ptr_mp+1	; previous page of 256
:	dey
@3:	mult100
	dey
@2:	mult100
	dey
@1:	mult100
	dey
@0:	mult100
	tya
	bne	:-
	ldx	xreg
	bne	:--

	; print array[0] (MSB)
	ldy	#0		; get first digit in array
	lda	(ptr),y
	tax			; now convert from hex to dec
	lda	bintobcd,x
	jsr	prbyte		; print it, only A gets hosed, safe to call

	dec	five		; five=five-1 for each base100 digit printed
	bne	:+		; if five base100 digits printed
	lda	#5		;   then reset five count
	sta	five		;   and print a space
	lda	#' '+$80
	jsr	cout	
	dec	seven		; seven=seven-1 for each set of 10 digits prted
	bne	:+		; if 7 sets of 10 printed
	lda	#7		;   then reset seven count
	sta	seven		;   and print a CR
	jsr	crout		;   then print 2 spaces
	lda	#' '+$80
	jsr	cout	
	jsr	cout	
:
				; dlength = dlength - 1
	lda	dlength		; check if dlength = 0
	bne	:+		;   if so, it will roll to $FF
	dec	dlength+1	;     so dec dlength+1
:	dec	dlength		; dec dlength
	bne	:+		; check dlength and dlength+1 for zeros
	lda	dlength+1	; 
	bne	:+
	rts			; dlength = 0, all done
:	jmp	print_mp1	; 


; Description: set array initial value
;
; Input:       Y(hi)/A(lo) pointer to mp
;              X = value left of decimal
;
; Output:      (ptr),0 = X
;              (ptr),1 - (ptr),length-1 = 0
;
; Clobbered:   A, X, Y
;
; Globals:     arraylen (16-bit), zp length (16-bit), zp ptr (16-bit)

set_mp:
	sta	ptr		; store ptr lo from A
	sty	ptr+1		; store ptr hi from Y
	ldy	#0		; left of decimal
	txa
	sta	(ptr),y		; store initial value
	inc	ptr		; bump up pointer
	bne	:+		;  to next digit
	inc	ptr+1		;  rest of array will be 0
: 	ldx	arrayend+1	; full pages (hack, arrayend = arraylen - 1)
	ldy	arrayend	; partial pages
	beq	@0		;   if not process full pages only
	inx			; bump up x +1 for the partial page
	clc			; adjust to point below source start
	tya			; - ex: ptr = $xx00, arraylen = 1
	adc	ptr		; we want ptr = $(xx-1)01, y = $ff (255)
	sta	ptr
	bcs	:+
	dec	ptr+1
:	tya			; adjust y index value, y = -y
	eor	#$ff		; one's complement
	tay
	iny			; two's complement
	lsr			; lsr lsr will put 2nd bit in carry status
	lsr			; remember checking A here not Y
	lda	#0		; A must be set to #0 to clear array 
				; use carry to check for one/even pairs
				; Duff's Device.   We know that arrayend is
	bcc	@1		;   odd, so just need to check for 2nd bit
	bcs	@3		;   (odd or even # of pairs).
				; NOTE: A = Y - 1, checking A, not Y
				; if A & %10 = 0, then Y%3 = 1
				; if A & %10 = 2, then Y%3 = 3
@0:	sta	(ptr),y		; move even byte (0, 2, ..., 254)
	iny
@1:	sta	(ptr),y		; move odd byte (1, 3, ..., 255)
	iny			; page done ?
@2:	sta	(ptr),y		; move even byte (0, 2, ..., 254)
	iny
@3:	sta	(ptr),y		; move odd byte (1, 3, ..., 255)
	iny			; page done ?
	bne	@0		; b: no
	inc	ptr+1		; next page
	dex			; full page left ?
	bne	@0
	rts 


; Description: Memory Allocation.  If you call malloc_pb it will move
;              heapptr up to start of next page and if allocation fails
;              it will not restore (yeah, it's a bug).  malloc will
;              allocate from the start of heapptr.  init_heap must be
;              called ONCE before any malloc calls.
;
; Input:       Y(hi)/A(lo) length of array
;
; Output:      Y(hi)/A(lo) pointer to array
;              CARRY=0: OK
;              CARRY=1: ERROR
;
; Clobbered:   A, X, Y, heapptr
;
; Allocation:  dynamic, returned by pointer
;
malloc_pb:
	ldx	heapptr		; check to see if already on page start
	beq	malloc		; on page? (lo = 0) move ahead with malloc
	ldx	#0		; not on page, zero out lo
	stx	heapptr		; 
	inc	heapptr+1	; bump up hi
malloc:
	clc
	adc	heapptr		; add heapptr lo to a
	tax			; save it in x
	tya			; copy y to a
	adc	heapptr+1	; add heapptr hi to a
	tay			; save it in y
	cpy	#>heapend	; cmp with heapend hi
	bcc	:+		; is < heapend hi ok
	bne	:++		; is > heapend hi not ok
	cpx	#<heapend	; is = headend hi need to check lo
	bcc	:+		; is < heapend lo
	bne	:++		; is > heapend lo
:	lda	heapptr+1	; get old heap hi in a
	sty	heapptr+1	; save new heapptr from y
	tay			; copy a to y to return
	lda	heapptr		; load old head lo in a
	stx	heapptr		; update heapptr with new from x
:	rts			; SEC set error


; Description: Convert string of ASCII to 16-bit hex.  Bug: no
;              overflow reported.  Keep strings to 5 digits.
;
; Input:       pointer to null terminated string in linebuf
;
; Output:      16-bit unsigned value in Y(hi)/A(lo)
;              CARRY=0: OK
;              CARRY=1: ERROR
;
; Clobbered:   A, Y, X
;
; Globals:     linebuf
;
; Locals:  hex (16-bit)

hex:	.byte	$0, $0

asciitohex:
	lda	#0		; setup hex with with 0
	sta	hex
	sta	hex+1
	ldx	#0		; main loop
	beq	asciitohex2	; get first digit
asciitohex1:
	; hex = hex * 10
	; hex = hex * (2 + 8)
	clc
	asl	hex
	rol	hex+1		; * 2
	lda	hex		; save lo byte
	ldy	hex+1		; save hi byte
	asl	hex
	rol	hex+1		; * 4
	asl	hex
	rol	hex+1		; * 8
	; add together hex * 2 (in a and y) to hex * 8
	; hex = hex * 2 + hex * 8
	clc
	adc	hex		; a + lo hex
	sta	hex		; store it
	tya			; get hi hex
	adc	hex+1		; add with carry and
	sta	hex+1		; store it
asciitohex2:
	lda	linebuf,x	; get next ascii
	and	#$7F		; strip off 8th bit (Apple II issue)
	cmp	#$3A		; ascii should be between $30-$39
	bcc	:+		; if < $3A ok
	rts			; else >= 40 and return with carry high
:	cmp	#$30		; if >= $30 ok
	bcs	:+		;
	sec			; use SEC for error flag
	rts			; return error
:	and	#$0F		; mask off 4 high bits to get decimal number
	clc			; clear carry
	adc	hex		; add to hex lo
	sta	hex		; save it
	bcc	:+		; no carry, no problem
	inc	hex+1		; got carry, inc hex hi
:	inx			; inc x
	lda	linebuf,x	; get next ascii
	beq	:+		; got null/EOL exit out
	bne	asciitohex1	; get next digit
:
	lda	hex		; return results in a (lo) and y (hi)
	ldy	hex+1
	clc			; clear carry to indicate no error
	rts


; Description: Failed to allocation memory error and die
;
; Input:       Nada
;
; Output:      String to screen, then exit code
;
; Clobbered:   A, Y, X, ptr
;
; Globals:     zp ptr (16-bit)

malloc_error:
	jsr	crout		; print CR
	sprint	"*** MEMORY ALLOCATION FAILED ***",$0D
	jmp	warm


; Description: Print a string.
;
; Input:       Y(hi)/A(lo) pointer to string
;
; Output:      String to screen
;
; Clobbered:   A, Y, X, ptr
;
; Globals:     zp ptr (16-bit)

print:	stay	ptr		; load Y/A (hi/lo) in ptr
	ldy	#0		; start with first char
	lda	(ptr),y		; load initial char
:	ora	#$80		; we do not want flashing or inverse
				;  (its an Apple II thing)
	jsr     cout		; call apple II char out
	iny			; y++
	lda	(ptr),y		; get next char
	bne	:-		; not NULL? then print it
	rts			; all done, move alone


; Description: Multiple 8-bit number by 10:  A = A * 10
;
; Input:       A (8-bit)
;
; Output:      A (8-bit)
;
; Clobbered:   A
;
; Locals:

x10tmp:	.byte	$0

x10:
	asl			; 2 x
	sta	x10tmp		; save it
	asl			; 4 x
	asl			; 8 x
	clc
	adc	x10tmp		; 8 x + 2 x
	rts


; Description: Detect Thunderclock card
;
; Output:      X = slot of card, else 0
;
; Clobbered:   A, X, Y

detect_thunderclock:
	ldx	#8		; start at slot 7
:	dex			; count down the slots
	beq	:+		;   got to zero?  then no card, return
	txa			; setup pointer at $CN08
	ora	#$C0
	sta	ptr+1
	lda	#$08
	sta	ptr
	ldy	#0		; check for 38 B0 01
	lda	(ptr),y
	cmp	#$38
	bne	:-		; This isn't the card you are looking for.
	iny
	lda	(ptr),y
	cmp	#$B0
	bne	:-		; This isn't the card you are looking for.
	iny
	lda	(ptr),y
	cmp	#$01
	bne	:-		; This isn't the card you are looking for.
	txa			; Found it!
				;   This IS the card you are looking for.
	ora	#$C0		; write out location of card
	sta	rdtcp+2		;   This is self-mod code,
	sta	wdtcp+2		;   generally, not a great idea.
	lda	#$08		;   wdtcp/rdtcp to be used later
	sta	rdtcp+1
	lda	#$0B
	sta	wdtcp+1
:	rts			; if x > 0, found it


; Description: Detect "No Slot Clock".  This is very hackish and can return
;              a false positive (just try it on the IIgs).  I could not find
;              any docs on how to detect the clock, other than to try to read
;              it.  The read code is a disassembly of the clockread DOS
;              utility.  To validate success the day of week must be 1, 2, 3,
;              4, 5, 6, or 7.  Week/weak check (ha! that was awesome :-).
;              I should probably check for proper ranges for all values to
;              reduce the risk of a false positive.
;
; Output:      CARRY set = success or false positive
;              CARRY cleared (0) = no no slot clock found
;
; Clobbered:   A, X, Y
;
; Globals:     nsc_dy

detect_noslotclock:
	jsr	read_noslotclock
	clc			; assume no clock
	lda	nsc_dy		; load day of week
	beq	:+		; must be 1 - 7
	sec			; set carry if clock
:	rts


; Description: If there is a clock, then load the time into (ptr)
;
; Input:       Y(hi)/A(lo) 16-bit pointer to 4-byte array
;              gotclock = 0, no clock, otherwise it is the type of clock:
;                1 = thunderclock
;                2 = no slot clock
;
; Output:      4-byte array  = s,m,h,1/100s (1/100 if supported by clock)
;
; Clobbered:   A, X, Y
;
; Globals:     ptr(16-bit), gotclock(8-bit, read-only), nsc_s(8-bit),
;              nsc_m(8-bit), nsc_h(8-bit), nsc_ss(8-bit)
;
; Calls:       read_noslotclock

gettime:
	sta	ptr
	sty	ptr+1

	lda	gotclock
	bne	:+
	rts
:	cmp	#1		; no slot clock
	bne	thunder
	jsr	read_noslotclock
	ldy	#0
	lda	nsc_s
	sta	(ptr),y
	iny
	lda	nsc_m
	sta	(ptr),y
	iny
	lda	nsc_h
	sta	(ptr),y
	iny
	lda	nsc_ss
	sta	(ptr),y
	rts
thunder:			; must be thunderclock
	lda	#$A3
wdtcp:	jsr	wdtcp		; self mod code, set read format
rdtcp:	jsr	rdtcp		; self mod code, read time into linebuf
				; parse out the thunderclock output from
	ldy	#0		;   linebuf, y holds the number of pairs to
	ldx	#17		;   get (SS,MM,HH)
:	dex
	lda	linebuf,x	; get 1's place
	eor	#$B0		; ASCII to decimal
	sta	(ptr),y		; store it
	dex
	lda	linebuf,x	; get 10's place
	eor	#$B0		; ASCII to decimal
	jsr	x10		; a = a * 10
	clc
	adc	(ptr),y		; add to 1's
	sta	(ptr),y		; store number (range 0-99)
	dex			; skip comma
	iny			; next pair
	cpy	#3		; got 3rd pair?
	bne	:-		;   no, then get next pair
	lda	#0		; There is no 1/100 with thunderclock so set
	sta	(ptr),y		;   1/100 to 0
	rts


; Description: If there is a clock, then print the time
;
; Input:       Y(hi)/A(lo) 16-bit pointer to 4-byte array
;
; Output:      Time (hh:mm:ss): hh:mm:ss[.1/100s] to screen
;
; Clobbered:   A, X, Y
;
; Globals:     starttime(32-bit), endtime(32-bit)

printtime:
	ldx	gotclock	; check for clock type
	bne	:+		;   if zero
	rts			;     then return
:	jsr	crout
	sprint	"Time (hh:mm:ss): "
	sec			; set carry
	lda	endtime+3	; sub starttime 1/100 sec from endtime
	sbc	starttime+3	;   1/100 sec
	bcs	:+		; carry still set?  Positive, no prob
	jsr	decsec		; dec a sec
	adc	#100		; got neg?  add 100
:	sta	starttime+3	; save it for printing
	sec			; set carry
	lda	endtime+0	; sub starttime sec from endtime sec
	sbc	starttime+0
	bcs	:+		; carry still set?  Positive, no prob
	jsr	decmin		; dec a min
	adc	#60		; got neg?  add 60
:	sta	starttime+0	; save it for printing
	sec			; set carry
	lda	endtime+1	; sub starttime min from endtime min
	sbc	starttime+1
	bcs	:+		; carry still set?  Positive, no prob
	dec	endtime+2	; dec an hour
	adc	#60		; got neg?  add 60
:	sta	starttime+1	; save it for printing
	sec			; set carry
	lda	endtime+2	; sub starttime hr from endtime hr
	sbc	starttime+2
	bcs	:+		; carry still set?  Positive, no prob
	adc	#24		; got neg?  add 60
:	tax			; print hours
	lda	bintobcd,x
	jsr	prbyte	
	lda	#':'+$80
	jsr	cout
	ldx	starttime+1	; print minutes
	lda	bintobcd,x
	jsr	prbyte	
	lda	#':'+$80
	jsr	cout
	ldx	starttime	; print seconds
	lda	bintobcd,x
	jsr	prbyte	
	ldx	starttime+3
	beq	:+
	lda	#'.'+$80
	jsr	cout
	ldx	starttime+3	; print 1/100 seconds
	lda	bintobcd,x
	jsr	prbyte	
:	jmp	crout		; crout will have rts

decsec:	ldx	endtime		; load endtime seconds
	bne	:+		; not zero, then just dec it
	jsr	decmin		; got zero, so take a min
	ldx	#60		;   and set sec to 60
	stx	endtime
:	dec	endtime
	rts

decmin:	ldx	endtime+1	; load endtime minites
	bne	:+		; not zero, then just dec it
	dec	endtime+2	; got zero, so take an hour
	ldx	#60		;   and set min to 60
	stx	endtime+1
:	dec	endtime+1
	rts


; Description: BIN/HEX to BCD table.  Used by print_mp and printtime

bintobcd:
	.byte	$00,$01,$02,$03,$04,$05,$06,$07,$08,$09
	.byte	$10,$11,$12,$13,$14,$15,$16,$17,$18,$19
	.byte	$20,$21,$22,$23,$24,$25,$26,$27,$28,$29
	.byte	$30,$31,$32,$33,$34,$35,$36,$37,$38,$39
	.byte	$40,$41,$42,$43,$44,$45,$46,$47,$48,$49
	.byte	$50,$51,$52,$53,$54,$55,$56,$57,$58,$59
	.byte	$60,$61,$62,$63,$64,$65,$66,$67,$68,$69
	.byte	$70,$71,$72,$73,$74,$75,$76,$77,$78,$79
	.byte	$80,$81,$82,$83,$84,$85,$86,$87,$88,$89
	.byte	$90,$91,$92,$93,$94,$95,$96,$97,$98,$99


; Description: 16-bit table of 0-255 * 100 broken down into multi100_lo and
;              multi100_hi.  Used by print_mp

	.linecont +
	.define	timestable \
		00000,00100,00200,00300,00400,00500,00600,00700,\
		00800,00900,01000,01100,01200,01300,01400,01500,\
		01600,01700,01800,01900,02000,02100,02200,02300,\
		02400,02500,02600,02700,02800,02900,03000,03100,\
		03200,03300,03400,03500,03600,03700,03800,03900,\
		04000,04100,04200,04300,04400,04500,04600,04700,\
		04800,04900,05000,05100,05200,05300,05400,05500,\
		05600,05700,05800,05900,06000,06100,06200,06300,\
		06400,06500,06600,06700,06800,06900,07000,07100,\
		07200,07300,07400,07500,07600,07700,07800,07900,\
		08000,08100,08200,08300,08400,08500,08600,08700,\
		08800,08900,09000,09100,09200,09300,09400,09500,\
		09600,09700,09800,09900,10000,10100,10200,10300,\
		10400,10500,10600,10700,10800,10900,11000,11100,\
		11200,11300,11400,11500,11600,11700,11800,11900,\
		12000,12100,12200,12300,12400,12500,12600,12700,\
		12800,12900,13000,13100,13200,13300,13400,13500,\
		13600,13700,13800,13900,14000,14100,14200,14300,\
		14400,14500,14600,14700,14800,14900,15000,15100,\
		15200,15300,15400,15500,15600,15700,15800,15900,\
		16000,16100,16200,16300,16400,16500,16600,16700,\
		16800,16900,17000,17100,17200,17300,17400,17500,\
		17600,17700,17800,17900,18000,18100,18200,18300,\
		18400,18500,18600,18700,18800,18900,19000,19100,\
		19200,19300,19400,19500,19600,19700,19800,19900,\
		20000,20100,20200,20300,20400,20500,20600,20700,\
		20800,20900,21000,21100,21200,21300,21400,21500,\
		21600,21700,21800,21900,22000,22100,22200,22300,\
		22400,22500,22600,22700,22800,22900,23000,23100,\
		23200,23300,23400,23500,23600,23700,23800,23900,\
		24000,24100,24200,24300,24400,24500,24600,24700,\
		24800,24900,25000,25100,25200,25300,25400,25500

	.align	256		; speed bump (~.35 sec for 1000 digits)
				; read_noslotclock has to be aligned anyway
				; so extra bytes in obj code already there
mult100_lo:
	.lobytes timestable
mult100_hi:
	.hibytes timestable


; Description: Cleaned up disassembly of the No Slot Clock DOS clockread
;              utility.  This code MUST be page aligned.
;
; Output:      nsc_* updated
;
; Clobbered:   A, X, Y
;
; Locals:

dosptr	=	$43
	.align	256		; must be aligned to page
read_noslotclock:
	sec
	bcs     :+
nsc_yr:	.byte	$57		; binary year
nsc_mo:	.byte	$04		; binary month
nsc_dt:	.byte	$08		; binary day of month
nsc_dy:	.byte	$04		; binary day of week
nsc_h:	.byte	$0E		; binary hour
nsc_m:	.byte	$2C		; binary minute
nsc_s:	.byte	$00		; binary second
nsc_ss:	.byte	$00		; binary 1/100th of second
	.byte	$5C,$A3,$3A,$C5,$5C,$A3,$3A,$C5
:	sei
	lda     $CFFF
	pha
	sta     $C300
	lda     $C804
	ldx     #$0A
:	lda     $41,x
	pha
	dex
	bne     :-
	lda     #$0B
	sta     $42
	lda     #$60
	sta     dosptr
	jsr     dosptr
	tsx
	lda     $0100,x
	sta     dosptr
	ldy     #$07
:	lda     ($42),y
	sec
	ror     a
:	pha
	lda     #$00
	rol     a
	tax
	lda     $C800,x
	pla
	lsr     a
	bne     :-
	dey
	bpl     :--
	ldx     #$07
:	ldy     #$07
:	lda     $C804
	ror     a
	ror     $44,x
	dey
	bpl     :-
	lda     $44,x
	pha
	and     #$0F
	sta     $44,x
	pla
	and     #$F0
	lsr     a
	lsr     a
	lsr     a
	lsr     a
	tay
	beq     :++
	lda     #$00
:	adc     #$0A
	dey
	bne     :-
:	adc     $44,x
	sta     $44,x
	dex
	bpl     :----
	lda     #$03
	sta     $42
	ldy     #$07
:	lda     $44,y
	sta     ($42),y
	dey
	bpl     :-
	ldx     #$00
:	pla
	sta     $42,x
	inx
	txa
	cmp     #$0A
	bne     :-
	pla
	rol     a
	bcs     :+
	sta     $CFFF
:	rts


;; end of subs start of heap

end:
heap:				; start of dynamic memory
