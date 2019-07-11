;; Apple-1 Pi
;;
;; Mar, 12 2013
;;
;; Egan Ford (egan@sense.net) with a lot of optimizations and pointers from
;; Anton Treuenfels (atreuenfels@earthlink.net)
;;
;; (c) 2013, All Rights Reserved, Egan Ford (egan@sense.net)
;;
;; THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
;; KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
;; PARTICULAR PURPOSE.
;;
;; Apple-1 Pi will compute 1000 decimal digits of Pi.
;;
;; All Apple-1 Pi computation is base 256 and then converted to base 10 for
;; display (technically base 100, but they both display the same).  Arrays
;; of bytes are used to represent multiprecision numbers.  In the comments
;; below "array" refers to a big endian multiprecision number.
;;
;; C snippets:  J.W. Stumpel, (c) May 1991
;;
;; Assembler: ca65
;;

;; Apple-1 variables/vectors

apple	=	1
	.if	apple = 1
cout	=	$FFEF		; character out sub
prbyte	=	$FFDC		; print a hex byte
warm	=	$FF1F		; back to monitor
org	=	$280		; start here
	.else
cout	=	$FDED		; character out sub
prbyte	=	$FDDA		; print a hex byte
warm	=	$FF69		; back to monitor
org	=	$800		; start here
	.endif


;; my global variables/vectors

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
dlength	=	$C		; $C-$D	16 bit


dec_len	=	1000		; 1000 decimal digits
bin_len	=	418		; ceil(1001 / log(256)) + 1 (+ 1 if odd) = 418
bin_end	=	bin_len-1
bcd_len	=	dec_len/2


;; start of global macros

.include	"pimacros.m"

;; end of macros

;; start of real code
	.org	org
main:
	jsr	crout		; print CR
	jsr	crout		; print CR

	sprint	"1000 DIGITS OF PI = "
cnton 1
	jsr	pi
cntoff 1
	mprint	#mp_a		; print it (note mp_a hosed)
	jsr	crout		; print CR
	jmp	warm


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
; Registers:   A, X, Y
;
; Calls:       matan/atan_mp, masl/asl_mp, msub/sub_mp

pi:	matan	#mp_a,#5		; a = atan(1/5)
	masl	#mp_a		; a = a * 4
	masl	#mp_a
	matan	#mp_b,#239	; b = atan(1/239)
	msub	#mp_a,#mp_b	; a = a - b
	masl	#mp_a		; a = a * 4
	masl	#mp_a
	rts

; Description: Compute arctan(N) using the Gregory expansion of Machin's
;              arctan formula and save in array (ptr_a).
;
;
;                            / 1     1      1       \
;              arctan(N) =  |  - - ---- + ---- - ... |
;                           |  N      3      5       |
;                            \     3xN    5xN       /
;
;
; Input:       Y/A (hi/lo) pointer to array
;              X = N (8 bit)
;
; Output:      Y/A (hi/lo) pointer to array = arctan(1/N)
;
; Registers:   A, X, Y
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

regx	=	$E		;  $E-$E    8 bit
x2	=	$F		;  $D-$F    8 bit, x^2 if x < 16
n	=	$10		; $10-$11  16 bit
ptr_a	=	$12		; $12-$13  16 bit
ptr_save=	$14		;     $14   8 bit

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

	mset	#mp_x,#1		; mp_x = 1
	mdiv	#mp_x,regx	; mp_x /= regx
	mcopy	ptr_a,#mp_x	; ptr_a = mp_x

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
	mdiv	#mp_x,x2		; x >= 16, then do two div
:	mdiv	#mp_x,x2		;   (faster than 16 bit div)

	mcopy	#mp_y,#mp_x	; y = x

	ldx	n+1		; if n < 256 then div else div16
	bne	:+		; >= 256 use 16 bit div
	mdiv	#mp_y,n		; < 256 use 8 bit div
	bcs	atan_mp_end	; dividend = 0, done
	bcc	:++		; still working on it
:	mdiv16	#mp_y,n		; div16
	bcs	atan_mp_end	; dividend = 0, done

:	lda	n		; add or sub
	and	#2
	beq	:+		; add it
	msub	ptr_a,#mp_y	; a = a - y 
	jmp	atan_mp_loop
:	madd	ptr_a,#mp_y	; a = a + y 
	jmp	atan_mp_loop
atan_mp_end:
	rts


; Description: set array initial value
;
; Input:       Y(hi)/A(lo) pointer to mp
;              X = value left of decimal
;
; Output:      (ptr),0 = X
;              (ptr),1 - (ptr),length-1 = 0
;
; Registers:   A, X, Y
;
; Globals:     zp length (16-bit), zp ptr (16-bit)

set_mp:	
	stay	ptr		; store ptr lo/hi from A/Y
	ldy	#0		; left of decimal
	txa			; x holds initial value left of decimal
	sta	(ptr),y		; store initial value
	inc	ptr		; bump up pointer
	bne	:+		;  to next digit
	inc	ptr+1		;  rest of array will be 0
: 	ldx	#>bin_end	; length of remaining digits to set
	ldy	#<bin_end
	inx			; bump up x +1 for the partial page
	clc			; adjust to point below source start
	tya			; - ex: ptr = $xx00, bin_len = 1
	adc	ptr		; we want ptr = $(xx-1)01, y = $ff (255)
	sta	ptr
	bcs	:+
	dec	ptr+1
:	tya			; adjust y index value, y = -y
	eor	#$ff		; one's complement
	tay
	iny			; two's complement
	lda	#0		; A must be set to #0 to clear array 
	beq	@3		; Duff's Device hard coded
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


; Description: Multiprecision add:  a = a + b
;
; Input:       Y/A (hi/lo) pointer to array (a)
;              ptr_mp set by macro to point to array (b)
;
; Output:      a = a + b
;
; Registers:   A, X, Y
;
; Globals:     zp ptr (16-bit),
;              zp ptr_mp (16-bit)

add_mp:
	sta	ptr		; store ptr lo from A
	tya
	clc
	adc	#>bin_end	; add number of pages since we have to go
	sta	ptr+1		;   backwards for add/sub/asl
	lda	ptr_mp+1
	clc
	adc	#>bin_end	; add number of pages since we have to go
	sta	ptr_mp+1	;   backwards for add/sub/asl
	ldx	#>bin_end	; full pages
	ldy	#<bin_end	; partial
	clc			; clear carry for many adds
				; Duff's Device
	bcc	@1		; bin_len hard coded, no need to guess
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
; Registers:   A, X, Y
;
; Globals:     zp ptr (16-bit),
;              zp ptr_mp (16-bit)

sub_mp:
	sta	ptr		; store ptr lo from A
	tya
	clc
	adc	#>bin_end	; add number of pages since we have to go
	sta	ptr+1		;   backwards for add/sub/asl
	lda	ptr_mp+1
	clc
	adc	#>bin_end	; add number of pages since we have to go
	sta	ptr_mp+1	;   backwards for add/sub/asl
	ldx	#>bin_end	; full pages
	ldy	#<bin_end	; partial
	sec			; sec carry for many subs
	bcs	@1		; bin_len hard coded, no need to guess
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
; Registers:   A, X, Y
;
; Globals:     zp ptr (16-bit),

asl_mp:
	sta	ptr		; store ptr lo from A
	tya
	clc
	adc	#>bin_end	; add number of pages since we have to go
	sta	ptr+1		;   backwards for add/sub/asl
	ldx	#>bin_end	; full pages
	ldy	#<bin_end	; partial
	clc			; clear carry for many rols
	bcc	@1		; bin_len hard coded, no need to guess
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
; Registers:   A, X, Y
;
; Globals:     zp ptr (16-bit), zp ptr_mp (16-bit)

copy_mp:
	sta	ptr		; store ptr lo from A
	sty	ptr+1		; store ptr hi from Y
	ldx	#>bin_len	; full pages
	ldy	#<bin_len	; partial pages
	beq	@0		;   if not process full pages only
	inx			; bump up x +1 for the partial page
	clc			; adjust to point below source start
	tya			; - ex: ptr = $xx00, bin_len = 1
	adc	ptr		; we want ptr = $(xx-1)01, y = $ff (255)
	sta	ptr
	bcs	:+
	dec	ptr+1
:	clc			; adjust to point below destination start
	tya			; - ex: ptr_mp = $xxff, bin_len = 2
	adc	ptr_mp		; we want ptr_mp = $xx01, y = $fe (254)
	sta	ptr_mp
	bcs	:+
	dec	ptr_mp+1
:	tya			; adjust y index value, y = -y
	eor	#$ff		; one's complement
	tay
	iny			; two's complement
				; Duff's Device.
	jmp	@2		; bin_len hard coded, no need to guess
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
; Registers:   A, X, Y
;
; Globals:     zp ptr (16-bit)

skipzeros:
	sta	ptr		; store ptr lo from A
	sty	ptr+1		; store ptr hi from Y
	ldx	#>bin_len	; full pages
	ldy	#<bin_len	; partial pages ?
	beq	@0		; b:no - process full pages only
	inx			; bump up x +1 for the partial page
	clc			; adjust to point below source start
	tya			; - ex: ptr = $xx00, bin_len = 1
	adc	ptr		; we want ptr = $(xx-1)01, y = $ff (255)
	sta	ptr
	bcs	:+
	dec	ptr+1
:	tya			; adjust y index value, y = -y
	eor	#$ff		; one's complement
	tay
	iny			; two's complement
				; Duff's Device.
	jmp	@2		; bin_len hard coded, no need to guess
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
; Registers:   A, X

.macro	div8			
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
.endmacro


; Description: Divide mp array by 8-bit number.
;
; Input:       Y(hi)/A(lo) pointer to mp
;              X = dividend/denominator
;
; Output:      (ptr) = (ptr)/x
;
; Registers:   A, X, Y
;
; Globals:     zp ptr (16-bit), dividend (8-bit),
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
	and	#%10		; Duff's device.  Check for odd or even number
	beq	@0		; of pairs
	jmp	@2
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
; Registers:   A, X, Y

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
; Registers:   A, X, Y
;
; Globals:     zp ptr (16-bit), dividend (32-bit),
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
	bne	:+
	jmp	@1		; 0,1 long branch
:	bcc	:+		; 1,0 long branch
	jmp	@3		; 1,1 long branch
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
; Registers:   A, X, Y, input array
;
; Globals:     zero page: ptr (16-bit) , ptr_mp (16-bit), carry_mp (8-bit)
;
; Locals:

print_mp:
	sta	ptr		; store ptr lo from A
	sty	ptr+1		; store ptr hi from Y
	tya
	clc
	adc	#>bin_end	; add number of pages since we have to go
	sta	ptr_save	;   backwards for multiply

	lda	#<bcd_len
	sta	dlength
	lda	#>bcd_len
	sta	dlength+1

				; print left of decimal
	ldy	#0		; get first digit in array (big endian)
	lda	(ptr),y
	tax
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

	ldx	#>bin_end	; full pages
	stx	xreg
	ldy	#<bin_end	; partial


;;;; skip trailing zeros


;	jmp	@10
;:	dec	xreg
;	dec	ptr_mp+1
;:	dey
;@30:	
;	lda	(ptr_mp),y	; get it
;	bne	@3
;	dey
;@20:	
;	lda	(ptr_mp),y	; get it
;	bne	@2
;	dey
;@10:	
;	lda	(ptr_mp),y	; get it
;	bne	@1
;	dey
;@00:	
;	lda	(ptr_mp),y	; get it
;	bne	@0
;	tya
;	bne	:-
;	ldx	xreg
;	bne	:--



				; Duff's Device.   We know that bin_len is
				;   even (#<bin_end odd), so just need to
	jmp	@1		;   check for 2nd bit (odd or even # of pairs).
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
	lda	(ptr),y
	tax
	lda	bintobcd,x	; table lookup
	jsr	prbyte		; print it, only A gets hosed, safe to call

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


; Description: Print a string.
;
; Input:       Y(hi)/A(lo) pointer to string
;
; Output:      String to screen
;
; Registers:   A, Y, X, ptr
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


crout:	lda	#$8D
	jmp	cout


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

mult100_lo:
	.lobytes timestable
mult100_hi:
	.hibytes timestable


; data 

mp_a:
	.org	*+bin_len
mp_b:
	.org	*+bin_len
mp_x:
	.org	*+bin_len
mp_y:
	.org	*+bin_len

end:
