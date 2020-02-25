;; Apple-1 6800 Pi
;;
;; Egan Ford (egan@sense.net)
;;
;; Sept, 10 2012
;;
;; Apple-1 6800 Pi will compute 1000 decimal digits of Pi.
;;
;; All Apple-1 Pi computation is base 256 and then converted to base 10 for
;; display (technically base 100, but they both display the same).  Arrays
;; of bytes are used to represent multiprecision numbers.  In the comments
;; below "array" refers to a little endian multiprecision number.
;;


	cpu	6800		; That's right, a 6800-based Apple-1

;; Apple-1 variables/vectors

echo	=	$FFB4		; character out sub
prbyte	=	$FFA2		; print a hex byte
warm	=	$FF21		; warm boot


;; my global variables/vectors

ptr_a	=	$0		; $0-$1 16 bit generic pointer
n	=	$2		; $2-$3 16 bit counter
carry_mp =	$4		; $4-$4  8 bit multiprecision carry
srcptr	=	$4		; $4-$5 16 bit stack pointers used for multi-
dstptr	=	srcptr		;              array operations (add_mp,
				;              sub_mp, copy_mp)
				;              32/16 div
dividend =	$4              ; $4-$5 16 bit dividend (lo)
divisor	=	$8		; $8-$9 16 bit
x2	=	$A		; $A-$A  8 bit cached atan square
regb	=	$B		; $B-$B  8 bit backup of regb
stkptr	=	$C		; $C-$D 16-bit stack pointer backup
xreg	=	$E		; $E-$F 16-bit x reg pointer
begloop	=	xreg
endloop	=	$10		; $10-$11 16-bit x reg pointer
dlength	=	$12		; $12-$13 16-bit number of digits (R/W)
m100hip	=	$16		; $14-$15
m100lop	=	$18		; $16-$17
bcdtp	=	$20
monitor	=	$24		; $24-$2B reserved for monitor


;; my constants (R/O)

dec_len	=	1000		; 1000 decimal digits
bin_len	=	418		; ceil(1001 / log(256)) + 1 (+ 1 if odd) = 418
bcd_len	=	dec_len/2
				; the number of 4x unrolled loop is deterministic
				; unrollc = ceil(bin_len/4)
unrollc	=	bin_len/4+(bin_len#4>0)


;; start of global macros

mset	macro	ptr, value	; a = x
	ldx	ptr
	ldab	value
	jsr	set_mp
	endm

mprint	macro	ptr
	ldx	ptr
	jsr	print_mp
	endm

mdiv	macro	ptr, value	; a = a / x
	ldx	ptr
	ldab	value
	jsr	div_mp
	endm

mdiv16	macro	ptr, value	; a = a / x
	ldx	value
	stx	divisor
	ldx	ptr
	jsr	div16_mp
	endm

mcopy	macro	dst, src	; a = b
	ldx	src
	stx	srcptr
	ldx	dst
	jsr	copy_mp
	endm

madd	macro	dst, src	; a = a + b
	ldx	src
	stx	srcptr
	ldx	dst
	jsr	add_mp
	endm

msub	macro	dst, src	; a = a - b
	ldx	dst
	stx	dstptr
	ldx	src
	jsr	sub_mp
	endm

masl	macro	ptr		; a = a * 2
	ldx	ptr
	jsr	asl_mp
	endm

matan	macro	ptr, value	; a = atan(1/x)
	ldx	ptr
	ldab	value
	jsr	atan_mp
	endm

add16	macro	a, b		; a = a + b, 4+4+5+4+4+5 = 26 cycles for extnd
	ldaa	b+1		; lo bits
	adda	a+1
	staa	a+1
	ldaa	b		; hi bits
	adca	a		; with carry
	staa	a
	endm

;; end of macros


;; start of real code

	.org	$280		; start here
main:
	sei			; disable interrupts

	ldx	#m100hi		; setup table pointers
	stx	m100hip
	ldx	#m100lo
	stx	m100lop
	ldx	#bcdtbl
	stx	bcdtp

	ldx	#pitext		; print "1000 DIGITS ..."
	jsr	print
	jsr	pi		; compute pi

	mprint	#mp_a		; print it
	jsr	crout		; print CR
	jmp	warm		; all done, jmp to monitor


;; end of main code, start of non-zp allocations

arraylen:
	.byt	bin_len >> 8, bin_len & $FF	; length of base 256 array
arrayend:					; length of base 256 array
	.byt	(bin_len-1) >> 8
	.byt	(bin_len-1) & $FF
						; mp pointers
;mp_a:	.byt	mp_a_array >> 8, mp_a_array & $FF
;mp_b:	.byt	mp_b_array >> 8, mp_b_array & $FF
;mp_x:	.byt	mp_x_array >> 8, mp_x_array & $FF
;mp_y:	.byt	mp_y_array >> 8, mp_y_array & $FF

sqrtbl:	.byt	0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
	.byt	121, 144, 169, 196, 225

pitext:	.byt	"\r\r1000 DIGITS OF PI = \0"


;; start of subs

pi:	matan	#mp_a,#5	; a = atan(1/5)
	masl	#mp_a		; a = a * 4
	masl	#mp_a
	matan	#mp_b,#239	; b = atan(1/239)
	msub	#mp_a,#mp_b	; a = a - b
	masl	#mp_a		; a = a * 4
	masl	#mp_a
        rts

atan_mp:
	stx	ptr_a		; store x in ptr_a
	stab	regb		; store b in regb

	ldaa	#1		; n = 1 big endian
	staa	n+1
	ldaa	#0
	staa	n+0
	staa	x2		; clear x2, hack used for getting x2
				; note regb MUST follow x2 in memory

	mset	#mp_x,#1		; x = 1
	mdiv	#mp_x,regb	; x = x / b
	mcopy	ptr_a,#mp_x	; a = x
				; square x if x < 16
	ldaa	regb
	cmpa	#16
	bcc	++		; > 16
	tab			; A -> B
	decb			; B--
/	adda	regb		; A = A*(B-1), i.e. A^2
	decb
	bne	-
/	staa	x2

atan_mp_loop:			; main loop
	inc	n+1		; n now even
	bne	+		; n not zero
	inc	n+0		; n rolled to 0, inc n (hi)
/	inc	n+1		; n to be odd, save to inc (cannot roll from
				;   even to odd)
	ldaa	regb		; x = x / regx ^ 2
	cmpa	#16
	bcs	+		; x already x*x, one div required
	mdiv	#mp_x,x2	; x >= 16, then do two div
/	mdiv	#mp_x,x2	;   (faster than 16 bit div)

	mcopy	#mp_y,#mp_x	; y = x

	ldaa	n+0		; if n < 256 then div else div16
	bne	+		; >= 256 use 16 bit div
	mdiv	#mp_y,n+1	; < 256 use 8 bit div
	bcs	atan_mp_end	; dividend = 0, done
	bcc	++		; still working on it
/	mdiv16	#mp_y,n		; div16
	bcs	atan_mp_end	; dividend = 0, done

/	ldaa	n+1		; add or sub
	anda	#2
	beq	+		; add it
	msub	ptr_a,#mp_y	; a = a - y 
	jmp	atan_mp_loop
/	madd	ptr_a,#mp_y	; a = a + y 
	jmp	atan_mp_loop
atan_mp_end:
	rts


set_mp:
	stx	endloop		; store x in xreg
	add16	endloop,arrayend; endloop = endloop + arrayend
	dex			; start back one
	ldaa	#0
	jmp	++		; Duff device hardcoded for fixed arraylen
/	inx			; next x
	staa	,x		; faster than clra
	inx			; next x
	staa	,x		; faster than clra
/	inx			; next x
	staa	,x		; faster than clra
	inx			; next x
	staa	,x		; faster than clra
	cpx	endloop		; all done?
	bne	--
	stab	,x		; write digit left of decimal
	rts


copy_mp:			; a = b (dest = src)
	sts	stkptr		; backup stack pointer
	lds	srcptr		; use stack for source
	des			; dec stack back one (just the way sp works)
	dex			; start back one
	ldab	#unrollc
	jmp	++		; Duff device hardcoded for fixed arraylen
/	inx			; next x
	pula			; read b
	staa	,x		; write to a
	inx			; next x
	pula			; read b
	staa	,x		; write to a
/	inx			; next x
	pula			; read b
	staa	,x		; write to a
	inx			; next x
	pula			; read b
	staa	,x		; write to a
	decb
	bne	--
	lds	stkptr		; restore stack pointer
	rts


add_mp:
	sts	stkptr		; backup stack pointer
	lds	srcptr		; use stack for source
	des			; dec stack back one (just the way sp works)
	clc			; clear carry
	dex			; start back one
	ldab	#unrollc
	jmp	++		; Duff device hardcoded for fixed arraylen
/	inx			; next x
	pula			; read b
	adca	,x		; a = a + b
	staa	,x		; write to a
	inx			; next x
	pula			; read b
	adca	,x		; a = a + b
	staa	,x		; write to a
/	inx			; next x
	pula			; read b
	adca	,x		; a = a + b
	staa	,x		; write to a
	inx			; next x
	pula			; read b
	adca	,x		; a = a + b
	staa	,x		; write to a
	decb
	bne	--
	lds	stkptr		; restore stack pointer
	rts


sub_mp:
	sts	stkptr		; backup stack pointer
	lds	dstptr		; use stack for dst
	des			; dec stack back one (just the way sp works)
	ldab	#unrollc
	clc			; clear carry
	dex			; start back one
	jmp	++		; Duff device hardcoded for fixed arraylen
/	inx			; next x
	pula			; read a
	sbca	,x		; a = a - b
	psha			; store a
	ins			; inc stack for next a
	inx			; next x
	pula			; read a
	sbca	,x		; a = a - b
	psha			; store a
	ins			; inc stack for next a
/	inx			; next x
	pula			; read a
	sbca	,x		; a = a - b
	psha			; store a
	ins			; inc stack for next a
	inx			; next x
	pula			; read a
	sbca	,x		; a = a - b
	psha			; store a
	ins			; inc stack for next a
	decb
	bne	--
	lds	stkptr		; restore stack pointer
	rts


asl_mp:
	clc			; clear carry
	dex			; start back one
	ldab	#unrollc
	jmp	++		; Duff device hardcoded for fixed arraylen
/	inx			; next x
	rol	,x		; a = a * 2
	inx			; next x
	rol	,x		; a = a * 2
/	inx			; next x
	rol	,x		; a = a * 2
	inx			; next x
	rol	,x		; a = a * 2
	decb
	bne	--
	rts


skipzeros:
	stx	endloop		; 6
	stx	xreg		; 6
	add16	xreg,arraylen	; 26
				; total 26 + 6 + 6 + 5 = 43
	ldx	xreg		; x set to end of array
	jmp	++		; Duff device hardcoded for fixed arraylen
/	dex
	ldaa	,x
	bne	even
	dex
	ldaa	,x
	bne	odd
/	dex
	ldaa	,x
	bne	even
	dex
	ldaa	,x
	bne	odd
	cpx	endloop
	bne	--
	sec			; all zeros
	rts
odd:	inx			; make even
even:	inx			; offset +1 because we lead with dex
	clc
	rts


div8	macro
	rept	8		; unroll 8 times
	asla
	rolb
	bcs	m0
	cmpb	divisor
	bcs	m1
m0:	subb	divisor
	inca
m1:	
	endm
	endm

div_mp:
	stab	divisor
	jsr	skipzeros
	bcc	+
	rts
/	clrb			; set remainder to 0

	stx	xreg		; duff device, odd or even number 
	ldaa	xreg+1		;   of pairs, check bit 1.
	suba	endloop+1
	anda	#2		;   this only works if array on
	beq	+		;   /4 boundary 
	jmp	++

/	dex
	ldaa	,x
	div8
	staa	,x
	dex
	ldaa	,x
	div8
	staa	,x
/	dex
	ldaa	,x
	div8
	staa	,x
	dex
	ldaa	,x
	div8
	staa	,x

	cpx	endloop
	beq	+		; long branch
	jmp	--
/	clc
	rts


div16	macro			; array little endian, 
				;   needed to swap dividend+0 and +1
	rept	16		; unroll 16 times
	asl	dividend+0	; 6
	rol	dividend+1	; 6
	rolb			; 2
	rola			; 2
	subb	divisor+1	; 3
	sbca	divisor+0	; 3
	bcs	m0		; 4
	inc	dividend+0	; 6 
	jmp	m1		; 3 = 35 cycles with sub
m0:	addb	divisor+1	; 3
	adca	divisor+0	; 3 = 32 cycles no sub
m1:
	endm
	endm


div16_mp:
	jsr	skipzeros
	bcc	+
	rts
/	sts	stkptr		; save stack pointer

	clrb			; set remainder to 0

	stx	xreg		; duff device, number of pairs % 4
	ldaa	xreg+1		;   of pairs, check bit 1.
	suba	endloop+1
	anda	#%0110
	beq	+++
	lsra
	lsra

	bne	+
	jmp	d1
/	ldaa	#0
	bcs	+
	jmp	d2
/	jmp	d3

/	dex
	dex	
	lds	,x
	sts	dividend
	div16
	lds	dividend
	sts	,x

d3:	dex
	dex	
	lds	,x
	sts	dividend
	div16
	lds	dividend
	sts	,x

d2:	dex
	dex	
	lds	,x
	sts	dividend
	div16
	lds	dividend
	sts	,x

d1:	dex
	dex	
	lds	,x
	sts	dividend
	div16
	lds	dividend
	sts	,x

	cpx	endloop
	beq	+		; long branch
	jmp	-
/	clc
	lds	stkptr		; restore stack pointer
	rts


print_mp:
				; setup start/end for x100 loop
	stx	xreg		; backup start of array pointer (LSB)
	stx	endloop		; store x in xreg
	add16	endloop,arrayend; endloop = endloop + arrayend
	ldx	#bcd_len	; backup bcd_len to dlength for multiple prints
	stx	dlength
				; print left of decimal point
	ldx	endloop		; get last digit (MSB) in array (little endian)
	ldaa	,x
	clr	,x		; zero out MSB
	jsr	bintobcd	; convert from hex to dec
	cmpa	#9		; if <= 9 skip leading zero
	bls	+		;   branch to print single digit ascii
	jsr	prbyte		; otherwise just print it
	jmp	++		; skip to print period
/	clc			; no leading zero code
	adca	#$B0		;   convert single digit to ascii
	jsr	echo		;   print it
/	ldaa	#'.'+$80	; print decimal point (fixed point math)
	jsr	echo		;   print it

print_mp1:			; loop through rest of decimal digits

m100	macro
	pula
	staa	m100hip+1	; store A to hi (lo) pointer
	ldx	m100hip		; X points to hi
	ldab	,x		; B = A*100 hi
	staa	m100lop+1	; store A to lo (lo) pointer
	ldx	m100lop		; X points to lo
	ldaa	,x		; A = A*100 lo
	adda	carry_mp	; + carry from chain
	adcb	#0
	psha
	ins
	stab	carry_mp
	endm

	sts	stkptr		; backup stack pointer
				; mp array * 100
	clr	carry_mp	; zero out carry
	lds	xreg
	des			; dec stack back one (just the way sp works)
	jmp	++		; Duff device hardcoded for fixed arraylen
/	m100	
	m100	
/	m100
	m100	
	tsx
	dex
	cpx	endloop		; all done?
	beq	+		; long branch (unroll too long)
	jmp	--
/
	lds	stkptr		; restore stack pointer

				; print MSB
				; A has last digit (MSB) in array (little endian)
	clr	,x		; zero out MSB
	jsr	bintobcd	; convert from hex to dec
	jsr	prbyte		; print it

	ldx	dlength		; dlength = dlength - 1
	dex
	beq	+
	stx	dlength
	jmp	print_mp1
/	rts


; Description: BIN/HEX to BCD.  BCD = BIN + 6*int(BIN/10).
;
; Input:       A (BIN)
;
; Output:      A (BCD)
;
; Clobbered:   A, B

bintobcd:
	staa	bcdtp+1
	ldx	bcdtp
	ldaa	,x
	rts
bintobcd_slow:
	psha			; backup A
	clc			; clear carry (6800 is NOT the 6502)
	ldab	#-1		; start counter at -1
/	incb			; inc b (first loop = 0)
	sbca	#10		; a = a - 10
	bpl	-		; > 0?, then keep - 10s
	clc			; clear carry, the sbca < 0 should have set it
	pula			; restore A
	jmp	++		; jump to decb
/	adca	#6		; a = a + 6
/	decb			; if a < 10, then b will be -1 and end loop
	bpl	--		;   otherwise add 6 for every 10
	rts


print:	ldaa	,x
	beq	+
	jsr	echo
	inx
	jmp	print
/	rts


crout:	ldaa	#$8D
        jmp	echo

;; tables for faster math must be page aligned

	rept	256-(*)&$FF
	fcb	0
	endm

m100lo:	fcb	$00,$64,$C8,$2C,$90,$F4,$58,$BC,$20,$84,$E8,$4C,$B0,$14,$78,$DC
	fcb	$40,$A4,$08,$6C,$D0,$34,$98,$FC,$60,$C4,$28,$8C,$F0,$54,$B8,$1C
	fcb	$80,$E4,$48,$AC,$10,$74,$D8,$3C,$A0,$04,$68,$CC,$30,$94,$F8,$5C
	fcb	$C0,$24,$88,$EC,$50,$B4,$18,$7C,$E0,$44,$A8,$0C,$70,$D4,$38,$9C
	fcb	$00,$64,$C8,$2C,$90,$F4,$58,$BC,$20,$84,$E8,$4C,$B0,$14,$78,$DC
	fcb	$40,$A4,$08,$6C,$D0,$34,$98,$FC,$60,$C4,$28,$8C,$F0,$54,$B8,$1C
	fcb	$80,$E4,$48,$AC,$10,$74,$D8,$3C,$A0,$04,$68,$CC,$30,$94,$F8,$5C
	fcb	$C0,$24,$88,$EC,$50,$B4,$18,$7C,$E0,$44,$A8,$0C,$70,$D4,$38,$9C
	fcb	$00,$64,$C8,$2C,$90,$F4,$58,$BC,$20,$84,$E8,$4C,$B0,$14,$78,$DC
	fcb	$40,$A4,$08,$6C,$D0,$34,$98,$FC,$60,$C4,$28,$8C,$F0,$54,$B8,$1C
	fcb	$80,$E4,$48,$AC,$10,$74,$D8,$3C,$A0,$04,$68,$CC,$30,$94,$F8,$5C
	fcb	$C0,$24,$88,$EC,$50,$B4,$18,$7C,$E0,$44,$A8,$0C,$70,$D4,$38,$9C
	fcb	$00,$64,$C8,$2C,$90,$F4,$58,$BC,$20,$84,$E8,$4C,$B0,$14,$78,$DC
	fcb	$40,$A4,$08,$6C,$D0,$34,$98,$FC,$60,$C4,$28,$8C,$F0,$54,$B8,$1C
	fcb	$80,$E4,$48,$AC,$10,$74,$D8,$3C,$A0,$04,$68,$CC,$30,$94,$F8,$5C
	fcb	$C0,$24,$88,$EC,$50,$B4,$18,$7C,$E0,$44,$A8,$0C,$70,$D4,$38,$9C

m100hi:	fcb	$00,$00,$00,$01,$01,$01,$02,$02,$03,$03,$03,$04,$04,$05,$05,$05
	fcb	$06,$06,$07,$07,$07,$08,$08,$08,$09,$09,$0A,$0A,$0A,$0B,$0B,$0C
	fcb	$0C,$0C,$0D,$0D,$0E,$0E,$0E,$0F,$0F,$10,$10,$10,$11,$11,$11,$12
	fcb	$12,$13,$13,$13,$14,$14,$15,$15,$15,$16,$16,$17,$17,$17,$18,$18
	fcb	$19,$19,$19,$1A,$1A,$1A,$1B,$1B,$1C,$1C,$1C,$1D,$1D,$1E,$1E,$1E
	fcb	$1F,$1F,$20,$20,$20,$21,$21,$21,$22,$22,$23,$23,$23,$24,$24,$25
	fcb	$25,$25,$26,$26,$27,$27,$27,$28,$28,$29,$29,$29,$2A,$2A,$2A,$2B
	fcb	$2B,$2C,$2C,$2C,$2D,$2D,$2E,$2E,$2E,$2F,$2F,$30,$30,$30,$31,$31
	fcb	$32,$32,$32,$33,$33,$33,$34,$34,$35,$35,$35,$36,$36,$37,$37,$37
	fcb	$38,$38,$39,$39,$39,$3A,$3A,$3A,$3B,$3B,$3C,$3C,$3C,$3D,$3D,$3E
	fcb	$3E,$3E,$3F,$3F,$40,$40,$40,$41,$41,$42,$42,$42,$43,$43,$43,$44
	fcb	$44,$45,$45,$45,$46,$46,$47,$47,$47,$48,$48,$49,$49,$49,$4A,$4A
	fcb	$4B,$4B,$4B,$4C,$4C,$4C,$4D,$4D,$4E,$4E,$4E,$4F,$4F,$50,$50,$50
	fcb	$51,$51,$52,$52,$52,$53,$53,$53,$54,$54,$55,$55,$55,$56,$56,$57
	fcb	$57,$57,$58,$58,$59,$59,$59,$5A,$5A,$5B,$5B,$5B,$5C,$5C,$5C,$5D
	fcb	$5D,$5E,$5E,$5E,$5F,$5F,$60,$60,$60,$61,$61,$62,$62,$62,$63,$63

bcdtbl:	fcb	$00,$01,$02,$03,$04,$05,$06,$07,$08,$09
	fcb	$10,$11,$12,$13,$14,$15,$16,$17,$18,$19
	fcb	$20,$21,$22,$23,$24,$25,$26,$27,$28,$29
	fcb	$30,$31,$32,$33,$34,$35,$36,$37,$38,$39
	fcb	$40,$41,$42,$43,$44,$45,$46,$47,$48,$49
	fcb	$50,$51,$52,$53,$54,$55,$56,$57,$58,$59
	fcb	$60,$61,$62,$63,$64,$65,$66,$67,$68,$69
	fcb	$70,$71,$72,$73,$74,$75,$76,$77,$78,$79
	fcb	$80,$81,$82,$83,$84,$85,$86,$87,$88,$89
	fcb	$90,$91,$92,$93,$94,$95,$96,$97,$98,$99

; data

mp_a:	.org	*+bin_len
mp_b:	.org	*+bin_len
mp_x:	.org	*+bin_len
mp_y:	.org	*+bin_len

end:
