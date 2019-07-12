;; 65816 Pie, 65816-based Pi/e
;;
;; Mar 17, 2013
;;
;; (c) 2013, All Rights Reserved, Egan Ford (egan@sense.net)
;;
;; THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
;; KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
;; PARTICULAR PURPOSE.
;;
;; 65816 Pie will compute 10000 decimal digits of Pi or e.
;;
;; All 65816 Pie computation is base 256 and then converted to base 10 for
;; display (technically base 100, but they both display the same).  Arrays
;; of bytes are used to represent multiprecision numbers.  In the comments
;; below "array" refers to a big endian multiprecision number.
;;
;; Assembler: Macro Assembler AS V1.42
;;

				; Assembler: Macro Assembler AS V1.42
	cpu	65816		; 65816/65802
				; should work with Apple IIe w/ 65802
				; and Apple IIgs ProDOS 8

				; 8-bit mode vectors
cout	=	$FDED		; character out sub
prbyte	=	$FDDA		; print a hex byte
crout	=	$FD8E		; CR out sub
warm	=	$FF69		; jmp to monitor


;; my constants (R/O)

dec_len	=	10000		; 10000 decimal digits
bin_len	=	2081		; ceil(10001 / log(65536)) + 4 (guard digits) = 2081
				;   Duff's Device 4x unrolled loops assume
				;   bin_len % 4 = 1
bin_end	=	bin_len-1
bcd_len	=	dec_len/2
unrollc	=	bin_len/4+(bin_len#4>0)


;; zeropage (R/W)

ptr_0	=	$0		; $0-$1 16 bit
dlength	=	$2		; $2-$3	16 bit
carry_mp =	$4		; $4-$4  8 bit multiprecision carry


;; direct page (R/W)

dp	=	$800		; $800 to be direct page for computation

ptr	=	dp+$10		; $0-$1 16 bit generic pointer
ptr_mp	=	dp+$12		; $2-$3 16 bit generic pointer
				;          32/16 div
dividend =	dp+$14		; $4-$7 32 bit
divisor	=	dp+$18		; $8-$9 16 bit

n	=	dp+$20		; $10-$11  16 bit
ptr_a	=	dp+$22		; $12-$13  16 bit

regy	=	dp+$2E
regx	=	dp+$30		;  $E-$E    8 bit
x2	=	dp+$32		;  $D-$F    8 bit, x^2 if x < 16



;; start of global macros

mset	macro	ptr, value	; a = x
	lda	ptr
	ldx	value
	jsr	set_mp
	endm

mcopy	macro	a, b		; a = b
	ldy	a		; Y = dest
	ldx	b		; X = source
	lda	#(bin_len*2)-1	; bytes to copy - 1
	mvn	0,0		; copy block
	endm

madd	macro	a, b		; a = a + b
	lda	a
	sta	ptr
	lda	b
	sta	ptr_mp
	jsr	add_mp
	endm

msub	macro	a, b		; a = a - b
	lda	a
	sta	ptr
	lda	b
	sta	ptr_mp
	jsr	sub_mp
	endm

masl	macro	a		; a = a * 2
	lda	a
	sta	ptr
	jsr	asl_mp
	endm

mdiv	macro	ptr, value	; a = a / x
	lda	ptr
	ldx	value
	jsr	div_mp
	endm

matan	macro	ptr, value	; a = a / x
	lda	ptr
	ldx	value
	jsr	atan_mp
	endm

mprint	macro	ptr
	lda	ptr
	jsr	print_mp
	endm



	org	$900		; start here
main:
	clc			; clear carry
	xce			; exchange emulation/carry flag
				; to switch to 65816 mode
	rep	#%00110000	; 16-bit data/index mode
	assume	m:0		; assume 16-bit memory
	assume	x:0		; assume 16-bit index
	assume	dpr:$0		; assume direct page is 0

	lda	#pitext		; print PI text
	jsr	print
				; setup full DP for better speed
	lda	#dp		; setup direct page
	tcd			; C(A) -> DPR
	assume	dpr:dp		; assume direct page is $800

	jsr	pi		; compute Pi

	lda	#$0		; setup DPR back to 0
	tcd			; setup direct page

	mprint	#mp_a		; print results

	sep	#%00110000	; 8-bit data/index mode
	jsr	crout		; print CR
				; back to full-on 8-bit mode
	sec			; set carry
	xce			; exchange emulation/carry flag
				; to switch to 6502 mode

	jmp	warm		; emu65816 version
	rts


pitext:	byt	"10000 DIGITS OF PI = \0"

;;;
;;; Direct Page $800 code
;;;
	assume	m:0		; assume 16-bit memory
	assume	x:0		; assume 16-bit index
	assume	dpr:dp		; assume direct page is dp


pi:	matan	#mp_a,#5	; a = atan(1/5)
	masl	#mp_a		; a = a * 4
	masl	#mp_a
	matan	#mp_b,#239	; b = atan(1/239)
	msub	#mp_a,#mp_b	; a = a - b
	masl	#mp_a		; a = a * 4
	masl	#mp_a
	rts


atan_mp:
	sta	ptr_a		; store ptr to ptr_a
	stx	regx		; save x
	txa			; X -> A
	asl			; 2x
	tax			; A -> X 
	lda	sqrtbl,x	; load from squares table
	sta	x2		;   and save
	mset	#mp_x,#1	; mp_x = 1
	mdiv	#mp_x,regx	; mp_x /= regx
	mcopy	ptr_a,#mp_x	; ptr_a = mp_x
	lda	#1		; n = 1
	sta	n
atan_mp_loop:			; main loop
	inc	n		; n += 2
	inc	n
	mdiv	#mp_x,x2	; x = x / x2
	mcopy	#mp_y,#mp_x	; y = x
	mdiv	#mp_y,n		; y = y / n
	bcs	atan_mp_end	; dividend = 0, done
	lda	n		; add or sub
	and	#2
	beq	+		; add it
	msub	ptr_a,#mp_y	; a = a - y 
	jmp	atan_mp_loop
/	madd	ptr_a,#mp_y	; a = a + y 
	jmp	atan_mp_loop
atan_mp_end:
	rts


set_mp: sta	ptr		; write pointer to ptr
	txa			; A = initial value left of decimal
	xba			; swap A/B to put init value in MSB
	ldy	#bin_end*2	; Y = MSW
	sta	(ptr),y		; write out init value
	lda	#0		; zero out rest of array
/	dey
	dey
	sta	(ptr),y		; write out zero
	dey
	dey
	sta	(ptr),y		; write out zero
	dey
	dey
	sta	(ptr),y		; write out zero
	dey
	dey
	sta	(ptr),y		; write out zero
	cpy	#0
	bne	-
	rts


add_mp: ldy	#0		; Y = LSW
	ldx	#unrollc	; number of unrolled loops
	clc			; clear carry
	jmp	++		; pre computed Duff's Device
/	lda	(ptr),y
	adc	(ptr_mp),y
	sta	(ptr),y
	iny
	iny
	lda	(ptr),y
	adc	(ptr_mp),y
	sta	(ptr),y
	iny
	iny
	lda	(ptr),y
	adc	(ptr_mp),y
	sta	(ptr),y
	iny
	iny
/	lda	(ptr),y
	adc	(ptr_mp),y
	sta	(ptr),y
	iny
	iny
	dex
	bne	--
	rts


sub_mp: ldy	#0		; Y = LSW
	ldx	#unrollc	; number of unrolled loops
	sec			; set carry
	jmp	++		; pre computed Duff's Device
/	lda	(ptr),y
	sbc	(ptr_mp),y
	sta	(ptr),y
	iny
	iny
	lda	(ptr),y
	sbc	(ptr_mp),y
	sta	(ptr),y
	iny
	iny
	lda	(ptr),y
	sbc	(ptr_mp),y
	sta	(ptr),y
	iny
	iny
/	lda	(ptr),y
	sbc	(ptr_mp),y
	sta	(ptr),y
	iny
	iny
	dex
	bne	--
	rts


asl_mp: ldy	#0		; Y = LSW
	ldx	#unrollc	; number of unrolled loops
	clc			; clear carry
	jmp	++		; pre computed Duff's Device
/	lda	(ptr),y
	rol
	sta	(ptr),y
	iny
	iny
	lda	(ptr),y
	rol
	sta	(ptr),y
	iny
	iny
	lda	(ptr),y
	rol
	sta	(ptr),y
	iny
	iny
/	lda	(ptr),y
	rol
	sta	(ptr),y
	iny
	iny
	dex
	bne	--
	rts


div16	macro		
	lda	(ptr),y	
	sta	dividend
	txa			; remainder
	asl	dividend
	rept 16			; unroll 16 times for speed
	rol
	bcs	m0
	cmp	divisor
	bcc	m1
m0:	sbc	divisor
	sec
m1:	rol	dividend
	endm
	tax			; remainder
	lda	dividend
	sta	(ptr),y
	endm

div_mp:
	sta	ptr		; store ptr
	stx	divisor		; save divisor
	ldy	#bin_end*2	; Y = MSW
				; Duff's Device
	jmp	++		; bin_len hard coded, no need to guess
/	lda	(ptr),y		; check	0,4,...,252
	bne	++
	dey
	dey
	lda	(ptr),y		; check 1,5,...,253
	bne	++
	dey
	dey
	lda	(ptr),y		; check 2,6,...,254
	bne	++
	dey
	dey
/	lda	(ptr),y		; check 3,7,...,255
	bne	+
	dey
	dey
	cpy	#-2
	bne	--
	sec			; flag: all cells are zero
	rts
/
	ldx	#0		; x will hold remainder
	tya			; Duff's Device
	lsr			; even number of non-zero
	inc	a
	and	#%11		; however there can be an odd number of pairs
	beq	d0		; 00 & 11 = 0
	lsr			; 00 => 0,0; 01 => 0,1; 10 => 1,0; 11 => 1,1
	bne	+
	jmp	d3		; 0,1 long branch
/	bcc	+		; 1,0 long branch
	jmp	d1		; 1,1 long branch
/	jmp	d2

d0:	div16
	dey
	dey
d1:	div16
	dey
	dey
d2:	div16
	dey
	dey
d3:	div16
	dey
	dey
	cpy	#-2
	beq	+
	jmp	d0
/	clc			; non zero result
	rts


;;;
;;; Direct Page $0 code
;;;

	assume	m:0		; assume 16-bit memory
	assume	x:0		; assume 16-bit index
	assume	dpr:$0		; assume direct page is 0

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
	sta	ptr_0		; write out pointer
	lda	#bcd_len	; number of # of multiplies to perform
	sta	dlength
	lda	#0		; wipe out high A bits (can cause problems with tax)

	sep	#%00100000	; 8-bit data mode
	assume	m:1		; assume 8-bit memory
				; print left of decimal
	ldy	#(bin_len*2)-1	; get first digit in array (little endian)
	lda	(ptr_0),y
	tax
	lda	bcdtbl,x	; bcd table lookup
	cmp	#10		; if less than 10 skip leading zero
	bcc	+
				; > 10 code here, going to skip for now
	jmp	++
/	ora	#$B0		;   convert single digit to ascii
	sep	#%00010000	; 8-bit index mode
	jsr	cout		
/	lda	#'.'+$80	; print decimal point (fixed point math)
	jsr	cout
				; IIgs cout sets 6502 emulation mode
	clc			; clear carry
	xce			; exchange emulation/carry flag
	rep	#%00010000	; 16-bit index mode

print_mp1:			; loop through rest of decimal digits
	ldy	#(bin_len*2)-1	; get first digit in array (little endian)
	sep	#%00100000	; 8-bit data mode
	assume	m:1		; assume 8-bit memory

	lda	#0
	sta	(ptr_0),y	; clear first digit
	sta	carry_mp	; clear multiprecision carry

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

m100	macro
	lda	(ptr_0),y	; get it
	tax
	lda	m100lo,x	; get product lo
	clc
	adc	carry_mp	; add carry_mp to it
	sta	(ptr_0),y	;   and save it
	lda	m100hi,x	; get product hi
	adc	#0		;   add carry if present
	sta	carry_mp	;   and save it
	endm

	ldy	#-1		; start from front
				; Duff's Device.   We know that (bin_len*2)%4 = 2
	bne	++		; zero flag unset from ldy
/	iny
	m100
	iny
	m100
/	iny
	m100
	iny
	m100
	cpy	#(bin_len*2)-1	; all done?
	bne	--

	lda	(ptr_0),y	; print array[0] (MSB)
	tax
	lda	bcdtbl,x	; table lookup

	sep	#%00010000	; 8-bit index mode
	jsr	prbyte
				; IIgs cout sets 6502 emulation mode
	clc			; clear carry
	xce			; exchange emulation/carry flag
	rep	#%00110000	; 16-bit data/index mode
	assume	m:0		; assume 16-bit memory
	dec	dlength		; check if dlength = 0
	bne	print_mp1
	rts			; dlength = 0, all done


print:	
	sta	ptr_0		; load A in ptr_0
	sep	#%00110000	; 8-bit data/index mode
	assume	m:1		; assume 8-bit memory
	assume	x:1		; assume 8-bit index
	ldy	#0		; start with first char
	lda	(ptr_0),y	; load initial char
/	ora	#$80		; we do not want flashing or inverse
				;  (its an Apple II thing)
	jsr	cout		; call apple II char out
	iny			; y++
	bne	+		; y rolled to 0?
	inc	ptr_0+1		;   then inc ptr_0+1 (hi)
/	lda	(ptr_0),y	; get next char
	bne	--		; not NULL? then print it
				; cout put us back in 6502 mode
	clc			; clear carry
	xce			; exchange emulation/carry flag
	rep	#%00110000	; 16-bit data/index mode
	rts			; all done, move alone


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

sqrtbl: fdb	$0000,$0001,$0004,$0009,$0010,$0019,$0024,$0031
	fdb	$0040,$0051,$0064,$0079,$0090,$00A9,$00C4,$00E1
	fdb	$0100,$0121,$0144,$0169,$0190,$01B9,$01E4,$0211
	fdb	$0240,$0271,$02A4,$02D9,$0310,$0349,$0384,$03C1
	fdb	$0400,$0441,$0484,$04C9,$0510,$0559,$05A4,$05F1
	fdb	$0640,$0691,$06E4,$0739,$0790,$07E9,$0844,$08A1
	fdb	$0900,$0961,$09C4,$0A29,$0A90,$0AF9,$0B64,$0BD1
	fdb	$0C40,$0CB1,$0D24,$0D99,$0E10,$0E89,$0F04,$0F81
	fdb	$1000,$1081,$1104,$1189,$1210,$1299,$1324,$13B1
	fdb	$1440,$14D1,$1564,$15F9,$1690,$1729,$17C4,$1861
	fdb	$1900,$19A1,$1A44,$1AE9,$1B90,$1C39,$1CE4,$1D91
	fdb	$1E40,$1EF1,$1FA4,$2059,$2110,$21C9,$2284,$2341
	fdb	$2400,$24C1,$2584,$2649,$2710,$27D9,$28A4,$2971
	fdb	$2A40,$2B11,$2BE4,$2CB9,$2D90,$2E69,$2F44,$3021
	fdb	$3100,$31E1,$32C4,$33A9,$3490,$3579,$3664,$3751
	fdb	$3840,$3931,$3A24,$3B19,$3C10,$3D09,$3E04,$3F01
	fdb	$4000,$4101,$4204,$4309,$4410,$4519,$4624,$4731
	fdb	$4840,$4951,$4A64,$4B79,$4C90,$4DA9,$4EC4,$4FE1
	fdb	$5100,$5221,$5344,$5469,$5590,$56B9,$57E4,$5911
	fdb	$5A40,$5B71,$5CA4,$5DD9,$5F10,$6049,$6184,$62C1
	fdb	$6400,$6541,$6684,$67C9,$6910,$6A59,$6BA4,$6CF1
	fdb	$6E40,$6F91,$70E4,$7239,$7390,$74E9,$7644,$77A1
	fdb	$7900,$7A61,$7BC4,$7D29,$7E90,$7FF9,$8164,$82D1
	fdb	$8440,$85B1,$8724,$8899,$8A10,$8B89,$8D04,$8E81
	fdb	$9000,$9181,$9304,$9489,$9610,$9799,$9924,$9AB1
	fdb	$9C40,$9DD1,$9F64,$A0F9,$A290,$A429,$A5C4,$A761
	fdb	$A900,$AAA1,$AC44,$ADE9,$AF90,$B139,$B2E4,$B491
	fdb	$B640,$B7F1,$B9A4,$BB59,$BD10,$BEC9,$C084,$C241
	fdb	$C400,$C5C1,$C784,$C949,$CB10,$CCD9,$CEA4,$D071
	fdb	$D240,$D411,$D5E4,$D7B9,$D990,$DB69,$DD44,$DF21
	fdb	$E100,$E2E1,$E4C4,$E6A9,$E890,$EA79,$EC64,$EE51
	fdb	$F040,$F231,$F424,$F619,$F810,$FA09,$FC04,$FE01

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

mp_a:	
	org	*+bin_len*2
mp_b:	
	org	*+bin_len*2
mp_x:	
	org	*+bin_len*2
mp_y:	
	org	*+bin_len*2

end:
