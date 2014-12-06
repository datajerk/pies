;; 8080 Pie, 8080-based Pi/e
;;
;; Mar 8, 2013
;;
;; (c) 2012, All Rights Reserved, Egan Ford (egan@sense.net)
;;
;; THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
;; KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
;; PARTICULAR PURPOSE.
;;
;; 8080 Pie will compute 1000 decimal digits of Pi or e.
;;
;; All 8080 Pie computation is base 256 and then converted to base 10 for
;; display (technically base 100, but they both display the same).  Arrays
;; of bytes are used to represent multiprecision numbers.  In the comments
;; below "array" refers to a big endian multiprecision number.
;;
;; Assembler: Macro Assembler AS V1.42
;;

				; Assembler: Macro Assembler AS V1.42
	cpu	8080		; Intel 8080

;; constants (R/O)

dec_len	=	1000		; 1000 decimal digits
bin_len	=	418		; ceil(1001 / log(256)) + 1 (+ 1 if odd) = 418
				;   Duff's Device 4x unrolled loops assume
				;   bin_len % 4 = 2
bin_end =	bin_len-1
				; the number of 4x unrolled loop is deterministic
				; unrollc = ceil(bin_len/4)
unrollc	=	bin_len/4+(bin_len#4>0)
				; counting up is easier
bcd_len	=	65536-(dec_len/2)

cpm	=	1

	if	cpm == 1
bdos	=	5
	else
dma_prb	=	0FFDCh
dma_ech	=	0FFEFh
	endif


;; start of global macros/functions

; Description:  Return hi 8-bits of a label
;               e.g.: hi(label)

hi	function x,(x>>8)&255


; Description:  Return lo 8-bits of a label
;               e.g.: lo(label)

lo	function x,x&255


; Description:  Print CR
; Calls:        echo
; Registers:    A

printcr	macro
	mvi	a,'\n'
	call	echo
	endm


; Description:  Set initial value left of decimal of arg1 to arg2
; Input:        arg1 (16-bit), arg2 (8-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate integer to be set left of decimal point
; Calls:        set_mp
; Registers:    A, B, C, D, E, H, L

mset	macro	arg1, arg2	; arg1 = arg2
	lxi	d,arg1		; DE = address of a
	mvi	a,arg2		; load A with initial value
	call	set_mp
	endm


; Description:  Convert and print a base 256 number (ptr_a) as base 10/100.
; Input:        arg1 (16-bit) immediate address at start of an array
; Calls:        print_mp
; Registers:    A, B, C, D, E, H, L

mprint	macro	arg1		; print (arg1) base 100
	lxi	h,arg1		; HL <- ptr
	call	print_mp
	endm


; Description:  Divide (arg1) by an 8-bit quantity (arg1 = arg1 / arg2)
; Input:        arg1 (16-bit), arg2 (8-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate address of divisor
; Calls:        div_mp
; Return:       If arg1 = 0 (array of zeros), then carry is set,
;               otherwise carry is clear.
; Registers:    A, B, C, D, E, H, L

mdiv	macro	arg1, arg2	; arg1 = arg1 / arg2
	lxi	h,arg1		; load HL ptr to array
	lxi	b,arg2		; load BC ptr to divisor
	call	div_mp
	endm


; Description:  Divide (arg1) by a 16-bit quantity (arg1 = arg1 / arg2)
; Input:        arg1 (16-bit), arg2 (16-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate address of divisor
; Calls:        div16_mp
; Return:       If a = 0 (array of zeros), then carry is set,
;               otherwise carry is clear.
; Registers:    A, B, C, D, E, H, L

mdiv16	macro	arg1, arg2	; arg1 = arg1 / arg2
	lxi	d,arg1		; load DE ptr to array
	lxi	b,arg2		; load BC ptr to divisor
	call	div16_mp
	endm


; Description:  Transfer array (arg2)[0,sizeof(arg2)] to array (arg1)
; Input:        arg1 (16-bit), arg2 (16-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate address at start of array
; Calls:        copy_mp
; Registers:    A, B, C, D, E, H, L

mcopy	macro	arg1, arg2	; arg1 = arg2
sarg1	set	"arg1"
        if	substr(sarg1,0,1) = "("
sarg1	set	substr(sarg1,1,strlen(sarg1)-2)
	lhld	val(sarg1)	; HL = (arg1)
	else
	lxi	h,arg1		; HL = arg1
	endif
	lxi	d,arg2		; DE = arg2
	call	copy_mp
	endm


; Description:  Left shift array (arg1)
; Input:        arg1 (16-bit)
;               arg1 = immediate address at start of array
; Calls:        asl_mp
; Registers:    A, D, E, H, L

masl	macro	arg1		; arg1 = arg1 * 2
	lxi	h,arg1		; HL = arg1
	call	asl_mp
	endm


; Description:  Add array arg2 to array arg1 (arg1 = arg1 + arg2)
; Input:        arg1 (16-bit), arg2 (16-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate address at start of array
; Calls:        add_mp
; Registers:    A, B, C, D, E, H, L

madd	macro	arg1, arg2	; arg1 = arg1 + arg2
sarg1	set	"arg1"
        if	substr(sarg1,0,1) = "("
sarg1	set	substr(sarg1,1,strlen(sarg1)-2)
	lhld	val(sarg1)	; HL = (arg1)
	else
	lxi	h,arg1		; HL = arg1
	endif
	lxi	d,arg2		; DE = arg2
	call	add_mp
	endm


; Description:  Subtract array b from array a (a = a - b)
; Input:        arg1 (16-bit), arg2 (16-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate address at start of array
; Calls:        sub_mp
; Registers:    A, B, C, D, E, H, L

msub	macro	arg1, arg2	; arg1 = arg1 - arg2
sarg1	set	"arg1"
        if	substr(sarg1,0,1) = "("
sarg1	set	substr(sarg1,1,strlen(sarg1)-2)
	lhld	val(sarg1)	; HL = (arg1)
	else
	lxi	h,arg1		; HL = arg1
	endif
	lxi	d,arg2		; DE = arg2
	call	sub_mp
	endm


; Description:  Compute arctan(1/arg2) and store at (arg1).
; Input:        arg1 (16-bit), arg2 (8-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate integer to be set left of decimal point
; Calls:        atan_mp
; Registers:    A, B, C, D, E, H, L

matan	macro	arg1, arg2	; arg1 = atan(1/arg2)
	lxi	h,arg1		; HL = arg1
	mvi	a,arg2		; A = arg2
	call	atan_mp
	endm


;; start of main code

	org	0100h
main:
	lxi	h,pitext
	call	print

	call	pi
	;call	e
	mprint	mp_a

	printcr
	if	cpm == 0
	hlt
	else
	ret
	endif


;; start of variables

pairs:
bcd_cnt:			; counter used for print_mp
	db	0, 0		; 0-1
stk_ptr:			; 16-bits used to backup stack ptr
beg_ptr:			; 16-bits used to store pointers to arrays
	db	0, 0		; 2-3
end_ptr:			; 16-bits used to store pointers to arrays
	db	0, 0		; 4-5
n:	db	0, 0		; 6-7, used by atan_mp
regx:	db	0		; B, the x in atan(1/x)
x2:	db	0		; C, cached x*x
ptr_a:	db	0, 0		; D-E, pointer to array used by atan_mp
				; table of squares
sqrtbl:	db	0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
	db	121, 144, 169, 196, 225
				; Change PI to E for, well, e
pitext:	db	"1000 DIGITS OF PI = \0"


;; start of subs

; Description:  Compute e using the infinite series:
;
;               oo
;               =====
;               \      1
;       e   =    >    --  = 1 + 1/1! + 1/2! + 1/3! + 1/4! + ...
;               /     k!
;               =====
;               k = 0
;
;               and save in array mp_a.
;
; Input:        None
;
; Output:       mp_a = e = 1 + 1/1! + 1/2! + 1/3! + 1/4! + ...
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        mset/set_mp, mdiv/div_mp, mdiv16/div16_mp, madd/add
;
; Globals:      n, mp_a, mp_x
;
; C Algorithm:
;
; int n = 1;
; setbig(A, 1, 0);
; setbig(X, 1, 0);
; while(divbig(X, n++))         // while(dividend != 0)
;     addbig(A, X);

e:	lxi	h,0		; n = 0 little endian
	shld	n		; n = HL
	mset	mp_a,1		; a = 1
	mset	mp_x,1		; x = 1

e_loop:
				; n = n + 1
	lhld	n		; HL = n
	inx	h		; n++
	shld	n		; n = HL
	lda	n+1		; if n < 256 then div else div16
	ana	a		; update zero flag
	jnz	+		; >= 256 use 16 bit div
	mdiv	mp_x,n		; < 256 use 8 bit div
	rc			; dividend = 0, done
	jmp	++		; still working on it
/	mdiv16	mp_x,n		; div16
	rc			; dividend = 0, done
/	madd	mp_a,mp_x	; a = a + x
	jmp	e_loop


; Description:  Compute pi using the Gregory expansion of Machin's arctan
;               formula and save in array mp_a.
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
; Input:        None
;
; Output:       mp_a = pi = 4 * (4 * atan(1/5) - atan(1/239))
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        matan/atan_mp, masl/asl_mp, msub/sub_mp

pi:	matan	mp_a,5		; a = atan(1/5)
	masl	mp_a		; a = a * 4
	masl	mp_a
	matan	mp_b,239	; b = atan(1/239)
	msub	mp_a,mp_b	; a = a - b
	masl	mp_a		; a = a * 4
	masl	mp_a
	ret


; Description:  Compute arctan(1/N) using the Gregory expansion of Machin's
;               arctan formula and save in array (ptr_a).
;
;
;                               / 1     1      1       \
;               arctan(1/N) =  |  - - ---- + ---- - ... |
;                              |  N      3      5       |
;                               \     3xN    5xN       /
;
;
; Input:        H/L (hi/lo) pointer to array
;               B = N (8 bit)
;
; Output:       H/L (hi/lo) pointer to array = arctan(1/N)
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        mset/set_mp, mdiv/div_mp, madd/add_mp, msub/sub_mp,
;               mdiv16/div16_mp, mcopy/copy_mp
;
; Globals:      mp_x (16-bit), mp_y (16-bit)
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

atan_mp:
	shld	ptr_a		; ptr_a to point to array
	sta	regx		; save A

	cpi	16		; if A < 16 then get square
	jnc	+
	lxi	h,sqrtbl	; HL points to sqrtbl
	add	l		; A += L
	mov	l,a		; L <- A
	mov	a,m		; load A with square
/	sta	x2		; x2 = A

	lxi	h,1		; n = 1 little endian
	shld	n

	mset	mp_x,1		; x = 1
	mdiv	mp_x,regx	; x /= regx
	mcopy	(ptr_a),mp_x	; a = x

atan_mp_loop:			; main loop
				; n = n + 2
	lhld	n		; HL = n
	inx	h		; n++, n is even now
	inr	l		; n++, n is odd now
	shld	n		; n = HL

	lda	regx		; A <- regx
	cpi	16		; if > 16 /x/x othersize /x2
	jc	+		; x already x*x, one div required
	mdiv	mp_x,x2		; x >= 16, then do two div
/	mdiv	mp_x,x2		;   (faster than 16 bit div)

	mcopy	mp_y,mp_x	; y = x

	lda	n+1		; if n < 256 then div else div16
	ana	a		; update zero flag
	jnz	+		; >= 256 use 16 bit div
	mdiv	mp_y,n		; < 256 use 8 bit div
	rc			; dividend = 0, done
	jmp	++		; still working on it
/	mdiv16	mp_y,n		; div16
	rc			; dividend = 0, done

/	lda	n		; add or sub?
	ani	2		; odd/even check on 2nd bit
	jz	+		; add it
	msub	(ptr_a),mp_y	; a = a - y
	jmp	atan_mp_loop	; back to top
/	madd	(ptr_a),mp_y	; a = a + y
	jmp	atan_mp_loop	; back to top


; Description:  Set mp array initial value:  a = b
;
; Input:        H/L (hi/lo) address to array (a)
;               B = value left of decimal
;
; Output:       a[0] = B
;               a[1] - a[length-1] = 0
;
; Registers:    A, B, D, E

set_mp:
	stax	d		; set inital value, rest of array will be zero
	mvi	b,unrollc	; count down to 0
	xra	a		; a = 0
	jmp	++		; Duff's Device hardcoded for fixed array length
				;   bin_len % 4 = 2
/	inr	e		; E += 1
	stax	d		; (DE) = 0
	inr	e		; E += 1
	stax	d		; (DE) = 0
	inx	d		; DE += 1
	stax	d		; (DE) = 0
/	inr	e		; E += 1
	stax	d		; (DE) = 0
	dcr	b		; B--
	jnz	--		; B=0?
	ret


; Description:  Multiprecision subtraction:  a = a - b
;
; Input:        H/L (hi/lo) address to array (a)
;               B/C (hi/lo) address to array (b)
;
; Output:       a = a - b
;
; Registers:    A, B, C, D, E, H, L

sub_mp:
	lxi	b,bin_len	; BC = bin_len
	dad	b		; HL(DE) += BC
	xchg			; HL <-> DE
	dad	b		; HL += BC
	mvi	b,unrollc	; count down to 0
	ora	a		; clear carry
	jmp	++		; Duff's Device hardcoded for fixed array length
/	dcx	d		; DE--
	ldax	d		; A <- (DE)
	dcx	h		; HL--
	sbb	m		; A - (HL) - carry
	stax	d		; (DE) <- A
	dcr	e		; E--
	ldax	d		; A <- (DE)
	dcr	l		; L--
	sbb	m		; A - (HL) - carry
	stax	d		; (DE) <- A
/	dcr	e		; E--
	ldax	d		; A <- (DE)
	dcr	l		; L--
	sbb	m		; A - (HL) - carry
	stax	d		; (DE) <- A
	dcr	e		; E--
	ldax	d		; A <- (DE)
	dcr	l		; L--
	sbb	m		; A - (HL) - carry
	stax	d		; (DE) <- A
	dcr	b		; B--
	jnz	--		; B=0?
	ret


; Description:  Multiprecision addition:  a = a + b
;
; Input:        B/C (hi/lo) address to array (a)
;               H/L (hi/lo) address to array (b)
;
; Output:       a = a + b
;
; Registers:    A, B, C, D, E, H, L

add_mp:
	lxi	b,bin_len	; BC = bin_len
	dad	b		; HL(DE) += BC
	xchg			; HL <-> DE
	dad	b		; HL += BC
	mvi	b,unrollc	; count down to 0
	ora	a		; clear carry
	jmp	++		; Duff's Device hardcoded for fixed array length
/	dcx	d		; DE--
	ldax	d		; A <- (DE)
	dcx	h		; HL--
	adc	m		; A + (HL) + carry
	stax	d		; (DE) <- A
	dcr	e		; E--
	ldax	d		; A <- (DE)
	dcr	l		; L--
	adc	m		; A + (HL) + carry
	stax	d		; (DE) <- A
/	dcr	e		; E--
	ldax	d		; A <- (DE)
	dcr	l		; L--
	adc	m		; A + (HL) + carry
	stax	d		; (DE) <- A
	dcr	e		; E--
	ldax	d		; A <- (DE)
	dcr	l		; L--
	adc	m		; A + (HL) + carry
	stax	d		; (DE) <- A
	dcr	b		; B--
	jnz	--		; B=0?
	ret


; Description:  Multiprecision left shift:  a = a * 2
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       a = a * 2
;
; Registers:    A, D, E, H, L

asl_mp:
	lxi	d,bin_len	; DE = bin_len
	dad	d		; HL += DE (end address)
	mvi	b,unrollc	; count down to 0
	ora	a		; clear carry
	jmp	++		; Duff's Device hardcoded for fixed array length
/	dcx	h		; HL--
	mov	a,m		; A <- (HL)
	ral			; rol
	mov	m,a		; (HL) <- A
	dcr	l		; L--
	mov	a,m		; A <- (HL)
	ral			; rol
	mov	m,a		; (HL) <- A
/	dcr	l		; L--
	mov	a,m		; A <- (HL)
	ral			; rol
	mov	m,a		; (HL) <- A
	dcr	l		; L--
	mov	a,m		; A <- (HL)
	ral			; rol
	mov	m,a		; (HL) <- A
	dcr	b		; B--
	jnz	--		; B=0?
	ret


; Description:  Multiprecision transfer:  a = b
;
; Input:        H/L (hi/lo) address to array (a)
;               D/E (hi/lo) address to array (b)
;
; Output:       a = b
;
; Registers:    A, B, D, E, H, L

copy_mp:
	mvi	b,unrollc	; count down to 0
	jmp	++		; Duff's Device hardcoded for fixed array length
				;   bin_len % 4 = 2
/	ldax	d		; A <- (DE)
	mov	m,a		; (HL) <- A
	inr	e		; E++
	inr	l		; L++
	ldax	d		; A <- (DE)
	mov	m,a		; (HL) <- A
	inx	d		; DE++
	inx	h		; HL++
/	ldax	d		; A <- (DE)
	mov	m,a		; (HL) <- A
	inr	e		; E++
	inr	l		; L++
	ldax	d		; A <- (DE)
	mov	m,a		; (HL) <- A
	inr	e		; E++
	inr	l		; L++
	dcr	b		; B--
	jnz	--		; B=0?
	ret


; Description:  Skip leading zeros (used by div_mp and div16_mp)
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       None
;
; Registers:    A, D, E, H, L
;
; Globals:      end_ptr

skipzeros16:
				; DE points to array
	lxi	h,bin_len	; HL = bin_len
	dad	d		; HL += DE
	shld	end_ptr		; store HL to end_ptr
	xchg			; DE <-> HL
				; HL points to array
				; DE = end of array
skipzeros:
	mvi	e,bin_len/2	; number of paris to test
	xra	a		; a = 0
	jmp	++		; Duff's Device hardcoded for fixed array length
				;   bin_len % 4 = 2
/	cmp	m
	jnz	even
	inr	l		; L += 1
	cmp	m
	jnz	odd
	inx	h		; HL += 1
	dcr	e		; E--
/	cmp	m
	jnz	even
	inr	l		; L += 1
	cmp	m
	jnz	odd
	inr	l		; L += 1
	dcr	e		; E--
	jnz	--		; E=0?
	stc			; set carry
	ret			;   all zeros, return
odd:	dcx	h		; HL--
even:	ora	a		; clear carry
	ret


; Description:  Multiprecision 16-bit division:  a = a / b
;
; Input:        H/L (hi/lo) address to array (a)
;               B/C (hi/lo) address to divisor (b)
;
; Output:       a = a / b
;
; Registers:    A, B, C, D, E, H, L, SP
;
; Globals:      end_ptr, stk_ptr
;
; Calls:        skipzeros
;
; C Algorithm:  see div_mp

div16	macro {noexpand}	; Input:  BCLH = dividend+0-3, DE = divisor
	rept	16		; do it, 16 times
	mov	a,h		; A <- H dividend+3
	add	a		; asl (A = A + A)
	mov	h,a		; H <- A dividend+3
	mov	a,l		; A <- L dividend+2
	ral			; rol
	mov	l,a		; L <- A dividend+2
	mov	a,c		; A <- C dividend+1
	ral			; rol
	mov	c,a		; C <- A dividend+1
	mov	a,b		; A <- B dividend+0
	ral			; rol
	mov	b,a		; B <- A dividend+0
				; trial division, nothing changed
	mov	a,c		; A <- C
	sub	e		; C = C - E (dividend+1 - divisor(lo))
	mov	a,b		; A <- B
	sbb	d		; B = B - D (dividend+0 - divisor(hi)) - carry
	jc	m0		; too small
				; do it again, but this time save it
	mov	b,a		; B <- A, (hi) done, just save it
	mov	a,c		; A <- C, (lo)
	sub	e		; C = C - E (dividend+1 - divisor(lo))
	mov	c,a		; C <- A
	inr	h		; H++, dividend+3
m0:
	endm			; end 32/16 division
	endm

div16_mp:
	call	skipzeros16	; skip leading zeros for speed
	rc			; return if carry set (all zeros)

				; HL pointing to array MS 16-bit Digit
				; A, D, E disposable
				; BC has pointer to divisor

	mov	a,e		; backup E (number of remaining pairs) to pairs
	sta	pairs

	xchg			; backup HL to DE
	di			; disable interrupts
				; backup stack ptr
	lxi	h,0		; HL = 0
	dad	sp		; HL += SP
	shld	stk_ptr		; store SP to stk_ptr
	xchg			; restore HL from DE
	sphl			; store HL to stack pointer
				; load divisor in DE
	ldax	b		; A <- (BC)
	mov	e,a		; E <- A lo(BC)
	inx	b		; BC++
	ldax	b		; A <- (BC)
	mov	d,a		; D <- A hi(BC)

	mvi	b,0		; set carry/remainder to 0, dividend+0
	mvi	c,0		; set carry/remainder to 0, dividend+1

				; SP pointing to array MS 16-bit digit
				; BC to be 16-bit carry/remainder/dividend
				; DE to be divisor
				; BCDE off limits, need for chain
				; BCLH = dividend+0-3
				; A free

				; Duff's Device hardcoded for fixed array length
	lda	pairs		; restore A from pairs
	ani	3		; mask off last 2 bits, clear carry
	jz	+		; is /4, i.e. do 4, 8, 12, ... loops
	rar			; roll right through carry, only updates carry
	inr	a		; trick used to update zero flag and preserve carry
	dcr	a
	jz	d1		; bit 1 must have been set to get here, now in carry
				; do 1, 5, 9, ... loops
	jnc	d2		; bits 10, do 2, 6, 10, ... loops
	jmp	d3		; bits 11, do 3 ,7, 11, ... loops


/	pop	h		; load 16-bit pair into HL
	div16			; 32/16 division
	push	h		; store 16-bit LSB pair HL
	pop	h		; SP += 2
d3:	pop	h		; load 16-bit pair into HL
	div16			; 32/16 division
	push	h		; store 16-bit LSB pair HL
	pop	h		; SP += 2
d2:	pop	h		; load 16-bit pair into HL
	div16			; 32/16 division
	push	h		; store 16-bit LSB pair HL
	pop	h		; SP += 2
d1:	pop	h		; load 16-bit pair into HL
	div16			; 32/16 division
	push	h		; store 16-bit LSB pair HL
	pop	h		; SP += 2

				; check SP = end_ptr, load SP in HL
	lxi	h,0		; HL = 0
	dad	sp		; HL += SP
				; compare HL to end_ptr
	lda	end_ptr		; A <- lo(end_ptr)
	cmp	l		; A = L?
	jnz	-
	lda	end_ptr+1	; A <- hi(end_ptr)
	cmp	h		; A = H?
	jnz	-
				; restore stack ptr
	lhld	stk_ptr		; HL = (stk_ptr)
	sphl			; SP = HL
	ei			; enable interrupts
	ora	a		; clear carry
	ret


; Description:  Multiprecision 8-bit division:  a = a / b
;
; Input:        H/L (hi/lo) address to array (a)
;               B/C (hi/lo) address to divisor (b)
;
; Output:       a = a / b
;
; Registers:    A, B, C, D, E, H, L
;
; Globals:      end_ptr
;
; Calls:        skipzeros, div8(macro)
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

div8	macro {noexpand}	; Input:  B = lo, C = hi, D = divisor
				; Output: B = lo, C = hi
	mov	b,m		; B <- M, dividend lo
	rept	8		; do it 8 times
	mov	a,b		; A <- B
	add	a		; asl (A = A + A)
	mov	b,a		; B <- A, dividend lo
	mov	a,c		; A <- C, dividend hi
	ral			; rol A (hi)
	jc	m0		; 9th bit hi?  Sub it.
	cmp	d		; A - D, if A < D set carry
	jc	m1		; too small
m0:	sub	d		; A = A - D
	inr	b		; B++
m1:	mov	c,a		; C <- A
	endm
	mov	m,b		; M <- B save it
	endm

div_mp:
	call	skipzeros	; skip leading zeros for speed
	rc			; return if carry set (all zeros)
				; BC points to divisor
	ldax	b		; A = lo(divisor)
	mov	d,a		; D = divisor
	mvi	c,0		; set carry to 0
				; we know HL is even and bin_len is also even
	mov	a,e		; number of pairs left A <- E
	ani	1		; odd or even number of pairs left
	jnz	++		; odd pairs left
/	div8			; div M / D
	inr	l		; L++
	div8			; div M / D
	inx	h		; HL++
	dcr	e		; E--
/	div8			; div M / D
	inr	l		; L++
	div8			; div M / D
	inr	l		; L++
	dcr	e		; E--
	jnz	--		; E=0?
	ora	a		; clear carry
	ret


; Description:  Print an mp array base 10/100
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       (a) base 10/100 out to screen
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        bintobcd, prbcd, mult100(macro)
;
; Globals:      beg_ptr, end_ptr, bcd_cnt, mt100_lo, mt100_hi


mult100	macro
	mov	a,m		; get A (page aligned table offset)
	mvi	b,hi(mt100_lo)	; BC to point to low table
	mov	c,a		; C = A (position of A*100)
	ldax	b		; A <- (BC), LSB A = A*100
	add	e		; LSB A = A + E, add mp carry
	mov	m,a		; M <- A, store LSB
	inr	b		; B++ to get MSB from table
	ldax	b		; A <- (BC), MSB A = A*100
	aci	0		; add carry
	mov	e,a		; E <- A, store mp carry
	endm

print_mp:
				; HL now set to MSB
	mov	a,m		; load A with MSB
	cpi	10		; is < 10, then use ASCII method
	jc	+		;   otherwise, use 2 digit BCD method
	call	bintobcd	; convert to BCD
	call	prbcd		; print it
	jmp	++		; skip over ASCII method
/	adi	030h		; make ASCII
	call	echo		; print it
/	mvi	a,'.'		; print decimal point
	call	echo		; print it
	mvi	m,0		; zero MSB
				; compute end of array since we have
				;   to go backwards
	lxi	b,bin_len	; BC = bin_len
	dad	b		; HL += BC
	shld	end_ptr		; save HL value to location end_ptr
				; setup bcd counter for outer loop
	lxi	h,bcd_len	; load HL with bcd_cnt
	shld	bcd_cnt		; save HL value to location bcd_cnt

print_mp1:			; main loop
				; multi array x 100
				; loop from LSB to MSB
				; 16-bit product = array[i] * 100 + carry_mp;
				; array[i] = product lo
				; carry_mp = product hi
				;
				; C Algorithm:
				;
				; while (j >= 0) {
				;    result.L = (long) number[j] * 100 + carry;
				;    number[j] = result.w.lo;
				;    carry = result.w.hi;
				;    j--;
				; }

	lhld	end_ptr		; load HL value from location end_ptr
				; backwards loop (LSB to MSB)
	mvi	e,0		; E = carry = 0
				; E is the carry from mult100 to mult100
				; A,B,C,E (carry) used for mult100
				; D and E must not be touched!
	mvi	d,unrollc	; count down to 0
	jmp	++		; mp_array * 100 loop
/	dcx	h		; HL--
	mult100			; M = lo(M*100), E = hi(M*100)
	dcr	l		; L--
	mult100			; M = lo(M*100), E = hi(M*100)
/	dcr	l		; L--
	mult100			; M = lo(M*100), E = hi(M*100)
	dcr	l		; HL--
	mult100			; M = lo(M*100), E = hi(M*100)
	dcr	d		; D--
	jnz	--		; D=0?
				; HL now set to MSB
	mov	a,m		; load A with MSB
	call	bintobcd	; convert to BCD
	call	prbcd		; print base 100 digit
	mvi	m,0		; zero MSB

				; check for all decimal digits printed
	lhld	bcd_cnt		; load HL with bcd_cnt value
				;   works with bcd_len = 65536-(dec_len/2)
				;   counting up is easier
	inx	h		; HL++
	shld	bcd_cnt		; store HL into bcd_cnt
	mov	a,l		; A <- L, check lo
	ora	h		; or with hi
	jnz	print_mp1	; if not zero keep going, otherwise
	ret
	

; Description:  Output BCD number to display.
;
; Input:        A = BCD number (00 - 99)
;
; Output:       BCD number to display
;
; Registers:    A, B

prbcd:	mov	b,a		; B <- A, backup A
	rrc			; shift right 4 bits
	rrc
	rrc
	rrc
	call	+		; do MSB
	mov	a,b		; then LSB
/	ani	00Fh		; mask off
	ori	030h		; make a number
	call	echo		; print it
	ret


; Description:  Convert BIN/HEX to BCD.
;
; Input:        A = BIN/HEX (00h - 63h)
;
; Output:       A = BCD (00 - 99)
;
; Registers:    A, B, C

bintobcd_o:
	mov	b,a		; B <- A, backup A
	mvi	c,-1		; start counter at -1
/	inr	c		; C=C+1
	sui	10		; A=A-10
	jp	-		; loop if positive
	mov	a,b		; A <- B, restore A
	jmp	++		; jump to dcc
/	adi	6		; A=A+6
/	dcr	c		; if a < 10, then c will be -1 and end loop
	jp	--		;  othersize add 6 for every 10
	ret


bintobcd:
	mov	e,a
	mvi	d,hi(bcdtable)	; BC to point to low table
	ldax	d
	ret


; Description:  Output a string.
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       String to screen
;
; Registers:    A, H, L

print:	mov	a,m		; A <- M (HL)
	ana	a		; update zero flag
	rz			; return if zero
	call	echo		; print char
	inx	h		; HL++
	jmp	print		; back to top


echo:
	if	cpm == 0
	sta	dma_ech
	else
	push    h
	push    d
	push    b
	mov     e,a             ; E <- A
	mvi     c,2             ; Function 2
	call    bdos            ; BDOS
	pop     b
	pop     d
	pop     h
	endif
	ret


;; tables for faster math must be page aligned

	rept	256-$&0FFh
	db	0
	endm

mt100_lo:
	db	000h,064h,0C8h,02Ch,090h,0F4h,058h,0BCh,020h,084h,0E8h,04Ch,0B0h,014h,078h,0DCh
	db	040h,0A4h,008h,06Ch,0D0h,034h,098h,0FCh,060h,0C4h,028h,08Ch,0F0h,054h,0B8h,01Ch
	db	080h,0E4h,048h,0ACh,010h,074h,0D8h,03Ch,0A0h,004h,068h,0CCh,030h,094h,0F8h,05Ch
	db	0C0h,024h,088h,0ECh,050h,0B4h,018h,07Ch,0E0h,044h,0A8h,00Ch,070h,0D4h,038h,09Ch
	db	000h,064h,0C8h,02Ch,090h,0F4h,058h,0BCh,020h,084h,0E8h,04Ch,0B0h,014h,078h,0DCh
	db	040h,0A4h,008h,06Ch,0D0h,034h,098h,0FCh,060h,0C4h,028h,08Ch,0F0h,054h,0B8h,01Ch
	db	080h,0E4h,048h,0ACh,010h,074h,0D8h,03Ch,0A0h,004h,068h,0CCh,030h,094h,0F8h,05Ch
	db	0C0h,024h,088h,0ECh,050h,0B4h,018h,07Ch,0E0h,044h,0A8h,00Ch,070h,0D4h,038h,09Ch
	db	000h,064h,0C8h,02Ch,090h,0F4h,058h,0BCh,020h,084h,0E8h,04Ch,0B0h,014h,078h,0DCh
	db	040h,0A4h,008h,06Ch,0D0h,034h,098h,0FCh,060h,0C4h,028h,08Ch,0F0h,054h,0B8h,01Ch
	db	080h,0E4h,048h,0ACh,010h,074h,0D8h,03Ch,0A0h,004h,068h,0CCh,030h,094h,0F8h,05Ch
	db	0C0h,024h,088h,0ECh,050h,0B4h,018h,07Ch,0E0h,044h,0A8h,00Ch,070h,0D4h,038h,09Ch
	db	000h,064h,0C8h,02Ch,090h,0F4h,058h,0BCh,020h,084h,0E8h,04Ch,0B0h,014h,078h,0DCh
	db	040h,0A4h,008h,06Ch,0D0h,034h,098h,0FCh,060h,0C4h,028h,08Ch,0F0h,054h,0B8h,01Ch
	db	080h,0E4h,048h,0ACh,010h,074h,0D8h,03Ch,0A0h,004h,068h,0CCh,030h,094h,0F8h,05Ch
	db	0C0h,024h,088h,0ECh,050h,0B4h,018h,07Ch,0E0h,044h,0A8h,00Ch,070h,0D4h,038h,09Ch

mt100_hi:
	db	000h,000h,000h,001h,001h,001h,002h,002h,003h,003h,003h,004h,004h,005h,005h,005h
	db	006h,006h,007h,007h,007h,008h,008h,008h,009h,009h,00Ah,00Ah,00Ah,00Bh,00Bh,00Ch
	db	00Ch,00Ch,00Dh,00Dh,00Eh,00Eh,00Eh,00Fh,00Fh,010h,010h,010h,011h,011h,011h,012h
	db	012h,013h,013h,013h,014h,014h,015h,015h,015h,016h,016h,017h,017h,017h,018h,018h
	db	019h,019h,019h,01Ah,01Ah,01Ah,01Bh,01Bh,01Ch,01Ch,01Ch,01Dh,01Dh,01Eh,01Eh,01Eh
	db	01Fh,01Fh,020h,020h,020h,021h,021h,021h,022h,022h,023h,023h,023h,024h,024h,025h
	db	025h,025h,026h,026h,027h,027h,027h,028h,028h,029h,029h,029h,02Ah,02Ah,02Ah,02Bh
	db	02Bh,02Ch,02Ch,02Ch,02Dh,02Dh,02Eh,02Eh,02Eh,02Fh,02Fh,030h,030h,030h,031h,031h
	db	032h,032h,032h,033h,033h,033h,034h,034h,035h,035h,035h,036h,036h,037h,037h,037h
	db	038h,038h,039h,039h,039h,03Ah,03Ah,03Ah,03Bh,03Bh,03Ch,03Ch,03Ch,03Dh,03Dh,03Eh
	db	03Eh,03Eh,03Fh,03Fh,040h,040h,040h,041h,041h,042h,042h,042h,043h,043h,043h,044h
	db	044h,045h,045h,045h,046h,046h,047h,047h,047h,048h,048h,049h,049h,049h,04Ah,04Ah
	db	04Bh,04Bh,04Bh,04Ch,04Ch,04Ch,04Dh,04Dh,04Eh,04Eh,04Eh,04Fh,04Fh,050h,050h,050h
	db	051h,051h,052h,052h,052h,053h,053h,053h,054h,054h,055h,055h,055h,056h,056h,057h
	db	057h,057h,058h,058h,059h,059h,059h,05Ah,05Ah,05Bh,05Bh,05Bh,05Ch,05Ch,05Ch,05Dh
	db	05Dh,05Eh,05Eh,05Eh,05Fh,05Fh,060h,060h,060h,061h,061h,062h,062h,062h,063h,063h

bcdtable:
	db	000h,001h,002h,003h,004h,005h,006h,007h,008h,009h
	db	010h,011h,012h,013h,014h,015h,016h,017h,018h,019h
	db	020h,021h,022h,023h,024h,025h,026h,027h,028h,029h
	db	030h,031h,032h,033h,034h,035h,036h,037h,038h,039h
	db	040h,041h,042h,043h,044h,045h,046h,047h,048h,049h
	db	050h,051h,052h,053h,054h,055h,056h,057h,058h,059h
	db	060h,061h,062h,063h,064h,065h,066h,067h,068h,069h
	db	070h,071h,072h,073h,074h,075h,076h,077h,078h,079h
	db	080h,081h,082h,083h,084h,085h,086h,087h,088h,089h
	db	090h,091h,092h,093h,094h,095h,096h,097h,098h,099h


;; Memory allocation for mp arrays:

	org	$+(4-$&3)	; force arrays on /4 boundary
mp_a:	org	$+bin_len
	org	$+(4-$&3)	; force arrays on /4 boundary
mp_b:	org	$+bin_len
	org	$+(4-$&3)	; force arrays on /4 boundary
mp_x:	org	$+bin_len
	org	$+(4-$&3)	; force arrays on /4 boundary
mp_y:	org	$+bin_len

