;; z80 Pie, z80-based Pi/e
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
;; z80 Pie will compute 1000 decimal digits of Pi or e.
;;
;; All z80 Pie computation is base 256 and then converted to base 10 for
;; display (technically base 100, but they both display the same).  Arrays
;; of bytes are used to represent multiprecision numbers.  In the comments
;; below "array" refers to a big endian multiprecision number.
;;
;; Assembler: Macro Assembler AS V1.42
;;


				; Assembler: Macro Assembler AS V1.42
	cpu	z80		; Intel z80

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

cpm	=	0

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
	ld	a,'\n'
	call	echo
	endm


; Description:  Set initial value left of decimal of arg1 to arg2
; Input:        arg1 (16-bit), arg2 (8-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate integer to be set left of decimal point
; Calls:        set_mp
; Registers:    A, B, C, D, E, H, L

mset	macro	arg1, arg2	; arg1 = arg2
	ld	de,arg1		; DE = address of a
	ld	a,arg2		; load A with initial value
	call	set_mp
	endm


; Description:  Convert and print a base 256 number (ptr_a) as base 10/100.
; Input:        arg1 (16-bit) immediate address at start of an array
; Calls:        print_mp
; Registers:    A, B, C, D, E, H, L

mprint	macro	arg1		; print (arg1) base 100
	ld	hl,arg1		; HL <- ptr
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
	ld	hl,arg1		; load DE ptr to array
	ld	bc,(arg2)	; load BC ptr to divisor
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
	ld	de,arg1		; load DE ptr to array
	ld	bc,(arg2)	; load BC ptr to divisor
	call	div16_mp
	endm


; Description:  Transfer array (arg2)[0,sizeof(arg2)] to array (arg1)
; Input:        arg1 (16-bit), arg2 (16-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate address at start of array
; Calls:        copy_mp
; Registers:    A, B, C, D, E, H, L

mcopy	macro	arg1, arg2	; arg1 = arg2
	ld	de,arg1
	ld	hl,arg2
	ld	bc,bin_len
	;ldir
	jp	++		; faster than 'ldir'
/	ldi
	ldi
/	ldi
	ldi
	jp	pe,--
	endm


; Description:  Left shift array (arg1)
; Input:        arg1 (16-bit)
;               arg1 = immediate address at start of array
; Calls:        asl_mp
; Registers:    A, D, E, H, L

masl	macro	arg1		; arg1 = arg1 * 2
	ld	hl,arg1		; HL = arg1
	call	asl_mp
	endm


; Description:  Add array arg2 to array arg1 (arg1 = arg1 + arg2)
; Input:        arg1 (16-bit), arg2 (16-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate address at start of array
; Calls:        add_mp
; Registers:    A, B, C, D, E, H, L

madd	macro	arg1, arg2	; arg1 = arg1 + arg2
	ld	hl,arg1		; HL = arg1
	ld	de,arg2		; DE = arg2
	call	add_mp
	endm


; Description:  Subtract array b from array a (a = a - b)
; Input:        arg1 (16-bit), arg2 (16-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate address at start of array
; Calls:        sub_mp
; Registers:    A, B, C, D, E, H, L

msub	macro	arg1, arg2	; arg1 = arg1 - arg2
	ld	hl,arg1		; HL = arg1
	ld	de,arg2		; DE = arg1
	call	sub_mp
	endm


; Description:  Compute arctan(1/arg2) and store at (arg1).
; Input:        arg1 (16-bit), arg2 (8-bit)
;               arg1 = immediate address at start of array
;               arg2 = immediate integer to be set left of decimal point
; Calls:        atan_mp
; Registers:    A, B, C, D, E, H, L

matan	macro	arg1, arg2	; arg1 = atan(1/arg2)
	ld	hl,arg1		; HL = arg1
	ld	a,arg2		; A = arg2
	call	atan_mp
	endm


;; start of main code

	org	0100h
main:
	ld	hl,pitext
	call	print

	call	pi
	;call	e
	mprint	mp_a

	printcr

	if cpm == 0
	halt
	else
	ret
	endif


;; start of variables

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

e:	ld	hl,0		; n = 0 little endian
	ld	(n),hl		; n = HL
	mset	mp_a,1		; a = 1
	mset	mp_x,1		; x = 1

	ld	ix,atan_mp_loop	; ix to point to top of loop
				; (2 cycles faster for jp)
e_loop:
				; n = n + 1
	ld	hl,(n)		; HL = n
	inc	hl		; n++
	ld	(n),hl		; n = HL
	ld	a,(n+1)		; if n < 256 then div else div16
	and	a		; update zero flag
	jp	nz,+		; >= 256 use 16 bit div
	mdiv	mp_x,n		; < 256 use 8 bit div
	ret	c		; dividend = 0, done
	jp	++		; still working on it
/	mdiv16	mp_x,n		; div16
	ret	c		; dividend = 0, done
/	madd	mp_a,mp_x	; a = a + x
	jp	(ix)


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
;               mdiv16/div16_mp, mcopy(m)/copy_mp
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
	ld	(ptr_a),hl	; ptr_a to point to array
	ld	(regx),a	; save A

	cp	16		; if A < 16 then get square
	jp	nc,+
	ld	hl,sqrtbl	; HL points to sqrtbl
	add	a,l		; A += L
	ld	l,a		; L <- A
	ld	a,(hl)		; load A with square
/	ld	(x2),a		; x2 = A

	ld	hl,1		; n = 1 little endian
	ld	(n),hl

	mset	mp_x,1		; x = 1
	mdiv	mp_x,regx	; x /= regx
	mcopy	(ptr_a),mp_x	; a = x

	ld	ix,atan_mp_loop	; ix to point to top of loop
				; (2 cycles faster for jp)
atan_mp_loop:			; main loop
				; n = n + 2
	ld	hl,(n)		; HL = n
	inc	hl		; n++, hl now even
	inc	l		; n++, can just inc l now
	ld	(n),hl		; n = HL

	ld	a,(regx)	; A <- regx
	cp	16		; if > 16 /x/x othersize /x2
	jp	c,+		; x already x*x, one div required
	mdiv	mp_x,x2		; x >= 16, then do two div
/	mdiv	mp_x,x2		;   (faster than 16 bit div)

	mcopy	mp_y,mp_x	; y = x

	ld	a,(n+1)		; if n < 256 then div else div16
	and	a		; update zero flag
	jp	nz,+		; >= 256 use 16 bit div
	mdiv	mp_y,n		; < 256 use 8 bit div
	ret	c		; dividend = 0, done
	jp	++		; still working on it
/	mdiv16	mp_y,n		; div16
	ret	c		; dividend = 0, done

/	ld	a,(n)		; add or sub?
	and	a,2		; odd/even check on 2nd bit
	jr	z,+		; add it
	msub	(ptr_a),mp_y	; a = a - y
	jp	(ix)		; back to top
/	madd	(ptr_a),mp_y	; a = a + y
	jp	(ix)		; back to top


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
	ld	(de),a		; set inital value, rest of array will be zero
	ld	b,unrollc	; count down to 0
	xor	a		; a = 0
	jp	++		; Duff's Device hardcoded for fixed array length
				;   bin_len % 4 = 2
/	inc	e		; E += 1
	ld	(de),a		; (DE) = 0
	inc	e		; E += 1
	ld	(de),a		; (DE) = 0
	inc	de		; DE += 1
	ld	(de),a		; (DE) = 0
/	inc	e		; E += 1
	ld	(de),a		; (DE) = 0
	djnz	--		; B--, B=0?
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
	ld	bc,bin_len	; BC = array length
	add	hl,bc		; DE += BC
	ex	de,hl
	add	hl,bc		; HL += BC
	ld	b,unrollc	; count down to 0
	or	a		; clear carry
	jp	++		; Duff's Device hardcoded for fixed array length
/	dec	de		; DE--
	ld	a,(de)		; A <- (DE)
	dec	hl		; HL--
	sbc	a,(hl)		; A - (HL) - carry
	ld	(de),a		; (DE) <- A
	dec	e		; E--
	ld	a,(de)		; A <- (DE)
	dec	l		; L--
	sbc	a,(hl)		; A - (HL) - carry
	ld	(de),a		; (DE) <- A
/	dec	e		; E--
	ld	a,(de)		; A <- (DE)
	dec	l		; L--
	sbc	a,(hl)		; A - (HL) - carry
	ld	(de),a		; (DE) <- A
	dec	e		; E--
	ld	a,(de)		; A <- (DE)
	dec	l		; L--
	sbc	a,(hl)		; A - (HL) - carry
	ld	(de),a		; (DE) <- A
	djnz	--		; B--, B=0?
	ret


; Description:  Multiprecision addition:  a = a + b
;
; Input:        H/L (hi/lo) address to array (a)
;               D/E (hi/lo) address to array (b)
;
; Output:       a = a + b
;
; Registers:    A, B, C, D, E, H, L

add_mp:
	ld	bc,bin_len	; BC = array length
	add	hl,bc		; DE += BC
	ex	de,hl
	add	hl,bc		; HL += BC
	ld	b,unrollc	; count down to 0
	or	a		; clear carry
	jp	++		; Duff's Device hardcoded for fixed array length
/	dec	de		; DE--
	ld	a,(de)		; A <- (DE)
	dec	hl		; HL--
	adc	a,(hl)		; A + (HL) + carry
	ld	(de),a		; (DE) <- A
	dec	e		; E--
	ld	a,(de)		; A <- (DE)
	dec	l		; L--
	adc	a,(hl)		; A + (HL) + carry
	ld	(de),a		; (DE) <- A
/	dec	e		; E--
	ld	a,(de)		; A <- (DE)
	dec	l		; L--
	adc	a,(hl)		; A + (HL) + carry
	ld	(de),a		; (DE) <- A
	dec	e		; E--
	ld	a,(de)		; A <- (DE)
	dec	l		; L--
	adc	a,(hl)		; A + (HL) + carry
	ld	(de),a		; (DE) <- A
	djnz	--		; B--, B=0?
	ret


; Description:  Multiprecision left shift:  a = a * 2
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       a = a * 2
;
; Registers:    A, D, E, H, L

asl_mp:
	ld	de,bin_len	; DE = bin_len
	add	hl,de		; HL += DE (end address)
	ld	b,unrollc	; count down to 0
	or	a		; clear carry
	jp	++		; Duff's Device hardcoded for fixed array length
/	dec	hl		; HL--
	rl	(hl)		; rol
	dec	l		; L--
	rl	(hl)		; rol
/	dec	l		; L--
	rl	(hl)		; rol
	dec	l		; L--
	rl	(hl)		; rol
	djnz	--		; B--, B=0?
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
	ld	hl,bin_len	; HL = bin_len
	add	hl,de		; HL += DE
	ld	(end_ptr),hl	; store HL to end_ptr
	ex	de,hl		; DE <-> HL
				; HL points to array
				; DE = end of array
skipzeros:
	ld	e,bin_len/2	; number of paris to test
	xor	a		; a = 0
	jp	++		; Duff's Device hardcoded for fixed array length
				;   bin_len % 4 = 2
/	cp	(hl)		; is zero?
	jp	nz,even
	inc	l		; L += 1
	cp	(hl)		; is zero?
	jp	nz,odd
	inc	hl		; HL += 1
	dec	e		; E--
/	cp	(hl)		; is zero?
	jp	nz,even
	inc	l		; L += 1
	cp	(hl)		; is zero
	jp	nz,odd
	inc	l		; L += 1
	dec	e		; E--
	jp	nz,--		; E=0?
	scf			; set carry
	ret			;   all zeros, return
odd:	dec	hl		; HL--
even:	or	a		; clear carry
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

div16	macro {noexpand}	; Input:  DELH = dividend+0-3, BC = divisor
	rept	16		; do it, 16 times
	sla	h	
	rl	l
	rl	e
	rl	d
				; trial division, nothing changed
	ld	a,e		; A <- C
	sub	c		; C = C - E (dividend+1 - divisor(lo))
	ld	a,d		; A <- B
	sbc	a,b		; B = B - D (dividend+0 - divisor(hi)) - carry
	jp	c,m0		; too small
				; do it again, but this time save it
	ld	d,a		; B <- A, (hi) done, just save it
	ld	a,e		; A <- C, (lo)
	sub	c		; C = C - E (dividend+1 - divisor(lo))
	ld	e,a		; C <- A
	inc	h		; H++, dividend+3
m0:
	endm			; end 32/16 division
	endm

div16_mp:
	call	skipzeros16	; skip leading zeros for speed
	ret	c		; return if carry set (all zeros)

				; HL pointing to array MS 16-bit Digit
				; A, D, E disposable
				; BC = divisor

	ld	a,e		; backup E (number of remaining pairs) to A

	di			; disable interrupts
	ld	(stk_ptr),sp	; backup stack ptr
	ld	sp,hl		; store HL to stack pointer
				; load divisor in DE

	ld	d,0		; set carry/remainder to 0, dividend+0
	ld	e,0		; set carry/remainder to 0, dividend+1

				; SP pointing to array MS 16-bit digit
				; DE to be 16-bit carry/remainder/dividend
				; BC to be divisor
				; BCDE off limits, need for chain
				; DELH = dividend+0-3
				; A free

				; Duff's Device
	and	a,3		; mask off last 2 bits, clear carry
	jp	z,+		; is /4, i.e. do 4, 8, 12, ... loops
	rra			; roll right through carry, only updates carry
	inc	a		; trick used to update zero flag and preserve carry
	dec	a
	jp	z,d1		; bit 1 must have been set to get here, now in carry
				; do 1, 5, 9, ... loops
	jp	nc,d2		; bits 10, do 2, 6, 10, ... loops
	jp	d3		; bits 11, do 3 ,7, 11, ... loops

/	pop	hl		; load 16-bit pair into HL
	div16			; 32/16 division
	push	hl		; store 16-bit LSB pair HL
	pop	hl		; SP += 2
d3:	pop	hl		; load 16-bit pair into HL
	div16			; 32/16 division
	push	hl		; store 16-bit LSB pair HL
	pop	hl		; SP += 2
d2:	pop	hl		; load 16-bit pair into HL
	div16			; 32/16 division
	push	hl		; store 16-bit LSB pair HL
	pop	hl		; SP += 2
d1:	pop	hl		; load 16-bit pair into HL
	div16			; 32/16 division
	push	hl		; store 16-bit LSB pair HL
	pop	hl		; SP += 2

	and	a		; clear carry
	ld	hl,(end_ptr)	; HL = endptr
	sbc	hl,sp		; HL = HL - SP
	jp	nz,-		; if not zero, then not done

	ld	sp,(stk_ptr)	; restore stack ptr
	ei			; enable interrupts
	or	a		; clear carry
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

div8	macro {noexpand}	; Input:  B = lo, A = hi/remainder/carry, C = divisor
				; Output: B = lo, A = hi/remainder/carry
	ld	b,(hl)		; B <- M, dividend lo
	rept	8		; do it 8 times
	sla	b		; left shift B
	rla			; rol A
	jp	c,m0		; 9th bit hi?  Sub it.
	cp	c		; A - D, if A < D set carry
	jp	c,m1		; too small
m0:	sub	c		; A = A - D
	inc	b		; B++
m1:	
	endm
	ld	(hl),b		; M <- B save it
	endm

div_mp:
	call	skipzeros	; skip leading zeros for speed
	ret	c		; return if carry set (all zeros)
				; C = divisor
				; we know HL is even and bin_len is also even
	ld	a,e		; number of pairs left A <- E
	and	a,1		; odd or even number of pairs left
	ld	a,0		; zero out A (carry) without changing zero flag
	jp	nz,++		; odd pairs left
/	div8			; div M / D
	inc	l		; L++
	div8			; div M / D
	inc	hl		; HL++
	dec	e		; E--
/	div8			; div M / D
	inc	l		; L++
	div8			; div M / D
	inc	l		; L++
	dec	e		; E--
	jp	nz,--		; E=0?
	or	a		; clear carry
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
	ld	a,(hl)		; get A (page aligned table offset)
	ld	b,hi(mt100_lo)	; BC to point to low table
	ld	c,a		; C = A (position of A*100)
	ld	a,(bc)		; A <- (BC), LSB A = A*100
	add	a,e		; LSB A = A + E, add mp carry
	ld	(hl),a		; M <- A, store LSB
	inc	b		; B++ to get MSB from table
	ld	a,(bc)		; A <- (BC), MSB A = A*100
	adc	a,0		; add carry
	ld	e,a		; E <- A, store mp carry
	endm

print_mp:
				; HL now set to MSB
	ld	a,(hl)		; load A with MSB
	cp	10		; is < 10, then use ASCII method
	jp	c,+		;   otherwise, use 2 digit BCD method
	call	bintobcd	; convert to BCD
	call	prbcd		; print it
	jp	++		; skip over ASCII method
/	add	a,030h		; make ASCII
	call	echo		; print it
/	ld	a,'.'		; print decimal point
	call	echo		; print it
	ld	(hl),0		; zero MSB
				; compute end of array since we have
				;   to go backwards
	ld	bc,bin_len	; BC = bin_len
	add	hl,bc		; HL += BC
	ld	(end_ptr),hl	; save HL value to location end_ptr
				; setup bcd counter for outer loop
	ld	hl,bcd_len	; load HL with bcd_cnt
	ld	(bcd_cnt),hl	; save HL value to location bcd_cnt

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

	ld	hl,(end_ptr)	; load HL value from location end_ptr
				; backwards loop (LSB to MSB)
	ld	e,0		; E = carry = 0
				; E is the carry from mult100 to mult100
				; A,B,C,E (carry) used for mult100
				; D and E must not be touched!
	ld	d,unrollc	; count down to 0
	jp	++		; mp_array * 100 loop
/	dec	hl		; HL--
	mult100			; M = lo(M*100), E = hi(M*100)
	dec	l		; L--
	mult100			; M = lo(M*100), E = hi(M*100)
/	dec	l		; L--
	mult100			; M = lo(M*100), E = hi(M*100)
	dec	l		; HL--
	mult100			; M = lo(M*100), E = hi(M*100)
	dec	d		; D--
	jp	nz,--		; D=0?
				; HL now set to MSB
	ld	a,(hl)		; load A with MSB
	call	bintobcd	; convert to BCD
	call	prbcd		; print base 100 digit
	ld	(hl),0		; zero MSB

				; check for all decimal digits printed
	ld	hl,(bcd_cnt)	; load HL with bcd_cnt value
				;   works with bcd_len = 65536-(dec_len/2)
				;   counting up is easier
	inc	hl		; HL++
	ld	(bcd_cnt),hl	; store HL into bcd_cnt
	ld	a,l		; A <- L, check lo
	or	h		; or with hi
	jp	nz,print_mp1	; if not zero keep going, otherwise
	ret
	

; Description:  Output BCD number to display.
;
; Input:        A = BCD number (00 - 99)
;
; Output:       BCD number to display
;
; Registers:    A, B

prbcd:	ld	b,a		; B <- A, backup A
	rrca			; shift right 4 bits
	rrca
	rrca
	rrca
	call	+		; do MSB
	ld	a,b		; then LSB
/	and	a,00Fh		; mask off
	or	a,030h		; make a number
	call	echo		; print it
	ret


; Description:  Convert BIN/HEX to BCD.
;
; Input:        A = BIN/HEX (00h - 63h)
;
; Output:       A = BCD (00 - 99)
;
; Registers:    A, B, C

bintobcd:
	ld	e,a
	ld	d,hi(bcdtable)	; BC to point to low table
	ld	a,(de)
	ret


; Description:  Output a string.
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       String to screen
;
; Registers:    A, H, L

print:	ld	a,(hl)		; A <- M (HL)
	and	a		; update zero flag
	ret	z		; return if zero
	call	echo		; print char
	inc	hl		; HL++
	jp	print		; back to top


echo:
	if cpm == 0
	ld	(dma_ech),a
	else
	push	hl
	push	de
	push	bc
	ld	e,a		; E <- A
	ld	c,2		; Function 2
	call	bdos		; BDOS
	pop	bc
	pop	de
	pop	hl
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

;	org	02000h

	org	$+(4-$&3)	; force arrays on /4 boundary
mp_a:	org	$+bin_len
	org	$+(4-$&3)	; force arrays on /4 boundary
mp_b:	org	$+bin_len
	org	$+(4-$&3)	; force arrays on /4 boundary
mp_x:	org	$+bin_len
	org	$+(4-$&3)	; force arrays on /4 boundary
mp_y:	org	$+bin_len

