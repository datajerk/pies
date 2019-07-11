.macro	cnton num
	sta	$E100+num
.endmacro

.macro	cntoff num
	sta	$E200+num
.endmacro

.macro	chkpt num
	sta	$E300+num
.endmacro

; Description:	Load A/Y registers with a 16-bit value,
;               low byte in A, high byte in Y
; Input:        value (16-bit) immediate, absolute and zero-page addressing
	
.macro	lday	value
	.if (.match (.left (1, {value}), #))
		lda	#<(.right (.tcount ({value})-1, {value}))
		ldy	#>(.right (.tcount ({value})-1, {value}))
	.else
		lda	value
		ldy	value+1
	.endif
.endmacro


; Description:  Store 16-bit value in A/Y registers to memory,
;               low byte in A, high byte in Y
; Input:        value (16-bit), absolute and zero-page addressing
	
.macro	stay	location
	sta	location
	sty	location+1
.endmacro


; Description:	Allocate an array of size "sizeof" at "ptr"
; Input:	ptr (16-bit), sizeof (16-bit)
;		ptr = pointer to start of memory (if successful)
;		sizeof = length of requested memory, length can be
;		immediate (e.g. #1000) or absolute/zp (label)
; Calls:	malloc, if malloc fails then calls malloc_error and dies
;		program.

.macro	alloc	ptr, sizeof
	lday	sizeof
	jsr	malloc		; call malloc
	bcc	:+		; if not failure (bcc) set ptr lo/hi
	jmp	malloc_error	; on failure jmp to malloc_error and die
:	stay	ptr
.endmacro


; Description:	Set initial value left of decimal of "ptr_a" array to
;		"value", i.e. (ptr_a),0 = value
; Input:	ptr_a (16-bit), value (8-bit)
;		ptr_a = pointer to start of allocated array
;		value = integer to be set left of decimal point
; Calls:	set_mp

.macro	mset	ptr_a, value	; a = value
cnton 19
	lday	ptr_a
	ldx	value
	jsr	set_mp		; set it
cntoff 19
.endmacro


; Description:	Transfer array (ptr_b)[0,sizeof(ptr_b)] to ptr_a (a = b)
; Input:	ptr_a (16-bit), ptr_b (16-bit)
;		ptr_a = pointer to start of allocated array
;		ptr_b = pointer to start of allocated array
; Calls:	copy_mp

.macro	mcopy	ptr_a, ptr_b	; a = b
cnton 20
	lday	ptr_a
	stay	ptr_mp
	lday	ptr_b
	jsr	copy_mp		; copy it
cntoff 20
.endmacro


; Description:	Left shift array ptr_a
; Input:	ptr_a (16-bit)
;		ptr_a = pointer to start of allocated array
; Calls:	asl_mp

.macro	masl	ptr_a		; a = a * 2
cnton 21
	lday	ptr_a
	jsr	asl_mp		; left shift it
cntoff 21
.endmacro


; Description:	Add array ptr_b to array ptr_a (a = a + b)
; Input:	ptr_a (16-bit), ptr_b (16-bit)
;		ptr_a = pointer to start of allocated array
;		ptr_b = pointer to start of allocated array
; Calls:	add_mp

.macro	madd	ptr_a, ptr_b	; a = a + b
cnton 22
	lday	ptr_b
	stay	ptr_mp
	lday	ptr_a
	jsr	add_mp		; add it
cntoff 22
.endmacro


; Description:	Subtract array ptr_b from array ptr_a (a = a - b)
; Input:	ptr_a (16-bit), ptr_b (16-bit)
;		ptr_a = pointer to start of allocated array
;		ptr_b = pointer to start of allocated array
; Calls:	sub_mp

.macro	msub	ptr_a, ptr_b	; a = a - b
cnton 23
	lday	ptr_b
	stay	ptr_mp
	lday	ptr_a
	jsr	sub_mp		; sub it
cntoff 23
.endmacro


; Description:	Divide ptr_a by an 8-bit quantity (a = a / divisor)
; Input:	ptr_a (16-bit), divisor (8-bit)
;		ptr_a = pointer to start of allocated array
;		divisor = 8-bit immediate or absolute/zp integer
; Calls:	div_mp
; Return:	If ptr_a = 0 (array of zeros), then carry is set,
;		otherwise carry is clear.

.macro	mdiv	ptr_a, divisor	; a = a / divisor (8-bit)
cnton 24
	lday	ptr_a
	ldx	divisor
	jsr	div_mp		; div it
cntoff 24
.endmacro


; Description:	Divide ptr_a by a 16-bit quantity (a = a / value)
; Input:	ptr_a (16-bit), value (8-bit)
;		ptr_a = pointer to start of allocated array
;		value = 16-bit immediate or absolute/zp integer
; Calls:	div16_mp
; Return:	If ptr_a = 0 (array of zeros), then carry is set,
;		otherwise carry is clear

.macro	mdiv16	ptr_a, value	; a = a / value (16-bit)
cnton 25
	lday	value
	sty	divisor+0	; big-endian
	sta	divisor+1
	lday	ptr_a
	jsr	div16_mp	; div it
cntoff 25
.endmacro


; Description:	Compute arctan(1/value) and store at ptr_a.
; Input:	ptr_a (16-bit), value (8-bit)
;		ptr_a = pointer to start of allocated array
;		value = 8-bit immediate or absolute/zp integer
; Calls:	atan_mp

.macro	matan	ptr_a, value	; a = atan(1/value)
cnton 26
	lday	ptr_a
	ldx	value
	jsr	atan_mp		; atan it
cntoff 26
.endmacro


; Description:	Convert and print/display a base 256 number (ptr_a) as
;		base 10.
; Input:	ptr_a (16-bit)
;		ptr_a = pointer to start of allocated array
; Calls:	print_mp

.macro	mprint	ptr_a		; print a (base 10)
cnton 27
	lday	ptr_a
	jsr	print_mp	; print it
cntoff 27
.endmacro


; Description:	Print a string (arg) with an optional trailing character
;		(usually a carriage return (CR = $0D)).
; Input:	immediate with extra:		"string",$0D
;		immediate w/o extra:		"string"
;		absolue/zp label with extra:	label,$0D
;		absolue/zp label w/o extra:	label
; Calls:	print, crout

.macro	sprint	arg, cr
	.local	str
	.local	skip
	.if .match ({arg},"")
		lday	#str
		jsr	print
		beq	skip
	str:	.byte	arg
	.ifnblank	cr	
		.byte	cr
	.endif
		.byte	$0
	skip:
	.else
		lday	#arg
		jsr	print
		.ifnblank	cr	
		jsr	crout
		.endif
	.endif
.endmacro
