/* pi.c: calculates pi to MAXDIGITS places.  J.W. Stumpel, May 1991*/

/*

Apple //e Z80 CP/M Aztec C 16-bit int version, E. Ford, Aug 2009

This program was originally written for machines with 32-bit ints and 64-bit 
long longs.  Aztec C has 16-bit ints and 32-bit longs.  All long longs were 
changed to long and all ints to short for testing on modern 32-bit systems. 
Since Aztec C short is also 16-bit no other changes where required except 
in subbig where casting an unsigned to a signed did not behave as expected. 

Other changes are specific to Aztec C console output and printing, source
reformatting with indent -kr -ts4 and comment rearranging.

*/

#include <stdio.h>

#define MAXDIGITS 1000
#define MAXSIZE 210
/* MAXSIZE = ceil(MAXDIGITS / LOG10(2^16)) + 1 (+ 1 more for 16 bit systems) */

typedef union {
	unsigned long L;
	struct {
		unsigned short lo;
		unsigned short hi;
	} w;
} dword;

FILE *fp;
char use_printer=0;


/* This program represents "big numbers" (numbers with many digits) as
   arrays of unsigned ints. The first element (with index zero)
   in such an array represents the integer part of the number, the other
   elements represent the part after the decimal point.  */

typedef unsigned short bignum[MAXSIZE];


/* Copies a big number from source to dest */
void copybig(dest, source)
bignum dest, source;
{
	short j;

	for (j = 0; j < MAXSIZE; j++)
		dest[j] = source[j];
}


/* multiplies a big number by an unsigned integer */
void multbig(number, x)
bignum number;
unsigned short x;
{
	short j = MAXSIZE - 1;
	dword result;
	unsigned short remember = 0;

	while (number[j] == 0 && j >= 0)
		j--;
	while (j >= 0) {
		result.L = (long) number[j] * x + remember;
		number[j] = result.w.lo;
		remember = result.w.hi;
		j--;
	}
}


/* prints a big number as a decimal float number, by repeatedly multiplying
   by 10000 and printing the integer part */
void printbig(number)
bignum number;
{
	bignum num;
	short count = 0, line = 0, ndigits = 0;

	copybig(num, number);
	printf("pi=");
	printf("%d.", num[0]);
	if (use_printer) {
		fprintf(fp, "\npi= ");
		fprintf(fp, " %d.", num[0]);
	}
	while (ndigits < MAXDIGITS) {
		num[0] = 0;
		multbig(num, 10000);
		printf("%04d", num[0]);
		if (use_printer)
			fprintf(fp, "%04d", num[0]);
		ndigits += 4;
		count++;
		if (count == 2) {
			if (use_printer)
				fprintf(fp, " ");
			line++;
			count = 0;
		}
		if (line == 6) {
			if (use_printer)
				fprintf(fp, "  %4d\n       ", ndigits);
			line = 0;
		}
	}
	printf("\n");
	if (use_printer)
		fprintf(fp,"\n");
}


void dumpbig(number)
bignum number;
{
	short j;

	fprintf(fp, "dump: ");
	for (j = 0; j < MAXSIZE; j++)
		fprintf(fp, "%d ", number[j]);
}


/* divides a big number by an unsigned integer, returns zero if result zero,
   otherwise returns 1 */
short divbig(number, x)
bignum number;
unsigned short x;
{
	dword result;
	short j = 0;
	unsigned short rest = 0;

	while (number[j] == 0 && j < MAXSIZE)
		j++;
	if (j == MAXSIZE)
		return (0);
	while (j < MAXSIZE) {
		result.w.lo = number[j];
		result.w.hi = rest;
		number[j] = result.L / x;
		rest = result.L % x;
		j++;
	}
	return (1);
}


/* adds "source" to "dest". "addbig" and "subbig" assume that "dest" and
   "source" are both positive and that the result of a subtraction will also
   always be positive. */
void addbig(dest, source)
bignum dest, source;
{
	short j = MAXSIZE - 1;
	dword result;
	unsigned short remember = 0;

	while (source[j] == 0 && j >= 0)
		j--;
	while (j >= 0) {
		result.L = (long) dest[j] + source[j] + remember;
		dest[j] = result.w.lo;
		remember = result.w.hi;
		j--;
	}
}


/* subtracts source from dest */
void subbig(dest, source)
bignum dest, source;
{
	short j = MAXSIZE - 1;
	unsigned short borrow = 0;
	dword result;

	while (source[j] == 0 && j >= 0)
		j--;
	while (j >= 0) {
		/*

		was (didn't work with Aztec C):

		result.L = (long) dest[j] - source[j] - (short) borrow;

		 */
		if (borrow > 32767)
			result.L = (long) dest[j] - source[j] - (65536 - borrow);
		else
			result.L = (long) dest[j] - source[j] + borrow;

		dest[j] = result.w.lo;
		borrow = result.w.hi;
		j--;
	}
}


/* fill a big number with some numbers */
void setbig(number, intpart, decpart)
bignum number;
short intpart;
unsigned short decpart;
{
	short j;

	number[0] = intpart;
	for (j = 1; j < MAXSIZE; j++)
		number[j] = decpart;
}


/* Calculates the arc tangent of (1/x) by series summation.
   Result is stored in big number A */
void atanbig(A, x)
bignum A;
unsigned short x;
{
	bignum X, Y;
	unsigned short n = 1;

	setbig(X, 1, 0);
	divbig(X, x);
	copybig(A, X);
	if (x < 128)
		x *= x;
	while (1) {
		n += 2;
		divbig(X, x);
		if (x > 127)
			divbig(X, x);
		copybig(Y, X);
		if (!divbig(Y, n))
			break;
		if (n & 2)
			subbig(A, Y);
		else
			addbig(A, Y);
	}
}


/* Calculates and prints pi using Machin's formula:
   pi = 16 * atan(1/5)  - 4 * atan (1/239) */
void make_pi()
{
	bignum A, B;

	atanbig(A, 5);
	atanbig(B, 239);
	multbig(A, 16);
	multbig(B, 4);
	subbig(A, B);
	printbig(A);
}


readtcp()
{
#asm
	MVI		A,'#'
	STA		0F045H
	LXI		H,0C70BH
	SHLD	0F3D0H
	LHLD	0F3DEH
	MOV		M,A
	LXI		H,0C708H
	SHLD	0F3D0H
	LHLD	0F3DEH
	MOV		M,A
#endasm
}


void scr_clear()
{
	printf("%c*",27);
}


getch()
{
	/* return the last key press */
	int ch;

	while((ch=bdos(6,0xff))==0);

	return ch;
}


int main()
{
	short N = MAXDIGITS;
	char c = 0;
	unsigned long start, end;
	char *tcp=(void*)0x0F200;

	scr_clear();

	while(1) {
		printf("Output to printer? [Y/N]");
		c = getch();
		if (c == 'Y' || c == 'y') {
			use_printer=1;
			break;
		}
		if (c == 'N' || c == 'n') {
			use_printer=0;
			break;
		}
		printf("\n");
	}

	scr_clear();
	printf("Calculating pi to %d places...\n\n", N);

	if (use_printer) {
		if ((fp = fopen("LST:", "w")) == NULL) {
			printf("Printer open failed!\n");
			printf("\nPress any key to exit.");
			getch();
			_exit(1);
		}
	}

	readtcp();
	start = (tcp[15] - 176) * 10 + (tcp[16] - 176);
	start += 60*((tcp[12] - 176) * 10 + (tcp[13] - 176));
	start += 3600*((tcp[9] - 176) * 10 + (tcp[10] - 176));

	make_pi();

	readtcp();
	end = (tcp[15] - 176) * 10 + (tcp[16] - 176);
	end += 60*((tcp[12] - 176) * 10 + (tcp[13] - 176));
	end += 3600*((tcp[9] - 176) * 10 + (tcp[10] - 176));

	printf("\nRuntime: %d sec\n\n",end-start);
	if (use_printer) {
		fprintf(fp,"\nRuntime: %d sec\n\n",end-start);
		fclose(fp);
	}

	printf("Press any key to exit.");
	getch();
	_exit(0);
}

/*

Output:

Calculating pi to 1000 places...

pi=3.141592653589793238462643383279502884197169399375105820974944592307816406286
20899862803482534211706798214808651328230664709384460955058223172535940812848111
74502841027019385211055596446229489549303819644288109756659334461284756482337867
83165271201909145648566923460348610454326648213393607260249141273724587006606315
58817488152092096282925409171536436789259036001133053054882046652138414695194151
16094330572703657595919530921861173819326117931051185480744623799627495673518857
52724891227938183011949129833673362440656643086021394946395224737190702179860943
70277053921717629317675238467481846766940513200056812714526356082778577134275778
96091736371787214684409012249534301465495853710507922796892589235420199561121290
21960864034418159813629774771309960518707211349999998372978049951059731732816096
31859502445945534690830264252230825334468503526193118817101000313783875288658753
32083814206171776691473035982534904287554687311595628638823537875937519577818577
805321712268066130019278766111959092164201989

*/

/*

Times:

6502 1.023 MHz:  2180 sec, 00:36:20

Z80 4 Mhz:       2572 sec, 00:42:52

Z80 2 Mhz:       5138 sec, 01:25:38

*/

