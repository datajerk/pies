/* pi.c: calculates pi to MAXDIGITS places.  J.W. Stumpel, May 1991*/

/*

Apple //e Z80 CP/M sdcc 16-bit int version, E. Ford, Jun 2011

This program was originally written for machines with 32-bit ints and 64-bit 
long longs.  cc65 has 16-bit ints and 32-bit longs.  All long longs were 
changed to long and all ints to short for testing on modern 32-bit systems. 
Since cc65 short is also 16-bit no other changes where required except 
in subbig where casting an unsigned to a signed did not behave as expected. 

Other changes are specific to cc65 console output.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stdlib.h"
#include "cpmbdos.h"
#include "cprintf.h"

/* THESE ARE USED BY THE LIBRARY ROUTINES */
char getchar(void)
{
	struct BDOSCALL cread = { C_READ, { (unsigned int)0 } };
	return cpmbdos(&cread);
}
void outchar(char c)
{
	struct BDOSCALL cwrite = { C_WRITE, { (unsigned int)c } };
	cpmbdos(&cwrite);
}

#define MAXDIGITS 1000
#define MAXSIZE 210
/* MAXSIZE = ceil(MAXDIGITS / LOG10(2^sizeof(int))) + 1 (+ 1 more for 16 bit systems) */

typedef union {
	unsigned long L;
	struct {
		unsigned short lo;
		unsigned short hi;
	} w;
} dword;

/*
FILE *fp;
char use_printer = 0;
*/


/* This program represents "big numbers" (numbers with many digits) as
   arrays of unsigned ints. The first element (with index zero)
   in such an array represents the integer part of the number, the other
   elements represent the part after the decimal point.  */

typedef unsigned short bignum[MAXSIZE];


/* Copies a big number from source to dest */
void copybig(bignum dest, bignum source)
{
	short j;

	for (j = 0; j < MAXSIZE; j++)
		dest[j] = source[j];
}


/* multiplies a big number by an unsigned integer */
void multbig(bignum number, unsigned short x)
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
void printbig(bignum number)
{
	bignum num;
	short count = 0, line = 0, ndigits = 0;

	copybig(num, number);
	printf("pi=");
	printf("%d.", num[0]);
/*
	if (use_printer) {
		fprintf(fp, "\npi= ");
		fprintf(fp, " %d.", num[0]);
	}
*/
	while (ndigits < MAXDIGITS) {
		num[0] = 0;
		multbig(num, 10000);
		printf("%04d", num[0]);
/*
		if (use_printer)
			fprintf(fp, "%04d", num[0]);
*/
		ndigits += 4;
		count++;
		if (count == 2) {
/*
			if (use_printer)
				fprintf(fp, " ");
*/
			line++;
			count = 0;
		}
		if (line == 6) {
/*
			if (use_printer)
				fprintf(fp, "  %4d\n       ", ndigits);
*/
			line = 0;
		}
	}
	printf("\n");
/*
	if (use_printer)
		fprintf(fp, "\n");
*/
}


/* divides a big number by an unsigned integer, returns zero if result zero,
   otherwise returns 1 */
short divbig(bignum number, unsigned short x)
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
void addbig(bignum dest, bignum source)
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
void subbig(bignum dest, bignum source)
{
	short j = MAXSIZE - 1;
	unsigned short borrow = 0;
	dword result;

	while (source[j] == 0 && j >= 0)
		j--;
	while (j >= 0) {
		/*

		   was (didn't work with cc65):

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
void setbig(bignum number, short intpart, unsigned short decpart)
{
	short j;

	number[0] = intpart;
	for (j = 1; j < MAXSIZE; j++)
		number[j] = decpart;
}


/* Calculates the arc tangent of (1/x) by series summation.
   Result is stored in big number A */
void atanbig(bignum A, unsigned short x)
{
	bignum X, Y;
	unsigned short n = 1;

	setbig(X, 1, 0);
printbig(X);
	divbig(X, x);
	copybig(A, X);
	x *= x;
	while (1) {
printf("%d ",n);
		n += 2;
		divbig(X, x);
printbig(X);
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

printf("test 1\n");
	atanbig(A, 5);
printf("test 2\n");
	atanbig(B, 239);
printf("3\n");
	multbig(A, 16);
printf("4\n");
	multbig(B, 4);
printf("5\n");
	subbig(A, B);
printf("6\n");
	printbig(A);
}


void readtcp()
{
/*
#asm
	MVI   A,'#'
	STA   0F045H
	LXI   H,0C70BH
	SHLD  0F3D0H
	LHLD  0F3DEH
	MOV   M,A
	LXI   H,0C708H
	SHLD  0F3D0H
	LHLD  0F3DEH
	MOV   M,A
#endasm
*/
__asm
	ld      a,#0xa3
	ld      (0x0F045),a
	ld      hl,(0x0C70B)
	ld      (0x0F3D0),hl
	ld      hl,(0x0F3DE)
	ld      (hl),a
	ld      hl,(0x0C708)
	ld      (0x0F3D0),hl
	ld      hl,(0x0F3DE)
	ld      (hl),a
__endasm;
}


int main()
{
	short N = MAXDIGITS;
	char c = 0;
	unsigned long start, end;
	char *tcp=(void*)0x0F200;

/*
	while (1) {
		printf("Output to PI.TXT? [Y/N]");
		c = cgetc();
		if (c == 'Y' || c == 'y') {
			use_printer = 1;
			break;
		}
		if (c == 'N' || c == 'n') {
			use_printer = 0;
			break;
		}
		printf("\n");
	}
*/

	printf("Calculating pi to %d places...\n\n", N);

/*
	if (use_printer) {
		if ((fp = fopen("PI.TXT", "w")) == NULL) {
			printf("Failed to open PI.TXT!\n");
			printf("\nPress any key to exit.");
			cgetc();
			return EXIT_FAILURE;
		}
	}
*/

	readtcp();
	start = (tcp[15] - 176) * 10 + (tcp[16] - 176);
	start += 60*((tcp[12] - 176) * 10 + (tcp[13] - 176));
	start += 3600*((tcp[9] - 176) * 10 + (tcp[10] - 176));

printf("starting\n");
	make_pi();

	readtcp();
	end = (tcp[15] - 176) * 10 + (tcp[16] - 176);
	end += 60*((tcp[12] - 176) * 10 + (tcp[13] - 176));
	end += 3600*((tcp[9] - 176) * 10 + (tcp[10] - 176));

	printf("\nRuntime: %ld sec\n\n",end-start);

/*
	if (use_printer) {
		fprintf(fp, "\nRuntime: %ld sec\n",end-start);
		fclose(fp);
	}
*/

	printf("Press any key to exit.");
	getchar();
	return 0;
}

