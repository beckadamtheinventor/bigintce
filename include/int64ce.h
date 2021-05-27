
#ifndef __int64ce_h__
#define __int64ce_h__

/* 64 bit integer routines for the ez80 microprocessor
     Author: Adam "beckadamtheinventor" Beckingham
     License: WTFPL
*/

/* 64-bit right shift */
uint64_t *shr64(uint64_t *A, uint8_t B);

/* 64-bit left shift */
uint64_t *shl64(uint64_t *A, uint8_t B);

/* unsigned 64-bit add */
uint64_t *uadd64(uint64_t *A, uint64_t *B);

/* unsigned 64-bit add with unsigned 24-bit */
uint64_t *uadd64i(uint64_t *A, unsigned int B);

/* unsigned 24-bit integer to unsigned 64-bit */
uint64_t *uito64(uint64_t *A, unsigned int B);

/* unsigned 64-bit integer to unsigned 24-bit */
#define u64tol(A) (*((unsigned int*)(A)))

/* unsigned 64-bit multiplication */
uint64_t *umul64(uint64_t *A, uint64_t *B, uint64_t *C);

/* unsigned 64-bit subtraction */
uint64_t *usub64(uint64_t *A, uint64_t *B);

/* unsigned 64-bit subtraction with unsigned 24-bit */
uint64_t *usub64i(uint64_t *A, unsigned int B);

/* 64-bit integer set to 0 */
uint64_t *zero64(uint64_t *A);

#endif