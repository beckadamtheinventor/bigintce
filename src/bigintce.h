
#ifndef __INT64CE_H__
#define __INT64CE_H__

typedef void ce_uint64_t;
typedef void ce_int64_t;

typedef struct __vint_t__ {
	uint8_t bytes;
	uint8_t value[];
} vint_t;
#define varint vint_t

/* Allocate a 64-bit integer */
ce_uint64_t *u64_alloc(void*(void(*_alloc)(size_t)));

/* Set a 64-bit integer to zero */
ce_uint64_t *u64_zero(ce_uint64_t *A);

/* Shift a 64-bit integer A by B bits left */
ce_uint64_t *u64_shl(ce_uint64_t *A, uint8_t B);

/* Shift a 64-bit integer A by B bits right */
ce_uint64_t *u64_shr(ce_uint64_t *A, uint8_t B);

/* Add a 64-bit integer A and B, storing to A */
ce_uint64_t *u64_add(ce_uint64_t *A, ce_uint64_t *B);

/* Add a 64-bit integer A and 24-bit integer B, storing to A */
ce_uint64_t *u64_addi(ce_uint64_t *A, unsigned int B);

/* Convert a 24-bit integer B, storing to 64-bit integer A */
ce_uint64_t *u64_ito64(ce_uint64_t *A, unsigned int B);

/* Multiply two 64-bit integers A and B, storing to A */
ce_uint64_t *u64_mul(ce_uint64_t *A, ce_uint64_t *B);

/* Subtract two 64-bit integers A minus B, storing to A */
ce_uint64_t *u64_sub(ce_uint64_t *A, ce_uint64_t *B);

/* Subtract two 64-bit integers A minus 24-bit integer B, storing to A */
ce_uint64_t *u64_subi(ce_uint64_t *A, unsigned int B);

/* Divide 64-bit integer A by 64-bit integer B, storing result to A and remainder to R */
ce_uint64_t *u64_div(ce_uint64_t *A, ce_uint64_t *B, ce_uint64_t *R);

/* allocate a variable-length integer
   Example usage:
     vint_alloc(16, malloc);

   Minimum number of bytes is 4.
*/
vint_t *vint_alloc(uint8_t bytes, void*(void(*_alloc)(size_t)));

/* Zero a variable-length integer */
vint_t *vint_zero(vint_t *A);

/* Add two variable-length integers A and B, storing to A */
vint_t *vint_add(vint_t *A);

/* Add a variable-length integer A and an unsigned 24-bit integer B, storing to A */
vint_t *vint_addi(vint_t *A, unsigned int B);

/* Subtract a variable-length integer B from A, storing to A */
vint_t *vint_sub(vint_t *A, vint_t *B);

/* Subtract an unsigned integer B from a variable-length integer A, storing to A */
vint_t *vint_subi(vint_t *A, unsigned int B);

/* Multiply two variable-length integers A and B, storing to A */
vint_t *vint_mul(vint_t *A, vint_t *B);

/* Divide a variable-length integer A by B, storing result to A and remainder to R */
vint_t *vint_div(vint_t *A, vint_t *B, vint_t *R);



#endif

