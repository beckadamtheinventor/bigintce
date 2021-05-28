

#include <stdint.h>
#include <stdlib.h>
#include <bigintce.h>

const char hex_chars[16] = "0123456789ABCDEF";

int main(void){
	varint_t *a;
	varint_t *b;
	varint_t *c;
	varint_t *r;
	varint_t *r2;
	char *sbuf;

	/* Allocate varints */
	if (!(a = vint_malloc(16))) return 1;
	if (!(b = vint_malloc(4))) return 1;
	if (!(c = vint_malloc(16))) return 1;
	if (!(r = vint_malloc(8))) return 1;
	if (!(r2 = vint_malloc(64))) return 1;

	/* Allocate output buffer */
	if (!(sbuf = malloc(129))) return 1;

	/* set varint_t *a to 0x123456789ABCDEF0 */
	vint_itov(a, 0x123456);
	vint_shl(a, 24);
	vint_addi(a, 0x789ABC);
	vint_shl(a, 16);
	vint_addi(a, 0xDEF0);

	/* Set varint_t *b to 0x42069 */
	vint_itov(b, 0x42069);

	vint_tohex(sbuf, vint_add(b, a));
	sprintf((void*)0xFB0000,"0x123456789ABCDEF0 + 0x42069 = 0x%s\n", sbuf);
	vint_itov(b, 0x42069);
	vint_tohex(sbuf, vint_mul(a, b, r));
	sprintf((void*)0xFB0000,"0x123456789ABCDEF0 * 0x42069 = 0x%s\n", sbuf);
	vint_tohex(sbuf, vint_mul(a, b, r2));
	sprintf((void*)0xFB0000,"0x123456789ABCDEF0 * 0x42069 = 0x%s\n", sbuf);

	return 0;
}

