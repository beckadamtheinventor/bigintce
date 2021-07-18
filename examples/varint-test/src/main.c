

#include <stdint.h>
#include <stdlib.h>
#include <bigintce.h>

const char hex_chars[16] = "0123456789ABCDEF";

const vint_t a_data = {
	0x10,0x12,0x34,0x56,0x78,0x9A,0xBC,0xDE,0xF0,0,0,0,0,0,0,0,0
};

int main(void){
	varint_t *a;
	varint_t *b;
	varint_t *c;
	char *sbuf;

	/* Allocate varints */
	if (!(a = vint_malloc(16))) return 1;
	if (!(b = vint_malloc(4))) return 1;
	if (!(c = vint_malloc(32))) return 1;

	/* Allocate output buffer */
	if (!(sbuf = malloc(129))) return 1;

	/* Set varint_t *b to 0x42069 */
	vint_itov(b, 0x42069);

	vint_copy(a, &a_data);
	vint_tohex(sbuf, vint_add(a, b));
	sprintf((void*)0xFB0000,"0x123456789ABCDEF0 + 0x42069 = 0x%s\n", sbuf);

	vint_copy(a, &a_data);
	vint_tohex(sbuf, vint_sub(a, b));
	sprintf((void*)0xFB0000,"0x123456789ABCDEF0 - 0x42069 = 0x%s\n", sbuf);

	vint_copy(a, &a_data);
	vint_tohex(sbuf, vint_mul(a, b, c));
	sprintf((void*)0xFB0000,"0x123456789ABCDEF0 * 0x42069 = 0x%s\n", sbuf);

	return 0;
}

