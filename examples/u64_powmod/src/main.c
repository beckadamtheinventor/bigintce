

#include <stdint.h>
#include <stdlib.h>
#include <bigintce.h>

const char hex_chars[16] = "0123456789ABCDEF";

const ce_uint64_t b_data = {
	0x83, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08
};

const ce_uint64_t e_data = {
	0x0B, 0, 0, 0x20, 0, 0, 0, 0
};

const ce_uint64_t m_data = {
	0x03, 0, 0, 0x40, 0, 0, 0, 0
};


int main(void){
	ce_uint64_t *b;
	ce_uint64_t *e;
	ce_uint64_t *m;
	char *sbuf;
	if (!(b = malloc(8))) return 1;
	if (!(e = malloc(8))) return 1;
	if (!(m = malloc(8))) return 1;
	if (!(sbuf = malloc(17))) return 1;

	u64_copy(b, &b_data);
	u64_copy(e, &e_data);
	u64_copy(m, &m_data);

	u64_tohex(sbuf, b);
	sprintf((void*)0xFB0000,"B = 0x%s\n", sbuf);

	u64_tohex(sbuf, e);
	sprintf((void*)0xFB0000,"E = 0x%s\n", sbuf);

	u64_tohex(sbuf, m);
	sprintf((void*)0xFB0000,"M = 0x%s\n", sbuf);

	u64_powmod(b, e, m);
	u64_tohex(sbuf, b);
	sprintf((void*)0xFB0000,"(B ^ E) mod M = 0x%s\n", sbuf);

	return 0;
}

