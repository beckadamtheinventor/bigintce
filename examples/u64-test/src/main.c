

#include <stdint.h>
#include <stdlib.h>
#include <bigintce.h>

const char hex_chars[16] = "0123456789ABCDEF";

void u64tohs(char *buf, ce_uint64_t *num);

int main(void){
	ce_uint64_t *a;
	ce_uint64_t *b;
	char *sbuf;
	if (!(a = malloc(8))) return 1;
	if (!(b = malloc(8))) return 1;
	if (!(sbuf = malloc(17))) return 1;
	u64_ito64(a, 0x123456);
	u64_ito64(b, 0x42069);
	u64tohs(sbuf, u64_add(a, b));
	sprintf((void*)0xFB0000,"0x123456 + 0x42069 = 0x%s\n", sbuf);
	u64_ito64(a, 0x123456);
	u64tohs(sbuf, u64_mul(a, b));
	sprintf((void*)0xFB0000,"0x123456 * 0x42069 = 0x%s\n", sbuf);
	return 0;
}

void u64tohs(char *buf, ce_uint64_t *num){
	int j;
	int i = 0;
	uint8_t *ptr = (uint8_t*)num;
	for (j=0; j<8; j++){
		buf[i] = hex_chars[ptr[7-j]>>4]; i++;
		buf[i] = hex_chars[ptr[7-j]&15]; i++;
	}
	buf[16] = 0;
}

