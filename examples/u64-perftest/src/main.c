

#include <stdint.h>
#include <stdlib.h>
#include <tice.h>
#include <bigintce.h>

#define num_tests 32768

const char hex_chars[16] = "0123456789ABCDEF";

#define end_timer() timer_GetSafe(1, TIMER_UP)

void start_timer(void){
	timer_Disable(1);
    timer_Set(1, 0);
    timer_Enable(1, TIMER_32K, TIMER_NOINT, TIMER_UP);
}



int main(void){
	ce_uint64_t *a;
	ce_uint64_t *b;
	unsigned int time;
	char *sbuf;
	if (!(a = malloc(8))) return 1;
	if (!(b = malloc(8))) return 1;
	if (!(sbuf = malloc(17))) return 1;
	u64_ito64(a, 0x123457);
	u64_copy(b, a);

	start_timer();
	for (int i = 1; i<num_tests; i++) {
		u64_mul(a, b);
	}
	time = end_timer();
	sprintf((void*)0xFB0000, "u64_mul took %i seconds, %i ms to run 32768 times.\n", time/32768, (time*1000)/32768);

	u64_tohex(sbuf, a);
	sprintf((void*)0xFB0000,"result: 0x%s\n", sbuf);
	return 0;
}

