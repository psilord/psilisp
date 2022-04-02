#include <stdio.h>
#include <stdlib.h>

extern int test_fun_inst_0(void);

int main(void)
{
	unsigned long long val;

	val = test_fun_inst_0();

	printf("test_fun_inst_0() returned: 0x%08llX\n", val);

	return 0;
}
