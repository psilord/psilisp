#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long u64;

#define U64(x) ((u64)x)
#define TAGVAL_FUNINST_PRED		0b1101
#define TAGVAL_FUNINST_FUN		0b1101
#define TAGVAL_FUNINST_CLOSURE	0b1111
#define TAG_FUNINST_FUN(v) ((u64)(((u64)v) | TAGVAL_FUNINST_FUN))
#define TAG_FUNINST_CLOSURE(v) ((u64)(((u64)v) | TAGVAL_FUNINST_CLOSURE))
#define IS_FUNINST(v) ((u64)(((u64)v) & TAGVAL_FUNINST_PRED))

extern u64 tinker_0(u64 v0, u64 v1);

extern u64 funinst_fun_0(void);
extern u64 funinst_fun_1(u64 v0);

extern u64 invoke_funinst_0(u64 funinst);
extern u64 invoke_funinst_1(u64 funinst, u64 v0);

void emit(char *msg, u64 val) 
{
	printf("%s -> 0x%08llX\n", msg, val);
}

int main(void)
{
	u64 val, fi;

	val = tinker_0(U64(0x10), U64(0x20));
	emit("tinker_0()", val);

	printf("\n");

	emit("Real funinst_fun_0", (u64)funinst_fun_0);
	fi = TAG_FUNINST_FUN(funinst_fun_0);
	emit("Tagged funinst_fun_0", fi);
	val = invoke_funinst_0(fi);
	emit("funinst_fun_0()", val);

	printf("\n");

	emit("Real funinst_fun_1", (u64)funinst_fun_1);
	fi = TAG_FUNINST_FUN(funinst_fun_1);
	emit("Tagged funinst_fun_1", fi);
	val = invoke_funinst_1(fi, (u64)0x10);
	emit("funinst_fun_1()", val);


	return 0;
}
