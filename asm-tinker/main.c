#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long u64;

#define U64(x) ((u64)x)
#define TAGVAL_FIXNUM			0b0000
#define TAGVAL_FIXNUM_MASK		0b0001
#define FIXNUM_SHIFT			1
#define TAG_FIXNUM(v)			((v) << FIXNUM_SHIFT)
#define UNTAG_FIXNUM(v)			((v) >> FIXNUM_SHIFT)
#define IS_FIXNUM(v) \
	((u64)(((u64)v) & TAGVAL_FIXNUM_MASK) == TAGVAL_FIXNUM)


#define TAGVAL_FUNINST			0b1101
#define TAGVAL_FUNINST_FUN		0b1101
#define TAGVAL_FUNINST_CLOSURE	0b1111
#define TAGVAL_FUNINST_MASK		0b1101

#define TAG_FUNINST_FUN(v) ((u64)(((u64)v) | TAGVAL_FUNINST_FUN))
#define TAG_FUNINST_CLOSURE(v) ((u64)(((u64)v) | TAGVAL_FUNINST_CLOSURE))
#define IS_FUNINST(v) \
	((u64)(((u64)v) & TAGVAL_FUNINST_MASK) == TAGVAL_FUNINST)

extern u64 tinker_0(u64 v0, u64 v1);

extern u64 funinst_fun_0(void);
extern u64 funinst_fun_1(u64 v0);

extern u64 invoke_funinst_0(u64 funinst);
extern u64 invoke_funinst_1(u64 funinst, u64 v0);

extern u64 funinst_closure_0_1(u64 closure);

void emit(char *msg, u64 val) 
{
	printf("%s -> 0x%08llX\n", msg, val);
}

int main(void)
{
	u64 val, fi, *cl;

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
	printf("\n");

	/* Generate a closure record */
	cl = (u64*)malloc(sizeof(u64) * 2); /* [0] is function, [1] is closed var */
	cl[0] = TAG_FUNINST_FUN(funinst_closure_0_1);
	cl[1] = (u64)0x42;
	fi = TAG_FUNINST_CLOSURE(cl);
	val = invoke_funinst_0(fi);
	emit("funinst_closure_0_1()", val);

	printf("\n");
	printf("Done.\n");

	return 0;
}
