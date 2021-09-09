#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <string.h>

#define BOOL_F		0x2f
#define BOOL_T		0x6f

#define FX_MASK		0x03
#define FX_TAG 		0x00
#define FX_SHIFT	2

#define CHAR_MASK	0xff
#define CHAR_TAG	0x0f
#define CHAR_SHIFT	8 /* bits */

#define NULL_VALUE	0x3f

typedef unsigned long ptr;

extern ptr scheme_entry(unsigned long val);
extern ptr explore(unsigned long val);

/* Literal constants used for relative addressing later. */
unsigned short c_true = BOOL_T;
unsigned short c_false = BOOL_F;
unsigned short c_null = NULL_VALUE;

static unsigned char* allocate_protected_space(int size)
{
	int page = getpagesize();
	int status;
	int aligned_size = ((size + page - 1) / page) * page;
	unsigned char *p = mmap(NULL, aligned_size + 2 * page,
							PROT_READ | PROT_WRITE,
							MAP_ANONYMOUS | MAP_PRIVATE,
							-1, 0);
	if (p == MAP_FAILED) {
		printf("mmap(): Failed: errno %d(%s)\n", errno, strerror(errno));
		exit(EXIT_FAILURE);
	}

	status = mprotect(p, page, PROT_NONE);
	if (status < 0) {
		printf("mprotect(): Failed on lowmem page: errno %d(%s)\n",
			errno, strerror(errno));
		exit(EXIT_FAILURE);
	}
	status = mprotect(p + page + aligned_size, page, PROT_NONE);
	if (status < 0) {
		printf("mprotect(): Failed on highmem page: errno %d(%s)\n",
			errno, strerror(errno));
		exit(EXIT_FAILURE);
	}

	return p + page;
}

static void deallocate_protected_space(unsigned char *p, int size)
{
	int page = getpagesize();
	int status;
	int aligned_size = ((size + page - 1) / page) * page;

	status = munmap(p - page, aligned_size + 2 * page);
	if (status < 0) {
		printf("munmap(): Failed: errno %d(%s)\n",
			errno, strerror(errno));
		exit(EXIT_FAILURE);
	}
}


static void print_ptr(ptr x)
{
	//printf("#<debug 0x%016lx>\n", (unsigned long)x);
	unsigned long fixnum;

	printf("INC-SCHEME> ");

	if ((x & FX_MASK) == FX_TAG) {
		/* we wil treat the fixnum as signed, this shift will sign extend it
			when it is negative */
		fixnum = ((long) x) >> FX_SHIFT;
		printf("%ld ;; fixnum (0x%016lx)", fixnum, fixnum);
	} else if (x == BOOL_T) {
		printf("#t ;; boolean");
	} else if (x == BOOL_F) {
		printf("#f ;; boolean");
	} else if ((x & CHAR_MASK) == CHAR_TAG) {
		printf("%c ;; character", 
			(char)(((unsigned long)x >> CHAR_SHIFT) & 0xff)); // screw unicode
	} else if (x == NULL_VALUE) {
		printf("() ;; null");
	} else {
		printf("#<unknown 0x%016lx> ;; FIXME\n", (unsigned long)x);
	}
	printf("\n");
}

static void debug(void)
{
	//printf("sizeof(int) = %ld\n", sizeof(int));
	//printf("sizeof(long) = %ld\n", sizeof(long));
}

int main(void)
{
	ptr scheme_val;
	int stack_size;
	unsigned char *stack_top; // lowmem address of lisp stack
	unsigned char *stack_base; // highmem address of lisp stack
	ptr exval;

	debug();

	stack_size = sizeof(ptr) * (16 * 1024); // 16K ptr sized cells

	printf("Stack size is %d bytes, or %d ptr cells.\n",
		stack_size, stack_size / (int)sizeof(ptr));

	stack_top = allocate_protected_space(stack_size);
	stack_base = stack_top + stack_size; 

	exval = explore(30);
	printf("Explore %%rax = 0x%016lx (%ld)\n", 
		(unsigned long)exval, (unsigned long) exval);

	printf("Executing scheme_entry...\n");

	scheme_val = scheme_entry((unsigned long)stack_base);

	print_ptr(scheme_val);

	deallocate_protected_space(stack_top, stack_size);

	printf("Good bye!\n");

	return 0;
}
