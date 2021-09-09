	# a file where I can just add some garbage into the C runtime in asm.

	# c_true and c_false labels are globals defined in main.c

    .text
    .globl explore

#if !__CYGWIN__
	# Windows PECOFF format doesn't support .type, figure out later.
    .type explore, @function
#endif

explore:
    movq %rdi, %rax
    movq $30, %rax # arg1
    cmpq $29, %rax # arg0
    setg %al
    movzbl %al, %rax
    #sal $6, %al
    #or $0x2f, %al

    movq $10, %rax
    subq $2, %rax
    ret
