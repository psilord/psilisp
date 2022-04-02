    .text                               
    .globl test_fun_inst_0
    .type test_fun_inst_0, @function       

test_fun_inst_0:
	# Prologue
	pushq %rbp	 
	movq %rsp, %rbp
	# Reserve some stack space. (unused)
	subq $8, %rsp

	# code

	movq %rdi, -8(%rbp)
	movq $0x42, %rax

	# Epilogue
	#movq %ebp, %esp
	#popq   %ebp
	leave
	ret

	

