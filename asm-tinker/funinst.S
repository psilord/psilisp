    .text                               
	.equ tagval_funinst, 0x0d

	###############################################################
	# C arg passing convention
	#movq    %rdi, -8(%rbp)
	#movq    %rsi, -16(%rbp)
	#movq    %rdx, -24(%rbp)
	#movq    %rcx, -32(%rbp)
	#movq    %r8, -40(%rbp)
	#movq    %r9, -48(%rbp)
	###############################################################

	# ----------------------------------------------------------------------
	# This is a chunk of code where I can figure out how things work before
	# attempting to use the knowledge elsewhere.

	.align 16
    .globl tinker_0
    .type tinker_0, @function       
tinker_0:
	pushq	%rbp	 
	movq	%rsp, %rbp
	subq	$0x18, %rsp

	# code
	# init local vars
	movq	$0x01, -24(%rbp)

	# store 2 args into stack
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)

	# sum the 2 args plus the local var
	xorq	 %rax, %rax
	addq	-8(%rbp), %rax
	addq	-16(%rbp), %rax
	addq	-24(%rbp), %rax

	leave
	ret

	# ----------------------------------------------------------------------

	#########################################################
	# A funinst of a bare function taking 0 arguments
	# Returns the value: 0x42
	#########################################################
	.align 16
    .globl funinst_fun_0
    .type funinst_fun_0, @function       
funinst_fun_0:
	pushq	%rbp	 
	movq	%rsp, %rbp
	subq	$8, %rsp

	movq	$0x42, -8(%rbp)
	movq	-8(%rbp), %rax

	leave
	ret

	# ----------------------------------------------------------------------

	#########################################################
	# A funinst of a bare function taking 1 argument
	# Returns the value: arg0 + 0x01
	#########################################################
	.align 16
    .globl funinst_fun_1
    .type funinst_fun_1, @function       
funinst_fun_1:
	pushq	%rbp	 
	movq	%rsp, %rbp
	subq	$16, %rsp

	movq	%rdi, -8(%rbp)
	movq	$0x01, -16(%rbp)
	xorq	%rax, %rax
	addq	-8(%rbp), %rax
	addq	-16(%rbp), %rax

	leave
	ret

	# ----------------------------------------------------------------------

	.align 16
    .globl funinst_closure_0
    .type funinst_closure_0, @function       
funinst_closure_0:
	pushq	%rbp	 
	movq	%rsp, %rbp
	subq	$8, %rsp

	# code
	movq	%rdi, -8(%rbp)
	addq	$0x08, -8(%rbp)
	movq	-8(%rbp), %rax

	leave
	ret

	# ----------------------------------------------------------------------

	.align 16
    .globl invoke_funinst_0
    .type invoke_funinst_0, @function       
invoke_funinst_0:
	pushq	%rbp	 
	movq	%rsp, %rbp

	# Check if it is a general funinst pointer
	movq	%rdi, %r10
	andq	$tagval_funinst, %r10
	cmpq	$tagval_funinst, %r10
	jne		.Lifi0_bad_funinst

	# invoke it with 0 arguments (assume fun only pointer)

	# First, get the pointer to the function...
	movq	%rdi, %r10
	movq	$0xfffffffffffffff0, %r11
	andq	%r11, %r10

	call	*%r10

	leave
	ret

.Lifi0_bad_funinst:
	movq	$0x11223344AABBCCD0, %rax
	leave
	ret

	# ----------------------------------------------------------------------

	.align 16
    .globl invoke_funinst_1
    .type invoke_funinst_1, @function       
invoke_funinst_1:
	pushq	%rbp	 
	movq	%rsp, %rbp

	# Check if it is a general funinst pointer
	movq	%rdi, %r10
	andq	$tagval_funinst, %r10
	cmpq	$tagval_funinst, %r10
	jne		.Lifi1_bad_funinst

	# invoke it with 1 argument (assume fun only pointer)

	# First, get the pointer to the function...
	movq	%rdi, %r10
	movq	$0xfffffffffffffff0, %r11
	andq	%r11, %r10

	# Then construct arg0 for the function.
	# Shift arg1 for the invoker to be arg0 for the callee
	movq	%rsi, %rdi

	call	*%r10

	leave
	ret

.Lifi1_bad_funinst:
	movq	$0x11223344AABBCCD1, %rax
	leave
	ret
