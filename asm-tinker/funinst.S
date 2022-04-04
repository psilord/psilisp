
	.data
    .globl addr_mask
	addr_mask: .quad 0xfffffffffffffff0

    .globl bad_value
	bad_value: .quad 0x11223344aabbccdd



    .text                               
	.equ tagval_funinst, 0b1101
	.equ tagval_funinst_mask, 0b1111
	.equ tagval_funinst_fun, 0b1101
	.equ tagval_funinst_closure, 0b1111

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

	#########################################################
	# A funinst of a closure taking 0 argument2, and with 1 closed over arg.
	# Returns the value: value of closed over arg + 1.
	#########################################################
	.align 16
    .globl funinst_closure_0_1
    .type funinst_closure_0_1, @function       
funinst_closure_0_1:
	# C goop
	pushq	%rbp	 
	movq	%rsp, %rbp
	subq	$8, %rsp

	# code

	# unsafely assume %r11 is a tagged closure reference
	# convert to a real address in %r11
	movq	addr_mask(%rip), %r10
	andq	%r10, %r11

	# cl[0]: the function
	# cl[1]: the 0th argument (assume fixnum)
	movq	8(%r11), %rax
	addq	$0x01, %rax

	# C goop
	leave
	ret

	# ----------------------------------------------------------------------

	# Invoke a function or a closure with 0 argumnts.
	# Since this is a C boundary function, it sorta has to play nice with
	# C's argument passing conventions.

	.align 16
    .globl invoke_funinst_0
    .type invoke_funinst_0, @function       
invoke_funinst_0:
	pushq	%rbp	 
	movq	%rsp, %rbp

	pushq	%r12
	movq	addr_mask(%rip), %r10

	# Validate if C's arg0 is a FUNINST of some kind
	movq	%rdi, %r12
	andb	$tagval_funinst_mask, %r12b

	# Check if FUN
	cmpb	$tagval_funinst_fun, %r12b
	je		.Lifi0_invoke_fun

	# Check if CLOSURE
	cmpb	$tagval_funinst_closure, %r12b
	je		.Lifi0_invoke_closure

	# Otherwise, it is bad, bail!
	movq	bad_value(%rip), %rax
	jmp		.Lifi0_done

.Lifi0_invoke_fun:
	# convert tagged FUN pointer to actual function address
	movq	%rdi, %r12
	andq	%r10, %r12

	# setup 0 arguments
	# call the function
	call	*%r12

	jmp		.Lifi0_done

.Lifi0_invoke_closure:
	# Convert tagged closure representation to actual closure address
	movq	%rdi, %r12
	andq	%r10, %r12

	# extract tagged function pointer (element 0) from closure
	movq	0(%r12), %r12

	# convert tagged function pointer to actual function pointer
	andq	%r10, %r12

	# Call the function at %r12, passing in the tagged closure in %r11
	mov		%rdi, %r11
	# setup 0 other arguments
	# call the function
	call	*%r12

	jmp		.Lifi0_done

.Lifi0_done:
	popq	%r12
	leave
	ret

	# ----------------------------------------------------------------------

	# XXX fixme.

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
	pushq	%rbx

	# First, get the pointer to the function...
	movq	%rdi, %rbx
	movq	addr_mask(%rip), %r11
	andq	%r11, %rbx

	# Then construct arg0 for the function.
	# Shift arg1 for the invoker to be arg0 for the callee
	movq	%rsi, %rdi

	call	*%rbx

	popq	%rbx
	leave
	ret

.Lifi1_bad_funinst:
	movq	bad_value(%rip), %rax
	leave
	ret
