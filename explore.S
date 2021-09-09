	# a file where I can just add some garbage into the C runtime in asm.

	# c_true and c_false labels are globals defined in main.c

    .text
    .globl explore
    .type explore, @function
explore:
    mov %rdi, %rax
    mov $30, %rax # arg1
    cmp $29, %rax # arg0
    setg %al
    movzbl %al, %rax
    #sal $6, %al
    #or $0x2f, %al

    mov $10, %rax
    sub $2, %rax
    ret
