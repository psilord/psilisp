    .text                               
    .globl scheme_entry                 
    .type scheme_entry, @function       
scheme_entry:
    mov %rsp, %rcx                      # Save C stack pointer
    mov %rdi, %rsp                      # Incoming arg0 -> lisp stack pointer.
    call L_scheme_entry                 
    mov %rcx, %rsp                      # Restore C stack pointer.
    ret                                 
L_scheme_entry:
    mov $0x4, %rax                      # imm: fixnum 1
    # LET*: Store var X at -8
    mov %rax, -8(%rsp)                  
    mov $0x4, %rax                      # imm: fixnum 1
    mov %rax, -16(%rsp)                 
    # Load var X at -8
    mov -8(%rsp), %rax                  
    add -16(%rsp), %rax                 # prim: fx+
    # LET*: Store var X at -16
    mov %rax, -16(%rsp)                 
    mov $0x4, %rax                      # imm: fixnum 1
    mov %rax, -24(%rsp)                 
    # Load var X at -16
    mov -16(%rsp), %rax                 
    add -24(%rsp), %rax                 # prim: fx+
    # LET*: Store var Y at -24
    mov %rax, -24(%rsp)                 
    # Load var Y at -24
    mov -24(%rsp), %rax                 
    ret                                 
