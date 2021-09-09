    .text                               
    .globl scheme_entry                 
    #if !__CYGWIN__                     
        # Windows PECOFF doesn't have this directive.
        .type scheme_entry, @function   
    #endif                              
scheme_entry:
    movq %rsp, %r12                     # Save C stack pointer (safe on unix/windows
    #if __CYGWIN__                      
    movq %rcx, %rsp                     # Incoming arg0 -> lisp stack pointer. (Windows)
    #else                               
    movq %rdi, %rsp                     # Incoming arg0 -> lisp stack pointer. (Linux)
    #endif                              
    call L_scheme_entry                 
    movq %r12, %rsp                     # Restore C stack pointer.
    ret                                 
L_scheme_entry:
    movq $0x4, %rax                     # imm: fixnum 1
    # LET*: Store var X at -8
    movq %rax, -8(%rsp)                 
    movq $0x4, %rax                     # imm: fixnum 1
    movq %rax, -16(%rsp)                
    # Load var X at -8
    movq -8(%rsp), %rax                 
    addq -16(%rsp), %rax                # prim: fx+
    # LET*: Store var X at -16
    movq %rax, -16(%rsp)                
    movq $0x4, %rax                     # imm: fixnum 1
    movq %rax, -24(%rsp)                
    # Load var X at -16
    movq -16(%rsp), %rax                
    addq -24(%rsp), %rax                # prim: fx+
    # LET*: Store var Y at -24
    movq %rax, -24(%rsp)                
    # Load var Y at -24
    movq -24(%rsp), %rax                
    ret                                 
