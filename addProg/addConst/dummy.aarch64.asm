    // subq	$0x8, %rsp
    mov x28, sp  
    sub sp, sp, #16
    
    //pushq	$0x1
    mov x0, #1 
    str x0, [sp, #-16]!

    //addq	$0x2, (%rsp)
    ldr x0, [sp], #16
 	add x0, x0, #2  
    str x0, [sp, #-16]!
	
    //popq	%rax
    ldr x0, [sp], #16
    
	//movq	%rax, (%rbp)
    str x0, [x28]

    //pushq	$0x4
    mov x0, #4
    str x0, [sp, #-16]!

    //addq	$0x5, (%rsp)
    ldr x0, [sp], #16
    add x0, x0, #5
     str x0, [sp, #-16]!

    //popq	%rax
    ldr x0, [sp], #16
    
    //movq	%rax, -0x8(%rbp)
    str x0, [x28, #-16]
    
    
    


    