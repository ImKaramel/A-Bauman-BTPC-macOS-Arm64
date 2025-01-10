# How to build:
# as rtl64.s -o rtl64.o
# ld rtl64.o -e _main -o rtl64 -lSystem
# ./rtl64
#
#==========================================
#------------------data--------------------
#==========================================
.section __DATA,__data

RTLWriteIntegerBuffer:
    .byte 0x3c, 0x32, 0x30, 0x30, 0x36, 0x5f, 0x42, 0x54, 0x50, 0x43, 0x5f
    .byte 0x61, 0x75, 0x74, 0x68, 0x6f, 0x72, 0x5f, 0x42, 0x65, 0x6e, 0x6a
    .byte 0x61, 0x6d, 0x69, 0x6e, 0x5f, 0x52, 0x6f, 0x73, 0x73, 0x65, 0x61
    .byte 0x75, 0x78, 0x5f, 0x62, 0x65, 0x6e, 0x6a, 0x61, 0x6d, 0x69, 0x6e
    .byte 0x40, 0x72, 0x6f, 0x73, 0x73, 0x65, 0x61, 0x75, 0x78, 0x2e, 0x63
    .byte 0x6f, 0x6d, 0x3e, 0x3c, 0x32, 0x30, 0x31, 0x37, 0x5f, 0x4c, 0x69
    .byte 0x6e, 0x75, 0x78, 0x50, 0x6f, 0x72, 0x74, 0x5f, 0x61, 0x75, 0x74
    .byte 0x68, 0x6f, 0x72, 0x5f, 0x41, 0x6e, 0x74, 0x68, 0x6f, 0x6e, 0x79
    .byte 0x5f, 0x42, 0x65, 0x6c, 0x79, 0x61, 0x65, 0x76, 0x5f, 0x61, 0x6e
    .byte 0x74, 0x6f, 0x6e, 0x73, 0x64, 0x6c, 0x40, 0x67, 0x6d, 0x61, 0x69
    .byte 0x6c, 0x2e, 0x63, 0x6f, 0x6d, 0x3e
         
ReadCharBuffer:
    .byte 0x3c
                  
ReadCharInited: 
    .byte 0
            
IsEOF:
    .byte 0
  
RTLFunctionTable:
    .quad RTLHalt
    .quad RTLWriteChar
    .quad RTLWriteInteger
    .quad RTLWriteLn
    .quad RTLReadChar
    .quad RTLReadInteger
    .quad RTLReadLn
    .quad RTLEOF
    .quad RTLEOLN
            
Copyright:                        
    
      
#==========================================
#-------------------bss--------------------
#==========================================    
.section __BSS,__bss
   
    .macro pushall
        # pushq %rdi
        str x4, [sp, #-16]!
        # pushq %rsi
        str x19, [sp, #-16]!
        # pushq %rbp
        str x27, [sp, #-16]!
        # pushq %rsp
        str sp, [sp, #-16]!
        # pushq %rdx
        str x3, [sp, #-16]!
        # pushq %rcx
        str x2, [sp, #-16]!
        # pushq %rbx
        str x1, [sp, #-16]!
        # pushq %rax
        str x0, [sp, #-16]!
        
    .endm
    

    .macro popall
        ldr x0, [sp], #16    // pop %rax
        ldr x1, [sp], #16     // pop %rbx
        ldr x2, [sp], #16      // pop %rcx
        ldr x3, [sp], #16     // pop %rdx
        ldr sp, [sp], #16    // pop %rsp
        ldr x27, [sp], #16    // pop %rbp
        ldr x19, [sp], #16    // pop %rsi
        ldr x4,  [sp], #16       // pop %rdi
    .endm


    .macro parleft
        mov x8, #'{'
        str x8, [sp, #-16]!
        bl RTLWriteChar  
    .endm
    
    .macro parright
        mov x8, #'}'
        str x8, [sp, #-16]!
        bl RTLWriteChar  
    .endm

    
    .macro space
        mov x8, #' '
        str x8, [sp, #-16]!
        bl RTLWriteChar  
    .endm
        
    .macro raxchar
        str x8, [sp, #-16]!
        bl RTLWriteChar  
    .endm
    
    # .macro raxint
    #    pushq %rax
    #    pushq $1
    #    #pushq $1
    #    call RTLWriteInteger
    #    #addq $16, %rsp
    #.endm
   
#==========================================
#------------------text--------------------
#==========================================
.section __TEXT,__text

.global _main
_main:
    b StubEntryPoint

/*
1. X  RTLHalt           — остановка программы,
2. X  RTLWriteChar      — запись char’а на stdout,
3. X  RTLWriteInteger   — запись целого на stdout, принимает два параметра: число и ширину вывода,
4. X  RTLWriteLn        — выводит на stdout символ новой строки (13, 10),
5. X  RTLReadChar       — считывает символ из stdin, результат кладёт в EAX,
6. X  RTLReadInteger    — считывает целое из stdin, результат кладёт в EAX,
7. X  RTLReadLn         — пропускает стандартный ввод до конца файла или ближайшего перевода строки,
8. X  RTLEOF            — возвращает в EAX число 1, если достигнут конец файла (следующий символ прочитать невозможно) или 0 в противном случае,
9. X  RTLEOLN           — возвращает в EAX число 1, если следующий символ \n, 0 — в противном случае.
**/
#------------------------------------------
#----------------WriteChar-----------------
#------------------------------------------
RTLWriteChar:
    pushall
    # movq    %rsp,   %rbp    #make stack frame
    mov x27, sp 
    # syscall #1 == Write();
    mov x0, #1                               
    # movq    $1,     %rdi    #param1 == write_to == 1 == stdout
    mov x4, #1  
    # movq    %rbp,   %rsi    #p2 == write_from == %rbp == top_of_stack

    add x19, x19, #128      # reach top of stack:[0-ret,16-arg1,32-arg2]
                            # we have all regs pushed so top is really really far

    mov x3, #1     #p3 == count == single_byte
    svc #0         # syscall
    popall
    # ret     $8
    ret
#------------------------------------------
#--------------WriteInteger----------------
#------------------------------------------    
RTLWriteInteger:
    pushq %rsi
    #pushq %rbp
    #movq %rsp,  %rbp
    
    movq 16(%rsp),  %rbx    #arg: count (stdout width). we do NOT care if it == 1
    movq 24(%rsp),  %rax    #arg: num
    
    cmpq $0,    %rax
    jnl RTLWriteIntegerNotSigned

        negq %rax
        decq %rbx
        pushq $'-'
        call RTLWriteChar 

    RTLWriteIntegerNotSigned:
    xorq %rcx,  %rcx
    pushq %rax
    pushq %rbx
    
    RTLWriteIntegerPreCheckLoop:
        testq %rax, %rax
        jz RTLWriteIntegerPreCheckLoopDone
        incq %rcx
        movq $10,   %rbx
        xorq %rdx,  %rdx
        idiv %rbx          
        
        jmp RTLWriteIntegerPreCheckLoop
        
    RTLWriteIntegerPreCheckLoopDone:
    testq %rcx, %rcx
    setz %dl                    #dl: (0 == rcx) ? 1 : 0
    orb %dl,    %cl
    
    popq %rbx
    popq %rax
    subq %rcx,  %rbx
    
    cmpq $0,    %rbx
    jle RTLWriteIntegerNotPadding
        pushq %rcx
    
        RTLWriteIntegerPaddingLoop:
            pushq $' '
            call RTLWriteChar
            decq %rbx
        jnz RTLWriteIntegerPaddingLoop
        popq %rcx
    
    RTLWriteIntegerNotPadding:
    #find last digit's address:     
    #we write from right to left cuz each time we divide number by 10, we only know it's right-most digit
    #so we need to write right-most digit into right-most byte:
    
    #e.g: num=-123; count=4
    #               |  < |  = |  > |    |  < |... 
    #init buffer:   |0x3b|0x3d|0x3e|0x00|0x3b|...
    #1st iter:      |0x3b|0x3d| 3d |0x00|0x3b|...
    #2st iter:      |0x3b| 2d | 3d |0x00|0x3b|...
    #3st iter:      | 1d | 2d | 3d |0x00|0x3b|...
    
    movq RTLWriteIntegerBuffer@GOTPCREL(%rip), %rdi
    # movq $RTLWriteIntegerBuffer, %rdi   #leaq RTLWriteIntegerBuffer-1(%rcx), %rdi 
    addq %rcx,  %rdi                    #-||-
    decq %rdi                           #-||-
    #LEA EDI,[OFFSET RTLWriteIntegerBuffer+ECX-1]
    
    pushq %rcx              #we dont care if (count < real_num_width)

    RTLWriteIntegerLoop:
        movq $10,   %rsi
        xorq %rdx,  %rdx
        idiv %rsi
        #convert to string
        movq %rdx,  %rbx    # ~= lea '0'(%rdx), %rbx
        addq $'0',  %rbx    
        
        movb %bl, (%rdi)
        decq %rdi
    loop RTLWriteIntegerLoop
    
    popq %rcx
    
    movq $0x2000004,    %rax                            # syscall 4 == Write
    movq $1,            %rdi                            # p1, write_to == 1 == stdout
    movq RTLWriteIntegerBuffer@GOTPCREL(%rip),  %rsi    # p2, write_from == buf addr
    movq %rcx,          %rdx                            # p3, count

    syscall

    
    #popq %rbp
    popq %rsi
    ret  $16    #take ret addr from stack and remove 2 args
    
#------------------------------------------
#-----------------WriteLn------------------
#------------------------------------------    
RTLWriteLn: 
    # pushq   $13             #13 == 0xD == CR
    mov x8, #13 
    str x8, [sp, #-16]!   
    # call    RTLWriteChar
    bl RTLWriteChar
    # addq    $8, %rsp
    # pushq   $10             #10 == 0xA == LF
    mov x8, #10 
    str x8, [sp, #-16]!  
    # call    RTLWriteChar
    bl RTLWriteChar
    # addq    $8, %rsp
    ret
   

ReadCharEx:
    pushall
    #movq    %rsp, %rbp
    
    #movq    %rax, %rbx              #copy to %rbx cuz %rax will be used for syscall
    
    movq    $0x2000003, %rax        #syscall #3 == Read();
    xorq    %rdi, %rdi              #p1 == read_from == 0 == stdin
    movq    ReadCharBuffer@GOTPCREL(%rip), %rsi   #p2 == write_to == buffer
    movq    $1, %rdx                #p3 == count == single_byte
    syscall

    #test against value read on prev. step 
    #beware of empty %rbx if ReadCharEx is the first function to be called
    #testq   %rbx, %rbx    
    #setz    %bl                     #al: (0 == rbx) ? 1:0
    #orb     %bl, (IsEOF)

    #test against num of bytes that were actually read
    cmpq    $0, %rax
    setz    %bl                         #bl: (0 == bytes_read) ? 1:0
    mov     IsEOF@GOTPCREL(%rip), %r8   # address of IsEOF in r8
    xor     %rcx, %rcx                  
    orb     %bl, (%r8)

    popall
    ret
    
ReadCharInit:
    mov     ReadCharInited@GOTPCREL(%rip), %r8   # address of ReadCharInited in r8    
    cmpb    $0, (%r8)
    jnz     ReadInitDone
        
        call    ReadCharEx
        movq    ReadCharInited@GOTPCREL(%rip), %r8   # address of ReadCharInited in r8 
        movb    $1, (%r8)

    ReadInitDone:
    ret

#------------------------------------------
#----------------ReadChar------------------
#------------------------------------------    
RTLReadChar:
    call    ReadCharInit

    xorq    %rax, %rax
    movq    ReadCharBuffer@GOTPCREL(%rip), %r8
    movb    (%r8), %al    # == movzxbl (ReadCharBuffer), %rax
    
    call    ReadCharEx

    ret

#------------------------------------------
#--------------ReadInteger-----------------
#------------------------------------------    
RTLReadInteger:
    call ReadCharInit
    
    pushall
    #movq %rsp,  %rbp
    
    xorq %rax, %rax
    #lea 1(%rax), %rcx
    movq $1, %rcx
    
    ReadIntegerSkipWhiteSpace:
        mov     IsEOF@GOTPCREL(%rip), %r8   # address of IsEOF in r8
        cmpb $0,    (%r8)                 #cmp with $1 and it works. no idea why {!}
        jnz  ReadIntegerDone          #{!}{!!} why jz works and jnz 
        movq ReadCharBuffer@GOTPCREL(%rip), %r8
        cmpb $0,    (%r8)
        je ReadIntegerSkipWhiteSpaceDone
        movq ReadCharBuffer@GOTPCREL(%rip), %r8
        cmpb $32,   (%r8)        #32d == 0x20 == space
        ja ReadIntegerSkipWhiteSpaceDone
        
        call ReadCharEx
        
        jmp ReadIntegerSkipWhiteSpace
        
    ReadIntegerSkipWhiteSpaceDone:
    movq ReadCharBuffer@GOTPCREL(%rip), %r8
    cmpb $'-',  (%r8)
    jne ReadIntegerNotSigned
        
        negq %rcx                   #rcx stores -1 or 1 and will multiply the result
        call ReadCharEx
        
    ReadIntegerNotSigned:
    ReadIntegerLoop:
        xorq %rbx, %rbx
        movq ReadCharBuffer@GOTPCREL(%rip), %r8
        movb (%r8),  %bl
        
        cmpb $'0',  %bl 
        jb ReadIntegerDone
        cmpb $'9',  %bl
        ja ReadIntegerDone
        
        imul $10,   %rax            #rax *= 10
        #cast string to int:
        subq $'0',  %rbx            # == lea -'0'(%rax,%rbx,1),  %rax
        addq %rbx,  %rax  
                  
        
        call ReadCharEx
        jmp ReadIntegerLoop
    
    ReadIntegerDone:

    #movq $1, %rcx

    imul %rcx               #rax *= rcx  (rcx={1;-1})

    movq %rax, (%rsp)       #rax is on top of stack (look at pushall())
    
    popall
    ret

#------------------------------------------
#------------------ReadLn------------------
#------------------------------------------  
#it does NOT skip input if %RAX is nulled
#so put there some trash before calling it
#------------------------------------------   
RTLReadLn:
    call ReadCharInit
    
    mov     IsEOF@GOTPCREL(%rip), %r8   # address of IsEOF in r8
    cmpb    $0, (%r8)
    jne     ReadLnDone
        movq ReadCharBuffer@GOTPCREL(%rip), %r8
        movb (%r8), %bl
        cmpb    $10, %bl  #cmp to LF
        je      ReadLnDone
    
            call    ReadCharEx
            jmp     RTLReadLn
    
    ReadLnDone:
    ret

#------------------------------------------
#-------------------EOF--------------------
#------------------------------------------         
RTLEOF:
    xorq    %rax, %rax
    mov     IsEOF@GOTPCREL(%rip), %r8   # address of IsEOF in r8
    movb    (%r8), %al
    ret

#------------------------------------------
#------------------EOLN--------------------
#------------------------------------------    
RTLEOLN:
    movq    ReadCharBuffer@GOTPCREL(%rip), %r8
    cmpb    $10, (%r8)       #cmp to LF
    sete    %dl                         #bero's legacy
    ret

#------------------------------------------
#------------------Halt--------------------
#------------------------------------------        
RTLHalt:
    mov x0, #93                  // syscall #93 == Exit()
    mov x4, #0                   // exit process state
    svc #0 


#------------------------------------------#
#-                 Tests                  -#
#------------------------------------------#
Test1:
    #test 1: ReadInt, WriteChar, WriteInt, WriteLine
    #test input(-1337 + space): "-1337 "

    #expected: 
    #%entered_num%- 1234
    #A

    call RTLReadInteger
    call RTLReadLn

    pushq %rax
    pushq $1
    call RTLWriteInteger
    #addq $16, %rsp
    
    #space

    pushq $-1234
    pushq $6
    call RTLWriteInteger   
    #addq $16, %rsp
    call RTLWriteLn

    pushq $'A'
    call RTLWriteChar  
    call RTLWriteLn
    
    call RTLHalt
    
Test2:
    #test 2: ReadChar, EndOfLine, EndOfFile, WriteInt
    #test input(B + linebreak): "B\n"

    #expected
    #{B66}{10}{1}{*1}

    call RTLReadChar
    parleft
    raxchar                     #should be "B"
    #raxint                      #should be 66(B)
    pushq %rax
    pushq $1
    call RTLWriteInteger    
    parright
    
    #
    parleft
    xorq %rax, %rax
    movq ReadCharBuffer@GOTPCREL(%rip), %r8
    movb (%r8), %al #should be 10 == LF
    # raxint
    pushq %rax
    pushq $1
    call RTLWriteInteger
    parright
    #
    
    xorq %rdx, %rdx
    call RTLEOLN
    parleft
    pushq %rdx
    pushq $1
    call RTLWriteInteger        #should be 1 cuz of linebreak
    #addq $16, %rsp
    parright
    

    call RTLEOF
    parleft
    raxchar
    #raxint
    pushq %rax
    pushq $1
    call RTLWriteInteger
    parright
    
    call RTLHalt

Test3:
    #test3: ReadLn, ReadChar
    #test input(linebreak + A):"\nA" 

    #expected
    #{A 65}
    
#put smth into %rax in case IsEOF wont be triggered after first char
    movq $1, %rax   
    call RTLReadLn
    
    call RTLReadChar
    parleft
    raxchar             #should be "A"
    space
    #raxint              #should be 65(A)
    pushq %rax
    pushq $1
    call RTLWriteInteger
    parright
    
    call RTLHalt


Test4:
    pushq $-1234
    pushq $6
    call RTLWriteInteger
    call RTLWriteLn
    call RTLHalt

    
#//==----------------------------------==//
#//----------------ENTRY-----------------//
#//==----------------------------------==//
#call some tests here or post introduction
#dont need to allocate(prepare) stack pages
#------------------------------------------
StubEntryPoint:
    mov sp, x27 
    adrp x10, RTLFunctionTable@PAGE
    add x10, x10, RTLFunctionTable@PAGEOFF
    mov x10, [x10] 
    mov x10, x10   
# movq RTLFunctionTable@GOTPCREL(%rip), %rsi      # store functionTable and don't change %rsi
ProgramEntryPoint:

#------------------------------------------
# code generated by btpc.pas goes here

