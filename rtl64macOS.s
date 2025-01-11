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
    # pushq %rsi
    str x19, [sp, #-16]!
    
    # movq 16(%rsp),  %rbx    #arg: count (stdout width). we do NOT care if it == 1
    ldr x1, [sp, #32] 

    # movq 24(%rsp),  %rax    #arg: num
    ldr x0, [sp, #48]
    
    # cmpq $0,    %rax
    cmp x0, #0             # Сравниваем num с 0

    # jnl RTLWriteIntegerNotSigned
    b.ge RTLWriteIntegerNotSigned  # Если num >= 0, переходим к RTLWriteIntegerNotSigned

        # negq %rax
        neg x0, x0            # Инвертируем значение num
        # decq %rbx
        sub x1, x1, #1        # Уменьшаем count на 1

        # pushq $'-'
        mov x8, #'-'
        str x8, [sp, #-16]!

        # call RTLWriteChar 
        bl RTLWriteChar # Вызываем RTLWriteChar для записи символа '-'

    RTLWriteIntegerNotSigned:
        # xorq %rcx,  %rcx
        mov x2, #0 
        # pushq %rax
        str x0, [sp, #-16]! 
        # pushq %rbx
        str x1, [sp, #-16]! 
    
    RTLWriteIntegerPreCheckLoop:
        # testq %rax, %rax
        cmp x0, #0  
        # jz RTLWriteIntegerPreCheckLoopDone
        b.eq RTLWriteIntegerPreCheckLoopDone
        # incq %rcx
        add x2, x2, #1 
        # movq $10,   %rbx
        mov x1, #10 
        # xorq %rdx,  %rdx
        mov x3, #0 
        # idiv %rbx   

        mov x9, x0
        sdiv x0, x0, x1    #  X0 = X0 / X1
        mul x8, x0, x1     #  X8 = X0 * X1 
        sub x3, x9, x8     # X3 = X9 - X8
        # jmp RTLWriteIntegerPreCheckLoop
        b RTLWriteIntegerPreCheckLoop 
        
    RTLWriteIntegerPreCheckLoopDone:
    # testq %rcx, %rcx
    cmp x2, #0 
    # setz %dl                    #dl: (0 == rcx) ? 1 : 0
    cset x3, eq 
    # orb %dl,    %cl
    orr x3, x2, x2   # Выполняем OR между X2 и X3, результат сохраняем в X2
    
    # popq %rbx
    ldr x1, [sp], #16
    # popq %rax
    ldr x0, [sp], #16 
    # subq %rcx,  %rbx  %rbx = %rbx - %rcx
    sub x1, x2, x1
    
    # cmpq $0,    %rbx
    cmp x1, #0 

    # jle RTLWriteIntegerNotPadding
    b.le RTLWriteIntegerNotPadding
        # pushq %rcx
        str x2, [sp, #-16]! 
        RTLWriteIntegerPaddingLoop:
            # pushq $' '
            mov x8, #' '  
            # call RTLWriteChar
            bl RTLWriteChar
            # decq %rbx
            sub x1, x1, #1 
        # jnz RTLWriteIntegerPaddingLoop
        b.ne RTLWriteIntegerPaddingLoop
        # popq %rcx
        ldr x2, [sp], #16
    
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
    
    # movq RTLWriteIntegerBuffer@GOTPCREL(%rip), %rdi
    adrp x4, RTLWriteIntegerBuffer@PAGE
    add x4, x4, RTLWriteIntegerBuffer@PAGEOFF

    # addq %rcx,  %rdi                    #-||- %rdi = %rdi + %rcx
    add x4, x4, x2
    # decq %rdi                           #-||-
    sub x4, x4, #1  
    #LEA EDI,[OFFSET RTLWriteIntegerBuffer+ECX-1]
    
    # pushq %rcx              #we dont care if (count < real_num_width)
    str x2, [sp, #-16]! 

    RTLWriteIntegerLoop:
        # movq $10,   %rsi
        mov x19, #10
        # xorq %rdx,  %rdx
        mov x3, #0
        # idiv %rsi
        mov x9, x0
        sdiv x0, x0, x19    #  X0 = X0 / X19
        mul x8, x0, x19     #  X8 = X0 * X19 
        sub x3, x9, x8     # X3 = X9 - X8

        #convert to string
        # movq %rdx,  %rbx    # ~= lea '0'(%rdx), %rbx  movq %rdx,  %rbx + addq $'0',  %rbx
        add x1, x3, #'0'   

        # movb %bl, (%rdi)
        strb w1, [x4]
        # decq %rdi
        sub x4, x4, #1 
        cmp x4, #0
    # loop RTLWriteIntegerLoop
    b.ne RTLWriteIntegerLoop
    
    # popq %rcx
    ldr x2, [sp], #16

    # movq $0x2000004,    %rax                            # syscall 4 == Write
    mov x0, #64     # syscall 64 == Write
    # movq $1,            %rdi                            # p1, write_to == 1 == stdout
    mov x4, #1 
    #movq RTLWriteIntegerBuffer@GOTPCREL(%rip),  %rsi    # p2, write_from == buf addr
    adrp x19, RTLWriteIntegerBuffer@PAGE
    add x19, x19, RTLWriteIntegerBuffer@PAGEOFF 

    # movq %rcx,          %rdx                            # p3, count
    mov x2, x3 
    # syscall
    svc #0 

    # popq %rsi
    ldr x19, [sp], #16
    ldr x30, [sp], #16 # take ret addr from stack
    add sp, sp, #32 
    # ret  $16    #take ret addr from stack and remove 2 args
    ret
    
#------------------------------------------
#-----------------WriteLn------------------
#------------------------------------------    
RTLWriteLn: 
    # pushq   $13             #13 == 0xD == CR
    mov x8, #13 
    str x8, [sp, #-16]!   
    # call    RTLWriteChar
    bl RTLWriteChar
 
    # pushq   $10             #10 == 0xA == LF
    mov x8, #10 
    str x8, [sp, #-16]!  
    # call    RTLWriteChar
    bl RTLWriteChar
    ret
   

ReadCharEx:
    pushall
  
    # movq    $0x2000003, %rax        #syscall #3 == Read();
    mov x0, #63              # syscall #63 == Read();
    # xorq    %rdi, %rdi              #p1 == read_from == 0 == stdin
    mov x4, #0 
    # movq    ReadCharBuffer@GOTPCREL(%rip), %rsi   #p2 == write_to == buffer
    adrp x19, ReadCharBuffer@PAGE
    add x19, x1, ReadCharBuffer@PAGEOFF  # p2 == write_to == buffer

    # movq    $1, %rdx                #p3 == count == single_byte
    mov x3, #1 
    # syscall
    svc #0 

    #test against value read on prev. step 
    #beware of empty %rbx if ReadCharEx is the first function to be called
    #testq   %rbx, %rbx    
    #setz    %bl                     #al: (0 == rbx) ? 1:0
    #orb     %bl, (IsEOF)

    #test against num of bytes that were actually read
    # cmpq    $0, %rax
    cmp x0, #0
    #setz    %bl                         #bl: (0 == bytes_read) ? 1:0
    cset w1, eq
    # mov     IsEOF@GOTPCREL(%rip), %r8   # address of IsEOF in r8
    adrp x8, IsEOF@PAGE
    add x8, x8, IsEOF@PAGEOFF  # address of IsEOF in x8  
    ldrb w8, [x3]  # Загружает байт из адреса IsEOF в регистр w8!!!!
    # xor     %rcx, %rcx     
    mov x2, 0             
    # orb     %bl, (%r8) Логическое ИЛИ значения в bl с байтом по адресу r8
    orr w8, w8, w1 
    strb w8, [x8] # Сохраняет байт из w2 обратно по адресу IsEOF.
    popall
    ret
    
ReadCharInit:
    # mov     ReadCharInited@GOTPCREL(%rip), %r8   # address of ReadCharInited in r8    
    adrp x8, ReadCharInited@PAGE
    add x8, x8, ReadCharInited@PAGEOFF
    ldrb w8, [x8] 
    # cmpb    $0, (%r8)
    cmp w8, #0
    # jnz     ReadInitDone
    b.ne ReadInitDone
        
        # call    ReadCharEx
        bl ReadCharEx 
        # movq    ReadCharInited@GOTPCREL(%rip), %r8   # address of ReadCharInited in r8 
        adrp x8, ReadCharInited@PAGE
        add x8, x8, ReadCharInited@PAGEOFF
        mov w8, #1 
        # movb    $1, (%r8)
        strb w8, [x8]

    ReadInitDone:
    ret

#------------------------------------------
#----------------ReadChar------------------
#------------------------------------------    
RTLReadChar:
    # call    ReadCharInit
    bl      ReadCharInit
    # xorq    %rax, %rax
    mov     x0, #0 
    # movq    ReadCharBuffer@GOTPCREL(%rip), %r8
    adrp    x8, ReadCharBuffer@PAGE
    add     x8, x8, ReadCharBuffer@PAGEOFF
    # movb    (%r8), %al    # == movzxbl (ReadCharBuffer), %rax
    ldrb    w0, [x8] 
    
    # call    ReadCharEx
    bl      ReadCharEx
    ret

#------------------------------------------
#--------------ReadInteger-----------------
#------------------------------------------    
RTLReadInteger:
    # call ReadCharInit
    bl ReadCharInit
    pushall

    # xorq %rax, %rax
    mov x0, #0
    # movq $1, %rcx
    mov x2, #1 
    
    ReadIntegerSkipWhiteSpace:
        # mov     IsEOF@GOTPCREL(%rip), %r8   # address of IsEOF in r8
        adrp x8, IsEOF@PAGE
        add x8, x8, IsEOF@PAGEOFF
        ldrb w8, [x8] 
        # cmpb $0,    (%r8)                 #cmp with $1 and it works. no idea why {!}
        cmp w8, #0 
        # jnz  ReadIntegerDone          #{!}{!!} why jz works and jnz 
        b.ne ReadIntegerDone
        # movq ReadCharBuffer@GOTPCREL(%rip), %r8
        adrp x8, ReadCharBuffer@PAGE
        add x8, x8, ReadCharBuffer@PAGEOFF
        ldrb w8, [x8]
        # cmpb $0,    (%r8)
        cmp w8, #0 
        # je ReadIntegerSkipWhiteSpaceDone
        b.eq ReadIntegerSkipWhiteSpaceDone
        # movq ReadCharBuffer@GOTPCREL(%rip), %r8
        adrp x8, ReadCharBuffer@PAGE
        add x8, x8, ReadCharBuffer@PAGEOFF
        ldrb w8, [x8]
        # cmpb $32,   (%r8)        #32d == 0x20 == space
        cmp w8, #32
        # ja ReadIntegerSkipWhiteSpaceDone
        b.hi ReadIntegerSkipWhiteSpaceDone # branch if higher
        # call ReadCharEx
        bl ReadCharEx
        # jmp ReadIntegerSkipWhiteSpace
        b ReadIntegerSkipWhiteSpace

    ReadIntegerSkipWhiteSpaceDone:
    # movq ReadCharBuffer@GOTPCREL(%rip), %r8
    adrp x8, ReadCharBuffer@PAGE
    add x8, x8, ReadCharBuffer@PAGEOFF
    ldrb w8, [x8] 

    # cmpb $'-',  (%r8)
    cmp w8, #'-' 
    #jne ReadIntegerNotSigned
    b.ne ReadIntegerNotSigned  
        #negq %rcx                   #rcx stores -1 or 1 and will multiply the result
        neg x2, x2 
        # call ReadCharEx
        bl ReadCharEx
        
    ReadIntegerNotSigned:
    ReadIntegerLoop:
        # xorq %rbx, %rbx
        mov x1, #0
        # movq ReadCharBuffer@GOTPCREL(%rip), %r8
        adrp x8, ReadCharBuffer@PAGE
        add x8, x8, ReadCharBuffer@PAGEOFF   
        ldrb w8, [x8]                # movb (%r8),  %bl

        
        # cmpb $'0',  %bl 
        cmp w8, #'0' 
        # jb ReadIntegerDone
        b.lt ReadIntegerDone
        # cmpb $'9',  %bl
        cmp w8, #'9' 
        # ja ReadIntegerDone
        b.gt ReadIntegerDone
        
        # imul $10,   %rax            #rax *= 10
        mov x9, #10 
        mul x0, x0, x9 
        #cast string to int:
        # subq $'0',  %rbx            # == lea -'0'(%rax,%rbx,1),  %rax
        sub w8, w8, #'0'
        #addq %rbx,  %rax 
        add x0, x0, x1 
                  
        
        # call ReadCharEx
        bl ReadCharEx 
        # jmp ReadIntegerLoop
        b ReadIntegerLoop
    
    ReadIntegerDone:

    # imul %rcx               #rax *= rcx  (rcx={1;-1})
    mul x0, x0, x2
    # movq %rax, (%rsp)       #rax is on top of stack (look at pushall())
    str x0, [sp]
    popall
    ret

#------------------------------------------
#------------------ReadLn------------------
#------------------------------------------  
#it does NOT skip input if %RAX is nulled
#so put there some trash before calling it
#------------------------------------------   
RTLReadLn:
    # call ReadCharInit
    bl ReadCharInit
    # mov     IsEOF@GOTPCREL(%rip), %r8   # address of IsEOF in r8
    adrp x8, IsEOF@PAGE             
    add x8, x8, IsEOF@PAGEOFF
    ldrb w8, [x8]  
    # cmpb    $0, (%r8)
    cmp w8, #0 
    # jne     ReadLnDone
    b.ne ReadLnDone
        # movq ReadCharBuffer@GOTPCREL(%rip), %r8
        adrp x8, ReadCharBuffer@PAGE   
        add x8, x8, ReadCharBuffer@PAGEOFF
        # movb (%r8), %bl
        # cmpb    $10, %bl  #cmp to LF
        ldrb w8, [x8] # Сравнение w8 с 10 (LF)

        # je      ReadLnDone
        b.eq ReadLnDone
    
            # call    ReadCharEx
            bl ReadCharEx                   
      
            # jmp     RTLReadLn
            b RTLReadLn
    
    ReadLnDone:
    ret

#------------------------------------------
#-------------------EOF--------------------
#------------------------------------------         
RTLEOF:
    # xorq    %rax, %rax
    mov x0, #0
    # mov     IsEOF@GOTPCREL(%rip), %r8   # address of IsEOF in r8
    adrp x8, IsEOF@PAGE         
    add x8, x8, IsEOF@PAGEOFF
    # movb    (%r8), %al
    ldrb w0, [x8]
    ret

#------------------------------------------
#------------------EOLN--------------------
#------------------------------------------    
RTLEOLN:
    # movq    ReadCharBuffer@GOTPCREL(%rip), %r8
    adrp x8, ReadCharBuffer@PAGE       
    add x8, x8, ReadCharBuffer@PAGEOFF
    ldrb w8, [x8]
    # cmpb    $10, (%r8)       #cmp to LF
    cmp w8, #10 
    # sete    %dl                         #bero's legacy
    cset w3, eq 
    ret

#------------------------------------------
#------------------Halt--------------------
#------------------------------------------        
RTLHalt:
    mov x0, #93
    mov x4, #0                    
    svc #0 

    
#//==----------------------------------==//
#//----------------ENTRY-----------------//
#//==----------------------------------==//
#call some tests here or post introduction
#dont need to allocate(prepare) stack pages
#------------------------------------------
StubEntryPoint:
    mov sp, x27 
    adrp x19, RTLFunctionTable@PAGE
    add x19, x19, RTLFunctionTable@PAGEOFF

# movq RTLFunctionTable@GOTPCREL(%rip), %rsi      # store functionTable and don't change %rsi
ProgramEntryPoint:

#------------------------------------------
# code generated by btpc.pas goes here

