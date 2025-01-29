// How to build:
// as rtl64macOS.s -o rtl64macOS.o
// ld rtl64macOS.o -e _main -o rtl64macOS -lSystem -syslibroot $(xcrun --show-sdk-path)
// ld -o asm asm.o -lSystem -syslibroot $(xcrun --show-sdk-path) -e _start
// ./rtl64macOS

// ==========================================
// ------------------data--------------------
// ==========================================
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
        str x4, [sp, #-16]!    // push %rdi
        str x19, [sp, #-16]!   // push %rsi
        str x27, [sp, #-16]!   // push %rbp
        mov x8, sp   
        str x8, [sp, #-16]!    // push %rsp
        str x3, [sp, #-16]!    // push %rdx
        str x2, [sp, #-16]!    // push %rcx
        str x1, [sp, #-16]!    // push %rbx
        str x0, [sp, #-16]!    // push %rax
    .endm

    .macro popall
        ldr x0, [sp], #16      // pop %rax
        ldr x1, [sp], #16      // pop %rbx
        ldr x2, [sp], #16      // pop %rcx
        ldr x3, [sp], #16      // pop %rdx
        ldr x8, [sp], #16
        mov sp, x28            // pop %rsp
        ldr x27, [sp], #16     // pop %rbp
        ldr x19, [sp], #16     // pop %rsi
        ldr x4, [sp], #16
    .endm

    .macro parleft
        mov x0, #'{'          // Загружаем символ '{' в регистр x0
        str x0, [sp, #-16]!   // Сохраняем x0 на стеке (аналог pushq)
        bl RTLWriteChar       // Вызываем функцию RTLWriteChar
    .endm

    // Макрос для вывода правой фигурной скобки '}'
    .macro parright
        mov x0, #'}'          // Загружаем символ '}' в регистр x0
        str x0, [sp, #-16]!   // Сохраняем x0 на стеке
        bl RTLWriteChar       // Вызываем функцию RTLWriteChar
    .endm

    // Макрос для вывода пробела
    .macro space
        mov x0, #' '          // Загружаем символ пробела в регистр x0
        str x0, [sp, #-16]!   // Сохраняем x0 на стеке
        bl RTLWriteChar       // Вызываем функцию RTLWriteChar
    .endm

    // Макрос для вывода символа из регистра x0
    .macro raxchar
        str x0, [sp, #-16]!   // Сохраняем значение x0 на стеке
        bl RTLWriteChar       // Вызываем функцию RTLWriteChar
    .endm

   
// ==========================================
// ------------------text--------------------
// ==========================================
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
// ------------------------------------------
// ----------------WriteChar-----------------
// ------------------------------------------
RTLWriteChar:
    pushall
    mov x27, sp 
    mov x16, #4    // syscall #4 == Write();
    mov x0, #1     // param1 == write_to == 1 == stdout
    mov x1, x27    // p2 == write_from == %rbp == top_of_stack
    add x1, x1, #144      // reach top of stack:[0-ret,16-arg1,32-arg2]
                            // we have all regs pushed so top is really really far
    mov x2, #1     // p3 == count == single_byte
    svc #0x80
    popall
    ret
// ------------------------------------------
// --------------WriteInteger----------------
// ------------------------------------------    
RTLWriteInteger:

    str x19, [sp, #-16]!
    
    ldr x1, [sp, #32]      // arg: count (stdout width). we do NOT care if it == 1
    ldr x0, [sp, #48]      // arg: num 
    
    cmp x0, #0             // Сравниваем num с 0

    // RTLWriteIntegerNotSigned
    b.ge RTLWriteIntegerNotSigned  // Если num >= 0, переходим к RTLWriteIntegerNotSigned
        neg x0, x0            // Инвертируем значение num
        sub x1, x1, #1        // Уменьшаем count на 1
        mov x0, #'-'
        bl RTLWriteChar // Вызываем RTLWriteChar для записи символа '-'

    RTLWriteIntegerNotSigned:
        mov x2, #0 
        str x0, [sp, #-16]! 
        str x1, [sp, #-16]! 
    
    RTLWriteIntegerPreCheckLoop:
        cmp x0, #0  
        b.eq RTLWriteIntegerPreCheckLoopDone
        add x2, x2, #1 

        mov x1, #10 
        udiv x8, x0, x1    // x8 = x0 / 10
        msub x3, x8, x1, x0 // x3 = x0 - (x8 * 10) (остаток)
        mov x0, x8         // x0 = x8 (обновляем x0 для следующей итерации)
        b RTLWriteIntegerPreCheckLoop
        
    RTLWriteIntegerPreCheckLoopDone:
    cmp x2, #0 
    // setz %dl                    // x3: (0 == x2) ? 1 : 0
    cset x3, eq 
    orr x2, x2, x3  // Выполняем OR между X2 и X3, результат сохраняем в X2 TODO
    
    ldr x1, [sp], #16
    ldr x0, [sp], #16 
    cmp x1, #0 


    b.le RTLWriteIntegerNotPadding
        str x2, [sp, #-16]! 
        RTLWriteIntegerPaddingLoop:
            mov x0, #' '  
            bl RTLWriteChar
            sub x1, x1, #1 
        cbnz x1, RTLWriteIntegerPaddingLoop
        ldr x2, [sp], #16
    
    RTLWriteIntegerNotPadding:
    // find last digit's address:     
    // we write from right to left cuz each time we divide number by 10, we only know it's right-most digit
    // so we need to write right-most digit into right-most byte:
    
    // e.g: num=-123; count=4
    //               |  < |  = |  > |    |  < |... 
    // init buffer:   |0x3b|0x3d|0x3e|0x00|0x3b|...
    // 1st iter:      |0x3b|0x3d| 3d |0x00|0x3b|...
    // 2st iter:      |0x3b| 2d | 3d |0x00|0x3b|...
    // 3st iter:      | 1d | 2d | 3d |0x00|0x3b|...
    
    adrp x4, RTLWriteIntegerBuffer@PAGE
    add x4, x4, RTLWriteIntegerBuffer@PAGEOFF
    add x4, x4, x2
    sub x4, x4, #1  
    
    str x2, [sp, #-16]! 

    RTLWriteIntegerLoop:
        mov x19, #10
        mov x3, #0
        mov x9, x0
        udiv x0, x0, x19    //  X0 = X0 / X19
        mul x8, x0, x19     //  X8 = X0 * X19 
        sub x3, x9, x8     // X3 = X9 - X8

        #convert to string
        add x1, x3, #'0'   

        strb w1, [x4]
        sub x4, x4, #1 
        // cmp x4, #0
        subs x2, x2, #1

    b.ne RTLWriteIntegerLoop
    
    ldr x2, [sp], #16

    mov x16, #4  // syscall 4 == Write                      
    mov x0, #1

    adrp x19, RTLWriteIntegerBuffer@PAGE
    add x19, x19, RTLWriteIntegerBuffer@PAGEOFF 
    mov x3, x2
 
    svc #0x80

    ldr x19, [sp], #16
    // ldr x30, [sp], #16 // take ret addr from stack
    //add sp, sp, #32 
    ret
    
// ------------------------------------------
// -----------------WriteLn------------------
// ------------------------------------------    
RTLWriteLn: 
    // pushq   $13             #13 == 0xD == CR
    mov x0, #13 
    bl RTLWriteChar
 
    mov x0, #10
    bl RTLWriteChar

    ret
   

ReadCharEx:
    pushall
  
    mov x0, #3            // syscall #3 == Read();
    mov x4, #0 
    adrp x19, ReadCharBuffer@PAGE
    add x19, x1, ReadCharBuffer@PAGEOFF  // p2 == write_to == buffer
    mov x3, #1 
    svc #0x80  
    cmp x0, #0
    cset w1, eq

    adrp x8, IsEOF@PAGE
    add x8, x8, IsEOF@PAGEOFF  // address of IsEOF in x8  
    ldrb w2, [x8]  
    orr w2, w2, w1             // Устанавливаем флаг EOF
    strb w2, [x8]              // Сохраняем обновленное значение IsEOF
    popall
    ret
    
ReadCharInit:

    adrp x8, ReadCharInited@PAGE
    add x8, x8, ReadCharInited@PAGEOFF
    ldrb w8, [x8] 
    cmp w8, #0
    b.ne ReadInitDone
        bl ReadCharEx 
        adrp x8, ReadCharInited@PAGE
        add x8, x8, ReadCharInited@PAGEOFF
        mov w8, #1 
        strb w8, [x8]
    ReadInitDone:
    ret

// ------------------------------------------
// ----------------ReadChar------------------
// ------------------------------------------    
RTLReadChar:
    bl      ReadCharInit
    mov     x0, #0 
    adrp    x8, ReadCharBuffer@PAGE
    add     x8, x8, ReadCharBuffer@PAGEOFF
    ldrb    w0, [x8] 

    bl      ReadCharEx
    ret

// ------------------------------------------
// --------------ReadInteger-----------------
// ------------------------------------------    
RTLReadInteger:
    bl ReadCharInit
    pushall
    mov x0, #0
    mov x2, #1 
    
    ReadIntegerSkipWhiteSpace:
 
        adrp x8, IsEOF@PAGE
        add x8, x8, IsEOF@PAGEOFF
        ldrb w8, [x8] 
        cmp w8, #0 
        b.ne ReadIntegerDone
 
        adrp x8, ReadCharBuffer@PAGE
        add x8, x8, ReadCharBuffer@PAGEOFF
        ldrb w8, [x8]
        cmp w8, #0 
 
        b.eq ReadIntegerSkipWhiteSpaceDone
 
        adrp x8, ReadCharBuffer@PAGE
        add x8, x8, ReadCharBuffer@PAGEOFF
        ldrb w8, [x8]
        cmp w8, #32
        b.hi ReadIntegerSkipWhiteSpaceDone // branch if higher
 
        bl ReadCharEx
 
        b ReadIntegerSkipWhiteSpace

    ReadIntegerSkipWhiteSpaceDone:
    adrp x8, ReadCharBuffer@PAGE
    add x8, x8, ReadCharBuffer@PAGEOFF
    ldrb w8, [x8] 
    cmp w8, #'-' 
    b.ne ReadIntegerNotSigned  
        neg x2, x2 
        bl ReadCharEx
        
    ReadIntegerNotSigned:
    ReadIntegerLoop:
        mov x1, #0
        adrp x8, ReadCharBuffer@PAGE
        add x8, x8, ReadCharBuffer@PAGEOFF   
        ldrb w1, [x8]
 
        cmp w1, #'0'
        b.lo ReadIntegerDone       // Если символ < '0', завершаем
        cmp w1, #'9'
        b.hi ReadIntegerDone       // Если символ > '9', завершаем

        mov x9, #10 
        mul x0, x0, x9 
        #cast string to int:
        sub x1, x1, #'0'
        add x0, x0, x1 

        bl ReadCharEx 
        b ReadIntegerLoop
    
    ReadIntegerDone:
    mul x0, x0, x2
    str x0, [sp]
    popall
    ret

// ------------------------------------------
// ------------------ReadLn------------------
// ------------------------------------------  
// it does NOT skip input if %RAX is nulled
// so put there some trash before calling it
// ------------------------------------------   
RTLReadLn:

    bl ReadCharInit
    adrp x8, IsEOF@PAGE             
    add x8, x8, IsEOF@PAGEOFF
    ldrb w8, [x8]  
    cmp w8, #0 
    b.ne ReadLnDone
        adrp x8, ReadCharBuffer@PAGE   
        add x8, x8, ReadCharBuffer@PAGEOFF
        ldrb w8, [x8]  
        cmp     w8, #10                    // Сравниваем с LF (10)
        b.eq ReadLnDone
            bl ReadCharEx                   
            b RTLReadLn
    
    ReadLnDone:
    ret

// ------------------------------------------
// -------------------EOF--------------------
// ------------------------------------------         
RTLEOF:
    mov x0, #0
    adrp x8, IsEOF@PAGE         
    add x8, x8, IsEOF@PAGEOFF
    ldrb w0, [x8]
    ret

// ------------------------------------------
// ------------------EOLN--------------------
// ------------------------------------------    
RTLEOLN:
    // movq    ReadCharBuffer@GOTPCREL(%rip), %r8
    adrp x8, ReadCharBuffer@PAGE       
    add x8, x8, ReadCharBuffer@PAGEOFF
    ldrb w8, [x8]
    // cmpb    $10, (%r8)       #cmp to LF
    cmp w8, #10 
    // sete    %dl                         #bero's legacy
    cset w3, eq 
    ret

// ------------------------------------------
// ------------------Halt--------------------
// ------------------------------------------        
RTLHalt:
    mov x0, #93
    mov x4, #0                    
    svc #0 

    
//==----------------------------------==//
//----------------ENTRY-----------------//
//==----------------------------------==//

StubEntryPoint:
    mov sp, x27 
    adrp x19, RTLFunctionTable@PAGE
    add x19, x19, RTLFunctionTable@PAGEOFF

// movq RTLFunctionTable@GOTPCREL(%rip), %rsi      // store functionTable and don't change %rsi
ProgramEntryPoint:

// ------------------------------------------
// code generated by btpc.pas goes here

