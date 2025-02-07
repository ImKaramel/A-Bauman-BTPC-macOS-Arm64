//as rtl64macOS.s -o rtl64macOS.o && ld rtl64macOS.o -e _main -o rtl64macOS -lSystem -syslibroot $(xcrun --show-sdk-path) 

.section __DATA,__data
    .align 3
RTLFunctionTable:
    .quad RTLWriteChar

Copyright:   
//objdump rtl64macOS > rtl64macOS.asm
.section __BSS,__bss

.macro pushall
    str x4, [sp, #-16]!    // push x4
    str x19, [sp, #-16]!   // push x19
    str x27, [sp, #-16]!   // push x27
    str x8, [sp, #-16]!    // push x8
    str x3, [sp, #-16]!    // push x3
    str x2, [sp, #-16]!    // push x2
    str x1, [sp, #-16]!    // push x1
    str x0, [sp, #-16]!    // push x0
.endm

.macro popall
    ldr x0, [sp], #16      // pop x0
    ldr x1, [sp], #16      // pop x1
    ldr x2, [sp], #16      // pop x2
    ldr x3, [sp], #16      // pop x3
    ldr x8, [sp], #16      // pop x8
    ldr x27, [sp], #16     // pop x27
    ldr x19, [sp], #16     // pop x19
    ldr x4, [sp], #16      // pop x4
.endm

.section __TEXT,__text
.global _main
_main:
    b StubEntryPoint

// Функция RTLWriteChar
RTLWriteChar:
    pushall
    mov x16, #4   // syscall #4 (write) на macOS
    mov x0, #1                 // file descriptor: stdout
    add x1, sp, #0            // адрес символа (x0 был сохранен первым)
    mov x2, #1                 // количество байт для записи
    svc #0x80                  // системный вызов
    popall
    ret

// Завершение программы
RTLHalt:
    mov x0, #0                 // exit code
    mov x16, #1
    svc #0x80                  // системный вызов
    ret

StubEntryPoint:
    mov x27, sp            
    adrp x19, RTLFunctionTable@PAGE
    add x19, x19, RTLFunctionTable@PAGEOFF

ProgramEntryPoint:
    mov x0, #'@'            
    str x0, [sp, #-16]! 
    bl RTLWriteChar         
    b RTLHalt