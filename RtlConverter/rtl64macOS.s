// ==========================================
// ------------------data--------------------
// ==========================================
.section __DATA,__data
.align 3 
RTLWriteIntegerBuffer:
    .space 64  // Буфер для хранения символов числа (достаточно для 64-битного числа)


.align 3
ReadCharBuffer:
    .byte 0x3c   

.align 3 
ReadCharInited: 
    .byte 0x00 

.align 3       
IsEOF:
    .byte 0x00
    .byte 0x00   
    .byte 0x00
    .byte 0x00
    .byte 0x00   
    .byte 0x00
    .byte 0x00
    .byte 0x00 


    .align 4
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

.section __BSS,__bss

.macro pushall
    str x4, [sp, #-16]!    // push x4
    str x19, [sp, #-16]!   // push x19
    str x27, [sp, #-16]!   // push x27 80
    str x8, [sp, #-16]!    // push x8 64 
    str x3, [sp, #-16]!    // push x3 48
    str x2, [sp, #-16]!    // push x2 32
    str x1, [sp, #-16]!    // push x1 16
    str x0, [sp, #-16]!    // push x0 0 
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
    mov x16, #4                 // syscall #4 (write) на macOS
    mov x0, #1                 // file descriptor: stdout
    add x1, sp, #64            // адрес символа (x8 - 64 )
    mov x2, #1                // количество байт для записи
    svc #0                 // системный вызов
    popall
    ret


// ------------------------------------------
// --------------WriteInteger----------------
// ------------------------------------------    

RTLWriteInteger:  
    str x19, [sp, #-16]!
    ldr x1, [sp, #16]        // Загружаем аргумент count (ширина вывода)
    ldr x0, [sp, #32]        // Загружаем аргумент num (число для записи)
    cmp x0, #0  

    b.ge RTLWriteIntegerNotSigned  // Если num >= 0
    // Обработка отрицательного числа
    neg x0, x0               // Инвертируем значение num
    sub x1, x1, #1           // Уменьшаем count на 1 (для знака минус)
    mov x8, #'-'             
    bl RTLWriteChar          // Вызываем RTLWriteChar для записи символа '-'

    RTLWriteIntegerNotSigned:
        mov x2, #0               // Инициализируем счетчик цифр
        str x0, [sp, #-16]!     
        str x1, [sp, #-16]!       

    // Цель этого цикла — подсчитать количество цифр в числе
    RTLWriteIntegerPreCheckLoop:
        cmp x0, #0               // Проверяем, осталось ли число
        b.eq RTLWriteIntegerPreCheckLoopDone  // Если число == 0, завершаем цикл
        add x2, x2, #1           // Увеличиваем счетчик цифр      
        mov x8, #10              // Загружаем 10 для деления
        udiv x0, x0, x8          // x0 = x0 / 10
        b RTLWriteIntegerPreCheckLoop   

    RTLWriteIntegerPreCheckLoopDone:
        cmp x2, #0               // Проверяем, есть ли цифры
        cset x3, eq              // x3 = (x2 == 0) ? 1 : 0
        add x2, x2, x3           // Если цифр нет, устанавливаем x2 = 1   

        ldr x1, [sp], #16        // Восстанавливаем count
        ldr x0, [sp], #16        // Восстанавливаем num
        sub x1, x1, x2
        cmp x1, #0               // Проверяем, нужно ли дополнение пробелами
        b.le RTLWriteIntegerNotPadding  // Если count <= 0, пропускаем дополнение
        str x2, [sp, #-16]!      // Сохраняем счетчик цифр в стеке

    RTLWriteIntegerPaddingLoop:
        mov x8, #' '             // Загружаем символ пробела
        bl RTLWriteChar          // Вызываем RTLWriteChar для записи пробела
        subs x1, x1, #1           // Уменьшаем count
        b.ne RTLWriteIntegerPaddingLoop  // Если count != 0, повторяем цикл
        ldr x2, [sp], #16        // Восстанавливаем счетчик цифр


    RTLWriteIntegerNotPadding:
        adrp x4, RTLWriteIntegerBuffer@PAGE  // Загружаем адрес буфера
        add x4, x4, RTLWriteIntegerBuffer@PAGEOFF
        add x4, x4, x2           // Перемещаем указатель на конец буфера
        //mov x1, #0               // Нулевой терминатор
        //strb w1, [x4]            // Записываем нулевой терминатор в буфер
        sub x4, x4, #1           // Корректируем указатель
        str x2, [sp, #-16]!      // Сохраняем счетчик цифр в стеке
    
    RTLWriteIntegerLoop:
        mov x5, #10             // Загружаем 10 для деления
        mov x3, #0               // Инициализируем остаток
        mov x9, x0               // Сохраняем текущее значение num
        udiv x0, x0, x5         // x0 = x0 / 10
        mul x8, x0, x5          // x8 = x0 * 10
        sub x3, x9, x8           // x3 = x9 - x8 (остаток)
        add x1, x3, #'0'         // Преобразуем остаток в символ
        strb w1, [x4]            // Записываем символ в буфер
        sub x4, x4, #1           // Перемещаем указатель буфера назад
        subs x2, x2, #1          // Уменьшаем счетчик цифр

        b.ne RTLWriteIntegerLoop  // Если счетчик != 0, повторяем цикл

    ldr x2, [sp], #16        // Восстанавливаем счетчик цифр

    //mov x8, #'$'
    //bl RTLWriteChar

    //add x8, x2, #'0'
    //bl RTLWriteChar

    mov x1, #0
        // Добавляем нулевой символ в конец буфера
    adrp x4, RTLWriteIntegerBuffer@PAGE
    add x4, x4, RTLWriteIntegerBuffer@PAGEOFF
    add x4, x4, x2
    mov w1, #0
    strb w1, [x4]            // Записываем нулевой символ

    mov x16, #4              // syscall 4 == write
    mov x0, #1               // file descriptor: stdout
    mov x2, x2
    adrp x1, RTLWriteIntegerBuffer@PAGE  // Загружаем адрес буфера в x1
    add x1, x1, RTLWriteIntegerBuffer@PAGEOFF
    svc #0               // Системный вызов
    ret                      // Возврат из функции


// ------------------------------------------
// -----------------WriteLn------------------
// ------------------------------------------    
RTLWriteLn: 
    // #13 == 0xD == CR
    mov x8, #13 
    bl RTLWriteChar
    mov x8, #10
    bl RTLWriteChar
    ret



ReadCharEx:
    pushall
    mov x0, #0              // file descriptor: stdin
    mov x2, #1              // Количество байт для чтения
    adrp x1, ReadCharBuffer@PAGE  // Адрес буфера
    add x1, x1, ReadCharBuffer@PAGEOFF
    mov x16, #0x3              // Загружаем младшие 16 бит (0x0003)
    movk x16, #0x2000, lsl #16
    //mov x16, #0x2000003     // syscall: read
    svc #0               // Системный вызов


    cmp x0, #0              // Проверяем результат системного вызова
    cset w1, eq             // Устанавливаем w1 = 1, если x0 == 0 (EOF)

    adrp x8, IsEOF@PAGE     // Адрес флага IsEOF
    add x8, x8, IsEOF@PAGEOFF
    ldrb w2, [x8]           // Загружаем текущее значение IsEOF
    orr w2, w2, w1          // Устанавливаем флаг EOF
    strb w2, [x8]           // Сохраняем обновленное значение IsEOF
    popall
    ret


ReadCharInit:
    adrp x8, ReadCharInited@PAGE         // Загружаем старшую часть адреса
    add x8, x8, ReadCharInited@PAGEOFF   // Добавляем младшую часть адреса
    ldrb w8, [x8]                        // Загружаем значение из ReadCharInited
    cmp w8, #0
    b.ne ReadInitDone                    // Если уже инициализировано, пропускаем

    bl ReadCharEx                        // Читаем первый символ

    adrp x8, ReadCharInited@PAGE         // Загружаем старшую часть адреса
    add x8, x8, ReadCharInited@PAGEOFF   // Добавляем младшую часть адреса
    mov w8, #1                           // Устанавливаем флаг инициализации
    strb w8, [x8]                        // Сохраняем значение в ReadCharInited

ReadInitDone:
    ret

RTLReadChar:
    bl ReadCharInit           
    adrp x8, ReadCharBuffer@PAGE
    add x8, x8, ReadCharBuffer@PAGEOFF
    ldrb w0, [x8]            // Загружаем символ из буфера

    bl ReadCharEx            // Читаем следующий символ
    ret


// ------------------------------------------
// --------------RTLReadInteger--------------
// ------------------------------------------  
// Считывает целое число из stdin и возвращает его в x0.
// Поддерживает отрицательные числа и пропускает пробелы.
// ------------------------------------------   


RTLReadInteger:
    bl ReadCharInit          
    pushall                  
    mov x0, #0                
    mov x2, #1               //  (x2 = 1, положительное число)
// Пропуск пробелов и незначащих символов
    ReadIntegerSkipWhiteSpace:
        adrp x8, IsEOF@PAGE
        add x8, x8, IsEOF@PAGEOFF
        ldrb w8, [x8] 
        cmp w8, #0 
        b.ne ReadIntegerDone  // Если EOF, завершаем

        adrp x8, ReadCharBuffer@PAGE
        add x8, x8, ReadCharBuffer@PAGEOFF
        ldrb w8, [x8]          
        cmp w8, #32           // Сравниваем с пробелом (ASCII 32)
        b.hi ReadIntegerSkipWhiteSpaceDone  // Если символ > пробела, выходим

        bl ReadCharEx         // Читаем следующий символ
        b ReadIntegerSkipWhiteSpace

        ReadIntegerSkipWhiteSpaceDone:

        // Проверка на знак минус
        adrp x8, ReadCharBuffer@PAGE
        add x8, x8, ReadCharBuffer@PAGEOFF
        ldrb w8, [x8] 
        cmp w8, #'-' 
        b.ne ReadIntegerNotSigned  
            neg x2, x2            // Меняем знак на отрицательный
            bl ReadCharEx         // Читаем следующий символ
        
        ReadIntegerNotSigned:

        // Основной цикл чтения цифр
        ReadIntegerLoop:
            adrp x8, ReadCharBuffer@PAGE
            add x8, x8, ReadCharBuffer@PAGEOFF   
            ldrb w1, [x8]          

            // Проверка, что символ — цифра
            cmp w1, #'0'
            b.lo ReadIntegerDone  // Если символ < '0', завершаем
            cmp w1, #'9'
            b.hi ReadIntegerDone  // Если символ > '9', завершаем
            mul x0, x0, x9        // Умножаем результат на 10
            add x0, x0, x1        // Добавляем текущую цифру

            bl ReadCharEx         
            b ReadIntegerLoop      

        ReadIntegerDone:
        mul x0, x0, x2            // Умножаем результат на знак (1 или -1)
        popall                    
        ret


// ------------------------------------------
// ------------------ReadLn------------------
// ------------------------------------------  
// Пропускает стандартный ввод до конца файла или ближайшего перевода строки.
// Не пропускает ввод, если x0 обнулен.
// ------------------------------------------   
RTLReadLn:
    bl ReadCharInit

    // Проверяем флаг EOF
    adrp x8, IsEOF@PAGE             
    add x8, x8, IsEOF@PAGEOFF
    ldrb w8, [x8]  
    cmp w8, #0 
    b.ne ReadLnDone  // Если EOF, завершаем функцию

    // Проверяем текущий символ в буфере
    adrp x8, ReadCharBuffer@PAGE   
    add x8, x8, ReadCharBuffer@PAGEOFF
    ldrb w8, [x8]  
    cmp w8, #10      // Сравниваем с LF (10, символ новой строки)
    b.eq ReadLnDone  // Если найден LF, завершаем функцию

    bl ReadCharEx                   
    b RTLReadLn       

ReadLnDone:
    ret

// ------------------------------------------
// -------------------EOF--------------------
// ------------------------------------------      
// Возвращает в x0:
//   1, если достигнут конец файла (EOF),
//   0, если EOF не достигнут.
// ------------------------------------------      
RTLEOF:
    mov x0, #0
    adrp x8, IsEOF@PAGE         
    add x8, x8, IsEOF@PAGEOFF
    ldrb w0, [x8]
    ret

// ------------------------------------------
// ------------------RTLEOLN-----------------
// ------------------------------------------  
// Возвращает в x0:
//   1, если следующий символ — \n (LF, ASCII 10),
//   0, если следующий символ не \n.
// ------------------------------------------   

RTLEOLN:
    bl ReadCharInit
    adrp x8, ReadCharBuffer@PAGE
    add x8, x8, ReadCharBuffer@PAGEOFF
    ldrb w0, [x8]  
    // Сравниваем символ с \n (LF, ASCII 10)
    cmp w0, #10
    cset x0, eq    // x0 = 1, если символ равен \n, иначе 0
    ret

// Завершение программы
RTLHalt:
    mov x0, #0                 // exit code
    mov x16, #0x1              // Загружаем младшие 16 бит (0x0003)
    movk x16, #0x2000, lsl #16
    svc #0                  // системный вызов
    ret

StubEntryPoint:
    mov x27, sp            
    adrp x19, RTLFunctionTable@PAGE
    add x19, x19, RTLFunctionTable@PAGEOFF
    b ProgramEntryPoint

.section __PAS,__pas

.align 3
ProgramEntryPoint:
    nop