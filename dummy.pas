program SimpleIfExample;
var a : integer;
begin
    a := 5;
    writeln('5 mod 2 = ', a mod 2);
    writeln('510 mod 2 = ', 510 mod 2);
    writeln('-510 mod 2 = ', -510 mod 2);
    writeln('-511 mod 2 = ', -511 mod 2);
    writeln('101 mod 5 = ', 101 mod 5);
    writeln('256 mod 256 = ', 256 mod 256);
    writeln('1001 mod 1000= ', 1001 mod 1000);
    writeln('1001 mod 999= ', 1001 mod 999);
    writeln('5 div 2 = ', a div 2);    
    writeln('1001 div 2 = ', 1001 div 2);   
    writeln('1001 div 999 = ', 1001 div 999);   
    
end.





//cd .. && cd target && chmod +x dummyAarch64Exec && r2 dummyAarch64Exec
//objdump -D -j .text  dummyAarch64Exec --start-address=16296 --stop-address=16328
//ld -o rtl64macOS rtl64macOS.o -nounwind

//ld rtl64macOS.o -e _main -o rtl64macOS -lSystem -syslibroot $(xcrun --show-sdk-path) -nounwind


// cd rtl2pas && as rtl64macOS.s -o rtl64macOS.o && ld -o rtl64macOS rtl64macOS.o -no_compact_unwind