program SimpleIfExample;
var a : integer;
begin
    a := 123456;
end.

//cd .. && cd target && chmod +x dummyAarch64Exec && r2 dummyAarch64Exec
//objdump -D -j .text  dummyAarch64Exec --start-address=16296 --stop-address=16328
//ld -o rtl64macOS rtl64macOS.o -nounwind

//ld rtl64macOS.o -e _main -o rtl64macOS -lSystem -syslibroot $(xcrun --show-sdk-path) -nounwind


// cd rtl2pas && as rtl64macOS.s -o rtl64macOS.o && ld -o rtl64macOS rtl64macOS.o -no_compact_unwind