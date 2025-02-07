import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.BitSet;
import java.util.concurrent.atomic.AtomicInteger;

public class RtlConverter {
    private static final int ENDING_OFFSET = 0x4000; // 16384

    private static final String PROLOGUE_RTL_BEGINNING = """
            procedure EmitStubCode;
            begin
                OutputCodeDataSize := 0;
            """;
    private static final String PROLOGUE_RTL_ENDING = """
            procedure EmitEndingStub;
            begin
            """;
    private static final String EPILOGUE = "end;\n";

    private static final String FUN_LONG_STRING_PREFIX = "    OutputCodeString(";
    private static final String FUN_CHAR_PREFIX = "    OutputCodeChar(";
    private static final String FUN_SUFFIX = ");\n";

    private static final int LONG_STRING_SIZE = 20;

    private BitSet selfCheckSet;
    private byte[] rtlContent;
    private AtomicInteger counter;

    public String convert(Path rtlFile) throws IOException {
        selfCheckSet = new BitSet(Math.toIntExact(Files.size(rtlFile)));
        rtlContent = Files.readAllBytes(rtlFile);

        counter = new AtomicInteger(0);
        String begin = convertBegin();
        @SuppressWarnings("unused")
        int beginByteSize = counter.get();

        counter = new AtomicInteger(0);
        String end = convertEnd();
        int endByteSize = counter.get();

        String ret = begin + "\n" + end + "\n" + convertConstants(endByteSize);
        checkAllBytesAreUsed();
        return ret;
    }

    private String convertConstants(int endByteSize) {
        return "const EndStubSize=" + endByteSize + ";";
    }

    private void checkAllBytesAreUsed() {
        for (int i = 0; i < rtlContent.length; i++) {
            if (!selfCheckSet.get(i)) {
                throw new IllegalStateException("Byte at " + i + " is not used.");
            }
        }
    }

    private String convertBegin() throws IOException {
        return PROLOGUE_RTL_BEGINNING + convertInternal(0, ENDING_OFFSET) + EPILOGUE;
    }

    private String convertEnd() throws IOException {
        int fileSize = rtlContent.length;
        return PROLOGUE_RTL_ENDING + convertInternal(ENDING_OFFSET, fileSize) + EPILOGUE;
    }

    private String convertInternal(int startFrom, int endAt) throws IOException {
        assert rtlContent.length > endAt;
        StringBuilder ret = new StringBuilder();
        // индекс в rtlContent
        for (int idx = startFrom; idx < endAt; ) {
            // Формируем вызов с 20 символами
            if (endAt - idx >= LONG_STRING_SIZE) {
                ret.append(FUN_LONG_STRING_PREFIX);
                for (int i = 0; i < LONG_STRING_SIZE; i++) {
                    int innerIdx = idx + i;
                    useByte(innerIdx);
                    ret.append(convertByteToPasString(rtlContent[innerIdx]));
                }
                ret.append(FUN_SUFFIX);
                idx += LONG_STRING_SIZE;
                continue;
            }

            useByte(idx);
            ret.append(FUN_CHAR_PREFIX);
            ret.append(convertByteToPasString(rtlContent[idx]));
            ret.append(FUN_SUFFIX);

            idx++;
        }
        return ret.toString();
    }

    private void useByte(int idx) {
        if (selfCheckSet.get(idx)) {
            throw new IllegalStateException("Byte at " + idx + " is already used.");
        }
        selfCheckSet.set(idx);
        counter.incrementAndGet();
    }

    private String convertByteToPasString(byte b) {
        return "#" + Byte.toUnsignedInt(b);
    }

    public static void main(String[] args) throws IOException {
        RtlConverter rtlConverter = new RtlConverter();
        Files.writeString(Paths.get("stub.txt"), rtlConverter.convert(Path.of("rtl64macOS")));
        System.out.println("I am OK.");
    }
}