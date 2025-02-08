import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.BitSet;
import java.util.concurrent.atomic.AtomicInteger;

public class RtlConverter {
    private static final int END_OFFSET = 0x4000 /* 16384 */;

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
        int beginByteSize = counter.get();

        counter = new AtomicInteger(0);
        String end = convertEnd();
        int endByteSize = counter.get();

        String ret = begin + "\n" + end + "\n" + convertConstants(beginByteSize, endByteSize);
        checkAllBytesAreUsed();
        return ret;
    }

    private String convertConstants(int beginByteSize, int endByteSize) {
        return "const StartStubSize=" + beginByteSize + ";\n" + "const EndStubSize=" + endByteSize + ";";
    }

    private void checkAllBytesAreUsed() {
        for (int i = 0; i < rtlContent.length; i++) {
            if (!selfCheckSet.get(i)) {
                throw new IllegalStateException("Byte at " + i + " is not used.");
            }
        }
    }

    private String convertBegin() throws IOException {
        return PROLOGUE_RTL_BEGINNING + convertInternal(0, END_OFFSET) + EPILOGUE;
    }

    private String convertEnd() throws IOException {
        int fileSize = rtlContent.length;
        return PROLOGUE_RTL_ENDING + convertInternal(END_OFFSET, fileSize) + EPILOGUE;
    }

    private String convertInternal(int startFromIncluded, int endAtNotIncluded) {
        assert rtlContent.length > endAtNotIncluded;
        StringBuilder ret = new StringBuilder();
        // индекс в rtlContent
        for (int idx = startFromIncluded; idx < endAtNotIncluded;) {
            // Формируем вызов с 20 символами
            if (endAtNotIncluded - idx >= LONG_STRING_SIZE) {
                ret.append(FUN_LONG_STRING_PREFIX);
                for (int i = 0; i < LONG_STRING_SIZE; i++) {
                    int innerIdx = idx + i;
                    ret.append(convertByteToPasString(useByte(innerIdx)));
                }
                ret.append(FUN_SUFFIX);
                idx += LONG_STRING_SIZE;
                continue;
            }

            ret.append(FUN_CHAR_PREFIX);
            ret.append(convertByteToPasString(useByte(idx)));
            ret.append(FUN_SUFFIX);

            idx++;
        }
        return ret.toString();
    }

    private byte useByte(int idx) {
        if (selfCheckSet.get(idx)) {
            throw new IllegalStateException("Byte at " + idx + " is already used.");
        }
        selfCheckSet.set(idx);
        counter.incrementAndGet();
        return rtlContent[idx];
    }

    private String convertByteToPasString(byte b) {
        return "#" + Byte.toUnsignedInt(b);
    }

    public static void main(String[] args) throws IOException {
        RtlConverter rtlConverter = new RtlConverter();
        Files.writeString(Paths.get(args[1]), rtlConverter.convert(Paths.get(args[0])));
        System.out.println("I am OK.");
    }
}