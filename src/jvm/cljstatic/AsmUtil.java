package cljstatic;

import org.objectweb.asm.commons.GeneratorAdapter;

public class AsmUtil {
    public static void pushLong(GeneratorAdapter gen, Long l) {
        gen.push(l.longValue());
    }

    public static void pushInt(GeneratorAdapter gen, Integer i) {
        gen.push(i.intValue());
    }

    public static void pushInt(GeneratorAdapter gen, Long l) {
        if (l > Integer.MAX_VALUE || l < Integer.MIN_VALUE)
            throw new IllegalArgumentException("Long " + l.toString() + " does not fit in Integer");

        gen.push(l.intValue());
    }
}
