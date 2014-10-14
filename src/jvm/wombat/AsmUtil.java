package wombat;

import org.objectweb.asm.commons.GeneratorAdapter;

public class AsmUtil {
    public static void pushLong(GeneratorAdapter gen, Number n) {
        gen.push(n.longValue());
    }

    public static void pushInt(GeneratorAdapter gen, Number n) {
        gen.push(n.intValue());
    }

    public static void pushFloat(GeneratorAdapter gen, Number n) {
        gen.push(n.floatValue());
    }

    public static void pushDouble(GeneratorAdapter gen, Number n) {
        gen.push(n.doubleValue());
    }

}
