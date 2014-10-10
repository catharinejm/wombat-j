package cljstatic;

import java.lang.invoke.*;
import clojure.lang.*;

public class Sample implements ILambda {

    public static final MethodHandle handle;

    static {
        try {
            handle = MethodHandles.lookup().findVirtual(Sample.class, "doit", MethodType.methodType(String.class, String.class));
        } catch (Throwable t) {
            throw new RuntimeException("shit.");
        }
    }
    
    final MethodHandle myHandle;
    public Sample() {
        myHandle = handle.bindTo(this);
    }

    public MethodHandle getHandle(int i) {
        return handle;
    }

    public String doit(String s) {
        return s;
    }

    public String somethingElse(String s) {
        return "This is different. Input: " + s;
    }

    public static String staticDoit(Object o, String s) {
        return "Static you said: " + s;
    }
}
