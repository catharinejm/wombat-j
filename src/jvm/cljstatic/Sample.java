package cljstatic;

import java.lang.invoke.*;
import clojure.lang.*;

public class Sample extends ALambda {

    public Sample() {
        try{
            handles.put(1, MethodHandles.lookup().findVirtual(Sample.class, "doit", MethodType.methodType(String.class, String.class)));
        }catch(Throwable t){ throw Util.sneakyThrow(t); }
    }

    public String doit(String s) {
        return "You said: " + s;
    }

    public String somethingElse(String s) {
        return "This is different. Input: " + s;
    }

    public static String staticDoit(Object o, String s) {
        return "Static you said: " + s;
    }
}
