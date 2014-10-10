package cljstatic;

import java.lang.invoke.*;
import clojure.lang.*;

public class SubSample extends ALambda {
    public SubSample() {
        try{
            handles.put(1, MethodHandles.lookup().findVirtual(SubSample.class, "doit", MethodType.methodType(String.class, String.class)));
        }catch(Throwable t){ throw Util.sneakyThrow(t); }
    }

    public String doit(String s) {
        return "TOTALLY UNRELATED " + s;
    }
}
