package wombat;

import java.lang.invoke.*;
import org.objectweb.asm.Opcodes;
import clojure.lang.RT;
import clojure.lang.IPersistentMap;
import clojure.lang.Symbol;

public class Global implements Opcodes {
    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType methodType) {
         IPersistentMap bindings = (IPersistentMap) RT.var("wombat.compiler", "global-bindings").deref();
         CallSite site = (CallSite) bindings.valAt(Symbol.intern(name));
         if (site == null)
             throw new IllegalStateException("Symbol " + name + " is not defined!");

         return site;
    }
}
