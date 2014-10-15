package wombat;

import java.lang.invoke.*;

import clojure.lang.RT;
import clojure.lang.IPersistentMap;
import clojure.lang.Symbol;
import clojure.lang.Atom;
import clojure.lang.Var;
import clojure.lang.Util;
import clojure.lang.DynamicClassLoader;

public class Global {
    final static Var COMPILER_BINDINGS = RT.var("wombat.compiler", "global-bindings");
    final static Var LOADER = RT.var("wombat.compiler", "*class-loader*");
    
    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType methodType, String symName) {
        Atom bindAtom = (Atom) COMPILER_BINDINGS.deref();
        IPersistentMap bindings = (IPersistentMap) bindAtom.deref();
        CallSite site = (CallSite) bindings.valAt(Symbol.intern(symName));
        if (site == null)
            throw new IllegalStateException("Symbol " + symName + " is not defined!");

        return site;
    }

    public static CallSite bootstrapInvoke(MethodHandles.Lookup caller, String name, MethodType methodType, String cname) {
        try {
            DynamicClassLoader loader = (DynamicClassLoader) LOADER.deref();
            Class ilambda = Class.forName(cname, true, loader);
            MethodHandle handle = caller.findVirtual(ilambda, name, methodType.dropParameterTypes(0,1));
            return new ConstantCallSite(handle.asType(methodType));
        } catch (Throwable t) {
            throw Util.sneakyThrow(t);
        }
    }

}
