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
    public final static Var COMPILER_BINDINGS = RT.var("wombat.compiler", "global-bindings");
    public final static Var LOADER = RT.var("wombat.compiler", "*class-loader*");
    public final static Var JAVALIST_TO_LIST = RT.var("wombat.datatypes", "javalist->list");
    
    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType methodType, String symName) {
        Atom bindAtom = (Atom) COMPILER_BINDINGS.deref();
        IPersistentMap bindings = (IPersistentMap) bindAtom.deref();
        CallSite site = (CallSite) bindings.valAt(Symbol.intern(symName));
        if (site == null)
            throw new IllegalStateException("Symbol " + symName + " is not defined!");

        return site;
    }

    public static Object invokeLambda(ILambda lambda, Object[] args) {
        try{
            MethodHandle handle = lambda.getHandle(args.length);
            Object ret;
            if (handle.isVarargsCollector())
                ret = handle.invoke(lambda, args);
            else
                ret = handle.asSpreader(Object[].class, args.length).invoke(lambda, args);

            while (ret instanceof Continuation)
                ret = ((Continuation)ret).invoke();
            return ret;
        }catch(Throwable t) { throw Util.sneakyThrow(t); }
    }

}
