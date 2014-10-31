package wombat;

import java.lang.invoke.*;
import java.util.Arrays;
import clojure.lang.*;

public class Global {
    public final static Var COMPILER_BINDINGS = RT.var("wombat.compiler", "global-bindings");
    public final static Var LOADER = RT.var("wombat.compiler", "*class-loader*");
    public final static Var JAVALIST_TO_LIST = RT.var("wombat.datatypes", "javalist->list");
    final static Keyword CALLSITE = Keyword.intern("call-site");
    
    public static CallSite getCallSite(Symbol name) {
        Atom bindAtom = (Atom) COMPILER_BINDINGS.deref();
        return (CallSite) RT.get(RT.get(bindAtom.deref(), name), CALLSITE);
    }

    public static Object getGlobal(Symbol name) {
        CallSite site = getCallSite(name);
        if (site == null)
            return null;

        try {
            return site.getTarget().invokeExact();
        } catch (Throwable t) {
            throw Util.sneakyThrow(t);
        }
    }

    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType methodType, String symName) {
        CallSite site = getCallSite(Symbol.intern(symName));
        if (site == null)
            throw new IllegalStateException("Symbol " + symName + " is not defined!");

        return site;
    }

    
    public static Object invokeLambda(ILambda lambda, Object... args) {
        Object ret = lambda.applyTo(JAVALIST_TO_LIST.invoke(Arrays.asList(args)));
        while (ret instanceof Continuation)
            ret = ((Continuation)ret).invoke();
        return ret;
    }

}
