package wombat;

import java.lang.invoke.*;
import clojure.lang.*;

public class Global {
    public final static Var COMPILER_BINDINGS = RT.var("wombat.compiler", "global-bindings");
    public final static Var LOADER = RT.var("wombat.compiler", "*class-loader*");
    public final static Var JAVALIST_TO_LIST = RT.var("wombat.datatypes", "javalist->list");
    final static Keyword CALLSITE = Keyword.intern("call-site");
    
    public static CallSite getCallSite(Symbol name) {
        Atom bindAtom = (Atom) COMPILER_BINDINGS.deref();
        IPersistentMap bindings = (IPersistentMap) bindAtom.deref();
        return (CallSite) RT.get(RT.get(bindings, name), CALLSITE);
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

    public static Object invokeLambda(ILambda lambda, Object[] args) {
        try {
            MethodHandle handle = lambda.getHandle(args.length);
            Object ret;
            int arity = handle.type().parameterCount() - 1; // first parameter is ILambda
            if (handle.isVarargsCollector() && args.length >= arity) {
                Object[] newArgs = new Object[arity];
                Object[] varArgs = new Object[args.length - arity + 1];
                System.arraycopy(args, 0, newArgs, 0, arity - 1);
                System.arraycopy(args, arity - 1, varArgs, 0, args.length - arity + 1);
                newArgs[arity - 1] = varArgs;

                ret = handle.asFixedArity().asSpreader(Object[].class, arity).invoke(lambda, newArgs);
            } else
                ret = handle.asSpreader(Object[].class, args.length).invoke(lambda, args);

            while (ret instanceof Continuation)
                ret = ((Continuation)ret).invoke();
            return ret;
        } catch(Throwable t) { throw Util.sneakyThrow(t); }
    }

}
