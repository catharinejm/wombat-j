package wombat;

import java.lang.invoke.*;

import clojure.lang.RT;
import clojure.lang.IPersistentMap;
import clojure.lang.Symbol;
import clojure.lang.Atom;
import clojure.lang.Var;

public class Global {
    final static Var COMPILER_BINDINGS = RT.var("wombat.compiler", "global-bindings");
    
    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType methodType) {
        Atom bindAtom = (Atom) COMPILER_BINDINGS.deref();
        IPersistentMap bindings = (IPersistentMap) bindAtom.deref();
        CallSite site = (CallSite) bindings.valAt(Symbol.intern(name));
        if (site == null)
            throw new IllegalStateException("Symbol " + name + " is not defined!");

        return site;
    }
}
