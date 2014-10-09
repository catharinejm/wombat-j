package cljstatic;

import java.lang.invoke.*;
import java.util.concurrent.atomic.AtomicReference;
import clojure.lang.PersistentArrayMap;
import clojure.lang.IPersistentMap;
import clojure.lang.PersistentVector;
import clojure.lang.IPersistentVector;
import java.util.*;

public abstract class ALambda {

    final java.util.Map<Integer, MethodHandle> handles = new HashMap<Integer, MethodHandle>();

    public MethodHandle getHandle(int arity) {
        return handles.get(arity);
    }
    
    // public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType methodType)
    //     throws NoSuchMethodException, IllegalAccessException, ClassNotFoundException {

    //     ALambda target = (ALambda) stack.get().peek();
    //     if (target == null)
    //         throw new IllegalStateException("No lambda registered to invoke!");

    //     try {
    //         MethodHandle m = caller.findVirtual(target.getClass(), name, methodType.dropParameterTypes(0, 1));
    //         return new ConstantCallSite(m.asType(methodType.changeParameterType(0, ALambda.class)));
    //     }
    //     finally {
    //         stack.set(stack.get().pop());
    //         IPersistentSet r = registry.get();
    //         while (! registry.compareAndSet(r, (IPersistentSet)r.cons(target)))
    //             r = registry.get();
    //     }
                    
    // }
}
