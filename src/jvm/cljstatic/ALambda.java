package cljstatic;

import java.lang.invoke.*;
import java.util.concurrent.atomic.AtomicReference;
import clojure.lang.PersistentHashSet;
import clojure.lang.IPersistentSet;
import clojure.lang.PersistentVector;
import clojure.lang.IPersistentVector;

public abstract class ALambda {
    public final static AtomicReference<IPersistentSet> registry = new AtomicReference<IPersistentSet>();
    public final static ThreadLocal<PersistentVector> stack = new ThreadLocal<PersistentVector>();

    static {
        registry.set(PersistentHashSet.EMPTY);
        stack.set(PersistentVector.EMPTY);
    }

    public void registerForBootstrap() {
        if (registry.get().get(this) == null) {
            stack.set(stack.get().cons(this));
        }
    }

    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType methodType)
        throws NoSuchMethodException, IllegalAccessException, ClassNotFoundException {

        ALambda target = (ALambda) stack.get().peek();
        if (target == null)
            throw new IllegalStateException("No lambda registered to invoke!");

        try {
            MethodHandle m = caller.findVirtual(target.getClass(), name, methodType.dropParameterTypes(0, 1));
            return new ConstantCallSite(m.asType(methodType.changeParameterType(0, ALambda.class)));
        }
        finally {
            stack.set(stack.get().pop());
            IPersistentSet r = registry.get();
            while (! registry.compareAndSet(r, (IPersistentSet)r.cons(target)))
                r = registry.get();
        }
                    
    }
}
