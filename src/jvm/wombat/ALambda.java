package wombat;

import java.lang.invoke.*;
import java.util.concurrent.atomic.AtomicReference;
import clojure.lang.PersistentArrayMap;
import clojure.lang.IPersistentMap;
import clojure.lang.PersistentVector;
import clojure.lang.IPersistentVector;
import java.util.*;

public abstract class ALambda implements ILambda {

    final java.util.Map<Integer, MethodHandle> handles = new HashMap<Integer, MethodHandle>();

    public MethodHandle getHandle(int arity) {
        return handles.get(arity);
    }
}
