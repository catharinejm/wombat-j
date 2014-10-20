package wombat;

import java.lang.invoke.*;
import clojure.lang.Util;

public class Continuation {
    final MethodHandle lambdaHandle;
    final ILambda lambda;
    final Object[] args;
    public Continuation(Object l, Object... args) {
        this.args = args;
        this.lambda = (ILambda) l;
        this.lambdaHandle = lambda.getHandle(args.length).asSpreader(Object[].class, args.length);
    }
    public Object invoke() {
        try {
            return lambdaHandle.invoke(lambda, args);
        } catch (Throwable t) {
            throw Util.sneakyThrow(t);
        }
    }
}
