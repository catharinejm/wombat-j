package wombat;

import java.lang.invoke.*;
import clojure.lang.*;

public class Continuation {
    public static final Var COMPILE = RT.var("wombat.compiler", "compile");
    public static final Var EMIT = RT.var("wombat.compiler", "emit");

    final ILambda lambda;
    public Continuation(Object l) {
        this.lambda = (ILambda) l;
    }
    public Object invoke() {
        return lambda.invoke();
    }
}
