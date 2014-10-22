package wombat;

import java.lang.invoke.*;
import clojure.lang.*;

public class Continuation {
    final ILambda lambda;
    public Continuation(Object l) {
        this.lambda = (ILambda) l;
    }
    public Object invoke() {
        return lambda.invoke();
    }
}
