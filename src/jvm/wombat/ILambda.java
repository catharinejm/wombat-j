package wombat;

import java.lang.invoke.*;

public interface ILambda {
    public MethodHandle getHandle(int arity);

    // thunk needed for continuations
    public Object invoke();
}
