package wombat;

import java.lang.invoke.*;

public interface ILambda {
    public MethodHandle getHandle(int arity);
}
