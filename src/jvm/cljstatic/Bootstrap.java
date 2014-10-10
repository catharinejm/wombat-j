package cljstatic;

import java.lang.invoke.*;
import java.io.PrintStream;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.Handle;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.commons.Method;

import java.util.concurrent.ConcurrentHashMap;

import clojure.lang.DynamicClassLoader;

public class Bootstrap implements Opcodes {
    static DynamicClassLoader classLoader = new DynamicClassLoader();

    static int calls = 0;

    public static CallSite bootstrap(MethodHandles.Lookup caller, String name, MethodType methodType)
        throws NoSuchMethodException, IllegalAccessException, ClassNotFoundException {

        calls++;

        return new ConstantCallSite(Sample.handle.asType(methodType.changeParameterType(0, ILambda.class)));
    }

    public static void main(String[] args)
        throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, java.lang.reflect.InvocationTargetException, Throwable {

        buildClass();

        Class ex = Class.forName("cljstatic.SubExample", true, classLoader);

        Example o = (Example) ex.newInstance();
        MethodHandle blah = MethodHandles.lookup().findVirtual(ex, "blah", MethodType.methodType(void.class, ILambda.class)).bindTo(o);

        long before = System.currentTimeMillis();
        Sample s = new Sample();
        // Example o = new Example();
        
        for (int i = 0; i < 30000000; ++i) {
            // ex.getMethod("blah", ILambda.class).invoke(o, s);
            // blah.invoke(s);
            o.blah(s);
        }
        long after = System.currentTimeMillis();

        System.out.printf("elapsed: %dms\n", after - before);
        // ex.getMethod("blah", ILambda.class).invoke(o, new SubSample());
        
        // System.out.println("Called bootstrap " + calls + " times.");
    }

    static void buildClass() {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        cw.visit(V1_7, ACC_PUBLIC, "cljstatic/SubExample", null, "cljstatic/Example", null);

        Method ctor = new Method("<init>", Type.VOID_TYPE, new Type[0]);
        GeneratorAdapter ctorgen = new GeneratorAdapter(Opcodes.ACC_PUBLIC, ctor, null, null, cw);

        ctorgen.visitCode();
        ctorgen.loadThis();
        ctorgen.invokeConstructor(Type.getType(Example.class), Method.getMethod("void <init>()"));
        ctorgen.returnValue();
        ctorgen.endMethod();

        Type[] ts = new Type[1];
        ts[0] = Type.getType(ILambda.class);
        Method m = new Method("blah", Type.VOID_TYPE, ts);
        GeneratorAdapter gen = new GeneratorAdapter(Opcodes.ACC_PUBLIC, m, null, null, cw);

        gen.visitCode();
        gen.loadArg(0);

        // With self-bound method handle
        // gen.push(1);
        // gen.invokeInterface(Type.getType(ILambda.class), Method.getMethod("java.lang.invoke.MethodHandle getHandle(int)"));
        // gen.push("input string");
        // gen.invokeVirtual(Type.getType(MethodHandle.class), Method.getMethod("String invoke(String)"));

        // Handle with no self binding
        // gen.dup();
        // gen.push(1);
        // gen.invokeInterface(Type.getType(ILambda.class), Method.getMethod("java.lang.invoke.MethodHandle getHandle(int)"));
        // gen.swap();
        // gen.push("input string");
        // gen.invokeVirtual(Type.getType(MethodHandle.class), Method.getMethod("String invoke(cljstatic.ILambda, String)"));

        // Direct invokeVirtual
        // gen.checkCast(Type.getType(Sample.class));
        // gen.push("input string");
        // gen.invokeVirtual(Type.getType(Sample.class), Method.getMethod("String doit(String)"));

        // invokeInterface
        // gen.push("input string");
        // gen.invokeInterface(Type.getType(ILambda.class), Method.getMethod("String doit(String)"));

        // println
        // gen.getStatic(Type.getType(System.class), "out", Type.getType(PrintStream.class));
        // gen.swap();
        // gen.invokeVirtual(Type.getType(PrintStream.class), Method.getMethod("void println(String)"));

        // invokeDynamic
        Handle h = new Handle(H_INVOKESTATIC, "cljstatic/Bootstrap", "bootstrap",
                              MethodType.methodType(CallSite.class, MethodHandles.Lookup.class, String.class, MethodType.class)
                              .toMethodDescriptorString());

        gen.push("input string");
        gen.invokeDynamic("foo", MethodType.methodType(String.class, ILambda.class, String.class).toMethodDescriptorString(), h);

        gen.returnValue();
        gen.endMethod();

        cw.visitEnd();

        classLoader.defineClass("cljstatic.SubExample", cw.toByteArray(), null);
    }
}
