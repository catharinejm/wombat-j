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

        MethodHandle mh = MethodHandles.invoker(methodType.dropParameterTypes(0, 1));
        return new ConstantCallSite(mh);
    }

    public static void main(String[] args)
        throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, java.lang.reflect.InvocationTargetException, Throwable {

        buildClass();

        Class ex = Class.forName("cljstatic.Example", true, classLoader);

        Object o = ex.newInstance();
        ex.getMethod("blah", ALambda.class).invoke(o, new Sample());
        ex.getMethod("blah", ALambda.class).invoke(o, new SubSample());
        
        System.out.println("Called bootstrap " + calls + " times.");
    }

    static void buildClass() {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        cw.visit(V1_7, ACC_PUBLIC, "cljstatic/Example", null, "java/lang/Object", null);

        Method ctor = new Method("<init>", Type.VOID_TYPE, new Type[0]);
        GeneratorAdapter ctorgen = new GeneratorAdapter(Opcodes.ACC_PUBLIC, ctor, null, null, cw);

        ctorgen.visitCode();
        ctorgen.loadThis();
        ctorgen.invokeConstructor(Type.getType(Object.class), Method.getMethod("void <init>()"));
        ctorgen.returnValue();
        ctorgen.endMethod();

        Type[] ts = new Type[1];
        ts[0] = Type.getType(ALambda.class);
        Method m = new Method("blah", Type.VOID_TYPE, ts);
        GeneratorAdapter gen = new GeneratorAdapter(Opcodes.ACC_PUBLIC, m, null, null, cw);

        gen.visitCode();
        gen.loadArg(0);
        gen.dup();
        gen.push(1);
        gen.invokeVirtual(Type.getType(ALambda.class), Method.getMethod("java.lang.invoke.MethodHandle getHandle(int)"));
        gen.swap();
        gen.push("input string");

        gen.invokeVirtual(Type.getType(MethodHandle.class), Method.getMethod("String invoke(cljstatic.ALambda, Object)"));

        gen.getStatic(Type.getType(System.class), "out", Type.getType(PrintStream.class));
        gen.swap();
        gen.invokeVirtual(Type.getType(PrintStream.class), Method.getMethod("void println(String)"));

        gen.returnValue();
        gen.endMethod();

        cw.visitEnd();

        classLoader.defineClass("cljstatic.Example", cw.toByteArray(), null);
    }
}
