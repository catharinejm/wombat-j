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

        // calls++;

        // Class targetClass = target.getClass();
        MethodHandle meth = caller.findVirtual(Sample.class, name, methodType.dropParameterTypes(0, 1));
        return new ConstantCallSite(meth);
    }

    public static void main(String[] args)
        throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, java.lang.reflect.InvocationTargetException, Throwable {

        buildClass();

        Class ex = Class.forName("cljstatic.Example", true, classLoader);
        ex.getMethod("blah").invoke(null);
    }

    static void buildClass() {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        Method m = new Method("blah", Type.VOID_TYPE, new Type[0]);
        GeneratorAdapter gen = new GeneratorAdapter(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, m, null, null, cw);

        MethodType mt = MethodType.methodType(CallSite.class, MethodHandles.Lookup.class, String.class, MethodType.class);
        Handle bh = new Handle(Opcodes.H_INVOKESTATIC, "cljstatic/ALambda", "bootstrap", mt.toMethodDescriptorString());
        
        cw.visit(V1_7, ACC_PUBLIC, "cljstatic/Example", null, "java/lang/Object", null);

        gen.visitCode();
        gen.newInstance(Type.getType(Sample.class));
        gen.dup();
        gen.invokeConstructor(Type.getType(Sample.class), new Method("<init>", Type.VOID_TYPE, new Type[0]));
        gen.dup();
        gen.invokeVirtual(Type.getType(Sample.class), Method.getMethod("void registerForBootstrap()"));
        gen.push("input string");

        gen.invokeDynamic("doit", MethodType.methodType(String.class, ALambda.class, String.class).toMethodDescriptorString(), bh);

        gen.getStatic(Type.getType(System.class), "out", Type.getType(PrintStream.class));
        gen.swap();
        gen.invokeVirtual(Type.getType(PrintStream.class), Method.getMethod("void println(String)"));

        gen.returnValue();
        gen.endMethod();

        cw.visitEnd();

        classLoader.defineClass("cljstatic.Example", cw.toByteArray(), null);
    }
}
