(ns cljstatic.core
  (:import [clojure.asm ClassWriter ClassVisitor Opcodes Type]
           [clojure.asm.commons GeneratorAdapter Method]
           [clojure.lang DynamicClassLoader Compiler RT]))



(comment
   (def cw (ClassWriter. ClassWriter/COMPUTE_MAXS))
   (. cw (visit Opcodes/V1_5 Opcodes/ACC_PUBLIC
                "cljstatic/core/TestClass" nil "java/lang/Object" (make-array String 0)))

   (let [m (Method. "turd" Type/VOID_TYPE (into-array Type [(Type/getType String)]))
         gen (GeneratorAdapter. (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                                m nil nil cw)]
     (.visitCode gen)
     (. gen push "clojure.core")
     (. gen push "*out*")
     (. gen invokeStatic (Type/getType clojure.lang.RT) (. Method getMethod "clojure.lang.Var var(String,String)"))
     (. gen invokeVirtual (Type/getType clojure.lang.Var) (. Method getMethod "java.lang.Object get()"))
     (. gen dup)
     (. gen checkCast (Type/getType java.io.PrintWriter))
     (. gen loadArg 0)
     (. gen invokeVirtual (Type/getType java.io.PrintWriter) (. Method getMethod "void println(String)"))
     (.returnValue gen)
     (.endMethod gen))

   (.visitEnd cw)

   (. @Compiler/LOADER (defineClass "cljstatic.core.TestClass" (.toByteArray cw) nil))

   (import 'cljstatic.core.TestClass))
