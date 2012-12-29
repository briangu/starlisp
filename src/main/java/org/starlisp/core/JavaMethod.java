package org.starlisp.core;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

public final class JavaMethod extends LispFn {

  // FIXME: ARRGGGHH I SEEM TO PREFER OBJECT OVER DOUBLE SOMEOFTHEMTIMES... is this bad? Is it instead maybe what we want?
  //        fix is probably in either accept (more probable) or argumentMoreSpecificThan

  // Maps argument classes to methods (this-object considered first argument in the list)
  private final static Map<String, Map<List<Class>, Monstructor>> methodMap = new HashMap<String, Map<List<Class>, Monstructor>>();
  private final Object obj;
  private final Monstructor[] methods;

  public JavaMethod(Monstructor[] methods, String name, Object obj) {
    super(name, 0, Integer.MAX_VALUE); // TODO: Send proper limits to super?
    this.methods = methods;
    this.obj = obj;
    if (!methodMap.containsKey(name)) {
      methodMap.put(name, new HashMap<List<Class>, Monstructor>());
    }
  }

  // NEWTHING: what about accpetiong when the lispArg is null? (anything can be null except int,float etc. (not Integer,Float etc.)
  // Checks if lispArg can be cast to the class javaArg represents
  // TODO: Arrays maybe, but they currently lack autoconverting and stuff since it's such a pita
  // TODO: For numbers (at least integers) only accept if a conversion won't lose precision. (in fact with this in place i
  // might just be able to, in argumentMoreSpecificThan, prefer narrower over wider for everything)
  private static boolean accept(Class javaArg, LispObject lispArg) {
    return (javaArg == Object.class || LispObject.class.isAssignableFrom(javaArg)) ? true : // Move this line? (with or without the LispObject part?)
        (javaArg == float.class || javaArg == Float.class ||
            javaArg == double.class || javaArg == Double.class) ? (lispArg instanceof LispFlonum) :
            (javaArg == char.class || javaArg == Character.class) ? (lispArg instanceof LispChar) :
                (javaArg == short.class || javaArg == Short.class ||
                    javaArg == int.class || javaArg == Integer.class ||
                    javaArg == long.class || javaArg == Long.class ||
                    javaArg == BigInteger.class) ? (lispArg instanceof LispInteger) :
                    (javaArg == String.class) ? (lispArg instanceof LispString) :
                        (javaArg == boolean.class || javaArg == Boolean.class) ? true :
                            (lispArg instanceof JavaObject) ? javaArg.isInstance(((JavaObject) lispArg).getObj()) :
                                false;
  }

  // Returns true if a is more specific than ("prefered" over) b, false otherwise. Additionally contains special case rules to
  // prefer "wider" argument types when it comes to numeric types instead of the other way around (i.e: prefer double over
  // float). TODO: Them arrays again
  // FIXME: bool support. think of it as a bool if it is either nil or t? special symbols javatrue javafalse?
  private static boolean argumentMoreSpecificThan(Class<?> a, Class<?> b) {
    return (a == b) ? false : // Can't make a judgement now can we
        // (a == Object.class)                        ? true : // BUGFISK? Object is always specificisestist
        // Special: LispObject is more specific than anything else yo, also tricky rules
        LispObject.class.isAssignableFrom(a) ? !(LispObject.class.isAssignableFrom(b) && a.isAssignableFrom(b)) :
            //(a == boolean.class || a == Boolean.class) ? (b != boolean.class || b != Boolean.class || !LispObject.class.isAssignableFrom(b)) : // But lispobject wins over bools
            (a == int.class || a == Integer.class) ? (b == short.class || b == Short.class) : // For numbers we prefer wider over narrower
                (a == long.class || a == Long.class) ? (b == int.class || b == Integer.class || b == short.class || b == Short.class) :
                    (a == BigInteger.class) ? (b == long.class || b == Long.class || b == int.class || b == Integer.class || b == short.class || b == Short.class) :
                        (a == double.class || a == Double.class) ? (b == float.class || b == Float.class) :
                            b.isAssignableFrom(a);                       // If b is assignable from a, a is more specific, "narrower", than b
  }

  // Conversion helpers, does the unboxing and boxing
  private static Object[] lispToJava(LispObject[] objs, Class[] argt) {
    Object[] res = new Object[objs.length];
    for (int i = 0; i < objs.length; ++i)
      res[i] = // (objs[i] instanceof JavaObject)                         ? ((JavaObject)objs[i]).getObj() :
          (LispObject.class.isAssignableFrom(argt[i])) ? objs[i] : // Wants a LispObject or subclass, no unboxing needed
              (objs[i] instanceof JavaObject) ? ((JavaObject) objs[i]).getObj() :
                  (argt[i] == float.class || argt[i] == Float.class) ? ((LispFlonum) objs[i]).toJavaFloat() :
                      (argt[i] == double.class || argt[i] == Double.class) ? ((LispFlonum) objs[i]).toJavaDouble() :
                          (argt[i] == int.class || argt[i] == Integer.class) ? ((LispInteger) objs[i]).toJavaInt() :
                              (argt[i] == short.class || argt[i] == Short.class) ? new Short((short) ((LispInteger) objs[i]).toJavaInt()) :
                                  (argt[i] == BigInteger.class) ? ((LispInteger) objs[i]).toJavaBigInteger() :
                                      (argt[i] == char.class || argt[i] == Character.class) ? ((LispChar) objs[i]).ch() :
                                          (argt[i] == long.class || argt[i] == Long.class) ? ((LispInteger) objs[i]).toJavaLong() :
                                              (argt[i] == Boolean.class || argt[i] == boolean.class) ? objs[i] != null :
                                                  (argt[i] == String.class) ? ((LispString) objs[i]).toJavaString() :
                                                      objs[i];
    return res;
  }

  // Matches a method given a list of LispObjects that this method will be called with. A three-stage rocket.
  // TODO: Split me into my three logical parts.
  private int matchMethod(LispObject[] lispArgs) {
    LinkedList<Integer> list = new LinkedList<Integer>();
    for (int i = 0; i < methods.length; ++i) list.add(i);

    // Prune all methods of insufficient length
    ListIterator<Integer> it = list.listIterator();
    while (it.hasNext())
      if (methods[it.next()].getParameterTypes().length != lispArgs.length) {
        it.remove();
      }
    if (list.isEmpty()) return -1;                  // No match
    if (list.size() == 1) {
      return list.getFirst();   // We have a match here (there was but one method of this length)
    }

    System.out.println("hej");

    // Prune all methods that don't have a chance of matching
    for (int argNumber = 0; argNumber < lispArgs.length; ++argNumber) {
      it = list.listIterator();
      while (it.hasNext())
        if (!accept(methods[it.next()].getParameterTypes()[argNumber], lispArgs[argNumber])) {
          it.remove();
        }
      if (list.isEmpty()) return -1;                  // No match
      if (list.size() == 1) return list.getFirst();   // We have a potential match here
    }

    System.out.println("fisk");

    for (Integer i : list)
      System.out.print(methods[i] + " \\\\//");
    System.out.println("\n----");

    // Find the most specific method of the matching methods
    for (int argNumber = 0; argNumber < lispArgs.length; ++argNumber) {
      it = list.listIterator();
      // Find the maximum (most specific) of all the argNumber'th method arguments classes
      int max = list.getFirst();
      int tmp;
      while (it.hasNext())
        max = argumentMoreSpecificThan(methods[tmp = it.next()].getParameterTypes()[argNumber], methods[max].getParameterTypes()[argNumber]) ? tmp : max;
      it = list.listIterator();
      // Remove all less specific methods.
      System.out.println("mask " + methods[max].getParameterTypes()[argNumber]);
      while (it.hasNext()) {
        int asdf = it.next();
        System.out.println("asdf " + methods[asdf].getParameterTypes()[argNumber]);
        if (methods[asdf].getParameterTypes()[argNumber] != methods[max].getParameterTypes()[argNumber]) {
          it.remove();
        }
      }

      if (list.size() == 1) return list.getFirst();   // We have found our match
    }

    for (Integer i : list)
      System.out.print(methods[i] + " \\\\//");
    System.out.println("\n----");

    // KLUDGE: If we still find ourselves confused and with more than a single method left. prefer those that don't return
    // an abstract type. Majorly kludgeish.... TODO: think up a scenario to make this explode or something (can there be
    // cases where we have several alternatives here yet all of them return an abstract type?)
    it = list.listIterator();
    while (it.hasNext())
      if (Modifier.isAbstract(methods[it.next()].getReturnType().getModifiers())) {
        it.remove();
      }

    for (Integer i : list)
      System.out.print(methods[i] + " \\\\//");
    System.out.println("\n----");

    if (list.size() == 1) return list.getFirst();


    throw new LispException(Symbol.internalError(), "This should not happen!"); // Seriously, it shouldn't!
  }

  private LispObject javaToLisp(Object obj) {
    return (obj == null) ? null :
        (obj instanceof LispObject) ? (LispObject) obj :
            (obj instanceof Float) ? new LispFlonum((Float) obj) :
                (obj instanceof Double) ? new LispFlonum((Double) obj) :
                    (obj instanceof Short) ? new LispFixnum((Short) obj) :
                        (obj instanceof Integer) ? new LispFixnum((Integer) obj) :
                            (obj instanceof Long) ? new LispFixnum((Long) obj) :
                                (obj instanceof Character) ? LispChar.create((Character) obj) :
                                    (obj instanceof BigInteger) ? new LispBigInt((BigInteger) obj) :
                                        (obj instanceof String) ? new LispString((String) obj) :
                                            (obj instanceof Boolean) ? (Boolean) obj == true ? Symbol$.MODULE$.t() : null :
                                                new JavaObject(obj);
  }

  // Gives a list of classes; the argument types of the list, used as a key for memoizing in methodMap
  private List<Class> getArgumentTypes(Object[] list) {
    List<Class> res = new ArrayList<Class>(list.length + 1);
    for (int i = 0; i < list.length; ++i)
      res.add((list[i] == null) ? null : // Don't want NullPointerExceptions
                  (list[i] instanceof JavaObject) ? ((JavaObject) list[i]).getObj().getClass() : // For JavaObject it is more useful to use the class of the object it contains
                      list[i].getClass());
    return res;
  }

  // Apply method to objects and the closed-over this object
  public LispObject apply(LispObject[] objects) {
    try {
      List<Class> argumentTypes = getArgumentTypes(objects);
      Class storeKlas = (obj instanceof Class) ? (Class) obj : obj.getClass(); // We need to use obj as key when it is an instance of Class
      argumentTypes.add(0, storeKlas);                                        // prepend the class to the list of argument types
      Monstructor method = methodMap.get(name()).get(argumentTypes);
      if (method == null) {
        int m = matchMethod(objects);
        if (m == -1) {
          throw new LispException(Symbol.internalError(), "No matching method found for the args: " + Arrays.toString(objects));
        }
        method = methods[m];
        methodMap.get(name()).put(argumentTypes, method);
      }
      return javaToLisp(method.invoke(obj, lispToJava(objects, method.getParameterTypes()))); // Wee...
    } catch (IllegalAccessException | InvocationTargetException | InstantiationException e) {
      throw new LispException(Symbol.internalError(), e);
    }
  }

  public String toString() {
    return "#<java-method " + name() + " | " + obj + ">";
  }

  public boolean equals(Object obj) {
    return (obj instanceof JavaMethod) ? this.obj == ((JavaMethod) obj).obj && Arrays.equals(methods, ((JavaMethod) obj).methods) : false;
  }

  public int hashCode() {
    return Arrays.hashCode(methods) ^ System.identityHashCode(obj);
  }
}
