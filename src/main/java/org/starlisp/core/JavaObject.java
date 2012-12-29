package org.starlisp.core;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public final class JavaObject extends LispFn {
  private final static Map<Class, Map<Symbol, Monstructor[]>> methodMap = new HashMap<Class, Map<Symbol, Monstructor[]>>();
  private final static Symbol newInstance = RootEnvironment.intern("newInstance");
  private final Class klas;
  private final Object obj;

  /* Wrap that object! */
  public JavaObject(Object obj) {
    super(obj.toString(), 1, 1);
    this.obj = obj;
    // klas = (obj != null) ? obj.getClass() : null;
    klas = obj.getClass();
    Class storeKlas = (klas == Class.class) ? (Class) obj : klas; // We need to use obj as key when it is an instance of Class
    if (!methodMap.containsKey(storeKlas)) {
      methodMap.put(storeKlas, new HashMap<Symbol, Monstructor[]>());
    }
  }

  /* Apply object to symbol generating a "closure", a.k.a. method. */
  public JavaMethod apply(LispObject[] o) {
    Symbol sbl = (Symbol) o[0];
    ArrayList<Monstructor> methodList = new ArrayList<Monstructor>();
    Monstructor[] methodArray;
    Class storeKlas = (klas == Class.class) ? (Class) obj : klas; // We need to use obj as key when it is an instance of Class
    if ((methodArray = methodMap.get(storeKlas).get(sbl)) != null) {
      return new JavaMethod(methodArray, sbl.name(), obj);
    }
    if (obj != Class.class && obj instanceof Class) {
      // Special case when obj is a Class object (but not a Class object representing a Class object. That is: not Class.class):
      // Allow, in addition to accessing the methods of the object, access to static methods, and the constructors of the
      // class this object represents. If you are confused now blame Javas reflection API.
      for (Method m : ((Class) obj).getMethods())       // Find static methods
        if (Modifier.isStatic(m.getModifiers()) && m.getName().equals(sbl.name())) {
          methodList.add(new Monstructor(m));
        }
      if (sbl == newInstance)                         // Yay, constructors! (Note: we do not try to fetch any more methods in  this case)
      {
        for (Constructor c : ((Class) obj).getConstructors())
          methodList.add(new Monstructor(c));
      } else {
        for (Method m : klas.getMethods())
          // for (Method m: klas.getDeclaredMethods())
          if (m.getName().equals(sbl.name()))
          // if (m.getName().equals(sbl.name()) && !Modifier.isAbstract(m.getReturnType().getModifiers())) // say no to methods returning abstract types
          {
            methodList.add(new Monstructor(m));
          }
      }
    } else {
      for (Method m : klas.getMethods())
        // for (Method m: klas.getDeclaredMethods())
        if (m.getName().equals(sbl.name()))
        // if (m.getName().equals(sbl.name()) && !Modifier.isAbstract(m.getReturnType().getModifiers())) // say no to methods returning abstract types
        {
          methodList.add(new Monstructor(m));
        }
    }
    if (methodList.isEmpty()) {
      throw new LispException(Symbol$.MODULE$.internalError(), "No such method: " + sbl.name() + ", " + toString());
    }
    methodArray = methodList.toArray(new Monstructor[0]);    // Umm... Not pretty API here Java...
    methodMap.get(storeKlas).put(sbl, methodArray);          // Cache the results
    return new JavaMethod(methodArray, sbl.name(), obj);
  }

  public Object getObj() {
    return obj;
  }

  public String toString() {
    return "#<java-object " + obj + " | " + klas + ">";
  }

  public boolean equals(Object obj) {
    return (obj instanceof JavaObject) && ((JavaObject) obj).obj.equals(this.obj);
  }

  public int hashCode() {
    return obj.hashCode();
  }
}
