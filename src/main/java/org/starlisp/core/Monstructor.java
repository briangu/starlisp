package org.starlisp.core;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

// Java-LJSP interface
// I combined Method and Constructor, and what do I get? Monstructor! of course! (this helps
// in treating cosntructors like methods)
final class Monstructor {
  private final Method method;
  private final Constructor ctor;

  public Monstructor(Constructor ctor) {
    this.ctor = ctor;
    this.method = null;
  }

  public Monstructor(Method method) {
    this.method = method;
    this.ctor = null;
  }

  public Class<?> getReturnType() {
    return (ctor == null) ? method.getReturnType() : ctor.getDeclaringClass();
  }

  public Class<?>[] getParameterTypes() {
    return (ctor == null) ? method.getParameterTypes() : ctor.getParameterTypes();
  }

  /**
   * obj is ignored for static methods and constructors
   */
  public Object invoke(Object obj, Object... args) throws IllegalAccessException, InvocationTargetException, InstantiationException {
    return (ctor == null) ? method.invoke(obj, args) : ctor.newInstance(args);
  }

  public String toString() {
    return (ctor == null) ? method.toString() : ctor.toString();
  }

  public int hashCode() {
    return (ctor == null) ? method.hashCode() : ctor.hashCode();
  }

  public boolean equals(Object obj) {
    return (obj instanceof Monstructor) ? ((ctor == null) ? method.equals(((Monstructor) obj).method) : ctor.equals(((Monstructor) obj).ctor)) :
        (obj instanceof Method) ? ((ctor == null) ? method.equals(obj) : false) :
            (obj instanceof Constructor) ? ((ctor == null) ? ctor.equals(obj) : false) :
                false;
  }
}
