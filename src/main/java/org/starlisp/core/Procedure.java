package org.starlisp.core;

public abstract class Procedure extends LispObject {
  public final String name;
  public final int minArgs, maxArgs;

  public Procedure() {
    this("", 0, Integer.MAX_VALUE);
  }

  public Procedure(String name) {
    this(name, 0, Integer.MAX_VALUE);
  } // FIXME: Don't know if this constructor will see much use

  public Procedure(String name, int numArgs) {
    this(name, numArgs, numArgs);
  }

  public Procedure(String name, int minArgs, int maxArgs) {
    this.name = name;
    this.minArgs = minArgs;
    this.maxArgs = maxArgs;
  }

  public final LispObject applyArgs(LispObject[] o) {
    if (o.length < minArgs)
      throw new LispException(Starlisp.internalError, "Too few args when calling procedure: " + toString());
    if (o.length > maxArgs)
      throw new LispException(Starlisp.internalError, "Too many args when calling procedure: " + toString());
    return apply(o);
  }

  public abstract LispObject apply(LispObject... objects);
}
