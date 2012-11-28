package org.starlisp.core;

abstract class LispSubr extends Procedure {             // FIXME: This class does nothing but modify toString, remove in cleanup later on?

  public LispSubr(String name) {
    super(name);
  }

  public LispSubr(String name, int numArgs) {
    super(name, numArgs);
  }

  public LispSubr(String name, int minArgs, int maxArgs) {
    super(name, minArgs, maxArgs);
  }

  public String toString() {
    return "#<subr " + name + ">";
  }
}
