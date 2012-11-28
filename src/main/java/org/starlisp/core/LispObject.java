package org.starlisp.core;

public class LispObject {
  public void printObject(LispStream stream) {
    stream.writeJavaString(this.toString());
  }
}
