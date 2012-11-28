package org.starlisp.core;

class LispObject {
  public void printObject(LispStream stream) {
    stream.writeJavaString(this.toString());
  }
}
