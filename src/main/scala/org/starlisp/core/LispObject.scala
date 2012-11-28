package org.starlisp.core

class LispObject {
  def printObject(stream: LispStream) {
    stream.writeJavaString(this.toString)
  }
}