package org.starlisp.core

object REPL extends App {
  Starlisp.initEnvironment();
  while(true) {
    try {
      while(true) {
        Starlisp.standardOutput.value.writeJavaString("\n>>");
        Starlisp.prin1(Starlisp.eval(Starlisp.read(null)), null);
      }
    } catch {
      case e: Exception => e.printStackTrace;
    }
  }
}