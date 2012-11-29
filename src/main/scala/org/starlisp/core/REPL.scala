package org.starlisp.core

object REPL extends App {
  Starlisp.initEnvironment();
  while(true) {
    try {
      while(true) {
        Starlisp.standardOutput.value.writeJavaString("\n>>");
        println("\n\t" + Starlisp.eval(Starlisp.read(Starlisp.standardInput.value)))
      }
    } catch {
      case e: Exception => e.printStackTrace;
    }
  }
}