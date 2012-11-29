package org.starlisp.core

object REPL extends App {
  Starlisp.initEnvironment;
  while(true) {
    try {
      while(true) {
        Starlisp.standardOutput.value.asInstanceOf[LispStream].writeJavaString("\n>>");
        println("\n\t" + Starlisp.eval(Starlisp.read(Starlisp.standardInput.value.asInstanceOf[LispStream])))
      }
    } catch {
      case e: Exception => e.printStackTrace;
    }
  }
}