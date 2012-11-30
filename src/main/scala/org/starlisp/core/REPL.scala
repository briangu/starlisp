package org.starlisp.core

object REPL extends App {
  Starlisp.initEnvironment;
  while(true) {
    try {
      while(true) {
        Symbol.standardOutput.value.asInstanceOf[LispStream].writeJavaString("\n>>");
        println("\n\t" + Starlisp.eval(Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispStream])))
      }
    } catch {
      case e: Exception => e.printStackTrace;
    }
  }
}