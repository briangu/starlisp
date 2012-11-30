package org.starlisp.core

object REPL extends App {
  val runtime = new Runtime
  while(true) {
    try {
      while(true) {
        Symbol.standardOutput.value.asInstanceOf[LispStream].writeJavaString("\n>>");
        println("\n\t" + runtime.eval(Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispStream])))
      }
    } catch {
      case e: Exception => e.printStackTrace;
    }
  }
}