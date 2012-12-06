package org.starlisp

import core._

object REPL extends App {
  val runtime = new Runtime
  while(!Starlisp.done) {
    try {
      while(!Starlisp.done) {
        Symbol.standardOutput.value.asInstanceOf[LispOutputStream].write("\n>>");
        val list = Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispInputStream])
        println("\n%s".format(runtime.eval(list)))
      }
    } catch {
      case e: Exception => e.printStackTrace;
    }
  }
}