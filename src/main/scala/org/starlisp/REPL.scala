package org.starlisp

import core._

object REPL extends App {
  val runtime = new Runtime
  while(!runtime.stopped) {
    try {
      val out = Symbol.standardOutput.value.asInstanceOf[LispOutputStream]
      val in = runtime.standardInput.value.asInstanceOf[LispInputStream]
      while(!runtime.stopped) {
        out.write("\n>> ")
        runtime.prin1(runtime.eval(runtime.read(in)), out)
      }
    } catch {
      case e: LispException => println(e.getMessage)
      case e: Exception => e.printStackTrace;
    }
  }
}