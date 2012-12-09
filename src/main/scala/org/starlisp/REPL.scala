package org.starlisp

import core._

object REPL extends App {
  val runtime = new Runtime
  while(!runtime.stopped) {
    try {
      val out = Symbol.standardOutput.value.asInstanceOf[LispOutputStream]
      val in = Symbol.standardInput.value.asInstanceOf[LispInputStream]
      while(!runtime.stopped) {
        out.write("\n>> ");
        Starlisp.prin1(runtime.eval(Starlisp.read(in)), out)
      }
    } catch {
      case e: Exception => e.printStackTrace;
    }
  }
}