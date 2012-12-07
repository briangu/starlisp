package org.starlisp

import core._

object REPL extends App {
  val runtime = new Runtime
  while(!runtime.done) {
    try {
      val out = Symbol.standardOutput.value.asInstanceOf[LispOutputStream]
      val in = Symbol.standardInput.value.asInstanceOf[LispInputStream]
      while(!runtime.done) {
        out.write("\n>> ");
        out.write(String.valueOf(runtime.eval(Starlisp.read(in))))
      }
    } catch {
      case e: Exception => e.printStackTrace;
    }
  }
}