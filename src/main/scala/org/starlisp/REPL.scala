package org.starlisp

import core._

object REPL {
  def warmup(runtime: Runtime) {
    Symbol.standardOutput.value.asInstanceOf[LispStream].writeJavaString("\n>>");
    val list = Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispStream])
    var sum : Long = 0
    (0 until 100000).foreach{ idx =>
      val start = System.currentTimeMillis()
      runtime.eval(list)
      sum += System.currentTimeMillis() - start
    }
    println("\n\t time = %d %f".format(sum, (sum / 100000.0)))
  }

  def warmup2(runtime: Runtime) {
    while(!Starlisp.done) {
      try {
        while(!Starlisp.done) {
          Symbol.standardOutput.value.asInstanceOf[LispStream].writeJavaString("\n>>");
          val list = Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispStream])
          println("\n\t %s".format(runtime.eval(list)))
        }
      } catch {
        case e: Exception => e.printStackTrace;
      }
    }
  }

  def main(args: Array[String]) {
    var runtime = new Runtime
/*
    warmup(runtime)
    warmup2(runtime)
    Starlisp.done = false
    runtime = new Runtime
    warmup(runtime)
*/
    val start = System.currentTimeMillis()
    while(!Starlisp.done) {
      try {
        while(!Starlisp.done) {
          Symbol.standardOutput.value.asInstanceOf[LispStream].writeJavaString("\n>>");
          val list = Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispStream])
          println("\n\t %s".format(runtime.eval(list)))
        }
      } catch {
        case e: Exception => e.printStackTrace;
      }
    }
    println("total time: %d".format(System.currentTimeMillis() - start))
  }
}