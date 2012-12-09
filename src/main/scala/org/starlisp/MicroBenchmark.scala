package org.starlisp

import core._
import java.io.StringBufferInputStream

object MicroBenchmark {
  def warmup(runtime: Runtime) {
    Symbol.standardOutput.value.asInstanceOf[LispOutputStream].write("\n>>");
    val list = Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispInputStream])
    val start = System.currentTimeMillis()
    (0 until 100000).foreach{ idx =>
      runtime.eval(list)
    }
    val now = System.currentTimeMillis()
    println("\n\t time = %d".format(now - start))
  }

  val fixed = "(set (quote defun) (macro (a) (cons (quote set) (cons (cons (quote quote) (cons (car (cdr a)) nil)) (cons (cons (quote lambda) (cdr (cdr a))) nil)))))"

  def warmupParser(runtime: Runtime) {
    var sum: Long = 0
    (0 until 100000).foreach { idx =>
      val start = System.currentTimeMillis()
      Starlisp.read(new LispStreamImpl(new StringBufferInputStream(fixed), null))
      sum += System.currentTimeMillis() - start
    }
    println("\n\t time = %d %f".format(sum, (sum / 100000.0)))
  }

  def warmup2(runtime: Runtime) {
    while(!runtime.stopped) {
      try {
        while(!runtime.stopped) {
          Symbol.standardOutput.value.asInstanceOf[LispOutputStream].write("\n>>");
          val list = Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispInputStream])
          println("\n\t %s".format(runtime.eval(list)))
        }
      } catch {
        case e: Exception => e.printStackTrace;
      }
    }
  }

  def main(args: Array[String]) {
    var runtime = new Runtime

    warmup(runtime)
    warmup(runtime)
    warmup(runtime)
    warmup2(runtime)
    runtime = new Runtime
    warmup(runtime)

    println("doing real job")

    var start : Long = 0
    var now: Long = 0
    while(!runtime.stopped) {
      try {
        while(!runtime.stopped) {
          Symbol.standardOutput.value.asInstanceOf[LispOutputStream].write("\n>>");
          val list = Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispInputStream])
          if (start == 0) start = System.currentTimeMillis()
          runtime.eval(list)
          now = System.currentTimeMillis()
          println("total time: %d".format(now - start))
        }
      } catch {
        case e: Exception => e.printStackTrace;
      }
    }
    now = System.currentTimeMillis()
    println("total time: %d".format(now - start))
    Counters.report()
  }
}