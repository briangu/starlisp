package org.starlisp

import core._
import java.io.StringBufferInputStream

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

  /*
  (set (quote defun) (macro (a) (cons (quote set) (cons (cons (quote quote) (cons (car (cdr a)) nil)) (cons (cons (quote lambda) (cdr (cdr a))) nil)))))

   */
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
    warmupParser(runtime)
    warmup(runtime)
    warmup2(runtime)
    Starlisp.done = false
    runtime = new Runtime
    warmup(runtime)
    System.gc()
    Thread.sleep(500)

    var start : Long = 0
    var now: Long = 0
    while(!Starlisp.done) {
      try {
        while(!Starlisp.done) {
//          Symbol.standardOutput.value.asInstanceOf[LispStream].writeJavaString("\n>>");
          val list = Starlisp.read(Symbol.standardInput.value.asInstanceOf[LispStream])
          if (start == 0) start = System.currentTimeMillis()
          runtime.eval(list)
         // println("\n\t %s".format(runtime.eval(list)))
          now = System.currentTimeMillis()
          if (now - start > 100) {
            println(list)
          }
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