package org.starlisp.core

import scala.Array

abstract class LispFn(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE)
  extends Procedure(name, minArgs, maxArgs) {}

abstract class LispFn1[A <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn(name, minArgs, maxArgs) {
  def a(o: Array[LispObject]): A = o(0).asInstanceOf[A]
}

abstract class LispFn2[A <: LispObject,B <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn(name, minArgs, maxArgs) {
  def a(o: Array[LispObject]): A = o(0).asInstanceOf[A]
  def b(o: Array[LispObject]): B = o(1).asInstanceOf[B]
}
