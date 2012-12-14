package org.starlisp.core

import scala.Array

abstract class LispFn(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE)
  extends Procedure(name, minArgs, maxArgs)
{
  def apply(objects: Array[LispObject]): LispObject
  def apply(env: Environment, objects: Array[LispObject]): LispObject = apply(objects)
}

abstract class LispFn1[A <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn(name, minArgs, maxArgs) {
  def a(o: Array[LispObject]): A = o(0).asInstanceOf[A]
}

abstract class LispFn2[A <: LispObject,B <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn(name, minArgs, maxArgs) {
  def a(o: Array[LispObject]): A = o(0).asInstanceOf[A]
  def b(o: Array[LispObject]): B = o(1).asInstanceOf[B]
}

// LispFnPair - same as LispFn2, but using the same types for A and B
abstract class LispFnP[A <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn2[A,A](name, minArgs, maxArgs)

abstract class LispFn3[A <: LispObject,B <: LispObject,C <: LispObject](name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends LispFn(name, minArgs, maxArgs) {
  def a(o: Array[LispObject]): A = o(0).asInstanceOf[A]
  def b(o: Array[LispObject]): B = o(1).asInstanceOf[B]
  def c(o: Array[LispObject]): C = o(2).asInstanceOf[C]
}
