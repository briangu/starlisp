package org.starlisp.core

import scala.Array

object LispSubr {
  def apply(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE)(t:(Array[LispObject]) => LispObject) : LispObject = {
    def f(arr: Array[LispObject]): LispObject = t(arr)
    new LispSubrImplicit(name, minArgs, maxArgs)(f)
  }
}

abstract class LispSubr(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends Procedure(name, minArgs, maxArgs) {
  override def toString: String = "#<subr %s >".format(name)
}

class LispSubrImplicit(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE)(t:(Array[LispObject]) => LispObject) extends Procedure(name, minArgs, maxArgs) {
  override def toString: String = "#<subr %s >".format(name)

  def bar = false

  def apply(objects: Array[LispObject]) : LispObject = t(objects)
}

