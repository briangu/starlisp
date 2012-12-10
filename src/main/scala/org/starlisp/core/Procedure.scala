package org.starlisp.core

abstract class Procedure(val name : String = "", val minArgs: Int = 0, val maxArgs: Int = Integer.MAX_VALUE) extends LispObject {
  def this(name: String, numArgs: Int) = this(name, numArgs, numArgs)

  def apply(objects: Array[LispObject]): LispObject

  override def toString: String = "#<subr %s >".format(name)
}