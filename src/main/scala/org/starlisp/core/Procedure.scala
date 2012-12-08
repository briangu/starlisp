package org.starlisp.core

abstract class Procedure(val name : String = "", val minArgs: Int = 0, val maxArgs: Int = Integer.MAX_VALUE) extends LispObject {
  def this(name: String, numArgs: Int) = this(name, numArgs, numArgs)

  final def applyArgs(o: Array[LispObject]): LispObject = {
    if (o.length < minArgs) throw new LispException(Symbol.internalError, "Too few args when calling procedure: " + toString)
    if (o.length > maxArgs) throw new LispException(Symbol.internalError, "Too many args when calling procedure: " + toString)
    apply(o)
  }

  def apply(objects: Array[LispObject]): LispObject

  override def toString: String = "#<subr %s >".format(name)
}