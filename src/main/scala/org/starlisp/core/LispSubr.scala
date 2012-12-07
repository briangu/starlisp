package org.starlisp.core

abstract class LispSubr(name : String = "", minArgs: Int = 0, maxArgs: Int = Integer.MAX_VALUE) extends Procedure(name, minArgs, maxArgs) {
  override def toString: String = "#<subr %s >".format(name)
}