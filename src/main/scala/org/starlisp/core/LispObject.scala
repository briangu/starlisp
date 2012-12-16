package org.starlisp.core

object LispObject {
  def toStringOrNil(obj: LispObject): String = Option(obj).getOrElse("nil").toString
}

class LispObject {
  def as[T] = this.asInstanceOf[T]
}
