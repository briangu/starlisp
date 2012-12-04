package org.starlisp.core

import collection.mutable

object LispChar {
  val cache = new mutable.HashMap[Char, LispChar]()
  def create(ch: Char) = new LispChar(ch)//cache.getOrElseUpdate(ch, new LispChar(ch))
}

class LispChar(val ch: Char = 0) extends LispObject {

  override def hashCode: Int = {
    return ch.hashCode
  }

  override def equals(obj: Any): Boolean = {
    return if ((obj.isInstanceOf[LispChar])) (obj.asInstanceOf[LispChar]).ch == ch else false
  }

  override def toString: String = {
    return "#\\" + ch
  }
}