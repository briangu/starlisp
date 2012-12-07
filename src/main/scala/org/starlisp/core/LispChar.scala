package org.starlisp.core

import collection.mutable

object LispChar {
  val lowCache = {
    val lc = new Array[LispChar](256)
    (0 until 255).foreach{idx => lc(idx) = new LispChar(idx.asInstanceOf[Char])}
    lc
  }

  val cache = new mutable.HashMap[Char, LispChar]()
  def create(ch: Char) = {
    if (ch <= 255) {
      lowCache(ch)
    } else {
      cache.getOrElseUpdate(ch, new LispChar(ch))
    }
  }
}

class LispChar(val ch: Char = 0) extends LispObject {
  override def hashCode: Int = ch.hashCode
  override def equals(obj: Any): Boolean = {
    if ((obj.isInstanceOf[LispChar])) (obj.asInstanceOf[LispChar]).ch == ch else false
  }
  override def toString: String = "#\\%c".format(ch)
}