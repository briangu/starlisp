package org.starlisp.core

import collection.mutable

object LispChar {
  val lowCache = {
    val lc = new Array[LispChar](256)
    (0 until 256).foreach{idx => lc(idx) = new LispChar(idx.asInstanceOf[Char])}
    lc
  }

  val cache = new mutable.HashMap[Char, LispChar]()
  def create(ch: Char) = {
    if (ch < 256) {
      lowCache(ch)
    } else {
      cache.getOrElseUpdate(ch, new LispChar(ch))
    }
  }
}

class LispChar(val ch: Char = 0) extends LispObject {
  override def hashCode = ch.hashCode
  override def equals(obj: Any) = obj.isInstanceOf[LispChar] && (obj.asInstanceOf[LispChar]).ch == ch
  override def toString = "#\\%c".format(ch)
}