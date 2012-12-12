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

class LispString(length: Int) extends LispArray(length) {

  def this(arr: String) = {
    this(arr.length)
    // TODO: use OBJLIST
    (0 until length).foreach(idx => ar(idx) = LispChar.create(arr.charAt(idx)))
  }

  def this(arr: Array[Char], length: Int) = {
    this(length)
    // TODO: use OBJLIST
    (0 until length).foreach(idx => ar(idx) = LispChar.create(arr(idx)))
  }

  def this(arr: Array[Char]) = {
    this(arr, arr.length)
  }

  def this(length: Int, ch: LispChar) = {
    this(length)
    (0 until length).foreach(ar(_) = ch)
  }

  override def aset(idx: Int, obj: LispObject): LispObject = {
    if (!(obj.isInstanceOf[LispChar])) throw new LispException(Symbol.internalError, "Only Char may be in a string.")
    super.aset(idx, obj)
  }

  def toJavaString: String = {
    val sb: StringBuffer = new StringBuffer
    for (o <- ar) sb.append((o.asInstanceOf[LispChar]).ch)
    sb.toString
  }

  override def toString: String = "\"%s\"".format(toJavaString)
}