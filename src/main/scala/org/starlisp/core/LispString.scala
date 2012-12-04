package org.starlisp.core

class LispString(length: Int) extends LispArray(length) {

  def this(arr: String) = {
    this(arr.length)
    Counters.inc("lispstring:str:create")
    // TODO: use OBJLIST
    (0 until length).foreach(idx => ar(idx) = LispChar.create(arr.charAt(idx)))
  }

  def this(arr: Array[Char], length: Int) = {
    this(length)
    Counters.inc("lispstring:arr1:create")
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
    return super.aset(idx, obj)
  }

  def toJavaString: String = {
    val sb: StringBuffer = new StringBuffer
    for (o <- ar) sb.append((o.asInstanceOf[LispChar]).ch)
    sb.toString
  }

  override def toString: String = {
    return '"' + toJavaString + '"'
  }
}