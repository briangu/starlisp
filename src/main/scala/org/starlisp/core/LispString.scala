package org.starlisp.core

class LispString(length: Int) extends LispArray(length) {

  var str: String = null

  def this(arr: String) = {
    this(arr.length)
    str = arr
    // TODO: use OBJLIST
    (0 until length).foreach(idx => ar(idx) = LispChar.create(arr.charAt(idx)))
  }

  def this(arr: Array[Char]) = {
    this(arr.length)
    // TODO: use OBJLIST
    (0 until length).foreach(idx => ar(idx) = LispChar.create(arr(idx)))
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
    if (str == null) {
      val sb: StringBuffer = new StringBuffer
      for (o <- ar) sb.append((o.asInstanceOf[LispChar]).ch)
      sb.toString
    } else {
      str
    }
  }

  override def toString: String = {
    return '"' + toJavaString + '"'
  }
}