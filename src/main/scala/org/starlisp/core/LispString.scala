package org.starlisp.core

class LispString(length: Int) extends LispArray(length) {

  def this(str: String) = {
    this(str.length)
    // TODO: use OBJLIST
    (0 until length).foreach(idx => ar(idx) = new LispChar(str.charAt(idx)))
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
    return sb.toString
  }

  override def toString: String = {
    return '"' + toJavaString + '"'
  }
}