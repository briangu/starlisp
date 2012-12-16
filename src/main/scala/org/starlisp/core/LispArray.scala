package org.starlisp.core

import java.util.Arrays

class LispArray(protected val ar: Array[LispObject]) extends LispObject {

  def this(length: Int) = this(new Array[LispObject](length))
  def this(list: Cell) = this(list.toArray)

  def length: Int = ar.length

  override def hashCode: Int = Arrays.deepHashCode(ar.asInstanceOf[Array[Object]])
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[LispArray] && Arrays.deepEquals(ar.asInstanceOf[Array[Object]], (obj.asInstanceOf[LispArray]).ar.asInstanceOf[Array[Object]])
  }

  def aref(idx: Int): LispObject = ar(idx)

  def aset(idx: Int, obj: LispObject): LispObject = {
    val res = ar(idx)
    ar(idx) = obj
    res
  }

  override def toString: String = {
    val sb: StringBuffer = new StringBuffer
    sb.append("#(")
    for (o <- ar) {
      sb.append(LispObject.toStringOrNil(o))
      sb.append(' ')
    }
    sb.setLength(sb.length - 1)
    sb.append(')')
    sb.toString
  }
}